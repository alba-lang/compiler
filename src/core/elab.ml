open Printf
open Std


module type ANY = Fmlib_std.Interfaces.ANY


module Pretty = Fmlib_pretty.Print


type range = Fmlib_parse.Position.range

type term    = Gamma.term
type gamma   = Gamma.t
type globals = Globals.t




let gamma_of_term (t: term): gamma =
    Gamma.gamma_of_term t


let term_of_term (t: term): Term.t =
    Gamma.term_of_term t


let type_of_term (t: term): term =
    Gamma.type_of_term t


let make_globals () =
    Globals.make ()


let make_gamma (g: globals): gamma =
    Gamma.empty g


let doc_of_term (t: term) (): Pretty.doc =
    Printer.Term.base_doc true true (term_of_term t)



module State =
struct
    type tracing =
        | Never
        | Always
        | On_error

    type t = {
        gamma: Gamma.t;
        tracing: tracing;
        tracer: Tracer.t;
    }


    let tracer t = t.tracer


    let make (gamma: gamma): t =
        {
            gamma;
            tracing = On_error;
            tracer  = Tracer.ignore;
        }

    let make_trace (gamma: gamma): t =
        {
            gamma;
            tracing = Always;
            tracer  = Tracer.empty
        }

    let make_no_trace (gamma: gamma): t =
        {
            gamma;
            tracing = Never;
            tracer  = Tracer.ignore
        }
end










module Hole =
struct
    type info =
        | Elab of
              int option (* parent hole *)
        | Constraint
    type t = {
        info: info;
        gamma: gamma;
        args: (term * bool) array;
        res_tp: term;
    }

    let e_type (parent: int option) (gamma: gamma): t =
        (* Create a type hole for an elaborated type (i.e. present in the source
           code). *)
        {
            info = Elab parent;
            gamma;
            args = [||];
            res_tp = Gamma.top 0 gamma;
        }


    let e_term (parent: int option) (res_tp: term) (gamma: gamma): t =
        {
            info = Elab parent;
            gamma;
            args = [||];
            res_tp
        }


    let c_type gamma =
        {
            info = Constraint;
            gamma;
            args = [||];
            res_tp = Gamma.top 0 gamma;
        }



    let gamma (h: t): gamma =
        h.gamma


    let is_unifiable (h: t): bool =
        match h.info with
        | Elab _ -> false
        | _      -> true


    let type_of (h: t): term =
        h.res_tp


    let doc (h: t) (): Pretty.doc =
        let open Pretty in
        let arg (t,impl) =
            let inner =
                doc_of_term t ()
                <+> char ':' <+> space
                <+> doc_of_term (type_of_term t) ()
            in
            if impl then
                space <+> char '{' <+> inner <+> char '}'
            else
                space <+> char '(' <+> inner <+> char ')'
        in
        text (if is_unifiable h then "c-hole" else "e-hole")
        <+> cat (List.map arg (Array.to_list h.args))
        <+> char ':' <+> space
        <+> doc_of_term h.res_tp ()
end








module Value =
struct
    type t = term
end







module Final =
struct
    type t =
        | Empty
        | Term of term
end











module GE =
struct
    include Generic_elaborator.Make (Hole) (Value) (Tracer) (Final) (Error)

    let create_hole (h: Hole.t): int t =
        let open Pretty in
        let* id = create_hole h in
        let* _  = trace
            (fun () ->
                text (sprintf "?%d" id)
                <+> space
                <+> Hole.doc h ())
        in
        return id


    let fill_hole (id: int) (t: term): unit t =
        let open Pretty in
        let* _ =
            trace
                (fun _ ->
                     text (sprintf "?%d :=" id)
                     <+> space
                     <+> doc_of_term t ()
                )
        in
        fill_hole id t


    let meta (id: int): term t =
        let* h = get_hole id in
        Gamma.meta id h.res_tp h.gamma |> return
end







open GE





module ListM =
struct
    include Fmlib_std.List.Monadic (GE)
end


module IntM =
struct
    let iter (n: int) (f: int -> unit GE.t): unit t =
        let rec iter i =
            if i = n then
                return ()
            else
                let* _ = f i in
                iter (i + 1)
        in
        iter 0

    let _ = iter
end



module Ast =
struct
    type term = gamma -> int -> unit GE.t
end











(*
    Internal Functions
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)






let rec head_normal (t: term): term t =
    match
        term_of_term t
    with
    | _, Sort _ ->

        return t

    | _, Meta id ->

        begin
            let* v = value_opt id in
            match v with
            | None ->
                return t
            | Some t ->
                head_normal t
        end

    | _, Pi _  ->

        return t




let rec unify (eq: bool) (act: term) (req: term): bool t =
    let* _ =
        trace (fun () ->
            let open Pretty in
            text "unify" <+> space
            <+> doc_of_term act ()
            <+> space <+> text "with" <+> space
            <+> doc_of_term req ()
        )
    in
    let* act_hn = head_normal act in
    let* req_hn = head_normal req in
    match
        term_of_term act_hn,
        term_of_term req_hn
    with
    | (_, Sort s_act), (_, Sort s_req) ->

        return (Sort.unify eq s_act s_req)

    | (_, Meta _), (_, Meta _) ->

        assert false

    | (_, Meta id), _ ->

        flex_rigid eq true id req_hn

    | _,      (_, Meta id) ->

        flex_rigid eq false id act_hn

    | (_, Pi _),  (_, Pi _) ->

        assert false

    | (_, Pi _),  _ ->

        assert false

    | _,  (_, Pi _) ->

        assert false



and flex_rigid (eq: bool) (sub: bool) (id: int) (t: term): bool t =
    (* See Note [Flex Rigid Simple] *)
    let* h   = get_hole id in
    let  gm  = Hole.gamma h
    and  gt  = gamma_of_term t
    in
    if Hole.is_unifiable h && Gamma.is_prefix gt gm then
        let* ok =
            unify false (type_of_term t) (Hole.type_of h)
        in
        if ok then
            let* _ = fill_hole id t in
            return ok
        else
            assert false
    else
        let* v = wait_hole id in
        if eq then
            unify eq v t
        else if sub then
            unify false v t
        else
            unify false t v



let fill_ehole (id: int) (_: range) (t: term): unit t =

    let  tp_act = Gamma.type_of_term t in
    let* h      = get_hole id in
    let  tp_req = h.res_tp in
    let* _      = unify false tp_act tp_req in
    fill_hole id t





let rec zonk_raw: Term.t -> Term.t t = function

    | _, Sort _  as t ->

        return t

    | _, Meta id ->

        map term_of_term (wait_hole id) >>= zonk_raw

    | _, Pi (n, args, (r, s)) ->
        assert (n = Array.length args);
        let args = Array.copy args in
        let* _ =
            IntM.iter
                (Array.length args)
                (fun i ->
                     let (b, ty, s) = args.(i) in
                     let* ty = zonk_raw ty in
                     let* s  = zonk_raw s in
                     args.(i) <-  (b, ty, s);
                     return ()
                )
        in
        let* r = zonk_raw r in
        let* s = zonk_raw s in
        return Term.(pi args (r, s))



let rec zonk:  term -> term t = function
    | Free _ as t ->

        return t

    | Typed (g, t_raw, tp) ->

        let* t_raw = zonk_raw t_raw in
        let* tp    = zonk tp in
        return (Gamma.Typed (g, t_raw, tp))



let error_handler _ _ =
    assert false





let run_ge (m: Final.t GE.t) (state: State.t)
    : (Final.t, Error.t) result * State.t
    =
    let run tracer =
        GE.run error_handler m tracer
    in
    match
        state.tracing
    with
    | Never ->
        let res, tracer  = run Tracer.ignore in
        res, {state with tracer}

    | Always ->
        let res, tracer = run Tracer.empty in
        res, {state with tracer}

    | On_error ->
        let res, tracer = run Tracer.ignore in
        match res with
        | Ok _  ->
            res, {state with tracer}

        | Error _ ->
            let res, tracer = run Tracer.empty in
            res, {state with tracer}





let elab_term (ast: Ast.term) (g: gamma) (id: int): int t =
    spawn (ast g id)








(*
    External Functions
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)


let prop (range: range): Ast.term =
    fun g id ->
    let* _ = trace (fun _ -> Pretty.text "Make Prop")
    in
    fill_ehole id range (Gamma.prop g)



let any (level: int) (range: range): Ast.term =
    fun g id ->
    let* _ = trace (fun _ -> Pretty.text (sprintf "Make (Any %d)" level))
    in
    fill_ehole id range (Gamma.any level g)



let arrow
        (args: Ast.term list)
        (_: int)
        (res: Ast.term)
        (range: range)
    : Ast.term
    (* A -> B -> ... -> R

       is equivalent to

        all (_: A) (_: B) ... : R
    *)
    =
    fun g0 root ->
        let args =
            List.rev args
        in
        let* _ =
            trace
                (fun _ ->
                     Pretty.text
                         (sprintf
                              "Make arrow with %d arguments"
                              (List.length args))
                )
        in
        let* g =
            ListM.fold_left
                (fun arg g ->
                     let* h_id = create_hole (Hole.e_type (Some root) g0) in
                     let* _    = elab_term arg g h_id in
                     let* tp   = wait_hole h_id in
                     let* tp   = zonk tp in (* ??? *)
                     let  g    =
                         Gamma.push_variable
                             Info.Bind.arrow false tp g
                     in
                     return g
                )
                args
                g0
        in
        let* h_id  = create_hole (Hole.e_type (Some root) g0) in
        let* _     = elab_term res g h_id in
        let* res   = wait_hole h_id in
        let* res   = zonk res in        (* ??? *)
        fill_ehole root range (Gamma.make_pi res g g0)






let make_term (t_ast: Ast.term) (state: State.t)
    : (Final.t, Error.t) result * State.t
    =
    run_ge
        (
            let  g  = state.gamma
            in
            let* _  =
                trace
                    (fun _ -> Pretty.(wrap_words "Make top level term"))
            in
            let* id =
                let* tp_id = create_hole (Hole.c_type g) in
                let* tp = meta tp_id in
                create_hole (Hole.e_term None tp g)
            in
            let* _  = spawn (t_ast g id) in
            let* t  = wait_hole id in
            let* t  = zonk t in        (* all metas must be zonked *)
            return (Final.Term t)
        )
        state




(*
    Note [Fill Hole]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Before filling a hole with a term, it has to be checked if the term
    satisfies the requirement.

    The hole contains its requirement:

        - type of the arguments (explicit or implicit)
        - type of the result after applying the arguments

    Steps:

        - If there are arguments, then the type of the term must be a function
        type. The type of the actual arguments have to conform to the formal
        argument types and the term has to be applied to the actual arguments.

        - If the elaborated term is a function type beginning with implicit
        arguments, then it has to be checked if the result type needs implicit
        arguments as well. I.e. how many implicit arguments have to be created
        and feeded as arguments?

        - The term applied to all arguments has to conform to the result type.
*)




(*
    Note [Flex Rigid Simple
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    We have to unify

        ?m ~ t      (or ?m <= t or t <= ?m)

    where ?m has not yet been instantiated.

    Filling of the hole is possible only if:

    - ?m is a unifiable variable

    - t is valid in the context of ?m or below

    - T <= M  where t: T and ?m: M

    If ?m is an elaboration variable or it has been defined in a higher context,
    then we have to wait for ?m and then

        unify true  v t        (if ?m ~ t)

        unify false v t        (if ?m <= t)

        unify false t v        (if t <= ?m)

    where v is the instantiation of ?m
*)
