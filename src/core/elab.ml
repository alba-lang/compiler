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
    Print_term.doc (term_of_term t)



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
    type t = {
        uni: bool;      (* can be filled by constraint solving *)
        gamma: gamma;   (* context of the metavariable *)
        args: (term * bool) array;
        res_tp: term;
    }

    let make uni gamma args res_tp = {uni; gamma; args; res_tp}

    let gamma (h: t): gamma =
        h.gamma

    let is_unifiable (h: t): bool =
        h.uni

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
        text (if h.uni then "c-hole" else "e-hole")
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
    Generic_elaborator.Make (Hole) (Value) (Tracer) (Final) (Error)

open GE



module Ast =
struct
    type term = gamma -> int -> unit GE.t
end






(*
    Internal Functions
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)

let create_hole (h: Hole.t): int t =
    let open Pretty in
    let* id = create h in
    let* _  = trace
        (fun () ->
            text (sprintf "?%d" id)
            <+> space
            <+> Hole.doc h ())
    in
    return id


let create_type_hole (uni: bool) (g: gamma): int t =
    create_hole (Hole.make uni g [||] (Gamma.top 0 g))



let create_term_hole (uni: bool) (ty: term) (g: gamma): int t =
    create_hole (Hole.make uni g [||] ty)



let meta (id: int): term t =
    let* h = get id in
    Gamma.meta id h.res_tp h.gamma |> return






let rec head_normal (t: term): term t =
    match
        term_of_term t
    with
    | Sort _ ->

        return t

    | Meta id ->

        begin
            let* v = value_opt id in
            match v with
            | None ->
                return t
            | Some t ->
                head_normal t
        end

    | _ ->

        assert false




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
    | Sort s_act, Sort s_req ->

        return (Sort.unify eq s_act s_req)

    | Meta _, Meta _ ->

        assert false

    | Meta id, _ ->

        flex_rigid eq true id req_hn

    | _,      Meta id ->

        flex_rigid eq false id act_hn

    | _, _ ->

        assert false (* nyi *)



and flex_rigid (eq: bool) (sub: bool) (id: int) (t: term): bool t =
    (* See Note [Flex Rigid Simple] *)
    let* h   = get id in
    let  gm  = Hole.gamma h
    and  gt  = gamma_of_term t
    in
    if Hole.is_unifiable h && Gamma.is_prefix gt gm then
        let* ok =
            unify false (type_of_term t) (type_of_term (Hole.type_of h))
        in
        if ok then
            let* _ = fill id t in
            return ok
        else
            assert false
    else
        let* v = wait id in
        if eq then
            unify eq v t
        else if sub then
            unify false v t
        else
            unify false t v



let fill_hole (id: int) (_: range) (t: term): unit t =

    let  tp_act = Gamma.type_of_term t in
    let* h      = get id in
    let  tp_req = h.res_tp in
    let* _      = unify false tp_act tp_req in
    fill id t



let zonc_raw: Term.t -> Term.t t = function

    | Sort _ | Local _ | Global _ as t ->

        return t


    | Meta id ->

        map term_of_term (wait id)

    | _ ->

        assert false



let rec zonc:  term -> term t = function
    | Free _ as t ->

        return t

    | Typed (g, t_raw, tp) ->

        let* t_raw = zonc_raw t_raw in
        let* tp    = zonc tp in
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





(*
    External Functions
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)



let prop (range: range): Ast.term =
    fun g id ->
    fill_hole id range (Gamma.prop g)



let any (range: range) (level: int): Ast.term =
    fun g id ->
    fill_hole id range (Gamma.any level g)





let make_term (t_ast: Ast.term) (state: State.t)
    : (Final.t, Error.t) result * State.t
    =
    run_ge
        (
            let  g  = state.gamma
            in
            let* id =
                let* tp_id = create_type_hole true g in
                let* tp = meta tp_id in
                create_term_hole false tp g
            in
            let* _  = t_ast g id in
            let* t  = wait id in
            let* t  = zonc t in        (* all metas must be zonked *)
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
