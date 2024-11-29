open Printf
open Std


module type ANY = Fmlib_std.Interfaces.ANY





module Pretty =
struct
    include Fmlib_pretty.Print

    let indent (d: doc): doc =
        nest 4 d



    let description (d1: doc) (d2: doc): doc =
        d1 <+> space <+> indent d2 |> group



    let descriptions (ds: (doc * doc) list): doc =
        List.map
            (fun (d1, d2) ->
                 d1
                 <+> space
                 <+> (indent d2)
                 |> group
            )
            ds
      |> separated_by space
      |> group
end





module Position = Fmlib_parse.Position

type range = Position.range

type 'a located = range * 'a


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
        | Elab
        | Constraint

    type t = {
        parent: int option; (* parent hole *)
        reason: string;
        info: info;
        args: (term * bool) array;
        res_tp: term;
    }

    let e_type (parent: int option) (reason: string) (gamma: gamma): t =
        (* Create a type hole for an elaborated type (i.e. present in the source
           code). *)
        {
            parent;
            reason;
            info = Elab;
            args = [||];
            res_tp = Gamma.top 0 gamma;
        }


    let e_term (parent: int option) (reason: string) (res_tp: term): t =
        {
            parent;
            reason;
            info = Elab;
            args = [||];
            res_tp
        }


    let c_type (parent: int option) (reason: string) gamma =
        {
            parent;
            reason;
            info = Constraint;
            args = [||];
            res_tp = Gamma.top 0 gamma;
        }


    let is_unifiable (h: t): bool =
        match h.info with
        | Elab        -> false
        | Constraint  -> true


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
        text
            (sprintf "%s for %s"
                 (if is_unifiable h then "c-hole" else "e-hole")
                 h.reason)
        <+> cat (List.map arg (Array.to_list h.args))
        <+> char ':'
        <+> space
        <+> (doc_of_term h.res_tp () |> nest 2)
        |> group
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



    let trace_doc (d: Pretty.doc): unit t =
        trace (fun _ -> d)



    let create_hole (h: Hole.t): int t =
        let open Pretty in
        let* id = create_hole h in
        let* _  = trace
            (fun () ->
                description
                    (sprintf "Create ?%d" id |> text)
                    (Hole.doc h ())
            )
        in
        return id


    let fill_hole (id: int) (t: term): unit t =
        let open Pretty in
        let* _ =
            trace
                (fun _ ->
                     description
                         (sprintf "Fill ?%d :=" id |> text)
                         (doc_of_term t ())
                )
        in
        fill_hole id t


    let wait_hole (id: int): Value.t t =
        let open Pretty in
        let* _ =
            trace
                (fun _ -> text (sprintf "Wait ?%d" id))
        in
        let* t = wait_hole id in
        let* () =
            trace (
                fun () ->
                    description
                        (text "Got")
                        (description
                             (text (sprintf "?%d =" id))
                             (doc_of_term t ()))
            )
        in
        return t



    let meta (id: int): term t =
        let* h = get_hole id in
        Gamma.meta id h.res_tp |> return
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












(*
    Unification
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)




let rec head_normal (t: term): term t =
    match
        term_of_term t |> snd
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

    | Pi _  ->

        return t









module Unify =
struct
    type quad = {
        parent: int option; (* The ehole of the parent term which requires the
                               unification *)
        hole: int;          (* The hole which has to filled with the unified
                               term [act]. *)
        act: term;
        req: term;
        acthn: term;
        reqhn: term;
    }

    type data = {
        quad:  quad;
        stack: quad list;
    }

    let make_quad (parent: int option) (act: term) (req: term): quad t =
        let* acthn = head_normal act in
        let* reqhn = head_normal req in
        let* hole  =
            create_hole
                (Hole.e_term
                     parent
                     "unified actual"
                     (type_of_term act))
        in
        { parent; hole; act; req; acthn; reqhn }
        |> return



    let make_data (parent: int option) (act: term) (req: term): data t =
        let* quad = make_quad parent act req
        in
        { quad; stack = [] }
        |> return


    let make_next_data (act: term) (req: term) data : data t =
        let* quad = make_quad data.quad.parent act req in
        { quad; stack = quad :: data.stack }
        |> return



    let rec uni data: unit t =
        let quad =
            data.quad
        in
        let* _ =
            trace (fun () ->
                let open Pretty in
                [
                    (text "Unify", doc_of_term quad.act ())
                    ;
                    (text "with", doc_of_term quad.req ())
                ]
                |> descriptions
            )
        in

        match
            term_of_term quad.acthn |> snd,
            term_of_term quad.reqhn |> snd
        with

        | Sort s_act, Sort s_req ->

            if Sort.unify false s_act s_req (* boolean flag makes no sense!!!! *)
            then
                fill_hole quad.hole quad.acthn
            else
                assert false

        | Meta _, Meta _ ->

            assert false

        | Meta id, _ ->

            flex_rigid
                id
                quad.acthn
                quad.reqhn
                data

        | _,      Meta id ->

            flex_rigid
                id
                quad.reqhn
                quad.acthn
                data

        | Pi _,  Pi _ ->

            assert false

        | Pi _,  _ ->

            assert false

        | _,  Pi _ ->

            assert false



    and flex_rigid
            (meta_id: int)
            (flex: term)
            (rigid: term)
            data
        : unit t
        =
        let* hole = get_hole meta_id
        in
        if Hole.is_unifiable hole then
            let* _ =
                (* Make sure that the type of the rigid term
                   satisfies the requirement of the
                   flex term. *)
                let* data_next =
                    make_next_data
                        (type_of_term rigid)
                        (type_of_term flex)
                        data
                in
                uni_wait data_next
            in
            let* _ = fill_flex_with_rigid meta_id rigid in
            let* data_next =
                     make_next_data
                         data.quad.acthn
                         data.quad.reqhn
                         data
            in
            uni_wait_fill
                data.quad.hole
                data_next
        else
            let* _ = wait_hole meta_id in
            let* data_next =
                     make_next_data
                         data.quad.acthn
                         data.quad.reqhn
                         data
            in
            uni_wait_fill
                data.quad.hole
                data_next



    and fill_flex_with_rigid (id: int) (t: term): unit t =
        match term_of_term t |> snd with

        | Sort _ ->

            fill_hole id t

        | Meta _ ->

            assert false (* Illegal call *)

        | Pi _ ->

            assert false (* nyi *)



    and uni_wait (data: data): term t =
        (* Spawn a task to make the unification with [data] and wait for unified
           actual term. *)
        let* _ = spawn (uni data) in
        wait_hole data.quad.hole



    and uni_wait_fill (id: int) (data: data): unit t =
        (* Spawn a task to make the unification with [data], wait for unified
           actual term and fill the term into the hole [id]. *)
        let* t = uni_wait data in
        fill_hole id t





    let unify (parent: int option) (act: term) (req: term): term t =
        (* Unify [act] with [req] and return the unified normal form of [act].
         *)
        let* data = make_data parent act req in
        uni_wait data
end













(*
    Helper Functions
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)


let fill_ehole (id: int) (t: term): unit t =

    let  tp_act = Gamma.type_of_term t in
    let* h      = get_hole id in
    let  tp_req = h.res_tp in
    let* tp_act = Unify.unify None tp_act tp_req in
    fill_hole id (Gamma.update_type t tp_act)





let error_handler
        (n: int)
        (f: int -> (int list * Hole.t * Value.t option))
    : Error.t
    =
    let open Pretty
    in
    let rec holes i  =
        if i = n then
            empty
        else
            match f i with
            | _, _, Some _ ->
                holes (i + 1)

            | _, h, None ->
                if Hole.is_unifiable h then
                    sprintf " %d" i |> text
                    <+> holes (i + 1)
                else
                    holes (i + 1)
    in
    Error.make
       Fmlib_parse.Position.(start, start)
       "Holes  not elaborated"
       (
           sprintf "Holes %d Not elaborated " n |> text
           <+> (holes 0) <+> cut
       )





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
    Elaboration
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)



module Ast =
struct
    type term = (gamma -> int -> unit GE.t) located

    let range t =
        fst t
end


type formal_argument = bool * Name.t located * Ast.term option





let elab_term (ast: Ast.term) (g: gamma) (id: int): int t   (* task id *)
    =
    (* Elaborate the term in a parallel task. *)
    let* _ =
        trace_doc Pretty.(sprintf "Elab term ?%d" id |> text)
    in
    spawn ((snd ast) g id)




let elab_term_wait (ast: Ast.term) (g: gamma) (id: int): term t =
    let* _ = elab_term ast g id in
    wait_hole id







let prop (range: range): Ast.term =
    range
    ,
    fun g id ->
    let* _ = trace (fun _ -> Pretty.text ">>> Make Prop <<<")
    in
    fill_ehole id (Gamma.prop g)



let any (level: int) (range: range): Ast.term =
    range
    ,
    fun g id ->
    let* _ = trace (fun _ -> Pretty.text (sprintf ">>> Make (Any %d) <<<" level))
    in
    fill_ehole id (Gamma.any level g)



let pi1
        (b: Info.Bind.t)
        (_: range) (* of name *)
        (ty: Ast.term option)
        (rtp: Ast.term)
        (range: range)
    : Ast.term
    =
    range
    ,
    fun g0 par_id ->
    let* _ = trace (fun _ -> Pretty.text ">>> Make pi <<<")
    in
    let* hty =
        match ty with
        | None ->
            create_hole (Hole.c_type (Some par_id) "pi unknown argument type" g0)

        | Some ty ->
            let* hty =
                create_hole
                    (Hole.e_type (Some par_id) "pi argument type" g0)
            in
            let* _   = elab_term ty g0 hty in
            return hty
    in
    let* tp   = meta hty in
    let  g    = Gamma.push_variable b true tp g0 in
    let* hrtp =
        create_hole
            (Hole.e_type (Some par_id) "pi result type" g)
    in
    let* rtp  = elab_term_wait rtp g hrtp in
    let* tp   = wait_hole hty in
    fill_ehole par_id (Gamma.make_pi1 b tp rtp)




let arrow
        (args: Ast.term list)
        (_: int)
        ((rrange, _ as res): Ast.term)
        (_: range)
    : Ast.term
    (*  A -> B -> ... -> R

        is equivalent to

        all (_: A) (_: B) ... : R
    *)
    =
    let rec aux: _ list -> Ast.term = function
        | [] ->
            res
        | (tyrange, _ as ty) :: args ->
            pi1
                Info.Bind.arrow
                tyrange
                (Some ty)
                (aux args)
                (Position.merge tyrange rrange)
    in
    aux (List.rev args)




let pi
        (args: formal_argument list)
        (arg: formal_argument)
        ((rrange, _ as rtp): Ast.term)
        (_: range)
    : Ast.term
    (*
        all (_: A) (_: B) ... : R
    *)
    =
    (* Rework of ranges needed !!!!!! *)
    let args =
        arg :: args
        |> List.rev_map
            (fun (impl, (r, n), ty) ->
                 r,
                 Info.Bind.make n impl (ty <> None),
                 ty)
    in
    let rec pi_aux: _ list -> Ast.term = function
        | [] ->
            assert false (* cannot happen *)

        | [range, b, ty] ->
            pi1 b range ty rtp (Position.merge range rrange)

        | (range, b, ty) :: args ->
            pi1
                b
                range
                ty
                (pi_aux args)
                (Position.merge range rrange)
    in
    pi_aux args







let make_term (t_ast: Ast.term) (state: State.t)
    : (Final.t, Error.t) result * State.t
    =
    run_ge
        (
            let  g  = state.gamma
            in
            let* _  =
                trace
                    (fun _ ->
                         Pretty.(text ">>> Make top level term <<<"))
            in
            let* id =
                let* tp_id =
                    create_hole
                        (Hole.c_type None "top level term type" g)
                in
                let* tp = meta tp_id in
                create_hole
                    (Hole.e_term None "top level term" tp)
            in
            let* _  = elab_term t_ast g id in
            let* t  = wait_hole id in
            (*let* t  = zonk t in        (* all metas must be zonked *)*)
            return (Final.Term t)
        )
        state
