(*  Note [Insertion of Implicit Arguments]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Suppose we have elaborated a term [t] with its type

        t : all {x: A} {y: B}: R

    and want to fill it into a hole.


    Is it necessary to create metavariables [X] and [Y] and fill

        t {X} {Y}: R[X,Y / x,y]

    into the hole?


    Let

        nit: maximum number of implicit actual arguments which can be inserted

        nir: maximum number of implicit arguments the required type needs

    Then the answer is

        nit - nir              or 0 if [nit < nir]



    Computation of [nit]:
    ------------------------------------------------------------

    Compute head normal form of the type of the term [t]

    [nit] is the number of implicit arguments before the first explicit
    argument of before the result type. If the result type has a
    metavariable at its head, we wait for the metavariable. If the
    metavariable is not instantiated we assume that the final result type
    has no implicit arguments.


    Computation of [niw]:
    ------------------------------------------------------------

    [nir] is the number of actual implicit arguments available in the source
    before the first explicit actual argument.

    If there is no explicit actual argument we add to [niw] the number of
    implicit arguments in the head normal form of the required type computed as
    above in the computation of [nit].
*)




open Printf
open Std



module Pretty =
struct
    include Fmlib_pretty.Print
    include Pretty_plus
end

include Base_elab


module Position = Fmlib_parse.Position


module Final =
struct
    type t =
        | Empty
        | Term of term
end





module BE = Base_elab.Make (Final)

open BE


module Unify = Unify.Make (Final)









(*
    Elaboration
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)


let act_args (h: int) (t: term): int array t =
    (* - Collect the actual arguments of the term [t].
       - Add implicits where necessary.
       - Check that the actual types of the arguments conform to the
         required types of the formal arguments.
    *)
    let* h  = get_hole h
    in
    let rec args i ty arglst tasklst =
        let* tyhn = head_normal ty in
        match term_of_term tyhn |> snd with
        | Meta hid  when i < Hole.count_args h ->
            let* ty = wait_hole hid in
            args i ty arglst tasklst

        | Pi (start, fargs, _) ->
            let nfi = Term.count_implicits start fargs
            and nai = Hole.count_implicits i h
            in
            if nfi > nai then
                (* create nfi - nai holes and insert them to the accu.
                 * make a new ty for the remaining formal arguments and call
                 * args with the new ty.*)
                assert false (* nyi *)
            else if nfi = nai then
                (* insert the next actual argument to the list, unify the actual
                 * type with the required type and call args with the new ty.*)
                assert false (* nyi *)
            else
                (* nfi < nai: there are actual implicit arguments, but there are
                   no corresponding formal implicit arguments. Report an error.
                 *)
                assert false (* nyi *)

        | _ ->
            if i < Hole.count_args h then
                (* merge range with the range of the last given actual argument
                   and report the error "is not a function" *)
                assert false (* nyi *)
            else
                let* () = wait_tasks tasklst  in
                Array.of_list (List.rev arglst) |> return

    in
    args 0 (Gamma.type_of_term t) [] []



let fill_e_hole (range: range) (id: int) (t: term): unit t=
    let* args = act_args id t
    in
    let* args = ArrayM.map wait_hole args
    in
    let  fa =
        if Array.length args = 0 then
            t
        else
            Gamma.application t args
    in
    let fa_ty = type_of_term fa
    in
    let* _ = Unify.into_hole range fa_ty id in
    let* fa_ty = zonk fa_ty in
    fill_hole id (Gamma.update_type fa fa_ty)


(* Note [Filling with Implicit Arguments]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    If
        - The actual type starts with n implicit arguments
        - The required type starts with m fewer implicit arguments
    Then
        - add m implicit actual arguments.

   The required type can start with explicitly provided implicit arguments or
   the required result type can start with some implicit arguments.

   If there are no explicit actual arguments, then the result type has to be
   inspected. We need the head normal form where the head of the base term in
   the result is not a metavariable.

   The hole has a signature [a0 a1 ...] r. We have to insert some implicit
   arguments into the argument list. All arguments are a hole which can either
   be filled by an elaborator (explicitly provided arguments) or by unification
   (implicit arguments not explicitly provided).

   We wait for all argument holes, construct the term [f a0 a1 ...], unify its
   type with the required type r and then fill the hole with [f a0 a1 ...].
*)




module Ast =
struct
    type term = (gamma -> int -> unit BE.t) located

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





let create_untyped_e_hole
        (range: range)
        (tag_term: string)
        (tag_tp:   string)
        (g: gamma)
    : int t
    =
    let* htp =
        create_hole
            Hole.(make_c_type
                      tag_tp
                      (Infer_type ("this expression.", range))
                      g
                 )
    in
    let* tp = meta htp in
    create_hole
        Hole.(make_e_term tag_term tp)





let prop (range: range): Ast.term =
    range
    ,
    fun g id ->
    let* _ = trace (fun _ -> Pretty.text ">>> Make Prop <<<")
    in
    fill_e_hole range id (Gamma.prop g)






let any (level: int) (range: range): Ast.term =
    range
    ,
    fun g id ->
    let* _ = trace (fun _ -> Pretty.text (sprintf ">>> Make (Any %d) <<<" level))
    in
    fill_e_hole range id (Gamma.any level g)






let name (n: Name.t) (range: range): Ast.term =
    range
    ,
    fun g id ->
        let* _ = trace (fun _ ->
            Pretty.text (sprintf ">>> Make (%s) <<<" (Name.string n)))
        in
        match Gamma.find_local n g with
        | Some i ->
            fill_e_hole
                range
                id
                (Gamma.var i g)
        | None ->
            Error.make
                range
                "name not found"
                Pretty.((wrap_words "I cannot find this identifier."))
            |> fail






let var (i: int) (range: range): Ast.term =
    assert (0 <= i);
    range
    ,
    fun g id ->
        let* _ = trace_doc (Pretty.text (sprintf ">>> Make var %d <<<" i))
    in
        if Gamma.length g <= i then
            Error.make
                range
                "Not a valid De Bruijn index."
                Pretty.(text "Not a valid De Bruijn index.")
            |> fail
        else
            fill_e_hole range id (Gamma.var i g)






let annotated (t: Ast.term) (tp: Ast.term) (range: range): Ast.term =
    (* term: Type *)
    range
    ,
    fun g id ->
        let* _ = trace_doc (Pretty.text ">>> Make annotated <<<")
    in
        let* htp = create_hole
            Hole.(make_e_type "type of annotated" g)
        in
        let* mtp = meta htp
        in
        let* ht = create_hole
            Hole.(make_e_term "term of annotated" mtp)
        in
        let* _ = elab_term t  g ht  in
        let* _ = elab_term tp g htp in
        let* _ = Unify.into_hole range mtp id in
        let* t = wait_hole ht in
        let* tp = wait_hole htp in
        let  t_an = Gamma.make_annotated t tp in
        fill_hole id t_an








let app1
        (f: Ast.term)
        (impl, a: bool * Ast.term)  (* implicit? *)
        (range: range)
    : Ast.term
    =
    range,
    fun g h ->
        let* _ = trace_doc (Pretty.text ">>> Make application <<<")
        in
        let* ha = create_untyped_e_hole
            (Ast.range a)
            "actual argument type"
            "actual argument"
            g
        in
        let* ()  = update_hole h Hole.(push_arg impl ha) in
        let* _   = elab_term a g ha in
        let* _   = elab_term f g h  in
        return ()



let app
        (f: Ast.term)
        (args: (bool * Ast.term) list)
        (arg: (bool * Ast.term))
        (_: range)
    : Ast.term
    =
    let rec aux = function
        | [] ->
            assert false (* cannot happen *)

        | [_, a as arg] ->
            let r = Position.merge (Ast.range f) (Ast.range a) in
            app1 f arg r

        | (_, a as arg) :: prefix ->
            let f = aux prefix in
            let r = Position.merge (Ast.range f) (Ast.range a) in
            app1 f arg r
    in
    aux (arg :: args)






let pi1
        (b: Info.Bind.t)
        (nrange: range) (* of name *)
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
            create_hole
                Hole.(make_c_type
                          "pi unknown argument type"
                          (Infer_type ("the variable.", nrange))
                          g0)

        | Some ty ->
            let* hty =
                create_hole
                    (Hole.make_e_type "pi argument type" g0)
            in
            let* _   = elab_term ty g0 hty in
            return hty
    in
    let* tp   = meta hty in
    let  g    = Gamma.push_variable b true tp g0 in
    let* hrtp =
        create_hole
            (Hole.make_e_type "pi result type" g)
    in
    let* _    = elab_term rtp g hrtp in
    let make tp rtp =
        fill_e_hole range par_id (Gamma.make_pi1 b tp rtp)
    in
    wait_one_of_holes
        (
            hrtp, fun rtp ->
                let* tp = wait_hole hty in
                make tp rtp
        )
        [(
            hty, fun tp ->
                let* rtp = wait_hole hrtp in
                make tp rtp
        )]




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
    run
        (
            let  g  = State.gamma state
            in
            let* _  =
                trace
                    (fun _ ->
                         Pretty.(text ">>> Make top level term <<<"))
            in
            let* id = create_untyped_e_hole
                (Ast.range t_ast)
                "top level term type"
                "top level term"
                g
            in
            let* _  = elab_term t_ast g id in
            let* t  = wait_hole id in
            (*let* t  = zonk t in        (* all metas must be zonked *)*)
            return (Final.Term t)
        )
        state
