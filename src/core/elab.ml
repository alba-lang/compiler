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


let fill_ehole (id: int) (t: term): unit t =
    let* tp_act =
        Unify.into_hole
            (Gamma.type_of_term t)
            id
    in
    fill_hole id (Gamma.update_type t tp_act)




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



(*
let elab_term_wait (ast: Ast.term) (g: gamma) (id: int): term t =
    let* _ = elab_term ast g id in
    wait_hole id
*)







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



let annotated (t: Ast.term) (tp: Ast.term) (range: range): Ast.term =
    (* term: Type *)
    range
    ,
    fun g id ->
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
        let* _ = Unify.into_hole mtp id in
        let* t = wait_hole ht in
        let* tp = wait_hole htp in
        let  t_an = Gamma.make_annotated t tp in
        fill_hole id t_an



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
        fill_ehole par_id (Gamma.make_pi1 b tp rtp)
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
(*
    let* rtp  = elab_term_wait rtp g hrtp in
    let* tp   = wait_hole hty in
    fill_ehole par_id (Gamma.make_pi1 b tp rtp)
*)




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
            let* id =
                let* tp_id =
                    create_hole
                        Hole.(make_c_type
                                  "top level term type"
                                  (Infer_type
                                       ("this expression.", Ast.range t_ast))
                                  g)
                in
                let* tp = meta tp_id in
                create_hole
                    (Hole.make_e_term "top level term" tp)
            in
            let* _  = elab_term t_ast g id in
            let* t  = wait_hole id in
            (*let* t  = zonk t in        (* all metas must be zonked *)*)
            return (Final.Term t)
        )
        state
