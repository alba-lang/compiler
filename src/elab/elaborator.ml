open Std
open Core

module Position = Fmlib_parse.Position
module Pretty   = Fmlib_pretty.Print


module M   = Elaboration_monad.Make (struct type t = Checker.globals end)

module Checker_m = Checker.Make (M)

module Listm = Fmlib_std.List.Monadic (M)


type range = Position.range
type 'a located = range * 'a


let ( let* ) = M.( let* )

let return = M.return





(* Types
   ================================================================================
 *)

type termf =
    Checker.req -> Checker.gamma -> Checker.term M.t


type term =
    termf located


type formal_argument =
    Checker.gamma -> Checker.gamma M.t


type universe_term


type error = Error.t


type t = {
    globals: Checker.globals;
}










(* Helper functions
   ================================================================================
 *)




let nyi (range: range) (s: string): 'a M.t =
    M.fail (
        Error.make
            range
            "not yet implemented"
            (fun () ->
                 Pretty.(text "<" <+> text s <+> text "> " <+>
                         text "is not yet implemented." <+> cut <+> cut))
    )


let cannot_infer_type (range: range): 'a M.t =
    M.fail ( Error.cannot_infer_type range)

let _ = nyi, cannot_infer_type



let make_type ((_, f): term) (gamma: Checker.gamma): Checker.term M.t =
    M.(
        let* req = Checker_m.type_requirement gamma in
        f req gamma
    )





let check_ec_term
        (range: range)
        (t:     Checker.term)
        (req:   Checker.req)
        (gamma: Checker.gamma)
    : Checker.term M.t
    =
    let* t_opt = Checker_m.check t req gamma in
    match t_opt with
    | Some t ->
        return t
    | _ ->
        M.fail (Error.make
                      range
                      "expression has illegal type"
                      (Checker.Print.not_check t req gamma))




let check_term
        (range: range)
        (f: Checker.gamma -> Checker.term M.t)
    : term
    =
    let f req gamma =
        let* t = f gamma in
        check_ec_term range t req gamma
    in
    range, f





let applyf
        (range: range)
        (rf: range)
        (f: termf)
        (implicit: bool)    (* [f {a}], explicitly given implicit argument *)
        (rarg: range)
        (arg: termf)
    : termf
    =
    let _ = range, rf, f, implicit, rarg, arg in
    fun req g ->
    let _ = g, req in

    (* Make the two metavariables [?a: ?AT] *)
    let* m_at =
        let* at_req =
            Checker_m.type_requirement g
        in
        Checker_m.make_meta
            (Some (Error.cannot_infer_type rarg))
            at_req
            g
    in
    let* a_req =
        Checker_m.requirement_of_type m_at g
    in
    let* m_a =
        Checker_m.make_meta None a_req g
    in

    (* Elaborate the term [a] and fill the corresponding hole in a spawned task.
     *)
    let* _ =
        M.spawn
            (
                let* a = arg a_req g in
                Checker_m.fill_meta m_a a g
            )
    in

    (* Elaborate the term [f] *)
    let* f_term =
        (* Make the requirement for the function term *)
        let* f_req =
            Checker_m.function_requirement implicit m_at req g
        in
        f f_req g
    in

    (* Elaborate [f ?a] *)
    let* fa = Checker_m.apply f_term m_a g
    in

    (* Check that the application satisfies its requirement.

       This extra check is neccessary, because the result type of [f ?a]
       might depend on the argument [a|. The elaboration of [f] does not
       have [?a] available, only its type [?A]. I.e. [f] satisfies the
       requirement of being a function accepting an argument of type
       [?A]. It remains to be checked that [f ?a] satisfies its
       requirement. This check might include the insertion of additional
       implicit arguments.
    *)
    check_ec_term range fa req g




let arrow ((r1,f1): term) ((r2,f2): term): term =
    let range = Position.merge r1 r2
    in
    let f req g =
        let* r = Checker_m.type_requirement g in
        let* t1 = f1 r g in
        let* t2 = f2 r g in
        let* arr = Checker_m.arrow t1 t2 g in
        check_ec_term range arr req g
    in
    range,
    f






(* Functions to satisfy the signature ELABORATOR
   ================================================================================
 *)

let range_of_semantic (e: error): Position.range =
    Error.range e

let doc_of_semantic (e: error): Pretty.doc =
    Error.doc e


module Universe = struct
    let id    _ _: universe_term = assert false
    let fixed _ _: universe_term = assert false
    let max _ _: universe_term = assert false
    let above _ _: universe_term = assert false
    let parens_around _ _ _ = assert false
end





let level (_: range): term =
    assert false





let prop (_: range): term =
    assert false





let any (range: range) (ut: universe_term option): term =
    match ut with
    | Some _ ->
        range, (fun _ _ -> nyi range "universe level")

    | None ->
        check_term range Checker_m.any





let name_term (range: range) (name: Name.t): term =
    let f req g =
        let* t = Checker_m.find name req g in
        match t with
        | None ->
            M.fail (Error.make range "not found" (assert false))
        | Some t ->
            check_ec_term range t req g
    in
    range, f


let string_term (_: range) (_: string): term =
    assert false


let char_term (_: range) (_: string): term =
    assert false



let decimal_term (_: range) (_: string): term =
    assert false



let float_term (_: range) (_: string): term =
    assert false



let tuple_term (_: range) (_: term list): term =
    assert false



let list_term (_: range) (_: term list): term =
    assert false


let apply ((rf, f): term) (implicit: bool) ((rarg, arg): term): term =
    let range = Position.merge rf rarg
    in
    range,
    applyf range rf f implicit rarg arg






let parens_term (_: Position.t) (_: Position.t) (term: term): term =
    term



let implicit_argument (_: Position.t) (_: Position.t) (_: term): term =
    assert false



let unary_expression
        (_: range) (_: Name.t) (_: term)
    : term
    =
    assert false




let binary_expression
        ((r1, t1f) as t1: term) (r_op: range) (name: Name.t) ((r2, t2f) as t2: term)
    : term
    =
    assert (Name.is_operator name);
    if Name.is_arrow name then
        arrow t1 t2
    else
        let range = Position.merge r1 r2 in
        let (_, op)  = name_term r_op name in
        let r_left   = Position.merge r1 r_op in
        let left     = applyf r_left r_op op false r1 t1f in
        let right    = applyf range r_left left false r2 t2f in
        range, right




let formal_argument_simple (range: range) (name: Name.t): formal_argument =
    fun g ->
        let* req =
            Checker_m.type_requirement g
        in
        let* tp =
            Checker_m.make_meta
                (Some (Error.cannot_infer_type range))
                req
                g
        in
        Checker_m.push_variable false false name tp g





let formal_argument
        (implicit: bool)                 (* implicit? *)
        (names: Name.t located list)     (* nonempty group of variables *)
        (tp: term option)                (* type of the group *)
    : formal_argument
    =
    assert (names <> []);
    fun g ->
        let* tp =
            match tp with
            | None ->
                assert false (* nyi: group of variables without type *)
            | Some tp ->
                make_type tp g
        in
        Listm.fold_left
            (fun (_, n) g ->
                 Checker_m.push_variable implicit true n tp g
            )
            names
            g





let product_expression
        (p1: Position.t)                 (* start of 'all' *)
        (fargs: formal_argument list)
        (((_, p2),_) as tp: term)        (* result type *)
    : term
    =
    let range = p1, p2
    in
    range,
    fun req g0 ->
        let* g =
            Listm.fold_left
                (fun farg g -> farg g)
                fargs
                g0
        in
        let* tp  = make_type tp g in
        let* pi  = Checker_m.make_pi tp g g0 in
        check_ec_term range pi req g





let lambda_expression
        (_: Position.t)                 (* start of lambda *)
        (_: formal_argument list)
        (_: term option)                (* result type *)
        (_: term)                       (* body *)
    : term
    =
    assert false




let add_definition
        ((_, name): Name.t located)
        (fargs: formal_argument list)         (* possibly empty *)
        (rtp: term option)                    (* result type    *)
        (bdy: term option)                    (* body           *)
        (elab: t)
    : (t, error) result
    =
    let mon =
        let open M in

        (* Create an empty context *)
        let* g0 =
            Checker_m.empty_gamma elab.globals
        in

        (* Push formal arguments into the context *)
        let* g =
            Listm.fold_left
                (fun farg g -> farg g)
                fargs
                g0
        in

        (* Elaborate the result type and the body *)
        match rtp, bdy with
        | None, None ->
            assert false    (* cannot happen, must be syntax error *)

        | Some ((range, _) as tp), None ->
            let* tp    = make_type tp g in
            let* tp    = Checker_m.make_pi tp g g0 in
            let* res   = Checker_m.add_definition name tp None g in
            begin
                match res with
                | Ok globals ->
                    return globals
                | Error _ ->
                    M.fail (
                        Error.make
                            range
                            "ambiguous definition"
                            (fun () -> Pretty.empty)
                    )
            end

        | None,   Some (_, _) ->
            assert false (* nyi: Definition without result type *)

        | Some (_, _), Some _ ->
            assert false (* nyi: Definition with result type *)
    in
    Result.map
        (fun globals -> {globals})
        (M.run mon (State.make ()))



let make (): t =
    {globals = Checker.make_globals ()}
