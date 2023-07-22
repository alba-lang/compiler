open Std
open Core

module Position = Fmlib_parse.Position
module Pretty   = Fmlib_pretty.Print


module M   = Monad.Make (struct type t = Checker.globals end)

module Ec  = Checker
module Ecm = Checker.Make (M)

module Listm = Fmlib_std.List.Monadic (M)


type range = Position.range
type 'a located = range * 'a


let ( let* ) = M.( let* )

let return = M.return





(* Types
   ================================================================================
 *)

type termf =
    Ec.gamma -> Ec.req -> Ec.term M.t


type term =
    termf located


type formal_argument =
    Ec.gamma -> Ec.gamma M.t


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
            Pretty.(text "<" <+> text s <+> text "> " <+>
                    text "is not yet implemented")
    )




let make_type ((_, f): term) (gamma: Ec.gamma): Ec.term M.t =
    M.(
        let* req = Ecm.type_requirement gamma in
        f gamma req
    )





let check_ec_term
        (range: range)
        (t:     Ec.term)
        (req:   Ec.req)
        (gamma: Ec.gamma)
    : Ec.term M.t
    =
    let* t = Ecm.check t req gamma in
    match t with
    | Some t ->
        return t
    | _ ->
        M.fail (Error.make
                      range
                      "expression has illegal type"
                      (assert false))




let check_term
        (range: range)
        (f: Ec.gamma -> Ec.term M.t)
    : term
    =
    let f gamma req =
        let* t = f gamma in
        check_ec_term range t req gamma
    in
    range, f








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
        check_term range Ecm.any





let name_term (_: range) (_: Name.t): term =
    assert false


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


let apply (_: term) (_: bool) (_: term): term =
    assert false






let parens_term (_: Position.t) (_: Position.t) (term: term): term =
    term



let implicit_argument (_: Position.t) (_: Position.t) (_: term): term =
    assert false



let unary_expression
        (_: range) (_: string) (_: Precedence.t) (_: term)
    : term
    =
    assert false




let binary_expression
        (_: term) (_: range) (_: string) (_: Precedence.t) (_: term)
    : term
    =
    assert false




let formal_argument_simple (_: range) (_: Name.t): formal_argument =
    assert false



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
                assert false (* nyi *)
            | Some tp ->
                make_type tp g
        in
        Listm.fold_left
            (fun (_, n) g ->
                 Ecm.push_variable implicit n tp g
            )
            names
            g




let product_expression
        (_: Position.t)                 (* start of 'all' *)
        (_: formal_argument list)
        (_: term)                       (* result type *)
    : term
    =
    assert false




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
    assert (fargs = []); (* nyi *)

    let mon =
        let open M in

        (* Create an empty context *)
        let* gamma =
            Ecm.empty_gamma elab.globals
        in

        (* Push formal arguments into the context *)
        let* gamma =
            Listm.fold_left
                (fun farg g -> farg g)
                fargs
                gamma
        in
        let nargs = Ec.gamma_length gamma
        in

        (* Elaborate the result type and the body *)
        match rtp, bdy with
        | None, None ->
            assert false    (* cannot happen, must be syntax error *)

        | Some ((range, _) as tp), None ->
            let* tp        = make_type tp gamma in
            let* tp, gamma = Ecm.make_pi nargs tp gamma in
            let* res       = Ecm.add_definition name tp None gamma in
            begin
                match res with
                | Ok globals ->
                    return globals
                | Error _ ->
                    M.fail (
                        Error.make range "ambiguous definition" Pretty.empty
                    )
            end

        | None,   Some (range, _) ->
            nyi range "Definition without result type"

        | Some (range, _), Some _ ->
            nyi range "Definition with result type"
    in
    Result.map
        (fun globals -> {globals})
        (M.run mon (State.make ()))



let make (): t =
    {globals = Checker.make_globals ()}
