open Std

module Pretty   = Fmlib_pretty.Print
module Position = Fmlib_parse.Position


type range      = Position.range
type 'a located = range * 'a


type error = Error.t

type term

type universe_term
type formal_argument

type t


let range_of_semantic (_: error): Position.range =
    assert false

let doc_of_semantic (_: error): Pretty.doc =
    assert false


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





let any (_: range) (ut: universe_term option): term =
    assert (ut = None);
    assert false




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


let apply (_: term) (_: (bool * term) list): term =
    assert false






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
        (_: term) (_: range) (name: Name.t) (_: term)
    : term
    =
    assert (Name.is_operator name);
    if Name.is_arrow name then
        assert false
    else
        assert false




let formal_argument_simple (_: range) (_: Name.t): formal_argument =
    assert false





let formal_argument
        (_: bool)                 (* implicit? *)
        (names: Name.t located list)     (* nonempty group of variables *)
        (_: term option)                (* type of the group *)
    : formal_argument
    =
    assert (names <> []);
    assert false





let product_expression
        (_: Position.t)                 (* start of 'all' *)
        (_: formal_argument list)
        (_: term)        (* result type *)
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
        (_: Name.t located)
        (_: formal_argument list)         (* possibly empty *)
        (_: term option)                    (* result type    *)
        (_: term option)                    (* body           *)
        (_: t)
    : (t, error) result
    =
    assert false



let make (): t =
    assert false
