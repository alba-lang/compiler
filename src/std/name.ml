type info =
    | Normal
    | Operator of Precedence.t
    | BracketEmpty
    | ParenEmpty
    | Meta
    | Wildcard

type t = info * string


let compare ((_, s1): t) ((_, s2): t): int =
    String.compare s1 s2


let is_wildcard ((i, _): t): bool =
    i = Wildcard


let is_operator ((i, _): t): bool =
    match i with
    | Operator _ -> true
    | _          -> false




let is_arrow ((_, s): t): bool =
    s = "->"



let string ((_, s): t): string =
    s



let normal str = Normal, str


let operator str =
    Operator (Precedence.of_string str), str


let operator_with_precedence prec str =
    Operator prec, str


let bracket_empty str =
    BracketEmpty, str


let paren_empty str =
    ParenEmpty, str


let meta str =
    Meta, str


let wildcard =
    Wildcard, "_"
