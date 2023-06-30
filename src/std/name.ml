type info =
    | Normal
    | Operator of Precedence.t
    | BracketEmpty
    | ParenEmpty
    | Meta
    | Wildcard

type t = info * string


let normal str = Normal, str

let operator str =
    Operator (Precedence.of_string str), str

let bracket_empty str =
    BracketEmpty, str

let paren_empty str =
    ParenEmpty, str

let meta str =
    Meta, str

let wildcard str =
    Wildcard, str
