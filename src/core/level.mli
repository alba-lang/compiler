type t =
    | Var of int
    | Meta of int
    | Succ of t
    | Max of t * t
