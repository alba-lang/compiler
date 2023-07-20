open Std

type t =
    | Sort
    | Global of Name.t * int * int
    | Local  of Name.t * int
    | Unknown
    | Meta of Name.t * int * int
    | App of bool * t * t array         (* operator flag *)
    | Fun of (bool * t) array * t       (* implicit flag *)
