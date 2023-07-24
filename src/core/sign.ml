open Std

type 'a decision =
    | Unique of 'a
    | Need   of (Name.t * int * int) list
    | Reject

type t =
    | Sort
    | Global of Name.t * int * int
    | Unknown
    | Meta of Name.t * int * int
    | App of bool * t * t array         (* operator flag *)
    | Fun of (bool * t) array * t       (* implicit flag *)



let sort: t =
    Sort


let decide (_: t) (_: ('a * t) list): 'a decision =
    assert false
