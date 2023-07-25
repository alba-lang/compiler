(** Type Signature *)


(**

   Types of globals do no contain metavariables.

   Two globals with the same name must have different type signatures. Two
   signatures are different if

   - they have a different number of explicit arguments
   - they have the same number of arguments and either the result type or one of
   the argument types is different.

   If there are more than global entries with the same name and the same number
   of explicit arguments, then there exist a set of positions (argument number
   or result type) which can be decisive.

   The task of the elaborator if there are multiple entries with the same name
   and the same number of explicit arguments: Try to elaborate the decisive
   positions until the disambiguation can be made.

   This is needed only if the required signature contains metavariables. If
   there are no metavariables, then the decision is immediate.
*)


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



val sort: t


val equivalent: t -> t -> bool
(** [equivalent a b] Are the signatures [a] and [b] equivalent?

    Precondition: Neither [a] nor [b] has metavariables.

    An unknown is equivalent with any other signature. Implicit arguments are
    ignored. The terms [Sort] and [Global] have to be the same.
*)


val decide: t -> ('a * t) list -> 'a decision
