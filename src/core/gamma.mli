include Intf.GAMMA
(*
type term
type equiv
type subtype

type t

val index: t -> int

val empty: int -> t

val push_var: int -> Info.Bind.t -> term -> t -> t


val any: int -> t -> term
*)
