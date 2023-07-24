open Std

type t


val length: t -> int


val index:  t -> int


val empty: int -> Globals.t -> t


val find_local: Name.t -> t -> Term.t option


val find_global: Name.t -> t -> (int * int) list


val globals: t -> Globals.t
