open Std

type t


val index:  t -> int


val length: t -> int


val de_bruijn: int -> t -> int


val empty: int -> Globals.t -> t


val typ: int -> t -> Term.t
(** [typ i g] Type of the [i]th entry. *)


val find_local: Name.t -> t -> int option


val find_global: Name.t -> t -> (int * int) list


val push_variable: Info.Bind.t -> bool -> Term.t -> int ->  t -> t


val globals: t -> Globals.t