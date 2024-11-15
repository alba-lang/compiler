open Std

type term

type t

module Entry:
sig
    type gamma = t

    type t

    val typ:  t -> Term.pair

    val info: t -> Info.Bind.t

    val definition: t -> Term.t option

    val gamma: t -> gamma
end

val length: t -> int


val de_bruijn: int -> t -> int


val empty: Globals.t -> t


val entry: int -> t -> Entry.t


val is_prefix: t -> t -> bool



val typ: int -> t -> Term.pair
(** [typ i g] Type of the [i]th entry. *)


val find_local: Name.t -> t -> int option


val find_global: Name.t -> t -> (int * int) list


val push_variable: Info.Bind.t -> bool -> Term.pair  ->  t -> t


val globals: t -> Globals.t
