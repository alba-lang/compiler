open Std


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

val index:  t -> int


val length: t -> int


val de_bruijn: int -> t -> int


val empty: int -> Globals.t -> t


val entry: int -> t -> Entry.t


val equal: t -> t -> bool


val is_prefix: t -> t -> bool



val typ: int -> t -> Term.pair
(** [typ i g] Type of the [i]th entry. *)


val find_local: Name.t -> t -> int option


val find_global: Name.t -> t -> (int * int) list


val push_variable: Info.Bind.t -> bool -> Term.pair -> int ->  t -> t


val globals: t -> Globals.t
