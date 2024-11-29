open Std

type t

type term =
    | Free  of t * Sort.t        (* Gamma |- s *)
    | Typed of t * Term.t * term (* Gamma |- t : T *)



val gamma_of_term: term -> t

val term_of_term:  term -> Term.t

val type_of_term:  term -> term


val update_type: term -> term -> term




module Entry:
sig
    type gamma = t

    type t

    val typ:  t -> term

    val info: t -> Info.Bind.t

    val definition: t -> term option

    val gamma: t -> gamma
end


val length: t -> int


val de_bruijn: int -> t -> int


val empty: Globals.t -> t


val entry: int -> t -> Entry.t


val gamma0: t -> t


val is_prefix: t -> t -> bool



val typ: int -> t -> term
(** [typ i g] Type of the [i]th entry. *)


val find_local: Name.t -> t -> int option


val find_global: Name.t -> t -> (int * int) list


val push_variable: Info.Bind.t -> bool -> term  ->  t -> t


val make_pi1: Info.Bind.t -> term -> term -> term
val make_pi: term -> t -> t -> term

val globals: t -> Globals.t


val any:  int -> t -> term
val top:  int -> t -> term
val prop: t -> term

val meta: int -> term -> term (* id, type, gamma *)
