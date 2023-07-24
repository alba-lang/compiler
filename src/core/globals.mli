open Std


module Entry:
sig
    type t

    val make: Name.t -> Term.t -> Sign.t -> Term.t option -> t

    val name: t -> Name.t

    val typ:  t -> Term.t

    val sign: t -> Sign.t

    val body: t -> Term.t option
end


type t

val make: unit -> t



val entry: int -> int -> t -> Entry.t


val add: Entry.t -> t -> t


val find: Name.t -> t -> (int * int) list
