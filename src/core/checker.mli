open Std

type globals

type gamma

type term

type req




type range = Fmlib_parse.Position.range


module type MONAD =
sig
    include Fmlib_std.Interfaces.MONAD
    (** @inline *)

    val map: ('a -> 'b) -> 'a t -> 'b t

    val new_id:      int t
    val new_context: int t

    type meta_reason

    val new_meta: meta_reason -> req -> int -> int t

    val spawn: unit t -> unit t
end







module Print:
sig
    val not_check: term -> req -> gamma -> unit -> Fmlib_pretty.Print.doc
end




val gamma_length: gamma -> int

val is_gamma_empty: gamma -> bool

val is_valid_req: req -> gamma -> bool

val is_valid_term: term -> gamma -> bool

val make_globals: unit -> globals






module Make (M: MONAD):
sig
    val empty_gamma: globals -> gamma M.t

    val type_requirement: gamma -> req M.t


    val requirement_of_type: term -> gamma -> req M.t

    val function_requirement: bool -> term -> req -> gamma -> req M.t

    val make_meta: M.meta_reason -> req -> gamma -> term M.t

    val fill_meta: term -> term -> gamma -> unit M.t


    val check: term -> req -> gamma -> term option M.t

    val any: gamma -> term M.t

    val apply: term -> term -> gamma -> term M.t

    val arrow: term -> term -> gamma -> term M.t

    val find: Name.t -> req -> gamma -> term option M.t


    val push_variable: bool -> bool -> Name.t -> term -> gamma -> gamma M.t
    (** [push_variable implicit with_type name typ g]

    *)


    val make_pi: term -> gamma -> gamma -> term M.t
    (** [make_pi tp g g0]

        Make a product term [all (x: A) (y: B) ... : tp] where [tp] is valid in
        the context [g = g0, (x:A) (y:B) ...].
    *)



    val add_definition:
        Name.t -> term -> term option -> gamma
        -> (globals, term) result M.t
end
