open Std

type globals

type gamma

type term


type range = Fmlib_parse.Position.range



val gamma_length: gamma -> int

val is_gamma_empty: gamma -> bool

val is_valid_term: term -> gamma -> bool

val make_globals: unit -> globals






module Make (M: Intf.CHECKER_MONAD):
sig
    val empty_gamma: globals -> gamma M.t

    val type_requirement: gamma -> Gamma.req M.t

    val make_meta: M.meta_reason -> Gamma.req -> gamma -> term M.t

    val check: term -> Gamma.req -> gamma -> term option M.t

    val any: gamma -> term M.t


    val arrow: term -> term -> gamma -> term M.t

    val find: Name.t -> Gamma.req -> gamma -> term option M.t


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
