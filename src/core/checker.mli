open Std
open Intf

type globals

type gamma

type term

type req



val gamma_length: gamma -> int

val is_gamma_empty: gamma -> bool

val is_valid_term: term -> gamma -> bool

val is_valid_req:  req  -> gamma -> bool

val make_globals: unit -> globals






module Make (M: CHECKER_MONAD):
sig
    val empty_gamma: globals -> gamma M.t

    val type_requirement: gamma -> req M.t

    val check: term -> req -> gamma -> term option M.t

    val any: gamma -> term M.t


    val arrow: term -> term -> gamma -> term M.t

    val find: Name.t -> req -> gamma -> term option M.t

    val push_variable: bool -> Name.t -> term -> gamma -> gamma M.t

    val make_pi: int -> term -> gamma -> (term * gamma) M.t

    val add_definition:
        Name.t -> term -> term option -> gamma
        -> (globals, term) result M.t
end
