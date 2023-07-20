open Intf


type globals

type gamma

type term

type req




val is_valid_term: term -> gamma -> bool

val is_valid_req:  req  -> gamma -> bool

val make_globals: unit -> globals






module Make (M: MONAD):
sig
    val empty_gamma: globals -> gamma M.t

    val type_requirement: gamma -> req M.t

    val check: term -> req -> gamma -> term option M.t

    val any: gamma -> term M.t
end
