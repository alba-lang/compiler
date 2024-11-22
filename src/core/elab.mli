open Std

module type ANY = Fmlib_std.Interfaces.ANY


type range =
    Fmlib_parse.Position.range

type term
type gamma
type globals

val gamma_of_term: term -> gamma
val term_of_term:  term -> Term.t
val type_of_term:  term -> term

val make_globals: unit -> globals
val make_gamma:   globals -> gamma

val doc_of_term: term  -> unit -> Fmlib_pretty.Print.doc


module State:
sig
    type t

    val make:          gamma -> t
    val make_trace:    gamma -> t
    val make_no_trace: gamma -> t

    val tracer: t -> Tracer.t
end



module Final:
sig
    type t =
        | Empty
        | Term of term
end




module Ast:
sig
    type term
end





val prop: range -> Ast.term

val any:  range -> int -> Ast.term

val make_term: Ast.term -> State.t -> (Final.t, Error.t) result * State.t
