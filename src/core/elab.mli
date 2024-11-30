open Std

module type ANY = Fmlib_std.Interfaces.ANY


type range =
    Fmlib_parse.Position.range

type 'a located =
    range * 'a



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

    val make:             gamma -> t
    val make_tracing:     gamma -> t
    val make_not_tracing: gamma -> t

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
    val range: term -> range
end


type formal_argument = bool * Name.t located * Ast.term option



val prop: range -> Ast.term

val any:  int -> range -> Ast.term

val arrow: Ast.term list -> int  -> Ast.term -> range ->  Ast.term


val pi: formal_argument list -> formal_argument -> Ast.term -> range -> Ast.term



val make_term: Ast.term -> State.t -> (Final.t, Error.t) result * State.t
