open Std


module type ANY = Fmlib_std.Interfaces.ANY

type range =
    Fmlib_parse.Position.range

type 'a located =
    range * 'a


type doc = Fmlib_pretty.Print.doc


type term    = Gamma.term
type gamma   = Gamma.t
type globals = Globals.t

val gamma_of_term: term -> gamma
val term_of_term:  term -> Term.t
val type_of_term:  term -> term

val make_globals: unit -> globals
val make_gamma:   globals -> gamma

val doc_of_term: term -> Fmlib_pretty.Print.doc




module State:
sig
    type t

    val make:             gamma -> t
    val make_tracing:     gamma -> t
    val make_not_tracing: gamma -> t

    val tracer: t -> Tracer.t
    val gamma:  t -> gamma
end



module Make (Final: ANY):
sig
    include Fmlib_std.Interfaces.MONAD

    type 'a hole_callback = int * (term -> 'a t)

    val trace: (unit -> doc) -> unit t
    val trace_doc: doc -> unit t
    val value_opt: int -> term option t
    val get_hole:  int -> Hole.t t
    val create_hole: Hole.t -> int t
    val fill_hole:   int -> term -> unit t
    val wait_hole:   int -> term t
    val wait_one_of_holes: 'a hole_callback -> 'a hole_callback list -> 'a t

    val spawn: unit t -> int t
    val meta: int -> term t

    val run: Final.t t -> State.t -> (Final.t, Error.t) result * State.t
end
