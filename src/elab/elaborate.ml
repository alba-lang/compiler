open Std


module Position = Fmlib_parse.Position


type range      = Position.range
type 'a located = range * 'a



module type MONAD_0 =
sig
    type _ t

    val return: 'a -> 'a t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
    val ( >>=  ): 'a t -> ('a -> 'b t) -> 'b t
    val ( >=>  ): ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)

end




module type MONAD =
sig
    type term           (* gamma |- term: type *)
    type req
    type gamma

    val gamma_of_term: term -> gamma


    type _ t

    val return: 'a -> 'a t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
    val ( >>=  ): 'a t -> ('a -> 'b t) -> 'b t
    val ( >=>  ): ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)


    val unify: range -> term  -> term -> unit t


    val prop: range -> term -> unit t
    (** [prop range target]

        Create a term 'Prop: Any' in the same context as the target term and unify
        the term with the target term.
    *)


    val any:  range -> term -> unit t
    (** [prop range target]

        Create a term 'Any: Top' in the same context as the target term and unify
        the term with the target term.
    *)


    val name: range -> req -> term -> unit t
end





module Make (M: MONAD) =
struct
    module Arraym =
        Array_plus.Make (M)


    type termf =
        (* Target term -> task *)
        M.term  -> unit M.t


    type term = termf located


    type formal_argument


    type universe_term      (* not needed *)


    type error = Error.t

    
    type t


    let ( let* ) = M.( let* )
    let ( >>=  ) = M.( let* )




    (*
    ======================================================================

    Public Functions

    ======================================================================
    *)

    let prop (range: range): term =
        range, M.prop range


    let any (range: range) (_: _) : term =
        range, M.any range



    let name_term (_: range) (_: Name.t): term =
        assert false





    let lambda_expression
            (_: Position.t)                 (* start of lambda *)
            (_: formal_argument list)       (* not emmpty *)
            (_: term option)                (* result type *)
            (_: term)                       (* body *)
        : term
        =
        assert false




    let add_definition
            (_: Name.t located)
            (_: formal_argument list)           (* possibly empty *)
            (_: term option)                    (* result type    *)
            (_: term option)                    (* body           *)
            (_: t)
        : (t, error) result
        =
        assert false
 end
