module type ELAB_MONAD =
sig
    type range
    type _ t
    val return: 'a -> 'a t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
    val ( >>=  ): 'a t -> ('a -> 'b t) -> 'b t
end


module Basic =
struct
    type term =
        (* gamma |- t : T : s *)
        Gamma.t * Term.t * Term.t * Term.t

    type gamma

    type goal (* A term with a metavariable as its head and a requirement *)
end


module Make (EM: ELAB_MONAD) =
struct
end
