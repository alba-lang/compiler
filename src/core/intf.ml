module type MON =
sig
    type term
    type gamma
    type 'a t

    val return:   'a -> 'a t
    val (>>=):    'a t -> ('a -> 'b t) -> 'b t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t

    val new_gamma: (int -> gamma) -> gamma t
    val new_hole:  term -> gamma -> int t
    val fill_hole: int -> term -> gamma -> unit t
end




module type GAMMA0 =
sig
    type term
    type fterm
    type equiv
    type subtype

    type fterm_error =
        [`Meta_needed of int * int * (term -> (fterm, fterm_error) result) |`Fatal]

    type t

    val index: t -> int

    val make_fterm: term -> t -> (fterm, fterm_error) result
end





module type GAMMA =
sig
    include GAMMA0

    val empty: int -> t

    val push_var: int -> Info.Bind.t -> term -> t -> t

    val any: int -> t -> term
end
