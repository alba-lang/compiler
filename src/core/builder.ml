(** Build wellformed and welltyped environments and terms. *)


module type ANY = Fmlib_std.Interfaces.ANY



module Name =
struct
    type t =
        | Normal of string
        | Operator of string
end




(**

    Definition of 'Predicate':
    {v
        Predicate: all {u: Uni}: Any u -> Any u
        :=
          \ {u} A := A -> A -> Prop

                                0[]
        uni
        prop                      0   Uni: Top, Prop: Any 0
        push "u" 0 0 0            1[u: Uni]
        any 1 0 0                 1   Any u: Any (u+1)
        push "A" 1 0 1            2[u: Uni, A: Any u]
        push "_" 2 0 0            3[u: Uni, A: Any u, _: A]
        push "_" 3 1 0            4[u: Uni, A: Any u, _: A, _: A]
        pi 4 2 4 1                2   A -> A -> Prop: Any u
        lam 2 2 0 0               0   ..., (\ {u} A := A -> A -> Prop)
                                              : all
                                                  {u: Uni} (A: Any u)
        def_global 0 "Predicate" 2
    v}


    Definition of 'U8':
    {v
        U8: Any 0

        any0 0
        dec_global 0 "U8" 1
    v}
*)
module type BUILDER =
sig
    type _ t

    val prop: unit t
    (** Add the term [Prop: Any 0] to the context [0].

    *)

    val any0: int t -> unit t
    (** Add the term [Any i: Any (i+1)] to the context [0]. *)


    val any: int -> int -> unit t
    (** [any c b t]

        Add the term [Any t] to the context [c] where the universe is the term
        [u] from the context [c - b].
    *)

    val push: string -> int -> int -> unit t
    (** [push name c b t]

        Make a new context based on context [c] with a new bound variable named
        [name] whose type is the term [t] from the context [c - b].
    *)

    val pi: int -> int -> int -> int -> unit t
    (** [pi c n b t]

        Make a product starting from context [c] using [n] arguments with the
        result type [(c - b,t)].
    *)

    val lam: int -> int -> int -> int -> unit t
    (** [lam c n b t]

        Make an abtraction starting from context [c] using [n] arguments with
        the body term [(c - b, t)].
    *)

    val def_global: string -> int -> int
    (** [def_global mod name t]

        Add a global definition to the module [mod] with [name] with the
        definition term [t] in the context [0].
    *)
end
