(** Build wellformed and welltyped environments and terms. *)


module type ANY = Fmlib_std.Interfaces.ANY


module type DECIDABLE =
sig
    type t
    val equal: t -> t -> bool
end


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


    Challenging example (universes ommitted):

    Given in the global context:
    {v
        (+): Nat -> Nat -> Nat
        (+): String -> String -> String
        (|>) {A: Any} {P: A -> Any} (a: A) (f: all x: P x): P a
        :=
            f a
    v}

    How to build?
    {v
        (1 |> (+)) 2: Nat
    v}
    Cannot build [1] and [2] because could be [Nat], [U8], ...

    Can build [|>].
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






module Signature (A: DECIDABLE) =
struct
    type t =
        | Const of A.t          (* Global symbols *)
        | Implicit
        | Meta of int           (* Unknown *)
        | App of t * t          (* Function [A -> B] *)


    let rec of_list: t list -> t = function
        | [t] ->
            t
        | h :: tl ->
            App (h, of_list tl)
        | _ ->
            assert false (* empty list not allowed *)

    let rec equal( t: t) (u: t): bool =
        match t, u with
        | Const a, Const b ->
            A.(equal a b)
        | Implicit, Implicit ->
            true
        | Meta i, Meta j ->
            i = j
        | App (a1, b1), App (a2, b2) ->
            equal a1 a2 && equal b1 b2
        | _ ->
            false

    let rec le (t: t) (u: t): bool =
        match t, u with
        | Const a, Const b ->
            A.(equal a b)
        | Meta i, Meta j ->
            i = j
        | App (Implicit, t), u ->
            le t u
        | App (a1, b1), App (a2, b2) ->
            equal a1 a2 && le b1 b2
        | _ ->
            false
end



(* Test signature *)
(*----------------*)

module Sig = Signature (Char)


let%test _ =
    let open Sig in
    let a = of_list [Const 'a'; Const 'b'] in
    equal a a


let%test _ =
    let open Sig in
    let lst = [Const 'a'; App (Const 'b', Const 'c'); Const 'd'] in
    let a = of_list lst
    and b = of_list (Implicit :: Implicit :: lst)
    in
    not (le a b) && le b a
