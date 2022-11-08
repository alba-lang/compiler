(** Build wellformed and welltyped environments and terms.

    The builder must be able to build the following core declarations and definitions:


    {v
        String: Any

        Char: Any

        (|>) {A: Any} {P: A -> Any} (a: A) (f: all x: P x): P a :=
            f a

        Predicate (A: Any): Any := A -> Prop

        Predicate: Any -> Any := \ (A: Any): A -> Prop

        Endorelation (A: Any): Any := A -> A -> Prop

        type Nat: Any :=
            zero: Nat
            succ: Nat -> Nat
    v}


    {[
        (* elaborate [String: Any] *)
        let* h =
            let* s = sig_meta in
            let* r = req s in
            hole r
        in
        let* a = any0 in
        let* any = put_hole a h in
        let* d   = declare "String" a in

        let* _   = push_seq d any in
        push_ready any
    ]}

*)


module type ANY = Fmlib_std.Interfaces.ANY

module Array = Fmlib_std.Array


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
*)
module type BUILDER0 =
sig
    (** {1 Types} *)

    type uni  (** Universe variable. *)

    type sign (** Signature of a type. *)


    type term (** A welltyped term i.e. a term with a context and a type. *)


    type req
        (** Requirement of a hole. It consists of a signature and an optional
            type or supertype.

            - Optional type: The hole can only be filled with with a term of
            that type.

            - Optional supertype: The hole can only be filled with a type which
            is a subtype of the supertype.
        *)

    type hole

    type task

    type _ t

    (** {1 Monad} *)

    val return: 'a -> 'a t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
    val ( >>=  ): 'a t -> ('a -> 'b t) -> 'b t


    (** {1 Signatures} *)

    val sign_unknown: sign t (** Make a signature metavariable *)

    val sign_implicit: sign t

    val sign_sort: sign t

    val sign_function: sign -> sign -> sign t
    (** [sig_function a r] Make a function signature with the argument
        signature [a] and the result signature [r].

        Example:
        {[
            sig_function [A, B] [C, D, E]
            -> [[A,B], C, D, E]
        ]}
    *)


    (** {1 Requirements} *)

    val req: sign -> req t

    val req_type: sign -> term -> req t

    val req_super: sign -> term -> req t



    (** {1 Metavariables} *)

    val uni: uni t       (** Make a universe metavariable *)

    val hole: req -> hole t (** Make a metavariable with requirement *)

    val hole_sign: sign -> hole t

    val hole_function: sign -> hole -> hole t

    val hole_of_hole: hole -> hole t (** Hole based of a hole used as required
                                         type *)

    val meta_of_hole: hole -> term t


    (** {1 Making terms} *)

    val prop: term t (** Make the term [Prop: Any 0]. *)

    val any0: term t (** Make the term [Any 0: Any 1]. *)

    val any:  uni -> term t (** Make the term [Any u: Any (u+1)]. *)

    val string: string -> term t (** Make the term ["abcd": String]. *)

    val app: hole -> hole -> term t


    (** {1 Tasks} *)


    val put_into: term -> hole -> task t
    (** Put a term into a hole

        - Make sure that the term satisfies the requirement. This requires
        signature and term unification which might be blocking. Therefore it is
        a task.

        - Put the term into the hole.

    *)

    val push_ready: task -> unit t (** Push a task onto the ready queue. *)

    val push_seq: task -> task -> unit t
    (** [push_seq before t] Make [t] wait for [before]. *)

    val push_seq_list: task list -> unit t

    val push_hole: task -> hole -> unit t (** [push_hole t h] Make [t] wait for hole [h]. *)
end






module Builder (B0: BUILDER0) =
struct
    (* Types *)
    include B0


    (* Helper functions *)
    let unify_hole (_: term) (_: hole): task t =
        assert false

    let unify_argument_type (f: hole) (atp: hole): task t =
        let _ = f,atp in
        assert false





    (* Term building *)

    let any (h: hole): task t =
        (* make the term [Any] and put it into the hole [h] *)
        let* a = any0 in
        let* u = unify_hole a h in
        let* p = put_into a h in (* does 'put_into' do the unification? *)
        let* _ = push_seq u p in
        return p



    let app (h: hole) (f: hole -> task) (a: hole -> task): task t =
        (* holes *)
        let* fh =
            (* hole for 'f' *)
            let* u = sign_unknown in
            hole_function u h
        in
        let* atph =
            (* hole for the type of 'a' *)
            let* s = sign_sort in
            hole_sign s
        in
        let* ah =
            (* hole for 'a' *)
            hole_of_hole atph
        in
        (* tasks *)
        let  a  = a ah in
        let* ua = unify_argument_type fh atph in
        let* fa =
            let* fa = app fh ah in
            let* u = unify_hole fa atph in
            let* p = put_into fa h in
            let* _ = push_seq u p in
            return p
        in
        let* _ = push_seq_list [a; ua; fa] in
        let* _ = push_ready a in
        return (f fh)




    let typed (h: hole) (e: hole -> task) (tp: hole -> task): task t =
        let _ = h, e, tp in
        assert false
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
