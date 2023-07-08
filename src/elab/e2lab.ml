open Fmlib_std
open Std
open Core

module Position = Fmlib_parse.Position

type range = Position.range
type 'a located = range * 'a






module Error =
struct
    type t = {
        rangef: unit -> range;
        tag:    string;
    }

    let make (rangef: unit -> range) (tag: string): t =
        {rangef; tag}
end






module type STATE =
sig
    type error
    type meta
    type term
    type gamma
    type final

    type t

    val set_result: (final, error) result -> t -> unit

    val get_meta: meta -> t -> term option

    val wait_meta: meta -> (term -> t -> unit) -> t -> unit

    val spawn: (t -> unit) -> t -> unit

    val execute: t -> (final, error) result
end






(* Basic monad to be usable for language specific modules like [Gamma] i.e.
 * modules from [Core]
 *)

module type MON =
sig
    type term
    type requirement
    type gamma

    type 'a t
    val return: 'a -> 'a t
    val (>>=): 'a t -> ('a -> 'b t) -> 'b t

    val fresh_id: int t

    val new_gamma: (int -> gamma) -> gamma t
end





module Econtext =
struct
    type gamma
    type term
    type requirement (* required type or signature *)

    type check_result =
        | Checked of term
        | Rejected
        | Maybe


    let empty_gamma (_: int) (_: Globals.t): gamma =
        assert false

    module Monadic
            (M: MON
             with type gamma       = gamma
              and type term        = term
              and type requirement = requirement
            )
    =
    struct
        let type_requirement (_: gamma): requirement M.t =
            M.return (assert false)



        let make_meta (_: requirement) (_: gamma): term M.t =
            (* Make a metavariables which satisfies a requirement *)
            assert false




        let check (_: term) (_: requirement) (_: gamma): check_result M.t =
            assert false

        let prop (_: gamma): term M.t =
            M.return (assert false)

        let any  (_: gamma): term M.t =
            M.return (assert false)


        let push_variable
                (_: bool)
                (_: Name.t)
                (_: term)
                (_: gamma)
            : gamma M.t
            =
            assert false
    end
end





module State
    (* mutable *)
=
struct
    type term        = Econtext.term
    type requirement = Econtext.requirement
    type gamma       = Econtext.gamma

    type t = {
        mutable ready:  (t -> unit) list;
        mutable gammas: gamma array; (* Never empty! *)
        mutable nids:   int;
    }


    let make (globals: Globals.t): t =
        { ready = [];
          gammas =
              [| Econtext.empty_gamma 0 globals |];
          nids = 0;
        }


    let fresh_id (s: t): int =
        let id = s.nids in
        s.nids <- s.nids + 1;
        id

    let is_valid_context (i: int) (s: t): bool =
        i < Array.length s.gammas


    let empty_context (s: t): gamma =
        s.gammas.(0)


    let rec execute (s: t): 'a =
        match s.ready with
        | [] ->
            assert false (* shall never happen *)
        | f :: ready ->
            s.ready <- ready;
            f s;
            execute s


    let make_meta
            (_: requirement)
            (i: int)            (* in context [i] *)
            (s: t)
        : int                   (* the id of the meta in the context *)
        =
        assert (is_valid_context i s);
        assert false



    let is_filled (_: int) (_: int) (_: t): bool =
        assert false


    let fill_term (gam: int) (meta: int) (s: t): term =
        assert (is_filled gam meta s);
        assert false


    let fill_meta
            (_: int)                (* context id *)
            (_: int)                (* id in the context *)
            (_: term)
            (_: t)
        : unit
        =
        assert false


    let spawn (f: t -> unit) (s: t): unit =
        s.ready <- f :: s.ready

    let wait_meta
            (_: int)
            (_: int)
            (_: term -> t -> unit) (_: t -> unit) (_: t)
        : unit
        =
        (* If [meta] is instantiated then do the action [f term s].
         * Otherwise push [f] and the error handler [e] onto the wait queue for
         * [meta]. *)
        assert false
end











module Mon =
struct
    exception Done of Globals.t
    exception Fail of Error.t

    type term        = Econtext.term
    type gamma       = Econtext.gamma
    type requirement = Econtext.requirement



    (* Basic Monad *)

    type 'a t =
        State.t -> ('a -> State.t -> unit) -> unit

    let return (a: 'a): 'a t =
        fun s k ->
            k a s

    let fail (e: Error.t): 'a t =
        (fun _ _ -> raise (Fail e))


    let (>>=) (m: 'a t) (f: 'a -> 'b t): 'b t =
        fun s k ->
        m s (fun a s -> f a s k)

    let ( let* ) = (>>=)


    let map (f: 'a -> 'b) (m: 'a t): 'b t =
        fun s k ->
        m s (fun a s -> k (f a) s)


    let run (m: Globals.t t) (s: State.t): (Globals.t, Error.t) result =
        m s (fun g _ -> raise (Done g));
        try
            let _ = State.execute s in
            assert false
        with Done g -> Ok g
           | Fail e -> Error e


    let fresh_id: int t =
        fun s k ->
        k (State.fresh_id s) s


    let new_gamma (_: int -> gamma): gamma t =
        assert false


    let wait_meta (gam: int) (meta: int) (f: term -> 'a t) (e: 'a t): 'a t =
        fun s k ->
        State.wait_meta gam meta (fun term s -> f term s k) (fun s -> e s k) s


    let next_tick (m: 'a t): 'a t =
        fun s k ->
        State.spawn (fun s -> m s k) s


    let spawn (m: unit t) (a: 'a) : 'a t =
        fun s k ->
        State.spawn
            (fun s ->
                 m s (fun () _ -> ()))
            s;
        k a s



    (* Helper functions *)

    let reduce_list
            (f: 'accu -> 'el -> 'accu t)
            (start: 'el -> 'accu t)
            (lst: 'el list)
        : 'accu t
        =
        match lst with
        | [] ->
            assert false (* illegal call *)
        | el :: lst ->
            let rec go accu = function
                | [] -> return accu
                | el :: lst ->
                    let* accu = f accu el in
                    go accu lst
            in
            let* accu = start el in
            go accu lst


    let fold_left
            (type accu el)
            (f: accu -> el -> accu t)
            (start: accu)
            (lst: el list)
        : accu t
        =
        let rec go accu = function
            | [] -> return accu
            | el :: lst ->
                let* accu = f accu el in
                go accu lst
        in
        go start lst
end







module Elab =
struct
    type 'a with_range =
        (unit -> range) * 'a

    module Ec = Econtext

    module Ecm = Econtext.Monadic (Mon)


    type term =
        (Ec.gamma -> Ec.term Mon.t) with_range

    type formal_argument =
        Econtext.gamma -> Econtext.gamma Mon.t


    type t = Globals.t


    let ( let* ) = Mon.( let* )



    (* Helper Functions *)


    let make_type_meta (gamma: Ec.gamma): Ec.term Mon.t =
        let* req = Ecm.type_requirement gamma in
        Ecm.make_meta req gamma



    let farg_type (gamma: Ec.gamma) ((_, f): term): Ec.term Mon.t =
        let* req = Ecm.type_requirement gamma in
        let* t   = f gamma in
        let* res = Ecm.check t req gamma in
        match res with
        | _ ->
            assert false (* nyi *)


    let range_of_names (names: Name.t located list) (): range =
        match names with
        | [] -> assert false (* Illegal call *)
        | ((pos1, pos2), _) :: names ->
            pos1,
            List.fold_left
                (fun _ ((_, pos2), _) -> pos2)
                pos2
                names








    (* ------------------------------------------------------------*)
    (* Functions to satisfy module type [ELABORATOR]               *)
    (* ------------------------------------------------------------*)



    let prop (range: range): term =
        (fun () -> range),
        Ecm.prop



    let any (range: range): term =
        (* nyi: universe term *)
        (fun () -> range),
        Ecm.any


    let name_term (_: range) (_: Name.t): term =
        assert false


    let application (_: term) (_: term list): term =
        assert false


    let formal_argument_simple
            (_: range)
            ((_, n): Name.t located) (* untyped and explicit *)
        : formal_argument
        =
        fun gamma ->
        let* tp = make_type_meta gamma in
        Ecm.push_variable false n tp gamma



    let formal_argument
            (implicit: bool)
            (names: Name.t located list)
            (tp: term option)
        : formal_argument
        =
        fun gamma ->
        let* tp =
            match tp with
            | None ->
                make_type_meta gamma
            | Some tp ->
                farg_type gamma tp
        in
        let push n gamma =
            Ecm.push_variable implicit n tp gamma
        in
        Mon.(reduce_list
                 (fun gamma (_, n) -> push n gamma)
                 (fun (_, n) -> push n gamma)
                 names)






    let product_expression
            (_: Position.t)
            (_: formal_argument list)
            (_: term) (* result type *)
        : term
        =
        assert false



    let lambda_expression
            (_: Position.t)
            (_: formal_argument list)
            (_: term option)          (* result type *)
            (_: term)                 (* body *)
        : term
        =
        assert false







    let add_definition
            (_: Name.t located)
            (fargs: formal_argument list)
            (tp: term)
            (body: term option)
            (elab: t)
        : (t, Error.t) result
        =
        let state = State.make elab in
        let mon =
            let open Mon in
            let* gamma =
                fold_left
                    (fun gamma f -> f gamma)
                    (State.empty_context state)
                    fargs
            in
            let* _ = farg_type gamma tp in
            match body with
            | None ->
                assert false
            | Some _ ->
                assert false
        in
        Mon.run mon state
end
