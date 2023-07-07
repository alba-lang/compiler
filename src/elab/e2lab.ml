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

    val new_gamma: (int -> gamma) -> gamma t
end





module Econtext =
struct
    type gamma
    type term
    type requirement (* required type or signature *)

    let empty_gamma (_: int) (_: Globals.t): gamma =
        assert false


    let top  (_: gamma): term =
        assert false

    let prop (_: gamma): term =
        assert false

    let any (_: gamma): term =
        assert false

    module Monadic
            (M: MON
             with type gamma       = gamma
              and type term        = term
              and type requirement = requirement
            )
    =
    struct
        let prop (_: requirement) (_: gamma): term M.t =
            M.return (assert false)
    end
end





module State
    (* mutable *)
=
struct
    type meta
    type term    = Econtext.term
    type gamma   = Econtext.gamma
    type res = (Globals.t, Error.t) result

    type t = {
        mutable res:    res option;
        mutable ready:  (t -> unit) list;
        mutable globals: Globals.t;
        mutable gammas: gamma array; (* Never empty! *)
    }


    let make (globals: Globals.t): t =
        { res = None;
          ready = [];
          globals;
          gammas =
              [| Econtext.empty_gamma 0 globals |];
        }


    let empty_context (s: t): gamma =
        s.gammas.(0)

    let blocking_error (_: t): Error.t =
        assert false

    let rec execute (s: t): res =
        match s.res,  s.ready with
        | None, [] ->
            Error (blocking_error s)
        | None, f :: ready ->
            s.ready <- ready;
            f s;
            execute s
        | Some res, _ ->
            res


    let set_result (res: res) (s: t): unit =
        assert (s.res = None);
        s.res <- Some res


    let spawn (f: t -> unit) (s: t): unit =
        s.ready <- f :: s.ready


    let get_meta (_: meta) (_: t): term option =
        (* Get the term associated with the metavariable, if the metavariable
         * has been instantiated. Otherwise return [None]. *)
        assert false


    let wait_meta (_: meta) (_: term -> t -> unit) (_: t): unit =
        (* If [meta] is instantiated then do the action [f term s].
         * Otherwise push [f] onto the wait queue for [meta]. *)
        assert false
end



module Mon =
struct
    type term        = Econtext.term
    type gamma       = Econtext.gamma
    type requirement = Econtext.requirement



    (* Basic Monad *)

    type 'a res = ('a, Error.t) result

    type 'a t =
        State.t -> ('a res -> State.t -> unit) -> unit

    let return (a: 'a): 'a t =
        fun s k ->
            k (Ok a) s

    let fail (e: Error.t): 'a t =
        fun s k -> k (Error e) s


    let (>>=) (m: 'a t) (f: 'a -> 'b t): 'b t =
        fun s k ->
        m s (fun res s ->
            match res with
            | Ok a          -> f a s k
            | Error _ as e  -> k e s)

    let ( let* ) = (>>=)


    let map (f: 'a -> 'b) (m: 'a t): 'b t =
        fun s k ->
        m s (fun res s ->
            match res with
            | Ok a         -> k (Ok (f a)) s
            | Error _ as e -> k e s)


    let init (m: 'a t) (s: State.t): unit =
        m s State.set_result

    let run (m: 'a t) (s: State.t): Globals.t res =
        init m s;
        State.(execute s)


    let get_meta (meta: State.meta) (f: term -> 'a t): 'a t =
        fun s k ->
        match State.get_meta meta s with
        | None ->
            State.wait_meta meta (fun term s -> f term s k) s
        | Some term ->
            f term s k


    let spawn (m: unit t) (a: 'a) : 'a t =
        fun s k ->
        State.spawn
            (fun s ->
                 m s (
                     fun res s ->
                         match res with
                         | Ok () -> ()
                         | Error _ as e ->
                             State.set_result e s
                 )
            )
            s;
        k (Ok a) s



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



    (* Language specific functions *)

    let type_requirement (): requirement t =
        assert false

    let top (g: gamma): term t =
        return (Econtext.top g)

    let prop (g: gamma): term t =
        return (Econtext.prop g)

    let any (g: gamma): term t =
        return (Econtext.any g)


    let make_meta (_: term) (_: unit -> Error.t) (_: gamma): term t =
        assert false

    let push_variable (_: bool) (_: Name.t) (_: term) (_: gamma): gamma t =
        assert false
end







module Elab =
struct
    type 'a with_range =
        (unit -> range) * 'a



    type term =
        (Econtext.gamma -> Econtext.term Mon.t) with_range

    type formal_argument =
        Econtext.gamma -> Econtext.gamma Mon.t


    type t = Globals.t


    let ( let* ) = Mon.( let* )



    (* Helper Functions *)


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
        Mon.prop



    let any (range: range): term =
        (* nyi: universe term *)
        (fun () -> range),
        Mon.any


    let formal_argument_simple
            (_: range)
            (_: Name.t) (* untyped and explicit *)
        : formal_argument
        =
        assert false



    let formal_argument
            (implicit: bool)
            (names: Name.t located list)
            (tp: term option)
        : formal_argument
        =
        let f gamma =
            let* tp =
                match tp with
                | None ->
                    let* top = Mon.top gamma
                    in
                    let reason = assert false (* nyi *)
                    in
                    Mon.make_meta top reason gamma
                | Some tp ->
                    (snd tp) gamma
            in
            Mon.(reduce_list
                    (fun gamma (_, n) ->
                        Mon.push_variable implicit n tp gamma)
                    (fun (_, n) ->
                        Mon.push_variable implicit n tp gamma)
                    names)
        in
        f



    let product_expression
            (_: Position.t)
            (_: formal_argument list)
            (_: term) (* result type *)
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
            let* _ = (snd tp) gamma
            in
            match body with
            | None ->
                assert false
            | Some _ ->
                assert false
        in
        Mon.run mon state
end
