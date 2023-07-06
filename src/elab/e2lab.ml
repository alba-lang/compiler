open Std
open Core

module Error =
struct
    type t
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


module type MON =
sig
    type error
    type meta
    type term
    type final
    type state
    type 'a res = ('a, error) result

    type 'a t

    val run: final t -> state -> final res

    val return: 'a -> 'a t
    val fail: Error.t -> 'a t
    val (>>=):    'a t -> ('a -> 'b t) -> 'b t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t

    val spawn: unit t -> 'a -> 'a t
    val get_meta: meta -> (term -> 'a t) -> 'a t
end





module Econtext =
struct
    type gamma
    type term
    type t

    let prop (_: gamma): term =
        assert false

    let any (_: gamma): term =
        assert false
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
        mutable gamma:  gamma;
    }

    let get_gamma (s: t): gamma =
        s.gamma

    let set_gamma (g: gamma) (s: t) =
        s.gamma <- g

    let init (_: Globals.t): t =
        assert false

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
    type term  = Econtext.term
    type gamma = Econtext.gamma

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


    let get_gamma: gamma t =
        fun s k -> k (Ok (State.get_gamma s)) s


    let set_gamma (g: gamma): unit t =
        fun s k ->
        State.set_gamma g s;
        k (Ok ()) s


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




    (* Language specific functions *)

    let prop: term t =
        map Econtext.prop get_gamma

    let any: term t =
        map Econtext.any get_gamma
end



module Dummy
        (State: STATE)
        (Mon: MON with type error = State.error)
=
struct
end


module Elab =
struct
    module Position = Fmlib_parse.Position

    type range = Position.range
    type 'a located = range * 'a


    type 'a elab = {
        rangef: unit -> range;
        monad: 'a Mon.t;
    }
    type term = Mon.term elab

    type formal_argument

    type t = Globals.t


    let make_rangef (range: range) (): range =
        range

    let prop (range: range): term =
        {
            rangef = make_rangef range;
            monad  = Mon.prop;
        }

    let any (range: range): term =
        (* nyi: universe term *)
        {
            rangef = make_rangef range;
            monad = assert false;
        }

    let formal_argument_simple
            (_: range)
            (_: Name.t) (* untyped and explicit *)
        : formal_argument
        =
        assert false


    let formal_argument
            (_: bool) (_: Name.t located list) (_: term option)
        : formal_argument
        =
        assert false



    let product_expression
            (_: Position.t)
            (_: formal_argument list)
            (_: term) (* result type *)
        : term
        =
        assert false

    let add_definition
            (_: Name.t located)
            (_: formal_argument list)
            (tp: term)
            (_: term option)
            (elab: t)
        : (t, Error.t) result
        =
        let state = State.init elab in
        let mon =
            let open Mon in
            let* _ = tp.monad in
            assert false
        in
        Mon.run mon state
end
