module Error =
struct
    type t
end



module type STATE =
sig
    type meta
    type term
    type gamma
    type error

    type 'a res = ('a, error) result

    type 'a t

    val set_result: 'a res -> 'a t -> unit

    val get_meta: meta -> 'a t -> term option

    val wait_meta: meta -> (term -> 'a t -> unit) -> 'a t -> unit

    val spawn: ('a t -> unit) -> 'a t -> unit

    val execute: 'a t -> 'a res
end


module type MON =
sig
    type meta
    type term
    type error
    type 'a state
    type 'a res = ('a, error) result
    type ('a, 'z) t

    val run: ('a, 'a) t -> 'a state -> 'a res

    val return: 'a -> ('a, 'z) t
    val fail: error -> ('a, 'z) t
    val (>>=):    ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t
    val ( let* ): ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t

    val spawn: (unit, 'z) t -> 'a -> ('a, 'z) t
    val get_meta: meta -> (term -> ('a, 'z) t) -> ('a, 'z) t
end


module State
    (* mutable *)
        : STATE with type error := Error.t
=
struct
    type meta
    type term
    type gamma
    type 'a res = ('a, Error.t) result

    type 'a t = {
        mutable res:   ('a, Error.t) result option;
        mutable ready: ('a t -> unit) list;
    }

    let blocking_error (_: 'a t): Error.t =
        assert false

    let rec execute (s: 'a t): 'a res =
        match s.res,  s.ready with
        | None, [] ->
            Error (blocking_error s)
        | None, f :: ready ->
            s.ready <- ready;
            f s;
            execute s
        | Some res, _ ->
            res


    let set_result (res: 'a res) (s: 'a t): unit =
        assert (s.res = None);
        s.res <- Some res


    let spawn (f: 'a t -> unit) (s: 'a t): unit =
        s.ready <- f :: s.ready


    let get_meta (_: meta) (_: 'a t): term option =
        (* Get the term associated with the metavariable, if the metavariable
         * has been instantiated. Otherwise return [None]. *)
        assert false


    let wait_meta (_: meta) (_: term -> 'a t -> unit) (_: 'a t): unit =
        (* If [meta] is instantiated then do the action [f term s].
         * Otherwise push [f] onto the wait queue for [meta]. *)
        assert false
end



module Mon (State: STATE)
    : MON
    with type meta  := State.meta
     and type term  := State.term
     and type error := State.error
     and type 'a state := 'a State.t
=
struct
    type 'a res = ('a, State.error) result
    type error = State.error

    type ('a, 'z) t =
        'z State.t -> ('a res -> 'z State.t -> unit) -> unit

    let return (type a z) (a: a): (a, z) t =
        fun s k ->
            k (Ok a) s

    let fail (e: error): ('a, 'z) t =
        fun s k -> k (Error e) s


    let (>>=) (m: ('a, 'z) t) (f: 'a -> ('b, 'z) t): ('b, 'z) t =
        fun s k ->
        m s (fun res s ->
            match res with
            | Ok a          -> f a s k
            | Error _ as e  -> k e s)

    let ( let* ) = (>>=)

    let init (type a) (m: (a, a) t) (s: a State.t): unit =
        m s State.set_result

    let run (m: ('a, 'a) t) (s: 'a State.t): 'a res =
        init m s;
        State.(execute s)


    let get_meta (meta: State.meta) (f: State.term -> ('a, 'z) t): ('a, 'z) t =
        fun s k ->
        match State.get_meta meta s with
        | None ->
            State.wait_meta meta (fun term s -> f term s k) s
        | Some term ->
            f term s k


    let spawn (m: (unit, 'z) t) (a: 'a) : ('a, 'z) t =
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
end
