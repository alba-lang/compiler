module Error =
struct
    type t
end


module State =
    (* mutable *)
struct
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
end



module type STATE =
sig
    type meta
    type term

    type 'a res = ('a, Error.t) result

    type 'a t

    val set_result: 'a res -> 'a t -> unit

    val get_meta: meta -> 'a t -> term option

    val wait_meta: meta -> (term -> 'a t -> unit) -> 'a t -> unit
end



module Mon (State: STATE) =
struct
    type 'a res = ('a, Error.t) result

    type ('a, 'z) t =
        'z State.t -> ('a res -> 'z State.t -> unit) -> unit

    let return (a: 'a): ('a, 'z) t =
        fun s k ->
            k (Ok a) s

    let (>>=) (m: ('a, 'z) t) (f: 'a -> ('b, 'z) t): ('b, 'z) t =
        fun s k ->
        m s (fun res s ->
            match res with
            | Ok a          -> f a s k
            | Error _ as e  -> k e s)

    let ( let* ) = (>>=)

    let make (m: ('z, 'z) t) (s: 'z State.t): unit =
        m s State.set_result

    let get_meta (meta: State.meta) (f: State.term -> ('a, 'z) t): ('a, 'z) t =
        fun s k ->
        match State.get_meta meta s with
        | None ->
            State.wait_meta meta (fun term s -> f term s k) s
        | Some term ->
            f term s k
end
