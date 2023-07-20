(* Basic Monad *)

exception Fail of Error.t

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
