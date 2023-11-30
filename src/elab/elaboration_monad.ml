open Std
open Core


(* Basic Monad *)

module Make (Final: Fmlib_std.Interfaces.ANY) =
struct
    exception Done of Final.t
    exception Fail of Error.t

    type meta_reason = Error.t option




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






    (* Monadic Functions *)

    let new_context: int t =
        fun s k ->
        let id = State.new_context s in
        k id s


    let new_id: int t =
        fun s k ->
        let id = State.new_id s in
        k id s



    let new_meta (r: meta_reason) (req: Checker.req) (ctxt: int): int t =
        fun s k ->
        let id =
            State.new_meta
                (Option.map (fun e _ -> raise (Fail e)) r)
                req ctxt s in
        k id s


    let spawn (task: unit t): unit t =
        fun s k ->
        State.spawn
            (fun s -> task s (fun () _ -> ()))
            s;
        k () s






    (* Run the monad *)

    let run (m: Final.t t) (s: State.t): (Final.t, Error.t) result =
        try
            m s (fun final _ -> raise (Done final));
            let _ = State.execute s in
            assert false (* shall never happen *)
        with
        | Done final -> Ok final
        | Fail e     -> Error e  (* Triggered by function [fail e] *)
end
