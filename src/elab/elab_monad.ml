open Std
open Core


module type ANY = Fmlib_std.Interfaces.ANY


module Position = Fmlib_parse.Position


module WTB = Welltyped.Basic





module State (Final: ANY) =
struct
    type t =
        {
            mutable final: (Final.t, Error.t) result option;
            global: Globals.t
        }


    let final (s: t): (Final.t, Error.t) result option =
        s.final



    let next (_: t): (t -> unit) option =
        assert false



    let fail (e: Error.t) (s: t): unit =
        s.final <- Some (Error e)


    let succeed (v: Final.t) (s: t): unit =
        s.final <- Some (Ok v)


    let push_ready (_: t -> unit) (_: t): unit =
        assert false


    let push_wait (_: int) (_: int) (_: WTB.term -> t -> unit) (_: t): unit =
        assert false


    let get_meta (_: int) (_: int) (_: t): WTB.term option =
        assert false


    let error (_: t): Error.t =
        assert false


    let make (global: Globals.t): t =
        {
            final = None;
            global;
        }
end






module Basic (Final: ANY) =
struct
    module State = State (Final)


    type range = Position.range


    type 'a t = State.t -> ('a -> State.t -> unit) -> unit


    let empty_continuation _ _ =
        ()


    let final_continuation a s =
        State.succeed a s
    



    (* Run the monad *)


    let rec iter (s: State.t): (Final.t, Error.t) result =
        match State.final s with
        | Some res ->
            res
        | None ->
            match State.next s with
            | Some task ->    (* task: State.t -> unit *)
                task s;
                iter s
            | None ->
                Error (State.error s)


    let run (g: Globals.t) (m: Final.t t): (Final.t, Error.t) result =
        let s = State.make g
        in
        m s final_continuation;
        iter s




    (* Monadic Functions *)


    let return (a: 'a): 'a t =
        fun s k -> k a s


    let fail (e: Error.t): 'a t =
        fun s _ -> State.fail e s



    let ( let* ) (m: 'a t) (f: 'a -> 'b t): 'b t =
        fun s k ->
        m s (fun a s -> f a s k)


    let ( >>= ) = ( let* )


    let spawn (task: unit t): unit t =
        fun s k ->
        State.push_ready (fun s -> task s empty_continuation) s;
        k () s


    let get_meta (ctxt: int) (id: int): WTB.term t =
        fun s k ->
        match State.get_meta ctxt id s with
        | None ->
            State.push_wait ctxt id k s;
            ()
        | Some t ->
            k t s


    let get_meta_opt (ctxt: int) (id: int): WTB.term option t =
        fun s k ->
        k (State.get_meta ctxt id s) s
end









module Make (Final: ANY) =
struct
    module Basic = Basic (Final)


    module WTM = Welltyped.Make (Basic)


    include Basic
end
