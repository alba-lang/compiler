open Std
open Core






module type ANY = Fmlib_std.Interfaces.ANY

module Pretty = Fmlib_pretty.Print
type   doc    = Pretty.doc

module Position = Fmlib_parse.Position










module State (Final: ANY) =
struct
    type meta = Welltyped.Basic.meta

    type started = bool ref

    type t =
        {
            mutable final: (Final.t, Error.t) result option;
            mutable doc: doc;
            mutable readyq: (t -> unit) list;
            contexts: meta_buffer Array_buffer.t Array_buffer.t;
            trace_flag: bool;
            global: Globals.t
        }


    and meta_buffer = {
        mutable meta:  meta;
        mutable waitq: (started * (meta -> t -> unit)) list;
    }



    let final (s: t): (Final.t, Error.t) result option =
        s.final



    let doc (s: t): doc =
        s.doc



    let log (doc: unit -> doc) (s: t): unit =
        if s.trace_flag then
            s.doc <- Pretty.(s.doc >> doc)


    let next (s: t): (t -> unit) option =
        match s.readyq with
        | [] ->
            None
        | task :: readyq ->
            s.readyq <- readyq;
            Some task



    let succeed (v: Final.t) (s: t): unit =
        s.final <- Some (Ok v)


    let push_ready (task: t -> unit) (s: t): unit =
        s.readyq <- task :: s.readyq



    let has_context (ctxt: int) (s: t): bool =
        0 <= ctxt && ctxt < Array_buffer.length s.contexts



    let has_meta (ctxt: int) (id: int) (s: t): bool =
        has_context ctxt s
        &&
        0 <= id
        &&
        Array_buffer.(id < length (get s.contexts ctxt))



    let new_context (s: t): int =
        let id = Array_buffer.length s.contexts in
        Array_buffer.(push s.contexts (make ()));
        id



    let new_meta (ctxt: int) (meta: meta) (s: t): int =
        assert (has_context ctxt s);
        let c = Array_buffer.get s.contexts ctxt in
        let id = Array_buffer.length c in
        Array_buffer.push c {meta; waitq = []};
        id



    let meta_buffer (ctxt: int) (id: int) (s: t): meta_buffer =
        assert (has_context ctxt s);
        let c = Array_buffer.get s.contexts ctxt
        in
        assert (id < Array_buffer.length c);
        Array_buffer.get c id



    let update_meta (ctxt: int) (id: int) (f: meta -> meta) (s: t): unit =
        assert (has_meta ctxt id s);
        let mb = meta_buffer ctxt id s
        in
        mb.meta <- f mb.meta;
        List.iter
            (fun (started, task) ->
                 if not !started then begin
                     s.readyq <-
                         (fun s -> task mb.meta s) (* mb.meta will be taken at
                                                      the start of the task.
                                                      More update_meta might
                                                      have happened in the
                                                      meantime. *)
                         ::
                         s.readyq;
                     started := true
                 end
            )
            mb.waitq;
        mb.waitq <- []



    let wait_update
            (ctxt: int)
            (id: int)
            (task: meta -> t -> unit)
            (s: t)
        : unit
        =
        assert (has_meta ctxt id s);
        let mb = meta_buffer ctxt id s
        in
        mb.waitq <- (ref false, task) :: mb.waitq




    let wait_one_of
            (arr: (int * int) array)
            (task: int -> meta -> t -> unit)
            (s: t)
        : unit
        =
        let started = ref false
        in
        Array.iteri
            (fun i (ctxt, id) ->
                 assert (has_meta ctxt id s);
                 let mb = meta_buffer ctxt id s
                 in
                 mb.waitq <- (started, task i) :: mb.waitq
            )
            arr





    let error (_: t): Error.t =
        assert false


    let make (trace_flag: bool) (global: Globals.t): t =
        {
            final    = None;
            doc      = Pretty.empty;
            readyq   = [];
            contexts = Array_buffer.make ();
            trace_flag;
            global;
        }
end












module Make (Final: ANY) =
struct
    module State = State (Final)

    type meta = Welltyped.Basic.meta


    type 'a t = State.t -> ('a -> State.t -> unit) -> unit


    let empty_continuation _ _ =
        ()


    let final_continuation a s =
        State.succeed a s




    (* Run the monad *)


    let rec iter (s: State.t): doc * (Final.t, Error.t) result =
        match State.final s with
        | Some res ->
            State.doc s, res
        | None ->
            match State.next s with
            | Some task ->    (* task: State.t -> unit *)
                task s;
                iter s
            | None ->
                State.doc s, Error (State.error s)


    let run
            (trace_flg: bool)
            (g: Globals.t)
            (m: Final.t t)
        : doc * (Final.t, Error.t) result
        =
        let s = State.make trace_flg g
        in
        m s final_continuation;
        iter s




    (* Monadic Functions *)


    let return (a: 'a): 'a t =
        fun s k -> k a s



    let ( let* ) (m: 'a t) (f: 'a -> 'b t): 'b t =
        fun s k ->
        m s (fun a s -> f a s k)


    let ( >>= ) = ( let* )



    let ( >=> ) (f: 'a -> 'b t) (g: 'b -> 'c t): 'a -> 'c t =
        fun a ->
        f a >>= g



    let map (f: 'a -> 'b) (m: 'a t): 'b t =
        fun s k ->
        m s (fun a s -> k (f a) s)




    let new_context: int t =
        fun s k ->
        let id = State.new_context s in
        k id s



    let new_meta (ctxt: int) (meta: meta): int t =
        fun s k ->
        let id = State.new_meta ctxt meta s in
        k id s



    let update_meta (ctxt: int) (id: int) (f: meta -> meta): unit t =
        fun s k ->
        State.update_meta ctxt id f s;
        k () s



    let spawn (task: unit t): unit t =
        fun s k ->
        State.push_ready (fun s -> task s empty_continuation) s;
        k () s



    let wait_update (ctxt: int) (id: int) (f: meta -> unit t): unit t =
        fun s k ->
        State.wait_update
            ctxt
            id
            (fun meta s -> f meta s empty_continuation)
            s;
        k () s



    let wait_one_of
            (arr: (int * int) array) (f: int -> meta -> unit t): unit t =
        fun s k ->
        State.wait_one_of
            arr
            (fun i meta s -> f i meta s empty_continuation)
            s;
        k () s




    let log (doc: unit -> doc): unit t =
        fun s k ->
        State.log doc s;
        k () s
end
