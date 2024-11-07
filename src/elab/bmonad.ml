open Std

module type ANY = Fmlib_std.Interfaces.ANY

module State (Meta: ANY) (Value: ANY) (Final: ANY) =
struct
    type t = {
        mutable ready: ready_task list;
        metas: wait_queue Array_buffer.t;
    }

    and wait_queue = {
        mutable meta: Meta.t;
        mutable waiting: waiting_task list;
        mutable value: Value.t option;
    }

    and ready_task = t -> Final.t option

    and waiting_task = Value.t -> t -> Final.t option


    let make (): t =
        {
            ready = [];
            metas = Array_buffer.make ();
        }

    
    let count (s: t): int =
        Array_buffer.length s.metas


    let create (meta: Meta.t) (s: t): int =
        let queue = {
            meta;
            waiting = [];
            value = None;
        }
        and id = count s in
        Array_buffer.push s.metas queue;
        id



    let queue (id: int) (s: t): wait_queue =
        assert (id < count s);
        Array_buffer.get s.metas id


    let meta (id: int) (s: t): Meta.t =
        (queue id s).meta


    let put (id: int) (meta: Meta.t) (s: t): unit =
        (queue id s).meta <- meta


    let resolve (id: int) (value: Value.t) (s: t): unit =
        let qu = queue id s in
        s.ready <-
            List.fold_right
                (fun wait_task ready ->
                     wait_task value :: ready)
                qu.waiting
                s.ready;
        qu.waiting <- [];
        qu.value <- Some value

    
    let add_wait (id: int) (task: waiting_task) (s: t): unit =
        let qu = queue id s in
        assert (qu.value = None);
        qu.waiting <- task :: qu.waiting


    let add_ready (task: ready_task) (s: t): unit =
        s.ready <- task :: s.ready


    let do_ready_task
            (next: t -> Final.t)                  (* Next ready task returns
                                                         [None]  *)
            (lock: int -> (int -> Meta.t) -> Final.t) (* Deadlock, all tasks are
                                                         waiting *)
            (s: t)
        : Final.t
        =
        match s.ready with
        | [] ->
            (* Deadlock *)
            lock (count s) (fun id -> meta id s)

        | task :: ready ->
            s.ready <- ready;
            match task s with
            | None ->
                next s
            | Some final ->
                final
end







module Make (Meta: ANY) (Value: ANY) (Final: ANY) =
struct

    module ST = State (Meta) (Value) (Final)

    type state = ST.t



    (* Basic monad *)

    type 'a cont = 'a -> state -> Final.t option

    type 'a t = 'a cont -> state -> Final.t option


    let return (a: 'a): 'a t =
        fun k s -> k a s


    let (>>=) (m: 'a t) (f: 'a -> 'b t): 'b t =
        fun k s->
        m (fun a s -> f a k s) s


    let ( let* ) = (>>=)


    let map (f: 'a -> 'b) (m: 'a t): 'b t =
        fun k s ->
        m (fun a s -> k (f a) s) s





    (* Monadic functions *)

    let create (meta: Meta.t): int t =
        fun k s ->
        k (ST.create meta s) s


    let get (id: int): Meta.t t =
        fun k s ->
        k (ST.meta id s) s


    let put (id: int) (meta: Meta.t): unit t =
        fun k s ->
        ST.put id meta s;
        k () s


    let resolve (id: int) (value: Value.t): unit t =
        fun k s ->
        ST.resolve id value s;
        k () s


    let wait (id: int): Value.t t =
        fun k s ->
        let queue = ST.queue id s in
        match queue.value with
        | None ->
            ST.add_wait id k s;
            None
        | Some value ->
            k value s


    let spawn (task: unit t): unit t =
        fun k s ->
        ST.add_ready (task k) s;
        None


    let terminate (final: Final.t): 'a t =
        fun _ _ ->
        Some final


    let run
            (success: Final.t t)
            (failure: int -> (int -> Meta.t) -> Final.t)
        : Final.t
        =
        let rec iterate (s: state): Final.t =
            ST.do_ready_task
                iterate
                failure
                s
        in
        let state = ST.make ()
        in
        ST.add_ready
            (success (fun final _ -> Some final))
            state;
        iterate state
end











(* ======================================================================
 * Unit Tests
 * ====================================================================== *)


type 'a tree =
    | Leaf of 'a
    | Node of 'a tree list

let rec string_of_tree: string tree -> string =
    function
    | Leaf s ->
        s
    | Node lst ->
        "("
        ^ String.concat "," (List.map string_of_tree lst)
        ^ ")"



module Final =
struct
    type t = (string, string) result
end


module Meta =
struct
    type t = string
end


module Value =
struct
    type t = string tree
end

include Make (Meta) (Value) (Final)


let make_leaf (id: int) (s: string): unit t =
    resolve id (Leaf s)


let simple : Final.t t =
    Ok (Leaf "simple" |> string_of_tree) |> return

let one_level: Final.t t =
    let* id_a = create "a" in
    let* id_b = create "b" in
    let* _ = spawn (make_leaf id_a "a") in
    let* _ = spawn (make_leaf id_b "b") in
    let* a = wait id_a in
    let* b = wait id_b in
    Ok (Node [a; b] |> string_of_tree) |> return


let%test _ =
    run simple (fun _ _ -> assert false)
    =
    Ok "simple"


let%test _ =
    run one_level (fun _ _ -> assert false)
    =
    Ok "(a,b)"
