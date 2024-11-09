open Std


module type ANY = Fmlib_std.Interfaces.ANY




module type TRACER =
sig
    type time = int
    type task = int list

    type item
    type t

    val empty: t
    val add: time -> task -> item -> t -> t
end



let string_of_path (path: int list): string =
    Printf.sprintf
        "[%s]"
        (String.concat "," (List.rev_map string_of_int path))



module State (Meta: ANY) (Value: ANY) (Tracer: TRACER) (Final: ANY) =
struct
    type t = {
        metas:  wait_queue Array_buffer.t;
        mutable ready: task list;
        mutable active: task_data;
        mutable tick:   int;
        mutable tracer: Tracer.t;
    }

    and wait_queue = {
        mutable meta: Meta.t;
        mutable waiting: waiting_task list;
        mutable value: Value.t option;
    }

    and task = {
        action: action;
        data: task_data;
    }

    and waiting_task = {
        started: bool ref;
        task: (int * Value.t) -> task;
    }

    and action = t -> Final.t option

    and task_data = {
        mutable n_childs: int;
        path: int list;
    }




    let make (): t =
        {
            ready  = [];
            metas  = Array_buffer.make ();
            active = {n_childs = 0; path = []};
            tick   = 0;
            tracer = Tracer.empty;
        }

    
    let count (s: t): int =
        Array_buffer.length s.metas


    let trace (msg: Tracer.item) (s: t): unit =
        s.tracer <-
            Tracer.add s.tick s.active.path msg s.tracer


    let create (meta: Meta.t) (s: t): int =
        let queue = {
            meta;
            waiting = [];
            value = None;
        }
        and id = count s in
        (*Printf.printf
            "%d: %s create %d\n"
            s.tick
            (string_of_path s.active.path)
            id;*)
        Array_buffer.push s.metas queue;
        id



    let queue (id: int) (s: t): wait_queue =
        assert (id < count s);
        Array_buffer.get s.metas id


    let find_value (id_lst: int list) (s: t): (int * Value.t) option =
        let rec find = function
            | [] ->
                None
            | id :: lst ->
                match (queue id s).value with
                | None ->
                    find lst
                | Some value ->
                    Some (id, value)
        in
        find id_lst


    let meta (id: int) (s: t): Meta.t =
        (queue id s).meta


    let put (id: int) (meta: Meta.t) (s: t): unit =
        (queue id s).meta <- meta


    let resolve (id: int) (value: Value.t) (s: t): unit =
        let q = queue id s in
        s.ready <-
            List.fold_right
                (fun wait ready ->
                     let started = !(wait.started) in
                     wait.started := true;
                     if started then
                         ready
                     else
                         wait.task (id, value) :: ready
                )
                q.waiting
                s.ready;
        q.waiting <- [];
        q.value <- Some value


    let put_active_wait
            (id: int)
            (started: bool ref)
            (action: (int * Value.t) -> action)
            (s: t)
        : unit
        =
        assert (not !started);
        let q    = queue id s
        and data = s.active
        in
        assert (q.value = None);
        (*Printf.printf
            "%d: %s wait for %d\n"
            s.tick
            (string_of_path s.active.path)
            id;*)
        let task ival = {
                    action = action ival;
                    data;
                }
        in
        q.waiting <-
            {started; task}
            ::
            q.waiting


    let make_child_of_active (action: action) (s: t): task =
        let data = {
            n_childs = 0;
            path = s.active.n_childs :: s.active.path;
        }
        in
        s.active.n_childs <- s.active.n_childs + 1;
        {action; data}


    let spawn (action: action) (s: t): unit =
        let task = make_child_of_active action s in
        (*Printf.printf
            "%d: %s spawn %s, children %d\n"
            s.tick
            (string_of_path s.active.path)
            (string_of_path task.data.path)
            s.active.n_childs;*)
        s.ready <- task :: s.ready


    let pop_ready (s: t): task option =
        match s.ready with
        | [] ->
            None
        | task :: ready ->
            s.ready <- ready;
            Some task


    let step (task: task) (s: t): Final.t option =
        s.active <- task.data;
        (*Printf.printf
            "%d: task %s, children %d\n"
            s.tick
            (string_of_path s.active.path)
            s.active.n_childs;*)
        let res = task.action s in
        s.tick <- s.tick + 1;
        res


    let run
            (root: action)
            (fail: int -> (int -> (Meta.t * Value.t option)) -> Final.t)
        : Final.t * Tracer.t
        =
        let state = make ()
        and task  = {
            action = root;
            data = {
                n_childs = 0;
                path = [];
            }
        }
        in
        let rec exe task =
            match step task state with
            | Some final ->
                (final, state.tracer)
            | None ->
                match pop_ready state with
                | Some task ->
                    exe task
                | None ->
                    assert (count state > 0);
                    let final =
                        fail
                            (count state)
                            (fun id ->
                                 let q = queue id state in
                                 (q.meta, q.value))
                    in
                    (final, state.tracer)
        in
        exe task
end











module Make (Meta: ANY) (Value: ANY) (Tracer: TRACER) (Final: ANY) =
struct

    module ST = State (Meta) (Value) (Tracer) (Final)

    type state = ST.t



    (* Basic monad *)

    type 'a cont = 'a -> state -> Final.t option

    type 'a t = 'a cont -> state -> Final.t option


    let final_continuation: Final.t cont =
        fun final _ -> Some final


    let spawn_continuation: unit cont =
        fun () _ -> None


    let return (a: 'a): 'a t =
        fun k s -> k a s


    let (>>=) (m: 'a t) (f: 'a -> 'b t): 'b t =
        fun k ->
        m (fun a -> f a k)


    let ( let* ) = (>>=)


    let map (f: 'a -> 'b) (m: 'a t): 'b t =
        fun k ->
        m (fun a -> k (f a))





    (* Monadic functions *)

    let trace (msg: Tracer.item): unit t =
        fun k s ->
        k (ST.trace msg s) s


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
        let k1 (_, value) = k value
        and queue = ST.queue id s in
        match queue.value with
        | None ->
            let started = ref false
            in
            ST.put_active_wait id started k1 s;
            None
        | Some value ->
            k value s


    let wait_some (id_lst: int list): (int * Value.t) t =
        fun k s ->
        match ST.find_value id_lst s with
        | None ->
            let started = ref false
            in
            List.iter
                (fun id -> ST.put_active_wait id started k s)
                id_lst;
            None
        | Some pair ->
            k pair s


    let spawn (task: unit t): unit t =
        fun k s ->
        ST.spawn (task spawn_continuation) s;
        k () s


    let terminate (final: Final.t): 'a t =
        fun _ _ ->
        Some final


    let run
            (success: Final.t t)
            (failure: int -> (int -> (Meta.t * Value.t option)) -> Final.t)
        : (Final.t * Tracer.t)
        =
        ST.run
            (success final_continuation)
            failure
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

module Tracer =
struct
    type time = int
    type task = int list
    type item = string
    type t = (time * task * item) list

    let empty: t = []

    let add (n: time) (task: task) (s: string) (tr: t): t =
        (n, task, s) :: tr

    let print (prefix: string) (tr: t): unit =
        let open Printf in
        List.iter
            (fun (time, path, msg) ->
                 printf
                     "%s%d: %s: %s\n"
                     prefix
                     time
                     (string_of_path path)
                     msg)
            (List.rev tr)
end


include Make (Meta) (Value) (Tracer) (Final)

open Printf






let reporter (n: int) (f: int -> (Meta.t * Value.t option)): Final.t =
    let rec report i =
        if i = n then
            Error "no empty values"
        else
            let (meta, value) = f i in
            match value with
            | None ->
                Error (sprintf "cannot make %s" meta)
            | Some _ ->
                report (i + 1)
    in
    report 0



let test (print_flag: bool) (m: Final.t t) (expect: string): bool =
    let string_of_res = function
        | Ok s    -> sprintf "Ok %s" s
        | Error s -> sprintf "Error %s" s
    in
    let (res, tracer) = run m reporter in
    let sres = string_of_res res in
    if print_flag then
        begin
            printf "%s\n" sres;
            Tracer.print "  " tracer
        end;
    sres = expect




let make_leaf (id: int) (s: string): unit t =
    let* _ = trace (Printf.sprintf "make (Leaf %s)" s) in
    resolve id (Leaf s)


let simple : Final.t t =
    Ok (Leaf "simple" |> string_of_tree) |> return


let one_level: Final.t t =
    let* _    = trace "start make (a,b)" in
    let* id_a = create "a" in
    let* id_b = create "b" in
    let* _    = spawn (make_leaf id_a "a") in
    let* _    = spawn (make_leaf id_b "b") in
    let* _    = trace "wait for a" in
    let* a    = wait id_a in
    let* _    = trace "wait for b" in
    let* b    = wait id_b in
    let* _    = trace "end make (a,b)" in
    Ok (Node [a; b] |> string_of_tree) |> return


let one_level2 (block_b: bool): Final.t t =
    let* _    = trace "start make (a,b)" in
    let* id_a = create "a" in
    let* id_b = create "b" in
    let* _    = spawn (make_leaf id_a "a") in
    let* _    =
        if block_b then
            return ()
        else
            spawn (make_leaf id_b "b")
    in
    let* _ = trace "wait for 'a' or 'b'" in
    let* (id_x, x) = wait_some [id_a; id_b] in
    let make t =
        let* _ = trace "end make (a,b)" in
        Ok (t |> string_of_tree) |> return
    in
    if id_x = id_a then
        let* _ = trace "wait fof 'b'" in
        let* b = wait id_b in
        make (Node [x; b])
    else
        let* _ = trace "wait fof 'a'" in
        let* a = wait id_a in
        make (Node [a; x])




let%test _ =
    test false simple "Ok simple"


let%test _ =
    test false one_level "Ok (a,b)"


let%test _ =
    test false (one_level2 false) "Ok (a,b)"


let%test _ =
    test false (one_level2 true) "Error cannot make b"
