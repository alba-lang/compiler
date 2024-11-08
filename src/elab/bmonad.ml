open Std

module type ANY = Fmlib_std.Interfaces.ANY

module State (Meta: ANY) (Value: ANY) (Final: ANY) =
struct
    type t = {
        mutable ready: task list;
        metas: wait_queue Array_buffer.t;
    }

    and wait_queue = {
        mutable meta: Meta.t;
        mutable waiting: waiting_task list;
        mutable value: Value.t option;
    }

    and task = t -> Final.t option

    and waiting_task = {
        started: bool ref;
        task: (int * Value.t) -> task;
    }


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


    let find (id_lst: int list) (s: t): (int * Value.t) option =
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
        let qu = queue id s in
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
                qu.waiting
                s.ready;
        qu.waiting <- [];
        qu.value <- Some value

    
    let add_wait
            (id: int)
            (started: bool ref)
            (task: (int * Value.t) -> task)
            (s: t)
        : unit
        =
        let qu = queue id s in
        assert (qu.value = None);
        assert (not !started);
        qu.waiting <- {started; task} :: qu.waiting


    let add_ready (task: task) (s: t): unit =
        s.ready <- task :: s.ready


    let do_ready_task
            (next: t -> Final.t)  (* Next ready task returns [None]  *)
            (lock: int
                   -> (int -> (Meta.t * Value.t option))
                   -> Final.t) (* Deadlock, all tasks are waiting *)
            (s: t)
        : Final.t
        =
        match s.ready with
        | [] ->
            (* Deadlock *)
            lock
                (count s)
                (fun id ->
                     let qu = queue id s in
                     (qu.meta, qu.value))

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
        let k1 (_, value) = k value
        and queue = ST.queue id s in
        match queue.value with
        | None ->
            let started = ref false
            in
            ST.add_wait id started k1 s;
            None
        | Some value ->
            k value s


    let wait_some (id_lst: int list): (int * Value.t) t =
        fun k s ->
        match ST.find id_lst s with
        | None ->
            let started = ref false
            in
            List.iter
                (fun id -> ST.add_wait id started k s)
                id_lst;
            None
        | Some pair ->
            k pair s


    let spawn (task: unit t): unit t =
        fun k s ->
        ST.add_ready (task k) s;
        None


    let terminate (final: Final.t): 'a t =
        fun _ _ ->
        Some final


    let run
            (success: Final.t t)
            (failure: int -> (int -> (Meta.t * Value.t option)) -> Final.t)
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


let print (flg: bool): Final.t -> Final.t =
    let open Printf in
    function
    | Ok s as res ->
        if flg then
            printf "%s %s\n" "Ok" s;
        res
    | Error s as res ->
        if flg then
            printf "%s %s\n" "Error" s;
        res

let _ = print


let reporter (n: int) (f: int -> (Meta.t * Value.t option)): Final.t =
    let rec report i =
        if i = n then
            Error "no empty values"
        else
            let open Printf in
            let (meta, value) = f i in
            match value with
            | None ->
                Error (sprintf "cannot make %s" meta)
            | Some _ ->
                report (i + 1)
    in
    report 0


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


let one_level2 (block_b: bool): Final.t t =
    let* id_a = create "a" in
    let* id_b = create "b" in
    let* _    = spawn (make_leaf id_a "a") in
    let* _    =
        if block_b then
            return ()
        else
            spawn (make_leaf id_b "b")
    in
    let* (id_x, x) = wait_some [id_a; id_b] in
    let make t =
        Ok (t |> string_of_tree) |> return
    in
    if id_x = id_a then
        let* b = wait id_b in
        make (Node [x; b])
    else
        let* a = wait id_a in
        make (Node [a; x])


let%test _ =
    run simple reporter
    =
    Ok "simple"


let%test _ =
    run one_level reporter
    =
    Ok "(a,b)"


let%test _ =
    print
        false
        (run (one_level2 false) reporter)
    =
    Ok "(a,b)"


let%test _ =
    print
        false
        (run (one_level2 true) reporter)
    =
    Error "cannot make b"
