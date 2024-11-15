module type ANY = Fmlib_std.Interfaces.ANY




module type TRACER =
sig
    type tick = int
    type task = int list

    type message
    type t

    val empty: t
    val add: tick -> task -> message -> t -> t
end



let string_of_path (path: int list): string =
    Printf.sprintf
        "[%s]"
        (String.concat "," (List.rev_map string_of_int path))



module State (Hole: ANY) (Value: ANY) (Tracer: TRACER) (Final: ANY) =
struct

    (* See Note [Execution State] *)
    type t = {
        holes:  wait_queue Array_buffer.t;
        mutable ready: task list;
        mutable active: task_data;
        mutable tick:   int;
        mutable tracer: Tracer.t;
    }

    and wait_queue =
        (* See Note [Holes and Values] *)
        {
            task_id: int list;   (* Task which created the hole *)
            mutable hole: Hole.t;
            mutable waiting: waiting_task list;
            mutable value: Value.t option;
        }

    and task = {
        action: action;
        data: task_data;
    }

    and waiting_task = {        (* See Note [Waiting Tasks] *)
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
            holes  = Array_buffer.make ();
            active = {n_childs = 0; path = []};
            tick   = 0;
            tracer = Tracer.empty;
        }

    
    let count (s: t): int =
        Array_buffer.length s.holes


    let tick (s: t): int =
        s.tick


    let path (s: t): int list =
        s.active.path


    let trace (msg: Tracer.message) (s: t): unit =
        s.tracer <-
            Tracer.add s.tick s.active.path msg s.tracer


    let create (hole: Hole.t) (s: t): int =
        let queue = {
            hole;
            task_id = s.active.path;
            waiting = [];
            value = None;
        }
        and id = count s in
        (*Printf.printf
            "%d: %s create %d\n"
            s.tick
            (string_of_path s.active.path)
            id;*)
        Array_buffer.push s.holes queue;
        id



    let queue (id: int) (s: t): wait_queue =
        assert (id < count s);
        Array_buffer.get s.holes id


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


    let hole (id: int) (s: t): Hole.t =
        (queue id s).hole


    let value (id: int) (s: t): Value.t option =
        (queue id s).value


    let put (id: int) (hole: Hole.t) (s: t): unit =
        (queue id s).hole <- hole


    let fill (id: int) (value: Value.t) (s: t): unit =
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
            (fail: int -> (int -> (int list * Hole.t * Value.t option)) -> Final.t)
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
                                 (q.task_id, q.hole, q.value))
                    in
                    (final, state.tracer)
        in
        exe task
end











module Make (Hole: ANY) (Value: ANY) (Tracer: TRACER) (Final: ANY) (Error: ANY) =
struct

    type res = (Final.t, Error.t) result

    module ST =
        State
            (Hole)
            (Value)
            (Tracer)
            (struct type t = res end)



    (* Basic monad (See Note [Elaboration Monad]) *)

    type action = ST.t -> res option

    type 'a t = ('a -> action) -> action



    let final_continuation (final: Final.t): action =
        fun _ -> Some (Ok final)


    let spawn_continuation ((): unit): action =
        fun _ -> None


    let return (a: 'a): 'a t =
        fun k -> k a


    let fail (e: Error.t): 'a t =
        fun _ _ -> Some (Error e)


    let (>>=) (m: 'a t) (f: 'a -> 'b t): 'b t =
        fun k ->
        m (fun a -> f a k)


    let ( let* ) = (>>=)


    let map (f: 'a -> 'b) (m: 'a t): 'b t =
        fun k ->
        m (fun a -> k (f a))


    let (>=>) (f: 'a -> 'b t) (g: 'b -> 'c t): 'a -> 'c t =
        fun a -> f a >>= g





    (* Monadic functions *)

    let trace (msg: Tracer.message): unit t =
        fun k s ->
        k (ST.trace msg s) s


    let tick: int t =
        fun k s ->
        k (ST.tick s) s


    let task: int list t =
        fun k s ->
        k (ST.path s) s


    let create (hole: Hole.t): int t =
        fun k s ->
        k (ST.create hole s) s


    let get (id: int): Hole.t t =
        fun k s ->
        k (ST.hole id s) s


    let put (id: int) (hole: Hole.t): unit t =
        fun k s ->
        ST.put id hole s;
        k () s


    let value_opt (id: int): Value.t option t =
        fun k s ->
        k (ST.value id s) s


    let update (id: int) (f: Hole.t -> Hole.t): unit t =
        fun k s ->
        ST.(put id (f (hole id s)) s);
        k () s


    let fill (id: int) (value: Value.t): unit t =
        fun k s ->
        ST.fill id value s;
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


    let wait_one (id: int) (id_lst: int list): (int * Value.t) t =
        fun k s ->
        match ST.find_value id_lst s with
        | None ->
            let started = ref false
            in
            List.iter
                (fun id -> ST.put_active_wait id started k s)
                (id :: id_lst);
            None
        | Some pair ->
            k pair s


    let spawn (task: unit t): unit t =
        fun k s ->
        ST.spawn (task spawn_continuation) s;
        k () s


    let run
            (main: Final.t t)
            (failure:
                 int
                 -> (int -> (int list * Hole.t * Value.t option))
                 -> Error.t)
        : (res * Tracer.t)
        =
        ST.run
            (main final_continuation)
            (fun n f -> Error (failure n f))
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
    type t = string
end


module Hole =
struct
    type t = string
end


module Value =
struct
    type t = string tree
end

module Tracer =
struct
    type tick = int
    type task = int list
    type message = string
    type t = (tick * task * message) list

    let empty: t = []

    let add (n: tick) (task: task) (s: string) (tr: t): t =
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


include Make (Hole) (Value) (Tracer) (Final) (Final)

open Printf






let reporter
        (n: int) (f: int -> (int list * Hole.t * Value.t option))
    : string
    =
    let rec report i =
        if i = n then
            "no empty values"
        else
            let (_, hole, value) = f i in
            match value with
            | None ->
                sprintf "cannot make %s" hole
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
    fill id (Leaf s)


let simple : Final.t t =
    (Leaf "simple" |> string_of_tree) |> return


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
    Node [a; b] |> string_of_tree |> return


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
    let* (id_x, x) = wait_one id_a [id_b] in
    let make t =
        let* _ = trace "end make (a,b)" in
        t |> string_of_tree |> return
    in
    if id_x = id_a then
        let* _ = trace "wait for 'b'" in
        let* b = wait id_b in
        make (Node [x; b])
    else
        let* _ = trace "wait for 'a'" in
        let* a = wait id_a in
        make (Node [a; x])


let one_level_terminate: Final.t t =
    let make_a  id =
        let* id_a = create "a" in
        let* _    = spawn (make_leaf id_a "a") in
        let* _    = trace "wait for a" in
        let* a    = wait id_a in
        let* _    = trace "terminate with a" in
        let* _    = string_of_tree a |> fail in
        fill id a
        in
    let* _ = trace "start make (a,b)" in
    let* id_a = create "a" in
    let* id_b = create "b" in
    let* _    = spawn (make_a id_a) in
    let* _    = spawn (make_leaf id_b "b") in
    let* _    = trace "wait for b" in
    let* b    = wait id_b in
    let* _    = trace "wait for a" in
    let* a    = wait id_a in
    let* _    = trace "end make (a,b)" in
    Node [a; b] |> string_of_tree |> return



let%test _ =
    test false simple "Ok simple"


let%test _ =
    test false one_level "Ok (a,b)"


let%test _ =
    test false (one_level2 false) "Ok (a,b)"


let%test _ =
    test false (one_level2 true) "Error cannot make b"


let%test _ =
    test false one_level_terminate "Error a"












(*
    Note [Execution State]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    The execution state consists of a ready queue of tasks and an array of
    holes. Each hole has a queue of waiting tasks (see Note [Waiting Tasks])
    which are waiting for the hole to be filled.

    The execution units are actions with the type

        t -> res option

    where 't' is the execution state. A task is basically an action with some
    additional data. The ready queue consists of a list of tasks.

    Each execution of an action increments the time stamp by 1.

    The execution state starts by executing the root action. If action returns a
    result, then the execution terminates and the result is returned to the
    user.

    If the action returns 'None', then the one of the following is done:

    - There is a ready task:

        Pop the task from the ready queue and execute its action.

    - The ready queue is empty:

        Call the error handler with all the holes and its optional values to
        create the final object.
*)



(*
    Note [Waiting Tasks]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    A waiting task consists of two components:

        started: bool ref

        task: (int * Value.t) -> task

   A waiting task waits for some hole to be filled. If the hole is filled, the
   function 'task' can be used to generate a task by applying it to the id of
   the hole and the filled value. The task can then be pushed to the ready
   queue.

   The started flag is a reference to a boolean value. Initially the started
   flag is false. As soon as the waiting task is removed from the wait queue and
   added to the ready queue the started flag is set to true.

   A waiting task can be in the waiting queue for several holes (i.e. it waits
   for one of the holes to be filled). The boolean flag is a shared value of all
   these entries. If the first of these holes is filled and the task is moved to
   the ready queue, the started flag is set to true. If another of the holes is
   filled the corresponding entry in the waiting queue has the flag set to true
   is there not put onto the ready queue. This mechanism avoids that a waiting
   task is started several times.
*)


(*

    Note [Holes and Values]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Objects of type 'Hole.t' are metadata describing the hole to be filled. A
    hole in the execution state (see Note [Execution State]) is an object with
    the components

        hole: Hole.t

        waiting: waiting_task list

        value: Value.t option

    The metadata 'hole' can be read and updated. Updating can be considered as
    partial filling. Finally a hole gets filled with a value. As soon as a hole
    has been filled, all tasks on the waiting list are put onto the ready queue.

    After filling of a hole, tasks are no longer put onto its waiting queue.
*)


(*

    Note [Tasks]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Tasks form a hierarchy.

        - root task []

            - subtask [0]           "subtask 0 of task []"

                - subtask [0,0]     "subtask 0 of task [0]"

            - subtask [1]

                - subtask [0,1]     "subtask 0 of task [1]"

                - subtask [1,1]     "subtask 1 of task [1]"

    Tasks are identified by their paths to the root. The root task has path
    '[]'. The i-th child of the task identified by 'path' has the path

        i :: path
*)



(*

    Note [Elaboration Monad]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    The basic type declarations of the monad are:

        type action = state -> res option

        type 'a t   = ('a -> action) -> action

    An elaborator which elaborates an object of type 'a has type 'a t. The
    execution state cannot work with objects of type 'a t'. It needs actions. An
    action can be generated from an elaborator by giving it a continuation which
    is a function of type

        'a -> action

    I.e. the combination of an elaborator and a continuation gives an action.


    There are two important continuations:

        let final_continuation (final: Final.t): action =
            fun _ -> Some (Ok final)

        let spawn_continuation ((): unit): action =
            fun _ -> None


    Having an elaborator 'm: Final.t t' the action

        m final_continuation

    can be given to the execution state to execute it as the main task.


    Tasks to be spawned are represented by elaborators 'task: unit t'. An object

        task spawn_continuation

    can be given to the execution state to push it into the ready queue.


    Things get interesting if a task has to wait for a hole to be filled. Holes
    are identified by their number. The call 'wait id' creates an elaborator of
    type 'Value.t t'. When given a continuation it produces an action. I.e. when
    given a continuation and a state it has to produce an object of type
    'Final.t option'. In the case that the hole has not yet been filled the
    implementation of 'wait' is quite simple.

        let wait (id: int): Value.t =
            fun k s ->
            ST.put_active_wait id k;
            None

    The currently active task is put onto the wait queue of the hole 'id'. Note
    that the continuation 'k' has the type 'Value.t -> action'. I.e. when the
    hole gets filled, the continuation 'k' can be used to generate the action of
    a task which can be shifted onto the ready queue.

    The current action immediately returns 'None' which signals to the execution
    state to pop other tasks from the ready queue.

    The actual function 'wait' is a little bit more complex.

    It first has to check, if the hole has already been filled. In that case the
    continuation can be immediately executed by giving it the value and the
    state.

    Furthermore the core action of a waiting task is called with the id of the
    hole and the filled value.

    Furthermore a waiting task needs a 'started' flag in order to handle task
    which can wait for the filling of one of a list of holes (See Note [Waiting
    Tasks]).
*)
