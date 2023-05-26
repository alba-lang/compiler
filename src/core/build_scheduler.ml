(*
    Root task created from an action.

    All tasks generated from a task are its subtask and the generator is the
    parent task. Only the root task has no parent.

    A task is finished when all its subtasks are finished.

    All subtasks generated by a parent task created in a queue. The tasks in the
    ready queue can start immediately. A task might wait for

    - the completion of another taks (chaining of tasks)
    - the filling of a hole
    - the filling of all holes of a context



*)

module Array    = Fmlib_std.Array

type task_id    = Task of int
type hole_id    = Hole of int
type context_id = Context of int

type user_context = Context.t
type spec         = Hole_spec.t
type hole_content = Hole_content.t

let no_task    = Task (-1)

type queue =
    | ReadyQ
    | TaskQ    of task_id
    | HoleQ    of hole_id
    | ContextQ of context_id



type error =
    | Blocked
    | Normal of Error.t

type 'a t =
    state -> ('a, error) result

and state = {
    mutable  n_waiting: int;
    mutable  tasks:   task array;
    mutable  holes:   hole array;
    mutable  contexts: context array;
    mutable  ready:   task_id list;
    mutable  current_task:    task_id;
    mutable  current_context: context_id;
}

and task = {
    parent:       task_id;              (* no parent, parent = -1 *)
    task_context: context_id;
    action:       unit t;
    mutable followers: task_id list;
    mutable n_pending_subtasks: int;
}
and hole = {
    hole_context: context_id;
    mutable spec:    spec;
    mutable content: hole_content option;
    mutable hole_waiting: task_id list;
}
and context = {
    context: user_context;
    mutable n_holes: int;
    mutable context_waiting: task_id list;
}




(* Monad *)
let return (a: 'a): 'a t =
    fun _ ->
    Ok a


let fail (e: Error.t): 'a t =
    fun _ ->
    Error (Normal e)


let ( >>= ) (m: 'a t) (f: 'a -> 'b t): 'b t =
    fun state ->
    match m state with
    | Ok a ->
        f a state
    | Error e ->
        Error e


let ( let* ) = (>>=)



let int_fold_left (n: int) (s: 'a t) (f: int -> 'a -> 'a t): 'a t =
    assert (0 <= n);
    let rec fold accu i =
        if i = n then
            accu
        else
            let* a = accu in
            fold (f i a) (i + 1)
    in
    fold s 0



let int_fold_right (n: int) (f: int -> 'a -> 'a t) (s: 'a t): 'a t =
    assert (0 <= n);
    let rec fold i accu =
        if i = 0 then
            accu
        else
            let i = i - 1 in
            let* a = accu in
            fold i (f i a)
    in
    fold n s



let map (f: 'a -> 'b) (m: 'a t): 'b t =
    let* a = m in
    return (f a)



(* Internal functions *)

let new_context (context: user_context): context =
    {
        context;
        n_holes = 0;
        context_waiting = [];
    }


let new_task (action: unit t) (state: state): task_id =
    let id = Array.length state.tasks in
    state.tasks <-
        Array.push
            {
                parent = state.current_task;
                n_pending_subtasks = 0;
                task_context = state.current_context;
                action;
                followers = [];
            }
            state.tasks;
    Task id


let init
        (context: user_context)
        (action: unit t)
    : state
    =
    (* Initialize the state by creating a task from [action]. The task is
       the only task in the system and the only task in the ready queue. The
       task has no parent and there is no current task.
    *)
    let state =
        {
            n_waiting = 0;
            tasks    = [||];
            holes    = [||];
            contexts = [| new_context context |];
            ready    = [];
            current_task    = no_task;
            current_context = Context 0;
        }
    in
    let task_id = new_task action state in
    state.ready <- [task_id];
    state



let get_task (Task id: task_id) (state: state): task =
    assert (0 <= id);
    assert (id < Array.length state.tasks);
    state.tasks.(id)


let unblock (lst: task_id list) (state: state): unit =
    let rec unblock = function
        | [] ->
            ()
        | hd :: tl ->
            assert (state.n_waiting > 0);
            state.n_waiting <- state.n_waiting - 1;
            unblock tl;
            state.ready <- hd :: state.ready;
    in
    unblock lst



let rec finish_task (task: task) (state: state): unit =
    (* Do nothing as long as there are pending subtasks.
     * If there are no pending subtasks, then
     *    - unblock tasks which are awaiting completion
     *    - finish parents recursively.
    *)
    if task.n_pending_subtasks > 0 then
        ()
    else if task.parent = no_task then
        ()
    else
        (* no pending subtasks and has parent *)
        (* unblock tasks awaiting completion *)
        (
            let followers = task.followers in
            task.followers <- [];
            unblock followers state
        );
    (* finish parent *)
    let parent = get_task task.parent state in
    let n = parent.n_pending_subtasks in
    assert (n > 0);
    parent.n_pending_subtasks <- n - 1;
    finish_task parent state




let queue_task (where: queue) (id: task_id) (state: state): unit =
    let incr_n_waiting () =
        state.n_waiting <- state.n_waiting + 1
    in
    match where with
    | ReadyQ ->
        state.ready <-
            id :: state.ready
    | HoleQ (Hole i) ->
        incr_n_waiting ();
        let hole = state.holes.(i) in
        hole.hole_waiting <- id :: hole.hole_waiting
    | TaskQ (Task i) ->
        incr_n_waiting ();
        let task = state.tasks.(i) in
        task.followers <- id :: task.followers
    | ContextQ (Context i) ->
        incr_n_waiting ();
        let context = state.contexts.(i) in
        context.context_waiting <- id :: context.context_waiting








(* Holes *)

let make_hole (spec: spec): hole_id t =
    fun state ->
    let id = Hole (Array.length state.holes) in
    state.holes <-
        Array.push
            {
                hole_context = state.current_context;
                spec;
                hole_waiting = [];
                content = None;
            }
            state.holes;
    Ok id


let get_hole_spec (Hole id: hole_id): spec t =
    fun state ->
    Ok state.holes.(id).spec


let set_hole_spec (Hole id: hole_id) (spec: spec): unit t =
    fun state ->
    let hole = state.holes.(id) in
    assert (hole.content = None);
    hole.spec <- spec;
    Ok ()



let fill_hole (Hole id: hole_id) (content: hole_content): unit t =
    fun state ->
    let hole = state.holes.(id) in
    assert (hole.content = None);
    hole.content <- Some content;
    let Context i = hole.hole_context in
    let context = state.contexts.(i) in
    assert (context.n_holes > 0);
    context.n_holes <- context.n_holes - 1;
    if context.n_holes = 0 then
        begin
            unblock context.context_waiting state;
            context.context_waiting <- []
        end;
    Ok ()



(* Contexts *)

let make_context (context: user_context): context_id t =
    fun state ->
    let id = Array.length state.contexts in
    state.contexts <-
        Array.push (new_context context) state.contexts;
    Ok (Context id)


let get_context: user_context t =
    fun state ->
    let Context i = state.current_context in
    Ok state.contexts.(i).context




(* Tasks *)

let make_task (where: queue) (action: unit t): task_id t =
    fun state ->
    let id = new_task action state in
    queue_task where id state;
    Ok id


let current_task: task_id t =
    fun state ->
    Ok state.current_task





(* Run *)

let run
        (context: user_context)
        (action: unit t)
    : (user_context, error) result
    =
    let state = init context action in
    let rec run state =
        match state.ready with
        | [] ->
            if state.n_waiting = 0 then
                Ok state.contexts.(0).context
            else
                (* There are still waiting tasks *)
                Error Blocked
        | id :: ready ->
            state.ready <- ready;
            (* run task [id] *)
            let task = get_task id state in
            state.current_task    <- id;
            state.current_context <- task.task_context;
            match task.action state with
            | Ok () ->
                finish_task task state;
                run state
            | Error e ->
                Error e
    in
    run state
