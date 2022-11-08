module type ANY = Fmlib_std.Interfaces.ANY
module Array    = Fmlib_std.Array

module Make
        (Environment: ANY)
        (Context: ANY)
        (Spec: ANY)
        (Content: ANY)
        (Error: ANY)
=
struct
    type task_id    = Task of int
    type hole_id    = Hole of int
    type context_id = Context of int

    type environment  = Environment.t
    type context      = Context.t
    type spec         = Spec.t
    type hole_content = Content.t

    let no_task    = Task (-1)

    type queue =
        | ReadyQ
        | HoleQ    of hole_id
        | TaskQ    of task_id
        | ContextQ of context_id



    type error =
        | Blocked
        | Normal of Error.t

    type 'a t =
        state -> ('a, error) result

    and state = {
        mutable  n_waiting: int;
        mutable  env:     environment;
        mutable  tasks:   task array;
        mutable  holes:   hole array;
        mutable  contexts: context array;
        mutable  ready:   task_id list;
        mutable  current: task_id;
        mutable  current_context: context_id;
    }

    and task = {
        parent:       task_id;
        task_context: context_id;
        action:       unit t;
        mutable followers: task_id list;
        mutable n_pending_subtasks: int;
    }
    and hole = {
        mutable spec:    spec;
        mutable content: hole_content option;
        mutable hole_waiting: task_id list;
    }



    (* Monad *)
    let return (_: 'a): 'a t =
        assert false

    let ( >>= ) (_: 'a t) (_: 'a -> 'b t): 'b t =
        assert false

    let ( let* ) = (>>=)



    (* Internal functions *)

    let new_task (action: unit t) (state: state): task_id =
        let id = Array.length state.tasks in
        state.tasks <-
            Array.push
                {
                    parent = state.current;
                    n_pending_subtasks = 0;
                    task_context = state.current_context;
                    action;
                    followers = [];
                }
                state.tasks;
        Task id


    let init (env: environment) (context: context) (action: unit t): state =
        let state =
            {
                n_waiting = 0;
                env;
                tasks = [||];
                holes = [||];
                contexts = [|context|];
                ready = [];
                current = no_task;
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
        | ContextQ _ ->
            incr_n_waiting ();
            assert false








    (* Holes *)

    let make_hole (spec: spec): hole_id t =
        fun state ->
        let id = Hole (Array.length state.holes) in
        state.holes <-
            Array.push
                {
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
        Ok ()



    (* Tasks *)

    let make_task (where: queue) (action: unit t): task_id t =
        fun state ->
        let id = new_task action state in
        queue_task where id state;
        Ok id





    (* Run *)

    let run
            (env: environment)
            (context: context)
            (action: unit t)
        : (environment, error) result
        =
        let state = init env context action in
        let rec run state =
            match state.ready with
            | [] ->
                if state.n_waiting = 0 then
                    Ok state.env
                else
                    (* There are still waiting tasks *)
                    Error Blocked
            | id :: ready ->
                state.ready <- ready;
                (* run task [id] *)
                let task = get_task id state in
                state.current <- id;
                state.current_context <- task.task_context;
                match task.action state with
                | Ok () ->
                    finish_task task state;
                    run state
                | Error e ->
                    Error e
        in
        run state
end
