module type ANY = Fmlib_std.Interfaces.ANY



(** {1 A tracer for the interleaved elaboration process} *)


(** Tracer for the interleaved elaboration process *)
module type TRACER =
sig
    type tick = int  (** Current tick *)

    type task = int list (** Path from the current task to the root task *)

    type message (** Trace message *)

    type t       (** Type of the tracer *)

    val empty: t (** An empty tracer with no messages *)

    val add: tick -> task -> message -> t -> t
    (** [add tick task message tracer]
        Add [message] at [tick] from [task] to [tracer]. *)
end




(** {1 Generic elaborator with interleaved tasks} *)

(** Generic elaborator with interleaved tasks *)
module Make (Hole: ANY) (Value: ANY) (Tracer: TRACER) (Final: ANY):
sig

    (**{1 Basics} *)

    (**
       The elaborator finally elaborates an object of type [Final.t]. The root
       task has type [Final.t t]. The root task is run by

        {[run task error_handler]}

       where [task: Final.t t].

       Any task can spawn parallel subtasks to do the job. The communication
       between the tasks is done via objects of type [Hole.t].

       Elaboration might fail. Therefore in many applications the type [Final.t]
       is e.g.  [(success, error) result].

       The type [Hole.t] is used for not yet elaborated objects. Holes are used
       to communicate between tasks. A task can create a hole, spawn a subtask
       which fills the hole in parallel. At any point the parent task can wait
       for the hole to be filled. This puts the parent task onto the wait queue
       for the hole.

       Holes can be read and updated from all tasks.

       A hole can be filled with an object of type [Value.t]. If a hole is
       filled, all tasks waiting for the filling of the hole become unblocked
       i.e. they are removed from the wait queue and are put onto the ready
       queue.

       The [Tracer.t] object is used to trace the elaboration. Any task can
       write trace messages to the tracer. The messages are logged with a time
       stamp and an idenfication of the writing task.

       The elaborator starts by executing the main task. The main task finally
       delivers an object of type [Final.t]. The main task can be interrupted if
       it waits for the filling of some hole. If the hole has not yet been
       filled, then a task from the ready queue is run. The task from the ready
       queue then becomes the active task and runs until it waits for the
       filling of some hole.

       The process continues until the main task can deliver the desired object.

       It might be the case the all tasks are on some wait queues and no more
       task is on the ready queue. In that case the user supplied error handler
       is called. The error handler is called with the content of all holes.
     *)



    (**{1 Monad} *)

    type 'a t (** An object of type ['a t] elaborates an object of type ['a]. *)


    val return:    'a -> 'a t
    (** Immediately elaborate an object of type ['a]. *)


    val (>>=):     'a t -> ('a -> 'b t) -> 'b t
    (** [m >>= f]

        Elaborate an object [a] of type ['a] via the elaborator [m] and then
        elaborate an object of type ['b] by [f a].
    *)


    val (>=>): ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
    (** [f >=> g]

        Composition of monadic functions [f] and [g]. When given an [a] then

        - first elaborate [b] with [f a]

        - and then elaborate [c] with [g b].
    *)


    val ( let* ):  'a t -> ('a -> 'b t) -> 'b t
    (** [let* a = m in f a] is equivalent to [m >>= f]. *)


    val map:       ('a -> 'b) -> 'a t -> 'b t
    (** [map f m]

        Elaborate an object [a] by the elaborator [m] and then elaborate [f a].
    *)




    (** {1 Run the Elaborator} *)

    val run:
        Final.t t
        -> (int -> (int -> (int list * Hole.t * Value.t option)) -> Final.t)
        -> (Final.t * Tracer.t)
    (** [run m error_handler]

        Run the elaborator [m] and all its created subtasks until
        - either the root task elaborates an object of type [Final.t]
        - or the elaboration process deadlocks (all tasks are waiting for the
        filling of some holes) and the final object is created by calling
        [error_handler] with the content of all holes
        - or some task calls {!val:terminate}.

        Finally return the elaborated object and the tracer which collected the
        tracing messages.

        The error handler gets for each hole:

        - The task which created the hole.
        - The hole metadata.
        - An optional value which indicates if the hole has been filled.
    *)






    (**{1 Holes} *)

    val create:    Hole.t -> int t
    (** [create hole]

        Create a hole with the object [hole] and return an id for the hole.
        Initially a hole is unfilled.
    *)


    val get:       int -> Hole.t t
    (** Get a hole by its id. *)


    val put:       int -> Hole.t -> unit t
    (** Update the information of a hole. Note: updating is not filling. Just
     * the metainformation is updated. *)


    val update:    int -> (Hole.t -> Hole.t) -> unit t
    (** Update the information of a hole. *)


    val fill:      int -> Value.t -> unit t
    (** Fill a hole with a value and unblock all tasks waiting for the value. *)


    val wait:      int -> Value.t t
    (** Wait for the filling of a hole. Use the value after the hole has been
        filled. *)


    val wait_one:  int -> int list -> (int * Value.t) t
    (** [wait_one hole list]

        Wait for the filling of one of the holes in the list [hole :: list].
        Return the id of the filled hole and its value after one of the holes
        has been filled.
    *)





    (**{1 Subtasks} *)

    val spawn: unit t -> unit t
    (** Spawn a subtask. The spawned task is put onto the ready queue. If there
        are task which wait for some holes to be filled, tasks from the ready
        queue are executed.
    *)





    (** {1 Early Termination} *)

    val terminate: Final.t -> 'a t
    (** Immediately terminate the execution and return the final object.

        This function might be needed if the elaboration encounters an error. In
        that case the final object is filled with error information.
    *)





    (**{1 Tracing} *)

    val trace:     Tracer.message -> unit t
    (** [trace msg]

        Put [msg] into the tracer.
    *)


    val tick: int t
    (** The current time stamp. *)

    val task: int list t
    (** The currently executing task.

        Tasks are identified by their path to the main task. The main task is
        is identified by the empty list. The task [i :: path] is the [i]th
        subtask of the task identified by [path].
    *)
end
