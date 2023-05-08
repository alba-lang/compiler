module type ANY = Fmlib_std.Interfaces.ANY

module Make
        (Environment: ANY)
        (Context: ANY)
        (Spec: ANY)
        (Content: ANY)
        (Error: ANY):
sig

    (** {1 Monad} *)


    type _ t

    val return: 'a -> 'a t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
    val ( >>=  ): 'a t -> ('a -> 'b t) -> 'b t



    (** {1 Holes} *)

    type hole_id

    val make_hole: Spec.t -> hole_id t
    (** Make a new hole with a specification in the current context. *)


    val get_hole_spec: hole_id -> Spec.t t
    (** Get the specification of a hole. *)


    val set_hole_spec: hole_id -> Spec.t -> unit t
    (** Update the specification of a hole.

        Precondition:
            The hole does not yet have any content
    *)

    val fill_hole: hole_id -> Content.t -> unit t
    (** Fill a hole with content.

        Precondition:
            The hole does not yet have any content
    *)




    (** {1 Contexts} *)

    type context_id

    val make_context: Context.t -> context_id t
    (** Make a new context. *)

    val get_context: Context.t t
    (** Get the current context. *)





    (** {1 Tasks} *)

    type task_id

    type queue =
        | ReadyQ              (** Ready queue *)
        | TaskQ of task_id    (** Waiting for the task to finish *)
        | HoleQ    of hole_id (** Waiting for the hole to be filled *)
        | ContextQ of context_id (** Waiting for all holes of a context to be
                                     filled *)
    (** Identify the queue to insert a task *)

    val make_task: queue -> unit t -> task_id t
    (** Make a new task within a queue based on an action. *)

    val current_task: task_id t
    (** The current task. *)




    (** {1 Run} *)

    type error =
        | Blocked
        | Normal of Error.t

    val run:
        Environment.t
        -> Context.t
        -> unit t ->
        (Environment.t, error) result
        (** [run env ctxt action] Run [action] in the environment [env] and
            context [ctxt]. *)
end
