module type ANY = Fmlib_std.Interfaces.ANY

module Make
        (Environment: ANY)
        (Context: ANY)
        (Spec: ANY)
        (Content: ANY)
        (Error: ANY):
sig

    (** {1 Types} *)

    type task_id
    type hole_id
    type context_id

    type queue =
        | ReadyQ
        | HoleQ    of hole_id
        | TaskQ    of task_id
        | ContextQ of context_id


    type error =
        | Blocked
        | Normal of Error.t


    (** {1 Monad} *)


    type _ t

    val return: 'a -> 'a t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
    val ( >>=  ): 'a t -> ('a -> 'b t) -> 'b t



    (** {1 Holes} *)

    val make_hole: Spec.t -> hole_id t
    (** Make a new hole with a specification. *)


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




    (** {1 Tasks} *)

    val make_task: queue -> unit t -> task_id t
    (** Make a new task within a queue based on an action. *)




    (** {1 Run} *)

    val run:
        Environment.t
        -> Context.t
        -> unit t ->
        (Environment.t, error) result
    (** Run an action within an environment. *)
end
