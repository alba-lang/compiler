module Make:
sig
    type task_id
    type hole_id
    type context_id

    type queue =
        | ReadyQ
        | HoleQ    of hole_id
        | TaskQ    of task_id
        | ContextQ of context_id

    type environment

    type error

    type _ t

    val return: 'a -> 'a t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
    val ( >>=  ): 'a t -> ('a -> 'b t) -> 'b t

    val make_task: queue -> unit t -> task_id t
    val run: environment -> unit t -> (environment, error) result
end
