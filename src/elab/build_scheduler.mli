open Core

(** {1 Monad} *)


type _ t

val return: 'a -> 'a t
val fail:   Error.t -> 'a t
val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
val ( >>=  ): 'a t -> ('a -> 'b t) -> 'b t
val map: ('a -> 'b) -> 'a t -> 'b t
val int_fold_left:  int -> 'a t -> (int -> 'a -> 'a t) -> 'a t
val int_fold_right: int -> (int -> 'a -> 'a t) -> 'a t -> 'a t




(** {1 Run} *)

type error =
    | Blocked
    | Normal of Error.t


val run:
    Context.t
    -> unit t
    -> (Context.t, error) result
(** [run ctxt action] Run [action] in the context [ctxt]

*)



(** {1 Holes} *)

type hole_id

val make_hole: Hole_spec.t -> hole_id t
(** Make a new hole with a specification in the current context. *)


val get_hole_spec: hole_id -> Hole_spec.t t
(** Get the specification of a hole. *)


val set_hole_spec: hole_id -> Hole_spec.t -> unit t
(** Update the specification of a hole.

    Precondition:
        The hole does not yet have any content
*)

val fill_hole: hole_id -> Hole_content.t -> unit t
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
