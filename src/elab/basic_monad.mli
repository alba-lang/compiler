(** Basic Elaboration Monad *)



open Std
open Core

type doc = Fmlib_pretty.Print.doc


module type ANY = Fmlib_std.Interfaces.ANY




module Make (Final: ANY):
sig
    (** {1 Basics} *)


    type meta = Welltyped.Basic.meta
                    (** Representation of a metavariable. *)


    type 'a t (** ['a t] is an elaborator which elaborates an object of type
                  ['a]. *)


    val run: bool -> Globals.t -> Final.t t -> doc * (Final.t, Error.t) result
    (** [run trace_flg globals elab]

        Elaborate an object of type [Final.t] using the elaborator [elab] in the
        global environment [globals]. Trace the elaboration if the trace flag is
        set.
    *)






    (** {1 Basic Monad} *)


    val return: 'a      -> 'a t

    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
    val ( >>=  ): 'a t -> ('a -> 'b t) -> 'b t

    val ( >=>  ): ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)

    val map: ('a -> 'b) -> 'a t -> 'b t






    (** {1 Metavariables}

        All metavariables belong to a certain context. A context is represented
        in the basic elaboration monad as an id.
    *)


    val new_context: int t
    (** [new_context]

        Make a new context and return the id of the context.
    *)



    val new_meta: int -> meta -> int t
    (** [new_meta id meta]

        Make a new metavariable [meta] in the context with [id] and return the
        id of the metavariable in that context.
    *)



    val update_meta: int -> int -> (meta -> meta) -> unit t
    (** [update_meta ctxt id f]

        Update the metavariable [id] in the context [ctxt] by the function [f].
        Put all tasks which are waiting for an update of the metavariable into
        the ready queue.
    *)





    (** {1 Task Management} *)



    val spawn: unit t -> unit t
    (** [spawn task]

        Add [task] to the ready queue. The scheduler runs a task on the ready
        queue as soon as the current task gets blocked.
    *)



    val wait_update: int -> int -> (meta -> unit t) -> unit t
    (** [wait_update ctxt id f]

        Wait for an update of the metavariable [(ctxt,id)]. If an update of the
        metavariable happened then spawn the task [f meta] with the
        new value [meta] of the metavariable.
    *)



    val wait_one_of: (int * int) array -> (int -> meta -> unit t) -> unit t
    (** [wait_one_of arr f]

        Wait for an update of one of the metavariables of the array [arr]. If an
        update of one of the metavariables happened, say the [i-]th in the
        array, then put the task [f i meta] into the ready queue with the new
        value [meta] of the [i]-th metavariable.
    *)





    (** {1 Logging} *)


    val log: (unit -> doc) -> unit t
    (** [log doc]

        Enter [doc] to the log (if the logflag is set).
    *)
end
