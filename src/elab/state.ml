open Std
open Core





type t = {
    mutable ready: (t -> unit) list;
    mutable n_ids: int;

    contexts: context Array_buffer.t
}

and context =
    {
        mutable n_open: int;
        metas: meta Array_buffer.t;
    }

and meta = {
    req: Checker.req;
    mutable report: (t -> unit) option; (* optional report action, in case the
                                           metavariable cannot be instantiated.
                                         *)
}




let make_context (): context = {
    n_open = 0;
    metas = Array_buffer.make ();
}


let make (): t = {
    ready = [];
    n_ids = 0;
    contexts = Array_buffer.make ();
}




let new_id (s: t): int =
    let id = s.n_ids in
    s.n_ids <- id + 1;
    id



let new_context (s: t): int =
    let open Array_buffer in
    let id = length s.contexts in
    push s.contexts (make_context ());
    id



let new_meta
        (report: (t -> unit) option) (req: Checker.req) (ctxt: int) (s: t)
    : int
    =
    let open Array_buffer in
    assert (ctxt < length s.contexts);
    let c = get s.contexts ctxt in
    let id = length c.metas in
    push c.metas {req; report;};
    id




let rec execute (s: t): unit =
    match s.ready with
    | [] ->
        (* scan the blocked tasks for alternatives *)

        (* there shall always be alternatives!! *)

        assert false (* shall never happen *)

    | f :: ready ->
        s.ready <- ready;
        f s;
        execute s
