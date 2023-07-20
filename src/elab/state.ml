open Std


type meta

type context =
    meta Array_buffer.t


type t = {
    mutable ready: (t -> unit) list;
    mutable n_ids: int;

    contexts: context Array_buffer.t
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
    push s.contexts (make ());
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
