module Pretty = Fmlib_pretty.Print

type tick    = int
type task    = int list
type message = unit -> Pretty.doc

type t = {
    trace_flag: bool;
    messages: (tick * task * message) list;
}



let empty: t = {
    trace_flag = true;
    messages = [];
}


let ignore: t = {
    trace_flag = false;
    messages = [];
}



let add (tick: tick) (task: task) (message: message) (tr: t): t =
    if tr.trace_flag then
        {tr with
         messages = (tick, task, message) :: tr.messages
        }
    else
        tr


let doc (t: t): Fmlib_pretty.Print.doc =
    let open Fmlib_pretty.Print
    in
    let entry (tick, task, doc) =
        let open Printf
        in
        let hdr =
            sprintf "%d [%s]:"
                tick
                (String.concat
                     ","
                     (List.rev_map (sprintf "%d") task))
        in
        group (text hdr <+> space <+> nest 4 (doc ()))
        <+> cut
    in
    cat (List.rev_map entry t.messages)
