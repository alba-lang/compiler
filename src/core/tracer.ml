module Pretty = Fmlib_pretty.Print

type tick    = int
type task    = int list
type message = unit -> Pretty.doc

type entry =
    tick * task * int * message

type t = {
    trace_flag: bool;
    messages: entry list;
}



let empty: t = {
    trace_flag = true;
    messages = [];
}


let ignore: t = {
    trace_flag = false;
    messages = [];
}



let add (tick: tick) (task: task) (nchilds: int) (message: message) (tr: t): t =
    if tr.trace_flag then
        {tr with
         messages = (tick, task, nchilds, message) :: tr.messages
        }
    else
        tr


let count (t: t): int =
    List.length t.messages



let doc (t: t): Pretty.doc =
    let open Fmlib_pretty.Print
    in
    let entry (tick, task, _, doc) =
        let open Printf
        in
        let level = List.length task in
        let hdr =
            if level = 0 then
                sprintf "@%d" tick
            else
                sprintf "@%d %s:"
                    tick
                    (String.concat
                         "."
                         (List.rev_map (sprintf "%d") task))
        in
        group (
            text hdr
            <+> space
            <+> nest 4 (doc ())
        ) |> nest (2 * level)
        <+> cut
    in
    cat (List.rev_map entry t.messages)




module Tree =
struct
    module Abuf = Std.Array_buffer

    type t =
        {
            blocks: message list Abuf.t;
            childs: t Abuf.t;
        }

    let make (): t = {
        blocks = Abuf.make ();
        childs = Abuf.make ();
    }

    let insert (_, path, nchilds, msg: entry) (tree: t): unit =
        let rec ins tree = function
            | [] ->

                let len = Abuf.length tree.blocks in
                if nchilds < len then
                    Abuf.update
                        tree.blocks
                        nchilds
                        (fun lst -> msg :: lst)
                else
                    begin
                        Abuf.push_many tree.blocks (nchilds - len) [];
                        Abuf.push tree.blocks [msg]
                    end

            | i :: path ->

                let len = Abuf.length tree.childs
                in
                if len <= i then
                    for _ = len to i do
                        Abuf.push tree.childs (make ())
                    done;

                ins (Abuf.get tree.childs i) path

        in
        ins tree (List.rev path)



    let doc_of_messages
            (depth: int)
            (doc: Pretty.doc)
            (ms: message list)
        : Pretty.doc
        =
        List.fold_left
            (fun doc msg -> Pretty.(
                 doc
                 <+> cut <+> cut
                 <+> (msg () |> nest (4 * depth))
                 )
            )
            doc
            ms



    let doc_of_tree (t: t): Pretty.doc =
        let rec make_doc depth i doc t =
            let nblocks = Abuf.length t.blocks
            and nchilds = Abuf.length t.childs
            in
            let doc =
                if i < nblocks then
                    doc_of_messages depth doc (Abuf.get t.blocks i)
                else
                    doc
            in
            let doc =
                if i < nchilds then
                    make_doc (depth + 1) 0 doc (Abuf.get t.childs i)
                else
                    doc
            in
            if i < nblocks || i < nchilds then
                make_doc depth (i + 1) doc t
            else
                doc
        in
        make_doc 0 0 Pretty.(cut <+> text "Sorted Trace") t



    let doc (lst: entry list): Pretty.doc =
        let tree = make () in
        List.iter (fun e -> insert e tree) lst;
        doc_of_tree tree
end


let doc_sorted (t: t): Pretty.doc =
    Tree.doc t.messages
