open Fmlib_pretty

type doc       = Print.doc
type doc_thunk = unit -> doc

open Print

type t = unit -> doc


let of_doc (doc: doc): t =
    fun () -> doc


let of_thunk (f: unit -> doc): t =
    f


let of_text (str: string): t =
    fun () -> text str <+> cut


let paragraph (lst: doc list): doc =
    pack " " lst <+> cut


let simple (lst: (int * doc list) list) (): doc =
    List.map
        (fun (i, lst) ->
             paragraph lst
             |> nest i
        )
        lst
    |> paragraphs
    <+> cut


let unexpected (str: string): t =
    [
        0, [wrap_words
                "The following unexpected system error occurred"];
        4,  [text str];
    ]
    |> simple

let no_root_found (wdir: string): t =
    [
        0, [wrap_words "The directory"];
        4, [text wdir];
        0, [wrap_words "is not within an alba project. Use the \
                        following command to initialize an alba \
                        project"];
        4, [text "alba init"];
    ]
    |> simple


let already_project (wdir: string) (root: string): t =
    [
        0, [wrap_words "The directory"];
        4, [text wdir];
        0, [wrap_words "already is part of an alba project rooted \
                        at the directory:"];
        4, [text root];
    ]
    |> simple


let nested_project (wdir: string) (dir: string): t =
    [
        0, [wrap_words "The directory"];
        4, [text wdir];
        0, [wrap_words "cannot be the root of an alba project because \
                        it already contains an alba project at"];
        4, [text dir];
    ]
    |> simple


let cannot_do (what: string) (path: string) (reason: string): t =
    [
        0, [wrap_words "I cannot"; wrap_words what];
        4, [text path];
        0, [wrap_words reason];
    ]
    |> simple


let cannot_do1 (what: string) (path: string): t =
    cannot_do what path
        "maybe it does not exist or you don't have the permissions \
        to do that."


let cannot_do2 (what: string) (path: string): t =
    cannot_do what path
        "maybe it don't have the permissions \
        to do that."


let expecting_one_of (lst: string list) (): doc =
    let expect =
        "I was expecting"
        ^
        (
            match lst with
            | [_] -> ""
            | _   -> " one of"
        )
    in
    [
        wrap_words expect <+> cut;

        List.map (fun str -> text "- " <+> text str) lst
        |> stack ""
        |> nest 4
        <+> cut;
    ]
    |> paragraphs
    <+> cut
