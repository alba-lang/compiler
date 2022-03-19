open Fmlib_std
open Fmlib_parse

module B = Basic_io

module type SOURCE = Fmlib_std.Interfaces.SOURCE
module type SINK   = Fmlib_std.Interfaces.SINK

type doc       = Fmlib_pretty.Print.doc
type doc_thunk = unit -> doc

type in_channel  = B.in_channel
type out_channel = B.out_channel

type 'a t = ('a, doc_thunk) B.t


(* Generic Combinators
 * ===================
 *)

let return: 'a -> 'a t =
    B.return

let fail: doc_thunk -> 'a t =
    B.fail

let (>>=): 'a t -> ('a -> 'b t) -> 'b t =
    B.(>>=)

let ( let* ) = (>>=)

let map: ('a -> 'b) -> 'a t -> 'b t =
    B.map

let catch (f: doc_thunk -> 'a t) (m: 'a t): 'a t =
    B.catch f m


let apply_basic (e: doc_thunk) (m: ('a, _) B.t): 'a t =
    B.map_error (fun _ -> e) m


let lift_basic (m: ('a, Void.t) B.t): 'a t =
    B.map_error Void.absurd m


let rec sequence: 'a t list -> 'a list t = function
    | [] ->
        return []
    | hd :: tl ->
        let* hd = hd in
        let* tl = sequence tl in
        return (hd :: tl)


let run (prog: unit t): int =
    let open B in
    catch
        (fun e ->
             let module W = B.Write (Pretty) in
             e ()
             |> Pretty.layout 80
             |> W.err_out
             |> map (fun _ -> 1)
        )
        (map (fun _ -> 0) prog)
    |> run




(* Paths
 * =====
 *)


let path_separator: char t =
    lift_basic B.path_separator


let path_delimiter: char t =
    lift_basic B.path_delimiter


let resolve_paths (paths: string list): string t =
    let* sep = path_separator in
    return (File_path.resolve sep paths)


let join_paths (paths: string list): string t =
    let* sep = path_separator in
    return (File_path.join sep paths)


let dirname (path: string): string t =
    let* sep = path_separator in
    File_path.dirname sep path |> return


let basename (path: string): string t =
    let* sep = path_separator in
    File_path.basename sep path |> return



(* Directory Functions
 * ===================
 *)

let getcwd: string t =
    apply_basic
        (Pretty_error.of_text "I cannot get the current directory.")
        B.getcwd


let relative_path (path: string): string t =
    let* sep = path_separator in
    let* wd  = getcwd in
    File_path.relative sep wd path |>  return


let mkdir (path: string) (perm: int): unit t =
    let str = Printf.sprintf
            "create the following directory with \
             the permissions %x"
            perm
    in
    apply_basic
        (Pretty_error.cannot_do1 str path)
        (B.mkdir path perm)


let rmdir (path: string): unit t =
    apply_basic
        (Pretty_error.cannot_do1 "remove the directory" path)
        (B.rmdir path)


let readdir (path: string): string array t =
    apply_basic
        (Pretty_error.cannot_do1 "read the directory" path)
        (B.readdir path)


let is_directory (path: string): bool t =
    apply_basic
        (Pretty_error.cannot_do1 "check if the following is a directory" path)
        (B.is_directory path)


let is_root_directory (path: string): bool t =
    let* sep = path_separator in
    File_path.is_root sep path |> return


let remove (path: string): unit t =
    apply_basic
        (Pretty_error.cannot_do1 "remove the file" path)
        (B.remove path)


let rename (old_path: string) (new_path: string): unit t =
    apply_basic
        (Pretty_error.cannot_do1 "rename the file" old_path)
        (B.rename old_path new_path)



(* Traversing a Directory
 * ======================
 *)

let fold_directory
        (enter: string -> string array -> 'a -> (bool * 'a) t)
        (file:  string -> string -> string -> 'a -> 'a t)
        (child_dir1:  string -> string -> string -> 'a -> (bool * 'a) t)
        (child_dir2:  string -> string -> string -> 'a -> 'a t)
        (start: 'a)
        (dir: string)
    : 'a t
    (* entries (file and child_dir) are called with
            parent_abs
            child
            child_abs
    *)
    =
    let rec fold a dir =
        let* es       = readdir dir in
        let* down, a  = enter dir es a in
        let n         = Array.length es in
        let rec entry i a =
            if i = n then
                return a
            else
                let* child   = join_paths [dir; es.(i)] in
                let* isdir   = is_directory child in
                let* a =
                    if isdir then
                        let* down_sub, a = child_dir1 dir es.(i) child a in
                        if down && down_sub then
                            fold a child
                            >>= child_dir2 dir es.(i) child
                        else
                            return a
                    else
                        file dir es.(i) child a
                in
                entry (i + 1) a
        in
        entry 0 a
    in
    fold start dir




(* File Functions
 * ==============
 *)

let open_in (path: string): in_channel t =
    apply_basic
        (Pretty_error.cannot_do1
                 "open the following file for reading"
                 path)
        (B.open_in path)


let close_in (path: string) (ch: in_channel): unit t =
    apply_basic
        (Pretty_error.cannot_do1
                 "close the file"
                 path)
        (B.close_in ch)


let rewind (path: string) (ch: in_channel): unit t =
    apply_basic
        (Pretty_error.cannot_do1 "Rewind the file" path)
        (B.seek_in ch 0)



(* Reading from Files
 * ==================
 *)
module Read (Sink: SINK with type item = char) =
struct
    module BR = B.Read (Sink)

    let from (path: string) (ch: in_channel) (sink: Sink.t): Sink.t t =
        apply_basic
            (Pretty_error.cannot_do1 "read a character from the file" path)
            (BR.from ch sink)
end




(* Parsing Files
 * =============
 *)

module type PARSER =
sig
    include Fmlib_parse.Interfaces.NORMAL_PARSER
        with type token = char
         and type expect = string * Indent.expectation option

    val position: t -> Position.t
    val range:    semantic -> Position.range
    val doc:      semantic -> Pretty.doc
end


module Parse (P: PARSER) =
struct
    let parse (path: string) (p: P.t): P.final t =
        let module R = Read (P) in
        let* ch = open_in path in
        let* p  = R.from path ch p in
        if P.has_succeeded p then
            let* _ = close_in path ch in
            return (P.final p)
        else
            let module ER = Error_reporter.Make (P) in
            let module R  = Read (ER) in
            let er  = ER.make P.range P.doc  p in
            let* _  = rewind path ch in
            let* er = R.from path ch er in
            let* _  = close_in path ch in
            let pos =
                if P.has_failed_syntax p then
                    P.position p
                else
                    P.(failed_semantic p |> range |> fst)
            in
            let str = Printf.sprintf
                    "%s:%d:%d Error"
                    path
                    (Position.line pos + 1)
                    (Position.column pos + 1)
            in
            Pretty.(cat [text str; cut; fill 60 '-'; cut; ER.document er])
            |> Pretty_error.of_doc
            |> fail
end
