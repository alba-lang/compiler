open Fmlib_std
open Lib

module Pretty = Fmlib_pretty.Print

module Error =
struct
    open Pretty

    type t = unit -> doc


    let of_doc (doc: doc): t =
        fun () -> doc


    let of_thunk (f: unit -> doc): t =
        f


    let of_text (str: string): t =
        fun () -> text str <+> cut


    let to_doc (er: t): doc =
        er ()


    let one_para (lst: doc list): doc =
        pack " " lst <+> cut


    let simple (lst: (int * doc list) list) (): doc =
        List.map
            (fun (i, lst) ->
                 one_para lst
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
end




module Alba_io =
struct
    module B = Basic_io

    module type SOURCE = Fmlib_std.Interfaces.SOURCE
    module type SINK   = Fmlib_std.Interfaces.SINK

    type in_channel  = B.in_channel
    type out_channel = B.out_channel

    type 'a t = ('a, Error.t) B.t


    (* Generic Combinators
     * ===================
     *)

    let return: 'a -> 'a t =
        B.return

    let fail: Error.t -> 'a t =
        B.fail

    let (>>=): 'a t -> ('a -> 'b t) -> 'b t =
        B.(>>=)

    let ( let* ) = (>>=)

    let map: ('a -> 'b) -> 'a t -> 'b t =
        B.map

    let catch (f: Error.t -> 'a t) (m: 'a t): 'a t =
        B.catch f m


    let apply_basic (e: Error.t) (m: ('a, _) B.t): 'a t =
        B.map_error (fun _ -> e) m


    let map_error (f: 'e -> Error.t) (m: ('a, 'e) B.t): 'a t =
        B.map_error f m


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
            )
            prog
        |> map (fun _ -> 0)
        |> run



    (* Directory Functions
     * ===================
     *)

    let path_separator: char t =
        lift_basic B.path_separator


    let path_delimiter: char t =
        lift_basic B.path_delimiter


    let getcwd: string t =
        apply_basic
            (Error.of_text "I cannot get the current directory.")
            B.getcwd


    let mkdir (path: string) (perm: int): unit t =
        let str = Printf.sprintf
                "create the following directory with \
                 the permissions %x"
                perm
        in
        apply_basic
            (Error.cannot_do1 str path)
            (B.mkdir path perm)


    let rmdir (path: string): unit t =
        apply_basic
            (Error.cannot_do1 "remove the directory" path)
            (B.rmdir path)


    let readdir (path: string): string array t =
        apply_basic
            (Error.cannot_do1 "read the directory" path)
            (B.readdir path)


    let is_directory (path: string): bool t =
        apply_basic
            (Error.cannot_do1 "check if the following is a directory" path)
            (B.is_directory path)


    let remove (path: string): unit t =
        apply_basic
            (Error.cannot_do1 "remove the file" path)
            (B.remove path)


    let rename (old_path: string) (new_path: string): unit t =
        apply_basic
            (Error.cannot_do1 "rename the file" old_path)
            (B.rename old_path new_path)





    (* File Functions
     * ==============
     *)

    let open_in (path: string): in_channel t =
        apply_basic
            (Error.cannot_do1
                     "open the following file for reading"
                     path)
            (B.open_in path)


    let close_in (path: string) (ch: in_channel): unit t =
        apply_basic
            (Error.cannot_do1
                     "close the file"
                     path)
            (B.close_in ch)


    let rewind (path: string) (ch: in_channel): unit t =
        apply_basic
            (Error.cannot_do1 "Rewind the file" path)
            (B.seek_in ch 0)



    (* Reading from Files
     * ==================
     *)
    module Read (Sink: SINK with type item = char) =
    struct
        module BR = B.Read (Sink)

        let from (path: string) (ch: in_channel) (sink: Sink.t): Sink.t t =
            apply_basic
                (Error.cannot_do1 "read a character from the file" path)
                (BR.from ch sink)
    end


    (* Writing to Files
     * ================
     *)
    module Write (Source: SOURCE with type item = char) =
    struct
        module BW = B.Write (Source)

        let err_out (src: Source.t): (unit, 'e) B.t =
            BW.err_out src
    end
end



module Located = Fmlib_parse.Located



module Package =
struct
    type t = unit
end


module Semantic =
struct
    type t = Fmlib_parse.Position.range * Error.t
end


module Package_parser =
struct
    open Yaml_parser
    module P = Make (Package)

    include P

    let type_decoder: string Decode.t =
        let open Decode in
        let* str = string in
        if str = "console-application" then
            return str
        else if str = "library" then
            return str
        else
            fail (fun () ->
                let open Pretty in
                wrap_words {|must be one of ["console-application", "library"]|}
                <+> cut <+> cut
            )

    let decoder: Package.t Decode.t =
        let open Decode in
        let* tp   = field "type" type_decoder in
        let* _ =
            field
                "use"
                (array located_string)
        in
        if tp = "console-application" then
            let* _ = field "main" located_string in
            return ()
        else if tp = "library" then
            let* _ =
                field
                    "export"
                    (array located_string)
            in
            return ()
        else
            assert false (* cannot happen *)

    let init: Parser.t =
        of_decoder decoder
end





module Alba_program =
struct
    include Alba_io


    let alba_project_string: string = ".alba_project"
    let alba_package_string: string = "alba-package.yml"


    let resolve_paths (paths: string list): string t =
        let* sep = path_separator in
        return (File_path.resolve sep paths)


    let join_paths (paths: string list): string t =
        let* sep = path_separator in
        return (File_path.join sep paths)


    let fold_directory
            (enter: string -> string array -> 'a -> (bool * 'a) t)
            (file:  string -> string -> string -> 'a -> 'a t)
            (child_dir:   string -> string -> string -> 'a -> 'a t)
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
                            let* a = child_dir dir es.(i) child a in
                            if down then
                                fold a child
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


    let get_work_dir (wdir: string): string t =
        let* cwd = getcwd in
        resolve_paths [cwd; wdir]


    let parse_package_yml (dir: string): Package.t t =
        let* file = join_paths [dir; alba_package_string] in
        let* ch   = open_in file in
        let module P = Package_parser.Parser in
        let module R = Read (P) in
        let* p    = R.from file ch Package_parser.init in
        if P.has_succeeded p then
            return (P.final p)
        else
            let module ER = Fmlib_parse.Error_reporter.Make (P) in
            let module R = Read (ER) in
            let er  = ER.make P.range P.doc p in
            let* _  = rewind file ch in
            let* er = R.from file ch er in
            fail (Error.of_doc (ER.document er))


    let check_nested_roots (wdir: string) (wdir_abs: string): unit t =
        (* Precondition: [wdir] is a directory. *)
        fold_directory
            (fun dir es () ->
                 match Array.find ((=) alba_project_string) es with
                 | None ->
                     return (true, ())
                 | Some _ ->
                     fail (Error.nested_project wdir dir))
            (fun _ _ _  -> return)
            (fun _ _ _  -> return)
            ()
            wdir_abs



    let collect_packages (wdir_abs: string): string list t =
        fold_directory
            (fun dir es lst ->
                 match Array.find ((=) alba_package_string) es with
                 | None ->
                     return (true, lst)
                 | Some _ ->
                     return (false, dir :: lst)
            )
            (fun _ _ _ -> return)
            (fun _ _ _ -> return)
            []
            wdir_abs



    let find_root (wdir_abs: string): string option t =
        let rec find dir files =
            match
                Array.find (fun file -> file = alba_project_string) files
            with
            | None ->
                let* sep = path_separator in
                if dir = String.one sep then
                    return None
                else
                    let  dir   = File_path.dirname sep dir in
                    let* files =
                        readdir dir in
                    find dir files
            | Some _ ->
                return (Some dir)
        in
        let* files = readdir wdir_abs
        in
        find wdir_abs files


    let compile_action (wdir: string): unit t =
        let* wdir_abs = get_work_dir wdir in
        let* root = find_root wdir_abs in
        match root with
        | None ->
            fail (Error.no_root_found wdir)
        | Some root ->
            let* pkg_dirs = collect_packages root in
            let* pkgs =
                sequence
                    (List.map parse_package_yml pkg_dirs)
            in
            let _ = pkgs in
            assert false


    let init_action (wdir: string): unit t =
        let* wdir_abs = get_work_dir wdir in
        let* root = find_root wdir_abs in
        match root with
        | None ->
            let* _ = check_nested_roots wdir wdir_abs in
            let* path = join_paths [wdir_abs; alba_project_string] in
            mkdir path 0x755
        | Some root ->
            fail (Error.already_project wdir root)


    let compile (wdir: string): int =
        compile_action wdir |> run


    let init (wdir: string): int =
        init_action wdir |> run
end








(* The main program
 * ================
 *)
let _ =
    let open Cmdliner in

    let wdir_arg =
        Arg.(
            value
            & opt string "."
            & info
                ["work-dir"]
                ~docv:"working directory"
                ~doc:"Working directory"
        )
    in
    let compile_term = Term.(const Alba_program.compile $ wdir_arg)
    and init_term    = Term.(const Alba_program.init $ wdir_arg)
    in
    let compile_cmd = Cmd.(
        v (info "compile" ~doc:"compile the alba project") compile_term
    )
    and init_cmd = Cmd.(
        v (info "init" ~doc:"initialize an alba project") init_term
    )
    in
    let cmds = Cmd.(
        group
            ~default:compile_term
            (info "alba" ~doc:"The Alba Compiler")
            [compile_cmd; init_cmd]
    )
    in
    exit Cmd.(eval' cmds)
