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
end


module Io = Io.Make (Error)

let alba_error (e: Error.t): (int, Void.t) Basic_io.t =
    let module Write = Io.Write (Pretty) in
    e ()
    |> Pretty.layout 80
    |> Write.err_out
    |> Basic_io.map (fun () -> 0)









module Alba_program =
struct
    module Io = Lib.Io.Make (Error)
    open   Io

    let alba_project_string: string = ".alba_project"
    let alba_package_string: string = "alba-package.yml"


    let readdir (path: string): string array t =
        Io.readdir
            (fun _ ->
                 Error.cannot_do1 "read the directory" path)
            path

    let is_directory (path: string): bool t =
        Io.is_directory
            (fun _ ->
                 Error.cannot_do1
                     "check if the following is a directory"
                     path)
            path


    let resolve_paths (paths: string list): string t =
        let* sep = path_separator in
        return (File_path.resolve sep paths)


    let join_paths (paths: string list): string t =
        let* sep = path_separator in
        return (File_path.join sep paths)


    let run (prog: unit t): int =
        run
            (fun e ->
                let module Write = Write (Pretty) in
                e ()
                |> Pretty.layout 80
                |> Write.err_out
                |> Basic_io.map (fun () -> 0)
            )
            prog


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
        let* cwd = Io.getcwd Error.unexpected in
        resolve_paths [cwd; wdir]



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
            let* _ = collect_packages root in
            assert false


    let init_action (wdir: string): unit t =
        let* wdir_abs = get_work_dir wdir in
        let* root = find_root wdir_abs in
        match root with
        | None ->
            let* _ = check_nested_roots wdir wdir_abs in
            let* path = join_paths [wdir_abs; alba_project_string] in
            mkdir
                Error.unexpected
                path
                0x755
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
