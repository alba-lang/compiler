open Fmlib_std
open Lib

module type ANY = Fmlib_std.Interfaces.ANY

module Pretty = Fmlib_pretty.Print

module Located = Fmlib_parse.Located



module Package =
struct
    type t = unit
end


module Semantic =
struct
    type t = Fmlib_parse.Position.range * Pretty_error.doc_thunk
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


    let alba_project_string: string = ".alba-project"
    let alba_package_string: string = "alba-package.yml"


    let get_work_dir (wdir: string): string t =
        let* cwd = getcwd in
        resolve_paths [cwd; wdir]


    let parse_package_yml (dir: string): Package.t t =
        let module P = Parse (Package_parser.Parser) in
        let* file    = join_paths [dir; alba_package_string] in
        P.parse
            file
            Package_parser.init



    let check_nested_roots (wdir: string): unit t =
        (* Precondition: [wdir] is a directory. *)
        fold_directory
            (fun dir es () ->
                 match Array.find ((=) alba_project_string) es with
                 | None ->
                     return (true, ())
                 | Some _ ->
                     fail (Pretty_error.nested_project wdir dir))
            (fun _ _ _  -> return)
            (fun _ _ _  a -> return (true, a))
            ()
            wdir



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
            (fun _ name _ a ->
                 if name = alba_project_string then
                     return (false, a)
                 else
                     return (true, a))
            []
            wdir_abs



    let find_root (wdir: string): string option t =
        let* cd = getcwd in
        let* dir= resolve_paths [cd; wdir] in
        let rec find dir =
            let* files = readdir dir in
            match Array.find ((=) alba_project_string) files with
            | None ->
                let* is_top = is_root_directory dir in
                if is_top then
                    return None
                else
                    let* upper = dirname dir in
                    find upper
            | Some _ ->
                return (Some dir)
        in
        find dir


    let compile_action (wdir: string): unit t =
        let* wdir_abs = get_work_dir wdir in
        let* root = find_root wdir_abs in
        match root with
        | None ->
            fail (Pretty_error.no_root_found wdir)
        | Some root ->
            let* pkg_dirs = collect_packages root in
            let* pkgs =
                sequence
                    (List.map parse_package_yml pkg_dirs)
            in
            let _ = pkgs in
            assert false


    let init_action (wdir: string): unit t =
        let* root = find_root wdir in
        match root with
        | None ->
            let* _ = check_nested_roots wdir in
            let* path = join_paths [wdir; alba_project_string] in
            mkdir path 0x755
        | Some root ->
            let* rel_root = relative_path root in
            fail (Pretty_error.already_project wdir rel_root)


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
