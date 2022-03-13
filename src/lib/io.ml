module type ANY    = Fmlib_std.Interfaces.ANY
module type SOURCE = Fmlib_std.Interfaces.SOURCE

module Void = Fmlib_std.Void

module Make (E: ANY) =
struct
    module B = Basic_io

    type 'a t = ('a, E.t) B.t

    let return: type a. a -> a t =
        B.return

    let fail: type a . E.t -> a t =
        B.fail

    let (>>=): type a b.  a t -> (a -> b t) -> b t =
        B.(>>=)

    let ( let* ) = (>>=)

    let map_void (type a) (m: (a, Void.t) B.t): a t =
        B.map_error Void.absurd m


    let apply_basic (f: string -> E.t) (m: ('a, string) B.t): 'a t =
        B.(map_error f m)


    let run (f: E.t -> (int, Void.t) B.t) (m: unit t): int =
        let open B in
        catch
            f
            (map (fun () -> 0) m)
        |> run



    let getcwd (f: string -> E.t): string t =
        apply_basic f B.getcwd


    let readdir (f: string -> E.t) (path: string): string array t =
        apply_basic f (B.readdir path)


    let mkdir (f: string -> E.t) (path: string) (perm: int): unit t =
        apply_basic f (B.mkdir path perm)


    let rmdir (f: string -> E.t) (path: string): unit t =
        apply_basic f (B.rmdir path)


    let is_directory (f: string -> E.t) (path: string): bool t =
        apply_basic f (B.is_directory path)


    let remove (f: string -> E.t) (path: string): unit t =
        apply_basic f (B.remove path)


    let rename
            (f: string -> E.t) (old_path: string) (new_path: string)
        : unit t
        =
        apply_basic f (B.rename old_path new_path)


    let path_separator = map_void B.path_separator

    let path_delimiter = map_void B.path_delimiter

    let resolve_paths (lst: string list): string t =
        let* sep = path_separator in
        File_path.resolve sep lst |> return


    module Write (Source: SOURCE with type item = char) =
    struct
        let err_out (src: Source.t): (unit, Void.t) B.t =
            let module Write = B.Write (Source) in
            Write.err_out src
    end
end
