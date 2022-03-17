open Fmlib_std

module type SOURCE = Fmlib_std.Interfaces.SOURCE
module type SINK   = Fmlib_std.Interfaces.SINK

type in_channel  = Stdlib.in_channel
type out_channel = Stdlib.out_channel

type ('a, 'e) t = unit -> ('a, 'e) result

let return (type a e) (a: a): (a, e) t =
    fun () -> Ok a

let fail (type a e) (e: e): (a, e) t =
    fun () -> Error e

let (>>=) (type a b e) (m: (a, e) t) (f: a -> (b, e) t): (b, e) t =
    fun () ->
    match m () with
    | Ok a ->
        f a ()
    | Error e ->
        Error e

let catch (type a e f) (f: e -> (a, f) t) (m: (a, e) t): (a, f) t =
    fun () ->
    match m () with
    | Ok a ->
        Ok a
    | Error e ->
        f e ()

let ( let* ) = (>>=)

let map2 (type a b e f) (f: a ->  b) (g: e -> f) (m: (a, e) t): (b, f) t =
    fun () ->
    match m () with
    | Ok a ->
        Ok (f a)
    | Error e ->
        Error (g e)

let map_error (type a e f) (f: e -> f) (m: (a, e) t): (a, f) t =
    map2 Fun.id f m


let map (type a b e) (f: a -> b) (m: (a, e) t): (b, e) t =
    map2 f Fun.id m


let run (m: (int, Void.t) t): int =
    match m () with
    | Ok code ->
        code
    | Error _ ->
        assert false (* cannot happen *)


let sys (type a) (cmd: unit -> a): (a, string) t =
    (* Execute a command [Sys.cmd] *)
    fun () ->
    try
        Ok (cmd ())
    with Sys_error str ->
        Error str


let unix (cmd: unit -> 'a): ('a, string) t =
    (* Execute a unix command. *)
    let open Unix in
    fun () ->
    try
        Ok (cmd ())
    with Unix_error (code, name, para) ->
        Error (
            error_message code
            ^ ", function name: " ^ name
            ^ ", parameter: " ^ para
        )


let getcwd: (string, string) t =
    sys Sys.getcwd


let readdir (path: string): (string array, string) t =
    sys (fun () -> Sys.readdir path)


let mkdir (path: string) (perm: int): (unit, string) t =
    unix (fun () -> Unix.mkdir path perm)


let rmdir (path: string): (unit, string) t =
    unix (fun () -> Unix.rmdir path)


let is_directory (path: string): (bool, string) t =
    sys (fun () -> Sys.is_directory path)

let remove (path: string): (unit, string) t =
    sys (fun () -> Sys.remove path)

let rename (old_path: string) (new_path: string): (unit, string) t =
    sys (fun () -> Sys.rename old_path new_path)


let path_separator: (char, Void.t) t =
    fun () ->
    if Sys.win32 then
        Ok '\\'
    else
        Ok '/'


let path_delimiter: (char, Void.t) t =
    fun () ->
    if Sys.win32 then
        Ok ';'
    else
        Ok ':'


let open_in (path: string): (in_channel, string) t =
    sys (fun () -> open_in path)


let close_in (ch: in_channel): (unit, string) t =
    sys (fun () -> close_in ch)


let seek_in (ch: in_channel) (pos: int): (unit, string) t =
    sys (fun () -> seek_in ch pos)


let input_char (ch: in_channel): (char option, string) t =
    fun () ->
    try
        Ok (Some (input_char ch))
    with
    | End_of_file ->
        Ok None
    | Sys_error str ->
        Error str


let err_out (c: char): (unit, 'e) t =
    fun () ->
    try
        Ok (prerr_char c)
    with Sys_error _ ->
        Ok ()


module Write (Source: SOURCE with type item = char) =
struct
    let err_out (src: Source.t): (unit, 'e) t =
        let rec out src =
            if Source.has_more src then
                let* _ = Source.peek src |> err_out in
                Source.advance src |> out
            else
                return ()
        in
        out src
end


module Read (Sink: SINK with type item = char) =
struct
    let from (ch: in_channel) (sink: Sink.t): (Sink.t, string) t =
        fun () ->
        try
            let str = Stream.of_channel ch
            and sinkr = ref sink
            in
            try
                while Sink.needs_more !sinkr do
                    sinkr := Sink.put (Stream.next str) !sinkr;
                done;
                Ok !sinkr
            with Stream.Failure ->
                Ok (Sink.put_end !sinkr)
        with Sys_error str ->
            Error str
end
