module type ANY    = Fmlib_std.Interfaces.ANY
module type SOURCE = Fmlib_std.Interfaces.SOURCE
module type SINK   = Fmlib_std.Interfaces.SINK

module Void = Fmlib_std.Void

module Make (E: ANY):
sig
    module B = Basic_io

    type in_channel  = B.in_channel
    type out_channel = B.out_channel

    type 'a t = ('a, E.t) B.t

    val return: 'a -> 'a t
    val fail:   E.t -> 'a t

    val (>>=):    'a t -> ('a -> 'b t) -> 'b t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t

    val run: (E.t -> (int, Void.t) B.t) -> unit t -> int

    val getcwd: (string -> E.t) -> string t

    val readdir: (string -> E.t) -> string -> string array t

    val mkdir: (string -> E.t) -> string -> int -> unit t
    val rmdir: (string -> E.t) -> string -> unit t
    val is_directory: (string -> E.t) -> string -> bool t
    val remove: (string -> E.t) -> string -> unit t
    val rename: (string -> E.t) -> string -> string -> unit t

    val path_separator: char t

    val path_delimiter: char t

    val resolve_paths: string list -> string t

    val open_in: (string -> E.t) -> string -> in_channel t
    val seek_in: (string -> E.t) -> in_channel -> int -> unit t
    val close_in: (string -> E.t) -> in_channel -> unit t

    module Write (Source: SOURCE with type item = char):
    sig
        val err_out: Source.t -> (unit, Void.t) Basic_io.t
    end


    module Read (Sink: SINK with type item = char):
    sig
        val from: (string -> E.t) -> in_channel -> Sink.t -> Sink.t t
    end
end
