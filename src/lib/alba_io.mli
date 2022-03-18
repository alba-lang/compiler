open Fmlib_parse

module type SOURCE = Fmlib_std.Interfaces.SOURCE
module type SINK   = Fmlib_std.Interfaces.SINK

type doc       = Fmlib_pretty.Print.doc
type doc_thunk = unit -> doc

type in_channel
type out_channel


type 'a t

val return: 'a -> 'a t
val fail: doc_thunk -> 'a t
val (>>=):   'a t -> ('a -> 'b t) -> 'b t
val (let* ): 'a t -> ('a -> 'b t) -> 'b t
val map: ('a -> 'b) -> 'a t -> 'b t
val catch: (doc_thunk -> 'a t) -> 'a t -> 'a t

val sequence: 'a t list -> 'a list t

val run: unit t -> int


val path_separator: char t
val path_delimiter: char t

val resolve_paths: string list -> string t
val join_paths:    string list -> string t
val relative_path: string -> string -> string t

val getcwd: string t
val mkdir: string -> int -> unit t
val rmdir: string -> unit t
val readdir: string -> string array t
val is_directory: string -> bool t

val remove: string -> unit t
val rename: string -> string -> unit t

val fold_directory:
    (string -> string array -> 'a -> (bool * 'a) t)
    -> (string -> string -> string -> 'a -> 'a t)
    -> (string -> string -> string -> 'a -> (bool * 'a) t)
    -> 'a
    -> string
    -> 'a t



val open_in:  string -> in_channel t
val close_in: string -> in_channel -> unit t
val rewind:   string -> in_channel -> unit t



module Read (Sink: SINK with type item = char):
sig
    val from: string -> in_channel -> Sink.t -> Sink.t t
end


module type PARSER =
sig
    include Interfaces.NORMAL_PARSER
        with type token = char
         and type expect = string * Indent.expectation option

    val position: t -> Position.t
    val range:    semantic -> Position.range
    val doc:      semantic -> doc
end

module Parse (P: PARSER):
sig
    val parse: string -> P.t -> P.final t
end
