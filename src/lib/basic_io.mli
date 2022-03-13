open Fmlib_std

module type SOURCE = Fmlib_std.Interfaces.SOURCE

type ('a, 'e) t

val return:   'a -> ('a, 'e) t

val fail:     'e -> ('a, 'e) t

val (>>=):    ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

val ( let* ): ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

val catch:    ('e -> ('a, 'f) t) -> ('a, 'e) t -> ('a, 'f) t

val map2: ('a -> 'b) -> ('e -> 'f) -> ('a, 'e) t -> ('b, 'f) t

val map: ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t

val map_error: ('e -> 'f) -> ('a, 'e) t -> ('a, 'f) t


val run: (int, Void.t) t -> int


val getcwd: (string, string) t
val readdir: string -> (string array, string) t
val mkdir: string -> int -> (unit, string) t
val rmdir: string -> (unit, string) t
val is_directory: string -> (bool, string) t
val remove: string -> (unit, string) t
val rename: string -> string -> (unit, string) t


val path_separator: (char, Void.t) t
val path_delimiter: (char, Void.t) t

val err_out: char -> (unit, Void.t) t


module Write (Source: SOURCE with type item = char):
sig
    val err_out: Source.t -> (unit, Void.t) t
end
