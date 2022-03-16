open Fmlib_std.Interfaces
open Fmlib_parse


module String_map:
    MAP with type key = string




module Error:
sig
    type doc         = Fmlib_pretty.Print.doc

    type range_thunk = unit -> Position.range
    type doc_thunk   = unit -> doc

    type t = range_thunk * doc_thunk

    val range: t -> Position.range

    val doc:   t -> doc
end


(** A datatype representing yaml values *)
module Yaml:
sig
    type scalar = string Located.t

    type t =
        | Scalar of scalar
        | Seq  of sequence
        | Map  of record

    and sequence = Position.t * t array

    and record   = Position.t * (scalar * t) String_map.t * (scalar * t) list

    val to_json_string: t -> string
end



(** A decoder for yaml values *)
module Decode:
sig
    type 'a t

    val decode: Yaml.t -> 'a t -> ('a, Error.t) result 

    val return: 'a -> 'a t

    val fail: Error.t -> 'a t

    val (>>=):   'a t -> ('a -> 'b t) -> 'b t

    val (let* ): 'a t -> ('a -> 'b t) -> 'b t

    val map: ('a -> 'b) -> 'a t -> 'b t

    val yaml: Yaml.t t

    val json_string: string t

    val string: string t

    val int: int t

    val bool: bool t

    val float: float t

    val field: string -> 'a t -> 'a t

    val element: int -> 'a t -> 'a t

    val array: 'a t -> 'a array t
end


(** The yaml parser *)
module Make (Final: ANY):
sig
    module Parser:
    sig
        include Fmlib_parse.Interfaces.NORMAL_PARSER
            with type token    = char
             and type final    = Final.t
             and type expect   = string * Indent.expectation option
             and type semantic = Error.t
    end

    val of_decoder: Final.t Decode.t -> Parser.t
    (** [of_decoder d]

        Make a yaml parser which parses yaml structures which can be decoded by
        the decoder [d].
    *)
end
