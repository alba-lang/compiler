open Fmlib_parse

module type ANY = Fmlib_std.Interfaces.ANY


type range = Position.range

type 'a located = range * 'a

type doc = Fmlib_pretty.Print.doc

module type ELABORATOR =
sig
    type t
    type error
    type term
    type universe_term
    type formal_argument

    val range_of_semantic: error -> range
    val doc_of_semantic:   error -> doc



    module Universe:
    sig
        val id: range -> string -> universe_term
        val fixed: range -> int -> universe_term
        val max: Position.t -> universe_term list -> universe_term
        val above: universe_term  -> Position.t -> universe_term
        val parens_around:
            Position.t -> Position.t -> universe_term
            -> universe_term
    end

    val prop: range -> term
    val level: range -> term
    val any: range -> universe_term option -> term
    val name_term: range -> Name.t -> term
    val string_term: range -> string -> term
    val char_term:   range -> string -> term
    val decimal_term: range -> string -> term
    val float_term: range -> string -> term
    val tuple_term: range -> term list -> term
    val list_term:  range -> term list -> term
    val application: term -> term list -> term
    val parens_term: Position.t -> Position.t -> term -> term
    val implicit_argument: Position.t -> Position.t -> term -> term

    val unary_expression:
        range
        -> string
        -> Precedence.t
        -> term
        -> term

    val binary_expression:
        term
        -> range
        -> string
        -> Precedence.t
        -> term
        -> term

    val formal_argument_simple: range -> Name.t -> formal_argument

    val formal_argument:
        bool
        -> Name.t located list
        -> term option
        -> formal_argument


    val product_expression:
        Position.t
        -> formal_argument list
        -> term (* result type *)
        -> term


    val lambda_expression:
        Position.t
        -> formal_argument list
        -> term option          (* result type *)
        -> term                 (* body *)
        -> term

    val add_definition:
            Name.t located
            -> formal_argument list
            -> term             (* result type *)
            -> term option      (* body *)
            -> t
            -> (t, error) result
end





module Error
        (Semantic: ANY)
        (E: ELABORATOR with type error = Semantic.t):
sig
    type t

    val to_range: t -> range
    val to_doc: t -> doc
end



module Make
        (Semantic: ANY)
        (E: ELABORATOR with type error = Semantic.t):
sig
    include Interfaces.FULL_PARSER
        with type state = E.t
         and type token = Position.range * Token.t
         and type expect = string * Indent.expectation option
         and type final  = Unit.t
         and type semantic = Error (Semantic) (E).t

    val make: state -> t

    val range_of_semantic: semantic -> range

    val doc_of_semantic: semantic -> doc
end
