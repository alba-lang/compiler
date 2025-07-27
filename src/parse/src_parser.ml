open Fmlib_std.Interfaces
open Fmlib_parse

open Std
open Parser_intf






(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        General Parser
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)
module Make (E: ELABORATOR) (Final: ANY):
sig
    module Parser:
    sig
        include Interfaces.FULL_PARSER
            with type state    := E.t
             and type token    := Position.range * Token.t
             and type final    := Final.t
             and type expect   := string * Indent.expectation option
             and type semantic := Error.t
    end


    include MONAD

    val elab_term: E.term t

    val make:         E.t -> Final.t t -> Parser.t
    val make_partial: E.t -> Final.t t -> Parser.t
end
=
struct
    module TP = Token_parser.Make (E) (Token) (Final) (Error)

    include TP


    (*
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            Term: Recursive Structure
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    *)

    let term (): E.src_term t =
        assert false






    (*
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            Elaborate a Term
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    *)

    let elab_term: E.term t =
        let* t    = term () in
        let* elab = get in
        match E.make_term t elab with
        | Ok t ->
            return t
        | Error e ->
            fail e
end










(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Term Parser
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)
module Term (E: ELABORATOR) =
struct
    include Make
            (E)
            (struct type t = E.term end)

    let make (elab: E.t): Parser.t =
        make elab elab_term


    let make_partial (elab: E.t): Parser.t =
        make_partial elab elab_term
end
