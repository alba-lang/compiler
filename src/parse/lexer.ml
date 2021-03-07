open Fmlib_std
open Fmlib_parse

module TP = Token_parser

module PL = Parse_with_lexer

module Token_plus = struct
    type t = Position.range * Token.t
end

module Char = struct
    include Char
    let is_lower c =
        'a' <= c && c <= 'z'
    let is_upper c =
        'A' <= c && c <= 'Z'
    let is_digit c =
        '0' <= c && c <= '9'
    let is_letter c =
        is_upper c || is_lower c
    let is_alpha_num c =
        is_upper c || is_lower c || is_digit c
    let is_alpha_num_ c =
        is_alpha_num c || c = '_'
end


module Lex_combinator =
struct
    module Basic = Character.Make (Unit) (Token_plus) (Void)
    include Basic


    (* Whitespace
     * ========== *)

    let line_comment_start: string t =
        backtrack (string "--") {|"--"|}

    let line_comment: unit t =
        let* _ = line_comment_start in
        let* _ =
            skip_zero_or_more
                (charp
                     (fun c -> c <> '\n')
                     "any char except newline")
        in
        return ()

    let multi_line_comment_start: string t =
        backtrack (string "{-") {|"{-"|}

    let multi_line_comment: unit t =
        let any_char =
            charp (fun _ -> true) "any character"
        in
        let rec rest flag =
            let* c = any_char in
            if not flag && c = '-' then
                rest  true
            else if flag && c = '}' then
                return ()
            else
                rest  false
        in
        let* _ = multi_line_comment_start in
        rest false

    let blank_or_newline: unit t =
        let* _ = char ' ' </> char '\n' in
        return ()

    let whitespace: int t =
        skip_zero_or_more
            (blank_or_newline </> line_comment </> multi_line_comment)
        |> no_expectations


    (* Identifiers and Keywords
     * ========================
     *)

    let raw_identifier: string t =
        let* str = word Char.is_letter Char.is_alpha_num_ "identifier" in
        (
            let* _ = char '?' in
            return (str ^ String.make 1 '?')
        )
        </>
        return str


    let identifier_or_keyword: Token.t t =
        let* str = raw_identifier in
        match Keyword.of_string str with
        | None ->
            return (Token.Identifier str)
        | Some kw ->
            return (Token.Keyword kw)


    let metavariable: Token.t t =
        let* u = char '_' in
        let* str = optional raw_identifier in
        let  ustr = String.make 1 u in
        match str with
        | None ->
            return (Token.Metavariable ustr)
        | Some str ->
            return (Token.Metavariable (ustr ^ str))



    (* Operators
     * =========
     *)

    let operator: Token.t t =
        let op_chars = "+-/|\\<>=~^:" in
        let is_opchar c =
            Stdlib.String.contains op_chars c
        in
        let* str =
            word is_opchar is_opchar "operator or colon"
        in
        if str = ":" then
            return Token.Colon
        else
            return (Token.Operator str)




    (* Symbols
     * =======
     *)

    let one_char (c: char) (t: Token.t): Token.t t =
        let* _ = char c in
        return t

    let second_char_opt (c: char) (t1: Token.t) (t2: Token.t): Token.t t =
        one_char c t2
        </>
        return t1

    let symbol: Token.t t =
        let str = "()[]{}.,;" in
        let* c =
            one_of_chars str ("one of \"" ^ str ^ "\"")
        in
        if c = '(' then
            second_char_opt ')' Token.ParenLeft Token.EmptyParen
        else if c = '[' then
            second_char_opt ']' Token.BracketLeft Token.EmptyBracket
        else if c = ')' then
            return Token.ParenRight
        else if c = ']' then
            return Token.BracketRight
        else if c = '{' then
            return Token.BraceLeft
        else if c = '}' then
            return Token.BraceRight
        else if c = '.' then
            return Token.Dot
        else if c = ',' then
            return Token.Comma
        else if c = ';' then
            return Token.Semicolon
        else
            assert false (* cannot happen *)




    (* Token
     * =====
     *)

    let token: (Position.range * Token.t) t =
        let one_token =
            identifier_or_keyword
            </>
            metavariable
            </>
            symbol
            </>
            operator
        in
        let* _ = whitespace in
        let* tok = located one_token in
        let* _ = whitespace in
        return tok





    (* Parser Module
     * =============
     *)

    module Parser = struct
        include Basic.Parser

        let put (c: char) (p: t): t =
            put c p

        let make (pos: Position.t): t =
            make_parser pos () token

        let start: t =
            make Position.start

        let restart (lex: t): t =
            assert (has_succeeded lex);
            fold_lookahead
                (make (position lex))
                put
                put_end
                lex

    end
end

include Lex_combinator.Parser







(* Unit Tests
 * ==========
 *)

module Test_parser = struct
    module TP = struct
        module Basic =
            Token_parser.Make
                (Unit)
                (Token)
                (struct type t = Token.t list end)
                (Fmlib_std.Void)
        include Basic

        let token: Token.t t =
            step
                "token"
                (fun _ _ tok -> Some (tok, ()))

        let parse =
            zero_or_more token

        module Parser = struct
            include Basic.Parser
            let start: t =
                make () parse
        end
    end

    module PL = Parse_with_lexer.Make
            (Unit)
            (Token)
            (struct type t = Token.t list end)
            (Fmlib_std.Void)
            (Lex_combinator.Parser)
            (TP.Parser)

    include PL

    let start: t =
        make Lex_combinator.Parser.start TP.Parser.start
end


let%test _ =
    let str = {|
        {- Initial comment -}
        class List (A: Any): Any :=
                -- List of 'A's
            []: List            {- empty list-}
            (::): A -> List -> List
        |}
    in
    let open Test_parser in
    let p = run_on_string str start in
    has_succeeded p
    &&
    List.map Token.to_string (final p)
    =
    ["class"; "List"; "("; "A"; ":"; "Any"; ")"; ":"; "Any"; ":=";
     "[]"; ":"; "List";
     "("; "::"; ")"; ":"; "A"; "->"; "List"; "->"; "List"
    ]
