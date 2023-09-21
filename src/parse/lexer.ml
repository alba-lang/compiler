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

    (* Convenience Combinators
     * =======================
     *)


    let optional_with_default (a: 'a) (p: 'a t): 'a t =
        p </> return a


    let with_trailing_char (c: char) (str: string): string t =
        (
            let* _ = char c in
            return (str ^ String.make 1 c)
        )
        </>
        return str


    let around (lpar: char) (rpar: char) (p: 'a t): 'a t =
        let* _ = char lpar in
        let* a = p in
        let* _ = char rpar in
        return a
    let _ = around


    let string_of (p: char t): string t =
        let rec many str =
            map (fun c -> str ^ String.make 1 c) p >>= many
            </>
            return str
        in
        many ""




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
            charp (fun _ -> true) "completion of comment"
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
        let* _ = char ' ' </> char '\n' </> char '\r' in
        return ()

    let whitespace: int t =
        skip_zero_or_more
            (blank_or_newline </> line_comment </> multi_line_comment)
        |> no_expectations










    (* Identifiers and Keywords
     * ========================
     *)

    let raw_identifier: string t =
        word Char.is_letter Char.is_alpha_num_ "identifier"
        >>= with_trailing_char '?'


    let identifier_or_keyword: Token.t t =
        let* str = raw_identifier in
        match Token_type.keyword_of_string str with
        | None ->
            return (Token_type.Identifier, str)
        | Some kw ->
            return (kw, str)


    let metavariable: Token.t t =
        let* u = char '_' in
        let* str = optional raw_identifier in
        let  ustr = String.make 1 u in
        match str with
        | None ->
            return (Token_type.Metavariable, ustr)
        | Some str ->
            return (Token_type.Metavariable, ustr ^ str)




    (* Characters and Strings
     * ======================
     *)

    let string_char: char t =
        charp
            (fun c -> c <> '\n' && c <> '"' && c <> '\r')
            "any character except newline and '\"'"

    let char_char: char t =
        charp
            (fun c -> c <> '\n' && c <> '\r')
            "any character except newline and \"'\""


    let escaped_char (p: char t): char t =
        let* c = p
        in
        if c <> '\\' then
            return c
        else
            let* c = p in
            match c with
            | 'n' ->
                return '\n'
            | 'r' ->
                return '\r'
            | 't' ->
                return '\t'
            | 'x' ->
                let* h1 = hex_digit in
                let* h2 = hex_digit in
                return (Char.chr (h1 * 16 + h2))
            | _ ->
                return c




    let alba_string: Token.t t =
        let* _ = char '"' in
        let* str = string_of (escaped_char string_char) in
        let* _ = char '"' in
        return (Token_type.String, str)


    let alba_char: Token.t t =
        let* _ = char '\'' in
        let* c = escaped_char char_char in
        let* _ = char '\'' in
        return (Token_type.Char, String.make 1 c)




    (* Operators
     * =========
     *)

    let operator: Token.t t =
        let op_chars = "+-*/|&\\<>=~^:" in
        let is_opchar c =
            Stdlib.String.contains op_chars c
        in
        let* str =
            word is_opchar is_opchar "operator or colon"
            >>= with_trailing_char '?'
        in
        match str with
        | ":" ->
            return Token.colon
        | ":=" ->
            return Token.assign
        | {|\|} ->
            return Token.lambda
        | _ ->
            return (Token_type.Operator, str)





    (* Numbers
     * =======
     *)

    let digits: string t =
        word Char.is_digit Char.is_digit "digits"


    let exponent: string t =
        let* _ = char 'E' </> char 'e' <?> "'E' or 'e'" in
        let* sign =
            map
                (String.make 1)
                (char '+' </> char '-')
            |> optional_with_default ""
        in
        let* ds = digits in
        return ("e" ^ sign ^ ds)


    let number: Token.t t =
        let* before_dot = digits in
        (
            let* _         = char '.' in
            let* after_dot = optional_with_default "" digits in
            let* exp       = optional_with_default "" exponent in
            return (
                Token_type.Float,
                before_dot ^ "." ^ after_dot ^ exp)
        )
        </>
        (
            let* exp = exponent in
            return
                (Token_type.Float, before_dot ^ exp)
        )
        </>
        return (Token_type.Decimal, before_dot)




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
            second_char_opt ')' Token.paren_left Token.paren_empty
        else if c = '[' then
            second_char_opt ']' Token.bracket_left Token.bracket_empty
        else if c = ')' then
            return Token.paren_right
        else if c = ']' then
            return Token.bracket_right
        else if c = '{' then
            return Token.brace_left
        else if c = '}' then
            return Token.brace_right
        else if c = '.' then
            return Token.dot
        else if c = ',' then
            return Token.comma
        else if c = ';' then
            return Token.semicolon
        else
            assert false (* cannot happen *)




    (* Token
     * =====
     *)


    let token: Token_plus.t t =
        lexer
            whitespace
            (Token_type.Unknown, "")
            (
                identifier_or_keyword
                </> metavariable
                </> symbol
                </> operator
                </> number
                </> alba_string
                </> alba_char
            )





    (* Lexer Module
     * =============
     *)

    module Parser = struct
        include Basic.Parser

        let start: t =
            make_partial () token

        let restart (lex: t): t =
            restart_partial token lex
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
                (fun state _ tok -> Some (tok, state))

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


module Pretty = Fmlib_pretty.Print

let write_error (str: string) (p: Test_parser.t): unit =
    let module Reporter = Error_reporter.Make (Test_parser) in
    if Test_parser.has_failed_syntax p then
        Reporter.(
            make_syntax p
            |> run_on_string str
            |> Pretty.layout 50
            |> Pretty.write_to_channel stdout
        )

let _ = write_error


let%test _ =
    let str = {|
        {- Initial comment -}
        class List (A: Any): Any :=
                -- List of 'A's
            []: List            {- empty list-}
            (::): A -> List -> List


        10 9.5 1.6e-6 1e4 1.e4

        'a' '\x7f'
        "blabla" "xx\x0a\n\t\r\\"
        _
        _meta
        even?
        |}
    in
    let open Test_parser in
    let p = run_on_string str start in
    write_error str p;
    (*if has_succeeded p then
        Stdlib.List.iter
            (fun t -> Printf.printf "%s\n" (Token.to_escaped t))
            (final p);*)
    has_succeeded p
    &&
    List.map Token.to_string (final p)
    =
    ["class"; "List"; "("; "A"; ":"; "Any"; ")"; ":"; "Any"; ":=";
     "[]"; ":"; "List";
     "("; "::"; ")"; ":"; "A"; "->"; "List"; "->"; "List";

     "10"; "9.5"; "1.6e-6"; "1e4"; "1.e4";

     "a"; "\x7f";

     "blabla"; "xx\x0a\n\t\r\\";

     "_"; "_meta"; "even?"
    ]
