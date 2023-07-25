open Alba_parse

module Position = Fmlib_parse.Position

module Pretty   = Fmlib_pretty.Print


module TP = Definition_parser.Make (Error) (Elaborator)

module PL =
    Fmlib_parse.Parse_with_lexer.Make
        (Elaborator)
        (Token)
        (Unit)
        (Definition_parser.Error (Error) (Elaborator))
        (Lexer)
        (TP)




let elab: Elaborator.t =
    Elaborator.make ()

let lexer: Lexer.t =
    Lexer.start

let token_parser: TP.t =
    TP.make elab

let parser_lex: PL.t =
    PL.make lexer token_parser





let write_error (str: string) (p: PL.t): unit =
    let module Reporter = Fmlib_parse.Error_reporter.Make (PL) in
    if not (PL.has_succeeded p) then
        Reporter.(
            make TP.range_of_semantic TP.doc_of_semantic p
            |> run_on_string str
            |> Pretty.layout 50
            |> Pretty.write_to_channel stdout
        )



let check_success (src: string): bool =
    let open PL in
    let p = run_on_string src parser_lex in
    write_error src p;
    has_succeeded p






let check_failure
        (desc: string) (tag: string) (row: int) (col: int) (src: string)
    : bool
    =
    let open PL in
    let open Printf
    in
    let p = run_on_string src parser_lex
    in
    if has_succeeded p then
        begin
            printf
                "unexpected success of\n  %s\  %s\n\n%s\n\n"
                desc tag src;
            false
        end

    else if
        has_failed_syntax p ||
        not (TP.is_elab_error (failed_semantic p))
    then
        begin
            printf
                "unexpected syntax error of <%s>\n\n" desc;
            write_error src p;
            false
        end

    else (* semantic failure *)
        let e = TP.elab_error (failed_semantic p)
        in
        if
            Error.tag e <> tag || Error.line1 e <> row || Error.column1 e <> col
        then
            begin
                printf "unexpected semantic error of <%s>\n" desc;
                printf "  expected (%d,%d) <%s>\n" row col tag;
                printf "  found    (%d,%d) <%s>\n"
                    (Error.line1 e) (Error.column1 e) (Error.tag e);
                write_error src p;
                false
            end
        else
            true






let check_successes (tests: string array): bool =
    Array.for_all check_success tests






let check_failures (tests: (string * string * int * int * string) array): bool =
    Array.for_all
        (fun (desc, tag, row, col, src) -> check_failure desc tag row col src)
        tests




(* Success test cases
 * ==================
 *)


let success_tests: string array =
    [|
        {|
            Nat:    Any

            String: Any

            (+): Nat -> Nat -> Nat

            (+): String -> String -> String

            (|>) {A: Any} {F: A -> Any} (a: A) (f: all x: F x): F a
            :=
                f a

            (<<) {A B C: Any} (f: B -> C) (g: A -> B): A -> C
            :=
                \ a := f (g a)
        |}
    |]










(* Failure test cases
 * ==================
 *)


let failure_tests: (string * string * int * int * string) array =
    [|
        "(|>) with a wrong function type",
        "cannot infer type",
        1, 55,
(*        10        20        30        40        50        60
012345678 012345678 012345678 012345678 012345678 012345678 012345678 *)
        {|
            (|>) {A: Any} {F: A -> Any} (a: A) (f: all x: F a): F a
        |}
    |]








(* Run test cases
 * ==============
 *)


let%test _ =
    check_failures failure_tests


let%test _ =
    check_successes success_tests
