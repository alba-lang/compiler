open Fmlib_parse

module Pretty = Fmlib_pretty.Print
module Error  = Std.Error
module Parser = Il_parser.Parser








let run_on_string (trace_flag: bool) (src: string): Parser.t =
    let open Il_parser in
    let open Parser in
    let gamma =
        Elab.(make_gamma (make_globals ()))
    in
    let state =
        if trace_flag then
            Elab.State.make_tracing gamma
        else
            Elab.State.make gamma
    in
    let p = term_parser state in
    run_on_string src p



let doc_with_tracer trace_flag p doc =
    if trace_flag then
        let open Pretty
        in
        doc
        <+>
        Tracer.doc_sorted (Elab.State.tracer (Parser.state p))
    else
        doc


let doc_of_result flag t: Pretty.doc =
    let open Pretty
    in
    if flag then
        Elab.doc_of_term t
        <+> text ": "
        <+> Elab.(doc_of_term (type_of_term t))
        <+> cut
    else
        empty



let doc_of_error flag src p: Pretty.doc =
    assert (not (Parser.has_succeeded p));
    let open Pretty in
    if flag then
        let module Reporter = Error_reporter.Make (Parser) in
        Reporter.(
            make Error.range Error.doc p
            |> run_on_string src
        )
    else
        empty



let print_doc doc: unit =
        doc
        |> Pretty.layout_with_ribbon 80 50
        |> Pretty.write_to_channel stdout



let print_success
        (print_res_flag: bool)
        (trace_flag: bool)
        (p: Parser.t)
    : unit
    =
    assert (Parser.has_succeeded p);
    match Parser.final p with
    | Elab.Final.Term t ->
        doc_of_result print_res_flag t
        |> doc_with_tracer trace_flag p
        |> print_doc

    | _ ->
        assert false (* cannot happen *)




let print_failure
        (error_flag: bool)
        (trace_flag: bool)
        (src: string)
        (p: Parser.t)
    : unit
    =
    assert (not (Parser.has_succeeded p));
    doc_of_error error_flag src p
    |> doc_with_tracer trace_flag p
    |> print_doc











(*
    Success Testcases
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)




type success_term_test =
    (* print result, print trace, source *)
    bool * bool * string




let success_term_tests
    : success_term_test list
    =
    [
        false, false,
        "Prop"
        ;
        false, false,
        " ( Any 0 ) "
        ;
        false, false,
        "(pi (#(%A (Any 0))) %A)"
        ;
        false, false,
        "(an Prop (Any 0))"
        ;
        false, false,
        "(ar (Any 1 ) Prop (Any 0))"
        ;
        false, false,
        "(pi (#(%x (Any 1)) (%y (Any 0))) Prop)"
        ;
    ]



let execute_success_term_test
        ((print_res_flag, trace_flag, src): success_term_test)
    : bool
    =
    let p = run_on_string trace_flag src in
    let ok = Parser.has_succeeded p in
    if not ok then
            print_failure true trace_flag src p
    else
            print_success print_res_flag trace_flag p;
    ok




let%test _ =
    List.for_all
        execute_success_term_test
        success_term_tests










(*
    Failure Testcases
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)

type failure_term_test =
    (* print error, print trace, source *)
    bool * bool * string


let failure_term_tests: failure_term_test list =
    [
        false, false,
        "(pi (%x %y) Prop)"
        ;
        false, false,
        "(an (Any 0) Prop)"
        ;
        false, false,
        "%xxx"
    ]


let execute_failure_term_test
        ((error_flag, trace_flag, src): failure_term_test)
    : bool
    =
    let p = run_on_string trace_flag src in
    let ok = Parser.has_succeeded p in
    if ok then
            print_success true trace_flag p
    else
            print_failure error_flag trace_flag src p;
    not ok

let%test _ =
    List.for_all
        execute_failure_term_test
        failure_term_tests
