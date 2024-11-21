open Fmlib_parse

module Pretty = Fmlib_pretty.Print
module Error  = Std.Error


type success_term_test =
    (* print result, print trace, source *)
    bool * bool * string

let success_term_tests
    : success_term_test list
    =
    [
        true, true, "Prop";
        true, true, "(Any 0)";
        (*
        true, true, "(arr [Prop] Prop)";
        true, true, "(pi [%x: (Any 0)]: Prop)";
        *)
    ]


let execute_success_term_test
        ((_, trace_flag,src): success_term_test)
    : bool
    =
    let open Il_parser in
    let open Parser in
    let gamma =
        Elab.(make_gamma (make_globals ()))
    in
    let state =
        if trace_flag then
            Elab.State.make_trace gamma
        else
            Elab.State.make gamma
    in
    let p = term_parser state in
    let p = run_on_string src p
    in
    let module Reporter =
        Error_reporter.Make (Parser) in
    if not (has_succeeded p) then
        begin
            Reporter.(
                make Error.range Error.doc p
                |> run_on_string src
                |> Pretty.layout 50
                |> Pretty.write_to_channel stdout
            );
            false
        end
    else
        begin
            (match final p with
             | Elab.Final.Empty ->
                 assert false
             | Elab.Final.Term t ->
                 Pretty.(Print_term.doc (Elab.term_of_term t) <+> cut)
                 |> Pretty.layout 50
                 |> Pretty.write_to_channel stdout
            );
            true
        end



let%test _ =
    List.for_all
        execute_success_term_test
        success_term_tests
