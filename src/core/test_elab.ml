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
        false, false,
        "Prop"
        ;
        false, false,
        " ( Any 0 ) "
        ;
        false, false,
        "(ar (Any 1 ) Prop (Any 0))"
        ;
        false, false,
        "(pi ((%x (Any 1)) (%y (Any 0))) Prop)"
        ;
    ]


let execute_success_term_test
        ((print_res_flag, trace_flag, src): success_term_test)
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
        Error_reporter.Make (Parser)
    in
    let doc_with_tracer doc =
        if trace_flag then
            let open Pretty
            in
            doc
            <+>
            Tracer.doc (Elab.State.tracer (Parser.state p))
        else
            doc
    in
    if not (has_succeeded p) then
        begin
            (
                doc_with_tracer
                Reporter.(
                    make Error.range Error.doc p
                    |> run_on_string src
                )
            )
            |> Pretty.layout 80
            |> Pretty.write_to_channel stdout;
            false
        end
    else if print_res_flag then
        begin
            (match final p with
             | Elab.Final.Term t ->

                 let open Pretty
                 in
                 let doc =
                     Elab.doc_of_term t ()
                     <+> text ": "
                     <+> Elab.(doc_of_term (type_of_term t) ())
                 in
                 let doc =
                     doc_with_tracer (doc <+> cut)
                 in
                 doc
                 |> Pretty.layout 80
                 |> Pretty.write_to_channel stdout

             | _ ->
                 assert false (* cannot happen *)
            );
            true
        end
    else
        true



let%test _ =
    List.for_all
        execute_success_term_test
        success_term_tests
