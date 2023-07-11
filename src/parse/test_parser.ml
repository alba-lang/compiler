open Fmlib_parse


module Pretty = Fmlib_pretty.Print

module Void = Fmlib_std.Void


module Elaborator = struct
    type doc = unit

    type t = unit
    type term = doc
    type universe_term = doc
    type formal_argument = doc
    type error = Void.t

    let range_of_semantic (_: error): Position.range =
        assert false

    let doc_of_semantic (_: error): Pretty.doc =
        assert false


    module Universe = struct
        let id    _ _: universe_term = ()
        let fixed _ _: universe_term = ()
        let max _ _: universe_term = ()
        let above _ _: universe_term = ()
        let parens_around _ _ _ = ()
    end

    let prop _: term = ()
    let level _: term = ()
    let any _ _: term = ()
    let name_term _ _: term = ()
    let string_term _ _: term = ()
    let char_term _ _: term = ()
    let decimal_term _ _: term = ()
    let float_term _ _: term = ()
    let tuple_term _ _: term = ()
    let list_term  _ _: term = ()
    let apply _ _ _: term = ()
    let parens_term _ _ _: term = ()
    let implicit_argument _ _ _: term = ()

    let unary_expression _ _ _ _ : term = ()
    let binary_expression _ _ _ _ _ : term = ()

    let formal_argument_simple _ _: formal_argument = ()

    let formal_argument _ _ _: formal_argument =
        ()

    let product_expression _ _ _: term = ()
    let lambda_expression  _ _ _ _: term = ()

    let add_definition _ _ _ _ _: (t, error) result =
        Ok ()

    let start: t =
        ()
end

module TP = Definition_parser.Make (Void) (Elaborator)

module PL =
    Parse_with_lexer.Make
        (Unit)
        (Token)
        (Elaborator)
        (Definition_parser.Error (Void) (Elaborator))
        (Lexer)
        (TP)





let lexer: Lexer.t = Lexer.start

let parse: TP.t = TP.make Elaborator.start

let parse_lex: PL.t =
    PL.make lexer parse


let write_error (str: string) (p: PL.t): unit =
    let module Reporter = Error_reporter.Make (PL) in
    if not (PL.has_succeeded p) then
        Reporter.(
            make TP.range_of_semantic TP.doc_of_semantic p
            |> run_on_string str
            |> Pretty.layout 50
            |> Pretty.write_to_channel stdout
        )







(* Success test cases
 * ==================
 *)

let success_tests: string array =
    [|
        {|
            T _ _meta (x y z: Prop) (+) ((-): Any)
             : Any 1
             :=
             Any 0
        |};


        {|
          f: T := Prop
          g: TG
        |};


        {|f: T := x + y * 'c' + "string" ^ 10 - 1.88e5 + 1e5 * 1.e5|};

        "(+): T := Any";

        "(,): T := Any";

        "(): T := Any";

        "[]: T := [1,2,3,4]";

        "f: T := g (,) (+) [] () ((,) a b)";

        "f: T := [1,2,3,4]";

        "f: T := (1,2,3,4)";

        "f: T := ((((x))))";

        {|f: all (x: A): R := \ x := e x|};

        {|Predicate: Any -> Any := \ A := A -> Prop|}
    |]







(* Failure test cases
 * ==================
 *)

let failure_tests: (string * string * int * int) array =
    [|
        "braced function term",
        "f x: T := {g} x", 0, 10;

        "not indented function body",
        "f x: T :=\nx", 1, 0;

        "double assign",
        "f x: T := x := y", 0, 12;

        "toplevel comma",
        "t:T := a, b", 0, 8;

        "associating relational operators",
        "t:T :=\n a <= b < c", 1, 10;
               (*01234567890 *)
    |]




(* Execute success tests
 * =====================
 *)
let%test _ =
    Array.for_all
        (fun str ->
            let open PL in
            let p = run_on_string str parse_lex in
            write_error str p;
            has_succeeded p)
        success_tests



(* Execute failure tests
 * =====================
 *)
let%test _ =
    Array.for_all
        (fun (desc, str, row, col) ->
            let open PL in
            let p = run_on_string str parse_lex
            in
            let line, column =
                let pos = position p in
                Position.(line pos, column pos)
            in
            if has_succeeded p then
                Printf.printf
                    "unexpected success of \"%s\"\n  %s\n\n"
                    desc
                    (String.escaped str)
            else if row <> line || col <> column then begin
                Printf.printf
                    "expected failure: line (%d/%d), col (%d/%d)\n%s\n"
                    row line col column str;
                write_error str p;
            end;
            not (has_succeeded p) && row = line && col = column
        )
        failure_tests








(* Other tests
 * ===========
 *)

let%test _ =
    let str = {|
        {- multi -}
        TAny0 _ _meta (x y z: Prop) (+) ((-): Any): Any 1 := Any 0 -- comment

        T2 {u v: Level} ((=?): Any): Any (max (u + 1) v + 1)
                -- header comment
            :=
         Any

        T3 {x: Level}: Any -- := Any

        T3 (u: Any): Any + Prop + 10

        f x: Int := (g {x}) + - y * 10 ^ z * 1.2e-6
        |}
    in
    let open PL in
    let p = run_on_string str parse_lex in
    write_error str p;
    has_succeeded p
