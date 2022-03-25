open Fmlib_std
open Fmlib_parse

module type ANY   = Fmlib_std.Interfaces.ANY

module String_map = Btree.Map (String)


type doc         = Fmlib_pretty.Print.doc

type range_thunk = unit -> Position.range
type doc_thunk   = unit -> doc


module Error =
struct
    type t = range_thunk * doc_thunk


    let thunk (a: 'a) (): 'a =
        a

    let range ((f, _ ): t): Position.range =
        f ()

    let doc ((_, f): t): doc =
        f ()


    let duplicate_key ((range,key): string Located.t): t =
        (thunk range),
        (fun () ->
             let open Pretty in
             text "duplicated key <"
             <+> text key
             <+> text ">" <+> cut
        )


    let missing_key (f: range_thunk) (key: string): t =
        f,
        (fun () ->
             let open Pretty in
             Doc.p [
                 wrap_words "A field with the key";
                 char '"' <+> text key <+> char '"';
                 wrap_words "is missing"
             ]
             <+> cut
        )


    let length (n: int) (f: range_thunk): t =
        f,
        (fun () ->
             let open Pretty in
             Doc.p [
                 wrap_words "I was expecting a sequence with at last";
                 text (string_of_int n);
                 text "elements.";
             ]
             <+> cut
        )


    let expect (f: range_thunk) (str: string): t =
        f,
        (fun () ->
             let open Pretty in
             Doc.p [
                 wrap_words "I was expecting";
                 wrap_words str
             ]
             <+> cut
        )
end





module Yaml =
struct
    type scalar = string Located.t

    type t =
        | Scalar of scalar
        | Seq  of sequence
        | Map  of record

    and sequence = Position.t * t array

    and record   = Position.t * (scalar * t) String_map.t * (scalar * t) list



    let scalar str = Scalar str

    let sequence pos arr = Seq (pos, arr)

    module Record =
    struct
        let empty pos = pos, String_map.empty, []

        let add
                ((range, str): scalar)
                (e: t)
                ((pos, map, lst): record)
            : record option
            =
            match String_map.find_opt str map with
            | None ->
                Some (
                    pos,
                    String_map.add str ((range, str), e) map,
                    ((range, str), e) :: lst
                )
            | Some _ ->
                None

        let finish ((pos, map, lst): record): t =
            Map (pos, map, List.rev lst)
    end


    let quoted_escaped (str: string): string =
        "\"" ^ Stdlib.String.escaped str ^ "\""


    let rec to_json_string: t -> string = function
        | Scalar (_, str) ->
            quoted_escaped str

        | Seq (_, arr) ->
            let lst = Array.to_list arr
            in
            "["
            ^ String.concat ", " (List.map to_json_string lst)
            ^ "]"

        | Map (_, _, lst) ->
            let lst =
                List.map
                    (fun ((_, str), y) ->
                         quoted_escaped str ^ ": " ^ to_json_string y)
                    lst
            in
            "{" ^ String.concat ", " lst ^ "}"


    let range (y: t) (): Position.range =
        match y with
        | Scalar (r, _) ->
            r

        | Seq (pos0, _) ->
            pos0, pos0
            (*List.fold_left
                (fun r y ->
                     Position.merge r (range y))
                (pos0, pos0)
                lst*)

        | Map (pos0, _, _) ->
            pos0, pos0
            (*List.fold_left
                (fun r ((rk, _), y) ->
                     Position.(
                         merge r (merge rk (range y))
                     )
                )
                (pos0,pos0)
                lst*)
end





module Decode =
struct
    type 'a t = Yaml.t -> ('a, Error.t) result


    let decode (y: Yaml.t) (d: 'a t): ('a, Error.t) result =
        d y


    let return (a: 'a): 'a t =
        fun _ -> Ok a


    let fail (f: doc_thunk): 'a t =
        fun y -> Error (Yaml.range y, f)


    let (>>=) (m: 'a t) (f: 'a -> 'b t): 'b t =
        fun y ->
        match m y with
        Ok a ->
          f a y
        | Error e ->
            Error e


    let (let* ) = (>>=)


    let map (f: 'a -> 'b) (m: 'a t): 'b t =
        let* a = m in
        return (f a)


    let optional (m: 'a t): 'a option t =
        fun y ->
        match m y with
        | Ok a ->
            Ok (Some a)
        | Error _ ->
            Ok None


    let optional_with_default (a: 'a)  (m: 'a t): 'a t =
        fun y ->
        match m y with
        | Ok a ->
            Ok a
        | Error _ ->
            Ok a


    let yaml: Yaml.t t =
        fun y -> Ok y


    let json_string: string t =
        fun y -> Ok (Yaml.to_json_string y)


    let string_expect (expect: string): string Located.t t = function
        | Yaml.Scalar str ->
            Ok str
        | y ->
            Error (Error.expect (Yaml.range y) expect)


    let range: Position.range t =
        fun y -> Ok (Yaml.range y ())


    let located_string: string Located.t t =
        string_expect "a yaml scalar value"


    let string: string t =
        map snd located_string


    let special_string (expect: string) (f: string -> 'a option): 'a t =
        let* r, str = string_expect expect in
        match f str with
        | None ->
            fun _ -> Error (Error.expect (fun () -> r) expect)
        | Some v ->
            return v


    let int: int t =
        special_string
            "an integer value"
            int_of_string_opt


    let bool: bool t =
        special_string
            "a boolean value"
            bool_of_string_opt


    let float: float t =
        special_string
            "a floating point value"
            float_of_string_opt



    let field (key: string) (dec: 'a t): 'a t =
        fun y ->
        match y with
        | Yaml.Map (_, map, _) ->
            begin
                match String_map.find_opt key map with
                | None ->
                    Error (
                        Error.missing_key
                            (Yaml.range y)
                            key
                    )
                | Some (_, y) ->
                    dec y
            end
        | _ ->
            Error (Error.expect (Yaml.range y) "a record")


    let element (i: int) (d: 'a t): 'a t =
        assert (0 <= i);
        fun y ->
        match y with
        | Yaml.Seq (_, arr) ->
            let len = Array.length arr in
            if len <= i then
                Error (Error.length (i + 1) (Yaml.range y))
            else
                d arr.(i)
        | _ ->
            Error (Error.expect (Yaml.range y) "an array")



    let array (d: 'a t): 'a array t =
        fun y ->
        match y with
        | Yaml.Seq (_, arr0) ->
            let open Result in
            let len = Array.length arr0 in
            let rec traverse i arr =
                if i = len then
                    Ok arr
                else
                    let* hd = d arr0.(i) in
                    traverse (i + 1) (Array.push hd arr)
            in
            traverse 0 [||]
        | _ ->
            Error (Error.expect (Yaml.range y) "an array")

end








module Make (Final: ANY) =
struct
    module B = Character.Make (Unit) (Final) (Error)

    include B

    module Parser =
    struct
        include B.Parser

        (* Semantic errors
         * ===============
         *)

        let put (c: char) (p: t): t =
            put c p

        let range: Error.t -> Position.range =
            Error.range

        let doc: Error.t -> doc =
            Error.doc
    end


    let special_chars: string = {|{}[]&*#?|-<>=!%@:`,'"|} (* '<' really? *)
    let _ = special_chars



    (* Tokens
     * ======
     *)

    let comment: char t =
        let* _ = char '#' in
        let* _ =
            (charp (fun c -> c <> '\n') "comment character")
            |> skip_zero_or_more
        in
        return '#'


    let whitespace: int t =
        char ' ' </> char '\n' </> comment
        |> skip_zero_or_more
        |> no_expectations
        |> detach


    let lexeme (p: 'a t): 'a t =
        let* a = p in
        let* _ = whitespace in
        return a


    let strip (a: 'a): 'a t =
        map (fun _ -> a) whitespace



    let colon: char t =
        char ':'


    let dash: char t =
        char '-'


    let raw_string: string t =
        let special = {|{}[]#:,|}
        in
        let ok (c: char): bool =
            ' ' <= c && c <= '~'
            &&
            not (Stdlib.String.contains special c)
        in
        let expect  = "chars not containing colon and newline"
        in
        (word ok ok expect)
        |> map Stdlib.String.trim


    let quoted_string: string t =
        let expect = "chars except newline and double quote" in
        let ok c =
            ' ' <= c && c <= '~'
            && c <> '"'
        in
        let* _   = char '"' in
        let* str = (word ok ok expect) </> return "" in
        let* _   = char '"'
        in
        return str


    let string: string Located.t t =
        quoted_string </> raw_string
        <?> "scalar value"
        |> located
        |> lexeme




    (* Basic Yaml
     * ==========
     *)

    let scalar: Yaml.t t =
        map Yaml.scalar string


    let add_record_element
            (r: Yaml.record)
            ((key,y): string Located.t * Yaml.t)
        : Yaml.record t
        =
        match Yaml.Record.add key y r with
        | Some r ->
            return r
        | None ->
            fail (Error.duplicate_key key)





    (* Json like Yaml Structure
     * ========================
     *)

    let rec json_object (pos: Position.t): Yaml.t t =
        let* _ = char '{' >>= strip in
        let empty = Yaml.Record.empty pos
        in
        let* r =
            one_or_more_separated
                (add_record_element empty)
                (fun r _ e -> add_record_element r e)
                (json_object_element ())
                (char ',' >>= strip)
            </>
            return empty
        in
        let* _ = char '}' >>= strip in
        return (Yaml.Record.finish r)


    and json_object_element ():  (string Located.t * Yaml.t) t =
        let* str = string in
        let* _   = lexeme colon in
        let* y   = json_atom () in
        return (str, y)


    and json_array (pos: Position.t): Yaml.t t =
        let* _ = char '[' >>= strip in
        let* lst =
            one_or_more_separated
                (fun y -> return [y])
                (fun lst _ y -> return (y :: lst))
                (json_atom ())
                (char ',' >>= strip)
            </>
            return []
        in
        let* _ = char ']' >>= strip in
        return (Yaml.sequence pos (List.rev lst |> Array.of_list))




    and json_atom (): Yaml.t t =
        let* pos = position
        in
        json_object (pos) </> json_array (pos) </> scalar








    (* Recursive Yaml Structure
       ========================
    *)

    let rec yaml (): Yaml.t t =
        let* pos = position in
        json_array pos
        </> json_object pos
        </> sequence_block pos
        </> record_block_or_scalar pos


    and record_block_or_scalar (pos: Position.t): Yaml.t t =
        (* See Note [Left factoring] *)
        let* str, y =
            string_or_record_element ()
            <?> "scalar or record block"
            |> align
        in
        match y with
        | None ->
            Yaml.scalar str |> return
        | Some y ->
            let* r = add_record_element (Yaml.Record.empty pos) (str, y)
            in
            let* r =
                zero_or_more_fold_left
                    r
                    add_record_element
                    (
                        record_element ()
                        <?> "key value pair: \"xxx: yyyy\""
                        |> align
                    )
            in
            return (Yaml.Record.finish r)


        and string_or_record_element ()
            : (string Located.t * Yaml.t option) t
            =
            let* str = string in
            (
                let* _ =
                    lexeme colon
                    <?>
                    {|second part of a key value pair ": <value>"|}
                in
                let* y = value_in_record () in
                return (str, Some y)
            )
            </>
            return (str, None)


        and value_in_record (): Yaml.t t =
            let* pos = position
            in
            (json_array pos |> indent 1)
            </>
            (json_object pos |> indent 1)
            </>
            (sequence_block pos |> indent 0)
            </>
            (record_block_or_scalar pos |> indent 1)


        and sequence_block (pos: Position.t): Yaml.t t =
            one_or_more
                (
                    sequence_element ()
                    <?>
                    "sequence element: \"- xxxx\""
                    |> align
                )
            <?> "sequence of aligned \"- xxxx\""
            |> map
                (fun (a, lst) ->
                     Yaml.sequence pos (a :: lst |> Array.of_list)
                )


        and sequence_element (): Yaml.t t =
            let* _ = lexeme dash in
            yaml () |> indent 1


        and record_element (): (string Located.t * Yaml.t) t =
            let* str = string in
            let* _   = lexeme colon in
            let* y   = value_in_record () in
            return (str, y)




        (* Decoding Yaml Values
         * ====================
         *)

        let decode (d: 'a Decode.t): 'a t =
            let* y = yaml () in
            match d y with
            | Ok a ->
                return a
            | Error e ->
                fail e



        (* Make the parser
         * ===============
         *)

        let make (d: Final.t Decode.t): Parser.t =
            make
                ()
                (let* _ = whitespace in decode d)
end




(* Note [Special keys]
   ~~~~~~~~~~~~~~~~~~~

   A line starting with a question mark starts a key:

        key: value    # normal key value pair

        ? key : still belongs to the key
        : value

        ? |-
            A very long key
            spanning several lines
        : value
*)




(* Note [Left factoring]
   ~~~~~~~~~~~~~~~~~~~~~

   A record and a scalar have a common prefix.

        # record
        name: [a,b,c]
        key:  - item1
              - item2

        # scalar
        name

   Therefore we need a combinator 'record_block_or_scalar' which parses first
   the name. In case no colon follows, it is a scalar. If a colon follows, it is
   the start of a record block.
*)





(* Unit Tests
 * ==========
 *)

open Make (String)



let write_errors (source: string) (p: Parser.t): unit =
    let open Parser in
    assert (has_result p);
    assert (not (has_succeeded p));
    Error_reporter.make Error.range Error.doc p
    |> Error_reporter.run_on_string source
    |> Pretty.layout 80
    |> Pretty.write_to_channel stdout


let test_success (input: string) (d: string Decode.t) (expect: string): bool =
    let open Parser in
    let p = run_on_string input (make d) in
    if has_succeeded p then
        let json = final p in
        if json = expect then
            true
        else
            begin
                Printf.printf
                    "result <%s>\nexpect <%s>\n\n"
                    json expect;
                false
            end
    else
        begin
            write_errors input p;
            false
        end



let test_failed (input: string) (d: string Decode.t) (expect: string): bool =
    let open Parser in
    let p = run_on_string input (make d) in
    if has_succeeded p then
        let json = final p in
        if json = expect then
            false
        else
            true
    else
        true
let _ = test_failed


let success_cases =
    let open Decode in
    [
        json_string, "hello", {|"hello"|};

        json_string,
        {| k: [a,b] |},
        {|{"k": ["a", "b"]}|};

        json_string,
        {|
        k:
        - a
        - b
        - c
        |},
        {|{"k": ["a", "b", "c"]}|};

        json_string,
        {|
        a: b d
        c: d |},
        {|{"a": "b d", "c": "d"}|};

        json_string,
        {|{a: [1,2], b: ","}|},
        {|{"a": ["1", "2"], "b": ","}|};

        json_string,
        {|[]|}, {|[]|};     (* empty array is possible *)

        json_string,
        {|{}|}, {|{}|};     (* empty object is possible *)

        field "a" (field "b" json_string),
        {|{a: {b: value}}|},
        {|"value"|};

        field "a" (element 3 json_string),
        {|{a: [0, 1, 2, 3]}|},
        {|"3"|};

        field
            "a"
            (map
                 (fun arr -> String.concat ", " (Array.to_list arr))
                 (array json_string)),
        {|{a: [0, 1, 2, 3]}|},
        {|"0", "1", "2", "3"|};
    ]


let%test _ =
    List.for_all
        (fun (d, src, expect) -> test_success src d expect)
        success_cases
