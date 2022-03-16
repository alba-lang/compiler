open Fmlib_std
open Fmlib_parse

module Pretty     = Fmlib_pretty.Print
module String_map = Btree.Map (String)



module Semantic =
struct
    type t = (unit -> Pretty.doc) Located.t

    let duplicate_key ((range,key): string Located.t): t =
        range,
        fun () ->
            Pretty.(text "duplicated key <"
                    <+> text key
                    <+> text ">" <+> cut)

    let range ((r, _ ): t): Position.range =
        r

    let doc ((_, f): t): Pretty.doc =
        f ()

end





module Yaml =
struct
    type scalar = string Located.t

    type t =
        | Scalar of scalar
        | List of sequence
        | Map  of record

    and sequence = Position.t * t list

    and record   = Position.t * (scalar * t) String_map.t * (scalar * t) list



    let scalar str = Scalar str

    let list pos lst = List (pos, lst)

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

        | List (_, lst) ->
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


    let rec range: t -> Position.range = function
        | Scalar (r, _) ->
            r

        | List (pos0, lst) ->
            List.fold_left
                (fun r y ->
                     Position.merge r (range y))
                (pos0, pos0)
                lst

        | Map (pos0, _, lst) ->
            List.fold_left
                (fun r ((rk, _), y) ->
                     Position.(
                         merge r (merge rk (range y))
                     )
                )
                (pos0,pos0)
                lst
end





module Decode =
struct
    type error = (unit -> Position.range) * (unit -> Pretty.doc)

    type 'a t = Yaml.t -> 'a option


    let return (a: 'a): 'a t =
        fun _ -> Some a


    let fail: 'a t =
        fun _ -> None


    let (>>=) (m: 'a t) (f: 'a -> 'b t): 'b t =
        fun y ->
        match m y with
        | None ->
            None
        | Some a ->
            f a y


    let (let* ) = (>>=)


    let map (f: 'a -> 'b) (m: 'a t): 'b t =
        let* a = m in
        return (f a)


    let yaml: Yaml.t t =
        fun y -> Some y


    let string: string t = function
        | Yaml.Scalar (_, str) ->
            Some str
        | _ ->
            None


    let int: int t =
        let* str = string in
        fun _ -> int_of_string_opt str


    let bool: bool t =
        let* str = string in
        fun _ -> bool_of_string_opt str


    let float: float t =
        let* str = string in
        fun _ -> float_of_string_opt str


    let field (key: string) (dec: 'a t): 'a t = function
        | Yaml.Map (_, map, _) ->
            begin
                match String_map.find_opt key map with
                | None ->
                    None
                | Some (_, y) ->
                    dec y
            end
        | _ ->
            None
end








module Combinator =
struct
    module B = Character.Make (Unit) (Yaml) (Semantic)

    include B


    let special_chars: string = {|{}[]&*#?|-<>=!%@:`,'"|} (* '<' really? *)



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
        char ':'


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
            fail (Semantic.duplicate_key key)





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
        return (Yaml.list pos (List.rev lst))




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
            |> map (fun (a, lst) -> Yaml.list pos (a :: lst))


        and sequence_element (): Yaml.t t =
            let* _ = lexeme dash in
            yaml () |> indent 1


        and record_element (): (string Located.t * Yaml.t) t =
            let* str = string in
            let* _   = lexeme colon in
            let* y   = value_in_record () in
            return (str, y)



        (* Make the parser
         * ===============
         *)

        let init: Parser.t =
            make () (let* _ = whitespace in yaml ())
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

open Combinator



let write_errors (source: string) (p: Parser.t): unit =
    let open Parser in
    assert (has_result p);
    assert (not (has_succeeded p));
    Error_reporter.make Semantic.range Semantic.doc p
    |> Error_reporter.run_on_string source
    |> Pretty.layout 80
    |> Pretty.write_to_channel stdout


let test_success (input: string) (expect: string): bool =
    let open Parser in
    let p = run_on_string input init in
    if has_succeeded p then
        let json = final p |> Yaml.to_json_string in
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



let test_failed (input: string) (expect: string): bool =
    let open Parser in
    let p = run_on_string input init in
    if has_succeeded p then
        let json = final p |> Yaml.to_json_string in
        if json = expect then
            false
        else
            true
    else
        true


let success_cases =
    [
        "hello", {|"hello"|};

        {|
        a: b d
        c: d |},
        {|{"a": "b d", "c": "d"}|};

        {|{a: [1,2], b: ","}|},
        {|{"a": ["1", "2"], "b": ","}|};

        {|[]|}, {|[]|};     (* empty array is possible *)

        {|{}|}, {|{}|};     (* empty object is possible *)

    ]


let%test _ =
    List.for_all
        (fun (src, expect) -> test_success src expect)
        success_cases
