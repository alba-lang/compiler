open Fmlib_parse

type doc         = Fmlib_pretty.Print.doc

type range_thunk = unit -> Position.range
type doc_thunk   = unit -> doc

module Pretty = Fmlib_pretty.Print

module Package =
struct
    type t = unit
end

module Parser =
struct
    open Lib.Yaml_parser
    module P = Make (Package)

    let type_decoder: string Decode.t =
        let open Decode in
        let* str = string in
        if str = "console-application" then
            return str
        else if str = "library" then
            return str
        else
            fail (fun () ->
                let open Pretty in
                wrap_words {|must be one of ["console-application", "library"]|}
                <+> cut <+> cut
            )

    let decoder: Package.t Decode.t =
        let open Decode in
        let* tp   = field "type" type_decoder in
        let* _ =
            field
                "use"
                (array located_string)
        in
        if tp = "console-application" then
            let* _ = field "main" located_string in
            return ()
        else if tp = "library" then
            let* _ =
                field
                    "export"
                    (array located_string)
            in
            return ()
        else
            assert false (* cannot happen *)

    include P.Parser

    let init: t = P.of_decoder decoder
end
