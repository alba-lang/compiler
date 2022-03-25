open Fmlib_parse

type doc         = Fmlib_pretty.Print.doc

type range_thunk = unit -> Position.range
type doc_thunk   = unit -> doc

module Pretty = Fmlib_pretty.Print

module Package =
struct
    type tp =
        | Console of {
            main: string Located.t;
        }
        | Library of {
              export: string Located.t array;
          }

    type t = {
        tp: tp;
        name: string Located.t;
        used: string Located.t array;
    }

    let make_console name used main =
        {
            tp = Console {main};
            name;
            used;
        }

    let make_library name used export =
        {
            tp = Library {export};
            name;
            used;
        }
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
        let* used =
            field
                "use"
                (array located_string)
            |> optional_with_default [||]
        in
        let* name = field "name" located_string
        in
        if tp = "console-application" then
            let* main = field "main" located_string in
            Package.make_console name used main |> return

        else if tp = "library" then
            let* export =
                field
                    "export"
                    (array located_string)
            in
            Package.make_library name used export |> return
        else
            assert false (* cannot happen *)

    include P.Parser

    let init: t = P.make decoder
end
