open Fmlib_std

module Map = Btree.Map (String)


type t =
    | Assign
    | BraceLeft
    | BraceRight
    | BracketLeft
    | BracketRight
    | BracketEmpty
    | Char
    | Colon
    | Comma
    | Decimal
    | Dot
    | Float
    | Identifier
    | Kw_all
    | Kw_any
    | Kw_class
    | Kw_level
    | Kw_prop
    | Kw_type
    | Kw_use
    | Lambda
    | Metavariable
    | Operator
    | ParenLeft
    | ParenRight
    | ParenEmpty
    | Pi
    | Semicolon
    | String
    | Unknown
    | Wildcard



let keywords =
    Map.(
        empty
        |> add "all"   Kw_all
        |> add "Any"   Kw_any
        |> add "class" Kw_class
        |> add "Level" Kw_level
        |> add "Prop"  Kw_prop
        |> add "type"  Kw_type
        |> add "use"   Kw_use
    )

let keyword_of_string (str: string): t option =
    Map.find_opt str keywords
