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


val keyword_of_string: string -> t option
