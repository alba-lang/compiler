type t =
    | BraceLeft
    | BraceRight
    | BracketLeft
    | BracketRight
    | Colon
    | Comma
    | Decimal of string
    | Dot
    | EmptyBracket          (* [] *)
    | EmptyParen            (* () *)
    | Identifier of string
    | Keyword of Keyword.t
    | Metavariable of string
    | Operator of string
    | ParenLeft
    | ParenRight
    | Semicolon
    | Wildcard


let comma: t =
    Comma

let identifier (str: string): t =
    Identifier str


let operator (str: string): t =
    Operator str


let to_string: t -> string = function
    | BraceLeft ->
        "{"
    | BraceRight ->
        "}"
    | BracketLeft ->
        "["
    | BracketRight ->
        "]"
    | Colon ->
        ":"
    | Comma ->
        ","
    | Decimal str ->
        str
    | Dot ->
        "."
    | EmptyBracket ->
        "[]"
    | EmptyParen ->
        "()"
    | Keyword kw ->
        Keyword.to_string kw
    | Identifier str ->
        str
    | Metavariable str ->
        str
    | Operator str ->
        str
    | ParenLeft ->
        "("
    | ParenRight ->
        ")"
    | Semicolon ->
        ";"
    | Wildcard ->
        "_"
