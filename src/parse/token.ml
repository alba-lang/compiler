type t = Token_type.t * string

let to_string (_, str) =
    str

let to_escaped (_, str) =
    String.escaped str

let paren_left: t =
    Token_type.ParenLeft, "("

let paren_right: t =
    Token_type.ParenRight, ")"

let paren_empty: t =
    Token_type.ParenEmpty, "()"

let bracket_left: t =
    Token_type.BracketLeft, "["

let bracket_right: t =
    Token_type.BracketRight, "]"

let bracket_empty: t =
    Token_type.BracketEmpty, "[]"

let brace_left: t =
    Token_type.BraceLeft, "{"

let brace_right: t =
    Token_type.BraceRight, "}"

let assign: t =
    Token_type.Assign, ":="

let colon: t =
    Token_type.Colon, ":"

let comma: t =
    Token_type.Comma, ","

let dot: t =
    Token_type.Dot, "."

let lambda: t =
    Token_type.Lambda, {|\|}

let semicolon: t =
    Token_type.Semicolon, ";"
