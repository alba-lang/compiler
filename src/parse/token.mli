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


val to_string: t -> string

val comma: t

val identifier: string -> t

val operator: string -> t
