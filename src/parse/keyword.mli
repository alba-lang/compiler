type t =
    | All
    | Class
    | Not
    | Use


val of_string: string -> t option
val to_string: t -> string
