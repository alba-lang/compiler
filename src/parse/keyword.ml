open Fmlib_std

module Map = Btree.Map (String)


type t =
    | All
    | Class
    | Not
    | Use


let to_string: t -> string = function
    | All ->
        "all"
    | Class ->
        "class"
    | Not ->
        "Not"
    | Use ->
        "use"


let keywords =
    Map.(
        empty
        |> add "all" All
        |> add "class" Class
        |> add "Not" Not
        |> add "use" Use
    )


let of_string (str: string): t option =
    let len = String.length str in
    assert (0 < len);
    if str.[len - 1] = '?' then
        None
    else
        Map.find_opt str keywords
