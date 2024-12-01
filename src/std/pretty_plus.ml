open Fmlib_pretty.Print

let indent (d: doc): doc =
    nest 4 d



let description (d1: doc) (d2: doc): doc =
    d1 <+> space <+> indent d2 |> group


let descriptions (ds: (doc * doc) list): doc =
    List.map
        (fun (d1, d2) ->
             d1
             <+> space
             <+> (indent d2)
             |> group
        )
        ds
    |> separated_by space
    |> group
