open Std
open Printf

module Pretty = Fmlib_pretty.Print


type doc = Pretty.doc

module Term =
struct
    open Pretty

    module Prec = Precedence

    type edoc = doc * Prec.t




    let sort_with_int (full: bool) (s: string) (i: int): edoc =
        if i = 0 && not full
        then
            text s, Prec.highest

        else
            text s <+> space <+> text (sprintf "%d" i),
            Prec.application



    let sort (full: bool): Sort.t -> edoc = function
        | Prop ->
            text "Prop", Prec.highest

        | Any i ->
            sort_with_int full "Any" i

        | Top i ->
            sort_with_int full "Top" i

        | Level ->
            text "Level", Prec.highest


    let edoc (full: bool): Term.t -> edoc = function
        | Sort s ->
            sort full s

        | Meta i ->
            sprintf "?%d" i |> text, Prec.highest


    let full_doc (t: Term.t): doc =
        fst (edoc true t)


    let doc (t: Term.t): doc =
        fst (edoc false t)


    let string (t: Term.t): string =
        doc t |> layout 70 |> string_of
end
