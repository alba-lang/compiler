open Std
open Printf



module Pretty =
struct
    include Fmlib_pretty.Print

    let indent = nest 4

    let parens (d: doc): doc =
        group (char '(' <+> cut <+> indent d <+> cut <+> char ')')
end


type doc = Pretty.doc



module Term =
struct
    open Pretty

    module Prec = Precedence

    type edoc = doc * Prec.t




    let sort_with_int (full: bool) (s: string) (i: int): edoc =
        if
            0 < i && full
        then
            text s <+> space <+> text (sprintf "%d" i),
            Prec.application
        else
            text s, Prec.highest



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
        | _, Sort s ->
            sort full s

        | _, Meta i ->
            sprintf "?%d" i |> text, Prec.highest


    let left_parens
            (full: bool)
            (prec: Prec.t) (* Precedence of root *)
            (t: Term.t)    (* Term which is a left operand of the root *)
        : doc
        =
        let tdoc, tprec = edoc full t in
        if
            Prec.(leaning tprec prec = Left)
        then
            tdoc
        else
            parens tdoc


    let base_doc (par: bool) (full: bool) (t: Term.t): doc =
        if par then
            left_parens full Prec.highest t
        else
            edoc full t |> fst


    let doc (t: Term.t): doc =
        fst (edoc false t)


    let string (t: Term.t): string =
        doc t |> layout 70 |> string_of
end
