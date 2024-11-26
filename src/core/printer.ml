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


    let operand p1 p2 doc  left_or_right: doc =
        if Prec.(leaning p1 p2 = left_or_right) then
            doc
        else
            parens doc


    let binop (left, lprec) parprec op_string (right, rprec): doc =
        operand lprec parprec left Prec.Left
        <+> space
        <+> text (sprintf "%s " op_string)
        <+> operand parprec rprec right Prec.Right





    (*  Mutually Recursive Functions to Print
        ----------------------------------------------------------------------
    *)


    let rec edoc (full: bool): Term.t -> edoc = function
        | _, Sort s ->
            sort full s

        | _, Meta i ->
            sprintf "?%d" i |> text, Prec.highest

        | _, Pi (nargs, args, res) ->
            pi full nargs args res



    and pi full nargs args ((res, _) as r) =
        let len = Array.length args in
        assert (0 <= nargs);
        assert (nargs <= len);
        if nargs = 0 then
            edoc full res
        else
            let (b, ty, _) = args.(len - nargs)
            in
            if Info.Bind.is_arrow b then
                binop
                    (edoc full ty)
                    Prec.arrow "->"
                    (pi full (nargs - 1) args r),
                Prec.arrow
            else
                assert false








    (*  External Interface
        ----------------------------------------------------------------------
    *)


    let base_doc (par: bool) (full: bool) (t: Term.t): doc =
        let doc, prec = edoc full t
        in
        if par then
            operand prec Prec.highest doc Prec.Left
        else
            doc


    let doc (t: Term.t): doc =
        fst (edoc false t)



    let base_string (par: bool) (full: bool) (t: Term.t): string =
        base_doc par full t |> layout 70 |> string_of


    let string (t: Term.t): string =
        doc t |> layout 70 |> string_of
end
