open Std
open Printf



module Pretty =
struct
    include Fmlib_pretty.Print

    let indent = nest 4

    let parens_generic (left: doc) (inner: doc) (right: doc): doc =
        left <+> cut <+> indent inner <+> cut <+> right
        |> group

    let parens (d: doc): doc =
        parens_generic (char '(') d (char ')')

    let braced (d: doc): doc =
        parens_generic (char '{') d (char '}')
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

        | _, Pi (start, args, res) ->
            pi full start args res



    and doc full t: doc =
        edoc full t |> fst



    and pi full start args ((res, _) as r): edoc =
        let len = Array.length args in
        assert (0 <= start);
        assert (start <= len);
        if start = len then
            edoc full res
        else
            let (b, ty, _) = args.(start)
            in
            if Info.Bind.is_arrow b then
                binop
                    (edoc full ty)
                    Prec.arrow "->"
                    (pi full (start + 1) args r),
                Prec.arrow
            else
                let start, arg_docs =
                    pi_fargs full b ty (start + 1) args
                in
                let rdoc, _ =
                    pi full start args r
                in
                group (
                    text "all"
                    <+> space
                    <+> (arg_docs |> stack_or_pack " " |> indent)
                    <+> cut
                    <+> text ": "
                    <+> rdoc
                ),
                Prec.colon



    and pi_fargs full b ty start args: int * doc list =
        let len = Array.length args
        in
        let rec aux docs start =
            if start = len then
                start, List.rev docs

            else
                let b, ty, _ = args.(start) in
                if Info.Bind.is_arrow b then
                    start, List.rev docs

                else
                    aux (formal_argument full b ty :: docs) (start + 1)
        in
        aux [formal_argument full b ty] start



    and formal_argument full b ty: doc =
        assert (not (Info.Bind.is_arrow b));
        let with_type = full || Info.Bind.with_type b
        and implicit = Info.Bind.is_implicit b
        and name =
            let nm  = Info.Bind.name b in
            let str = Name.string nm
            in
            if Name.is_operator nm then
                sprintf "(%s)" str |> text
            else
                text str
        in
        let fwty ty =
            name
            <+> char ':'
            <+> space
            <+> indent (doc full ty)
        in
        match with_type, implicit with
        | true, true ->
            fwty ty |> braced

        | true, false ->
            fwty ty |> parens

        | false, true ->
            braced name

        | false, false ->
            name











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
