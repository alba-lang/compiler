open Fmlib_pretty
open Std
open Printf


module Term = Term_old
open Term

type doc = Print.doc


let name_with_precedence (n: Name.t): Print.doc * Precedence.t =
    Print.text (Name.string n),
    Name.precedence n


let (<+>) = Print.(<+>)
let cut   = Print.cut
let text  = Print.text
let group = Print.group
let indent = Print.nest 4


let parenthesized (d: doc): doc =
    group (text "(" <+> cut <+> indent d <+> cut <+> text ")")


let sort_with_int (s: string) (i: int): doc * Precedence.t =
    if i = 0 then
        text s,
        Precedence.highest
    else
        text (sprintf "%s %d" s i),
        Precedence.application


let rec doc_with_precedence (t: t): doc * Precedence.t =
    let open Sort in
    match t with
    | Sort (Top i) ->
        sort_with_int "Top" i

    | Sort Prop ->
        text "Prop",
        Precedence.highest

    | Sort (Any i) ->
        sort_with_int "Any" i

    | Sort Level ->
        text "Univ",
        Precedence.highest


    | Local (n, _)  | Global (n, _, _) ->
        name_with_precedence n

    | Meta id ->
        sprintf "?%d" id |> text,
        Precedence.highest

    | Pi (args, res) ->
        pi_normal 0 args res

    | Lam _ ->
        assert false

    | Let _ ->
        assert false

    | App _ ->
        assert false

    | Type _ ->
        assert false

    | Case _ ->
        assert false

    | Cta _ ->
        assert false


and left_parenthesized
        (t: t)
        (prec: Precedence.t)            (* root precedence *)
    : Print.doc
    =
    let tdoc, tprec = doc_with_precedence t
    in
    if Precedence.leaning tprec prec = Precedence.Left then
        tdoc
    else
        parenthesized tdoc


and right_parenthesized
        (prec: Precedence.t)            (* root precedence *)
        (t: t)
    : Print.doc
    =
    let tdoc, tprec = doc_with_precedence t
    in
    if Precedence.leaning prec tprec = Precedence.Right then
        tdoc
    else
        parenthesized tdoc


and pi_normal
        (start: int) (args: var_binder array) ((tp, _) as tp_pair: pair)
    : Print.doc * Precedence.t
    =
    let len = Array.length args in
    assert (start <= len);
    let rec count n p =
        assert (start + n <= len);
        let i = start + n
        in
        if i < len && p (fst args.(i)) then
            count (n + 1) p
        else
            n
    in
    if start = len then
        doc_with_precedence tp
    else
        let narr =
            count start Info.Bind.is_arrow
        in
        if narr = 0 then
            let nnarr =
                count
                    start
                    (fun bnd -> not (Info.Bind.is_arrow bnd))
            in
            assert (nnarr > 0);
            pi_not_arrows start nnarr args tp_pair
        else
            pi_arrows start narr args tp_pair

and pi_arrows
        (start: int) (n: int) (args: var_binder array) ((res, _): pair)
    : Print.doc * Precedence.t
    =
    let len = Array.length args in
    assert (0 < n);
    assert (start + n <= len);
    let with_arrow i doc =
        if i = start then
            doc
        else
            Print.(text "-> " <+> doc)
    in
    let rec go i lst =
        assert (start <= i);
        if i = start then
            lst
        else
            let i = i - 1 in
            let bnd, (tp, _) = args.(i) in
            assert (Info.Bind.is_arrow bnd);
            go
                i
                (
                    (left_parenthesized
                         tp
                         Precedence.arrow
                     |> with_arrow i)
                    ::
                    lst
                )
    in
    Print.pack
        " "
        (go
             (start + n)
             [
                 right_parenthesized Precedence.arrow res
                 |> with_arrow (start + n)
             ])
    ,
    Precedence.arrow


and pi_not_arrows
        (start: int) (n: int) (args: var_binder array) (_: pair)
    : Print.doc * Precedence.t
    =
    let len = Array.length args in
    assert (0 < n);
    assert (start + n <= len);
    assert false



let doc (t: t): Print.doc =
    fst (doc_with_precedence t)


let string (t: t): string =
    Print.(doc t |> layout 70 |> string_of)








(*
Note [Print appplications and operator expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Binary and unary operators are printed within parens (e.g. (+)) except when
    appearing in an operator position (e.g. x + y, x^y).

    1. Function terms in parens
    ===========================

    An operator expression can have arguments. E.g. the binary operator (+)
    might have more than 2 arguments. For a binary operator the first two
    non-implicit arguments are used as operators and the remaining arguments are
    just applied normally.

        (a + b) a1 a2 ...

    In that case the precedence of 'a + b' is lower than application precedence.
    Therefore it needs parentheses as opposed to

        f a b a1 a2

    Here we have

        leaning (+) app = Right             -- Parens required
                                            -- Imagine application as a blank
                                            -- binary operator.

        leaning app app = Left

    Furthermore we can have

        (+) a b ...

        leaning (+) app = Right             -- Parens required


    Indication for parens around the function term:

        leaning f app <> Left

    If it is left leaning no parens are required.



    2. Arguments in parens
    ======================

    For arguments we can have the following situations:

        f a
        f (g a)
        f (a + b)
        f (+)


    Indication for parens

        leaning app arg <> Right

    If it is right leaning no parens are required.

        f (g a)                 leaning app app = Left
        f (a + b)               leaning app (+) = Left
        f (+)                   leaning app (+) = Left
        f name                  leaning app highest = Right


    3. Operands in parens
    =====================

    3.1 Binary operator
    -------------------

        a op b

    Indication for parens around left operand:

        leaning a op <> Left

    Indication for parens around right operand

        leaning op b <> Right

    3.2 Unary operator
    ------------------

        op a

    Indication for parens around operand:

        leaning op a <> Right
*)
