open Fmlib_std
open Std



(* Type definition *)

type t =
    (* sorts *)
    | Prop
    | Any of int            (* 0 or 1 *)
    | Top

    (* variables *)
    | Local  of Name.t * int        (* De Bruijn index *)
    | Global of Name.t * int * int  (* Module, id in module *)
    | Meta   of Name.t * int * int  (* Context length, id in context *)

    (* bindings *)
    | Pi  of var_binder array * pair
    | Lam of var_binder array * t
    | Let of let_binder array * t

    (* application *)
    | App of Info.App.t * t * argument array

    (* inductive types *)
    | Type of
          var_binder array   (* parameters *)
          * var_binder
          * constructor array
          * int Name_map.t

    (* pattern match *)
    | Case of var_binder        (** function variable (for recursion) *)
              * clause array    (** clauses *)
              * tree            (** case tree *)

    | Cta  of
          t         (** refers to pattern match expression *)
          * tree    (** case tree *)
          * t array (** collected subterms of the arguments *)
          * pointer (** pointer into arguments (including arguments) *)


and pair = t * t    (* usually a term and its type or a type and its sort *)

and tp  = t  (* synonym *)

and var_binder = Info.Bind.t * pair         (* type *)

and let_binder = Info.Bind.t * pair * t     (* type + definition term *)

and argument = Info.Arg.t * t

and constructor =
    Name.t
    * constructor_arg array
    * t

and constructor_arg =
    Info.Bind.t
    * var_binder array
    * t

and clause =
    var_binder array (* pattern variables *)
    * pattern array  (* pattern *)
    * t              (* right hand side *)

and pattern =
    | Pvar of int
    (* nyi: Constants missing !!*)
    | Pmake of int * pattern array

and tree =
    | Rhs of t
    | Catch_all of tree
    | Node of tree Name_map.t * tree option

and pointer = unit (* nyi *)









(* Functions *)


let map_de_bruijn (f: int -> int) (t: t): t =
    let rec go nb t =
        match t with
        | Top | Prop | Any _ | Global _ | Meta _ ->
            t

        | Local (name, i) as t ->
            if i < nb then
                t
            else
                let j = f (i - nb) in
                assert (0 <= j);
                Local (name, j + nb)

        | Pi (args, r) ->
            Pi (
                Stdlib.Array.mapi
                    (fun i (bnd, p) -> bnd, pair_go (nb + i) p)
                    args,
                pair_go (nb + Array.length args) r
            )

        | _ ->
            assert false (* nyi *)

    and pair_go nb (t, tp) =
        go nb t,
        go nb tp
    in

    go 0 t




let up_from (n: int) (start: int) (t: t): t =
    if n = 0 then
        t
    else
        map_de_bruijn
            (fun i ->
                 if i < start then
                     i
                 else
                     i + n
            )
            t


let pair_up_from (n: int) (start: int) ((t, tp): pair): pair =
    up_from n start t,
    up_from n start tp





let up (n: int) (t: t): t =
    up_from n 0 t




let pair_up (n: int) ((t, tp): pair): pair =
    up n t,
    up n tp





let prop: t = Prop
let any0: t = Any 0
let any1: t = Any 1



let pi_sort (sa: t) (sb: t): t =
    match sa, sb with
    | _,     Prop  -> Prop
    | Any i, Any j -> Any (max i j)
    | _            -> assert false (* Illegal call *)



let arrow (a: pair) ((bt, _) as b: pair): t =
    match bt with
    | Pi (args, r) ->
        let args = Array.map (fun (bnd, p) -> bnd, pair_up 1 p) args in
        let args = Array.push_front (Info.Bind.arrow, a) args in
        Pi (args, pair_up 1 r)
    | _ ->
        Pi ([| Info.Bind.arrow, a|], pair_up 1 b)



let equal (_: t) (_: t): bool =
    assert false



module Print:
sig
    val doc: t -> Fmlib_pretty.Print.doc

    val string: t -> string
end
=
struct
    open Fmlib_pretty
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


    let rec doc_with_precedence (t: t): doc * Precedence.t =
        match t with
        | Top ->
            text "Any max",
            Precedence.application

        | Prop ->
            text "Prop",
            Precedence.highest

        | Any i ->
            if i = 0 then
                text "Any"
              , Precedence.highest
            else
                text ("Any " ^ string_of_int i)
                , Precedence.application

        | Local (n, _)  | Global (n, _, _) | Meta (n, _, _) ->
            name_with_precedence n

        | Pi (args, res) ->
            pi 0 args res

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


    and pi
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
end




(*
===================================================
Note [Print produce types]
===================================================

    A product type is a sequence of binders and a result type. Binders can be
    arrows (name of the formal argument cannot appear in the result type).

    There might be n (e.g. 2) arrow binders:

        all (_: A) (_: B) ...  is printed as    A -> B -> all ...

    There might be n (e.g. 2) non-arrow binders:

        all (x: A) (y: B): R

    where R either is not a product or starts with arrow binders.

    There are neither arrow nor non-arrow binders anymore:

        R

    then just print R


    Arrow types

        A -> B
        -> C
        -> D

    Dependent types

        all
            (x: A) (y: B)
            (z: C)
        :
            D
            -> E
            -> F

    Function definitions

        f
            (x: A) (y: B)
            (z: C)
        :
            D
            -> E
            -> F
        :=
            ...

*)



(*
===================================================
Note [Print appplications and operator expressions]
===================================================

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
