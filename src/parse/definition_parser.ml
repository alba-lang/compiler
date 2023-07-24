open Fmlib_parse
open Std

module type ANY = Fmlib_std.Interfaces.ANY

module Pretty = Fmlib_pretty.Print

type range = Position.range

type 'a located = range * 'a

type doc = Fmlib_pretty.Print.doc

module type ELABORATOR =
sig
    type t
    type error
    type term
    type universe_term
    type formal_argument

    val range_of_semantic: error -> range
    val doc_of_semantic:   error -> doc



    module Universe:
    sig
        val id: range -> string -> universe_term
        val fixed: range -> int -> universe_term
        val max: Position.t -> universe_term list -> universe_term
        val above: universe_term  -> Position.t -> universe_term
        val parens_around:
            Position.t -> Position.t -> universe_term
            -> universe_term
    end

    val prop: range -> term
    val level: range -> term
    val any: range -> universe_term option -> term
    val name_term: range -> Name.t -> term
    val string_term: range -> string -> term
    val char_term:   range -> string -> term
    val decimal_term: range -> string -> term
    val float_term: range -> string -> term
    val tuple_term: range -> term list -> term
    val list_term:  range -> term list -> term
    val apply: term -> bool -> term -> term
    val parens_term: Position.t -> Position.t -> term -> term
    val implicit_argument: Position.t -> Position.t -> term -> term

    val unary_expression:
        range
        -> Name.t
        -> term
        -> term

    val binary_expression:
        term
        -> range
        -> Name.t
        -> term
        -> term

    val formal_argument_simple: range -> Name.t -> formal_argument

    val formal_argument:
        bool
        -> Name.t located list
        -> term option
        -> formal_argument


    val product_expression:
        Position.t
        -> formal_argument list
        -> term (* result type *)
        -> term


    val lambda_expression:
        Position.t
        -> formal_argument list
        -> term option          (* result type *)
        -> term                 (* body *)
        -> term


    val add_definition:
            Name.t located
            -> formal_argument list
            -> term option      (* result type *)
            -> term option      (* body *)
            -> t
            -> (t, error) result
end







module Error
        (Semantic: ANY)
        (E: ELABORATOR with type error = Semantic.t)
=
struct
    type t =
        | Parse of range * string * string
        | Elab  of Semantic.t


    let is_elab: t -> bool = function
        | Parse _ -> false
        | Elab  _ -> true


    let elab: t -> Semantic.t = function
        | Parse _ -> assert false (* Illegal call *)
        | Elab e  -> e


    let to_range: t -> range = function
        | Parse (range, _, _) ->
            range
        | Elab e ->
            E.range_of_semantic e

    let to_doc: t -> Pretty.doc = function
        | Parse (_, op1, op2) ->
            let open Pretty in
            let quoted str =
                char '"' <+> text str <+> char '"'
            in
            [
                separated_by (group space) [
                    wrap_words "The two operators";
                    quoted op1;
                    text "and";
                    quoted op2;
                    wrap_words
                        "are relational operators. You have used them in an \
                         expression of the form"

                ] <+> cut;

                nest 4 (text "a " <+> text op1 <+> text " b " <+> text op2
                        <+> text " c" <+> cut);

                wrap_words
                    "where \"a\", \"b\" and \"c\" are abbreviations for \
                    the operands. This does not make sense for relational \
                    operators. Maybe you wanted to express that both \
                    relations are valid i.e. something like"
                <+> cut;

                nest 4 (text "a " <+> text op1 <+> text " b  /\\  b "
                        <+> text op2 <+> text " c" <+> cut);
            ]
            |> paragraphs
            <+> cut

        | Elab e ->
            E.doc_of_semantic e
end









(* Combinator part of the Token_parser *)
module Combinator
        (Semantic: ANY)
        (E: ELABORATOR with type error = Semantic.t)
=
struct
    module Error = Error (Semantic) (E)

    module Basic   =
        Token_parser.Make (E) (Token) (Unit) (Error)

    include Basic







    (* Token
     * =====
     *)


    let step
            (expect: string)
            (etp: Token_type.t)
            (f: range -> string -> 'a)
        : 'a t
        =
        Basic.step
            expect
            (fun state range (tp, str) ->
                 if tp = etp then
                     Some (f range str, state)
                 else
                     None)



    let special
            (expect: string)
            (estr: string)
            (etp: Token_type.t)
            (f: range -> 'a)
        : 'a t
        =
        Basic.step
            expect
            (fun state range (tp, str) ->
                 if tp = etp && str = estr then
                     Some (f range, state)
                 else
                     None)

    let select_start (p1, _) _ = p1
    let select_end   (_, p2) _ = p2
    let select_range r       _ = r
    let unit1 _   = ()
    let unit2 _ _ = ()

    let assign (f: range -> string -> 'a): 'a t =
        step {|":="|} Token_type.Assign f

    let brace_left (f: range -> string -> 'a): 'a t =
        step "'('" Token_type.BraceLeft f

    let brace_right (f: range -> string -> 'a): 'a t =
        step "')'" Token_type.BraceRight f

    let bracket_empty (f: range -> string -> 'a): 'a t =
        step {|"[]"|} Token_type.BracketEmpty f

    let bracket_left (f: range -> string -> 'a): 'a t =
        step "'('" Token_type.BracketLeft f

    let bracket_right (f: range -> string -> 'a): 'a t =
        step "')'" Token_type.BracketRight f

    let colon (f: range -> string -> 'a): 'a t =
        step "':'" Token_type.Colon f

    let comma (f: range -> string -> 'a): 'a t =
        step "','" Token_type.Comma f

    let decimal (f: range -> string -> 'a): 'a t =
        step "decimal" Token_type.Decimal f

    let identifier (f: range -> string -> 'a): 'a t =
        step "identifier" Token_type.Identifier f

    let lambda (f: range -> string -> 'a): 'a t =
        step {|'\'|} Token_type.Lambda f

    let meta (f: range -> string -> 'a): 'a t =
        step {|metavariable "_name"|} Token_type.Metavariable f

    let paren_empty (f: range -> string -> 'a): 'a t =
        step {|"()"|} Token_type.ParenEmpty f

    let paren_left (f: range -> string -> 'a): 'a t =
        step "'('" Token_type.ParenLeft f

    let paren_right (f: range -> string -> 'a): 'a t =
        step "')'" Token_type.ParenRight f

    let operator (f: range -> string -> 'a): 'a t =
        step "operator" Token_type.Operator f

    let operator_with_precedence: (range * string * Precedence.t) t =
        operator (fun range str -> range, str, Precedence.of_string str)

    let wildcard (f: range -> string -> 'a): 'a t =
        step "'_'" Token_type.Wildcard f


    let kw_all (f: range -> string -> 'a): 'a t =
        step {|"all"|} Token_type.Kw_all f

    let kw_class (f: range -> string -> 'a): 'a t =
        step {|"class"|} Token_type.Kw_class f

    let kw_type (f: range -> string -> 'a): 'a t =
        step {|"type"|} Token_type.Kw_type f




    (* Utilities
     * =========
     *)

    let const (a: 'a): _ -> 'a =
        fun _ -> a


    (*let add_range (a: 'a located): 'a located tl =
        return (fst a, a)*)

    let around
            (lpar: Position.t t)
            (rpar: Position.t t)
            (f: Position.t -> Position.t -> 'a -> 'b)
            (p: unit -> 'a t)
        : 'b t
        =
        let* p1 = lpar in
        let* a  = p () in
        let* p2 = rpar in
        return (f p1 p2 a)

    let parens_around
            (f: Position.t -> Position.t -> 'a -> 'b)
            (p: unit -> 'a t)
        : 'b t
        =
        around
            (paren_left  select_start)
            (paren_right select_end)
            f
            p

    (*let brackets_around
            (f: Position.t -> Position.t -> 'a -> 'b)
            (p: unit -> 'a t)
        : 'b t
        =
        around
            (bracket_left  select_start)
            (bracket_right select_end)
            f
            p*)

    let braces_around
            (f: Position.t -> Position.t -> 'a -> 'b)
            (p: unit -> 'a t)
        : 'b t
        =
        around
            (brace_left  select_start)
            (brace_right select_end)
            f
            p


    let rec many0 (p: 'a t): 'a list t =
        (
            let* a = p in
            let* lst = many0 p in
            return (a :: lst)
        )
        </>
        return []


    let many1 (p: 'a t): 'a list t =
        let* a = p in
        let* lst = many0 p in
        return (a :: lst)


    let many2 (p: 'a t): 'a list t =
        let* a = p in
        let* lst = many1 p in
        return (a :: lst)


    let sep_by (sep: _ t) (p: 'a t): 'a list t =
        let* a = p in
        let* lst = many0 (let* _ = sep in p) in
        return (a :: lst)


    let elaborate (f: E.t -> (E.t, E.error) result): unit t =
        let* elab = get in
        match f elab with
        | Ok elab ->
            set elab
        | Error e ->
            fail (Error.Elab e)





    (* Universes
     * =========
     *)


    module Universe = struct
        (* A universe term occurs immediately after 'Any'. 'Any' expects
           exactly one argument, therefore the term must be either atomic
           (variable or a fixed level) or it has to be parenthesized.
        *)
        let atomic: E.universe_term t =
            identifier E.Universe.id
            </>
            decimal
                (fun range str ->
                     E.Universe.fixed range (int_of_string str))

        let rec term (): E.universe_term t =
            atomic
            </>
            parens_around E.Universe.parens_around inner
            <?>
            "universe expression; examples: u, u+1, min u v, where
            u and v are universe variables i.e. declared as 'u v: Level'"

        and inner (): E.universe_term t =
            let one =
                special "'1'" "1" Token_type.Decimal snd
            and plus =
                special "'+'" "+" Token_type.Operator unit1
            in
            let* t = (max () </> term ()) in
            (
                let* _ = plus in
                let* p2 = one in
                return (E.Universe.above t p2)
            )
            </>
            return t

        and max () =
            let string_max =
                special {|"max"|} "max" Token_type.Identifier fst
            in
            let* p1 = string_max in
            let* lst = many2 (term ()) in
            return (E.Universe.max p1 lst)
    end







    (* Simple Terms
     * ============
     *)

    let of_keyword
            (expect: string)
            (tp: Token_type.t)
            (f: range -> 'a)
        : 'a t
        =
        step expect tp (fun range _ -> f range)


    let prop: E.term t =
        of_keyword "Prop" Token_type.Kw_prop E.prop


    let level: E.term t =
        of_keyword "Level" Token_type.Kw_level E.prop


    let string_term: E.term t =
        step "string" Token_type.String E.string_term


    let char_term: E.term t =
        step "character" Token_type.Char E.char_term


    let decimal_term: E.term t =
        step "decimal" Token_type.Decimal E.decimal_term


    let float_term: E.term t =
        step "float" Token_type.Float E.float_term


    let get_name (f: range -> Name.t -> 'a): 'a t =
        let to_name g  range str = f range (g str)
        in
        identifier (to_name Name.normal)
        </>
        meta (to_name Name.meta)
        </>
        wildcard (to_name Name.meta)
        </>
        paren_empty (to_name Name.paren_empty)
        </>
        bracket_empty (to_name Name.bracket_empty)
        </>
        (
            let expect = {|"(operator)"|}
            in
            backtrack
                (
                    operator (to_name Name.operator)
                    </>
                    comma (to_name Name.operator)
                    |> const
                    |> parens_around (fun _ _ x -> x)
                    <?> expect
                )
                expect
        )



    let name_term: E.term t =
        get_name E.name_term


    let any: E.term t =
        let* r_any = step "Any" Token_type.Kw_any select_range in
        let* uni   = optional (Universe.term ()) in
        return (E.any r_any uni)


    let atomic: E.term t =
        prop
        </> level
        </> string_term </> char_term
        </> decimal_term </> float_term




    (* Terms: Recursive Structure
     * ==========================
     *)


    let rec term (): E.term  t =
        operator_expression ()
        </> lambda_abstraction ()
        </> pi_term ()
        </> type_term ()
        <?> "expression"

    and comma_term
            (lpar: Position.t t)
            (rpar: Position.t t)
            (f: range -> E.term list -> E.term)
        : E.term t
        (* lists [a, b, c, d] or tuples (a, b, c, d) *)
        =
        let* p1  = lpar in
        let* lst = sep_by (comma unit2) (term ()) in
        let* p2  = rpar in
        return (f (p1, p2) lst)


    and paren_term (): E.term t =
        comma_term
            (paren_left select_start)
            (paren_right select_end)
            E.tuple_term
        </>
        comma_term
            (bracket_left select_start)
            (bracket_right select_end)
            E.list_term


    and primary (): E.term t =
        atomic
        </>
        any
        </>
        application ()
        </>
        paren_term ()
        <?>
        "expression"


    and operator_expression (): E.term t =
        Basic.operator_expression
            (primary ())
            (Some operator_with_precedence)
            operator_with_precedence
            (fun (_, op1, p1) (range, op2, p2) ->
                 let open Precedence in
                 match leaning p1 p2 with
                 | Left ->
                     return true
                 | Right ->
                     return false
                 | _ ->
                     fail (Error.Parse (range, op1, op2))
            )
            (fun (range, str, prec) t ->
                 let name = Name.operator_with_precedence prec str in
                 return (E.unary_expression range name t)
            )
            (fun t1 (range, str, prec) t2 ->
                 let name = Name.operator_with_precedence prec str in
                 return (E.binary_expression t1 range name t2))


    and application (): E.term t =
        let* f = function_term () in
        arguments f


    and function_term (): E.term t =
        name_term
        </>
        paren_term ()


    and arguments (f: E.term): E.term t =
        (
            let* (implicit, arg) = argument () in
            arguments (E.apply f implicit arg)
        )
        </>
        return f


    and argument (): (bool * E.term) t =
        let explicit = map (fun t -> false, t)
        and implicit = map (fun t -> true, t)
        in
        explicit atomic
        </>
        explicit name_term
        </>
        explicit (parens_around E.parens_term term)
        </>
        implicit (braces_around E.implicit_argument term)
        <?>
        "function argument"


    and lambda_abstraction (): E.term t =
        let* pos = lambda select_start in
        let* fargs = formal_arguments () in
        let* res = result_type () |> optional in
        let* body = function_body () in
        return (E.lambda_expression pos fargs res body)


    and pi_term (): E.term t =
        let* pos = kw_all select_start in
        let* fargs, res = signature () in
        return (E.product_expression pos fargs res)


    and type_term (): E.term t =
        let* _ = kw_type select_start in
        let* _ = get_name (fun range str -> range, str) |> optional in

        assert false



    and result_type (): E.term t =
        let* _ = colon unit2 <?> {|": <result type>"|} in
        term () <?> "type"


    and function_body (): E.term t =
        let* _ = assign unit2 <?> {|":= <function body>"|} in
        term () <?> "function body" |> indent 1


    and formal_argument_inner ()
            (f: Name.t located list -> E.term option -> 'a)
            ()
        : 'a t
        =
        let* names = many1 (get_name (fun range nme -> range, nme)) in
        let* tp    = result_type () |> optional in
        return (f names tp)


    and formal_argument (): E.formal_argument t =
        get_name E.formal_argument_simple
        </>
        (
            formal_argument_inner () (E.formal_argument false)
            |> parens_around (fun _ _ farg -> farg)
        )
        </>
        (
            formal_argument_inner () (E.formal_argument true)
            |> braces_around (fun _ _ farg -> farg)
        )


    and formal_arguments (): E.formal_argument list t =
        formal_argument () <?> "formal argument" |> many0


    and signature () : (E.formal_argument list * E.term) t =
        let* fargs = formal_arguments ()
        in
        let* rtp = result_type ()
        in
        return (fargs, rtp)




    (* Top Level Definitions
     * =====================
     *)

    let top_definition (): unit t =
        let* name = get_name (fun range name -> range, name) in
        let* fargs, rtp =
            indent 1 (
                let* fargs = formal_arguments () in
                let* rtp   = optional (result_type ()) in
                return (fargs,rtp)
            )
        in
        match rtp with
        | None ->
            let* bdy = function_body () in
            elaborate (E.add_definition name fargs None (Some bdy))
        | Some _ as rtp ->
            let* bdy = optional (function_body ()) in
            elaborate (E.add_definition name fargs rtp bdy)



    let adt_definition (): unit t =
        let* _ = kw_class unit2 <?> "type definition" in
        assert false


    let definitions (leftmost: bool): unit t =
        let al =
            if leftmost then left_align else align
        in
        let* _ =
            skip_zero_or_more (
                (top_definition () </> adt_definition ())
                |> al
            )
        in
        return ()

    module Parser =
    struct
        include Basic.Parser
    end
end









(* Parser part of the Token_parser *)
module Make
        (Semantic: ANY)
        (E: ELABORATOR with type error = Semantic.t)
=
struct
    module C = Combinator (Semantic) (E)

    module Error = Error (Semantic) (E)

    include C.Parser

    let make (e: E.t): t =
        C.(make e (definitions false))


    let range_of_semantic (e: Error.t): Position.range =
        Error.to_range e

    let doc_of_semantic (e: Error.t): Pretty.doc =
        Error.to_doc e

    let is_elab_error (e: Error.t): bool =
        Error.is_elab e

    let elab_error (e: Error.t): Semantic.t =
        Error.elab e
end
