open Fmlib_std
open Std



type range = Fmlib_parse.Position.range


type globals =
    Globals.t

type gamma =
    Gamma.t




module Req =
struct
    type t = {
        rid:        int;
        rgid:       int;
        rglen:      int;
        rtyp:       Term.t;                 (* result type *)
        arg_typs:   (bool * Term.t) array;  (* argument types *)
    }

    let is_normal (req: t): bool =
        (* Is the requirement a required type for a term? *)
        Array.is_empty req.arg_typs


    let is_function_type (req: t): bool =
        not (Array.is_empty req.arg_typs)


    let n_implicits (req: t): int =
        Array_plus.count_cond
            0
            fst
            req.arg_typs


    let has_explicit_argument (n_implicits: int) (req: t): bool =
        let nargs = Array.length req.arg_typs
        in
        assert (n_implicits <= nargs);
        n_implicits < nargs
        &&
        not (fst req.arg_typs.(n_implicits))


    let is_type (req: t): bool =
        (* Is the requirement for a type term? *)
        is_normal req
        &&
        req.rtyp = Term.Top


    let argument_type (req: t): Term.t =
        assert (is_function_type req);
        snd req.arg_typs.(0) (* MISSING: Correct handling of implicit arguments
                             *)


    let of_type (rid: int) (rtyp: Term.t) (g: gamma): t =
        {
            rid;
            rgid  = Gamma.index g;
            rglen = Gamma.length g;
            rtyp;
            arg_typs  = [||];
        }

    let type_requirement (rid: int) (g: gamma): t =
        of_type rid Term.Top g
end


type req = Req.t





type term = {
    (* A term is valid in a certain context. *)
    tgid:  int;         (* id of gamma *)
    tglen: int;         (* length of gamma *)
    term:  Term.t;
    typ:   Term.t;
    req:   req option;
}


type doc = Fmlib_pretty.Print.doc



module type MONAD =
sig
    include Fmlib_std.Interfaces.MONAD
    (** @inline *)

    val map: ('a -> 'b) -> 'a t -> 'b t

    val new_id:      int t
    val new_context: int t

    type meta_reason

    val new_meta: meta_reason -> req -> int -> int t

    val spawn: unit t -> unit t
end







let gamma_length (g: gamma): int =
    Gamma.length g



let is_gamma_empty (g: gamma): bool =
    Gamma.length g = 0



let global_entry (m: int) (i: int) (g: gamma): Globals.Entry.t =
    Globals.entry m i (Gamma.globals g)



let is_valid_req (req: req) (g: gamma): bool =
    req.rgid  = Gamma.index g
    &&
    req.rglen = Gamma.length g






let is_valid_term (t: term) (g: gamma): bool =
    t.tgid = Gamma.index g
    &&
    t.tglen = Gamma.length g





let is_valid_term_opt (t: term option) (g: gamma): bool =
    match t with
    | None ->    true
    | Some t ->  is_valid_term t g







let is_prefix (g0: gamma) (g: gamma): bool =
    Gamma.is_prefix g0 g








let is_type (t: term) (_: gamma): bool =
    (* Does the term [t] satisfy a type requirement.

       Note: The term might be a type, but it has not yet been checked that it
       satisfies a type requirement. In that case the function [is_type] returns
       [false].
    *)
    match t.req with
    | None ->
        false
    | Some req ->
        Req.is_type req



let term_in (term: Term.t) (typ: Term.t) (g: gamma): term =
    {
        tgid  = Gamma.index g;
        tglen = Gamma.length g;
        term;
        typ;
        req   = None;
    }


let term_req_in (term: Term.t) (typ: Term.t) (req: Req.t) (g: gamma): term =
    {
        tgid  = Gamma.index g;
        tglen = Gamma.length g;
        term;
        typ;
        req = Some req;
    }



let term_with_req (req: req) (term: term): term =
    assert (term.req = None);
    {term with req = Some req}





let make_globals (): globals =
    Globals.make ()






(*
============================================================
Printing Errors
============================================================
 *)




module Print:
sig
    val not_check:
        term -> req -> gamma -> unit -> Fmlib_pretty.Print.doc
end
=
struct
    open Fmlib_pretty

    let indent (d: Print.doc): Print.doc =
        Print.nest 4 d

    let not_check_function
            (t: term) (req: req) (_: gamma)
        : Print.doc
        =
        let open Print in
        [
            wrap_words "The term has type" <+> cut
            ;
            Term.Print.doc t.typ <+> cut |> indent
            ;
            wrap_words
                "but should it have a function type accepting at least one \
                 argument of type" <+> cut
            ;
            Req.argument_type req |> Term.Print.doc <+> cut |> indent

        ]
        |> paragraphs



    let not_check
            (t: term) (req: req) (g: gamma) ()
        : Print.doc
        =
        if Req.is_normal req then
            assert false
        else if Req.is_function_type req then
            not_check_function t req g
        else
            assert false
end










(*
============================================================
Monadic Functions
============================================================
 *)

module Make (M: MONAD) =
struct
    open M

    module Intm   = Intm.Make (M)

    module Arraym = Array_plus.Make (M)

    module Optm =
    struct
        let map (f: 'a -> 'b t) (o: 'a option): 'b option t =
            match o with
            | None as o  -> return o
            | Some a     ->
                let* b = f a in
                return (Some b)
    end

    module Res =
    struct
        type 'a t = ('a, unit -> doc) result M.t

        let return (a: 'a): 'a t =
            M.return (Ok a)


        let fail (e: unit -> doc): 'a t =
            M.return (Error e)

        let ( let* ) (m: 'a t) (f: 'a -> 'b t): 'b t =
            M.(
                let* r = m in
                match r with
                | Ok a ->
                    f a
                | Error _ as r ->
                    return r
            )

        let _ = return, fail, ( let* )
    end












    (* Internal Functions with Raw Terms *)


    let head_normal_base
            (with_meta: bool )
            (t: Term.t)
            (_: gamma)
        : Term.t t
        =
        match t with
        | Prop | Any _ | Top | Pi _ | Lam _ | Type _
        | Local _ | Global _ ->
            return t

        | Meta _ ->
            if with_meta then
                return t
            else
                assert false (* nyi: wait for instantiation *)

        | _ ->
            assert false (* nyi *)


    let head_normal (t: Term.t) (g: gamma): Term.t t =
        head_normal_base true t g



    let head_normal_wait (t: Term.t) (g: gamma): Term.t t =
        head_normal_base false t g



    let normalize_function_type
            (_: int)
            (tp: Term.t)
            (g: gamma)
        : Term.t t
        =
        let is_implicit (bnd, _) = Info.Bind.is_implicit bnd
        in
        let* tp = head_normal_wait tp g
        in
        match tp with
        | Pi (args, _) ->

            let len      = Array.length args in
            let ni_act   = Array_plus.count_cond 0 is_implicit args
            in
            if ni_act = len then
                (* analyze result type *)
                assert false
            else
                return tp

        | _ as tp ->

            return tp




    let rec unify (t1: Term.t) (t2: Term.t) (g: gamma): bool t =
        (* Is [t1] a subtype of [t2]? *)
        let* t1 = head_normal t1 g in
        let* t2 = head_normal t2 g in

        match t1, t2 with
        | Prop, Prop | Prop, Any _ | Prop, Top ->

            return true

        | Any i, Any j when i <= j ->

            return true

        | Any _, Top ->

            return true

        | Meta (_, _, _), Meta (_, _, _) ->

            assert false (* nyi *)

        | Meta (_, len, id), t2 ->

            unify_meta len id t2 g

        | _, Meta (_, _, _) ->
            assert false (* nyi *)

        | Pi (_, _), Pi (_, _) ->
            assert false (* nyi *)

        | _, _ ->
            assert false (* nyi *)


    and unify_meta (_: int) (_: int) (_: Term.t) (_: gamma): bool t =
        assert false






    let push_variable_bnd
            (bnd: Info.Bind.t)
            (with_map: bool)
            (tp: Term.pair)
            (g: gamma)
        : gamma t
        =
        let* id = new_context in
        return (Gamma.push_variable bnd with_map tp id g)




    let rec signature (tp: Term.t) (g: gamma): Sign.t t =
        (* [tp] must be a type! *)
        let* tp = head_normal tp g in
        match tp with
        | Top ->
            assert false (* Illegal call *)

        | Prop | Any _ ->
            return Sign.Sort

        | Local _ ->
            return Sign.Unknown

        | Global (name, m, i) ->
            return (Sign.Global (name, m, i))

        | Meta (name, len, i) ->
            return (Sign.Meta (name, len, i))

        | Pi (args, (r, _)) ->
            let* lst, g =
                Arraym.fold_left
                    (fun (lst, g) (bnd, (a, _ as arg)) ->
                         let* si = signature a g in
                         let* g  = push_variable_bnd bnd false arg g in
                         return
                             ((Info.Bind.is_implicit bnd, si) :: lst, g))
                    ([], g)
                    args
            in
            let* rsi = signature r g
            in
            return (Sign.Fun ( Array.of_list (List.rev lst), rsi))

        | _ ->
            assert false (* nyi *)





    let strip_metas (t: Term.t) (n: int) (g: gamma): Term.t t =
        (* Strip all metavariables of level [n] or higher *)
        assert (0 <= n);
        assert (n <= Gamma.length g);
        let rec strip t =
            let open Term in
            match t with
            | Prop | Any _ | Top | Global _ | Local _ ->
                return t

            | Meta (_, i, _) as m ->
                assert (i <= Gamma.length g);
                if i < n then
                    return m
                else
                    assert false (* nyi *)

            | Pi (args, r) ->
                let* args =
                    Arraym.mapi
                        (fun _ (bnd, arg) ->
                             map (fun arg -> bnd, arg) (strip_pair arg)
                        )
                        args
                in
                let* r =
                    strip_pair r
                in
                return (Pi (args, r))

            | _ ->
                assert false

        and strip_pair (t, tp) =
            let* t = strip t in
            map (fun tp -> t, tp) (strip tp)
        in

        if n = Gamma.length g then
            return t
        else
            strip t














    (* Internal Functions with Terms *)





    let strip_metas1 (t: term) (n: int) (g: gamma): term t =
        map (fun term -> {t with term}) (strip_metas t.term n g)



    let check_function_term
            (_: range)
            (t: term)
            (req: req)
            (g: gamma)
        : term t
        =
        assert (Req.is_function_type req);
        let ni_req   = Req.n_implicits req in
        let expl_req = Req.has_explicit_argument ni_req req
        in
        let* tp = normalize_function_type ni_req t.typ g
        in
        match tp with
        | Pi (args, _) as tp ->
            let _ = term_req_in in  (* DELETE *)
            let nargs = Array.length args
            in
            let ni_act =
                Array_plus.count_cond
                    0
                    (fun (bnd, _) -> Info.Bind.is_implicit bnd)
                    args
            in
            let expl_act =
                ni_act < nargs
                && Info.Bind.is_explicit (fst args.(ni_act))
            in
            if ni_act < ni_req then

                (* error: too few implicit arguments *)
                assert false (* nyi *)

            else if expl_req && not expl_act then

                (* error: no explicit argument *)
                assert false (* nyi *)

            else if ni_req < ni_act then

                (* fill with metavariables *)
                assert false (* nyi *)

            else
                let (_, (arg_tp, _)) = args.(0) in
                let* ok = unify (Req.argument_type req) arg_tp g in
                if ok then
                    return (term_req_in t.term tp req g)
                else
                    (* error: actual argument type does not conform to the
                     * argument type of the function term *)
                    assert false
        | _ ->
            (* error: term does not have a function type *)
            assert false





    (* External Functions *)



    let empty_gamma (globals: globals): gamma M.t =
        M.(
            let* id = new_context in
            return (Gamma.empty id globals)
        )




    let type_requirement (g: gamma): req t =
        let* rid = new_id in
        return (Req.type_requirement rid g)




    let requirement_of_type (tp: term) (g: gamma): req t =
        assert (is_type tp g);
        let* rid = new_id in
        return (Req.of_type rid tp.term g)


    let function_requirement
            (i: bool)   (* [f {a}] explicitly given implicit argument *)
            (tp: term)  (* Metavariable describing the argument type *)
            (req: req)  (* Requirement for the result type *)
            (g: gamma)
        : req t
        =
        assert (is_valid_req  req g);
        assert (is_valid_term tp g);
        assert (is_type tp g);
        return {
            req with
            arg_typs = Array.push_front (i, tp.term) req.arg_typs;
        }



    let make_meta (r: meta_reason) (req: req) (g: gamma): term t =
        let* midx =
            M.new_meta r req (Gamma.index g)
        in
        let  name =
            String.concat ""
                ["?"
                ;string_of_int (Gamma.length g)
                ; "."
                ; string_of_int midx
                ]
            |> Name.normal
        in
        return (
            term_in
                (Term.(Meta (name, Gamma.length g, midx)))
                req.rtyp
                g
            |> term_with_req req
        )




    let fill_meta (_: term) (_: term) (_: gamma): unit t =
        assert false


    let check (range: range) (term: term) (req: req) (g: gamma): term t =
        (* check if the term satisfies the requirement in the context *)
        Printf.printf "Check term: %s\n" (Term.Print.string term.term);
        assert (is_valid_term term g);
        assert (is_valid_req  req  g);

        let check () =
            let uni t tp =
                let* ok = unify t tp g in
                if ok then
                    return {term with req = Some req}
                else
                    return (assert false)
            in
            if Req.is_normal req then
                uni term.typ req.rtyp
            else if Req.is_function_type req then
                (* Function requirement *)
                check_function_term range term req g
            else
                assert false (* nyi: supertype *)
        in
        match term.req with
        | Some r ->
            if r.rid = req.rid then
                return term
            else
                check ()

        | None ->
            check ()




    let any (gamma: gamma): term M.t =
        return (term_in (Term.Any 0) (Term.Any 1) gamma)



    let apply (_: term) (_: term) (_: gamma): term M.t =
        assert false


    let arrow (a: term) (b: term) (g: gamma): term M.t =
        assert (is_valid_term a g);
        assert (is_valid_term b g);
        assert (is_type a g);
        assert (is_type b g);
        let* sa  = head_normal a.typ g in
        let* sb  = head_normal b.typ g in
        return (
            term_in
                (Term.arrow (a.term, a.typ) (b.term, b.typ))
                (Term.pi_sort sa sb)
                g
        )





    let find (name: Name.t) (_: req) (g: gamma): term option M.t =
        match Gamma.find_local name g with
        | Some i ->
            return (Some (
                term_in
                    (Term.Local (name, Gamma.de_bruijn i g))
                    (fst (Gamma.typ i g))
                    g
            ))
        | None ->
            match Gamma.find_global name g with
            | [] ->
                return None

            | [m, idx] ->
                let e = global_entry m idx g
                in
                return (
                    Some (
                        term_in
                            (Term.Global (name, m, idx))
                            (Globals.Entry.typ e)
                            g
                    )
                )

            | _ ->
                assert false (* nyi *)





    let push_variable
            (implicit: bool)
            (with_type: bool)
            (name: Name.t)
            (tp: term)
            (g: gamma)
        : gamma M.t
        =
        assert (is_type tp g);
        let bnd = Info.Bind.make name implicit with_type
        in
        push_variable_bnd bnd true (tp.term, tp.typ) g





    let make_pi (tp: term) (g: gamma) (g0: gamma): term M.t =
        assert (is_valid_term tp g);
        assert (is_prefix g0 g);
        assert (is_type tp g);
        if Gamma.equal g0 g then
            return tp
        else
            let len  = Gamma.length g
            and len0 = Gamma.length g0
            in
            assert (len0 < len);
            let nargs = len - len0
            in
            (* MISSING:!!!!!
                    Stripping of metavariables not valid in [g0].
            *)
            let* sort =
                head_normal tp.term g
            in
            let* _ =
                Intm.fold_down
                    (fun i sb ->
                         let e = Gamma.entry i g in
                         let* sa =
                             Gamma.Entry.(head_normal (fst (typ e)) (gamma e))
                         in
                         return (Term.pi_sort sa sb)
                    )
                    sort
                    nargs
            in
            assert false





    let add_definition
            (name: Name.t) (tp: term) (bdy: term option) (g: gamma)
        : (globals, term) result M.t
        =
        assert (is_gamma_empty g);
        assert (is_valid_term tp g);
        assert (is_valid_term_opt bdy g);
        let f t = t.term
        in
        let* tp  = map f (strip_metas1 tp 0 g) in
        let* si  = signature tp g in
        let* bdy = Optm.map (fun bdy -> map f (strip_metas1 bdy 0 g)) bdy in
        let e    = Globals.Entry.make name tp si bdy in
        return (
            Fmlib_std.Result.map_error
                (fun (m, i) ->
                     let e = global_entry m i g
                     in
                     term_in (Global (name, m, i)) (Globals.Entry.typ e) g)
                (Globals.add e (Gamma.globals g))
        )
end


(* Note [Possible Errors in Normalizing a Function Type]

    Situation:
        A term is applied to [ni_req] explicitly given implicit arguments
        followed by an optional explicit argument. The term has type [typ]. This
        requires that [typ] reduces to the form

            all {a1: A1} .. {an: An} (e: E): ....

        where [ni_req <= n] and [e] is present if an explicit argument is
        present. This makes sense only if there is at least one argument, either
        an explicitly given implicit argument or an explicit argument.

    Possible errors:

        - The type [typ] does not reduce to a function type.

        - The type [typ] has not sufficient implicit arguments after reduction.

        - The type [typ] has sufficient implict arguments, but no explicit
        argument and an explicit argument is present.
*)
