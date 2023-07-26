open Std
open Intf



type globals =
    Globals.t

type gamma =
    Gamma.t



type req = {
    rid:   int;
    rgid:  int;
    rglen: int;
    rtyp:  Term.t;
    sign:  Sign.t option;      (* only for function terms *)
}




type term = {
    tgid:  int;
    tglen: int;
    term:  Term.t;
    typ:   Term.t;
    req:   req option;
}






let gamma_length (g: gamma): int =
    Gamma.length g



let is_gamma_empty (g: gamma): bool =
    Gamma.length g = 0



let global_entry (m: int) (i: int) (g: gamma): Globals.Entry.t =
    Globals.entry m i (Gamma.globals g)



let is_valid_term (t: term) (g: gamma): bool =
    t.tgid = Gamma.index g
    &&
    t.tglen = Gamma.length g




let is_valid_term_opt (t: term option) (g: gamma): bool =
    match t with
    | None ->    true
    | Some t ->  is_valid_term t g




let is_valid_req (req: req) (g: gamma): bool =
    req.rgid = Gamma.index g
    &&
    req.rglen = Gamma.length g





let is_prefix (g0: gamma) (g: gamma): bool =
    Gamma.is_prefix g0 g





let is_type_req (req: req) (_: gamma): bool =
    req.rtyp = Term.Top





let is_type (t: term) (g: gamma): bool =
    (* Does the term [t] satisfy a type requirement.

       Note: The term might be a type, but it has not yet been checked that it
       satisfies a type requirement. In that case the function [is_type] returns
       [false].
    *)
    match t.req with
    | None ->
        false
    | Some req ->
        is_type_req req g



let term_in (term: Term.t) (typ: Term.t) (g: gamma): term =
    {
        tgid  = Gamma.index g;
        tglen = Gamma.length g;
        term;
        typ;
        req   = None;
    }




let make_globals (): globals =
    Globals.make ()












(* Monadic Functions *)

module Make (M: CHECKER_MONAD) =
struct
    open M

    module Intm   = Intm.Make (M)

    module Arraym = Arraym.Make (M)

    module Optm =
    struct
        let map (f: 'a -> 'b t) (o: 'a option): 'b option t =
            match o with
            | None as o  -> return o
            | Some a     ->
                let* b = f a in
                return (Some b)
    end












    (* Internal Functions with Raw Terms *)



    let head_normal (t: Term.t) (_: gamma): Term.t t =
        match t with
        | Prop | Any _ | Top | Pi _ | Lam _ | Type _
        | Local _ | Global _ | Meta _ ->
            return t

        | _ ->
            assert false (* nyi *)



    let unify (t1: Term.t) (_: bool) (t2: Term.t) (g: gamma): bool t =
        let* t1 = head_normal t1 g in
        let* t2 = head_normal t2 g in

        match t1, t2 with
        | Prop, Prop | Prop, Any _ | Prop, Top ->
            return true

        | Any i, Any j when i <= j ->
            return true

        | Any _, Top ->
            return true

        | _, _ ->
            assert false (* nyi *)



    let push_variable
            (bnd: Info.Bind.t)
            (with_map: bool)
            (tp: Term.t)
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

        | Pi (args, r) ->
            let* lst, g =
                Arraym.fold_left
                    (fun (lst, g) (bnd, arg) ->
                         let* si = signature arg g in
                         let* g  = push_variable bnd false arg g in
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
        (* Strip all metavariables not valid in a context above level [n] *)
        assert (0 <= n);
        assert (n <= Gamma.length g);
        let rec strip t =
            let open Term in
            match t with
            | Prop | Any _ | Top | Global _ | Local _ ->
                return t

            | Meta (_, i, _) as m ->
                assert (i <= Gamma.length g);
                if i <= n then
                    return m
                else
                    assert false (* nyi *)

            | Pi (args, r) ->
                let* args =
                    Arraym.mapi
                        (fun _ (bnd, arg) ->
                             map (fun arg -> bnd, arg) (strip arg)
                        )
                        args
                in
                let* r =
                    strip r
                in
                return (Pi (args, r))

            | _ ->
                assert false
        in
        if n = Gamma.length g then
            return t
        else
            strip t












    (* Internal Functions with Terms *)



    let strip_metas1 (t: term) (i: int) (g: gamma): term t =
        map (fun term -> {t with term}) (strip_metas t.term i g)





    (* External Functions *)



    let empty_gamma (globals: globals): gamma M.t =
        M.(
            let* id = new_context in
            return (Gamma.empty id globals)
        )




    let type_requirement (gamma: gamma): req t =
        let* rid = new_id in
        return {
            rid;
            rgid  = Gamma.index gamma;
            rglen = Gamma.length gamma;
            rtyp  = Term.Top;
            sign  = None;
        }




    let check (term: term) (req: req) (g: gamma): term option t =
        (* check if the term satisfies the requirement in the context *)
        assert (is_valid_term term g);
        assert (is_valid_req  req  g);

        (* MISSING: Shortcut when the term already satisfies the requirement *)

        let* ok = unify term.typ true req.rtyp g in
        if ok then
            return (Some {term with req = Some req})
        else
            return None





    let any (gamma: gamma): term t =
        return (term_in (Term.Any 0) (Term.Any 1) gamma)





    let arrow (a: term) (b: term) (g: gamma): term t =
        assert (is_valid_term a g);
        assert (is_valid_term b g);
        assert (is_type a g);
        assert (is_type b g);
        let* sa  = head_normal a.typ g in
        let* sb  = head_normal b.typ g in
        return (
            term_in
                (Term.arrow a.term b.term)
                (Term.pi_sort sa sb)
                g
        )





    let find (name: Name.t) (_: req) (g: gamma): term option t =
        match Gamma.find_local name g with
        | Some i ->
            return (Some (
                term_in
                    (Term.Local (name, Gamma.de_bruijn i g))
                    (Gamma.typ i g)
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
        : gamma t
        =
        assert (is_type tp g);
        let bnd = Info.Bind.make name implicit with_type
        in
        push_variable bnd true tp.term g





    let make_pi (tp: term) (g: gamma) (g0: gamma): term t =
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
                             Gamma.Entry.(head_normal (typ e) (gamma e))
                         in
                         return (Term.pi_sort sa sb)
                    )
                    sort
                    nargs
            in
            assert false





    let add_definition
            (name: Name.t) (tp: term) (bdy: term option) (g: gamma)
        : (globals, term) result t
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
