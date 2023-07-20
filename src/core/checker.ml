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
    req:   req option;
}







let is_valid_term (t: term) (g: gamma): bool =
    t.tgid = Gamma.index g
    &&
    t.tglen = Gamma.length g



let is_valid_req (req: req) (g: gamma): bool =
    req.rgid = Gamma.index g
    &&
    req.rglen = Gamma.length g



let term_in (term: Term.t) (g: gamma): term =
    {
        tgid  = Gamma.index g;
        tglen = Gamma.length g;
        term;
        req   = None;
    }


let type_of (t: term) (_: gamma): Term.t =
    let open Term in
    match t.term with
    | Prop ->
        Any 0
    | Any i ->
        Any (i + 1)
    | _ ->
        assert false (* nyi *)





let make_globals (): globals =
    Globals.make ()








module Make (M: MONAD) =
struct
    open M

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


    let head_normal (t: Term.t) (_: gamma): Term.t t =
        match t with
        | Prop | Any _ | Top | Pi _ | Lam _ | Type _ ->
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





    let check (term: term) (req: req) (g: gamma): term option t =
        (* check if the term satisfies the requirement in the context *)
        assert (is_valid_term term g);
        assert (is_valid_req  req  g);

        (* MISSING: Shortcut when the term already satisfies the requirement *)

        let tp = type_of term g in
        let* ok = unify tp true req.rtyp g in
        if ok then
            return (Some {term with req = Some req})
        else
            return None


    let any (gamma: gamma): term t =
        return (term_in (Term.Any 0) gamma)
end
