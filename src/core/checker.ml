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
    req:   req option;
}






let gamma_length (g: gamma): int =
    Gamma.length g



let is_gamma_empty (g: gamma): bool =
    Gamma.length g = 0



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



let is_type_req (req: req) (_: gamma): bool =
    req.rtyp = Term.Top



let is_type (t: term) (g: gamma): bool =
    match t.req with
    | None ->
        false
    | Some req ->
        is_type_req req g



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








module Make (M: CHECKER_MONAD) =
struct
    open M

    module Intm = Intm.Make (M)

    module Optm =
    struct
        let map (f: 'a -> 'b t) (o: 'a option): 'b option t =
            match o with
            | None as o  -> return o
            | Some a     ->
                let* b = f a in
                return (Some b)
    end






    (* Internal Functions *)

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




    let signature (tp: Term.t) (g: gamma): Sign.t t =
        (* [tp] must be a type! *)
        let* tp = head_normal tp g in
        match tp with
        | Top ->
            assert false (* Illegal call *)

        | Prop | Any _ ->
            return Sign.Sort

        | _ ->
            assert false (* nyi *)



    let strip_metas (t: term) (_: gamma): term t =
        let strip t =
            let open Term in
            match t with
            | Prop | Any _ | Top | Global _ | Local _ ->
                return t
            | _ ->
                assert false
        in
        let* term = strip t.term in
        return {t with term}






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

        let tp = type_of term g in
        let* ok = unify tp true req.rtyp g in
        if ok then
            return (Some {term with req = Some req})
        else
            return None


    let any (gamma: gamma): term t =
        return (term_in (Term.Any 0) gamma)



    let push_variable (_: bool) (_: Name.t) (_: term) (_: gamma): gamma t =
        assert false




    let make_pi (nargs: int) (tp: term) (g: gamma): (term * gamma) t =
        assert (is_valid_term tp g);
        assert (nargs <= Gamma.length g);
        assert (is_type tp g);
        if nargs = 0 then
            return (tp, g)
        else
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
        let* tp  = map f (strip_metas tp g) in
        let* si  = signature tp g in
        let* bdy = Optm.map (fun bdy -> map f (strip_metas bdy g)) bdy in
        return (Ok (Globals.add_definition name tp si bdy (Gamma.globals g)))
end
