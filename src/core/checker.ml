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



let is_type_req (req: req) (_: gamma): bool =
    req.rtyp = Term.Top



let is_type (t: term) (g: gamma): bool =
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

        | Pi (_, _) ->
            assert false

        | _ ->
            assert false (* nyi *)



    let strip_metas (t: term) (_: gamma): term t =
        let rec strip t =
            let open Term in
            match t with
            | Prop | Any _ | Top | Global _ | Local _ ->
                return t

            | Meta (_, _, _) ->
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
        let t    =
            term_in
                (Term.arrow a.term b.term)
                (Term.pi_sort sa sb)
                g
        in
        let* req = type_requirement g in
        return {t with req = Some req}





    let find (name: Name.t) (_: req) (g: gamma): term option t =
        match Gamma.find_local name g with
        | Some _ ->
            assert false (* nyi *)
        | None ->
            match Gamma.find_global name g with
            | [] ->
                assert false (* nyi *)

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
        let e    = Globals.Entry.make name tp si bdy in
        return (Ok (Globals.add e (Gamma.globals g)))
end
