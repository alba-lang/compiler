open Fmlib_std
open Std






type t = {
    globals: Globals.t;
    content: entry Rb_array.t;
    map: int Name_map.t;
}

and entry = {
    previous: t;
    info: Info.Bind.t;
    typ:  term;
    def:  term option;
}

and term =
    | Free of  t * Sort.t
    | Typed of t * Term.t * term (* Gamma |- t : T *)






let length (g: t): int =
    Rb_array.length g.content



let gamma_of_term: term -> t = function
    | Free (g, _)     -> g
    | Typed (g, _, _) -> g


let term_of_term: term -> Term.t = function
    | Free (g, s)    -> Term.(sort s |> up (length g))
    | Typed(_, t, _) -> t


let type_of_term: term -> term = function
    | Free (g, s) ->
        Free (g, Sort.type_of s)
    | Typed(_, _, tp) ->
        tp


let update_type (t: term) (ty: term): term =
    match t with
    | Typed (g, raw, _) ->
        Typed (g, raw, ty)
    | _ ->
        t



let make_sort (s: Term.t) (g: t): term =
    match s with
    | (_, Sort s0) ->
        Free (g, s0)

    | _ ->
        assert false (* nyi: Polymorphic Any *)




let string_of_term (t: term): string =
    Printer.Term.base_string true true (term_of_term t)
let _ = string_of_term







module Entry =
struct
    type gamma = t

    type t = entry


    let make previous info typ def = {previous; info; typ; def}

    let typ (e: t): term =
        e.typ

    let info (e: t): Info.Bind.t =
        e.info

    let definition (e: t): term option =
        e.def

    let gamma (e: t): gamma =
        e.previous
end





let de_bruijn (i: int) (g: t): int =
    assert (i < length g);
    length g - 1 - i



let globals (g: t): Globals.t =
    g.globals



let empty (globals: Globals.t) : t =
    {
        globals;
        content  = Rb_array.empty;
        map      = Name_map.empty;
    }


let entry (i: int) (g: t): Entry.t =
    assert (i < length g);
    Rb_array.element i g.content



let entry_of_bruijn (i: int) (g: t): Entry.t =
    entry (de_bruijn i g) g



let previous (g: t): t =
    assert (0 < length g);
    (entry_of_bruijn 0 g).previous



let gamma0 (g: t): t =
    if length g = 0 then
        g
    else
        (entry 0 g).previous


let is_prefix (g0: t) (g: t): bool =
    let n0 = length g0
    and n  = length g
    in
    if n0 > n then
        false
    else if n0 = n then
        g0 == g
    else
        let e = entry n0 g in
        g0  == e.previous








let typ (i: int) (g: t): term =
    assert (i < length g);
    assert false
    (*Term.pair_up (length g - i) (Entry.typ (entry i g))*)




let find_local (name: Name.t) (g: t): int option =
        Name_map.find_opt name g.map



let find_global (name: Name.t) (g: t): (int * int) list =
    Globals.find name g.globals


let push_variable
        (bnd: Info.Bind.t)
        (with_map: bool)
        (tp: term)
        (g: t)
    : t
    =
    { g with
      content  =
          Rb_array.push (Entry.make g bnd tp None) g.content;
      map =
          if with_map then
              Name_map.add (Info.Bind.name bnd) (length g) g.map
          else
              g.map;
    }




let make_pi1 (b: Info.Bind.t) (tp: term) (rtp: term): term =
    let g0 = gamma_of_term tp
    and g  = gamma_of_term rtp in
    assert (g0 == previous g);
    let s1 = term_of_term (type_of_term tp)
    and s2 = term_of_term (type_of_term rtp)
    in
    Typed (
        g0
        ,
        Term.pi1
            (b, term_of_term tp, s1)
            (term_of_term rtp, s2)
        ,
        make_sort (Term.pi_sort s1 s2) g0
    )





let make_pi (t: term) (g: t) (g0: t): term =
    assert (length g0 <= length g);
    let rec make_args i (args, s) =
        if i = length g0 then
            args, s
        else
            let i = i - 1
            in
            let e = entry i g in
            assert (e.def = None);
            let ty = term_of_term e.typ in
            let st = term_of_term (type_of_term e.typ) in
            let s  = Term.pi_sort st s in
            make_args i ((e.info, ty, s) :: args, s)
    in
    let res  = term_of_term t in
    let sres = term_of_term (type_of_term t)
    in
    let args, s = make_args (length g) ([], sres)
    in
    let args = Array.of_list args
    in
    let pi = Term.pi args (res, sres) in
    Typed (g0, pi, make_sort s g0)





let any (level: int) (g: t): term =
    Free (g, Sort.Any level)


let top (level: int) (g: t): term =
    Free (g, Sort.Top level)


let prop (g: t): term =
    Free (g, Sort.Prop)


let meta (id: int) (tp: term): term =
    Typed (gamma_of_term tp, Term.meta id, tp)
