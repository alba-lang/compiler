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
    typ:  Term.t * Term.t;        (* Type and the sort of the type. *)
    def:  Term.t option;
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



module Entry =
struct
    type gamma = t

    type t = entry


    let make previous info typ def = {previous; info; typ; def}

    let typ (e: t): Term.t * Term.t =
        e.typ

    let info (e: t): Info.Bind.t =
        e.info

    let definition (e: t): Term.t option =
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








let typ (i: int) (g: t): Term.t * Term.t =
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
        (tp: Term.t * Term.t)
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




let any (level: int) (g: t): term =
    Free (gamma0 g, Sort.Any level)


let top (level: int) (g: t): term =
    Free (gamma0 g, Sort.Top level)


let prop (g: t): term =
    Free (gamma0 g, Sort.Prop)


let meta (id: int) (tp: term) (g: t): term =
    Typed (g, Term.meta id, tp)
