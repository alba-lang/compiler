open Fmlib_std
open Std





type req = {
    rid:   int;
    rgid:  int;
    rglen: int;
    rtyp:  Term.t;
    sign:  Sign.t option;      (* only for function terms *)
}







type t = {
    id: int;
    globals: Globals.t;
    content: entry Rb_array.t;
    map: int Name_map.t;
}

and entry = {
    previous: t;
    info: Info.Bind.t;
    typ:  Term.pair;        (* Type and the sort of the type. *)
    def:  Term.t option;
}


module Entry =
struct
    type gamma = t

    type t = entry


    let make previous info typ def = {previous; info; typ; def}

    let typ (e: t): Term.pair =
        e.typ

    let info (e: t): Info.Bind.t =
        e.info

    let definition (e: t): Term.t option =
        e.def

    let gamma (e: t): gamma =
        e.previous
end





let index (g: t ): int =
    g.id




let length (g: t): int =
    Rb_array.length g.content



let de_bruijn (i: int) (g: t): int =
    assert (i < length g);
    length g - 1 - i



let equal (g1: t) (g2: t): bool =
    g1.id = g2.id


let globals (g: t): Globals.t =
    g.globals



let empty (id: int) (globals: Globals.t) : t =
    {
        id;
        globals;
        content  = Rb_array.empty;
        map      = Name_map.empty;
    }



let entry (i: int) (g: t): Entry.t =
    assert (i < length g);
    Rb_array.element i g.content



let is_prefix (g0: t) (g: t): bool =
    let n0 = length g0
    and n  = length g
    in
    if n0 > n then
        false
    else if n0 = n then
        equal g0 g
    else
        let e = entry n0 g in
        equal g0 e.previous



let is_valid_req (req: req) (g: t): bool =
    req.rgid  = index g
    &&
    req.rglen = length g




let type_requirement (rid: int) (g: t): req =
    {
        rid;
        rgid  = index g;
        rglen = length g;
        rtyp  = Term.Top;
        sign  = None;
    }






let typ (i: int) (g: t): Term.pair =
    assert (i < length g);
    Term.pair_up (length g - i) (Entry.typ (entry i g))




let find_local (name: Name.t) (g: t): int option =
        Name_map.find_opt name g.map



let find_global (name: Name.t) (g: t): (int * int) list =
    Globals.find name g.globals


let push_variable
        (bnd: Info.Bind.t)
        (with_map: bool)
        (tp: Term.pair)
        (id: int)
        (g: t)
    : t
    =
    { g with
      id;
      content  =
          Rb_array.push (Entry.make g bnd tp None) g.content;
      map =
          if with_map then
              Name_map.add (Info.Bind.name bnd) (length g) g.map
          else
              g.map;
    }
