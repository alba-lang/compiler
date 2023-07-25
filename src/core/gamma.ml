open Fmlib_std
open Std



module Entry =
struct
    type t = {
        info: Info.Bind.t;
        typ:  Term.t;
        def:  Term.t option;
    }

    let make info typ def = {info; typ; def}

    let typ (e: t): Term.t =
        e.typ
(*
    let info (e: t): Info.Bind.t =
        e.info

    let definition (e: t): Term.t option =
        e.def
*)
end


type t = {
    id: int;
    previous: t option;
    globals: Globals.t;
    content: Entry.t Rb_array.t;
    map: int Name_map.t;
}




let index (g: t ): int =
    g.id




let length (g: t): int =
    Rb_array.length g.content



let de_bruijn (i: int) (g: t): int =
    assert (i < length g);
    length g - 1 - i



let globals (g: t): Globals.t =
    g.globals



let empty (id: int) (globals: Globals.t) : t =
    {
        id;
        previous = None;
        globals;
        content  = Rb_array.empty;
        map      = Name_map.empty;
    }




let entry (i: int) (g: t): Entry.t =
    assert (i < length g);
    Rb_array.element i g.content



let typ (i: int) (g: t): Term.t =
    assert (i < length g);
    Term.up (length g - i) (Entry.typ (entry i g))




let find_local (name: Name.t) (g: t): int option =
        Name_map.find_opt name g.map



let find_global (name: Name.t) (g: t): (int * int) list =
    Globals.find name g.globals


let push_variable
        (bnd: Info.Bind.t)
        (with_map: bool)
        (tp: Term.t)
        (id: int)
        (g: t)
    : t
    =
    { g with
      id;
      previous = Some g;
      content  =
          Rb_array.push (Entry.make bnd tp None) g.content;
      map =
          if with_map then
              Name_map.add (Info.Bind.name bnd) (length g) g.map
          else
              g.map;
    }
