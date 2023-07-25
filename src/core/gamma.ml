open Fmlib_std
open Std





type t = {
    id: int;
    previous: t option;
    globals: Globals.t;
    content: Term.gen_binder Rb_array.t;
    map: int Name_map.t;
}




let index (g: t ): int =
    g.id




let length (g: t): int =
    Rb_array.length g.content




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




let find_local (name: Name.t) (g: t): Term.t option =
    Option.map
        (fun i -> Term.Local (name, length g - i - 1))
        (Name_map.find_opt name g.map)



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
          Rb_array.push (bnd, tp, None) g.content;
      map =
          if with_map then
              Name_map.add (Info.Bind.name bnd) (length g) g.map
          else
              g.map;
    }
