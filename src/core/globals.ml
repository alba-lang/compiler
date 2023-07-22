open Fmlib_std
open Std




module Package =
struct
    type t = {
        name: string;
    }

    let make name = {name}
end






module Module =
struct
    type entry =
        Name.t * Term.t * Sign.t * Term.t option

    type t = {
        name:    string;
        package: int;
        defs:    entry Rb_array.t
    }



    let length (m: t): int =
        Rb_array.length m.defs



    let make name package =
        {
            name;
            package;
            defs = Rb_array.empty;
        }

    let add
            (name: Name.t) (tp: Term.t) (si: Sign.t) (bdy: Term.t option)
            (m: t)
        : t
        =
        {m with defs = Rb_array.push (name, tp, si, bdy) m.defs}
end








type t = {
    packages: Package.t Rb_array.t;
    modules:  Module.t  Rb_array.t;
    current:  int;          (* current module *)
    map: (int * int) Name_map.t;
}


let make (): t =
    {
        packages = Rb_array.(push Package.(make "alba.core") empty);
        modules  = Rb_array.(push Module.(make "core" 0) empty);
        current  = 0;
        map      = Name_map.empty;
    }




let add_definition
        (name: Name.t)
        (tp: Term.t)
        (si: Sign.t)
        (bdy: Term.t option)
        (g: t)
    : t
    =
    assert (Name_map.find_opt name g.map = None);
    let m   = Rb_array.element g.current g.modules in
    let idx = Module.length m in
    let map = Name_map.add name (g.current, idx) g.map in
    let m   = Module.add name tp si bdy m in
    { g with
      modules = Rb_array.replace g.current m g.modules;
      map
    }
