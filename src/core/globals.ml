open Fmlib_std
open Std


module Map = Name_map.Multi


module Entry =
struct
    type t = {
        name: Name.t;
        typ:  Term.t;
        si:   Sign.t;
        bdy:  Term.t option;
    }

    let name e = e.name
    let typ  e = e.typ
    let sign e = e.si
    let body e = e.bdy

    let make name typ si bdy = {name; typ; si; bdy; }
end





module Package =
struct
    type t = {
        name: string;
    }

    let make name = {name}
end






module Module =
struct
    type t = {
        name:    string;
        package: int;
        defs:    Entry.t Rb_array.t
    }



    let length (m: t): int =
        Rb_array.length m.defs



    let make name package =
        {
            name;
            package;
            defs = Rb_array.empty;
        }


    let entry (i: int) (m: t): Entry.t =
        assert (i < length m);
        Rb_array.element i m.defs


    let add (e: Entry.t) (m: t): t =
        {m with defs = Rb_array.push e m.defs}
end








type t = {
    packages: Package.t Rb_array.t;
    modules:  Module.t  Rb_array.t;
    current:  int;          (* current module *)
    map: (int * int) Map.t;
}


let make (): t =
    {
        packages = Rb_array.(push Package.(make "alba.core") empty);
        modules  = Rb_array.(push Module.(make "core" 0) empty);
        current  = 0;
        map      = Map.empty;
    }




let n_modules (g: t): int =
    Rb_array.length g.modules




let entry (m: int) (i: int) (g: t): Entry.t =
    assert (m < n_modules g);
    Module.entry i (Rb_array.element m g.modules)



let add (e: Entry.t) (g: t): t =
    assert (Map.find (Entry.name e) g.map = []); (* nyi *)

    let m   = Rb_array.element g.current g.modules in
    let idx = Module.length m in
    let map = Map.add (Entry.name e) (g.current, idx) g.map in
    let m   = Module.add e m in
    { g with
      modules = Rb_array.replace g.current m g.modules;
      map
    }


let find (name: Name.t) (g: t): (int * int) list =
    Printf.printf "Globals.find %s\n" (Name.string name);
    Map.find name g.map
