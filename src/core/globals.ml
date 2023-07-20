open Fmlib_std
open Std


module Package =
struct
    type t = {
        name: string;
    }

    let make name = {name}
end


module Entry =
struct
    type t =
        | Declaration of Name.t * Term.t            (* name, type *)
        | Definition  of Name.t * Term.t * Term.t   (* name, type, body *)
end



module Module =
struct
    type t = {
        name:    string;
        package: int;
        defs:    Entry.t Rb_array.t
    }

    let make name package =
        {
            name;
            package;
            defs = Rb_array.empty;
        }
end


type t = {
    packages: Package.t Rb_array.t;
    modules:  Module.t  Rb_array.t;
    current:  int;          (* current module *)
}


let make (): t =
    {
        packages = Rb_array.(push Package.(make "alba.core") empty);
        modules  = Rb_array.(push Module.(make "core" 0) empty);
        current  = 0;
    }
