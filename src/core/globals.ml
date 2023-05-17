open Fmlib_std
open Alba_parse


module Package =
struct
    type t = {
        name: string;
    }
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
end


type t = {
    packages: Package.t Rb_array.t;
    modules:  Module.t  Rb_array.t;
}

