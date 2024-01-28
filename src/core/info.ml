open Std


module Pos =
struct
    type t
end



module Bind =
struct
    type t = {
        name: Name.t;
        implicit: bool;
        with_type: bool;        (* Explicitly typed *)
        arrow: bool;            (* Only for products (A -> B) *)
    }

    let make name implicit with_type = {
        name;
        implicit;
        with_type;
        arrow = false;
    }

    let arrow = {
        name      = Name.wildcard;
        implicit  = false;
        with_type = true;
        arrow     = true;
    }


    let name (b: t): Name.t =
        b.name


    let is_arrow (b: t): bool =
        b.arrow


    let is_implicit (b: t): bool =
        b.implicit


    let is_explicit (b: t): bool =
        not b.implicit
end



module Arg =
struct
    type t =
        | Normal
        | Implicit_braced
        | Implicit
        | Operand
end


module App =
struct
    type t =
        | Normal
        | Unary
        | Binary
end


module Def =
struct
    type t = bool (* is transparent *)
end
