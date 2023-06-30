open Std



module Binder =
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
end



module Argument =
struct
    type t = {
        implicit: bool;
    }
end


module Application =
struct
    type t =
        | Normal
        | Unary
        | Binary
end
