module Binder_info =
    struct
        type t = {
            name: string;
            implicit: bool;
            ghost: bool;
            with_type: bool;
        }
    end


type t =
    | Zero         (* Lowest predicative universe *)
    | Succ of t    (* Next predicative universe *)
    | Prop
    | Any of t
    | Local of int (* De Bruijn index *)
    | Meta  of int (* De Bruijn level *)
    | Pi of  binder array * t
    | Lam of binder array * t

and binder = Binder_info.t * t
