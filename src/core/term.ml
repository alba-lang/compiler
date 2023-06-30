open Std



module Binder_info =
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



module Argument_info =
struct
    type t = {
        implicit: bool;
    }
end


module Application_info =
struct
    type t =
        | Normal
        | Unary
        | Binary
end


(* Experiment: Terms with phantom types *)
type me
type el

type 'a tt =
    | Prop: 'a tt
    | Any:  int -> 'a tt
    (* ... *)
    | Meta: (int * int) -> me tt





type t =
    | Prop
    | Any of int            (* 0 or 1 *)
    | Local  of int         (* De Bruijn index *)
    | Global of int * int   (* Module, id in module *)
    | Meta  of int * int    (* Context, id in context *)
    | Pi of  binder array * t
    | Lam of binder array * t
    | Application of Application_info.t * t * argument array

and binder   = Binder_info.t   * t

and argument = Argument_info.t * t



let prop: t = Prop

let any0: t = Any 0

let any1: t = Any 1
