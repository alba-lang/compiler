(* Experiment: Terms with phantom types *)
type hole
type nohole

type 'a tt =
    | Prop: 'a tt
    | Any:  int -> 'a tt
    (* ... *)
    | Meta: (int * int) -> hole tt





type t =
    | Prop
    | Any of int            (* 0 or 1 *)
    | Local  of int         (* De Bruijn index *)
    | Global of int * int   (* Module, id in module *)
    | Meta  of int * int    (* Context, id in context *)
    | Pi of  binder array * t
    | Lam of binder array * t
    | Application of Info.Application.t * t * argument array

and binder   = Info.Binder.t   * t

and argument = Info.Argument.t * t



let prop: t = Prop

let any0: t = Any 0

let any1: t = Any 1
