open Std

(* Experiment: Terms with phantom types *)
type hole
type nohole

type 'a tt =
    | Prop: 'a tt
    | Any:  int -> 'a tt
    (* ... *)
    | Meta: (int * int) -> hole tt


module Name_map =
struct
    type 'a t (* nyi *)
end




(* Type definition *)

type t  = Info.Pos.t option * t0

and tp  = t  (* synonym *)

and t0 =
    (* sorts *)
    | Prop
    | Any of int            (* 0 or 1 *)
    | Top

    (* variables *)
    | Local  of int         (* De Bruijn index *)
    | Global of int * int   (* Module, id in module *)
    | Meta  of int * int    (* Context, id in context *)

    (* bindings *)
    | Pi  of var_binder array * t
    | Lam of var_binder array * t
    | Let of let_binder array * t

    (* application *)
    | App of Info.App.t * t * argument array

    (* inductive types *)
    | Type of
          var_binder array   (* parameters *)
          * var_binder
          * constructor array
          * int Name_map.t

    (* pattern match *)
    | Case of var_binder
              * clause array
              * tree

    | Cta  of
          t         (* refers to pattern match expression *)
          * tree    (* case tree *)
          * pointer (* pointer into arguments *)


and var_binder = Info.Bind.t * tp

and let_binder = Info.Bind.t * tp * t

and gen_binder = Info.Bind.t * tp * t option

and argument = Info.Arg.t * t

and constructor =
    Name.t
    * constructor_arg array
    * t

and constructor_arg =
    Info.Bind.t
    * var_binder array
    * t

and clause =
    var_binder array (* pattern variables *)
    * pattern array  (* pattern *)
    * t              (* right hand side *)

and pattern =
    | Pvar of int
    (* nyi: Constants missing !!*)
    | Pmake of int * pattern array 

and tree =
    | Rhs of t
    | Catch_all of tree
    | Node of tree Name_map.t * tree option

and pointer = unit (* nyi *)






(* Functions *)

let prop: t = (None, Prop)
let any0: t = (None, Any 0)
let any1: t = (None, Any 1)
