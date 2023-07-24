open Fmlib_std
open Std



(* Type definition *)

type t =
    (* sorts *)
    | Prop
    | Any of int            (* 0 or 1 *)
    | Top

    (* variables *)
    | Local  of Name.t * int        (* De Bruijn index *)
    | Global of Name.t * int * int  (* Module, id in module *)
    | Meta   of Name.t * int * int  (* Context, id in context *)

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


and tp  = t  (* synonym *)

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



let up_from (n: int) (_: int) (t: t): t =
    if n = 0 then
        t
    else
        assert false




let up (n: int) (t: t): t =
    up_from n 0 t




let prop: t = Prop
let any0: t = Any 0
let any1: t = Any 1


let arrow (a: t) (b: t): t =
    match b with
    | Pi (args, r) ->
        let args = Array.map (fun (b, r) -> b, up 1 r) args in
        let args = Array.push_front (Info.Bind.arrow, a) args in
        Pi (args, r)
    | b ->
        Pi ([| Info.Bind.arrow, a|], b)



let equal (_: t) (_: t): bool =
    assert false
