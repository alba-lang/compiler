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
    | Meta   of Name.t * int * int  (* Context length, id in context *)

    (* bindings *)
    | Pi  of var_binder array * pair
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


and pair = t * t    (* usually a term and its type or a type and its sort *)

and tp  = t  (* synonym *)

and var_binder = Info.Bind.t * pair         (* type *)

and let_binder = Info.Bind.t * pair * t     (* type + definition term *)

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


let map_de_bruijn (f: int -> int) (t: t): t =
    let rec go nb t =
        match t with
        | Top | Prop | Any _ | Global _ ->
            t

        | Local (name, i) as t ->
            if i < nb then
                t
            else
                let j = f (i - nb) in
                assert (0 <= j);
                Local (name, j + nb)

        | Pi (args, r) ->
            Pi (
                Stdlib.Array.mapi
                    (fun i (bnd, p) -> bnd, pair_go (nb + i) p)
                    args,
                pair_go (nb + Array.length args) r
            )

        | _ ->
            assert false (* nyi *)

    and pair_go nb (t, tp) =
        go nb t,
        go nb tp
    in

    go 0 t




let up_from (n: int) (start: int) (t: t): t =
    if n = 0 then
        t
    else
        map_de_bruijn
            (fun i ->
                 if i < start then
                     i
                 else
                     i + n
            )
            t


let pair_up_from (n: int) (start: int) ((t, tp): pair): pair =
    up_from n start t,
    up_from n start tp





let up (n: int) (t: t): t =
    up_from n 0 t




let pair_up (n: int) ((t, tp): pair): pair =
    up n t,
    up n tp





let prop: t = Prop
let any0: t = Any 0
let any1: t = Any 1



let pi_sort (sa: t) (sb: t): t =
    match sa, sb with
    | _,     Prop  -> Prop
    | Any i, Any j -> Any (max i j)
    | _            -> assert false (* Illegal call *)



let arrow (a: pair) ((bt, _) as b: pair): t =
    match bt with
    | Pi (args, r) ->
        let args = Array.map (fun (bnd, p) -> bnd, pair_up 1 p) args in
        let args = Array.push_front (Info.Bind.arrow, a) args in
        Pi (args, pair_up 1 r)
    | _ ->
        Pi ([| Info.Bind.arrow, a|], pair_up 1 b)



let equal (_: t) (_: t): bool =
    assert false
