open Std


type t =
    int * t0

and t0 =
   | Sort of Sort.t
   | Meta of int
   | Var  of Name.t
   | App  of t * t array
   | Lam  of int * varbind array * (t * t * t) (* start of args *)
   | Pi   of int * varbind array * (t * t)     (* start of args *)
   | Ann  of t * t * t

and varbind = Info.Bind.t * t * t



let is_up_by (j, _) = j



let up (i: int) ((j,t): t): t =
    i + j, t


let count_implicits (_: int) (_: varbind array): int =
    assert false


let sort (s: Sort.t): t =
    0, Sort s


let prop  : t = sort Prop
let any i : t = sort (Any i)
let top i : t = sort (Top i)

let meta i: t = 0, Meta i


let var i n = i, Var n


let annotated (i1, t: t) (i2, tp: t) (i3, s: t): t =
    let _ =
        match s with
        | Sort _ ->
            ()
        | _ ->
            assert false (* [s] is the type of a type and therefore must be a
                            sort. *)
    in
    let upn = min (min i1 i2) i3 in
    upn, Ann ((i1 - upn, t), (i2 - upn, tp), (i3 - upn, s))




let pi_sort (t1: t) (t2: t): t =
    match t1, t2 with
    | (i, Sort s1), (_, Sort s2) ->
        i, (Sort (Sort.pi s1 s2))

    | _, _ ->
        assert false (* illegal call, polymorphic sorts nyi *)



let pi1 (a: (Info.Bind.t * t * t)) ((_, r0), _ as r: t * t): t =
    0
    ,
    match r0 with
    | Pi (start, args, rtp) ->
        assert (start = 0);
        Pi (start, Fmlib_std.Array.insert 0 a args, rtp)

    | _ ->
        Pi (0, [|a|], r)
