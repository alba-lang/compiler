type t =
    int * t0

and t0 =
   | Sort of Sort.t
   | Meta of int
   | Pi of
         int                            (* Start of arguments *) 
         * (Info.Bind.t * t * t) array  (* (bind, ty, s): s is always a sort *)
         * (t * t)                      (* Result type and its sort *)
   | Ann of t * t * t


let is_up_by (j, _) = j



let up (i: int) ((j,t): t): t =
    i + j, t



let sort (s: Sort.t): t =
    0, Sort s


let prop  : t = sort Prop
let any i : t = sort (Any i)
let top i : t = sort (Top i)

let meta i: t = 0, Meta i



let annotated (i1,_ as t: t) (i2,_ as tp: t) (i3,s0 as s: t): t =
    assert (i1 = i2);
    assert (i2 = i3);
    let _ =
        match s0 with
        | Sort _ ->
            ()
        | _ ->
            assert false (* [s] is the type of a type and therefore must be a
                            sort. *)
    in
    i1, Ann (t, tp, s)



let pi_sort (t1: t) (t2: t): t =
    match t1, t2 with
    | (i, Sort s1), (_, Sort s2) ->
        i, (Sort (Sort.pi s1 s2))

    | _, _ ->
        assert false (* illegal call, polymorphic sorts nyi *)



let pi1
        ((_, (na, _), _) as a: (Info.Bind.t * t * t))
        (((nr, r0), _)   as r: t * t)
    : t
    =
    min na nr
    ,
    match r0 with
    | Pi (start, args, rtp) ->
        assert (start = 0);
        Pi (
            start,
            Fmlib_std.Array.insert 0 a args,
            rtp
        )
    | _ ->
        Pi (0, [|a|], r)




let pi
        (args: (Info.Bind.t * t * t) array)
        (((nres, _), _) as res: t * t)
    : t
    =
    let n =
        Array.fold_left
            (fun n (_, (narg, _), _) -> min narg n)
            nres
            args
    in
    n, Pi (0, args, res)


(*  Note [Pi Lifted]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Examples:

        all (x: (0, A)): (0, R)         -> 0, Pi ...

        all (x: (0, A)): (_, R)         -> 0, Pi ...

        all (x: (_, A)): (0, R)         -> 0, Pi ...


        all (x: (n, A)): (m, R)         -> min n m, Pi ...
*)
