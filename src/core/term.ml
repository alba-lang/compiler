type t =
    int * t0

and t0 =
   | Sort of Sort.t
   | Meta of int
   | Pi of
         int                            (* Number of actual fargs, only the tail
                                           of fargs is actually used *)
         * (Info.Bind.t * t * t) array  (* (bind, ty, s): s is always a sort *)

         * (t * t)                      (* Result type and its sort *)



let is_up_by (j, _) = j



let up (i: int) ((j,t): t): t =
    i + j, t



let sort (s: Sort.t): t =
    0, Sort s


let prop  : t = sort Prop
let any i : t = sort (Any i)
let top i : t = sort (Top i)

let meta i: t = 0, Meta i



let pi_sort (t1: t) (t2: t): t =
    match t1, t2 with
    | (i, Sort s1), (_, Sort s2) ->
        i, (Sort (Sort.pi s1 s2))

    | _, _ ->
        assert false (* illegal call, polymorphic sorts nyi *)



let pi
        (args: (Info.Bind.t * t * t) array)
        (((nres, _), _) as res: t * t)
    : t
    =
    let nargs  = Array.length args in
    let n =
        Array.fold_left
            (fun n (_, (narg, _), _) -> min narg n)
            nres
            args
    in
    n, Pi (nargs, args, res)


(*  Note [Pi Lifted]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Examples:

        all (x: (0, A)): (0, R)         -> 0, Pi ...

        all (x: (0, A)): (_, R)         -> 0, Pi ...

        all (x: (_, A)): (0, R)         -> 0, Pi ...


        all (x: (n, A)): (m, R)         -> min n m, Pi ...
*)
