type t =
    int * t0

and t0 =
   | Sort of Sort.t
   | Meta of int


let up_by (j, _) = j

let up (i: int) ((j,t): t): t =
    i + j, t



let sort (s: Sort.t): t =
    0, Sort s


let prop  : t = sort Prop
let any i : t = sort (Any i)
let top i : t = sort (Top i)

let meta i: t = 0, Meta i
