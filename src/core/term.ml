type t =
   | Sort of Sort.t
   | Meta of int



let prop  : t = Sort Prop
let any i : t = Sort (Any i)
let top i : t = Sort (Top i)
let meta i: t = Meta i


