type assoc =
    | Left
    | Right
    | No

type t = int * assoc


let leaning ((p1, a1): t) ((p2, _): t): assoc =
    if p1 > p2 then
        Left
    else if p1 = p2 then
        a1
    else
        Right


let make_left (prec: int): t =
    prec, Left

let make_right (prec: int): t =
    prec, Right

let make_no (prec: int): t =
    prec, No

let where: t =
    make_no 0

let comma: t =
    make_right 10

let assign: t =
    make_right 20

let colon: t =
    make_right 30

let arrow: t =
    make_right 40

let or_: t =
    make_right 50

let and_: t =
    make_right 60

let not_: t =
    make_no 70

let apply_left: t =
    make_left 80

let apply_right: t =
    make_right 80

let compose_left: t =
    make_left 90

let compose_right: t =
    make_right 90

let relation: t =
    make_no 100

let add_left: t =
    make_left 110

let add_right: t =
    make_right 110

let mult: t =
    make_left 120

let exp: t =
    make_right 130

let highest_op_left: t =
    make_left 140

let highest_op_right: t =
    make_right 140

let application: t =
    make_left 150


module Map =  Fmlib_std.Btree.Map (String)

let map: 't Map.t =
    let open Map in
    empty
    |> add ","   comma

    |> add "->"  arrow
    |> add "=>"  arrow

    |> add {|\/|} or_
    |> add "|"    or_

    |> add "&"    and_
    |> add {|/\|} and_

    |> add "|>"  apply_left
    |> add "<|"  apply_right

    |> add ">>"  compose_left
    |> add "<<"  compose_right

    |> add "="   relation
    |> add "~"   relation
    |> add "<"   relation
    |> add ">"   relation
    |> add "<="  relation
    |> add ">="  relation
    |> add "=?"  relation
    |> add "<?"  relation
    |> add ">?"  relation
    |> add "~?"  relation
    |> add "<=?" relation
    |> add ">=?" relation
    |> add "/="  relation
    |> add "/~"  relation
    |> add "::"  add_right
    |> add "+:"  add_right
    |> add "+"   add_left
    |> add "-"   add_left
    |> add "*"   mult
    |> add "/"   mult
    |> add "^"   exp


let of_string (str: string): t =
    match Map.find_opt str map with
    | None ->
        highest_op_left
    | Some prec ->
        prec
