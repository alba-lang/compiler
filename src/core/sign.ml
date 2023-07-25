open Std

type 'a decision =
    | Unique of 'a
    | Need   of (Name.t * int * int) list
    | Reject

type t =
    | Sort
    | Global of Name.t * int * int
    | Unknown
    | Meta of Name.t * int * int
    | App of bool * t * t array         (* operator flag *)
    | Fun of (bool * t) array * t       (* implicit flag *)



let sort: t =
    Sort


let rec equivalent (a: t) (b: t): bool =
    match a, b with
    | Meta _, _ | _, Meta _ ->
        assert false (* Illegal call! *)

    | Unknown, _ | _, Unknown ->
        true

    | Sort, Sort ->
        true

    | Global (_, m1, i1), Global (_, m2, i2) ->
        m1 = m2 && i1 = i2

    | App (_, h1, args1), App (_, h2, args2) ->
        let len1 = Array.length args1
        and len2 = Array.length args2
        in
        equivalent h1 h2
        &&
        len1 = len2
        &&
        Intm.for_all
            (fun i -> equivalent args1.(i) args2.(i))
            len1

    | Fun (args1, r1), Fun (args2, r2) ->
        let len1 = Array.length args1
        and len2 = Array.length args2
        in
        let rec go i j =
            if i = len1 && j = len2 then
                equivalent r1 r2
            else
                let (imp1, arg1) = args1.(i)
                and (imp2, arg2) = args2.(j)
                in
                if imp1 then
                    go (i + 1) j
                else if imp2 then
                    go i (j + 1)
                else
                    equivalent arg1 arg2
                    &&
                    go (i + 1) (j + 1)
        in
        go 0 0

    | _, _ ->
        false


let decide (_: t) (_: ('a * t) list): 'a decision =
    assert false
