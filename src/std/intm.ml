let for_all (p: int -> bool) (n: int): bool =
    assert (0 <= n);
    let rec go i =
        if i = n then
            true
        else if p i then
            go (i + 1)
        else
            false
    in
    go 0



module Make (M: Fmlib_std.Interfaces.MONAD) =
struct
    open M

    let fold_up (f: int -> 'a -> 'a M.t) (start: 'a) (n: int): 'a t =
        let rec go i a =
            if i = n then
                return a
            else
                let* a = f i a in
                go (i + 1) a
        in
        go 0 start



    let fold_down (f: int -> 'a -> 'a M.t) (start: 'a) (n: int): 'a t =
        let rec go i a =
            if i = 0 then
                return a
            else
                let i = i - 1 in
                let* a = f i a in
                go i a
        in
        go n start
end
