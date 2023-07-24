module Make (M: Fmlib_std.Interfaces.MONAD) =
struct
    open M

    let mapi (f: int -> 'a -> 'b M.t) (arr: 'a array): 'b array t =
        let rec go i lst =
            if i = 0 then
                return lst
            else
                let i = i - 1 in
                let* b = f i arr.(i) in
                go i (b :: lst)
        in
        let* lst = go (Array.length arr) [] in
        return (Array.of_list lst)
end
