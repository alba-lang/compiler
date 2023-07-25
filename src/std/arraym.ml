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


    let foldi_left
            (f: 'accu -> int -> 'el -> 'accu t)
            (start: 'accu)
            (arr: 'el array)
        : 'accu t
        =
        let len = Array.length arr
        in
        let rec go i accu =
            if i = len then
                return accu
            else
                let* accu = f accu i arr.(i) in
                go (i + 1) accu
        in
        go 0 start



    let fold_left (f: 'accu -> 'el -> 'accu t) (start: 'accu) (arr: 'el array)
        : 'accu t
        =
        foldi_left
            (fun accu _ el -> f accu el)
            start
            arr
end
