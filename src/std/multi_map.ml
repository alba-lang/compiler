open Fmlib_std



module Make (Key: Interfaces.SORTABLE) =
struct
    module Map = Btree.Map (Key)


    type 'a t = 'a list Map.t

    let empty = Map.empty

    let add (k: Key.t) (a: 'a) (m: 'a t): 'a t =
        Map.update
            k
            (function
                | None   -> Some [a]
                | Some l -> Some (a :: l))
            m


    let find (k: Key.t) (m: 'a t): 'a list =
        match Map.find_opt k m with
        | None   -> []
        | Some l -> l
end
