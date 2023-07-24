open Fmlib_std


include Btree.Map (Name)


let find_opt (k: key) (m: 'a t): 'a option =
    if Name.is_wildcard k then
        None
    else
        find_opt k m



let add (k: key) (v: 'a) (m: 'a t): 'a t =
    if Name.is_wildcard k then
        m
    else
        add k v m


let update (k: key) (f: 'a option -> 'a option) (m: 'a t): 'a t =
    if Name.is_wildcard k then
        m
    else
        update k f m


let remove (k: key) (m: 'a t): 'a t =
    if Name.is_wildcard k then
        m
    else
        remove k m



module Multi =
struct
    module Map = Multi_map.Make (Name)

    include Map

    let add (n: Name.t) (a: 'a) (m: 'a t): 'a t =
        if Name.is_wildcard n then
            m
        else
            add n a m


    let find (n: Name.t) (m: 'a t): 'a list =
        if Name.is_wildcard n then
            []
        else
            find n m
end
