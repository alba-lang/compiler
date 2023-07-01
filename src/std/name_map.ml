open Fmlib_std


include Btree.Map (Name)


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
