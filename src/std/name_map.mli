include Fmlib_std.Interfaces.MAP with type key = Name.t


module Multi:
sig
    type 'a t

    val empty: 'a t

    val add: Name.t -> 'a -> 'a t -> 'a t

    val find: Name.t -> 'a t -> 'a list
end
