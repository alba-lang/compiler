module type ANY = Fmlib_std.Interfaces.ANY


module Make (Meta: ANY) (Value: ANY) (Final: ANY):
sig
    type 'a t

    val return:    'a -> 'a t
    val (>>=):     'a t -> ('a -> 'b t) -> 'b t
    val ( let* ):  'a t -> ('a -> 'b t) -> 'b t
    val map:       ('a -> 'b) -> 'a t -> 'b t

    val create:   Meta.t -> int t
    val get:      int -> Meta.t t
    val wait:     int -> Value.t t
    val put:      int -> Meta.t -> unit t
    val resolve:  int -> Value.t -> unit t

    val spawn: unit t -> unit t

    val terminate: Final.t -> 'a t

    val run: Final.t t -> (int -> (int -> Meta.t) -> Final.t) -> Final.t
end
