open Intf


type globals =
    Globals.t

type gamma    = Gamma.t
type req
type term     = Gamma.term


let make_globals (): globals =
    Globals.make ()


module Make (Mon: MONAD) =
struct
end
