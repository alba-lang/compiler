open Core


module type ANY = Fmlib_std.Interfaces.ANY


module Position = Fmlib_parse.Position


module WTB = Welltyped.Basic






module Make (Final: ANY) =
struct
    module Basic = Basic_monad.Make (Final)


    module WTM = Welltyped.Make (Basic)


    include Basic
end
