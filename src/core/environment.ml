module Module =
struct
    type t = unit

    let empty: t =
        ()
end

type t = {
    modules: Module.t array;
}


let init (_: string) (_: string): t =
    {modules = [| Module.empty |]}
