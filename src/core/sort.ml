type t =
    | Prop
    | Any of int
    | Level
    | Top of int



let type_of: t -> t = function
    | Prop   -> Any 0
    | Any i  -> Any (i + 1)
    | Level  -> Top 0
    | Top i  -> Top (i + 1)


let pi (s1: t) (s2: t): t =
    match s1, s2 with
    | Any _, Prop ->
        Prop

    | Any i, Any j ->
        Any (max i j)

    | _, _ ->
        assert false (* illegal argument *)



let unify (eq: bool) (s_act: t) (s_req: t): bool =
    if eq then
        s_act = s_req
    else
        match
            s_act, s_req
        with
        | Prop, Prop ->
            true

        | Prop, Any _ ->
            true

        | Prop, Top _ ->
            true

        | Any i, Any j ->
            i <= j

        | Any _, Top _ ->
            true

        | Level, Top _ ->
            true

        | Top i, Top j ->
            i <= j

        | _, _ ->
            false
