open Fmlib_std
open Std


type t = {
    id: int;
    previous: t option;
    content: Term.gen_binder Rb_array.t;
    map: int Name_map.t;
}


let count (g: t): int =
    Rb_array.length g.content


let empty ?(id = 0) () : t =
    {
        id;
        previous = None;
        content  = Rb_array.empty;
        map      = Name_map.empty;
    }



let default_id (g: t): int =
    g.id + 1




let push ?(id: int option) ((info, _, _) as v: Term.gen_binder) (g: t): t =
    let id =
        match id with
        | None ->    default_id g
        | Some id -> id
    in
    {
        id;
        previous = Some g;
        content  = Rb_array.push v g.content;
        map      = Name_map.add (Info.Bind.name info) (count g) g.map;
    }

