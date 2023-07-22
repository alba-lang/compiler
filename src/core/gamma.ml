open Fmlib_std
open Std



type term2 = {
    tid:  int;
    tgid: int;
    term: Term.t;
    req:  req;
}
and req = {
    rid:  int;
    rgid: int;
    rtyp: term2 option;
    sign: Sign.t;
}


type term    = int  *  Term.t  *  Term.tp
type fterm   = int  *  Term.t  *  Term.tp  *  Term.tp
type equiv   = int  *  Term.t  *  Term.t
type subtype = int  *  Term.tp *  Term.tp

type fterm_error =
    [`Meta_needed of int * int * (term -> (fterm, fterm_error) result) |`Fatal]

type t = {
    id: int;
    previous: t option;
    globals: Globals.t;
    content: Term.gen_binder Rb_array.t;
    map: int Name_map.t;
}




let index (g: t ): int =
    g.id




let length (g: t): int =
    Rb_array.length g.content




let globals (g: t): Globals.t =
    g.globals



let empty (id: int) (globals: Globals.t) : t =
    {
        id;
        previous = None;
        globals;
        content  = Rb_array.empty;
        map      = Name_map.empty;
    }





let push_var (new_id: int) (info: Info.Bind.t) ((id, t, _): term) (g: t): t =
    assert (id = g.id);
    { g with
      id       = new_id;
      previous = Some g;
      content  = Rb_array.push (info, t, None) g.content;
      map      = Name_map.add (Info.Bind.name info) (length g) g.map;
    }


let make_fterm (_: term) (_: t): (fterm, fterm_error) result =
    assert false

(* Terms in the context *)

let any (u: int) (g: t): term =
    Term.(g.id, Any u, Any (u + 1))
