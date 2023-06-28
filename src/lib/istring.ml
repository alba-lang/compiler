type t = {
    string: string;
    idx:    int;
}

type istring = t

let value (s: t): string =
    s.string

let compare (a: t) (b: t): int =
    compare a.idx b.idx


module Map =
struct
    open Fmlib_std.Btree

    module M = Map (String)

    type t = {
        map: int M.t;
        cnt: int;
    }

    let empty = {
        map = M.empty;
        cnt = 0;
    }

    let get (string: string) (m: t): istring * t =
        let idx = ref m.cnt in
        let map =
            M.update
                string
                (function
                    | None ->
                        Some !idx
                    | Some i as o ->
                        idx := i;
                        o)
                m.map
        in
        { string; idx = !idx},
        { map;
          cnt =
              if !idx = m.cnt then
                  m.cnt
              else
                  m.cnt + 1
        }
end
