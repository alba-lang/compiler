let remove_parents (sep: char) (path: string): string =
    (* Remove prefixes of the form "../../../" from the path. *)
    let len = String.length path
    in
    let rec find_start (i: int): int =
        if
            i + 2 <= len
            && path.[i] = '.'
            && path.[i+1] = '.'
        then
            if i + 2 < len && path.[i+2] = sep then
                find_start (i + 3)
            else
                i + 2
        else
            i
    in
    let i = find_start 0 in
    String.sub path i (len - i)


let normalize (sep: char) (path: string) =
    (* remove duplicate separators and normalize "." and "..". *)
    let one_char = String.make 1
    in
    let rec norm rlst = function
        | [] ->
            String.concat
                (one_char sep)
                (List.rev rlst)

        | "" :: tl ->
            (* Empty string means that there is a duplicate separator. *)
            norm rlst tl

        | "." :: tl ->
            norm rlst tl

        | ".." :: tl ->
            let rlst =
                match rlst with
                | [] ->
                    [".."]
                | ".." :: _ ->
                    ".." :: rlst
                | _ :: tl ->
                    tl
            in
            norm rlst tl

        | hd :: tl ->
            norm (hd :: rlst) tl
    in

    (* Treat the corner cases *)
    match String.split_on_char sep path with
    | [] ->
        assert false (* cannot happen *)
    | [""] ->
        "."
    | [str] ->
        str
    | "" :: lst ->
        (* absolute path *)
        one_char sep ^ (norm [] lst |> remove_parents sep)
    | lst ->
        let str = norm [] lst in
        if str = "" then
            "."
        else
            str



let resolve (sep: char) (paths: string list): string =
    let paths = List.map (normalize sep) paths in
    let rec resolve = function
        | [] ->
            "."
        | [last] ->
            normalize sep last
        | hd :: tl ->
            let path = resolve tl in
            if 0 < String.length path && path.[0] = sep then
                path
            else
                normalize sep ((hd ^ String.make 1 sep) ^ path)
    in
    resolve paths



let join (sep: char) (paths: string list): string =
    let concat = String.concat (String.make 1 sep)
    in
    match
        paths
    with
    | hd :: tl when String.length hd = 1 && hd.[0] = sep ->
        (* When head is only the root ["/"], then no separator shall be
           inserted *)
        hd ^ concat tl
    | paths ->
        concat paths



let dirname (sep: char) (path: string): string =
    (* Precondition: [path] normalized. *)
    let len = String.length path in
    if len = 0 then
        "."
    else if len = 1 && path.[0] = sep then
        path
    else
        match
            Stdlib.String.rindex_opt path sep
        with
        | None ->
            "."
        | Some i ->
            if i = 0 then
                String.sub path 0 1
            else
                String.sub path 0 i


let basename (sep: char) (path: string): string =
    (* Precondition: [path] normalized. *)
    let len = String.length path in
    match
        Stdlib.String.rindex_opt path sep
    with
    | None ->
        path
    | Some i ->
        assert (i + 1 <= len);
        String.sub path (i + 1) (len - (i + 1))



(* Unit Tests
 * ==========
 *)


(* Test normalization. *)
let%test _ =
    let tests: (string * string) list =
        [
            "",                     ".";

            "/",                    "/";

            "//////",               "/";

            "..",                   "..";

            "../../",               "../..";

            "/../../",              "/";

            "a/b/c/",               "a/b/c";

            "/a/./b/c",             "/a/b/c";

            "a/b/c/../../..",       ".";

            "a/b/c/../../../..",    "..";

            "a/b/c/../..",          "a";

            "/a/..",                "/";

            "/a/b/c/../../..",      "/";

            "/a/b/c/../../../..",   "/";
        ]
    in
    List.for_all
        (fun (str, expected) ->
             let norm = normalize '/' str in
             if norm <> expected then
                 Printf.printf "path <%s>, norm <%s>, expected <%s>\n"
                     str norm expected;
             norm = expected)
        tests


(* Test resolve *)
let%test _ =
    let tests: (string list * string) list =
        [
            [],                         ".";

            ["a/b/"; "c"],              "a/b/c";

            ["a/"; "/c"],               "/c";

            ["a"; "b"; "c"],            "a/b/c";

            ["."; "."; "."],            ".";

            ["."; ".."; "."],           "..";
        ]
    in
    List.for_all
        (fun (lst, expected) ->
             let res = resolve '/' lst in
             if res <> expected then
                 Printf.printf "list [%s], res <%s>, expected <%s>\n"
                     (String.concat ", "
                          (List.map (fun str -> "<" ^ str ^ ">") lst))
                     res expected;
             res = expected)
        tests


(* Test dirname *)
let%test _ =
    let tests: (string * string * string) list =
        [
            "",                     ".",            "";
            "a",                    ".",            "a";
            "/",                    "/",            "";
            "/a",                   "/",            "a";
            "a/b",                  "a",            "b";
        ]
    in
    List.for_all
        (fun (path, edir, ebase) ->
             let dir = dirname '/' path
             and base = basename '/' path
             in
             if dir <> edir || base <> ebase then
                 Printf.printf
                     "path <%s>, dir (<%s>, <%s>), base (<%s>, <%s>)\n"
                     path dir edir base ebase;
             dir = edir && base = ebase)
        tests
