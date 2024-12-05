open Printf

type range = Fmlib_parse.Position.range


module Pretty = Fmlib_pretty.Print


type c_hole =
    | Infer_type of string * range


type info =
    | Elab
    | Constraint of c_hole


type t = {
    reason: string;
    info: info;
    args: (bool * int) array;
    res_tp: Gamma.term;
}

let make_e_type (reason: string) (gamma: Gamma.t): t =
    (* Create a type hole for an elaborated type (i.e. present in the source
       code). *)
    {
        reason;
        info = Elab;
        args = [||];
        res_tp = Gamma.top 0 gamma;
    }


let make_e_term (reason: string) (res_tp: Gamma.term): t =
    {
        reason;
        info = Elab;
        args = [||];
        res_tp
    }


let make_c_type (reason: string) (c_info: c_hole) gamma =
    {
        reason;
        info = Constraint c_info;
        args = [||];
        res_tp = Gamma.top 0 gamma;
    }


let push_arg (impl: bool) (a: int) (h: t): t =
    {h with args =
                Fmlib_std.Array.insert 0 (impl, a) h.args
    }


let count_args (h: t): int =
    Array.length h.args



let arg (i: int) (h: t): bool * int =
    assert (i < count_args h);
    h.args.(i)



let is_unifiable (h: t): bool =
    match h.info with
    | Elab          -> false
    | Constraint _  -> true



let c_hole_info (h: t): c_hole =
    match h.info with
    | Elab ->
        assert false (* Illegal call *)

    | Constraint c_hole ->
        c_hole


let type_of (h: t): Gamma.term =
    h.res_tp



let doc (h: t): Pretty.doc =
    let open Pretty in
    text
        (sprintf "%s for %s"
             (if is_unifiable h then "c-hole" else "e-hole")
             h.reason)
    <+> char ':'
    <+> space
    <+> (Gamma.doc_of_term h.res_tp |> nest 2)
    |> group
