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
    args: (Gamma.term * bool) array;
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
    let arg (t,impl) =
        let inner =
            Gamma.doc_of_term t
            <+> char ':' <+> space
            <+> Gamma.(doc_of_term (type_of_term t))
        in
        if impl then
            space <+> char '{' <+> inner <+> char '}'
        else
            space <+> char '(' <+> inner <+> char ')'
    in
    text
        (sprintf "%s for %s"
             (if is_unifiable h then "c-hole" else "e-hole")
             h.reason)
    <+> cat (List.map arg (Array.to_list h.args))
    <+> char ':'
    <+> space
    <+> (Gamma.doc_of_term h.res_tp |> nest 2)
    |> group
