open Std
open Printf

type range = Fmlib_parse.Position.range



module Pretty =
struct
    include Fmlib_pretty.Print
    include Pretty_plus
end



type kind =
    | Elab
    | Infer_type of string


type t = {
    range: range;
    reason: string; (* used as information for the tracer *)
    kind: kind;
    args: (bool * int) array;
    res_tp: Gamma.term;
}

let make_e_type (range: range) (reason: string) (gamma: Gamma.t): t =
    (* Create a type hole for an elaborated type (i.e. present in the source
       code). *)
    {
        range;
        reason;
        kind = Elab;
        args = [||];
        res_tp = Gamma.top 0 gamma;
    }


let make_e_term (range: range) (reason: string) (res_tp: Gamma.term): t =
    {
        range;
        reason;
        kind = Elab;
        args = [||];
        res_tp
    }


let make_c_variable_type range reason gamma =
    { range;
      reason;
      kind = Infer_type "variable.";
      args = [||];
      res_tp = Gamma.top 0 gamma;
    }


let make_c_expression_type range reason gamma =
    { range;
      reason;
      kind = Infer_type "expression.";
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



let args (h: t): (bool * int) array =
    h.args



let signature (h: t): (bool * int) array * Gamma.term =
    h.args, h.res_tp



let count_implicits (start: int) (h: t): int =
    let len = count_args h in
    assert (start <= len);
    let rec cnt n i =
        if i = len then
            n
        else
            let im, _ = h.args.(i) in
            if im then
                cnt (n + 1) (i + 1)
            else
                n
    in
    cnt 0 start



let is_unifiable (h: t): bool =
    match h.kind with
    | Elab          -> false
    | _             -> true



let get_range (_: t): range =
    (* Source location associated with the hole. *)
    assert false




let type_of (h: t): Gamma.term =
    h.res_tp



let trace_doc (h: t): Pretty.doc =
    let open Pretty in
    text
        (sprintf "%s for %s"
             (if is_unifiable h then "c-hole" else "e-hole")
             h.reason)
    <+> char ':'
    <+> space
    <+> (Gamma.doc_of_term h.res_tp |> nest 2)
    |> group




let error (id: int) (h: t): Error.t =
    let open Pretty
    in
    match h.kind with
    | Infer_type str ->
        Error.make
            h.range
            "cannot infer type"
            (
                wrap_words "I cannot infer the type of the"
                <+> group space
                <+> text str
                <+> group space
                <+> text (sprintf "(Hole ?%d)" id)
                <+> cut
            )

    | Elab ->
        assert false (* Illegal call *)
