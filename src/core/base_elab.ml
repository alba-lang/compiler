open Std
open Printf

module type ANY = Fmlib_std.Interfaces.ANY


module Pretty =
struct
    include Fmlib_pretty.Print
    include Pretty_plus
end


type range =
    Fmlib_parse.Position.range

type 'a located =
    range * 'a

type doc = Fmlib_pretty.Print.doc


type term    = Gamma.term
type gamma   = Gamma.t
type globals = Globals.t




let gamma_of_term (t: term): gamma =
    Gamma.gamma_of_term t


let term_of_term (t: term): Term.t =
    Gamma.term_of_term t


let type_of_term (t: term): term =
    Gamma.type_of_term t


let make_globals () =
    Globals.make ()


let make_gamma (g: globals): gamma =
    Gamma.empty g


let doc_of_term (t: term): Pretty.doc =
    Gamma.(doc_of_term t)




module State =
struct
    type tracing =
        | Never
        | Always
        | On_error

    type t = {
        gamma: Gamma.t;
        tracing: tracing;
        tracer: Tracer.t;
    }


    let tracer t = t.tracer

    let gamma t = t.gamma


    let make (gamma: gamma): t =
        {
            gamma;
            tracing = On_error;
            tracer  = Tracer.ignore;
        }

    let make_tracing (gamma: gamma): t =
        {
            gamma;
            tracing = Always;
            tracer  = Tracer.empty
        }

    let make_not_tracing (gamma: gamma): t =
        {
            gamma;
            tracing = Never;
            tracer  = Tracer.ignore
        }
end




module Value =
struct
    type t = term
end




(*
    Error Handler
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)


module Hole_error:
sig
    type hole_data =
        int list * bool * Hole.t * term option

    val handler: int -> (int -> hole_data) -> Error.t
end
=
struct
    type hole_data =
        int list * bool * Hole.t * term option

    type hd = int * int list * Hole.t



    let rec compare (task1: int list) (task2: int list): int =
        match task1, task2 with
        | [], [] ->
            0

        | _ :: _, [] ->
            +1

        | [], _ :: _ ->
            -1

        | i1 :: tl1, i2 :: tl2 ->
            if i1 < i2 then
                -1
            else if i1 = i2 then
                compare tl1 tl2
            else
                +1


    let better2
            (task1: int list) (i1: int)
            (task2: int list) (i2: int)
        : bool
        =
        let cmp = compare task1 task2 in
        if cmp < 0 then
            false
        else if cmp = 0 then
            i1 < i2
        else
            true





    let find_next i n f: hd option =
        let rec next i =
            if i = n then
                None
            else
                let task, has_wait, h, v = f i
                in
                if has_wait && Hole.is_unifiable h && v = None
                then
                    Some (i, List.rev task, h)
                else
                    next (i + 1)
        in
        next i


    let find n f: (int * Hole.t) option =
        let rec fnd i (hdopt: hd option): (int * Hole.t) option =
            match hdopt, find_next i n f with
            | None, None ->
                None

            | Some (i1, _, h1), None ->
                Some (i1, h1)

            | None, Some (j, _, _ as hd2) ->
                fnd (j + 1) (Some hd2)

            | Some (_, task1, _), Some (j, task2, _ as hd2) ->
                if better2 task1 i task2 j then
                    fnd (j + 1) (Some hd2)
                else
                    fnd (j + 1) hdopt
        in
        fnd 0 None




    let handler (n: int) (f: int -> hole_data) : Error.t =
        let open Pretty
        in
        match find n f with
        | None ->
            Error.make
                Fmlib_parse.Position.(start, start)
                "internal error"
                (sprintf "No hole of %d found" n |> text)

        | Some (id, h) ->
            match Hole.c_hole_info h with
            | Infer_type (s, range) ->
                Error.make
                    range
                    "cannot infer type"
                    (
                        wrap_words "I cannot infer the type of"
                        <+> group space
                        <+> text s
                        <+> group space
                        <+> text (sprintf "(Hole ?%d)" id)
                        <+> cut
                    )
end









(*
    Basic Elaboration Services
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)




module Make (Final: ANY) =
struct
    include Generic_elaborator.Make (Hole) (Value) (Tracer) (Final) (Error)



    type 'a hole_callback = int * (term -> 'a t)


    let trace_doc (d: Pretty.doc): unit t =
        trace (fun _ -> d)



    let create_hole (h: Hole.t): int t =
        let open Pretty in
        let* id = create_hole h in
        let* _  = trace
            (fun () ->
                description
                    (sprintf "Create ?%d" id |> text)
                    (Hole.doc h)
            )
        in
        return id


    let fill_hole (id: int) (t: term): unit t =
        let open Pretty in
        let* _ =
            trace
                (fun _ ->
                     description
                         (sprintf "Fill ?%d :=" id |> text)
                         (doc_of_term t)
                )
        in
        fill_hole id t



    let got_hole id t (): Pretty.doc =
        let open Pretty in
        description
            (text "Got")
            (description
                 (text (sprintf "?%d =" id))
                 (doc_of_term t))



    let wait_hole (id: int): Value.t t =
        let open Pretty in
        let* _ =
            trace
                (fun _ -> text (sprintf "Wait ?%d" id))
        in
        let* t =
            wait_hole id in
        let* () =
            trace (got_hole id t)
        in
        return t


    let wait_one_of_holes
            (cb:  'a hole_callback)
            (cbs: 'a hole_callback list)
        : 'a t
        =
        let open Pretty in
        let* _ = trace (
            fun _ ->
                sprintf "Wait one of: %s"
                    (String.concat
                         " "
                         (List.rev_map
                              (fun (i, _) -> sprintf "?%d" i)
                              (cb :: cbs)))
                |> text
        ) in
        let callback (i, f): 'a hole_callback =
            i,
            fun t ->
                let* () = trace (got_hole i t) in
                f t
        in
        wait_one_of_holes
            (callback cb)
            (List.rev_map callback cbs)



    let meta (id: int): term t =
        let* h = get_hole id in
        Gamma.meta id (Hole.type_of h) |> return



    let run (m: Final.t t) (state: State.t)
        : (Final.t, Error.t) result * State.t
        =
        let run tracer =
            run Hole_error.handler m tracer
        in
        match
            state.tracing
        with
        | Never ->
            let res, tracer  = run Tracer.ignore in
            res, {state with tracer}

        | Always ->
            let res, tracer = run Tracer.empty in
            res, {state with tracer}

        | On_error ->
            let res, tracer = run Tracer.ignore in
            match res with
            | Ok _  ->
                res, {state with tracer}

            | Error _ ->
                let res, tracer = run Tracer.empty in
                res, {state with tracer}
end
