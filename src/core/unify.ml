open Std
open Base_elab


module Pretty =
struct
    include Fmlib_pretty.Print
    include Pretty_plus
end


type range = Fmlib_parse.Position.range


module Make (Final: ANY) =
struct
    open Base_elab.Make (Final)

    type quad = {
        act: term;
        req: term;
        acthn: term;
        reqhn: term;
    }

    type data = {
        range: range;
        quad:  quad;
        stack: quad list;
    }




    let make_quad (act: term) (req: term): quad t =
        let* acthn = head_normal act in
        let* reqhn = head_normal req in
        { act; req; acthn; reqhn }
        |> return



    let make_data (range: range) (act: term) (req: term): data t =
        let* quad = make_quad act req
        in
        { range; quad; stack = [] }
        |> return


    let make_next_data (act: term) (req: term) data : data t =
        let* quad = make_quad act req in
        { data with quad; stack = quad :: data.stack }
        |> return


    let make_next_data_default data: data t =
        make_next_data
            data.quad.acthn
            data.quad.reqhn
            data



    let rigid_rigid_failure data =
        let open Pretty in
        let* _ =
            trace_doc (text "rigid-rigid unification failure")
        in
        let* act = zonk_avail data.quad.act in
        let* req = zonk_avail data.quad.req in
        Error.make
            data.range
            "invalid type"
            (text "This term has type"
             <+> cut <+> cut
             <+> indent (doc_of_term act)
             <+> cut <+> cut
             <+> text "but it should have type"
             <+> cut <+> cut
             <+> indent (doc_of_term req)
             <+> cut <+> cut
            )
        |> fail



    let rec uni data: unit t =
        let quad =
            data.quad
        in
        let* _ =
            trace (fun () ->
                let open Pretty in
                [
                    (text "Unify", doc_of_term quad.act)
                    ;
                    (text "with", doc_of_term quad.req)
                    ;
                    (text "hn", doc_of_term quad.acthn)
                    ;
                    (text "with", doc_of_term quad.reqhn)
                ]
                |> descriptions
            )
        in
        match
            term_of_term quad.acthn |> snd,
            term_of_term quad.reqhn |> snd
        with

        | Sort s_act, Sort s_req when Sort.unify s_act s_req ->

            return ()

        | Meta id1, Meta id2 ->

            flex_flex id1 id2 data

        | Meta id, _ ->

            flex_rigid
                id
                quad.acthn
                quad.reqhn
                data

        | _,      Meta id ->

            flex_rigid
                id
                quad.reqhn
                quad.acthn
                data

        | Var _, Var _
            when
                (term_of_term quad.acthn |> fst)
                =
                (term_of_term quad.reqhn |> fst)
            ->
            return ()

        | App _, App _ ->

            assert false (* nyi *)

        | Lam _, Lam _ ->

            assert false (* nyi *)

        | Pi _,  Pi _ ->

            assert false

        | Ann (_, _, _), _ ->

            assert false

        | _, Ann (_, _, _) ->

            assert false


        (* rigid-rigid failures *)

        | Var _,     _
        | App _,     _
        | Lam _,     _
        | Sort _,    _
        | Pi   _,    _
            ->

            rigid_rigid_failure data



    and flex_rigid
            (meta_id: int)
            (flex: term)
            (rigid: term)
            data
        : unit t
        =
        let* hole = get_hole meta_id
        in
        if Hole.is_unifiable hole then
            let* _ =
                (* Make sure that the type of the rigid term
                   satisfies the requirement of the
                   flex term. *)
                let* data_next =
                    make_next_data
                        (type_of_term rigid)
                        (type_of_term flex)
                        data
                in
                uni_wait data_next
            in
            let* _ = fill_flex_with_rigid meta_id rigid in
            let* data_next =
                     make_next_data_default data
            in
            uni_wait data_next
        else
            let* _ = wait_hole meta_id in
            let* data_next =
                     make_next_data_default data
            in
            uni_wait data_next



    and fill_flex_with_rigid (id: int) (t: term): unit t =
        match term_of_term t |> snd with

        | Sort _ ->

            fill_hole id t

        | Meta _ ->

            assert false (* Illegal call *)

        | Var _ ->

            assert false (* nyi *)

        | App _ ->

            assert false (* nyi *)

        | Lam _ ->

            assert false (* nyi *)

        | Pi _ ->

            assert false (* nyi *)

        | Ann _ ->

            assert false (* Cannot happen, [t] must be in head normal form and
                            an annotated term is not in head normal form. *)


    and flex_flex id1 id2 data: unit t =
        let cb _ =
            let* data_next = make_next_data_default data in
            uni_wait data_next
        in
        let cb1 = id1, cb
        and cb2 = id2, cb
        in
        wait_one_of_holes cb1 [cb2]


    and uni_wait (data: data): unit t =
        (* Spawn a task to make the unification with [data] and wait for the end
           of the task. *)
        (* MISSING: It has to be a task list which can be run in parallel. *)
        let* id = spawn (uni data) in
        wait_tasks [id]





    let two (range: range) (act: term) (req: term): unit t =
        (* Unify [act] with [req] and return the unified normal form of [act].
         *)
        let* data = make_data range act req in
        uni_wait data


    let into_hole (range: range) (act: term) (id: int): unit t =
        let* h = get_hole id in
        two range act (Hole.type_of h)
end
