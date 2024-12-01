open Std
open Base_elab


module Pretty =
struct
    include Fmlib_pretty.Print
    include Pretty_plus
end




module Make (Final: ANY) =
struct
    open Base_elab.Make (Final)

    type quad = {
        hole: int;          (* The hole which has to filled with the unified
                               term [act]. *)
        act: term;
        req: term;
        acthn: term;
        reqhn: term;
    }

    type data = {
        quad:  quad;
        stack: quad list;
    }



    let rec head_normal (t: term): term t =
        match
            term_of_term t |> snd
        with
        | Sort _ ->

            return t

        | Meta id ->

            begin
                let* v = value_opt id in
                match v with
                | None ->
                    return t
                | Some t ->
                    head_normal t
            end

        | Pi _  ->

            return t



    let make_quad (act: term) (req: term): quad t =
        let* acthn = head_normal act in
        let* reqhn = head_normal req in
        let* hole  =
            create_hole
                (Hole.make_e_term
                     "unified actual"
                     (type_of_term act))
        in
        { hole; act; req; acthn; reqhn }
        |> return



    let make_data (act: term) (req: term): data t =
        let* quad = make_quad act req
        in
        { quad; stack = [] }
        |> return


    let make_next_data (act: term) (req: term) data : data t =
        let* quad = make_quad act req in
        { quad; stack = quad :: data.stack }
        |> return


    let make_next_data_default data: data t =
        make_next_data
            data.quad.acthn
            data.quad.reqhn
            data





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
                ]
                |> descriptions
            )
        in

        match
            term_of_term quad.acthn |> snd,
            term_of_term quad.reqhn |> snd
        with

        | Sort s_act, Sort s_req ->

            if Sort.unify s_act s_req
            then
                fill_hole quad.hole quad.acthn
            else
                assert false

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

        | Pi _,  Pi _ ->

            assert false

        | Pi _,  _ ->

            assert false (* nyi: rigid-rigid failure *)

        | _,  Pi _ ->

            assert false (* nyi: rigid-rigid failure *)



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
            uni_wait_fill
                data.quad.hole
                data_next
        else
            let* _ = wait_hole meta_id in
            let* data_next =
                     make_next_data_default data
            in
            uni_wait_fill
                data.quad.hole
                data_next



    and fill_flex_with_rigid (id: int) (t: term): unit t =
        match term_of_term t |> snd with

        | Sort _ ->

            fill_hole id t

        | Meta _ ->

            assert false (* Illegal call *)

        | Pi _ ->

            assert false (* nyi *)


    and flex_flex id1 id2 data: unit t =
        let cb _ =
            let* data_next = make_next_data_default data in
            uni_wait_fill data.quad.hole data_next
        in
        let cb1 = id1, cb
        and cb2 = id2, cb
        in
        wait_one_of_holes cb1 [cb2]



    and uni_wait (data: data): term t =
        (* Spawn a task to make the unification with [data] and wait for unified
           actual term. *)
        let* _ = spawn (uni data) in
        wait_hole data.quad.hole



    and uni_wait_fill (id: int) (data: data): unit t =
        (* Spawn a task to make the unification with [data], wait for unified
           actual term and fill the term into the hole [id]. *)
        let* t = uni_wait data in
        fill_hole id t




    let two (act: term) (req: term): term t =
        (* Unify [act] with [req] and return the unified normal form of [act].
         *)
        let* data = make_data act req in
        uni_wait data


    let into_hole (act: term) (id: int): term t =
        let* h = get_hole id in
        two act (Hole.type_of h)
end
