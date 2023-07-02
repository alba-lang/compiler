open Std
open Core

module Position = Fmlib_parse.Position
module Pretty = Fmlib_pretty.Print

type range = Position.range
type 'a located = range * 'a






module BS = Build_scheduler


type hole_id = BS.hole_id

type action = unit BS.t


type t = unit

type term = {
    rangef: unit -> range;
    build:  hole_id -> action;
    is_implicit: bool;
}



let make_term rangef build: term = {
    rangef; build; is_implicit = false
}




type universe_term

type formal_argument

type error = Error.t





(* Helper Functions
   ================================================================================
 *)


let list_last (lst: 'a list): 'a =
    assert (lst <> []);
    let rec last = function
        | [] ->
            assert false
        | [a] ->
            a
        | _ :: tail ->
            last tail
    in
    last lst





let make_application_holes
        (_: term)
        (_: term array)
    : (hole_id * hole_id array) BS.t
    =
    assert false



let build_task
        (queue: BS.queue)
        (hole: hole_id)
        (f: hole_id -> action)
    : BS.task_id BS.t
    =
    BS.make_task queue (f hole)



let build_task_wo_id
        (queue: BS.queue)
        (hole: hole_id)
        (f: hole_id -> action)
    : unit BS.t
    =
    BS.(map (fun _ -> ()) (make_task queue (f hole)))



let build_application
        (_: hole_id)            (* f *)
        (_: hole_id array)      (* a1 .. an *)
        (_: hole_id)            (* f a1 .. an *)
    : unit BS.t
    =
    (* Hole for [f] must be filled *)
    assert false




(* Check Specification
   ================================================================================
 *)


let check (_: Hole_content.t) (_: hole_id): action =
    (* Verify that the content satisfies the specification of the hole. *)
    assert false





(* Functions to satisfy the signature ELABORATOR
   ================================================================================
 *)

let range_of_semantic (_: error): Position.range =
    assert false

let doc_of_semantic (_: error): Pretty.doc =
    assert false


module Universe = struct
    let id    _ _: universe_term = assert false
    let fixed _ _: universe_term = assert false
    let max _ _: universe_term = assert false
    let above _ _: universe_term = assert false
    let parens_around _ _ _ = assert false
end





let level (_: range): term =
    assert false





let prop (range: range): term =
    (*
        The term is [Prop: Any 0].
        The following tasks have to be executed in sequence:
        - Unify [Any 0] with the requirement
        - Put [Prop: Any 0] into [hole]

     *)
    let rangef () = range
    in
    let t = Hole_content.make rangef Term.prop Term.any0
    in
    let build hole =
        BS.(
            let* id = check t hole     |> make_task ReadyQ in
            let* _  = fill_hole hole t |> make_task (TaskQ id) in
            return ()
        )
    in
    make_term rangef build






let any (range: range) (_: universe_term option): term =
    let rangef () = range
    in
    let t = Hole_content.make rangef Term.any0 Term.any1
    in
    let build hole =
        BS.(
            let* id = check t hole     |> make_task ReadyQ in
            let* _  = fill_hole hole t |> make_task (TaskQ id) in
            return ()
        )
    in
    make_term rangef build





let name_term (_: range) (_: Name.t): term =
    assert false


let string_term (_: range) (_: string): term =
    assert false


let char_term (_: range) (_: string): term =
    assert false



let decimal_term (_: range) (_: string): term =
    assert false



let float_term (_: range) (_: string): term =
    assert false



let tuple_term (_: range) (_: term list): term =
    assert false



let list_term (_: range) (_: term list): term =
    assert false



let application (fterm: term) (args: term list): term =
    (* Note [Function Application] *)
    assert (args <> []);
    let rangef () =
        fterm.rangef () |> fst,
        (list_last args).rangef () |> snd
    in
    let build hole =
        let args  = Array.of_list args in
        BS.(
            let* fhole, argholes =
                (* Make holes *)
                make_application_holes fterm args
            in
            let* _ =
                (* Put argument elaborators into the ready queue *)
                int_fold_right
                    (Array.length args)
                    (fun i () ->
                         build_task_wo_id
                             ReadyQ
                             argholes.(i)
                             args.(i).build)
                    (return ())
            in
            let* fid =
                (* Put function term elaborator into the ready queue *)
                build_task ReadyQ fhole fterm.build
            in
            (* Elaborate the hole application after the elaboration of the
             * function term.
             *)
            build_task_wo_id (TaskQ fid) hole (build_application fhole argholes)
        )
    in
    make_term rangef build




let parens_term (_: Position.t) (_: Position.t) (term: term): term =
    term



let implicit_argument (_: Position.t) (_: Position.t) (term: term): term =
    { term with is_implicit = true }



let unary_expression
        ((pos1, _): range) (_: string) (_: Precedence.t) (operand: term)
    : term
    =
    let rangef () = pos1, operand.rangef () |> snd
    in
    let build _ =
        assert false
    in
    make_term rangef build




let binary_expression
        (leftop: term) (_: range) (_: string) (_: Precedence.t) (rightop: term)
    : term
    =
    let rangef () = leftop.rangef () |> fst, rightop.rangef () |> snd
    in
    let build _ =
        assert false
    in
    make_term rangef build




let formal_argument_simple (_: range) (_: Name.t): formal_argument =
    assert false



let formal_argument
        (_: bool)                       (* implicit? *)
        (_: Name.t located list)        (* nonempty group of variables *)
        (_: term option)                (* type of the group *)
    : formal_argument
    =
    assert false




let product_expression
        (_: Position.t)                 (* start of 'all' *)
        (_: formal_argument list)
        (_: term)                       (* result type *)
    : term
    =
    assert false




let lambda_expression
        (_: Position.t)                 (* start of lambda *)
        (_: formal_argument list)
        (_: term option)                (* result type *)
        (_: term)                       (* body *)
    : term
    =
    assert false




let add_definition
        (_: Name.t located)
        (_: formal_argument list)       (* possibly empty *)
        (_: term)                       (* result type    *)
        (_: term option)                (* body           *)
        (_: t)
    : (t, error) result
    =
    assert false





let init (_: string) (_: string): t =
    ()






(* Note [Function Application]

   The goal is to elaborate the function applicaition

       f a1 a2 ... an

   into a hole with some requirement [R].

   We need one hole for the function term [f] and two holes [ai] [Ai] for each
   argument (one for the argument term and one for its type)

        a1: A1: Sort
        ...
        an: An: Sort

        f: function with n arguments (some of them potentially implicit) with
           the corresponding types Ai + the arguments in R.

   One task for the elaboration of the function term and one task the
   elaboration of each argument term. All tasks are put into the ready queue.

   The elaboration task Ef for f puts f into the hole, if the signature
   requirements are satisfied. This is not yet unification.

   One task for the elaboration of f a1 ... an: Starts after f has been
   elaborated. It unifies the argument types of f with all Ai and the result
   type of f with R and then puts f a1 ... an into the hole.

*)
