open Alba_parse

module Position = Fmlib_parse.Position
module Pretty = Fmlib_pretty.Print

type range = Position.range
type 'a located = range * 'a



module Semantic =
struct
    type t
end



module Elaboration_context =
struct
    type t = unit
    let init (_: string) (_: string): t =
        ()
end



(* Specification of a hole *)
module Spec =
struct
    type t
end



(* Content of a hole *)
module Content =
struct
    type t

    let make (_: Term.t) (_: Term.t): t =
        assert false
end




module Scheduler =
    Build_scheduler.Make (Elaboration_context) (Spec) (Content) (Semantic)


type hole_id = Scheduler.hole_id

type action = unit Scheduler.t


type t = Elaboration_context.t

type term = hole_id -> action

type universe_term

type formal_argument

type error = Semantic.t




(* Check Specification
   ================================================================================
 *)


let check (_: Content.t) (_: hole_id): action =
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





let prop (_: range): term =
    fun hole ->
    (*
        The term is [Prop: Any 0].
        The following tasks have to be executed in sequence:
        - Unify [Any 0] with the requirement
        - Put [Prop: Any 0] into [hole]

     *)
    let t = Content.make Term.prop Term.any0
    in
    Scheduler.(
        let* id = check t hole     |> make_task ReadyQ in
        let* _  = fill_hole hole t |> make_task (TaskQ id) in
        return ()
    )






let any (_: range) (_: universe_term option): term =
    fun hole ->
    let t = Content.make Term.any0 Term.any1
    in
    Scheduler.(
        let* id = check t hole     |> make_task ReadyQ in
        let* _  = fill_hole hole t |> make_task (TaskQ id) in
        return ()
    )





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


let application (_: term) (_: term list): term =
    assert false


let parens_term (_: Position.t) (_: Position.t) (_: term): term =
    assert false


let implicit_argument (_: Position.t) (_: Position.t) (_: term): term =
    assert false


let unary_expression
        (_: range) (_: string) (_: Precedence.t) (_: term)
    : term
    =
    assert false


let binary_expression
        (_: term) (_: range) (_: string) (_: Precedence.t) (_: term)
    : term
    =
    assert false


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





let init (pname: string) (mname: string): t =
    Elaboration_context.init pname mname
