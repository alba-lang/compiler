open Std
open Fmlib_parse


module type ANY = Fmlib_std.Interfaces.ANY


type 'a located = Position.range * 'a



module Char = struct
    include Char
    let is_lower c =
        'a' <= c && c <= 'z'
    let is_upper c =
        'A' <= c && c <= 'Z'
    let is_digit c =
        '0' <= c && c <= '9'
    let is_letter c =
        is_upper c || is_lower c
    let is_alpha_num c =
        is_upper c || is_lower c || is_digit c
    let is_alpha_num_ c =
        is_alpha_num c || c = '_'
end






module CP   = Character.Make (Elab.State) (Elab.Final) (Error)

include CP

type term = Elab.Ast.term


let whitespace: int t =
    skip_zero_or_more (char ' ' </> char '\n')


let ws_after (p: 'a t): 'a t =
    let* a = p in
    let* _ = whitespace in
    return a


let identifier: string t =
    word Char.is_letter Char.is_alpha_num_ "identifier"


let digits: string t =
    word Char.is_digit Char.is_digit "digits"




let tagged (p: string located -> 'a t): 'a t =
    let* _ = char '(' |> ws_after in
    let* tag = located identifier |> ws_after in
    let* a   = p tag |> ws_after in
    let* _ = char ')'in
    return a




let name: term t =
    let* _ = char '%' in
    let* _ = identifier in
    assert false


let int_level: term t =
    let* _ = digits in
    assert false


let atomic_term: term t =
    name
    </>
    int_level
    </>
    let* (range, s) = located identifier in 
    match s with
    | "Prop" ->
        Elab.prop range |> return
    | _ ->
        assert false (* Error *)





(*
    ----------------------------------------------------------------------
        Terms
    ----------------------------------------------------------------------
*)


let rec term (): term t =
    atomic_term
    </>
    tagged compound_term


and compound_term ((_, tag): string located): term t =
    match tag with
    | "var" ->
        (* local variable *)
        let* _ = digits in
        assert false

    | "Any" ->
        any ()

    | "app" ->
        assert false

    | "arr" ->
        assert false

    | "pi" ->
        assert false

    | _ ->
        assert false (* Error case *)


and any (): term t =
    let* range, level = located digits in
    Elab.any range (int_of_string level) |> return




(*
    Finalize
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)


let final_term: Elab.Final.t t =
    let* t = term () in
    let* state = get in
    let  res, state =  Elab.make_term t state in
    let* _  = set state in
    match res with
    | Ok final ->
        return final
    | Error error ->
        fail error



let term_parser (state: Elab.State.t): Parser.t =
    make state final_term









(* Examples

    type Nat: Any :=
        zero: _
        succ: _ -> _


    type Decision (P: Prop): Any :=
        true:  P -> _
        false: P -> _


    Decider {A: Any} (P: A -> Prop): Any :=
        all x: Decision (P x)


    (ty
        %Nat
            []: [] (Any 0)
        :=
            [%true: _, %succ: (arr [_] _)]
    )

    (ty
        %Decision
            [%P Prop]: [] (Any 0)
        :=
            [%true: (arr [%P] _),
             %false: (arr [%P] _)]
    )

    (def
        %Decider
        [ {%A: (Any 0)}, (%P: (arr %A Prop))] : (Any 0)
        :=
        (pi [%x: _] : (app %Decision (%P %x))
    )


*)
