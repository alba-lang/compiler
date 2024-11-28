open Std
open Fmlib_parse


module type ANY = Fmlib_std.Interfaces.ANY


type range      = Position.range

type 'a located = range * 'a



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




let (|=) (mf: ('a -> 'b) t) (ma: 'a t): 'b t =
    let* f = mf in
    let* a = ma in
    return (f a)



let (|.) (mf: ('a -> 'b) t) (m: _ t): ('a -> 'b) t =
    let* f = mf in
    let* _ = m  in
    return f


let _ = (|=), (|.)




let whitespace: int t =
    skip_zero_or_more (char ' ' </> char '\n')



let ws_after (p: 'a t): 'a t =
    let* a = p in
    let* _ = whitespace in
    return a



let ws_before (p: 'a t): 'a t =
    let* _ = whitespace in
    p



let ws_around (p: 'a t): 'a t =
    ws_before p |> ws_after



let zero_or_more_rev (p: 'a t): (int * 'a list) t =
    let rec scan n lst =
        (
            let* a = ws_before p in
            scan (n + 1) (a :: lst)
        )
        </>
        return (n, lst)
    in
    scan 0 []
let _ = zero_or_more_rev



let one_or_more_rev (p: 'a t): (int * 'a list * 'a) t =
    let rec scan n lst a0 =
        (
            let* a = ws_before p in
            scan (n + 1) (a0 :: lst) a
        )
        </>
        return (n, lst, a0)
    in
    let* a = p in
    scan 0 [] a





let identifier: string t =
    word Char.is_letter Char.is_alpha_num_ "identifier"



let name: Name.t t =
    let* _ = char '%' in
    identifier |> map Name.normal



let digits: string t =
    word Char.is_digit Char.is_digit "digits"



let parens_generic (left: _ t) (p: 'a t) (right: _ t): 'a t =
    let* _ = left in
    let* a = ws_around p in
    let* _ = right in
    return a


let parens (p: 'a t): 'a t =
    parens_generic (char '(') p (char ')')



let tagged (p: string -> 'a t): 'a t =
    parens (ws_after identifier >>= p)



let tagged_term (p: string -> (range -> term) t): term t =
    map (fun (range, f) -> f range) (p |> tagged |> located)



let atomic_term: term t =
    (*name
    </>*)
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
    tagged_term compound_term


and compound_term (tag: string): (range -> term) t =
    match tag with
    | "var" ->
        (* local variable *)
        let* _ = digits in
        assert false

    | "Any" ->
        any ()

    | "ap" ->
        assert false

    | "ar" ->
        arrow ()

    | "pi" ->
        pi ()

    | _ ->
        assert false (* Error case *)



and any (): (range -> term) t =
    map (fun dstr -> Elab.any (int_of_string dstr)) digits



and arrow (): (range -> term) t =
    let* n, lst, res = one_or_more_rev (term ()) in
    Elab.arrow lst n res |> return


and pi (): (range -> term) t =
    (*
        ((%a: A) #(%b B) %c #%d ... ) R
     *)
    let* _, args, arg =
        one_or_more_rev (formal_argument ()) |> parens
    in
    let* rty  =
        term () |> ws_before
    in
    Elab.pi args arg rty |> return



and formal_argument (): (bool * Name.t located * term option) t =
    (*
            %x                  untyped
            (%x ty)             typed
            #%x                 implicit untyped
            #(%x ty)            implicit typed
    *)
    let* hash =
        char '#' |> ws_after |> optional
    in
    let* n, ty =
        (
            parens
                (
                    let* n = located name |> ws_after in
                    let* ty = (term ()) |> optional in
                    return (n, ty)
                )
            </>
            map
                (fun n -> n, None)
                (located name)
        )
    in
    return (hash <> None, n, ty)









(*
    Finalize
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)


let final_term: Elab.Final.t t =
    let* t = term () |> ws_around in
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
