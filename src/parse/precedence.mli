type assoc =
    | Left
    | Right
    | No

type t = int * assoc


val of_string: string -> t
(** [of_string str] The precedence of the operator [str] *)


val leaning: t -> t -> assoc
(** [leaning p1 p2] To which side leans a parse tree?

    Is an expression of the form

    {[
        a op1 b op2 c
    ]}

    leaning to the left i.e. parsed as

    {[
        (a op1 b) op2 c
    ]}

    or leaning to the right i.e. parsed as

    {[
        a op1 (b op2 c)
    ]}

    or not parseable (e.g. relational operators)

*)


val where: t

val comma: t

val assign: t

val colon: t

val arrow: t

val or_: t

val and_: t

val not_: t

val apply_left: t

val apply_right: t

val compose_left: t

val compose_right: t

val relation: t

val add_left: t

val add_right: t

val mult: t

val exp: t

val highest_op_left: t

val highest_op_right: t

val application: t




(**
{v
    expression                          parsed as

    a + b + c                           (a + b) + c

    - a + b                             (- a) + b

    - a * b                             - (a * b)

    x |> f |> g                         (x |> f) |> g

    f <| g <| x                         f <| (g <| x)

    x |> f <| y                         (x |> f) <| y

    x |> f >> g                         x |> (f >> g)

    a :: b :: c                         a :: (b :: c)

    A : B : C                           A : ( B : C)

    \ x := a, b                         (\ x := a), b
    --                                  comma < assign

    \ x := a : T                        \ x := (a: T)
    --                                  assign < colon

    all x: T, e                         (all x: T), e
    --                                  comma < colon

    all y: T -> U                        all y: (T -> U)
    --                                  colon < arrow
v}
*)
