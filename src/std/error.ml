open Fmlib_parse
open Fmlib_pretty


type t = {
    range: Position.range;
    tag:   string;
    doc:   unit -> Print.doc;
}


let make (range: Position.range) (tag: string) (doc: unit -> Print.doc): t =
    { range; tag; doc }


let range (e: t): Position.range =
    e.range


let line1 (e: t): int =
    Position.line (fst e.range)


let column1 (e: t): int =
    Position.column (fst e.range)


let line2 (e: t): int =
    Position.line (snd e.range)


let column2 (e: t): int =
    Position.column (snd e.range)


let tag (e: t): string =
    e.tag


let doc (e: t): Print.doc =
    e.doc ()
