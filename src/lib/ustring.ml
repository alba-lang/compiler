type t = {
    str:    string;
    len:    int;
    valid:  bool;
}



let string (ustr: t): string =
    ustr.str


let is_valid (ustr: t): bool =
    ustr.valid


let length (ustr: t): int =
    ustr.len


let byte_length (ustr: t): int =
    String.length ustr.str



let fold_intern (f: 'a -> int -> Uchar.t -> 'a) (start: 'a) (str: string): 'a =
    let byte_len = String.length str
    in
    let rec fold ibyte i a =
        assert (ibyte <= byte_len);

        if ibyte = byte_len then
            a

        else
            let decode = String.get_utf_8_uchar str ibyte in
            fold
                (ibyte + Uchar.utf_decode_length decode)
                (i + 1)
                (f a i (Uchar.utf_decode_uchar decode))
    in
    fold 0 0 start




let foldi (f: 'a -> int -> Uchar.t -> 'a) (start: 'a) (ustr: t): 'a =
    fold_intern
        f
        start
        ustr.str




let fold (f: 'a -> Uchar.t -> 'a) (start: 'a) (ustr: t): 'a =
    fold_intern
        (fun a _ uc -> f a uc)
        start
        ustr.str


let (^) (s1: t) (s2: t): t =
    assert (is_valid s1);
    assert (is_valid s2);
    {
        str   = s1.str ^ s2.str;
        len   = s1.len + s2.len;
        valid = true;
    }


let make (str: string): t =
    let len, valid =
        fold_intern
            (fun (len, valid) _ uc ->
                 len + 1,
                 valid && (uc <> Uchar.rep)
            )
            (0, true)
            str
    in
    {str; len; valid}








(* Unit Tests
 * ***************************************************************************
 *)

let%test _ =
    let ustr = make "hello" in
    is_valid ustr && length ustr = 5 && byte_length ustr = 5
