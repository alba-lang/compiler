type 'a t = {
    mutable length: int;
    mutable buffer: 'a array;
}




let make_room (b: 'a t) (n: int): unit =
    assert (0 <= n);
    assert (0 < b.length); 
    if
        b.length + n <= Array.length b.buffer
    then
        (* capacity is sufficient, do nothing *)
        ()
    else
        (* double the capacity *)
        begin
            let cap = 2 * Array.length b.buffer in
            let buffer = Array.make cap b.buffer.(0) in
            Array.blit b.buffer 0 buffer 0 b.length;
            b.buffer <- buffer
        end



let length (b: 'a t): int =
    b.length




let make (): 'a t = {
    length = 0;
    buffer = [||];
}




let get (b: 'a t) (i: int): 'a =
    assert (i < length b);
    b.buffer.(i)



let set (b: 'a t) (i: int) (a: 'a): unit =
    assert (i < length b);
    b.buffer.(i) <- a




let push (b: 'a t) (a: 'a): unit =
    if b.length = 0 then
        begin
            b.length <- 1;
            b.buffer <- [| a |]
        end
    else
        begin
            make_room b 1;
            b.buffer.(b.length) <- a;
            b.length <- b.length + 1
        end
