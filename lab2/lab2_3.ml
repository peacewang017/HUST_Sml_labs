fun printInt (a: int) =
    print (Int.toString (a) ^ " ");

fun getInt () =
    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);

fun printIntTable ([]) = ()
  | printIntTable (x::xs) = 
    let
        val tmp = printInt (x)
    in
        printIntTable (xs)
    end;

fun getIntTable (0) = []
  | getIntTable (N: int) = getInt () :: getIntTable (N - 1);

fun printArray (Arr) =
    let
        val cur = ref 0
        val len = Array.length (Arr)
    in
        while !cur < len do
        (
            printInt (Array.sub (Arr, !cur));
            cur := !cur + 1
        )
    end;

fun printString (s) = print (s ^ " ");

(*****Begin*****)			 
fun maxnum (a, b): int =
    if a > b then a else b;

fun firmax ([], mark: int, len: int, pos: int) = 0
  | firmax (x::xs, mark, len, pos) =
    if mark = 0 then
        if pos = 0 then
            if x = 0 then firmax (xs, mark + 1, len + 1, pos + 1)
            else firmax (xs, mark - 1, len + 1, pos + 1)
        else len
    else if mark < 0 then 0
    else
        if x = 0 then firmax (xs, mark + 1, len + 1, pos + 1)
        else firmax (xs, mark - 1, len + 1, pos + 1);

fun measure (l: int list) = firmax (l, 0, 0, 0);

fun tableofml ([]) = []
  | tableofml (x::xs) = measure (x::xs) :: tableofml (xs);

fun maxval ([]) = 0
  | maxval (x::xs) = maxnum (x, maxval (xs));

val n = getInt ();
val tit = getIntTable (n);
val ans = tableofml (tit);
printInt (maxval (ans));
(*****End*****)
