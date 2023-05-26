fun printInt(a: int) =
    print (Int.toString (a) ^ " ");

fun getInt() =
    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);

fun printIntTable([]) = ()
  | printIntTable(x::xs) = 
    let
        val tmp = printInt(x)
    in
        printIntTable(xs)
    end;

fun getIntTable(0) = []
  | getIntTable(N: int) = getInt () :: getIntTable (N - 1);

fun printArray(Arr) =
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

fun printString(s) = print (s ^ " ");

(*****Begin*****)			 
fun printEndOfLine() =
    print ("\n");

fun max(a, b): int =
    if a > b then a else b;

fun min(a, b): int =
    if a > b then b else a;


(* 收录房子 *)
fun scanf(n: int, l: int array, pos: int) =
    if pos >= n then Array.update (l, pos, getInt ())
    else 
        let
            val value = getInt ();
            val tmp = Array.update (l, pos, value);
        in
            scanf (n, l, pos + 1)
        end;

(* 找天际线长度 *)
fun maxm(l: int array, pos: int, n: int) =
    if pos = n then Array.sub (l, pos)
    else
    let
        val maxi = Array.sub (l, pos);
    in
        max (maxi, maxm (l, pos + 3, n))
    end;

fun minm(l: int array, pos: int, n: int) =
    if pos = n - 2 then Array.sub (l, pos)
    else
    let
        val mini = Array.sub (l, pos);
    in
        min (mini, minm (l, pos + 3, n))
    end;

(* 更新天际线 *)
fun renew(l: int array, left, height, right) =
    if left >= right then l
    else 
        let
            val leftval = Array.sub (l, left);
            val myval = max (leftval, height);
        in
            (Array.update (l, left, myval); renew (l, left + 1, height, right))
        end; 

fun skyline(l: int array, line: int array, pos: int, n: int) =
    if pos > n then line
    else
    let
        val left = Array.sub (l, pos);
        val height = Array.sub (l, pos + 1);
        val right = Array.sub (l, pos + 2);
    in
        (renew (line, left, height, right); skyline (l, line, pos + 3, n))
    end;

fun printans(l: int array, pos: int, exval: int, len: int) =
    if pos > len then printEndOfLine ()
    else
        let
            val value = Array.sub (l, pos);
        in
            if value = exval then printans (l, pos + 1, exval, len)
            else (printInt (pos); printInt (value); printEndOfLine (); printans (l, pos + 1, value, len))
        end;
         
val n = getInt ();
val num = 3 * n;
val arr = Array.array (num + 1, 0);
scanf (num, arr, 1);
val finish = maxm (arr, 3, num);
val start = minm (arr, 1, num);
val line = Array.array (finish + 1, 0);
val sky = skyline (arr, line, 1, num);
printans (sky, start, 500, finish);

(*****End*****)