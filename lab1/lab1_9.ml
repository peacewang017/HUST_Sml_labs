fun printInt (a:int) =
    print(Int.toString(a)^" ");

fun printIntInf (a:IntInf.int) =
    print(IntInf.toString(a)^" ");


fun printReal (a:real) =
    print(Real.toString(a)^" ");

fun printString (a:string) =
    print(a^" ");

fun getInt () =
    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);

fun getIntInf () =
    Option.valOf (TextIO.scanStream (IntInf.scan StringCvt.DEC) TextIO.stdIn);

fun getReal () =
    Option.valOf (TextIO.scanStream (Real.scan) TextIO.stdIn);

(*****Begin*****)
fun addlist (m:int, n:int) =
    if m > n then []
    else m :: addlist (m+1, n);

fun printlist [] = ()
  | printlist (x::xs) = (printInt (x) ; printlist (xs));

val m = getInt ();
val n = getInt ();
val lst = addlist (m , n);
val () = printlist lst;



(*****End*****)

