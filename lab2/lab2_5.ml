fun printInt(a: int) =
    print (Int.toString (a) ^ " ");

fun printIntInf(a: IntInf.int) =
    print (IntInf.toString (a) ^ " ");

fun printReal(a: real) =
    print (Real.toString (a) ^ " ");

fun printString(a: string) =
    print (a ^ " ");

fun getInt() =
    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);

fun getIntInf() =
    Option.valOf (TextIO.scanStream (IntInf.scan StringCvt.DEC) TextIO.stdIn);

fun getReal() =
    Option.valOf (TextIO.scanStream (Real.scan) TextIO.stdIn);

fun printIntTable([]) = ()
  | printIntTable(x::xs) = 
    let
        val tmp = printInt(x)
    in
        printIntTable(xs)
    end;

fun printIntInfTable([]) = ()
  | printIntInfTable(x::xs) = 
    let
        val tmp = printIntInf(x)
    in
        printIntInfTable(xs)
    end;

fun getIntTable(0) = []
  | getIntTable(N: int) = getInt() :: getIntTable(N - 1);

fun getIntInfTable(0) = []
  | getIntInfTable(N: int) = getIntInf() :: getIntInfTable(N - 1);

fun getIntVector(0) = Vector.fromList []
  | getIntVector(N: int) = Vector.fromList (getIntTable(N));

fun getIntInfVector(0) = Vector.fromList []
  | getIntInfVector(N: int) = Vector.fromList (getIntInfTable(N));


(*****Begin*****)
fun parent([], m: int) =
    if m = 0 then 1 else 0
  | parent(x::xs, m) =
    if m < 0 then 0
    else 
        if x = 0 then parent(xs, m + 1)
        else parent(xs, m - 1);

val n = getInt ();
val tit = getIntTable (n);
printInt (parent (tit, 0));
(*****End*****)