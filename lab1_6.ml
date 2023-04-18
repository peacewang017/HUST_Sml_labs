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


type matrix = IntInf.int * IntInf.int * IntInf.int * IntInf.int

fun matrix_mul_with_mod(a:matrix , b:matrix , m:IntInf.int) = 
    ((#1 a * #1 b + #2 a * #3 b) mod m,
    (#1 a * #2 b + #2 a * #4 b) mod m,
    (#3 a * #1 b + #4 a * #3 b) mod m,
    (#3 a * #2 b + #4 a * #4 b) mod m);

fun square (a:matrix , m:IntInf.int) =
    matrix_mul_with_mod(a , a , m);


(*****Begin*****)
val n = getIntInf();
val m = getIntInf();

val n1 : IntInf.int = 1;
val n0 : IntInf.int = 0;
val a = (n1 , n1 , n1 , n0);

fun quick (a : matrix , n : IntInf.int , m : IntInf.int) = 
    if n = 1 then (1,1,1,0)
    else if n mod 2 = 0 then square(quick(a , n div 2 , m) , m)
    else matrix_mul_with_mod(square(quick(a, (n-1) div 2 , m) , m) , a , m);

val result = quick(a , n , m);
printIntInf(#2 result); 
(*****End*****)