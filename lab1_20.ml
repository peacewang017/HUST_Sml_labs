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


fun printIntTable ( [] ) = ()
  | printIntTable ( x::xs ) = 
    let
	val tmp = printInt(x)
    in
	printIntTable(xs)
    end;

fun printIntInfTable ( [] ) = ()
  | printIntInfTable ( x::xs ) = 
    let
	val tmp = printIntInf(x)
    in
	printIntInfTable(xs)
    end;

fun getIntTable ( 0 ) = []
  | getIntTable ( N:int) = getInt()::getIntTable(N-1);

fun getIntInfTable ( 0 ) = []
  | getIntInfTable ( N:int) = getIntInf()::getIntInfTable(N-1);

fun getIntVector ( 0 ) =  Vector.fromList []
  | getIntVector ( N:int) = Vector.fromList(getIntTable(N));

fun getIntInfVector ( 0 ) = Vector.fromList []
  | getIntInfVector ( N:int) = Vector.fromList(getIntInfTable(N));

fun istaken (n:int , m:int , x:int , y:int) = 
  if ((n = x-2) andalso (m = y-1)) orelse
     ((n = x-1) andalso (m = y-2)) orelse
     ((n = x+1) andalso (m = y-2)) orelse
     ((n = x+2) andalso (m = y-1)) orelse
     ((n = x) andalso (m = y)) orelse
     ((n = x-1) andalso (m = y+2)) orelse
     ((n = x-2) andalso (m = y+1)) orelse
     ((n = x+1) andalso (m = y+2)) orelse
     ((n = x+2) andalso (m = y+1)) then 1
  else 0;

fun getans (n:int , m:int , x:int , y:int) = 
  if (n < 0) then 0
  else if (m < 0) then 0
  else if ((n = 0) andalso (m = 0)) then 1
  else if (istaken(n , m , x, y) = 1) then 0
  else let 
       val ans1 = getans(n-1 , m , x , y);
       val ans2 = getans(n , m-1 , x , y);
       in
       (ans1+ans2)
       end;
       
(*****Begin*****)
val n = getInt();
val m = getInt();
val x = getInt();
val y = getInt();
printInt(getans(n , m , x , y));
(*****End*****)

