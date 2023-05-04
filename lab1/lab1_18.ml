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

fun getvecnum (vec:int vector , n:int):int = Vector.sub(vec,n-1);

fun bsearch (vec:int vector , x:int , head:int , tail:int):int =
    if ((head = tail) andalso (getvecnum(vec , head) = x)) then head-1
    else if (head >= tail) then ~1 
    else let 
         val mid = (head + tail) div 2;
         val leftans = bsearch(vec , x , head , mid);
         val rightans = bsearch(vec , x ,mid+1 , tail);
         in 
         if (leftans <> ~1) then leftans
         else if (rightans <> ~1) then rightans
         else ~1
         end;

fun printans (vec1:int vector , vec2:int vector , i:int) = 
    if (i > Vector.length(vec2)) then 0
    else let   
         val searchnum = getvecnum(vec2 , i);
         val temp = printInt(bsearch(vec1 , searchnum , 1 , Vector.length(vec1)));
         in
         printans(vec1 , vec2 , i+1)
         end;

(*****Begin*****)
val n = getInt();
val q = getInt();
val vec1 = getIntVector(n);
val vec2 = getIntVector(q);
printans(vec1 , vec2 , 1);
(*****End*****)