fun printInt (a:int) =
    print(Int.toString(a)^" ");

fun getInt () =
    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);

fun printIntTable ( [] ) = ()
  | printIntTable ( x::xs ) = 
    let
	val tmp = printInt(x)
    in
	printIntTable(xs)
    end;

fun getIntTable ( 0 ) = []
  | getIntTable ( N:int) = getInt()::getIntTable(N-1);

fun insert(x , []) = [x]
  | insert(x , y::xs) = 
    if (x < y) then x::y::xs
    else  y::insert(x , xs);

fun insertsort([]) = []
  | insertsort(x::xs) = insert(x , insertsort(xs));

(*****Begin*****)			 
val n = getInt();
val list = getIntTable(n);
val sortlist = insertsort(list);
printIntTable(sortlist);
(*****End*****)

