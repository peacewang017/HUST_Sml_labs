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


fun geti [] = 0 
  | geti (x::xs) =
    let
    val next = hd xs;
    in
    if(x > next) then next
    else geti(xs)
    end;

fun getj (list:int list , i:int) = 
    let 
    val head = hd list;
    val leftlist = List.drop(list,1);
    in
    if null list then 0
    else if(head>i) then head
    else getj(leftlist , i)
    end;

fun getindex(list:int list , x:int) = 
    let 
    val head = hd list;
    val leftlist = List.drop(list , 1);
    in
    if (head = x) then 1
    else 1+getindex(leftlist , x)
    end;

fun reverse [] = []
  | reverse (x::xs) = reverse (xs) @ [x];    
  
(*****Begin*****)
val n = getInt();
val list = getIntTable(n);
val i = geti(list);
val j = getj(list,i);
val i_index = getindex(list , i);
val j_index = getindex(list , j);
val list1 = List.take(list , j_index-1);
val leftlist = List.drop(list , j_index-1);
val revleft = reverse(leftlist);
val list2 = List.drop(revleft , n-i_index);
val list3 = List.drop(list , i_index);
val temp = (list1 @ list2) @ list3;
val a = List.take(temp , i_index-1);
val b = List.drop(temp , i_index-1);
val c = reverse(a);
val ans = c @ b;
printIntTable(ans);
(*****End*****)
