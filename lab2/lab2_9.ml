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

fun printEndOfLine () =
    print("\n");

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


(*****Begin*****)
fun square (a:IntInf.int)=a * a;
(*快速幂*)
fun newQuickpowMod (m:IntInf.int,n:IntInf.int,x:IntInf.int)=
    if n=1 then m mod x
    else if n mod 2 = 0 then (square(newQuickpowMod(m,n div 2,x) mod x)) mod x
    else m * square(newQuickpowMod(m,n div 2,x) mod x) mod x;

fun Fermatmode (n:IntInf.int)=
    if newQuickpowMod(3,n-1,n)=1
        then if newQuickpowMod(5,n-1,n)=1
            then if newQuickpowMod(7,n-1,n)=1 then 1
                else 0
        else 0
    else 0;
(*素性测试*)
(*拆解n-1，找到基数d和2的r次幂*)
fun find_r (n:IntInf.int,r:IntInf.int)=
    if n mod 2 = 0 then find_r(n div 2,r+1)
    else r;

fun find_d (n:IntInf.int)=
    if n mod 2 = 0 then find_d(n div 2)
    else n;   
(*测试*)
fun check (x:IntInf.int,d:IntInf.int,r:IntInf.int,pos:IntInf.int)=
    if pos=0 then
        if newQuickpowMod(2,d,x)=1 then 1
        else if newQuickpowMod(2,d,x)=x-1 then 1
        else 0
    else
        let
            val tmp=newQuickpowMod(3,d,x);
            val mi=newQuickpowMod(2,pos,x);
            val flag=newQuickpowMod(tmp,mi,x);
        in
            if flag=x-1 then 1
            else if flag=1 then check(x,d,r,pos-1)
            else 0
        end;
(*2特殊单列*)
fun isprime(2:IntInf.int)=1
    |isprime(x:IntInf.int):IntInf.int=
    if Fermatmode(x)<>1 then 0
    else 
        let
            val d=find_d(x-1);
            val r=find_r(x-1,0);
        in
            if check(x,d,r,r)=1 then 1
            else 0
        end;

fun printans(n:IntInf.int)=if n=1 then printString("True") else printString("False");

val m=getIntInf();
printans(isprime(m));
(*****End*****)