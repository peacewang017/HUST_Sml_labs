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
fun getnum (l:int list,0)=l
    | getnum (l,n)=
        let
            val c=getInt();
        in
            if n>0 then getnum(c::l,n-1) else l
        end;
fun scanNum (n:int)=getnum([],n);(*输入数据*)

fun retaken ([]:int list,l)=l
    |retaken (x::xs,l)=retaken (xs,x::l);

fun correct ([])=[]
    |correct (x::xs:int list)=if x<=0 then correct (xs) else x::xs;

(*保证第一个数的长度更长*)

fun add (x::xs:int list,[],l:int list,carry:int)=add (xs,[],(x+carry)::l,0)
    |add ([],[],l,carry)=if carry=0 then l else carry::l
    |add ([],y::ys,l,carry)=l
    |add (x::xs,y::ys,l,carry)=
        let 
            val midvalue=carry+x+y;
            val cur=midvalue mod 10;
        in 
            add(xs,ys,cur::l,midvalue div 10)
        end;
fun addans (a,b)=if correct(add(a,b,[],0))=nil then [0] else correct(add(a,b,[],0));
(*题目保证大数减小数成立*)
fun sub ([],[],l,carry)=l
    |sub ([],y::ys,l,carry)=l
    |sub (x::xs :int list,[],l,carry)=
        if x>=carry then sub (xs,[],(x-carry)::l,0)
        else sub (xs,[],(10+x-carry)::l,1)
    |sub (x::xs,y::ys,l,carry)=
        let
            val cutNum=y+carry;
            val cur=if x>=cutNum then x-cutNum else 10+x-cutNum;
        in
            sub (xs,ys,cur::l,if x>=cutNum then 0 else 1)
        end;

fun subans (a,b)=if correct(sub(a,b,[],0))=nil then [0] else correct(sub(a,b,[],0));

fun simx ([],a:int,l,carry)=if carry=0 then l else carry::l
    |simx (x::xs,a,l,carry)=
        let
            val num=x*a + carry;
            val cur=num mod 10;
        in 
            simx (xs,a,cur::l,num div 10)
        end;

fun addzerox (x,a,l,pos)=
    if pos=0 then simx (x,a,l,0)
    else addzerox (x,a,0::l,pos-1);
fun multi (x,a,pos)=retaken(addzerox(x,a,[],pos),[]);(*右高左低*)

fun acux (x:int list,[],pos,l)=l
    |acux ([],y:int list,pos,l)=l
    |acux (x,y::ys,pos,l)=
        let
            val midval=multi (x,y,pos);
        in 
            acux(x,ys,pos+1,retaken(addans(midval,l),[]))
        end;
fun multians (a,b)=retaken(acux(a,b,0,[]),[]);

val n=getInt();
val a=scanNum(n);
val m=getInt();
val b=scanNum(m);
printIntTable(addans(a,b));
printEndOfLine();
printIntTable(subans(a,b));
printEndOfLine();
printIntTable(multians(a,b));
(*****End*****)