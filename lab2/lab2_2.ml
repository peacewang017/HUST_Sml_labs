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

datatype 'a graph = Graph of 'a list * ('a -> ('a * real) list)

fun shortest_path (Graph (vertices, edges)) start =
    let
        val dist = List.tabulate(length vertices, fn i => if i = start then 0.0 else Real.maxFinite)
        val visited = Array.array(length vertices, false)
        
        fun update_dist (v, d, q) =
            let
                val new_dist = dist start + d
            in
                if new_dist < Array.sub(dist, v) then
                    (Array.update(dist, v, new_dist); Array.update(q, v, (new_dist, v)::Heap.delete (op =) (new_dist, v) q))
                else
                    q
            end
            
        fun visit (v, q) =
            if Array.sub(visited, v) then
                q
            else
                (Array.update(visited, v, true); List.foldr update_dist q (edges v))
    
        val q = Heap.fromList [(0.0, start)]
        val q' = while not (Heap.isEmpty q) do
                     let
                         val (d, v) = Heap.deleteMin q
                     in
                         visit (v, q)
                     end
    in
        Array.toList dist
    end

val n = getInt();


  