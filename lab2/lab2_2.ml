fun printInt(a: int) =
  print (Int.toString(a) ^ " ");

fun printIntInf(a: IntInf.int) =
  print (IntInf.toString(a) ^ " ");

fun printReal(a: real) =
  print (Real.toString(a) ^ " ");

fun printString(a: string) =
  print (a ^ " ");

fun getInt() =
  Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);

fun getIntInf() =
  Option.valOf (TextIO.scanStream (IntInf.scan StringCvt.DEC) TextIO.stdIn);

fun getReal() =
  Option.valOf (TextIO.scanStream (Real.scan) TextIO.stdIn);

fun printEndOfLine() =
  print ("\n");

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

(*****开始*****)
(* 求最小值 *)
fun min(a: int, b: int) = if a < b then a else b;

(* 初始化距离数组 *)
fun reset(l: int Array2.array, 0) = l
  | reset(l, m) =
      let
        val start = getInt();
        val finish = getInt();
        val len = getInt();
        val newLen = min(len, Array2.sub(l, start, finish));
      in
        (Array2.update(l, start, finish, newLen);
         Array2.update(l, finish, start, newLen);
         reset(l, m - 1))
      end;

(* 搜索 *)
(* 搜索判断条件: 原点, 起点, 终点, 邻接矩阵, 距离列表, 点总数, 计步器 *)
fun findRoad(flag: int, s: int, start: int, finish: int, length: int Array2.array, road: int array, n: int, foot: int) =
  if foot > n then 0
  else
    if start > n then findRoad(flag, s, 1, 2, length, road, n, foot)
    else if finish > n then findRoad(flag, s, start, 1, length, road, n, foot)
    else if finish = start then
      if start = s then
        if flag = 1 then 1 (* 递归终止条件，所有点都遍历结束 *)
        else findRoad(1, s, start + 1, start + 2, length, road, n, foot + 1)
      else findRoad(flag, s, start + 1, start + 2, length, road, n, foot + 1)
    else
      let
        val roadf = Array.sub(road, finish);
        val roads = Array.sub(road, start);
        val dis = Array2.sub(length, start, finish);
        val newroad = min(roadf, dis + roads);
      in
        if newroad < roadf then
          (Array.update(road, finish, newroad);
           findRoad(flag, s, finish, finish + 1, length, road, n, foot + 1);
           findRoad(flag, s, start, finish + 1, length, road, n, foot))
        else findRoad(flag, s, start, finish + 1, length, road, n, foot)
      end;

(* 输出答案 *)
fun ans(a: int) = if a < 200000 then a else ~1;

fun printAns(road: int array, n: int, pos: int) =
  let
    val answer = ans(Array.sub(road, pos));
  in
    if pos = n then printInt(answer)
    else (printInt(answer); printAns(road, n, pos + 1))
  end;

val n = getInt();
val m = getInt();
val x = getInt();
val road = Array.array(n + 1, 200000);
val length = Array2.array(n + 1, n + 1, 200000);
reset(length, m);
Array.update(road, x, 0);
findRoad(0, x, x, x + 1, length, road, n, 0);
printAns(road, n, 1);
(*****结束*****)
