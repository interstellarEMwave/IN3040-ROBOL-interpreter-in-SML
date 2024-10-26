fun max(a:int,b:int) = if a > b then a else b;
fun min(a:int,b:int) = if a < b then a else b;




(*Exp datatype, an exp can be made explicitly by using Nested_exp(), otherwise
* it somewhat functions as an abstract class, in that you make the subtype, but
* they all pass as an exp.
* every type is made as in-place as possible*)
datatype exp = Nested_exp of exp 
             | Number of int 
             | Identifier of string 
             | Arithmetic_exp of (string * exp * exp)
             | Boolean_exp of (string * exp * exp);

datatype stmt = Stop of int
              | Turn of string
              | Step of exp
              | Assignment of (string * string)
              | Loop of (exp * stmt list)
              | Call of (string * exp list);


datatype lang_stmt = Stmt of stmt
                   | Start of exp * exp
                   | Grid of int * int
                   | Binding of string * int
                   | Debug of int
                   | Proc_decl of string * string list * stmt list;


(*state that will be passed around.
* In order:
* 2-int tuple for Size
* tuple for robot state:
*   2-int tuple for position
*   2-int tuple for current facing direction
*   list of string-int tuple for local bindings
*   list of string-int tuple for global bindings
*   list of tuple of:
*     string (proc name)
*     string list (paramaters)
*     stmt list (proc body)
*   bool (flag for drawing board)*)
datatype state = State of ((int * int) 
                         * ((int * int) 
                          * (int * int) 
                          * (string * int) list 
                          * (string * int) list 
                          * (string * string list * stmt list) list)
                          * bool);
fun makeState(x:int, y:int,g:bool) = State((x, y), ((0, 0), (1, 0), [], [], []),g);

fun printPos(State(_,((x,y),_,_,_,_),_)) = print("(" ^ Int.toString(x) ^ "," ^ Int.toString(y) ^ ")\n");

fun printBindings(nil) = print("")
  | printBindings((x:string,y:int)::l) = 
  let val a = print("\n    " ^ x ^ ": " ^ Int.toString(y))
  in 
    printBindings(l)
  end;

fun printState(State((w,h),((x,y),(east,north),loc,glob,funcs),g)) =
  let val a =print("state:\n    " ^ Int.toString(x) ^ "," ^ Int.toString(y) ^
  "\n    " ^ Int.toString(east) ^ "," ^ Int.toString(north))
      val b = printBindings(loc)
      val c = printBindings(glob)
      val d = print("\n\n");
  in 
    State((w,h),((x,y),(east,north),loc,glob,funcs),g)
  end;


(*exceptions, some to remove the 'nonexhaustive match'-warning/runtime error
* , some for easier code to write, and STOP, to halt the program wherever*)
exception INVALID;
exception NOT_FOUND;
exception STOP of state;
exception OUT_OF_BOUNDS of state;
exception DEBUG of state;


fun takeArg(a:string option) = case(a) of NONE => ""
                           |       SOME(j) => j;
fun getDir( 1, 0) = ">"
  | getDir( 0, 1) = "^"
  | getDir(~1, 0) = "<"
  | getDir( 0,~1) = "v"
  | getDir(_,_) = raise INVALID;


fun actuallyDrawBoard(i, s, State((w,h),((x,y),(east,north),d,e,f),g)) = 
    if i = 0 then  actuallyDrawBoard(i+1, ANSITerm.toString(ANSITerm.FG(ANSITerm.Red)::[]), State((w,h),((x,y),(east,north),d,e,f),g))
    else if i = (h+2)*(w*2+5) then s^ANSITerm.toString(ANSITerm.FG(ANSITerm.Default)::[])
    else if i mod (w*2+5) = 2*x+2 andalso i div (w*2+5) = h-y then actuallyDrawBoard(i+1, 
                                                                                     s
                                                                                    ^ANSITerm.toString(ANSITerm.FG(ANSITerm.Default)::[])
                                                                                    ^getDir(east,north)
                                                                                    ^ANSITerm.toString(ANSITerm.FG(ANSITerm.Red)::[]) 
                                                                                    , State((w,h),((x,y),(east,north),d,e,f),g))
    else if i mod (w*2+5) = w*2+4 then actuallyDrawBoard(i+1, s^"\n", State((w,h),((x,y),(east,north),d,e,f),g))
    else if (i mod (w*2+5) mod 2) = 1 then actuallyDrawBoard(i+1, s^" ", State((w,h),((x,y),(east,north),d,e,f),g))
    else if i < w*2+5 
     orelse i > (h+1)*(w*2+5)
     orelse i mod (w*2+5) = 0
     orelse i mod (w*2+5) = w*2+2
    then actuallyDrawBoard(i+1, s^" ", State((w,h),((x,y),(east,north),d,e,f),g))
    else actuallyDrawBoard(i+1, s^".", State((w,h),((x,y),(east,north),d,e,f),g));

fun drawBoard(State(a,((x,y),c,d,e,f),g)) = 
  let val _ = takeArg(TextIO.inputLine(TextIO.stdIn))
  in
    let val _ = print(actuallyDrawBoard(0,"",State(a,((x,y),c,d,e,f),g)) ^ Int.toString(x) ^ ", " ^ Int.toString(y) ^"\n")
    in State(a,((x,y),c,d,e,f),g) end
  end;

fun interpretBinary_op("+") = (op +)
  | interpretBinary_op("-") = (op -)
  | interpretBinary_op("*") = (op *)
  | interpretBinary_op("<") = (fn(l, r) => if l < r then 1 else 0)
  | interpretBinary_op(">") = (fn(l, r) => if l > r then 1 else 0)
  | interpretBinary_op("=") = (fn(l, r) => if l = r then 1 else 0)
  | interpretBinary_op(_)   = raise INVALID;

fun interpretNumber(n) = n:int;

fun searchBinds(s, nil) = raise NOT_FOUND
  | searchBinds(s, (sc:string, n:int)::l) = if s = sc then n else searchBinds(s, l);

(*A small hack is used to check both local and global bindings*)
fun interpretIdentifier(s, State(a,(b,c,binds_loc, binds_glob,f),g)) =
  searchBinds(s, binds_loc) handle NOT_FOUND => searchBinds(s, binds_glob);

fun interpretExp(e, s) = case e of Number(n) => interpretNumber(n)
                              | Identifier(i) => interpretIdentifier(i,s)
                              | Arithmetic_exp(a) => interpretArithmetic_exp(a, s)
                              | Boolean_exp(b) => interpretBoolean_exp(b, s)
                              | Nested_exp(w) => interpretExp(w, s)

and interpretArithmetic_exp((a:string, l:exp, r:exp), s:state) = interpretBinary_op(a)(interpretExp(l, s), interpretExp(r, s))
and interpretBoolean_exp((b:string, l:exp, r:exp), s:state) = interpretBinary_op(b)(interpretExp(l, s), interpretExp(r, s));


fun interpretStop(s) = raise STOP s;

fun interpretTurn("clockwise", State(a,(b,(east, north),d,e,f),g))        = State(a,(b,(north, ~east),d,e,f),g) 
  | interpretTurn("counterclockwise", State(a,(b,(east, north),d,e,f),g)) = State(a,(b,(~north, east),d,e,f),g)
  | interpretTurn(_,_)                                                  = raise INVALID;

fun interpretStep(dis, State((w,h),((x,y),(east, north),d,e,f),g)) = 
  let val distance = interpretExp(dis, State((w,h),((x,y),(east,north),d,e,f),g))
  in
    if    (x+distance*east   < 0 
    orelse x+distance*east  >= w 
    orelse y+distance*north  < 0 
    orelse y+distance*north >= h) 
    then 
      if g then raise OUT_OF_BOUNDS(drawBoard(State((w,h),((max(~1,min(w,x+distance*east)),max(~1,min(h,y+distance*north))),(east,north),d,e,f),g)))
      else      raise OUT_OF_BOUNDS(          State((w,h),((max(~1,min(w,x+distance*east)),max(~1,min(h,y+distance*north))),(east,north),d,e,f),g))
    else
      State((w,h),((x+distance*east,y+distance*north), (east,north),d,e,f),g)
    end;

fun subtituteBind(name, add, nil) = raise NOT_FOUND
  | subtituteBind(name, add, (s:string,n:int)::l) = if name = s then (s,n+add)::l 
                                                    else (s,n)::subtituteBind(name, add, l);

fun interpretAssignment((name, "++"), State(a,(b,c,loc,glob,f),g)) =(
      State(a,(b,c,subtituteBind(name,1,loc),glob,f),g) 
      handle NOT_FOUND => State(a,(b,c,loc,subtituteBind(name,1,glob),f),g))
  | interpretAssignment((name, "--"), State(a,(b,c,loc,glob,f),g)) =(
      State(a,(b,c,subtituteBind(name,~1,loc),glob,f),g) 
      handle NOT_FOUND => State(a,(b,c,loc,subtituteBind(name,~1,glob),f),g))
  | interpretAssignment(_,_) = raise INVALID;


fun interpretLoop((Boolean_exp(e), l), s) = if
  interpretBoolean_exp(e,s) = 1 then interpretLoop((Boolean_exp(e), l),(interpretLoopBody(l,s)))
                                 else s

and makeParamBinds((e:exp)::exps,(str:string)::names,s:state) =
(str,interpretExp(e,s))::makeParamBinds(exps,names,s)
  | makeParamBinds(_,nil,s) = []
  | makeParamBinds(nil,_,s) = [] 

and searchProc(name, nil) = raise INVALID
  | searchProc(name, (namec, paramNames, body)::l) = 
    if name = namec then (namec, paramNames, body)
    else searchProc(name, l)

and interpretLoopBody(nil, s)  = s
  | interpretLoopBody((x:stmt)::l, s) = interpretLoopBody(l,interpretStmt(x,s))

and interpretStmts(stat::stats,s) = interpretStmts(stats,interpretStmt(stat,s))
  | interpretStmts(nil,s) = s

and interpretCall((name, params), State(a,(b,c,d,e,f),g)) = 
  let val (pName,paramNames,body) = searchProc(name,f)
  in 
    let
      val State(a2,(b2,c2,d2,e2,f2),g2) = interpretStmts(body,State(a,(b,c,makeParamBinds(params,paramNames,State(a,(b,c,d,e,f),g)),e,f),g))
    in
      State(a2,(b2,c2,d,e,f),g)
    end
  end

and interpretStmt(stat,State(a,(b,c,d,e,f),g)) = 
    let val s = State(a,(b,c,d,e,f),g)
    in 
      if g then
        case stat of Stop(_) => interpretStop(s)
           |       Turn(dir) => drawBoard(interpretTurn(dir,s))
           |      Step(step) => drawBoard(interpretStep(step,s))
           |   Assignment(a) => interpretAssignment(a,s)
           |         Loop(l) => interpretLoop(l,s)
           |         Call(c) => interpretCall(c,s)
      else
        case stat of Stop(_) => interpretStop(s)
           |       Turn(dir) => interpretTurn(dir,s)
           |      Step(step) => interpretStep(step,s)
           |   Assignment(a) => interpretAssignment(a,s)
           |         Loop(l) => interpretLoop(l,s)
           |         Call(c) => interpretCall(c,s)
    end;


fun interpretStart((x,y),State(a,(b,c,d,e,f),g)) = 
  let val s = State(a,(b,c,d,e,f),g)
  in 
    State(a,((interpretExp(x,s),interpretExp(y,s)),c,d,e,f),g)
  end;

fun interpretBinding((s,n),State(a,(b,c,d,l,f),g)) = State(a,(b,c,d,(s,n)::l,f),g);

fun interpretProc_decl((name,params,body),State(a,(b,c,d,e,f),g)) =
  State(a,(b,c,d,e,(name,params,body)::f),g);

fun interpretLang_stmt(node, State(a,(b,c,d,e,f),g)) =
  let val s = State(a,(b,c,d,e,f),g) in
    if g then
                                  case node of Stmt(l) => interpretStmt(l,s)
                                     |         Grid((g1,g2)) => makeState(g1,g2,g)
                                     |         Start(st) => drawBoard(interpretStart(st,s))
                                     |         Binding(b) => interpretBinding(b,s)
                                     |         Proc_decl(fd) => interpretProc_decl(fd,s)
                                     |         Debug(a) => raise DEBUG(s)
    else
                                  case node of Stmt(l) => interpretStmt(l,s)
                                     |         Grid((g1,g2)) => makeState(g1,g2,g)
                                     |         Start(st) => interpretStart(st,s)
                                     |         Binding(b) => interpretBinding(b,s)
                                     |         Proc_decl(fd) => interpretProc_decl(fd,s)
                                     |         Debug(a) => raise DEBUG(s)
  end

fun interpret((lstm:lang_stmt)::l,s) = interpret(l,interpretLang_stmt(lstm,s))
  | interpret(nil,s) = s;

fun add(l,n) = l::n;


fun test1(g) =
  interpret(
      Grid(64,64)
    ::Start(
        Number(23),
        Number(30))
    ::Stmt( Turn("clockwise"))
    ::Stmt( Turn("clockwise"))
    ::Stmt( Step( Number(15)))
    ::Stmt( Turn("counterclockwise"))
    ::Stmt( Step( Number(15)))
    ::Stmt( Turn("counterclockwise"))
    ::Stmt( 
        Step(
          Arithmetic_exp(
            "+",
            Number(2),
            Number(3))))
    ::Stmt( Turn("counterclockwise"))
    ::Stmt( 
        Step(
          Arithmetic_exp(
            "+",
            Number(17),
            Number(20))))
    ::Stmt( Stop(0))
    ::[]
    ,makeState(0,0,g))
  handle STOP(s) => s;

fun test2(g) = 
  interpret(
      Grid(64,64)
    ::Binding("i",5)
    ::Binding("j",3)
    ::Start(
        Number(23),
        Number(6))
    ::Stmt(Turn("counterclockwise"))
    ::Stmt(Step(
        Arithmetic_exp(
          "*",
          Number(3),
          Identifier("i"))))
    ::Stmt(Turn("clockwise"))
    ::Stmt(Step(Number(15)))
    ::Stmt(Turn("clockwise"))
    ::Stmt(Step(
        Arithmetic_exp(
          "-",
          Arithmetic_exp(
            "-",
            Number(12),
            Identifier("i")),
          Identifier("j"))))
    ::Stmt(Turn("clockwise"))
    ::Stmt(Step(
        Arithmetic_exp(
          "+",
          Arithmetic_exp(
            "*",
            Number(2),
            Identifier("i")),
          Arithmetic_exp(
            "+",
            Arithmetic_exp(
              "*",
              Number(3),
              Identifier("j")),
            Number(1)))))
    ::Stmt(Stop(0))
    ::[]
    ,makeState(0,0,g))
    handle STOP(s) => s;


fun test3(g) =
  interpret(
      Grid(64,64)
    ::Binding("i",5)
    ::Binding("j",4)
    ::Start(
        Number(23),
        Number(6))
    ::Stmt(Turn("counterclockwise"))
    ::Stmt(Step(
        Arithmetic_exp(
          "*",
          Number(3),
          Identifier("i"))))
    ::Stmt(Turn("counterclockwise"))
    ::Stmt(Step(Number(15)))
    ::Stmt(Turn("clockwise"))
    ::Stmt(Turn("clockwise"))
    ::Stmt(Step(Number(4)))
    ::Stmt(Turn("clockwise"))
    ::Stmt(Loop(
        Boolean_exp(
          ">",
          Identifier("j"),
          Number(1)),
        Step(Identifier("j"))
      ::Assignment("j","--")
      ::[]))
    ::Stmt(Stop(0))
    ::[]
    ,makeState(0,0,g))
    handle STOP(s) => s;

fun test4(g) = 
  interpret(
      Grid(64,64)
    ::Binding("i",8)
    ::Start(Number(1),Number(1))
    ::Stmt(
        Loop(
          Boolean_exp(
            "<",
            Identifier("i"),
            Number(100)),
            Step(Identifier("i"))::[]))
    ::[]
    ,makeState(0,0,g))
    handle STOP(s) => s
         | OUT_OF_BOUNDS(State(a,((x,y),c,e,d,f),g)) => 
             let val www = print("OUT_OF_WORLD Exception\nF\nOur man fell off at (" ^
             Int.toString(x)^","^Int.toString(y)^")\n")
             in
              State(a,((x,y),c,e,d,f),g)
             end;

fun test5(g) = 
  interpret(
      Grid(64,64)
    ::Binding("x",1)
    ::Binding("y",5)
    ::Proc_decl(
        "p1",
        "a"::"b"::[],
        Step(Identifier("a"))
      ::Turn("clockwise")
      ::Step(Identifier("b"))
      ::[])
    ::Start(Number(23),Number(30))
    ::Stmt(
        Call(
          "p1",
          Identifier("x")::Identifier("y")::[]))
    ::Stmt(Turn("clockwise"))
    ::Stmt(
        Call(
          "p1",
          Identifier("y")::Identifier("x")::[]))
    ::Stmt(Stop(0))
    ::[]
    ,makeState(0,0,g))
    handle STOP(s) => s
         | OUT_OF_BOUNDS(State(a,((x,y),c,e,d,f),g)) => 
             let val www = print("OUT_OF_WORLD Exception\nF\nOur man fell off at (" ^
             Int.toString(x)^","^Int.toString(y)^")\n")
             in
              State(a,((x,y),c,e,d,f),g)
             end;

fun test6(g) =
  interpret(
      Grid(64,64)
    ::Binding("x",3)
    ::Binding("y",3)
    ::Proc_decl(
        "p1",
        "a"::"b"::[],
        Step(Identifier("a"))
      ::Step(Identifier("b"))
      ::Assignment("a","--")
      ::[])
    ::Proc_decl(
        "p2",
        "a"::"b"::[],
        Step(Identifier("a"))
      ::Step(Identifier("b"))
      ::Assignment("b","--")
      ::[])
    ::Start(Number(23),Number(30))
    ::Stmt(Call( "p1", Identifier("x")::Identifier("y")::[]))
    ::Stmt(Call( "p2", Identifier("x")::Identifier("y")::[]))
    ::Stmt(Step(Identifier("x")))
    ::Stmt(Step(Identifier("y")))
    ::Stmt(Stop(0))
    ::[]
    ,makeState(0,0,g))
    handle STOP(s) => s
         | OUT_OF_BOUNDS(State(a,((x,y),c,e,d,f),g)) => 
             let val www = print("OUT_OF_WORLD Exception\nF\nOur man fell off at (" ^
             Int.toString(x)^","^Int.toString(y)^")\n")
             in
              State(a,((x,y),c,e,d,f),g)
             end;

fun test7(g) = 
  interpret(
      Grid(64,64)
    ::Binding("x",3)
    ::Binding("y",3)
    ::Proc_decl(
        "p1",
        "a"::"b"::[],
        Loop(
            Boolean_exp(
              ">",
              Identifier("a"),
              Number(1)),
            Step(Identifier("a"))
          ::Turn("clockwise")
          ::Step(Identifier("b"))
          ::Assignment("a","--")
          ::Assignment("b","--")
          ::Call( "p1", Identifier("a")::Identifier("b")::[])
          ::Step(Identifier("x"))
          ::[])
        ::[])
    ::Start(Number(23),Number(30))
    ::Stmt(Call("p1",Identifier("x")::Identifier("y")::[]))
    ::Stmt(Stop(0))
    ::[]
    ,makeState(0,0,g))
    handle STOP(s) => s
         | OUT_OF_BOUNDS(State(a,((x,y),c,e,d,f),g)) => 
             let val www = print("OUT_OF_WORLD Exception\nF\nOur man fell off at (" ^
             Int.toString(x)^","^Int.toString(y)^")\n")
             in
              State(a,((x,y),c,e,d,f),g)
             end;



fun runTests("1",g) = printPos(test1(g))
  | runTests("2",g) = printPos(test2(g))
  | runTests("3",g) = printPos(test3(g))
  | runTests("4",g) = printPos(test4(g))
  | runTests("5",g) = printPos(test5(g))
  | runTests("6",g) = printPos(test6(g))
  | runTests("7",g) = printPos(test7(g))
  | runTests("all",g) = 
      let val _ = printPos(test1(g))
          val _ = printPos(test2(g))
          val _ = printPos(test3(g))
          val _ = printPos(test4(g))
          val _ = printPos(test5(g))
          val _ = printPos(test6(g))
          val _ = printPos(test7(g))
      in print("") end
  | runTests(_) = raise INVALID;

fun getArg(_,_,nil) = raise INVALID
  | getArg(i,ic,x::l) = if i = ic then x else getArg(i+1,ic,l);

fun mandatory2() = 
  let val arg1 = getArg(0,0,CommandLine.arguments())
  in
    if arg1 = "d" then
      let val arg2 = getArg(0,1,CommandLine.arguments())
      in runTests(arg2,true) end
    else
      runTests(arg1,false)
  end;
 

mandatory2();
val _ = OS.Process.exit(OS.Process.success);
