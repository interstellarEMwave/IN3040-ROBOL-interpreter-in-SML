(*exceptions, some to remove the 'nonexhaustive match'-warning/runtime error
* , some for easier code to write, and STOP, to halt the program wherever*)
exception INVALID;
exception STOP;
exception NOT_FOUND;
exception OUT_OF_BOUNDS

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
              | Loop of ((string * exp * exp) * stmt list)
              | Call of (string * exp list);


datatype ast_node = AST_Exp of exp;

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
*     stmt list (proc body)*)
datatype state = State of ((int * int) 
                         * ((int * int) 
                          * (int * int) 
                          * (string * int) list 
                          * (string * int) list 
                          * (string * string list * stmt list) list));
fun makeState(x, y) = State((x, y), ((0, 0), (1, 0), [], [], []));

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
fun interpretIdentifier(s, State(_,(_,_,binds_loc, binds_glob,_))) =
  searchBinds(s, binds_loc) handle NOT_FOUND => searchBinds(s, binds_glob);

fun interpretExp(e, s) = case e of Number(n) => interpretNumber(n)
                              | Identifier(i) => interpretIdentifier(i, s)
                              | Arithmetic_exp(a) => interpretArithmetic_exp(a, s)
                              | Boolean_exp(b) => interpretBoolean_exp(b, s)
                              | Nested_exp(w) => interpretExp(w, s)
and interpretArithmetic_exp((a:string, l:exp, r:exp), s:state) = interpretBinary_op(a)(interpretExp(l, s), interpretExp(r, s))
and interpretBoolean_exp((b:string, l:exp, r:exp), s:state) = interpretBinary_op(b)(interpretExp(l, s), interpretExp(r, s));


fun interpretStop(s) = raise STOP;

fun interpretTurn("clockwise", State(a,(b,(east, north),d,e,f)))        = State(a,(b,(north, ~east),d,e,f)) 
  | interpretTurn("counterclockwise", State(a,(b,(east, north),d,e,f))) = State(a,(b,(~north, east),d,e,f))
  | interpretTurn(_,_)                                                  = raise INVALID;

fun interpretStep(dis, State((w,h),((x,y),(east, north),d,e,f))) = 
  let val distance = interpretExp(dis, State((w,h),((x,y),(east,north),d,e,f)))
  in
    if    (x+distance*east   < 0 
    orelse x+distance*east  >= w 
    orelse y+distance*north  < 0 
    orelse y+distance*north >= h) 
    then raise OUT_OF_BOUNDS
    else
      State((w,h),((x+distance*east,y+distance*north), (east,north),d,e,f))
    end;

fun subtituteBind(name, add, nil) = raise NOT_FOUND
  | subtituteBind(name, add, (s:string,n:int)::l) = if name = s then (s,n+add)::l 
                                                    else subtituteBind(name, add, l);

fun interpretAssignment((name, "++"), State(a,(b,c,loc,glob,f))) =(
      State(a,(b,c,subtituteBind(name,1,loc),glob,f)) 
      handle NOT_FOUND => State(a,(b,c,loc,subtituteBind(name,1,glob),f)))
  | interpretAssignment((name, "--"), State(a,(b,c,loc,glob,f))) =(
      State(a,(b,c,subtituteBind(name,~1,loc),glob,f)) 
      handle NOT_FOUND => State(a,(b,c,loc,subtituteBind(name,~1,glob),f)))
  | interpretAssignment(_,_) = raise INVALID;


fun interpretLoop((check, l), s) = if interpretBoolean_exp(check,s) = 1 then interpretLoopBody(l,s)
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

and interpret(stat::stats,s) = interpret(stats,interpretStmt(stat,s))
  | interpret(nil,s) = s

and interpretCall((name, params), State(a,(b,c,d,e,f))) = 
  let val (pName,paramNames,body) = searchProc(name,f)
  in
    interpret(body,
    State(a,(b,c,makeParamBinds(params,paramNames,State(a,(b,c,d,e,f))),e,f)))
  end

and interpretStmt(stat,s) = case stat of Stop(_) => interpretStop(s)
                               |       Turn(dir) => interpretTurn(dir,s)
                               |      Step(step) => interpretStep(step,s)
                               |   Assignment(a) => interpretAssignment(a,s)
                               |         Loop(l) => interpretLoop(l,s)
                               |         Call(c) => interpretCall(c,s);

fun interpretAST_Node(node, s) = case node of AST_Exp(e) => interpretExp(e, s);



fun test() = print(Int.toString(interpretAST_Node
  (
    AST_Exp
    (
      Arithmetic_exp
      (
        "+", 
        Number(5), 
        Arithmetic_exp
        (
          "-",
          Number(8), 
          Number(9))
      )
    )
  ,makeState(5, 5)
  )) ^ "\n");


test();
