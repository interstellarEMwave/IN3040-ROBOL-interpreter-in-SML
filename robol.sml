datatype exp = Nested_exp of exp 
             | Number of int 
             | Identifier of string 
             | Arithmetic_exp of (string * exp * exp)
             | Boolean_exp of (string * exp * exp)

datatype ast_node = AST_Exp of exp;

fun interpretBinary_op("+") = (op +)
  | interpretBinary_op("-") = (op -)
  | interpretBinary_op("*") = (op *)
  | interpretBinary_op("<") = (fn(l, r) => if l < r then 1 else 0)
  | interpretBinary_op(">") = (fn(l, r) => if l > r then 1 else 0)
  | interpretBinary_op("=") = (fn(l, r) => if l = r then 1 else 0)
  | interpretBinary_op(_)   =  (fn(a, b) => 999);


fun interpretNumber(n) = n:int;


fun interpretIdentifier(s) = 1;


fun interpretExp(e) = case e of Number(n) => interpretNumber(n)
                              | Identifier(i) => interpretIdentifier(i)
                              | Arithmetic_exp(a) => interpretArithmetic_exp(a)
                              | Boolean_exp(b) => interpretBoolean_exp(b)
                              | Nested_exp(w) => interpretExp(w)

and interpretArithmetic_exp(a:string, l:exp, r:exp) = interpretBinary_op(a)(interpretExp(l), interpretExp(r))

and interpretBoolean_exp(b:string, l:exp, r:exp) = interpretBinary_op(b)(interpretExp(l), interpretExp(r));


fun interpretAST_Node(node) = case node of AST_Exp(e) => interpretExp(e);


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
  )) ^ "\n");


test();
