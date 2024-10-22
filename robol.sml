datatype exp = Exp of exp;
datatype number = Number of int;
datatype identifier = Identifier of string;
datatype binary_op = Binary_op of string; 
datatype arithmetic_exp = Arithmetic_exp of (string * exp * exp);
datatype boolean_exp = Boolean_exp of (string * exp * exp);
datatype exp =  Number of int 
             | Identifier of string 
             | Arithmetic_exp of (string * exp * exp)
             | Boolean_exp of (string * exp * exp);
datatype AST_Node = Exp of exp;

fun interpretBinary_op("+") = (op +)
  | interpretBinary_op("-") = (op -)
  | interpretBinary_op("*") = (op *);

fun interpretNumber(n) = n:int;
fun interpretIdentifier(s) = 1;
fun interpretExp(e:exp) = 0;
fun interpretArithmetic_exp(a:string, l:exp, r:exp) = interpretBinary_op(a)(interpretExp(l), interpretExp(r));
fun interpretBoolean_exp(b:string, l:exp, r:exp) = interpretBinary_op(b)(interpretExp(l), interpretExp(r));
fun interpretExp(Exp(e)) = case e of Number(n) => interpretNumber(n)
                              |        Identifier(i) => interpretIdentifier(i)
                              |        Arithmetic_exp(a) => interpretArithmetic_exp(a)
                              |        Boolean_exp(b) =>
                                  interpretBoolean_exp(b);
fun interpretAST_Node(node:AST_Node) = case node of Exp(e) => interpretExp(e)
                                           |         _ => 0;


fun test() = print(Int.toString(interpretAST_Node(AST_Node(Number(5)))) ^ "\n");
test();
