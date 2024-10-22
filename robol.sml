datatype exp = Exp of exp;
type number = int;
type identifier = string;
datatype binary_op = Binary_op of string; 
datatype arithmetic_exp = Arithmetic_exp of (binary_op * number * number);
datatype boolean_exp = Boolean_exp of arithmetic_exp;
datatype exp = Exp of exp
             | Number of number
             | Identifier of identifier
             | Arithmetic_exp of (binary_op * number * number)
             | Boolean_exp of boolean_exp;
datatype AST_Node = Number of number
                  | Exp of exp
                  | Identifier of identifier
                  | Arithmetic_exp of arithmetic_exp
                  | Boolean_exp of boolean_exp;


fun getOp("+") = (fn(l:number, r:number) => (l + r):number)
  | getOp("-") = (fn(l:number, r:number) => (l - r):number)
  | getOp("*") = (fn(l:number, r:number) => (l * r):number);

fun interpret(n:number) = n
  | interpret(_) = 3:number;
fun test() = print(Int.toString(interpret(Number(5))) ^ "\n");
test();
