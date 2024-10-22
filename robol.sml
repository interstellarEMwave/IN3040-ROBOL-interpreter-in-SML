datatype exp = Exp of exp;
type number = int;
type identifier = string;
datatype binary_op = Binary_op of string; 
datatype arithmetic_exp = Arithmetic_exp of (binary_op * exp *exp);
datatype boolean_exp = Boolean_exp of arithmetic_exp;
datatype exp = Exp of exp
             | Number of number
             | Identifier of identifier
             | Arithmetic_exp of arithmetic_exp
             | Boolean_exp of boolean_exp;

datatype AST_Node = Number of number
                  | Exp of exp
                  | Identifier of identifier
                  | Arithmetic_exp of arithmetic_exp
                  | Boolean_exp of boolean_exp;

fun interpret(node) = case node of Number(n) => n
                                 | Identifier(s) => 4
                                 | _ => 3;
fun test() = print(Int.toString(interpret(Number(5))) ^ "\n");
test();
