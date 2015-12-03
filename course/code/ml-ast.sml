structure MLAst: ML_AST =
struct

exception NoRule

(*util*)
val out: TextIO.outstream option ref = ref NONE
fun print (s) =
    case !out 
     of SOME out' => 
        TextIO.output(out', s)
      | NONE => 
        TextIO.output(TextIO.stdOut, s)

structure PrimOp =
struct
datatype t 
  = Print
  | Add
  | Sub
  | Times
  | Int2String

fun pp t = 
    case t 
     of Add => print "+"
      | Sub => print "-"
      | Times => print "*"
      | _ => raise NoRule
end

datatype t
  = Var of string
  | Empty 
  | Num of int
  | String of string
  (* | Pair of t * t *) (*(e,e)*)
  | App of t * t 
  | FuncVal of string * t (*fn x => e*)
  | Tag of int * t (*ini e*)
  | LetVal of string * t * t
  (*let val x = e in e' end*)
  | Tuple of t list
  | Proj of int * t (* #i e *)
  | Case of t * string * t * string * t
  (* case e of in1 x1 => e1 | in2 x2 => e2 *)
  | Prims of PrimOp.t * t list
  | If0 of t * t * t  
  | LetFix of string * string * t * t

exception NoRule

fun printArgs (l: string list) =
    (*print something like "a,b,c" *)
    case l
     of [] => ()
      | x::[] => print x
      | x::xs => 
        let val _ = print x
            val _ = print(", ")
        in printArgs xs
        end

fun printList (l: t list) =
    (*print something like "a,b,c" *)
    case l
     of [] => ()
      | x::[] => dumpt x
      | x::xs => 
        let val _ = dumpt x
            val _ = print(", ")
        in printList xs
        end

and dumpt t =
    case t
     of Var (x) =>
        print(x)
      | Empty =>
        print("()") 
      | Num i =>
        print(Int.toString(i)) 
      | String(x) =>
        print("\""^x^"\"")
      | App (x, y) =>
        let val _ = print("(")
            val _ = dumpt(x)
            val _ = print(") (")
            val _ = dumpt(y)
            val _ = print(")")
        in ()
        end
      | FuncVal (x, e) =>
 (* a legal value in SML: (fn x => #1 x) (1,2)*)
        let val _ = print("fn " ^ x ^ " => (")
            val _ = dumpt(e)
        in print(")")
        end
      | Tag (i, e) =>
        let val _ = print("In"^Int.toString(i)^"(")
            val _ = dumpt(e)
        in print(")")
        end
      | LetVal (x, e1, e2) =>
        let val _ = print("let val " ^ x ^ " = ")
            val _ = dumpt(e1)
            val _ = print(" in ")
            val _ = dumpt(e2)
            val _ = print(" end ")
        in ()
        end
      | Tuple l =>
        let val _ = print("(")
            val _ = printList l;
        in print(")")
        end
      | Proj (i, e) =>
        let val _ = print("#"^Int.toString(i)^" ")
        in dumpt(e)
        end
      | Case (e, x1, e1, x2, e2) =>
     (* eg: case 1 of 1 => "1" |2 => "2" *)
        let val _ = print("case ")
            val _ = dumpt(e)
            val _ = print(" of In1 "^ x1^" => ")
            val _ = dumpt(e1)
            val _ = print("| In2 "^x2^" => ")
            val _ = dumpt(e2)
            val _ = print(" ")
        in ()
        end
      (*  | Pair (x, y) =>
        let val _ = print("(")
            val _ = dumpt(x)
            val _ = print(", ")
            val _ = dumpt(y)
            val _ = print(")")
        in ()
        end *)
      | Prims (PrimOp.Print, t::[]) => 
        let val _ = print ("print (")
            val _ = dumpt(t)
            val _ = print (")")
        in ()
        end
      | Prims (PrimOp.Int2String, t::[]) => 
        let val _ = print ("Int.toString (")
            val _ = dumpt(t)
            val _ = print (")")
        in ()
        end
      | Prims (option, t1::t2::[]) => 
        let val _ = print ("(")
            val _ = dumpt(t1)
            val _ = print (")")
            val _ = PrimOp.pp option
            val _ = print ("(")
            val _ = dumpt(t2)
            val _ = print (")")
        in ()
        end
      | If0 (t1, t2, t3) =>
        let val _ = print ("if (")
            val _ = dumpt(t1)
            val _ = print (")= 0 then (")
            val _ = dumpt(t2)
            val _ = print (") else (")
            val _ = dumpt(t3)
            val _ = print (")")
        in ()
        end
      | LetFix (f, x, t1, t2) =>
        let val _ = print ("let fun "^f^" ("^x^") = (")
            val _ = dumpt(t1)
            val _ = print (") in (")
            val _ = dumpt(t2)
            val _ = print (") end")
        in ()
        end
      | _ => raise NoRule


fun dump2file (t: t, fname: string) =
    let val out' = TextIO.openOut (fname)
        val _ = out:= SOME out'
        val _ = print("datatype ('a, 'b) s = In1 of 'a | In2 of 'b\n")
        val _ = print("val ml_code = ")
        val _ = dumpt(t)
        val _ = TextIO.flushOut (out')
        val _ = TextIO.closeOut(out')
    in ()
    end

fun pp (t: t) =
let val _ = out:= NONE
    val _ = dumpt(t)
    in print "\n"
    end
end
