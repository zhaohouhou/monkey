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
  | LessThan
  | LargerThan
  | Equals
  | AndAlso
  | OrElse
  | Not

fun pp t = 
    case t 
     of Add => print "+"
      | Sub => print "-"
      | Times => print "*" 
      | LessThan => print "<"
      | LargerThan => print ">"
      | Equals => print "="
      | Not => print "not"
      | AndAlso => print "andalso"
      | OrElse => print "orelse"
      | Print => print "print"
      | Int2String => print "Int.toString"
end

datatype t
  = Var of string
  | Empty 
  | True
  | False
  | Num of int
  | String of string
  | App of t * t 
  | FuncVal of string * t (*fn x => e*)
  | Tag of int * t 
  | LetVal of string * t * t
  | Tuple of t list
  | Proj of int * t (* #i e *)
  | Case of t * ((int * string * t) list )
      (* case e of (INi xi => ei) list *)
  | Prims of PrimOp.t * t list
  | If of t * t * t 
  | LetFix of string * string * t * t

fun 'a printlist (l: 'a list, p: 'a->unit, sep: string, afterlast: bool) =
    let fun doit l =
            case l
             of [] => ()
              | [x] => (p x; if afterlast then print sep else ())
              | x::xs => (p x; print sep; doit xs)
    in  doit l
    end

fun dumpt t:unit =
    case t
     of Var (x) =>  print(x)
      | Empty =>  print("()") 
      | True =>  print "true"
      | False =>  print "false"
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
        let val _ = print("IN"^Int.toString(i)^"(")
            val _ = dumpt e
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
            val _ = printlist (l, dumpt, ", ", false);
        in print(")")
        end
      | Proj (i, e) =>
        let val _ = print("#"^Int.toString(i)^" ")
        in dumpt(e)
        end
      | Case (e, cases) =>
        let val _ = print("(case ")
            val _ = dumpt(e)
            val _ = print(" of ")
            val _ = printlist(cases
                            , fn (i, x, t) =>
                                 (print ("IN"^Int.toString(i)
                                         ^"("^x^") => ")
                                ; dumpt t)
                            , " | "
                            ,false)
            val _ = print(" )")
        in ()
        end
      | Prims (opr, t::[]) =>
        let val _ = PrimOp.pp opr
            val _ = print (" (")
            val _ = dumpt(t)
            val _ = print (")")
        in ()
        end
      | Prims (opr, t1::t2::[]) => 
        let val _ = print ("(")
            val _ = dumpt(t1)
            val _ = print (")")
            val _ = PrimOp.pp opr
            val _ = print ("(")
            val _ = dumpt(t2)
            val _ = print (")")
        in ()
        end
      | If (t1, t2, t3) =>
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
       (* val _ = print("datatype ('a, 'b) s = IN1 of 'a | IN2 of 'b\n")*)
        (*Don't know how many...*)
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
