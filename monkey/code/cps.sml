structure Cps: CPS =
struct

type cont = string
type func = string

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
  = LetVal of string * v * t 
  | LetCont of cont * string * t * t  
  | ContApp of cont * string   
  | FuncApp of func * cont * string  
  | Case of string * ((int * string * t) list) 
  | LetPrim of string * PrimOp.t * string list * t 
  | If of string * cont * cont
  | LetFix of func * cont * string * t * t 
  | Exit of string  
     and v
       = Empty
       | True
       | False
       | Num of int
       | String of string 
       | Tuple of string list 
       | Tag of int * string 
       | FuncVal of cont * string * t 
       | Proj of int * string

fun printSpace (t: int) =
    if 0 = t
    then ()
    else let val _ = print "    "
         in printSpace (t-1)
         end
         
fun 'a printlist (l: 'a list, p: 'a->unit, sep: string, afterlast: bool) =
    let fun doit l =
            case l
             of [] => ()
              | [x] => (p x; if afterlast then print sep else ())
              | x::xs => (p x; print sep; doit xs)
    in  doit l
    end

fun dumpv (t: v, space : int): unit =
    case t
     of Empty => print "()"
      | True =>  print "true"
      | False =>  print "false"
      | Num i => print (Int.toString(i))
      | String s =>
        let val _ = print ("\""^s^"\"") in () end
      | Tuple l =>
        let val _ = print("(")
            val _ = printlist (l, print, ", ", false);
        in print(")")
        end
      | Tag (l, x) =>
        let val _ = print "IN"
            val _ = print (Int.toString l)
            val _ = print (" " ^ x)
        in () 
        end
      | FuncVal (k, x, tBody) =>
        let val _ = print ("fn (" ^ k ^", " ^ x ^ ") => \n")
            val _ = dumpt (tBody, space +1)
            val _ = print "\n"
        in () 
        end
      | Proj (i, x) =>
        let val _ = print "#"
            val _ = print (Int.toString i)
            val _ = print (" " ^ x)
        in () 
        end


and dumpt (t: t, space: int): unit =
    (*print with indent*)
    case t
     of LetVal(x, vBody, tBody) =>
        let val _ = printSpace space
            val _ = print ("let val " ^ x ^ " = ")
            val _ = dumpv (vBody, space +1)
            val _ = print "\n"
            val _ = printSpace space
            val _ = print "in\n"
            val _ = dumpt (tBody, space +1)
            val _ = print "\n"
            val _ = printSpace space
            val _ = print "end\n"
        in ()
        end
      | LetCont(k, x, letBody, inBody) =>
        let val _ = printSpace space
            val _ = print ("let fun " ^ k ^ " " ^ x ^ " =\n")
            val _ = dumpt (letBody, space +1)
            val _ = print "\n"
            val _ = printSpace space
            val _ = print "in\n"
            val _ = dumpt (inBody, space +1)
            val _ = print "\n"
            val _ = printSpace space
            val _ = print "end\n" 
        in ()
        end
      | ContApp(k, x) =>
        let val _ = printSpace space
            val _ = print (k ^ " " ^ x ^ "\n")
        in ()
        end
      | FuncApp(f, k, x) =>
        let val _ = printSpace space
            val _ = print (f ^ " (" ^ k ^ ", " ^ x ^ ")\n")
        in () end
      | Case(x, cases) =>
        let val _ = printSpace space
            val _ = print ("(case " ^ x ^ " of\n")
            val _ = printlist (cases
                             , fn (i, z, t) =>
                                  (print ("IN"^Int.toString(i)
                                          ^" "^z^" =>\n")
                                 ;dumpt(t, space +1))
                             , " | "
                             , false)
            val _ = print(" )")
        in ()
        end
      | Exit x => 
        let val _ = printSpace space
        in print ("exit " ^ x)
        end
      | LetPrim (x, PrimOp.Print, [s], t) =>
        let val _ = printSpace space
            val _ = print ("let val " ^ x ^ " = print "^s^"\n in\n")
            val _ = dumpt (t, space +1)
            val _ = print "\n"
            val _ = printSpace space
            val _ = print "end\n"
        in ()
        end
      | LetPrim (x, option, [x1, x2], t) =>
        let val _ = printSpace space
            val _ = print ("let val " ^ x ^ " = "^x1)
            val _ = PrimOp.pp option
            val _ = print (x2^"\n")
            val _ = printSpace space
            val _ = print ("in\n")
            val _ = dumpt (t, space +1)
            val _ = print "\n"
            val _ = printSpace space
            val _ = print "end\n"
        in ()
        end
      | LetFix (f, k, x, fbody, e) =>
        let val _ = printSpace space
            val _ = print ("let fun " ^ f ^ "("^k^", "^x^") = (\n")
            val _ = dumpt (fbody, space +1)
            val _ = printSpace (space+1)
            val _ = print ")\n"
            val _ = printSpace space
            val _ = print ("in\n")
            val _ = dumpt (e, space +1)
            val _ = print "\n"
            val _ = printSpace space
            val _ = print "end\n"
        in ()
        end
      | If (x, k1, k2) =>
        let val _ = printSpace space
            val _ = print ("if " ^ x ^ " then " ^ k1 ^ "() else " ^ k2 ^ "()\n")
        in ()
        end
      | LetPrim (x, PrimOp.Int2String, [s], t) =>
        let val _ = printSpace space
            val _ = print ("let val " ^ x ^ " = Int.toString "^s^"\n in\n")
            val _ = dumpt (t, space +1)
            val _ = print "\n"
            val _ = printSpace space
            val _ = print "end\n"
        in ()
        end
      | _ => raise NoRule


fun pp ( t) = 
    let val _ = out := NONE
    in  dumpt(t, 0)
    end

fun dump2file (t: t, fname: string) =
    let val out' = TextIO.openOut (fname)
        val _ = out:= SOME out'
        val _ = print ("datatype ('a, 'b) s = IN1 of 'a | IN2 of 'b\n")
        val _ = print ("fun exit x = ()\n")
        val _ = print ("val cps_code =\n")
        val _ = dumpt(t, 0)
        val _ = TextIO.flushOut (out')
        val _ = out := NONE
        val _ = TextIO.closeOut(out')
    in ()
    end

end

