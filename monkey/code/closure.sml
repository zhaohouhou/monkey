structure Closure: CLOSURE =
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
  = LetVal of {name: string
             , value: v , body: t}
  | Let of {name: string, index: int
          , tuple: string, body: t} 
  | LetCont of {cont: string
              , env: string
              , arg: string
              , fbody: t
              , body: t
              , freevars: string list}
  | ContApp of {cont: string
              , env: string
              , arg: string}
  | FuncApp of {func: string
              , cont: string
              , env: string
              , arg: string}
  | Case of {cond: string
           , branches: (int * string * t) list} 
  | LetPrim of {name: string
              , opr: PrimOp.t
              , args: string list
              , body: t}
  | If of {cond: string
         , thenn: t
         , elsee: t} 
  | LetFix of {func :string
             , env: string
             , cont: string
             , arg: string
             , fbody: t
             , body: t
             , freevars: string list}
  | Exit of string
     and v
       = Empty
       | True
       | False
       | Num of int
       | String of string 
       | Tuple of string list 
       | Tag of {label: int
               , name: string}  
       | FuncVal of {env: string
                   , cont: string
                   , arg: string
                   , fbody: t
                   , freevars: string list}

fun ppv (t: v, space : int) =
    case t
     of Empty => print "()"
      | True =>  print "true"
      | False =>  print "false"
      | Num i =>
        print (Int.toString(i))
      | String s =>
        let val _ = print ("\""^s^"\"") in () end
      | Tuple xs =>
        let val _ = print "("
            val _ = printlist(xs, print, ", ", false)
        in  print ")"
        end
      | Tag {label, name} =>
        let val _ = print "IN"
            val _ = print (Int.toString(label))
            val _ = print (" " ^ name)
        in () 
        end
      | FuncVal {env, cont, arg, fbody, freevars} =>
        let val _ = print ("fn ("^env^", "^cont^", "^arg^ ") => \n") 
            val _ = pp0 (fbody, space +1)
            val _ = print "\n"
      (*      val _ = printSpace space
            val _ = print "freevars=["
            val _ = List.app (fn x => (print x; print ", ")) freevars
            val _ = print "]\n"    *)
        in () 
        end

and pp0 (t: t, space: int) =
    case t
     of LetVal{name, value, body} =>
        let val _ = printSpace space
            val _ = print ("let val " ^ name ^ " = ")
            val _ = ppv (value, space +1)
            val _ = print "\n"
            val _ = printSpace space
            val _ = print "in\n"
            val _ = pp0 (body, space +1)
            val _ = printSpace space
            val _ = print "end\n"
        in ()
        end
      | Let{name, index, tuple, body} =>
        let val _ = printSpace space
            val _ = print ("let val " ^ name ^ " = #")
            val _ = print (Int.toString(index))
            val _ = print (" " ^ tuple ^ "\n")
            val _ = printSpace space
            val _ = print ("in\n")
            val _ = pp0 (body, space +1)
            val _ = printSpace space
            val _ = print "end\n"
        in ()
        end
      | LetCont{cont, env, arg, fbody, body, freevars} =>
        let val _ = printSpace space
            val _ = print ("let fun "^cont^"("^env^", "^arg^") =\n")
            val _ = pp0 (fbody, space +1)
            val _ = printSpace space
            val _ = print "in\n"
            val _ = pp0 (body, space +1)
            val _ = printSpace space
            val _ = print "end\n"
     (*       val _ = printSpace space
            val _ = print "freevars=["
            val _ = List.app (fn x => (print x; print ", ")) freevars
            val _ = print "]\n"  *)
        in ()
        end
      | ContApp{cont, env, arg} =>
        let val _ = printSpace space
            val _ = print (cont^ "("^env^", " ^arg^ ")\n")
        in ()
        end
      | FuncApp{func, env, cont, arg} =>
        let val _ = printSpace space
            val _ = print (func^" ("^env^", " ^cont^ ", "^arg^ ")\n")
        in () end
      | Case{cond, branches} =>
        let val _ = printSpace space
            val _ = print ("case " ^cond^ " of\n")
            val _ = printlist (branches
                             , fn (i, z, t) =>
                                  (printSpace (space+1)
                                 ; print ("IN"^Int.toString(i)
                                          ^" "^z^" =>\n")
                                 ; pp0(t, space +1))
                             , " |\n"
                             , false)
            val _ = print(" )")
        in ()
        end
      | LetPrim {name, opr, args = [x1], body} =>
        let val _ = printSpace space
            val _ = print ("let val " ^name^ " = ")
            val _ = PrimOp.pp opr
            val _ = print ("(" ^ x1^ ")\n")
            val _ = printSpace space
            val _ = print "in\n"
            val _ = pp0 (body, space +1)
            val _ = printSpace space
            val _ = print "end\n"
        in ()
        end
      | LetPrim {name, opr, args = [x1, x2], body} =>
        let val _ = printSpace space
            val _ = print ("let val " ^name^ " = "^x1^" ")
            val _ = PrimOp.pp opr
            val _ = print (" "^ x2 ^"\n")
            val _ = printSpace space
            val _ = print "in\n"
            val _ = pp0 (body, space +1)
            val _ = printSpace space
            val _ = print "end\n"
        in ()
        end
      | If {cond, thenn, elsee} =>
        let val _ = printSpace space
            val _ = print ("if " ^cond^ "\n")
            val _ = printSpace space
            val _ = print ("then\n")
            val _ = pp0 (thenn, space +1)
            val _ = printSpace space
            val _ = print ("else\n")
            val _ = pp0 (elsee, space +1)
        in ()
        end
      | LetFix{func, env, cont, arg, fbody, body, freevars} =>
        let val _ = printSpace space
            val _ = print ("let fun "^func^"(" ^env^ ", "^cont^", " ^arg^ ") =\n")
            val _ = pp0 (fbody, space +1)
            val _ = printSpace space
            val _ = print "in\n"
            val _ = pp0 (body, space +1)
            val _ = printSpace space
            val _ = print "end\n"
     (*       val _ = printSpace space
            val _ = print "freevars=["
            val _ = List.app (fn x => (print x; print ", ")) freevars
            val _ = print "]\n"   *)
        in ()
        end
      | Exit x => 
        let val _ = printSpace space
        in print ("exit " ^ x^"\n")
        end
      | _ => raise NoRule; 

fun pp ( t) = 
    let val _ = out := NONE
    in  pp0(t, 0)
    end

fun dump2file (t: t, fname: string) =
    let val out' = TextIO.openOut (fname)
        val _ = out:= SOME out'
        val _ = TextIO.output(out',"datatype ('a, 'b) s = IN1 of 'a | IN2 of 'b\n")
        val _ = TextIO.output(out',"fun exit x = ()\n")
        val _ = TextIO.output(out', "val closure_code =\n")
        val _ = pp0(t, 0)
        val _ = TextIO.flushOut (out')
        val _ = out := NONE
        val _ = TextIO.closeOut(out')
    in ()
    end

end

