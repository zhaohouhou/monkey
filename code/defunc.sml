structure Defunc: DEFUNC = 
struct

exception NoRule

val out: TextIO.outstream option ref = ref NONE
fun print (s) =
    case !out 
     of SOME out' => 
        TextIO.output(out', s)
      | NONE => 
        TextIO.output(TextIO.stdOut, s)

fun 'a printlist (l: 'a list, p: 'a->unit, sep: string, afterlast: bool) =
    let fun doit l =
            case l
             of [] => ()
              | [x] => (p x; if afterlast then print sep else ())
              | x::xs => (p x; print sep; doit xs)
    in  doit l
    end

fun toString (i: int) =
    if i >= 0
    then Int.toString i
    else ("A"^(Int.toString(i))) (*to identify*)

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
  | AppFunc of {func: string  
              , cont: string
              , arg: string }
  (*apply_func (f, cont, arg)*)
  | AppCont of {cont: string
              , arg: string }
  (*apply_cont (cont, arg)*)
  | Case of {cond: string
           , branches: (int * string * t) list} 
  | LetPrim of {name: string
              , opr: PrimOp.t
              , args: string list
              , body: t}
  | If of {cond: string
         , thenn: t
         , elsee: t} 
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
       | Proj of {index: int
                , tuple: string}
                 
datatype applycont
  = ApplyCont of {cont: string
               , arg: string
               , body: t}
                
datatype applyfunc
  = ApplyFunc of {fname: string
               , cont: string
               , arg: string
               , body: t}
      
datatype program
      = Program of {applycont: applycont
                  , applyfunc: applyfunc
                  , main: t}

fun printSpace (t: int) =
    if 0 = t
    then ()
    else let val _ = print "    "
         in printSpace (t-1)
         end

fun dumpv (t: v, space : int) =
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
            val _ = print (toString(label))
            val _ = print (" " ^ name)
        in () 
        end
      | Proj {index, tuple} =>
        let val _ = print "#"
            val _ = print (Int.toString(index))
            val _ = print (" " ^ tuple)
        in () 
        end


and dumpt (t: t, space : int): unit =
    case t
     of LetVal {name, value, body} =>
        let val _ = printSpace space
            val _ = print ("let val " ^ name ^ " = ")
            val _ = dumpv (value, space +1)
            val _ = print "\n"
            val _ = printSpace space
            val _ = print "in\n"
            val _ = dumpt (body, space +1)
            val _ = printSpace space
            val _ = print "end\n"
        in ()
        end
      | AppFunc {func, cont, arg} =>
        let val _ = printSpace space
            val _ = print (concat["apply_func("
                                , func , ", "
                                , cont , ", "
                                , arg , ")\n"])
        in ()
        end
      (*apply_func (f, cont, arg)*)
      | AppCont {cont, arg} =>
        let val _ = printSpace space
            val _ = print (concat["apply_cont("
                                , cont , ", "
                                , arg , ")\n"])
        in ()
        end
      (*apply_cont (cont, arg)*)
      | Case{cond, branches} =>
        let val _ = printSpace space
            val _ = print ("(case " ^cond^ " of\n")
            val _ = printlist (branches
                             , fn (i, z, t) =>
                                  (printSpace (space+1)
                                 ; print ("IN"^toString(i)
                                          ^" "^z^" =>\n")
                                 ; dumpt(t, space +1))
                             , " |\n"
                             , false)
            val _ = print(" )\n") (*add "()" to be safe*)
        in ()
        end
      | LetPrim {name, opr, args = [x1], body} =>
        let val _ = printSpace space
            val _ = print ("let val " ^name^ " = ")
            val _ = PrimOp.pp opr
            val _ = print ("(" ^ x1^ ")\n")
            val _ = printSpace space
            val _ = print "in\n"
            val _ = dumpt (body, space +1)
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
            val _ = dumpt (body, space +1)
            val _ = printSpace space
            val _ = print "end\n"
        in ()
        end
      | If {cond, thenn, elsee} =>
        let val _ = printSpace space
            val _ = print ("if " ^cond^ "\n")
            val _ = printSpace space
            val _ = print ("then\n")
            val _ = dumpt (thenn, space +1)
            val _ = printSpace space
            val _ = print ("else\n")
            val _ = dumpt (elsee, space +1)
        in ()
        end
      | Exit x => 
        let val _ = printSpace space
        in print ("exit " ^ x^"\n")
        end
      | _ => raise NoRule; 
          
fun dumpApplyCont (ApplyCont{cont, arg, body}) =
    case body
     of Case{cond, branches = []} =>
        () (*not likely, though*)
      | _ => (print (concat["fun apply_cont ("
                          , cont
                          , ", "
                          , arg
                          , ") =\n"])
            ;dumpt (body, 0))

fun dumpApplyFunc (ApplyFunc{fname, cont, arg, body}) =
   case body
     of Case{cond, branches = []} =>
        ()
      | _ => (print (concat["fun apply_func ("
                          , fname
                          , ", "
                          , cont
                          , ", "
                          , arg
                          , ") =\n"])
            ; dumpt (body, 0))

fun dump2file  (Program{applycont, applyfunc, main}, fname: string) =
    let val out' = TextIO.openOut (fname)
        val _ = out:= SOME out'
   (* val _ = print ("datatype ('a, 'b) s = IN1 of 'a | IN2 of 'b\n") *)
        val _ = print ("fun exit x = ()\n")
        val _ = dumpApplyFunc applyfunc
        val _ = print "\n\n"
        val _ = dumpApplyCont applycont
        val _ = print "\n\n"
        val _ = print ("val defunc_code =\n")
        val _ = dumpt(main, 0)
        val _ = TextIO.flushOut (out')
        val _ = out := NONE
        val _ = TextIO.closeOut(out')
    in ()
    end

fun pp (Program{applycont, applyfunc, main}) = 
    let val _ = out := NONE
        val _ = dumpApplyFunc applyfunc
        val _ = dumpApplyCont applycont
    in  dumpt(main, 0)
    end

end
