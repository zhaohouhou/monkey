structure Flat: FLAT =
struct

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

structure PrimOp =
struct
datatype t
  = Add
  | Sub
  | Times
  | Print
  | Int2String
    
end (* Prim *)

structure Exp =
struct

datatype t
  = Call of {func: string
           , args: string list}
  | If0 of {cond: string
          , thenn: t
          , elsee: t}
  | Case of {cond: string
           , thenarg: string
           , thenn: t
           , elsearg: string
           , elsee: t} (*keep x1&x2 for now*)
  | Exit of string

fun dump2file (t) =
    case t
     of Call {func, args} =>
        (print "((void * (*)())"
       ; print func
       ; print ")("
       ; printlist (args, print, ", ", false)
       ; print ")")
      | If0 {cond, thenn, elsee} =>
        let val _ = print (concat ["if(0==", cond, "){"])
            val _ = dump2file thenn
            val _ = print ";}else{"
            val _ = dump2file elsee 
            val _ = print ";}"
        (*thenn and elsee will be call and will not return*)
        in  ()
        end
      | Case {cond, thenarg, thenn, elsearg, elsee} =>
        let val _ = print (concat ["void *", thenarg, "=(void *)((int *)"
                                 , cond, ")[1];\n"])
            val _ = print (concat ["void *", elsearg, "=(void *)((int *)"
                                 , cond, ")[1];\n"])
            val _ = print (concat ["if(((int *)", cond, ")[0]==1){\n"])
            val _ = dump2file thenn
            val _ = print (concat [";}\nelse if(((int *)", cond, ")[0]==2){\n"])
            val _ = dump2file elsee 
            val _ = print ";}\n"
            val _ = print ("else printf(\"CASE is wrong!!!\");\n")
        in  ()
        end
      | Exit x => print "exit (0)"

end (* Exp *)

structure Binding =
struct

datatype t
  = Empty
  | Proj of int * string
  | Num of int
  | String of string
  | Tuple of string list
  | Prim of PrimOp.t * string list 
  | Tag of int * string            
             
fun convertString s = (*won't be used if we read from file?*)
    let fun doit (l, r) =
            case l
             of [] => r
              | (#"\n")::xs => doit (xs, (#"n")::(#"\\")::r)
              | x::xs => doit (xs, x::r)
    in  implode (rev (doit (explode s, [])))
    end

fun dump2file t =
    case t
     of Empty => print "0" (*empty, for now*)
      | Proj (i, x) => print (concat ["((int *)", x, ")[", Int.toString i, "]"])
      (*index 0 is saved*)
      | Num i => print (Int.toString i)
      | String s => print (concat ["\"", convertString s, "\""])
      | Prim (operator, args) =>
        (case operator
          of PrimOp.Print => print (concat ["print(", hd args, ")"])
           | PrimOp.Add => print (concat ["(int)", hd args," +(int)", hd (tl args)])
           | PrimOp.Sub => print (concat ["(int)", hd args," -(int)", hd (tl args)])
           | PrimOp.Times => print (concat ["(int)", hd args," *(int)", hd (tl args)])
           | PrimOp.Int2String => print (concat ["int2string (", hd args, ")"]))
            (*need library*)
      | Tuple l => 
        (case List.length(l)
          of 0 => print "0"
           | len =>
             (print (concat["allocTuple(", Int.toString(len), ", "])
            ; printlist (l, print, ", ", false)
            ; print ")"))
      | Tag (i, x) =>
        print (concat["allocTag(", Int.toString(i), ", ", x, ")"])
           

end (* Binding *)

structure Function =
struct

datatype t
  = T of {name: string
        , args: string list
        , bindings: (string * Binding.t) list
        , body: Exp.t}

fun dump2file (T{name, args, bindings, body}) =
    let val _ = print (String.concat ["void ", name, "("])
        val _ = if List.length(args) = 0
                then ()
                else print ("void *")
        val _ = printlist (args, print, ", void *", false)
        val _ = print (")\n{\n")
        val _ = printlist (bindings
                         , (fn (x, b) => (print "  void *"
                                        ; print x
                                        ; print " = (void *)"
                                        ; Binding.dump2file b))
                         , ";\n"
                         , true)
        val _ = print "\n  "
        val _ = Exp.dump2file body
        val _ = print ";\n}\n\n"
    in  ()
    end 
end (* Function *)
         
structure Program =
struct

datatype t
  = T of {main: Function.t
        , funcs: Function.t list}
       
fun dumpincludes () =
    print "#include <stdio.h>\n#include <stdlib.h>\n#include <stdarg.h>\n#include \"runtime_flat.h\"\n\n"

fun dumpmain () =
    print ("int main ()\n{\n  ml_main ();\n  return 0;\n}\n\n")

fun dump2file (T{main, funcs}, fname) =  
    let val out' = TextIO.openOut (fname)
        val _ = out:= SOME out'
        val _ = print "// Compiler auto-generated.\n\n"
        val _ = dumpincludes()
        val _ = printlist (funcs, Function.dump2file, "\n", true)
        val _ = Function.dump2file main
        val _ = dumpmain ()
        val _ = TextIO.flushOut (out')
        val _ = out := NONE
        val _ = TextIO.closeOut(out')
    in ()
    end

end (* Program *)

end (* Flat *)
