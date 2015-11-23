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
    
end (* Prim *)

structure Exp =
struct

datatype t
  = Call of {func: string
           , args: string list}
  | If of {cond: string
         , thenn: t
         , elsee: t}
  | Case of {cond: string
           , branches: (int * string * t) list}
  | Exit of string

fun dump2file (t) =
    case t
     of Call {func, args} =>
        (print "((void * (*)())"
       ; print func
       ; print ")("
       ; printlist (args, print, ", ", false)
       ; print ")")
      | If {cond, thenn, elsee} =>
        let val _ = print (concat ["if(", cond, "){"])
            val _ = dump2file thenn
            val _ = print ";}else{"
            val _ = dump2file elsee 
            val _ = print ";}"
        in  ()
        end
      | Case {cond, branches} =>
        let val _ = printlist(branches
                            , fn (i, x, t) =>
                                 print (concat ["void *"
                                              , x, "=(void *)((int *)"
                                              , cond, ")[1];\n"])
                            , "", false)
            val _ = print (concat["switch(((int*)", cond, ")[0]){\n"])
            val _ = printlist(branches
                            , fn (i, x, t) =>
                                 (print (concat["case ", Int.toString  i
                                              , ":\n"]); dump2file(t))
                            , ";\n", true)
            val _ = print "}\n"
        (*"default" and "break"s are omitted, since a branch will not return*)
        in  ()
        end
      | Exit x => print "exit (0)"

end (* Exp *)

structure Binding =
struct

datatype t
  = Empty
  | True
  | False
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
      | True => print "1"
      | False => print "0"
      | Proj (i, x) => print (concat ["((int *)", x, ")[", Int.toString i, "]"])
      (*index 0 is saved*)
      | Num i => 
        if i < 0
        then print ("-"^(Int.toString(0-i)))
        else print (Int.toString i)
      | String s => print (concat ["\"", convertString s, "\""])
      | Prim (operator, args) =>
        (case operator
          of PrimOp.Print =>
             print (concat ["print(", hd args, ")"])
           | PrimOp.Add =>
             print (concat ["(int)", hd args," +(int)", hd (tl args)])
           | PrimOp.Sub =>
             print (concat ["(int)", hd args," -(int)", hd (tl args)])
           | PrimOp.Times =>
             print (concat ["(int)", hd args," *(int)", hd (tl args)])
           | PrimOp.Int2String =>
             print (concat ["int2string (", hd args, ")"])
           | PrimOp.LessThan =>
             print (concat ["((int)", hd args," <(int)", hd (tl args), ")"])
           | PrimOp.LargerThan =>
             print (concat ["((int)", hd args," >(int)", hd (tl args), ")"])
           | PrimOp.Equals =>
             print (concat ["((int)", hd args," ==(int)", hd (tl args), ")"])
           | PrimOp.AndAlso =>
             print (concat ["((int)", hd args," &&(int)", hd (tl args), ")"])
           | PrimOp.OrElse =>
             print (concat ["((int)", hd args," ||(int)", hd (tl args), ")"])
           | PrimOp.Not =>
             print (concat ["(!(int)", hd args, ")"]))
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
