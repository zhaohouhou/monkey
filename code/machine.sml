structure Machine: MACHINE =
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

fun toStringC i =
    if i >= 0
    then Int.toString i
    else ("-"^ (Int.toString (0-i)))

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


structure Binding =
struct

datatype v
  = Null
  | True
  | False
  | Fetch of int * string
  | Num of int
  | String of string
  | AllocTuple of {numFields: int}
  | Prim of PrimOp.t * string list
  | AllocTag of {tag: int}
                
datatype t
  = Bind of {var: string, binding: v}
  | Init of {dst: string, index: int, src: string} (* dst[index] = src *)
             
fun convertString s = (*won't be used if we read from file?*)
    let fun doit (l, r) =
            case l
             of [] => r
              | (#"\n")::xs => doit (xs, (#"n")::(#"\\")::r)
              | x::xs => doit (xs, x::r)
    in  implode (rev (doit (explode s, [])))
    end

fun dump2file' t =
    case t
     of Null => print "0"
      | True => print "1"
      | False => print "0"
      | Fetch (i, x) => print (concat ["(ty_int_ptr)((ty_int_ptr)", x, ")[", toStringC i, "]"])
      | Num i => (print "(ty_int_ptr)"; print (toStringC i))
      | String s => print (concat ["(ty_int_ptr)\"", convertString s, "\""])
      | Prim (operator, args) =>
        (case operator
          of PrimOp.Print =>
             print (concat ["print(", hd args, ")"])
           | PrimOp.Add =>
             print (concat ["((int)", hd args," +(int)", hd (tl args), ")"])
           | PrimOp.Sub =>
             print (concat ["((int)", hd args," -(int)", hd (tl args), ")"])
           | PrimOp.Times =>
             print (concat ["((int)", hd args," *(int)", hd (tl args), ")"])
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
             print (concat ["!(int)", hd args]))
      | AllocTuple {numFields} => 
        (print (concat["allocTuple(", toStringC(numFields)])
       ; print ")")
      | AllocTag {tag} =>
        print (concat["allocTag(", toStringC tag, ")"])
           
fun dump2file t =
    case t
     of Bind {var, binding} =>
        (print "ty_int_ptr "; print var; print " = "; dump2file' binding)
      | Init {dst, index, src} =>
        (print dst; print"[(int)"; print (toStringC index); print "] = (int)"; print src)

end (* Binding *)



structure Block =
struct
datatype exp
  = Call of {func: string
           , arg: string}
  | If of {cond: string
         , thenn: t
         , elsee: t}
  | Case of {cond: string
           , branches: (int * string * t) list}
  | Exit of string
            
     and t
       = Block of {bindings: Binding.t list, exp: exp}
            
fun dump2file' (e: exp) =
    case e
     of Call {func, arg} =>
        (print "((ty_fun_ptr)"
       ; print func
       ; print ")("
       ; print arg
       ; print ");")
      | If {cond, thenn, elsee} =>
        let val _ = print (concat ["if(", cond, "){"])
            val _ = dump2file thenn
            val _ = print "\n}else{"
            val _ = dump2file elsee 
            val _ = print "}\n"
        in  ()
        end
      | Case {cond, branches} =>
        if (List.length branches) = 0
        then () (*the apply_func and apply_cont may contain empty cases*)
        else  let val _ = printlist(branches
                                  , fn (i, x, t) =>
                                       print (concat ["void *"
                                                    , x, "=(void *)((int *)"
                                                    , cond, ")[1];\n"])
                                  , "", false)
                  val _ = print (concat["switch(((int*)", cond, ")[0]){\n"])
                  val _ = printlist(branches
                            , fn (i, x, t) =>
                                 (print (concat["case ", toStringC i
                                              , ":{\n"]); dump2file(t))
                                  , ";\n}\n", true)
                  val _ = print "}\n"
              (*"default" and "break"s are omitted, since a branch will not return*)
              in  ()
              end
      | Exit x => (print "ml_exit ("; print x; print ");")

and dump2file (Block{bindings, exp}) =
    let val _ = printlist (bindings
                         , fn b => (print "  "; Binding.dump2file b)
                         , ";\n"
                         , true)
        val _ = print "\n  "
        val _ = dump2file' exp
        val _ = print "\n  "
    in ()
    end

end (* Block *)


structure Function =
struct

datatype t
  = T of {name: string
        , arg: string
        , bindings: Binding.t list
        , body: Block.t}

fun dump2file (T{name, arg, bindings, body}) =
    let val _ = print (String.concat ["void ", name, "("])
        val _ = if arg=""
                then () 
                else (print "void *"; print arg)
        val _ = print (")\n{\n")
        val _ = printlist (bindings
                         , fn b => (print "  "; Binding.dump2file b)
                         , ";\n"
                         , true)
        val _ = print "\n  "
        val _ = Block.dump2file body
        val _ = print "\n}\n\n"
    in  ()
    end 
end (* Function *)
         
structure Program =
struct

datatype t
  = T of {main: Function.t
        , funcs: Function.t list
        , declares: string list}
       
fun printDeclares fnames =
    case fnames
     of [] => ()
      | fname::fs =>
        (print (concat["void ", fname, "(void *);\n"])
       ; printDeclares fs)

fun dumpincludes () =
    print "#include <stdio.h>\n#include <stdlib.h>\n#include \"runtime_nogc.h\"\n\n"

fun dumpmain () =
    print ("int main ()\n{\n  ml_main ();\n  return 0;\n}\n\n")

fun dump2file (T{main, funcs, declares}, fname) =  
    let val out' = TextIO.openOut (fname)
        val _ = out:= SOME out'
        val _ = print "// Compiler auto-generated.\n\n"
        val _ = dumpincludes()
        val _ = print ("typedef void (*ty_fun_ptr)();\ntypedef int *ty_int_ptr;\n\n")
        val _ = printDeclares declares
        val _ = print ("void ml_exit (void *x)\n{\n  exit (0);\n}\n\n")(*printf (\"%d\\n\", (int)x);*)
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
