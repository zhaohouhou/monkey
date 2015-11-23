structure GcTime: GC =
struct

val funcSize = ref 0;
val funcMap: (string * int) list ref = ref [];

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

fun convertString s = 
    let fun doit (l, r) =
            case l
             of [] => r
              | (#"\n")::xs => doit (xs, (#"n")::(#"\\")::r)
              | x::xs => doit (xs, x::r)
    in  implode (rev (doit (explode s, [])))
    end

fun dumpValue (v) =
    case v
     of Machine.Binding.Null => print "0"
      | Machine.Binding.True => print "1"
      | Machine.Binding.False => print "0"
      | Machine.Binding.Fetch (i, x) =>
        print (concat ["(ty_int_ptr)((ty_int_ptr)", x, ")["
                     , toStringC i, "]"])
      | Machine.Binding.Num i => 
        print (concat["(ty_int_ptr)((", toStringC i
                    , "<<1)|1)"])
      | Machine.Binding.String s => 
        print (concat ["(ty_int_ptr)\"", convertString s, "\""])
      | Machine.Binding.Prim (operator, args) =>
        (case operator
          of Machine.PrimOp.Print =>
             print (concat ["(ty_int_ptr)ml_print((const char *)"
                          , hd args, ")"])
           | Machine.PrimOp.Add =>
             print (concat ["(ty_int_ptr)ml_add((int)"
                          , hd args, ", (int)", hd (tl args), ")"])
           | Machine.PrimOp.Sub => 
             print (concat ["(ty_int_ptr)ml_sub((int)"
                          , hd args," , (int)", hd (tl args), ")"])
           | Machine.PrimOp.Times =>
             print (concat ["(ty_int_ptr)ml_times((int)"
                          , hd args, ", (int)", hd (tl args), ")"])
           | Machine.PrimOp.LessThan =>
             print (concat ["(ty_int_ptr)ml_lessThan((int)"
                          , hd args, ", (int)", hd (tl args), ")"])
           | Machine.PrimOp.LargerThan => 
             print (concat ["(ty_int_ptr)ml_largerThan((int)"
                          , hd args," , (int)", hd (tl args), ")"])
           | Machine.PrimOp.Equals =>
             print (concat ["((int)"
                          , hd args, " ==(int)", hd (tl args), ")"])
           (*operation on boolean values: *)
           | Machine.PrimOp.AndAlso =>
             print (concat ["((int)"
                          , hd args, " &&(int)", hd (tl args), ")"])
           | Machine.PrimOp.OrElse =>
             print (concat ["((int)"
                          , hd args, " ||(int)", hd (tl args), ")"])
           | Machine.PrimOp.Not =>
             print (concat ["!(int)" , hd args])
           | Machine.PrimOp.Int2String =>
             print (concat ["(ty_int_ptr)ml_int2string ((int)"
                          , hd args, ")"]))
      | Machine.Binding.AllocTuple {numFields} => 
        (funcSize := (!funcSize + (numFields+2)) (*forwarding+length*)
       ; print (concat["Gc_allocTuple(", toStringC(numFields)])
       ; print ")")
      | Machine.Binding.AllocTag {tag} =>
        (funcSize := (!funcSize + 3) (*tag+value+forwarding*)
       ; print (concat["Gc_allocTag(", toStringC tag, ")"]))

fun dumpBinding (t: Machine.Binding.t) = 
    case t
     of Machine.Binding.Bind {var, binding} =>
        if var = "real_env" (*a hack ...*)
        then (print var; print " = "; dumpValue binding)
        else (print "ty_int_ptr ";
              print var; print " = "; dumpValue binding)
      | Machine.Binding.Init {dst, index, src} =>
        (print dst; print"[(int)";
         print (Int.toString index); print "] = (int)"; print src)


fun dumpExp (e: Machine.Block.exp) =
    case e
     of Machine.Block.Call {func, arg} =>
        print (concat["_f = ", func, ";\n"
                    , "  _arg = ", arg, ";\n"])
      | Machine.Block.If {cond, thenn, elsee} =>
        let val _ = print (concat ["if(", cond, "){"])
            val _ = dumpBlock thenn
            val _ = print "\n}else{"
            val _ = dumpBlock elsee 
            val _ = print "}\n"
        in  ()
        end
      | Machine.Block.Case {cond, branches} =>
        let val _ = printlist(branches
                            , fn (i, x, t) =>
                                 print (concat ["void *"
                                              , x, "=(void *)((int *)"
                                              , cond, ")[1];\n"])
                            , "", false)
            val _ = print (concat["switch(((int*)", cond, ")[0]>>1){\n"])
            val _ = printlist(branches
                            , fn (i, x, t) =>
                                 (print (concat["case ", toStringC i
                                              , ":\n{"]); dumpBlock(t))
                            , "return;\n}\n", true)
            val _ = print "}\n"
        (*here the "return" statement is needed!*)
        in  ()
        end
      | Machine.Block.Exit x => (print "ml_exit ("; print x; print ");")

and dumpBlock (Machine.Block.Block{bindings, exp}) =
    let val _ = printlist (bindings
                         , fn b => (print "  "; dumpBinding b)
                         , ";\n"
                         , true)
        val _ = print "\n  "
        val _ = dumpExp exp
        val _ = print "\n  "
    in ()
    end

fun dumpFunc (Machine.Function.T{name, arg, bindings, body}) = 
    let val _ = print (String.concat ["void ", name, "("])
        val _ = if arg=""
                then () 
                else (print "void *"; print arg)
        val _ = print (")\n{\n")
        val _ = funcSize:= 0
        val _ = printlist (bindings
                         , fn b => (print "  "; dumpBinding b)
                         , ";\n"
                         , true)
        val _ = print "\n  "
        val _ = dumpBlock body
        val _ = print "\n}\n"
        val _ = funcMap := ((name, !funcSize)::(!funcMap))
    in  ()
    end 


fun dumpincludes () =
    print "#include <stdio.h>\n#include <stdlib.h>\n#include <Windows.h>\n#include \"runtime.h\"\n\n"

fun dumpmain () =
    let val _ = print (concat["int main (int argc,char *argv[])\n{\n"
                            ,"  _f = ml_main;\n"
                            ,"  _arg = 0;\n"
							,"QueryPerformanceCounter(&start);\n"
                            ,"  if(argv[1]==0){Gc_init(2000);}\n" (*default heap: 2000B*)
                            ,"  else{Gc_init(String2Int(argv[1]));}\n"
                            ,"  Gc_newMap();\n"])
        val _ = printlist(!funcMap
                        , fn (fname, size)=> 
                             print(concat["  Gc_insertMap("
                                        , fname
                                        , ", "
                                        , Int.toString(size)
                                        , ");"])
                        , "\n", true)
         
        val _ = print (concat["  while(1){\n"
                            ,"    Gc_retrieve(Gc_lookupMap(_f));\n"
                            ,"    ((ty_fun_ptr)_f)(_arg);\n  }\n}\n\n"])
    in ()
    end
                
fun dumpGlobal () =
   print (concat["ty_int_ptr _arg;\nty_int_ptr _f;\n\n"
   ,"ty_int_ptr real_env;\n"
	,"LARGE_INTEGER start;\n", "LARGE_INTEGER end ;\n"
	])

fun printDeclares fnames =
    case fnames
     of [] => ()
      | fname::fs =>
        (print (concat["void ", fname, "(void *);\n"])
       ; printDeclares fs)

fun dumpProg (Machine.Program.T{main, funcs, declares}, fname) =
    let val out' = TextIO.openOut (fname)
        val _ = out:= SOME out'
        val _ = funcMap := []
        val _ = print "// Compiler auto-generated.\n\n"
        val _ = dumpincludes()
        val _ = print ("typedef void (*ty_fun_ptr)();\ntypedef int *ty_int_ptr;\n\n")
        val _ = dumpGlobal()
        val _ = printDeclares declares
        val _ = print (concat["void ml_exit (void *x)\n{\n"
		," QueryPerformanceCounter(&end);\n"
		," printf(\"time used:%d\\n\", (int)(end.QuadPart - start.QuadPart));\n"
  ," exit (0);\n}\n\n"])
        val _ = printlist (funcs, dumpFunc, "\n", true)
        val _ = dumpFunc main
        val _ = dumpmain ()
        val _ = TextIO.flushOut (out')
        val _ = out := NONE
        val _ = TextIO.closeOut(out')
    in ()
    end

end
