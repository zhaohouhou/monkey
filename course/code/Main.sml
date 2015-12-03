structure Main =
struct

fun lambdak (x: string) = Cps.Exit x

fun main (p: MLAst.t, fname): unit =
    let val fname = concat ["test/", fname]
        val _ = print "Monkey starting...\n\n"

        val _ = print "\tCPS conversion starting...\n"
        val cps = CpsConvert.trans(p)
        val _ = Cps.dump2file(cps, fname^".cps.sml")
        val _ = print "\tCPS conversion finished\n\n"

        val _ = print "\tClosure conversion starting...\n"
        val closure = ClosureConvert.trans(cps)
        val _ = Closure.dump2file(closure, fname^".closure.sml")                              
        val _ = print "\tClosure conversion finished\n\n"
                                      
        val _ = print "\tHoisting starting...\n"
        val flat = Hoist.trans(closure)
        val _ = Flat.Program.dump2file(flat, fname^".flat.c")
        val _ = print "\tHoisting finished\n\n"

        val _ = print "\tCode generation starting...\n"
        val code = Codegen.trans(flat)
        val _ = Machine.Program.dump2file(code, fname^".codegen.c")
        val _ = print "\tCode generation finished\n\n"

        val _ = print "\tCode generation for gc starting...\n"
        val _ = Gc.dumpProg(code, fname^".gc.c")
        val _ = print "\tCode generation for gc finished\n\n"

     (*   val _ = print "\tAssembling and linking starting...\n"
        val _ = OS.Process.system (concat ["gcc ", fname^".codegen.c", " test/runtime.c"])
        val _ = print "\tAssembling and linking finished\n\n"*)

        val _ = print "Monkey finished\n\n"
    in ()
    end

end
