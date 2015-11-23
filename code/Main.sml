structure Main =
struct

fun lambdak (x: string) = Cps.Exit x

val opt_defunc = ref 0
val opt_closure = ref 0
val opt_time = ref 0
val opt_all = ref 0

val args = CommandLine.arguments()

fun parseArgs (args) = 
    case args
     of [] => (raise Fail "no input file\n")
      | arg::[] => ()
      | arg::args =>
        let val _ = (if arg = "-defunc"  
                    then opt_defunc := 1
                    else if arg = "-closure"
                    then opt_closure := 1
                    else if arg = "-all"
                    then opt_all := 1
                    else if arg = "-t"
                    then opt_time := 1
                    else (let val _ = print "Invalid option.\nValid options: -defunc|-closure|-all|-t\n"
                          in raise Fail "invalid op"
                          end))
        in parseArgs args
        end  
        
(*
fun main () =
    let val _ = parseArgs args
        val fname = List.last args
        val path = OS.FileSys.fullPath fname
        val dir = OS.Path.dir path
        val ast = Parser.parse fname
        val _ = if !opt_defunc = 0 andalso !opt_closure = 0
                then opt_defunc := 1
                else ()

        val cps = CpsConvert.trans(ast)
        val _ = if !opt_all = 1
                then Cps.dump2file(cps, fname^".cps.sml")
                else ()
        val _ = if !opt_defunc = 1
                then let val defunc = Defunctional.trans(cps)
                         val code = CodegenDefunc.trans(defunc)
                         val _ = if !opt_time = 0
                                 then Gc.dumpProg(code, fname^".gcdefunc.c")
                                 else GcTime.dumpProg(code, fname^".gcdefunc.c")
                         val command = concat ["gcc -o ", path^"_defunc.exe ", path^".gcdefunc.c ", dir^"\\runtime.c"]
                         val _ = OS.Process.system (command)
                         val _ = if !opt_all = 1
                                 then let val _ = Defunc.dump2file(defunc, fname^".defunc.sml")
                                      in ()
                                      end
                                 else ()
                     in ()
                     end
                else ()

        val _ = if !opt_closure = 1
                then let val closure = ClosureConvert.trans(cps)
                         val flat = Hoist.trans(closure)
                         val code = Codegen.trans(flat)
                         val _ = if !opt_time = 0
                                 then Gc.dumpProg(code, fname^".gcclosure.c")
                                 else GcTime.dumpProg(code, fname^".gcclosure.c")
                         val command = concat ["gcc -o ", path^"_closure.exe ", path^".gcclosure.c ", dir^"\\runtime.c"]
                         val _ = OS.Process.system (command)
                         val _ = if !opt_all = 1
                                 then let val _ = Closure.dump2file(closure, fname^".closure.sml") 
                                          val _ = Flat.Program.dump2file(flat, fname^".flat.c")
                                      in ()
                                      end
                                 else ()
                     in ()
                     end
                else ()

        val _ = print "Monkey finished\n\n"

    in ()
    end
*)

fun mainWindows () =
    let val _ = parseArgs args
        val fname = List.last args
        val path = OS.FileSys.fullPath fname
        val dir = OS.Path.dir path
        val ast = Parser.parse fname
        val _ = if !opt_defunc = 0 andalso !opt_closure = 0
                then opt_defunc := 1
                else ()

        val cps = CpsConvert.trans(ast)
        val _ = if !opt_all = 1
                then Cps.dump2file(cps, fname^".cps.sml")
                else ()
        val _ = if !opt_defunc = 1
                then let val defunc = Defunctional.trans(cps)
                         val code = CodegenDefunc.trans(defunc)
                         val _ = if !opt_time = 0
                                 then Gc.dumpProg(code, fname^".gcdefunc.c")
                                 else GcTime.dumpProg(code, fname^".gcdefunc.c")
                         val command = concat [" -o ", path^"_defunc.exe ", path^".gcdefunc.c ", dir^"\\runtime.c"]
                         val _ = Windows.simpleExecute ("gcc", command)
                         val _ = if !opt_all = 1
                                 then let val _ = Defunc.dump2file(defunc, fname^".defunc.sml")
                                      in ()
                                      end
                                 else ()
                     in ()
                     end
                else ()

        val _ = if !opt_closure = 1
                then let val closure = ClosureConvert.trans(cps)
                         val flat = Hoist.trans(closure)
                         val code = Codegen.trans(flat)
                         val _ = if !opt_time = 0
                                 then Gc.dumpProg(code, fname^".gcclosure.c")
                                 else GcTime.dumpProg(code, fname^".gcclosure.c")
                         val command = concat [" -o ", path^"_closure.exe ", path^".gcclosure.c ", dir^"\\runtime.c"]
                         val _ = Windows.simpleExecute ("gcc", command)
                         val _ = if !opt_all = 1
                                 then let val _ = Closure.dump2file(closure, fname^".closure.sml") 
                                          val _ = Flat.Program.dump2file(flat, fname^".flat.c")
                                      in ()
                                      end
                                 else ()
                     in ()
                     end
                else ()

        val _ = print "Monkey finished\n\n"

    in ()
    end


val _ = main() (*need this to run*)

end

