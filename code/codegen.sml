structure Codegen: CODEGEN =
struct

fun transOperator t =
    case t
     of Flat.PrimOp.Add => Machine.PrimOp.Add
      | Flat.PrimOp.Sub => Machine.PrimOp.Sub
      | Flat.PrimOp.Times => Machine.PrimOp.Times
      | Flat.PrimOp.Print => Machine.PrimOp.Print
      | Flat.PrimOp.Int2String => Machine.PrimOp.Int2String
      | Flat.PrimOp.LessThan => Machine.PrimOp.LessThan
      | Flat.PrimOp.LargerThan => Machine.PrimOp.LargerThan
      | Flat.PrimOp.Equals => Machine.PrimOp.Equals
      | Flat.PrimOp.AndAlso => Machine.PrimOp.AndAlso
      | Flat.PrimOp.OrElse => Machine.PrimOp.OrElse
      | Flat.PrimOp.Not => Machine.PrimOp.Not

fun funArgInit (dst, args, acc, index) =
    case args
     of [] => acc
      | x::xs => 
        funArgInit (dst
                  , xs
                  , (Machine.Binding.Init{dst = dst
                                        , index = index
                                        , src = x})::acc
                  , index+1)
        
fun transExp (t: Flat.Exp.t): Machine.Block.t =
    case t
     of Flat.Exp.Call {func, args} =>
        let val size = List.length args
        in  case size
             of 0 => raise Fail "bug"
              | 1 => Machine.Block.Block{bindings =[]
                                       , exp = Machine.Block.Call {
                                         func = func
                                       , arg = hd args}}
              | _ => let val newVar = Temp.freshEnv()
                     in  Machine.Block.Block{
                         bindings = 
                         Machine.Binding.Bind{var=newVar
                                            , binding =
                                              Machine.Binding.AllocTuple
                                                  {numFields = size}}
                         :: rev (funArgInit (newVar, args, [], 1))
                       , exp = Machine.Block.Call{func = func
                                                , arg = newVar}}
                     end
        end
      | Flat.Exp.If {cond, thenn, elsee} =>
        Machine.Block.Block{ bindings = []
                           , exp = 
                             Machine.Block.If {cond = cond
                                             , thenn = transExp thenn
                                             , elsee = transExp elsee}}
      | Flat.Exp.Case {cond, branches} =>
        let fun transcases (cases, acc) = 
                (case cases
                  of [] => List.rev acc
                   | (i, x, e)::cs =>
                     transcases(cs, (i, x, transExp(e))::acc)) 
        in Machine.Block.Block{bindings = []
                             , exp = 
                               Machine.Block.Case{cond = cond
                                                , branches = 
                                                  transcases(branches, [])}}
        end
      | Flat.Exp.Exit x => 
        Machine.Block.Block{ bindings = []
                           , exp = Machine.Block.Exit x}

fun addInit (dst, args, acc, index) =
    case args
     of [] => acc
      | x::xs => 
        addInit (dst
               , xs
               , (Machine.Binding.Init {dst = dst
                                      , index = index
                                      , src = x})::acc
               , index+1)
        
fun transOneBinding ((x, b), acc) =
    case b
     of Flat.Binding.Empty => 
        (Machine.Binding.Bind {var = x
                            , binding = Machine.Binding.Null})::acc
      | Flat.Binding.True => 
        (Machine.Binding.Bind {var = x
                            , binding = Machine.Binding.True})::acc
      | Flat.Binding.False => 
        (Machine.Binding.Bind {var = x
                            , binding = Machine.Binding.False})::acc
      | Flat.Binding.Proj (i, y) =>
        (Machine.Binding.Bind {var = x
                            , binding = Machine.Binding.Fetch (i, y)})::acc
      | Flat.Binding.Num i =>
        (Machine.Binding.Bind {var = x
                            , binding = Machine.Binding.Num i})::acc
      | Flat.Binding.String s =>
        (Machine.Binding.Bind {var = x
                            , binding = Machine.Binding.String s})::acc
      | Flat.Binding.Tuple (list) =>
        (case list (*might be empty via free vars calculating*)
         of [] => (Machine.Binding.Bind {var = x
                                      , binding = Machine.Binding.Null})::acc
          | _ => addInit (x
                        , list
                        , (Machine.Binding.Bind {var = x
                                               , binding = 
                                                 Machine.Binding.AllocTuple 
                                                     {numFields=
                                                      List.length list}})
                          ::acc
                        , 1)
        )
      | Flat.Binding.Prim (operator, xs) =>
        (Machine.Binding.Bind {var = x
                             , binding =
                               Machine.Binding.Prim(transOperator operator, xs)})::acc
      | Flat.Binding.Tag (i, y) =>
        (Machine.Binding.Init {dst=x, index=1, src=y})
        ::(Machine.Binding.Bind {var = x
                               , binding = Machine.Binding.AllocTag {tag=i}})::acc
        

fun transBindings(l, acc) =
    case l
     of [] => acc
      | x::xs => transBindings (xs, transOneBinding (x, acc))

fun fetchArgs (newArg, l, r, index) =
    case l
     of [] => r
      | x::xs => 
        fetchArgs (newArg
                 , xs
                 , (x, Flat.Binding.Proj (index, newArg))::r
                 , index+1)
        
fun transFunction (Flat.Function.T {name
                                  , args
                                  , bindings
                                  , body}) =
    let val newArg = Temp.freshVal()
        val freshBindings =
            case args
             of [] => raise Fail name
              | [x] => []
              | _ => fetchArgs (newArg, args, [], 1)
        val newFreshBindings = transBindings (rev freshBindings, [])
        val newBindings = transBindings (bindings, newFreshBindings)
    in  Machine.Function.T {name = name
                          , arg = (case args
                                    of [] => raise Fail "bug"
                                     | [x] => x
                                     | _ => newArg)
                          , bindings = rev newBindings
                          , body = transExp body}
    end

fun transMain (Flat.Function.T {name
                              , args
                              , bindings
                              , body}) =
    let val newBindings = transBindings (bindings, [])
    in  Machine.Function.T {name = name
                          , arg = (case args
                                    of [] => ""
                                     | _ => raise Fail "bug")
                          , bindings = rev newBindings
                          , body = transExp body}
    end

fun transProgram (Flat.Program.T{funcs, main}) =
    Machine.Program.T 
        {funcs = List.map transFunction funcs
       , main = transMain main
       , declares = []}

fun trans (t) =
    transProgram t

end
