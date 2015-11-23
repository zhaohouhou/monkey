structure CodegenDefunc: CODEGENDEFUNC =
struct

fun fetchArgs (newArg, args, acc, index) =
    case args
     of [] => List.rev acc
      | x::xs => 
        fetchArgs (newArg
                 , xs
                 , Machine.Binding.Bind
                       {var = x
                      , binding = 
                        Machine.Binding.Fetch(index, newArg)}::acc
                 , index+1)
        
fun funArgInit (dst, args, acc, index) = 
    case args
     of [] => List.rev acc
      | x::xs => 
        funArgInit (dst
                  , xs
                  , (Machine.Binding.Init{dst = dst
                                        , index = index
                                        , src = x})::acc
                  , index+1)
        
fun transOpt opt =
    case opt
     of Defunc.PrimOp.Print => Machine.PrimOp.Print
      | Defunc.PrimOp.Sub => Machine.PrimOp.Sub
      | Defunc.PrimOp.Add => Machine.PrimOp.Add
      | Defunc.PrimOp.Times => Machine.PrimOp.Times
      | Defunc.PrimOp.Int2String => Machine.PrimOp.Int2String
      | Defunc.PrimOp.LessThan => Machine.PrimOp.LessThan
      | Defunc.PrimOp.LargerThan => Machine.PrimOp.LargerThan
      | Defunc.PrimOp.Equals => Machine.PrimOp.Equals
      | Defunc.PrimOp.AndAlso => Machine.PrimOp.AndAlso
      | Defunc.PrimOp.OrElse => Machine.PrimOp.OrElse
      | Defunc.PrimOp.Not => Machine.PrimOp.Not

fun transOneBinding (x:string, v: Defunc.v):
    (Machine.Binding.t list) = 
    case v
     of Defunc.Empty =>
        [Machine.Binding.Bind {var = x
                             , binding = Machine.Binding.Null}]
      | Defunc.True =>
        [Machine.Binding.Bind {var = x
                             , binding = Machine.Binding.True}]
      | Defunc.False =>
        [Machine.Binding.Bind {var = x
                             , binding = Machine.Binding.False}]
      | Defunc.Num i =>
        [Machine.Binding.Bind {var = x
                             , binding = Machine.Binding.Num i}]
      | Defunc.String s =>
        [Machine.Binding.Bind {var = x
                             , binding = Machine.Binding.String s}]
      | Defunc.Tuple l =>
        (case l (*might be empty*)
          of [] => 
             [Machine.Binding.Bind {var = x
                                  , binding = Machine.Binding.Null}]
           | _ =>  
             funArgInit(x
                      , l
                      , [Machine.Binding.Bind 
                             {var = x
                            , binding = 
                              Machine.Binding.AllocTuple 
                                  {numFields=
                                   List.length l}}]
                      , 1)
        )
      | Defunc.Tag {label, name} =>
        [Machine.Binding.Bind {var = x
                             , binding = Machine.Binding.AllocTag
                                             {tag=label}}
       , Machine.Binding.Init {dst=x, index=1, src=name}]
      | Defunc.Proj {index, tuple} =>
        [Machine.Binding.Bind {var = x
                             , binding = 
                               Machine.Binding.Fetch (index, tuple)}]

fun convert (t: Defunc.t, acc):Machine.Block.t =
    case t
     of Defunc.LetVal{name, value, body} =>
        let val bindings = transOneBinding(name, value)
        in convert(body, acc@bindings)
        end
      | Defunc.AppFunc {func, cont, arg} =>
        let val newVar = Temp.freshVal()
        in Machine.Block.Block
           {bindings =
            acc@(Machine.Binding.Bind{var=newVar
                                    , binding =
                                      Machine.Binding.AllocTuple
                                          {numFields = 3}}
                 :: rev (funArgInit (newVar, [func, cont, arg], [], 1)))
          , exp = Machine.Block.Call{func = "apply_func"
                                   , arg = newVar}}
        end
      | Defunc.AppCont {cont, arg} =>
        let val newVar = Temp.freshVal()
        in Machine.Block.Block
           {bindings =
            acc@(Machine.Binding.Bind{var=newVar
                                    , binding =
                                      Machine.Binding.AllocTuple
                                          {numFields = 2}}
                 :: rev (funArgInit (newVar, [cont, arg], [], 1)))
          , exp = Machine.Block.Call{func = "apply_cont"
                                   , arg = newVar}}
        end
      | Defunc.Case {cond, branches} =>
        let val cases = transCases(branches, [])
        in Machine.Block.Block{bindings = acc
                             , exp = Machine.Block.Case
                                         {cond = cond
                                        , branches = cases}}
        end
      | Defunc.LetPrim {name, opr, args, body} =>
        let val bind = Machine.Binding.Bind
                           {var = name
                          , binding =
                            Machine.Binding.Prim(transOpt opr
                                               , args)}
        in convert(body, acc@[bind])
        end
      | Defunc.If {cond, thenn, elsee} =>
        Machine.Block.Block{bindings = acc
                          , exp = Machine.Block.If
                                      {cond = cond
                                     , thenn = convert(thenn, [])
                                     , elsee = convert(elsee, [])}}
      | Defunc.Exit x =>
        Machine.Block.Block{bindings = acc
                          , exp = Machine.Block.Exit x}

and transCases (cases, acc) =
    (case cases
      of [] => List.rev acc
       | (i, z, t)::cs =>
         transCases(cs, (i, z, convert(t, []))::acc)
    )

fun trans p: Machine.Program.t =
    case p
     of Defunc.Program{applycont=
                       Defunc.ApplyCont{cont = cont1
                                      , arg = arg1
                                      , body = body1}
                     , applyfunc=
                       Defunc.ApplyFunc{fname=func
                                      , cont = cont2
                                      , arg = arg2
                                      , body = body2}
                     , main} =>
        let val body1' = convert(body1, [])
            val body2' = convert(body2, [])
            val env1 = Temp.freshVal()
            val env2 = Temp.freshVal()
            val main'= convert (main, [])
            val f1 = Machine.Function.T
                         {name = "apply_cont"
                        , arg = env1
                        , bindings = fetchArgs(env1
                                              , [cont1, arg1]
                                              , []
                                              , 1)
                        , body = body1'}
            val f2 = Machine.Function.T
                         {name = "apply_func"
                        , arg = env2
                        , bindings = fetchArgs(env2
                                              , [func, cont2, arg2]
                                              , []
                                              , 1)
                        , body = body2'}
            val mainf = Machine.Function.T
                            {name = "ml_main"
                           , arg = ""
                           , bindings = []
                           , body = main'}
        in Machine.Program.T{main = mainf
                           , funcs = [f1, f2]
                           , declares = ["apply_func"
                                       , "apply_cont"]}
        end

end
