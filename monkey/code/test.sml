structure Test =
struct

val m_hello = MLAst.Prims (MLAst.PrimOp.Print
                         , [MLAst.String "hello, world\n"])

val m0 = MLAst.FuncVal("x", MLAst.Num 1)

val add = MLAst.LetFix("add", "x", MLAst.FuncVal("y", MLAst.Prims(MLAst.PrimOp.Add, [MLAst.Var("x"), MLAst.Var("y")])), MLAst.App(MLAst.App(MLAst.Var("add"), MLAst.Num(10)), MLAst.Num(5)))

val m1 = MLAst.App(
         MLAst.FuncVal("x",
                       MLAst.Proj(1, MLAst.Var("x"))),
         MLAst.Tuple([MLAst.String("a"), MLAst.String("b")]))

val m2 = MLAst.Case(MLAst.Tag(2, MLAst.String("a"))
                  , [(1, "x", MLAst.Var("x"))
                   , (2, "y", MLAst.String("b"))
                   , (3, "z", MLAst.String("z"))])

val id = MLAst.FuncVal("x",MLAst.Var("x"))

val m3 = MLAst.App(id, MLAst.String("hello"))

val m4 = MLAst.FuncVal("x", MLAst.FuncVal ("y", MLAst.Var("x")))


(* fun f x = if x=0 then 0 else x+f(x-1)*)
val sum = MLAst.LetFix("f", "x"
                     , MLAst.If(MLAst.Prims(MLAst.PrimOp.Equals
                                          , [MLAst.Var("x")
                                           , MLAst.Num(0)])
                              , MLAst.Num(0)
                              , MLAst.Prims( MLAst.PrimOp.Add
                                           ,[ MLAst.Var("x")
                                            , MLAst.App( MLAst.Var("f")
                                                       , MLAst.Prims(MLAst.PrimOp.Sub
                                                                   ,[ MLAst.Var("x")
                                                                    , MLAst.Num(1)]))]))
                     ,MLAst.App(MLAst.Var "f", MLAst.Num 100))

val sum2 = MLAst.LetFix("f", "x"
                      , MLAst.LetVal("y"
                                   , MLAst.Prims (MLAst.PrimOp.Print, [
                                                  MLAst.Prims (MLAst.PrimOp.Int2String
                                                             , [MLAst.Var "x"])])
                                   ,MLAst.If(MLAst.Prims(MLAst.PrimOp.Equals
                                                       , [MLAst.Var("x")
                                                        , MLAst.Num(0)])
                                           , MLAst.Num(0)
                                           , MLAst.Prims(MLAst.PrimOp.Add
                                                       ,[MLAst.Var("x")
                                                       , MLAst.App(MLAst.Var("f")
                                                                 , MLAst.Prims(MLAst.PrimOp.Sub
                                                                             ,[MLAst.Var("x")
                                                                             , MLAst.Num(1)]))])))
                      ,MLAst.App(MLAst.Var "f", MLAst.Num 100))

val tostring = MLAst.Prims (MLAst.PrimOp.Print, [
                                MLAst.Prims (MLAst.PrimOp.Int2String
                                           , [sum])])

end

