structure Test =
struct

val m_hello = MLAst.Prims (MLAst.PrimOp.Print, [MLAst.String "hello, world\n"])

val m0 = MLAst.FuncVal("x", MLAst.Num 1)

val m1 = MLAst.App(
         MLAst.FuncVal("x",
                       MLAst.Proj(1, MLAst.Var("x"))),
         MLAst.Tuple([MLAst.String("a"), MLAst.String("b")]))

val m2 = MLAst.Case(MLAst.Tag(1, MLAst.String("a"))
                  , "x"
                  , MLAst.Var("x")
                  , "y"
                  , MLAst.String("b"))

val id = MLAst.FuncVal("x",MLAst.Var("x"))

val m3 = MLAst.App(id, MLAst.String("hello"))

val m4 = MLAst.FuncVal("x", MLAst.FuncVal ("y", MLAst.Var("x")))


(* fun f x = if0 x then 0 else x+f(x-1)*)
val sum = MLAst.LetFix("f", "x"
                        , MLAst.If0(MLAst.Var("x")
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
                         ,MLAst.If0(MLAst.Var("x")
                                  , MLAst.Num(0)
                                  , MLAst.Prims( MLAst.PrimOp.Add
                                               ,[ MLAst.Var("x")
                                                , MLAst.App( MLAst.Var("f")
                                                           , MLAst.Prims(MLAst.PrimOp.Sub
                                                                       ,[ MLAst.Var("x")
                                                                        , MLAst.Num(1)]))])))
                        ,MLAst.App(MLAst.Var "f", MLAst.Num 100))

val tostring = MLAst.Prims (MLAst.PrimOp.Print, [
                                MLAst.Prims (MLAst.PrimOp.Int2String
                                           , [sum])])

end

