structure CpsConvert: CPS_CONVERT =
struct

exception NoRule

open Temp

fun transo (option: MLAst.PrimOp.t): Cps.PrimOp.t =
    case option 
     of MLAst.PrimOp.Add => Cps.PrimOp.Add
      | MLAst.PrimOp.Sub => Cps.PrimOp.Sub
      | MLAst.PrimOp.Times => Cps.PrimOp.Times
      | MLAst.PrimOp.Print => Cps.PrimOp.Print
      | MLAst.PrimOp.Int2String => Cps.PrimOp.Int2String
      | MLAst.PrimOp.LessThan => Cps.PrimOp.LessThan
      | MLAst.PrimOp.LargerThan => Cps.PrimOp.LargerThan
      | MLAst.PrimOp.Equals => Cps.PrimOp.Equals
      | MLAst.PrimOp.AndAlso => Cps.PrimOp.AndAlso
      | MLAst.PrimOp.OrElse => Cps.PrimOp.OrElse
      | MLAst.PrimOp.Not => Cps.PrimOp.Not

fun transTuple (ts: MLAst.t list, strs: string list
              , g: string list -> Cps.t): Cps.t =
    case ts
     of [] =>
        g(List.rev(strs))
      | t::ts1 =>
        trans(t, fn x => transTuple(ts1, x::strs, g))

and trans (x: MLAst.t , k: string -> Cps.t) =
    case x of
        MLAst.Var x => k(x)
      | MLAst.String s => 
        let val x1 = freshVal()
        in  Cps.LetVal(x1, Cps.String(s), k(x1))
        end
      | MLAst.Empty => 
        let val s = freshVal()
        in Cps.LetVal(s, Cps.Empty, k(s))
        end
      | MLAst.True => 
        let val s = freshVal()
        in Cps.LetVal(s, Cps.True, k(s))
        end
      | MLAst.False => 
        let val s = freshVal()
        in Cps.LetVal(s, Cps.False, k(s))
        end
      | MLAst.Num i => 
        let val x = freshVal()
        in Cps.LetVal(x, Cps.Num i, k(x))
        end
      | MLAst.App (t1,t2) =>
        let val k1 = freshCont()
            val x = freshVal()
        in  trans (t1
                 ,fn z1=> trans(t2
                              ,fn z2=> Cps.LetCont(k1
                                                 , x
                                                 , k x
                                                 , Cps.FuncApp(z1,k1,z2 ))))
        end
      | MLAst.Tuple tlist =>
        let val x = freshVal()
        in transTuple(tlist, []
                    , fn l =>
                         Cps.LetVal(x, Cps.Tuple(l)
                                  , k x))
        end
      | MLAst.FuncVal (x, e) =>
        let val f = freshVal()
            val k1 = freshCont()
        in Cps.LetVal(f
                    , Cps.FuncVal(k1
                                , x
                                , trans(e,fn z=>Cps.ContApp(k1, z)))
                    , k f)
        end
      | MLAst.Tag (i, e) =>
        let val x = freshVal()
        in trans(e, fn z=>
                       Cps.LetVal(x
                                , Cps.Tag(i
                                        , z)
                                , k x))
        end
      | MLAst.LetVal(x, e1, e2) =>
        let val j = freshCont()
        in Cps.LetCont(j
                     , x
                     , trans(e2 , k )
                     , trans(e1,fn z =>
                                   Cps.ContApp(j, z)))
        end
      | MLAst.Case(e, cases) =>
        let val k0 = freshCont()
            val x = freshVal()
        in trans(e
               ,fn z=>
                   Cps.LetCont(k0, x, k x, transcases(cases, k0, z, [])
                ))
        end
      | MLAst.Proj(i, e) =>
        let val x = freshVal()
        in trans(e, fn z=>
                       Cps.LetVal(x
                                , Cps.Proj(i
                                        , z)
                                , k x))
        end
      | MLAst.Prims(option, elist) =>
        let val x = freshVal()
        in transTuple(elist, []
               ,fn l=>Cps.LetPrim(x, transo(option), l, k x))
        end
      | MLAst.LetFix(f, x, fbody, e) =>
        let val j = freshCont()
        in Cps.LetFix(f, j, x
                    , trans(fbody, fn z => Cps.ContApp(j, z))
                    , trans(e, k))
        end
      | MLAst.If (e1, e2, e3) =>
        let val k0 = freshCont()
            val k1 = freshCont()
            val k2 = freshCont()
            val x = freshVal()
            val x1 = freshVal()
            val x2 = freshVal()
        in trans(e1
               , fn z1=>
                    Cps.LetCont(k0, x, k x,
                                Cps.LetCont(k1
                                          , x1
                                          , trans(e2
                                                , fn z=> Cps.ContApp(k0, z))
                                          , Cps.LetCont(k2, x2
                                                      ,trans(e3
                                                           ,fn z=>
                                                               Cps.ContApp(k0, z))
                                                      ,Cps.If(z1
                                                             ,k1
                                                             ,k2)))))
        end

and transcases (cases, cont, cond, acc:(int * string * Cps.t) list) =
    case cases
     of [(i, x, e)] => 
        let val k = freshCont()
            val y = freshVal()
        in  Cps.LetCont(k, x
                      , trans(e, fn z => Cps.ContApp(cont, z))
                      , Cps.Case(cond
                               , List.rev((i, y, Cps.ContApp(k, y))::acc)))
        end
      | (i, x, e)::cs =>
        let val k = freshCont()
            val y = freshVal()
        in  Cps.LetCont(k, x
                      , trans(e, fn z => Cps.ContApp(cont, z))
                      , transcases(cs, cont, cond
                                 , (i, y, Cps.ContApp(k, y))::acc))
        end
                        
val trans = fn (t) => trans (t, fn x => Cps.Exit x)
        
end

