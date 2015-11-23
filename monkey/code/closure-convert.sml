structure ClosureConvert: CLOSURE_CONVERT =
struct

open Temp

fun transOpt (opt:Cps.PrimOp.t):Closure.PrimOp.t =
    case opt
     of Cps.PrimOp.Add => Closure.PrimOp.Add
      | Cps.PrimOp.Sub => Closure.PrimOp.Sub
      | Cps.PrimOp.Times => Closure.PrimOp.Times
      | Cps.PrimOp.Print => Closure.PrimOp.Print
      | Cps.PrimOp.Int2String => Closure.PrimOp.Int2String
      | Cps.PrimOp.LessThan => Closure.PrimOp.LessThan
      | Cps.PrimOp.LargerThan => Closure.PrimOp.LargerThan
      | Cps.PrimOp.Equals => Closure.PrimOp.Equals
      | Cps.PrimOp.AndAlso => Closure.PrimOp.AndAlso
      | Cps.PrimOp.OrElse => Closure.PrimOp.OrElse
      | Cps.PrimOp.Not => Closure.PrimOp.Not

(* calulate free variables and store some results in AST. *)
fun freeVarTerm (t: Cps.t): Closure.t * Set.t =
    case t
     of Cps.LetVal(x, Cps.Proj(i, y), t) =>
        let val (t', set2) = freeVarTerm t
            val set = Set.add(y, Set.remove(x, set2))
        in (Closure.Let{name=x, index=i,tuple=y, body= t'}, set)
        end
      | Cps.LetVal(x, v, t) =>
        let val (v', set1) = freeVarValue v
            val (t', set2) = freeVarTerm t
            val set = Set.union(set1,
                                 Set.remove(x, set2))
        in (Closure.LetVal{name=x, value=v',body= t'}, set)
        end
      | Cps.LetCont(k, x, t1, t2) =>
        let val (t1', set1) = freeVarTerm t1
            val (t2', set2) = freeVarTerm t2
            val env = freshEnv()
            val set1' = Set.remove(k, Set.remove(x, set1))
            val set2' = Set.remove(k, set2)
            val set = Set.union(set1', set2')
        in (Closure.LetCont{cont=k, env=env, arg=x
                          , fbody=t1', body=t2', freevars=set1'}, set)
        end
      | Cps.ContApp (k, x) =>
        let val env  = freshEnv()
        in (Closure.ContApp{cont=k, env=env, arg=x}
          , Set.add(k, Set.add(x, Set.new())))
        end
      | Cps.FuncApp (f, k, x) =>
        let val env  = freshEnv()
        in (Closure.FuncApp{func=f, env=env, cont=k, arg=x}
          , Set.add(f, Set.add(k, Set.add(x, Set.new()))))
        end
      | Cps.Case (x, cases) =>
        let fun freevar (cases, cond, tl, fset) =
                (case cases 
                  of [] => (Closure.Case{cond=x
                                       , branches=List.rev tl}
                          , fset)
                   | (i, xi, ti)::cs =>
                     let val (ei, set) = freeVarTerm(ti)
                     in freevar (cs, cond, (i, xi, ei)::tl
                               , Set.union(fset, Set.remove(xi,set)))
                     end)
            val (e, fset) = freevar (cases, x, [], [])
        in (e, Set.add(x, fset))
        end
      | Cps.LetPrim (x, option, xs, t) =>  
        let val (t', set1) = freeVarTerm t
            val set2 = Set.addAll(xs, Set.new())
        in (Closure.LetPrim {name=x, opr=transOpt(option)
                           , args=xs, body=t'}
          , Set.union(Set.remove(x, set1), set2))
        end
      | Cps.If (x, k1, k2) =>
        let val x1 = freshVal()
            val env1 = freshEnv()
            val env2 = freshEnv()
        in (Closure.If{cond=x
                     , thenn=Closure.ContApp {cont=k1
                                            , env=env1, arg=x1}
                     ,elsee=Closure.ContApp {cont=k2
                                           , env=env2, arg=x1}} 
          (*not fully converted*)
          , Set.add(x, Set.add(k1, Set.add(k2, Set.new()))))
        end
      | Cps.LetFix (f, k, x, t1, t2)=>
        let val env = freshEnv()
            val (t1', set1) = freeVarTerm(t1)
            val (t2', set2) = freeVarTerm(t2)
            val set1' = Set.remove(f, Set.remove(k, Set.remove(x, set1)))
            val set2' = Set.remove(f, set2)
        in (Closure.LetFix {func=f, env=env, cont=k
                          , arg=x, fbody=t1', body=t2'
                          , freevars=set1'}
            , Set.union(set1', set2'))
        end
      | Cps.Exit x =>
        (Closure.Exit(x), Set.add(x, Set.new()))

and freeVarValue (v: Cps.v): Closure.v * Set.t =
    case v
     of Cps.Empty => (Closure.Empty, Set.new())
      | Cps.True => (Closure.True, Set.new())
      | Cps.False => (Closure.False, Set.new())
      | Cps.Num i => (Closure.Num(i), Set.new())
      | Cps.String x => 
        (Closure.String(x), Set.new())
      | Cps.Tuple xs =>
        (Closure.Tuple xs
       , Set.addAll(xs, Set.new()))
      | Cps.Tag (i, x) =>
        (Closure.Tag{label=i, name=x}, Set.add(x, Set.new()))
      | Cps.FuncVal (k, x, t) =>
        let val env = freshEnv()
            val (t1, set) = freeVarTerm(t)
            val set' = Set.remove(k, Set.remove(x, set))
        in (Closure.FuncVal {env=env, cont=k, arg=x
                           , fbody=t1, freevars=set'}, set')
        end
      | Cps.Proj x => raise Fail "compiler inner bug" 
    (*should be handled in freeVarTerm*)
                     
fun doit(t:Closure.t, n: int, env:string, xs:string list) =
    (*unfold all free vars in xs*)
    case xs
     of [] => convert t
      | x::xs =>
        Closure.Let{name=x, index=n, tuple=env
                  , body=doit (t, n+1, env, xs)}

and convert t =
    case t
     of Closure.LetVal{name
                     , value=Closure.FuncVal{env, cont
                                           , arg, fbody, freevars}
                     , body}=>
        (*let val x = fn env k y => t1
          in t2 *)
        let val x_code = freshVal ()
            val env' = freshEnv()
        in Closure.LetVal{name=x_code
                        ,value=Closure.FuncVal{env=env, cont=cont, arg=arg
                                       , fbody=doit(fbody, 1, env, freevars)
                                       , freevars=[]}
                        ,body=Closure.LetVal{ name=env'
                                       ,value=Closure.Tuple(freevars)
                                       ,body=
                                        Closure.LetVal{name=name
                                                     ,value=Closure.Tuple [x_code, env']
                                                     ,body=convert body}}}
        end
      | Closure.LetVal{name, value , body} => (*v: not FuncVal*) 
        Closure.LetVal{name=name, value=value , body=convert(body)}
      | Closure.Let{name, index, tuple, body} =>
        Closure.Let{name=name, index=index, tuple=tuple, body=convert(body)}
      | Closure.LetCont{cont, env, arg, fbody, body, freevars} =>
        let val k_code = freshVal()
            val env' = freshEnv()
        in Closure.LetCont{cont=k_code, env=env, arg=arg
                         , fbody=doit(fbody, 1, env, freevars)
                         , body=Closure.LetVal{name=env'
                                        ,value=Closure.Tuple(freevars)
                                        ,body=Closure.LetVal{name=cont
                                                      ,value=Closure.Tuple [k_code, env']
                                                      ,body=convert body}}, freevars=[]}
        end
      | Closure.ContApp{cont, env, arg}=> 
        let val k_code = freshCont()
        in Closure.Let{name=k_code, index=1, tuple=cont
                     , body=Closure.Let{name=env, index=2, tuple=cont
                                 , body=Closure.ContApp{cont=k_code, env=env, arg=arg}}}
        end
      | Closure.FuncApp{func, env, cont, arg}=> 
        let val f_code = freshCont() (*TODO: or func?*)
        in Closure.Let{name=f_code, index=1, tuple=func
                     , body=Closure.Let{name=env, index=2, tuple=func
                                      , body=Closure.FuncApp{func=f_code, env=env
                                                           , cont=cont, arg=arg}}}
        end
      | Closure.Case {cond, branches} =>
        let fun transcases (cases, acc) =
                case cases 
                 of [] => List.rev acc
                  | (i, x, t)::cs => transcases(cs, (i, x, convert t)::acc)
        in  Closure.Case {cond=cond, branches=transcases (branches, [])}
        end
      | Closure.LetPrim {name, opr, args, body} =>
        Closure.LetPrim {name=name, opr=opr, args=args, body=convert body}
      | Closure.If {cond
                  , thenn=Closure.ContApp{cont, env, arg}
                  , elsee} =>
        (*guaranteed in freeVars() that t1= k1 x1, t2= k2 x1*)
        Closure.LetVal{name=arg, value=Closure.Empty,
                       body=Closure.If {cond=cond
                                      , thenn=
                                        convert (Closure.ContApp{cont=cont
                                                               , env=env, arg=arg})
                                      , elsee=convert elsee}}
      | Closure.LetFix{func, env, cont, arg, fbody, body, freevars} =>
        let val f_code = freshCont()
            val env' = freshEnv()
        in Closure.LetFix{func=f_code, env=env, cont=cont, arg=arg
                        ,fbody=Closure.LetVal{name=func
                                            , value=Closure.Tuple [f_code, env]
                                            , body=doit(fbody, 1, env, freevars)}
                        ,body=Closure.LetVal{name=env'
                                           ,value=Closure.Tuple(freevars)
                                           ,body=Closure.LetVal
                                                     {name=func
                                                    ,value=Closure.Tuple [f_code, env']
                                                    ,body=convert body}}
                        , freevars=[]}
        end
      | Closure.Exit(x)=> Closure.Exit(x)

fun trans t =
    let val (t1, set) = freeVarTerm (t)
        val t2 = convert t1
    in t2
    end
                    

end

