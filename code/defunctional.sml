structure Defunctional: DEFUNCTIONAL =
struct

open Temp

fun transOpt (opt:CpsDefunc.PrimOp.t):Defunc.PrimOp.t =
    case opt
     of CpsDefunc.PrimOp.Add => Defunc.PrimOp.Add
      | CpsDefunc.PrimOp.Sub => Defunc.PrimOp.Sub
      | CpsDefunc.PrimOp.Times => Defunc.PrimOp.Times
      | CpsDefunc.PrimOp.Print => Defunc.PrimOp.Print
      | CpsDefunc.PrimOp.Int2String => Defunc.PrimOp.Int2String
      | CpsDefunc.PrimOp.LessThan => Defunc.PrimOp.LessThan
      | CpsDefunc.PrimOp.LargerThan => Defunc.PrimOp.LargerThan
      | CpsDefunc.PrimOp.Equals => Defunc.PrimOp.Equals
      | CpsDefunc.PrimOp.AndAlso => Defunc.PrimOp.AndAlso
      | CpsDefunc.PrimOp.OrElse => Defunc.PrimOp.OrElse
      | CpsDefunc.PrimOp.Not => Defunc.PrimOp.Not

fun freeVarTerm (t: Cps.t): CpsDefunc.t * Set.t =
    case t
     of Cps.LetVal(x, v, t1) =>
        let val (v', set1) = freeVarValue v
            val (t', set2) = freeVarTerm t1
            val set = Set.union(set1,
                                Set.remove(x, set2))
        in (CpsDefunc.LetVal(x, v', t'), set)
        end
      | Cps.LetCont(k, x, t1, t2) =>
        let val (t1', set1) = freeVarTerm t1
            val (t2', set2) = freeVarTerm t2
            val set1' = Set.remove(x, set1)
            (*continuations won't recurse? *)
            val set2' = Set.remove(k, set2)
            val set = Set.union(set1', set2')
        in (CpsDefunc.LetCont(k, x
                            , t1', t2', set1'), set)
        end
      | Cps.ContApp (k, x) =>
        (CpsDefunc.ContApp(k, x)
       , Set.add(k, Set.add(x, Set.new())))
      | Cps.FuncApp (f, k, x) =>
        (CpsDefunc.FuncApp(f, k, x)
          , Set.add(f, Set.add(k, Set.add(x, Set.new()))))
      | Cps.Case (x, cases) =>
        let fun freevar (cases, cond, tl, fset) =
                (case cases 
                  of [] => (CpsDefunc.Case(x
                                         , List.rev tl)
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
            fun convOpt opt = (case opt
                                of Cps.PrimOp.Add => CpsDefunc.PrimOp.Add
                                 | Cps.PrimOp.Sub => CpsDefunc.PrimOp.Sub
                                 | Cps.PrimOp.Times => CpsDefunc.PrimOp.Times
                                 | Cps.PrimOp.Print => CpsDefunc.PrimOp.Print
                                 | Cps.PrimOp.Int2String => CpsDefunc.PrimOp.Int2String
                                 | Cps.PrimOp.LessThan => CpsDefunc.PrimOp.LessThan
                                 | Cps.PrimOp.LargerThan => CpsDefunc.PrimOp.LargerThan
                                 | Cps.PrimOp.Equals => CpsDefunc.PrimOp.Equals
                                 | Cps.PrimOp.AndAlso => CpsDefunc.PrimOp.AndAlso
                                 | Cps.PrimOp.OrElse => CpsDefunc.PrimOp.OrElse
                                 | Cps.PrimOp.Not => CpsDefunc.PrimOp.Not)
        in (CpsDefunc.LetPrim (x, convOpt option, xs, t')
          , Set.union(Set.remove(x, set1), set2))
        end
      | Cps.If (x, k1, k2) =>
        (CpsDefunc.If(x, k1, k2)
       , Set.add(x, Set.add(k1, Set.add(k2, Set.new()))))
      | Cps.LetFix (f, k, x, t1, t2)=>
        let val (t1', set1) = freeVarTerm(t1)
            val (t2', set2) = freeVarTerm(t2)
            val set1' = Set.remove(f, Set.remove(k, Set.remove(x, set1)))
            val set2' = Set.remove(f, set2)
        in (CpsDefunc.LetFix(f, k, x, t1', t2', set1')
            , Set.union(set1', set2'))
        end
      | Cps.Exit x =>
        (CpsDefunc.Exit(x), Set.add(x, Set.new()))

and freeVarValue (v: Cps.v): CpsDefunc.v * Set.t =
    case v
     of Cps.Empty => (CpsDefunc.Empty, Set.new())
      | Cps.True => (CpsDefunc.True, Set.new())
      | Cps.False => (CpsDefunc.False, Set.new())
      | Cps.Num i => (CpsDefunc.Num(i), Set.new())
      | Cps.String x => 
        (CpsDefunc.String(x), Set.new())
      | Cps.Tuple xs =>
        (CpsDefunc.Tuple xs
       , Set.addAll(xs, Set.new()))
      | Cps.Tag (i, x) =>
        (CpsDefunc.Tag(i, x), Set.add(x, Set.new()))
      | Cps.Proj(i, x) =>
        (CpsDefunc.Proj(i, x), Set.add(x, Set.new()))
      | Cps.FuncVal (k, x, t) =>
        let val (t1, set) = freeVarTerm(t)
            val set' = Set.remove(k, Set.remove(x, set))
        in (CpsDefunc.FuncVal(k, x, t1, set'), set')
        end

val contcases = ref ([]:(int * string * Defunc.t) list)
val funccases = ref ([]:(int * string * Defunc.t) list)
val farg = "_farg"
val karg = "_karg"
val xarg = "_xarg" (*unique within a program*)

val counteri = ref ~1 
(*return a new tag value, decrease from -1*)
fun freshTag ()=
    let val n = !counteri
        val _ = counteri :=(n-1)
    in n
    end


fun emitcont (i: int, x: string, t: Defunc.t) =
    contcases := (i, x, t):: (!contcases)

fun emitfunc (i: int, x: string, t: Defunc.t) =
    funccases := (i, x, t):: (!funccases)

fun replacel (old, new, l, acc) = 
    (case l
      of [] => List.rev acc
       | x::xs =>
         let val newx = if x = old
                        then new
                        else x
         in replacel (old, new, xs, newx::acc)
         end)

fun replace (old: string, new: string, t:Defunc.t) =
    (*given that "new" is unique*)
    case t
     of Defunc.LetVal {name, value, body} =>
        if name = old
        then Defunc.LetVal{name=name
                         , value=replacev(old, new, value) (*still needed!*)
                         , body=body}
        else Defunc.LetVal{name=name
                         , value=replacev(old, new, value)
                         , body=replace(old, new, body)}
      | Defunc.AppFunc {func, cont, arg} =>
        let val newf = if func = old then new else func
            val newc = if cont = old then new else cont
            val newa = if arg = old then new else arg
        in Defunc.AppFunc {func=newf, cont=newc, arg=newa}
        end
      | Defunc.AppCont {cont, arg} =>
        let val newc = if cont = old then new else cont
            val newa = if arg = old then new else arg
        in Defunc.AppCont {cont=newc, arg=newa}
        end
      | Defunc.Case {cond, branches} =>
        let val newcond = if cond = old
                          then new
                          else cond
            val newbranches = replacebs(old, new, branches, [])
        in Defunc.Case{cond= newcond, branches=newbranches}
        end
      | Defunc.LetPrim {name, opr, args, body} =>
        let val newbody = if name = old
                          then body
                          else replace(old, new, body)
        in Defunc.LetPrim {name=name, opr=opr
                         , args=replacel(old, new, args, [])
                         , body=newbody}
        end
      | Defunc.If {cond, thenn, elsee} =>
        let val newcond = if cond = old
                          then new
                          else cond
            val newthen = replace(old, new, thenn)
            val newelse = replace(old, new, elsee)
        in Defunc.If{cond = newcond
                   , thenn=newthen, elsee=newelse}
        end
      | Defunc.Exit x =>
        let val newx = if x = old then new else x
        in Defunc.Exit newx
        end

and replacebs (old: string, new: string
             , branches: (int * string * Defunc.t) list, acc) =
    case branches
     of [] => List.rev acc
      | (i, x, t)::bs => 
        if x = old
        then replacebs(old, new, bs, (i, x, t)::acc)
        else replacebs(old, new, bs
                     ,(i, x, replace(old, new, t))::acc)

and replacev (old: string, new: string, v:Defunc.v) =
    case v 
     of Defunc.Empty => v
      | Defunc.True => v
      | Defunc.False => v
      | Defunc.Num i => v
      | Defunc.String s => v 
      | Defunc.Tuple xs =>
        Defunc.Tuple(replacel(old, new, xs, []))
      | Defunc.Tag {label, name} =>
        let val newname = if name = old then new else name
        in Defunc.Tag{label=label, name=newname}
        end
      | Defunc.Proj {index, tuple} =>
        let val newtuple = if tuple = old then new else tuple
        in Defunc.Proj{index=index, tuple=newtuple}
        end

fun convertv (v: CpsDefunc.v) =
    case v
     of CpsDefunc.Empty => Defunc.Empty
      | CpsDefunc.True => Defunc.True
      | CpsDefunc.False => Defunc.False
      | CpsDefunc.Num i => Defunc.Num i
      | CpsDefunc.String s => Defunc.String s
      | CpsDefunc.Tuple xs => Defunc.Tuple xs 
      | CpsDefunc.Tag (i, x) =>
        Defunc.Tag{label=i, name=x}
      | CpsDefunc.Proj (i, x) =>
        Defunc.Proj{index=i, tuple=x}
      | _ => (*funcval should not reach here*)
        raise Fail "inner error" 

fun doit(t:Defunc.t, n: int, env:string, xs:string list) =
    (*unfold all free vars in xs*)
    case xs
     of [] => t
      | x::xs =>
        Defunc.LetVal{name=x
                    , value=Defunc.Proj{index=n, tuple=env}
                    , body=doit (t, n+1, env, xs)}

fun convert (t: CpsDefunc.t):Defunc.t =
    case t
     of CpsDefunc.LetVal(x
                       , CpsDefunc.FuncVal(k, z, body, fvs), t) =>
        let val body' = convert body
            val t' = convert t
            val env = freshEnv()
            val env'= freshEnv()
            val tag = freshTag()
            val _ = emitfunc(tag, env'
                           , doit(replace(k, karg
                                        , replace(z, xarg
                                                , body'))
                                , 1, env', fvs))
        in Defunc.LetVal{name=env
                       , value=Defunc.Tuple(fvs)
                       , body=
                         Defunc.LetVal{name=x
                                     , value=Defunc.Tag{label=tag
                                                      ,name=env}
                                     , body = t'}}
        end
      | CpsDefunc.LetVal (x, v, t) =>
        let val t' = convert t
            val v' = convertv v
        in Defunc.LetVal{name=x, value=v', body=t'}
        end
      | CpsDefunc.LetCont (k, z, t1, t2, fvs) =>
        let val t1' = convert t1
            val t2' = convert t2
            val env = freshEnv()
            val env'= freshEnv()
            val tag = freshTag()
            val _ = emitcont(tag, env'
                           , doit(replace(z, xarg, t1')
                                , 1, env', fvs))
        in Defunc.LetVal{name=env
                       , value=Defunc.Tuple(fvs)
                       , body=
                         Defunc.LetVal{name=k
                                     , value=Defunc.Tag{label=tag
                                                      ,name=env}
                                     , body = t2'}}
        end
      | CpsDefunc.ContApp (k, x) =>
        Defunc.AppCont {cont=k, arg=x}
      | CpsDefunc.FuncApp (f, k, x) =>
        Defunc.AppFunc {func=f, cont=k, arg=x}
      | CpsDefunc.Case (x, cases) =>
        Defunc.Case{cond=x
                  , branches=convertcs(cases, [])}
      | CpsDefunc.LetPrim (x, opr, xs, t) => 
        let val t' = convert t
        in Defunc.LetPrim {name=x, opr=transOpt(opr)
                         ,args=xs
                         ,body=t'}
        end
      | CpsDefunc.If (cond, k1, k2) =>
        let val x1 = freshVal()
        in Defunc.LetVal{name=x1
                       , value=Defunc.Empty
                       , body=Defunc.If{cond=cond
                                      , thenn=
                                        Defunc.AppCont{cont=k1
                                                     , arg=x1}
                                      , elsee=
                                        Defunc.AppCont{cont=k2
                                                     , arg=x1}}}
        end
      | CpsDefunc.LetFix (f, k, x, t1, t2, fvs) =>
        let val t1' = convert t1
            val t2' = convert t2
            val env = freshEnv()
            val env'= freshEnv()
            val tag = freshTag()
            val _ = emitfunc(tag, env'
                           , Defunc.LetVal{name=f
                                         , value=
                                           Defunc.Tag{label=tag
                                                    , name=env'}
                                         , body=
                                           doit(replace(k, karg
                                                      , replace(x, xarg
                                                              , t1'))
                                              , 1, env', fvs)})
        in Defunc.LetVal{name=env
                       , value=Defunc.Tuple(fvs)
                       , body=
                         Defunc.LetVal{name=f
                                     , value=Defunc.Tag{label=tag
                                                      ,name=env}
                                     , body = t2'}}
        end
      | CpsDefunc.Exit x =>
        Defunc.Exit x 

and convertcs (cases, acc) =
    case cases
     of [] => List.rev acc
      | (i, x, t)::cs =>
        convertcs(cs, (i, x, convert(t))::acc)
        
fun trans (t: Cps.t): Defunc.program =
    let val (cps_freevar, set) = freeVarTerm (t)
        val _ = contcases := []
        val _ = funccases := []
        val main = convert cps_freevar
    in Defunc.Program{applycont =
                      Defunc.ApplyCont{cont=karg
                                     ,arg=xarg
                                     ,body= Defunc.Case
                                                {cond=karg
                                               , branches= !contcases}}
                    , applyfunc=
                      Defunc.ApplyFunc{fname=farg
                                     ,cont=karg
                                     ,arg=xarg
                                     ,body= Defunc.Case
                                                {cond=farg
                                               , branches= !funccases}}
                    , main=main}
    end

end
