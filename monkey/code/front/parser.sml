structure Parser: PARSER =
struct

val isType = ref (fn (s:string) => false)

fun emitType (t: Token.t) =
    case t 
     of Token.Var s =>
        let val old = !isType
        in isType := (fn x => if x = s
                              then true
                              else old x)
        end
      | _  => 
        let val _ = Lexer.printLine()
        in raise Fail "invalid type name"
        end

val mapCons = ref (fn (s:string) => 0)
(* return 0 if current string is not a constructor*)
val nextCons = ref 1

fun emitCons (t: Token.t) = 
    case t 
     of Token.Var s =>
        let val old = !mapCons
            val num = !nextCons
            val _ = nextCons := (num +1)
        in mapCons := (fn x => if x = s
                               then num
                               else old x)
        end
      | _ => 
        let val _ = Lexer.printLine()
        in raise Fail "invalid constructor name"
        end

fun parseType () =
    let val t = Lexer.getNext()
        val _ = (case t
                  of Token.String => ()
                   | Token.Int => ()
                   | Token.Unit => ()
                   | Token.Var s =>
                     if (!isType s)
                     then ()
                     else let val _ = Lexer.printLine()
                          in raise Fail "invalid type name"
                          end
                   | _ =>
                     let val _ = Lexer.printLine()
                     in raise Fail "invalid type name"
                     end)
        val next = Lexer.getNext()
    in (case next
         of Token.Times => parseType()
          | Token.Arrow => parseType()
          | _ => Lexer.pushBack(next))
           (*no list types supported*)
    end

fun parseConsType () =
(*start with "of" or no type info*)
    let val next = Lexer.getNext()
    in (case next
         of Token.Of => parseType()
          | _ => Lexer.pushBack(next))
    end


fun parseSingleType () =
    let val name = Lexer.getNext()
        val _ = emitCons(name)
        val _ = parseConsType()
        val next = Lexer.getNext()
    in case next
        of Token.Bar => parseSingleType ()
         | _ =>  Lexer.pushBack(next)
    end


fun parseDatatype () =
    (*no additional info for type checking*)
    let val t = Lexer.getNext()
    in case t
        of Token.DataType =>
           let val name = Lexer.getNext() 
               val _ = emitType(name)
               val _ = Lexer.getNext() (*=*)
               val _ = parseSingleType()
           in parseDatatype ()
           end
         | _ => 
           let val _ = Lexer.pushBack(t)
           in ()
           end
    end

fun parseArg () = (*parse formal arg*)
    let val head = Lexer.getNext ()
        val t = (case head
                  of Token.LParen =>
                     let val x = Lexer.getNext ()
                         val _ = Lexer.getNext() (*)*)
                     in x
                     end
                   | _ => head)
    in case t
        of Token.Var s => s
         | _ =>
           let val _ = Lexer.printLine()
           in raise Fail "invalid formal argument name"
           end
    end

fun parseExp (): MLAst.t =
    let val head = Lexer.getNext ()
    in case head
        of Token.Let =>
           let val typet = Lexer.getNext() (*val/fun*)
               val namet = Lexer.getNext ()
               val name = (case namet
                         of Token.Var x => x
                          | _ => 
                            let val _ = Lexer.printLine()
                            in raise Fail "invalid var name"
                            end)
               val arg = (case typet
                         of Token.Val => ""
                          | Token.Fun => parseArg())
               val _ = Lexer.getNext () (*=*)
               val exp1 = parseApp ()
               val _ = Lexer.getNext () (*"in"*)
               val exp2 = parseApp ()
               val _ = Lexer.getNext () (*"end"*)
           in (case typet
                of Token.Val =>
                   MLAst.LetVal(name, exp1, exp2)
                 | Token.Fun =>
                   MLAst.LetFix(name, arg, exp1, exp2))
           end
         | Token.Var x => 
           let val tag = !mapCons x
           in if tag = 0
              then MLAst.Var x
              else let val exp = parseExp()
                   in MLAst.Tag (tag, exp)
                   end
           end
         | Token.LParen => (*empty, tuple or (expression)*)
           let val list = parseTuple ([])
               val length = List.length list
           in  if length = 0
               then MLAst.Empty
               else if length = 1
               then List.hd list
               else MLAst.Tuple(List.rev list)
           end
         | Token.Fn =>
           let val arg = parseArg()
               val _ = Lexer.getNext() (*"="*)
               val _ = Lexer.getNext() (*">"*)
               val exp = parseApp()
           in MLAst.FuncVal(arg, exp)
           end
         | Token.True => MLAst.True
         | Token.False => MLAst.False
         | Token.Num i => MLAst.Num i
         | Token.StringV s => MLAst.String s
         | Token.Sharp =>
           let val num = Lexer.getNext()
               val i = (case num 
                         of Token.Num i => i
                          | _ => 
                            let val _ = Lexer.printLine()
                            in raise Fail "expect a number after #"
                            end)
               val exp = parseExp()
           in MLAst.Proj(i, exp)
           end
         | Token.Case => 
           let val cond = parseApp()
               val _ = Lexer.getNext() (*"of"*)
               val list = parseCases([])
           in MLAst.Case (cond, List.rev list)
           end
         | Token.If =>
           let val cond = parseApp()
               val _ = Lexer.getNext() (*then*)
               val thenn = parseApp()
               val _ = Lexer.getNext() (*else*)
               val elsee = parseApp()
           in MLAst.If(cond, thenn, elsee)
           end
         | Token.Print =>
           let val exp = parseExp()
           in MLAst.Prims(MLAst.PrimOp.Print, [exp])
           end
         | Token.ToString =>
           let val exp = parseExp()
           in MLAst.Prims(MLAst.PrimOp.Int2String
                        , [exp])
           end
         | Token.Not =>
           let val exp = parseExp()
           in MLAst.Prims(MLAst.PrimOp.Not, [exp])
           end
         | _ =>
           let val _ = Lexer.printLine()
               val _ = Token.pp head
               val _ = print "\n"
           in raise Fail "invalid begining of expression"
           end
    end

and parseCases (acc) =
    let val t = Lexer.getNext()
        val i = (case t
                  of Token.Var s => !mapCons s
                   | _ => 
                     let val _ = Lexer.printLine()
                     in raise Fail "invalid constructor"
                     end)
        val _ = if i = 0
                then let val _ = Lexer.printLine()
                     in raise Fail "invalid constructor"
                     end
                else ()
        val t = Lexer.getNext()
        val arg = (case t
                    of Token.Var s => s
                     | _ => 
                       let val _ = Lexer.printLine()
                       in raise Fail "expect a var name"
                       end)
        val _ = Lexer.getNext() (*"="*)
        val _ = Lexer.getNext() (*">"*)
        val exp = parseApp()
        val next = Lexer.getNext()
    in case next 
        of Token.Bar => parseCases((i, arg, exp)::acc)
         | _ =>
           let val _ = Lexer.pushBack(next)
           in (i, arg, exp)::acc
           end
    end

and parseTuple (acc) =
    let val next = Lexer.getNext()
    in case next
        of Token.RParen => acc
         | _ =>
           let val _ = Lexer.pushBack(next)
               val new = parseApp()
               val next = Lexer.getNext()
           in (case next
                of Token.RParen => new :: acc (*order reversed*)
                 | Token.Comma => parseTuple(new:: acc)
                 | _ =>
                   let val _ = Lexer.printLine()
                   in raise Fail "expect \",\" or \")\""
                   end)
           end
    end


and parseTimes0 (left) =
    let val next = Lexer.getNext()
    in case next
        of Token.Times => 
           let val right = parseExp()
           in parseTimes0(MLAst.Prims(
                          MLAst.PrimOp.Times
                        , [left, right]))
           end
         | _ =>
           let val _ = Lexer.pushBack(next)
           in left
           end
    end

and parseTimes () = 
    let val left = parseExp()
    in parseTimes0(left)
    end

and parseAdd0 (left) =
    let val next = Lexer.getNext()
    in case next
        of Token.Add => 
           let val right = parseTimes()
           in parseAdd0(MLAst.Prims(
                        MLAst.PrimOp.Add
                      , [left, right]))
           end
         | Token.Sub => 
           let val right = parseTimes()
           in parseAdd0(MLAst.Prims(
                        MLAst.PrimOp.Sub
                      , [left, right]))
           end
         | _ =>
           let val _ = Lexer.pushBack(next)
           in left
           end
    end

and parseAdd () = 
    let val left = parseTimes()
    in parseAdd0(left)
    end

and parseComp0 (left) =
    let val next = Lexer.getNext()
    in case next
        of Token.LArrow => 
           let val right = parseAdd()
           in parseComp0(MLAst.Prims(
                        MLAst.PrimOp.LessThan
                      , [left, right]))
           end
         | Token.RArrow => 
           let val right = parseAdd()
           in parseComp0(MLAst.Prims(
                        MLAst.PrimOp.LargerThan
                      , [left, right]))
           end
         | _ =>
           let val _ = Lexer.pushBack(next)
           in left
           end
    end

and parseComp () = 
    let val left = parseAdd()
    in parseComp0(left)
    end

and parseEq0 (left) =
    let val next = Lexer.getNext()
    in case next
        of Token.Equal => 
           let val right = parseComp()
           in parseEq0(MLAst.Prims(
                       MLAst.PrimOp.Equals
                     , [left, right]))
           end
         | _ =>
           let val _ = Lexer.pushBack(next)
           in left
           end
    end

and parseEq () = 
    let val left = parseComp()
    in parseEq0(left)
    end

and parseAnd0 (left) =
    let val next = Lexer.getNext()
    in case next
        of Token.AndAlso => 
           let val right = parseEq()
           in parseAnd0(MLAst.Prims(
                        MLAst.PrimOp.AndAlso
                      , [left, right]))
           end
         | Token.OrElse => 
           let val right = parseEq()
           in parseAnd0(MLAst.Prims(
                        MLAst.PrimOp.OrElse
                      , [left, right]))
           end
         | _ =>
           let val _ = Lexer.pushBack(next)
           in left
           end
    end

and parseAnd () = 
    let val left = parseEq()
    in parseAnd0(left)
    end

and parseApp0 (left) = 
    let val next = Lexer.getNext()
    in case next
        of Token.Let => 
           let val _ = Lexer.pushBack(next)
               val right = parseAnd()
           in parseApp0(MLAst.App(left, right))
           end
         | Token.Fn =>
           let val _ = Lexer.pushBack(next)
               val right = parseAnd()
           in parseApp0(MLAst.App(left, right))
           end
         | Token.If =>
           let val _ = Lexer.pushBack(next)
               val right = parseAnd()
           in parseApp0(MLAst.App(left, right))
           end
         | Token.Case =>
           let val _ = Lexer.pushBack(next)
               val right = parseAnd()
           in parseApp0(MLAst.App(left, right))
           end
         | Token.Print =>
           let val _ = Lexer.pushBack(next)
               val right = parseAnd()
           in parseApp0(MLAst.App(left, right))
           end
         | Token.ToString =>
           let val _ = Lexer.pushBack(next)
               val right = parseAnd()
           in parseApp0(MLAst.App(left, right))
           end
         | Token.LParen => 
           let val _ = Lexer.pushBack(next)
               val right = parseAnd()
           in parseApp0(MLAst.App(left, right))
           end
         | Token.Sharp =>
           let val _ = Lexer.pushBack(next)
               val right = parseAnd()
           in parseApp0(MLAst.App(left, right))
           end
         | Token.Not =>
           let val _ = Lexer.pushBack(next)
               val right = parseAnd()
           in parseApp0(MLAst.App(left, right))
           end
         | Token.StringV x =>
           let val _ = Lexer.pushBack(next)
               val right = parseAnd()
           in parseApp0(MLAst.App(left, right))
           end
         | Token.Var x =>
           let val _ = Lexer.pushBack(next)
               val right = parseAnd()
           in parseApp0(MLAst.App(left, right))
           end
         | Token.Num i =>
           let val _ = Lexer.pushBack(next)
               val right = parseAnd()
           in parseApp0(MLAst.App(left, right))
           end
         | _ => 
           let val _ = Lexer.pushBack(next)
           in left
           end
    end

and parseApp () = 
    let val left = parseAnd()
    in parseApp0(left)
    end

fun parse (fname) =
    let val _ = Lexer.openFile(fname)
        val _ = parseDatatype ()
        val _ = Lexer.getNext ()
        val _ = Lexer.getNext ()
        val _ = Lexer.getNext ()
                (*cause we begin with val xxx = ...*)
        val ast = parseApp ()
        val _ = isType := (fn (s:string) => false)
        val _ = mapCons := (fn (s:string) => 0)
        val _ = nextCons := 1
                (*initialise references..*)
        val _ = Lexer.close ()
    in ast
    end

end


