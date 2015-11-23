structure Lexer: LEXER =
struct
val input: TextIO.instream option ref = ref NONE
val line = ref 1

val buffer = ref Token.None

fun pushBack (t: Token.t) =
    buffer := t 

fun printLine () =
    print ("line "^(Int.toString(!line))^":\n")

fun close () =
    case !input
     of SOME input0 =>
        (TextIO.closeIn input0;
         input := NONE)
      | NONE => ()

fun openFile fname =
    case !input
     of SOME input' =>
        ( close(); openFile fname)
      | NONE =>
        (input := SOME (TextIO.openIn(fname))
       ; line := 1)

fun getString (): string =
    let fun doit (acc:string) =
            (case !input
              of SOME input =>
                 ( case TextIO.lookahead(input)
                    of NONE => acc
                     | SOME c =>
                       if c = #" " orelse c = #")"
                          orelse c = #"+" orelse c = #"-" 
                          orelse c = #"*" orelse c = #"="
                          orelse c = #">" orelse c = #"<"
                          orelse c = #"," orelse c = #"("
                          orelse c = #"|" orelse c = #"\n"
                       then acc
                       else if Char.isDigit(c) orelse Char.isAlpha(c)
                       (*var names: c+[c|i]*, no underlines for now *)
                       then let val _ = TextIO.input1(input)
                            in doit(acc^(Char.toString(c)))
                            end
                       else raise Fail ("Invalid token "
                                        ^(Char.toString c)^" at line:"
                                        ^Int.toString(!line)))
               | _ => raise Fail "no input file" )
    in doit ("")
    end 

fun map (name: string): Token.t =
  let fun doit x =
          if x = "let" then Token.Let
          else if x = "fun" then Token.Fun 
          else if x = "val" then Token.Val
          else if x = "in" then Token.In 
          else if x = "of" then Token.Of 
          else if x = "datatype" then Token.DataType 
          else if x = "end" then Token.End 
          else if x = "fn" then Token.Fn
          else if x = "if" then Token.If 
          else if x = "then" then Token.Then
          else if x = "else" then Token.Else 
          else if x = "case" then Token.Case 
          else if x = "true" then Token.True
          else if x = "false" then Token.False 
          else if x = "int2string" then Token.ToString 
          else if x = "andalso" then Token.AndAlso 
          else if x = "print" then Token.Print
          else if x = "orelse" then Token.OrElse 
          else if x = "string" then Token.String 
          else if x = "unit" then Token.Unit 
          else if x = "int" then Token.Int 
          else Token.None
  in doit name
  end

fun getName (): Token.t =
    let val name = getString ()
        val token = map name
    in case token
        of Token.None => Token.Var(name)
         | _ => token
    end

fun getNum ():int =
    let fun doit (acc:int) =
            (case !input
              of SOME input =>
                 (case TextIO.lookahead(input)
                   of NONE => acc
                    | SOME c =>
                      if Char.isDigit c
                      then let val _ = TextIO.input1(input)
                           in doit(acc*10+ Char.ord(c)-Char.ord(#"0"))
                            end
                      else if c = #" " orelse c = #")"
                              orelse c = #"+" orelse c = #"-" 
                              orelse c = #"*" orelse c = #"="
                              orelse c = #">" orelse c = #"<"
                              orelse c = #"," orelse c = #"|"
                              orelse c = #"\n"
                      then acc
                      else raise Fail ("Invalid token "
                                       ^(Char.toString c)^" at line:"
                                       ^Int.toString(!line)))
               | _  => raise Fail "no input file")
    in doit(0)
    end 
    
fun getFormatString () = 
(*get string withing ""*)
    let fun doit (acc:string) =
            (case !input
              of SOME input =>
                 (case TextIO.lookahead(input)
                   of NONE => let val _ = printLine ()
                              in raise Fail "string not ended"
                              end
                    | SOME c =>
                      if c = #"\\" 
                      then let val _ = TextIO.input1(input)
                           in (case TextIO.lookahead(input)
                                of NONE => let val _ = printLine ()
                                           in raise Fail "string not ended"
                                           end
                                 | SOME c => 
                                   let val _ = TextIO.input1(input)
                                   in doit(acc^(Char.toString c))
                                   end)
                           end (*no matter what's after "\", just take in*)
                      else if c = #"\"" (*leave to getNext()*)
                      then acc
                      else let val _ = TextIO.input1(input)
                           in doit(acc^(Char.toString c))
                           end)
               | _ => raise Fail "no input file" )
    in doit ("")
    end 
    
fun getNext () =
    let val buffer0 = !buffer
    in case buffer0 
        of Token.None =>
           (let  val input = 
                     (case !input
                       of SOME x => x
                        | _ => raise Fail "no input file")
                 val c = TextIO.lookahead(input)
            in case c
                of NONE => Token.None
                 | SOME c =>
                   (*number*)
                   if Char.isDigit(c)
                   then Token.Num(getNum())
                   else if c = #"~"
                   then let val _ = TextIO.input1(input)
                        in Token.Num(0-getNum())
                        end
                   (*character*)
                   else if Char.isAlpha(c)
                   then getName()
                   (*symbols*)
                   else if c = #"("
                   then let val _ = TextIO.input1(input)
                        in Token.LParen
                        end
                   else if c = #")"
                   then let val _ = TextIO.input1(input)
                        in Token.RParen
                        end
                   else if c = #"="
                   then let val _ = TextIO.input1(input)
                        in Token.Equal
                        end
                   else if c = #"*"
                   then let val _ = TextIO.input1(input)
                        in Token.Times
                        end
                   else if c = #"-"
                   then let val _ = TextIO.input1(input)
                            val next = TextIO.lookahead(input)
                        in if next = SOME(#">")
                           then let val _ =  TextIO.input1(input)
                                in Token.Arrow
                                end
                           else Token.Sub
                        end
                   else if c = #"+"
                   then let val _ = TextIO.input1(input)
                        in Token.Add
                        end
                   else if c = #"|"
                   then let val _ = TextIO.input1(input)
                        in Token.Bar
                        end
                   else if c = #"#"
                   then let val _ = TextIO.input1(input)
                        in Token.Sharp
                        end
                   else if c = #"<"
                   then let val _ = TextIO.input1(input)
                        in Token.LArrow
                        end
                   else if c = #">"
                   then let val _ = TextIO.input1(input)
                        in Token.RArrow
                        end
                   else if c = #"\""
                   then let val _ = TextIO.input1(input) (*"\""*)
                            val s = getFormatString()
                            val _ = TextIO.input1(input) (*"\""*)
                        in Token.StringV(s)
                        end
                   else if c = #"!"
                   then let val _ = TextIO.input1(input)
                        in Token.Not
                        end
                   else if c = #","
                   then let val _ = TextIO.input1(input)
                        in Token.Comma
                        end
                   (*passing blanks, not suitable for annotations*)
                   else if c = #" "
                   then let val _ = TextIO.input1(input)
                        in getNext()
                        end
				   else if c = #"\t"
                   then let val _ = TextIO.input1(input)
                        in getNext()
                        end
                   else if c = #"\n"
                   then let val _ = TextIO.input1(input)
                            val _ = line := (!line +1)
                        in getNext()
                        end
                   else raise Fail ("Invalid token "
                                    ^(Char.toString c)^" at line:"
                                    ^(Int.toString(!line)))
            end)
         | _ =>
           let val _ = buffer := Token.None
           in buffer0
           end
    end
            
(*this is just for visualization *)
fun lex fname =
    let val _ = openFile fname
        fun doit () = 
            (case getNext()
              of Token.None => ()
               | x =>
                 let val _ = Token.pp x
                     val _ = print " "
                 in doit ()
                 end)
        val _ = doit ()
    in close ()
    end

end
