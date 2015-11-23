structure Token: TOKEN =
struct

datatype t
  (*keywords*)
  = Let (*"let"*)
  | Fun (*"fun"*)
  | Val (*"val"*)
  | In (*"in"*)
  | Of (*"of"*)
  | DataType (* "datatype" *)
  | End (*"end"*)
  | Fn (*"fn"*)
  | If (*"if"*)
  | Then (*"then"*)
  | Else (*"else"*)
  | Case (*"case"*)
  | Print (*"print"*)
  | ToString (*"toString"*)
  | AndAlso
  | OrElse
  | True
  | False

  (*types*)
  | String (*"string"*)
  | Int (*"int"*)
  | Unit (*"unit"*)

  (*symbols*)
  | LParen (*"("*)
  | RParen (*")"*)
  | Equal (*"="*)
  | Times  (*"*"*)
  | Sub  (*"-"*)
  | Add  (*"+"*)
  | Bar (*"|"*)
  | Sharp  (*"#"*)
  | LArrow (*"<"*)
  | RArrow (*">"*)
  | Comma (*","*)
  | Arrow (*"->"*)
  | Not (*"!"*)

  | StringV of string
  | Var of string (*variables*)
  | Num of int  (*text: add "~" for negative numbers*)
  | None (*as the end of tokens*)

(*util*)
val out: TextIO.outstream option ref = ref NONE
fun print (s) =
    case !out 
     of SOME out' => 
        TextIO.output(out', s)
      | NONE => 
        TextIO.output(TextIO.stdOut, s)

fun pp (t) =
    (case t
      of Let  => print "Let"
       | Fun  => print "Fun"
       | Val => print "Val"
       | In  => print "In"
       | Of  => print "Of"
       | DataType => print "Datatype"
       | End => print "End"
       | Fn  => print "Fn"
       | If => print "If"
       | Then  => print "Then"
       | Else => print "Else"
       | Case => print "Case"
       | Print => print "Print"
       | ToString  => print  "ToString"
       | AndAlso  => print  "AndAlso"
       | OrElse  => print "OrElse"
       | True => print "True"
       | False => print "false"
                    
       | String  => print "String"
       | Int  => print "Int"
       | Unit  => print "Unit"
                  
       | LParen  => print "LPAren"
       | RParen  => print "RParen"
       | Equal => print "Equal"
       | Times => print "Times"
       | Sub  => print "Sub"
       | Add  => print "Add"
       | Bar => print "Bar"
       | Sharp  => print "Sharp"
       | LArrow  => print "LArrow"
       | RArrow  => print "RArrow"
       | Comma  => print "Comma"
       | Arrow  => print "Arrow"
       | Not  => print "Not"
                   
       | StringV s => print ("\""^s^"\"")
       | Var s => print s
       | Num i => print (Int.toString(i))
       | None => print "None")

fun dump2file (l, fname: string) =
    let val out' = TextIO.openOut (fname)
        val _ = out:= SOME out'
        fun doit (l) =
            (case l
              of [] => ()
               | x::xs =>
                 let val _ = pp x
                     val _ = print " "
                 in doit xs
                 end )
        val _ = doit l
        val _ = TextIO.flushOut (out')
        val _ = out := NONE
        val _ = TextIO.closeOut(out')
    in ()
    end

end
