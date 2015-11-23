signature TOKEN =
sig
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

      | StringV of string (*"s"*)
      | Var of string (*variables*)
      | Num of int  (*text: add "~" for negative numbers*)
      | None (*as the end of tokens*)

val pp: t -> unit
val dump2file : (t list * string)  -> unit
end
