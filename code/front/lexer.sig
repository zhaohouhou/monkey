signature LEXER =
sig
    val openFile: string -> unit 
    val getNext: unit -> Token.t
    val close: unit -> unit
    val lex: string -> unit
    val pushBack: Token.t -> unit
    val printLine: unit -> unit
end
