signature CPS = 
sig
    type cont
    type func

    structure PrimOp:
              sig
                  datatype t 
                    = Print
                    | Add
                    | Sub
                    | Times
                    | Int2String
                    | LessThan
                    | LargerThan
                    | Equals
                    | AndAlso
                    | OrElse
                    | Not

                  val pp: t -> unit 
              end               
 
    datatype t
      = LetVal of string * v * t 
      | LetCont of cont * string * t * t  
      | ContApp of cont * string   
      | FuncApp of func * cont * string  
      | Case of string * ((int * string * t) list) 
      | LetPrim of string * PrimOp.t * string list * t 
      (*let val x = option [x1, ...] in t*)
      | If of string * cont * cont
      | LetFix of func * cont * string * t * t 
      | Exit of string  
         and v
           = Empty
           | True
           | False
           | Num of int
           | String of string 
           | Tuple of string list 
           | Tag of int * string 
           | FuncVal of cont * string * t 
           | Proj of int * string

    val dump2file : t * string -> unit
    val pp : t  -> unit
end
