signature DEFUNC = 
sig
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

    (*for now we don't print the definition of
     constructors (LAMn), won't pass anyway*)
    datatype t
      = LetVal of {name: string
                 , value: v , body: t}
      | AppFunc of {func: string  
                  , cont: string
                  , arg: string }
      (*apply_func (f, cont, arg)*)
      | AppCont of {cont: string
                  , arg: string }
      (*apply_cont (cont, arg)*)
      | Case of {cond: string
               , branches: (int * string * t) list} 
      | LetPrim of {name: string
                  , opr: PrimOp.t
                  , args: string list
                  , body: t}
      | If of {cond: string
             , thenn: t
             , elsee: t} 
      | Exit of string
         and v
           = Empty
           | True
           | False
           | Num of int
           | String of string 
           | Tuple of string list 
           | Tag of {label: int
                   , name: string} 
           | Proj of {index: int
                    , tuple: string}
                       
    datatype applycont
      = ApplyCont of {cont: string
                   , arg: string
                   , body: t}

    datatype applyfunc
      = ApplyFunc of {fname: string
                   , cont: string
                   , arg: string
                   , body: t}

    datatype program
      = Program of {applycont: applycont
                  , applyfunc: applyfunc
                  , main: t}
 
    val dump2file : program * string -> unit
    val pp : program  -> unit
end
