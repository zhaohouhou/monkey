signature FLAT =
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
              end

    structure Exp:
              sig
                  datatype t
                    = Call of {func: string
                             , args: string list}
                    | If of {cond: string
                           , thenn: t
                           , elsee: t}
                    | Case of {cond: string
                             , branches: (int * string * t) list}
                    | Exit of string
              end

    structure Binding:
              sig
                  datatype t
                    = Empty
                    | True
                    | False
                    | Proj of int * string
                    | Num of int
                    | String of string
                    | Tuple of string list
                    | Prim of PrimOp.t * string list
                    | Tag of int * string       
              end

    structure Function:
              sig
                  datatype t
                    = T of {name: string
                          , args: string list
                          , bindings: (string * Binding.t) list
                          , body: Exp.t}
                           
              end

    structure Program:
              sig
                  datatype t
                    = T of {main: Function.t
                          , funcs: Function.t list}

                  val dump2file: t * string -> unit
              end
end
