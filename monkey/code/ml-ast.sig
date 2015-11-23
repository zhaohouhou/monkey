signature ML_AST = 
sig
 
    structure PrimOp: sig
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
      = Var of string
      | Empty 
      | True
      | False
      | Num of int
      | String of string
      | App of t * t 
      | FuncVal of string * t (*fn x => e*)
      | Tag of int * t 
      | LetVal of string * t * t
      | Tuple of t list
      | Proj of int * t (* #i e *)
      | Case of t * ((int * string * t) list )
      (* case e of (INi xi => ei) list *)
      | Prims of PrimOp.t * t list
      | If of t * t * t 
      | LetFix of string * string * t * t
(* let fun f (x) = e1 in e2 *)   

val dump2file : t * string -> unit
val pp : t-> unit 
end
