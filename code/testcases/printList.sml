datatype node 
  = Nil of unit
  | Some of int * node

val code
  = let fun printList list
          = case list
             of Some x =>
                let val value = #1 x
                in let val rest = #2 x
                   in let val y = print (int2string value)
                      in let val z = print " "
                         in printList rest
                         end
                      end
                   end
                end
              | Nil x => ()
    in printList
           (Some(100, Some(20, Some(44, Some(~89, Some(900, Some (777, Some(822, Some(5, Some(~99, Some(88, Some(50, Some (24, Some (0, Some(1, Nil())))))))))))))))
                end

