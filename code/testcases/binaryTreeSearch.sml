datatype node 
  = Nil of unit
  | Node of int * node * node

val code = 
    let fun search value = 
            let fun f node = 
                    (case node
                      of Nil x => Nil x
                       | Node y =>
                         let val left = #2 y
                         in let val right = #3 y
                            in let val v = #1 y
                               in if (v - value) = 0
                                  then node
                                  else (case (f left)
                                         of Nil y => f right
                                          | Node x => Node x)
                               end
                            end
                         end)
            in f
            end
    in (search 46) Node (380, Node (16, Node (530, Node (680, Node (623, Node (134, Node (868, Nil (), Nil ()) , Node (373, Nil (), Nil ()) ) , Node (520, Node (736, Nil (), Nil ()) , Node (331, Nil (), Nil ()) ) ) , Node (873, Node (660, Node (893, Nil (), Nil ()) , Node (298, Nil (), Nil ()) ) , Node (648, Node (950, Nil (), Nil ()) , Node (212, Nil (), Nil ()) ) ) ) , Node (621, Node (586, Node (347, Node (624, Nil (), Nil ()) , Node (898, Nil (), Nil ()) ) , Node (866, Node (908, Nil (), Nil ()) , Node (452, Nil (), Nil ()) ) ) , Node (518, Node (321, Node (756, Nil (), Nil ()) , Node (616, Nil (), Nil ()) ) , Node (227, Node (371, Nil (), Nil ()) , Node (595, Nil (), Nil ()) ) ) ) ) , Node (893, Node (273, Node (288, Node (162, Node (174, Nil (), Nil ()) , Node (515, Nil (), Nil ()) ) , Node (749, Node (592, Nil (), Nil ()) , Node (327, Nil (), Nil ()) ) ) , Node (539, Node (572, Node (773, Nil (), Nil ()) , Node (674, Nil (), Nil ()) ) , Node (944, Node (969, Nil (), Nil ()) , Node (45, Nil (), Nil ()) ) ) ) , Node (320, Node (983, Node (167, Node (470, Nil (), Nil ()) , Node (949, Nil (), Nil ()) ) , Node (656, Node (307, Nil (), Nil ()) , Node (270, Nil (), Nil ()) ) ) , Node (487, Node (907, Node (30, Nil (), Nil ()) , Node (111, Nil (), Nil ()) ) , Node (464, Node (995, Nil (), Nil ()) , Node (601, Nil (), Nil ()) ) ) ) ) ) , Node (756, Node (410, Node (961, Node (236, Node (81, Node (890, Nil (), Nil ()) , Node (540, Nil (), Nil ()) ) , Node (303, Node (877, Nil (), Nil ()) , Node (264, Nil (), Nil ()) ) ) , Node (279, Node (133, Node (627, Nil (), Nil ()) , Node (478, Nil (), Nil ()) ) , Node (939, Node (302, Nil (), Nil ()) , Node (706, Nil (), Nil ()) ) ) ) , Node (71, Node (223, Node (84, Node (74, Nil (), Nil ()) , Node (819, Nil (), Nil ()) ) , Node (901, Node (88, Nil (), Nil ()) , Node (992, Nil (), Nil ()) ) ) , Node (912, Node (16, Node (205, Nil (), Nil ()) , Node (36, Nil (), Nil ()) ) , Node (946, Node (987, Nil (), Nil ()) , Node (5, Nil (), Nil ()) ) ) ) ) , Node (328, Node (62, Node (226, Node (284, Node (899, Nil (), Nil ()) , Node (219, Nil (), Nil ()) ) , Node (23, Node (914, Nil (), Nil ()) , Node (391, Nil (), Nil ()) ) ) , Node (547, Node (46, Node (796, Nil (), Nil ()) , Node (928, Nil (), Nil ()) ) , Node (921, Node (322, Nil (), Nil ()) , Node (253, Nil (), Nil ()) ) ) ) , Node (767, Node (858, Node (124, Node (17, Nil (), Nil ()) , Node (469, Nil (), Nil ()) ) , Node (489, Node (289, Nil (), Nil ()) , Node (274, Nil (), Nil ()) ) ) , Node (380, Node (702, Node (499, Nil (), Nil ()) , Node (231, Nil (), Nil ()) ) , Node (410, Node (383, Nil (), Nil ()) , Node (899, Nil (), Nil ()) ) ) ) ) ) ) 
    end
    
