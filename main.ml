open Types
open TreePrinter


(* -------------------------------- Testing terms --------------------------------*)
let t1 = Abs("x",Abs("y",Appl(Var 0,Var 1)))
let ty1 = Arrow(TVar "Alpha", Arrow (Arrow (TVar "alpha", TVar "alpha"), TVar "alpha")) 
let g1 = {env = []; ter = t1; ty = ty1}
let tree = Node(One g1, Leaf (One g1))


            
                    
let () =
  let res1 = print_proof_tree tree in
  Printf.printf "resultat \n %s \n" res1




      
