open Types
open TreePrinter
open TypeChecker

(* -------------------------------- Testing terms --------------------------------*)
let t1 = Abs("x",Abs("y",Var 1))
            
                    
let () =
  match type_check_with_tree t1 [] with
  |Some (t,s,p) ->
         let res1 = print_proof_tree p in
         Printf.printf "resultat \n %s \n" res1
  |None -> ()




      
