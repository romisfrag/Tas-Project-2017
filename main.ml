open Types
open TreePrinter
open TypeChecker
open Parser
open Sexplib
       
(* -------------------------------- Testing terms --------------------------------*)
let t1 = Abs("x",Abs("y",Var 1))
            
                    
let () =
  let () = Printf.printf "Enter the terme you wan't to type check \n" in
  let s = read_line () in
  let t = parse_term (Sexp.of_string s) in
  let () = Printf.printf "This is your term: \n %s \n" (term_to_latex t) in
  match type_check_with_tree t [] with
  |Some (t,s,p) ->
    let () = Printf.printf "This is your type: \n %s \n" (typ_to_latex t) in
    let res1 = print_proof_tree p in
    Printf.printf "resultat \n %s \n" res1
  |None -> ()




      
