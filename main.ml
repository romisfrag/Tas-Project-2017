open Types
open TreePrinter
open TypeChecker
open Parser
open Sexplib
       
(* -------------------------------- Testing terms --------------------------------*)
let t1 = Abs("x",Abs("y",Var 1))
let file = "latexOutputs/out.txt"


            
(* for test purpose functions *)
let rec print_substitution (s : substitution) : string =
  match s with
  | [] -> ""
  | (name,ty) :: next -> "(" ^ name ^ ":" ^ typ_to_latex ty ^ ");" ^ print_substitution next
                    
let () =
  let () = Printf.printf "Enter the terme you wan't to type check \n" in
  let s = read_line () in
  let t = parse_term (Sexp.of_string s) in
  let () = Printf.printf "This is your term: \n %s \n" (term_to_latex t) in
  match type_check_with_tree t [] with
  |Some (t,s,p) ->
    let () = Printf.printf "This is your type: \n %s \n" (typ_to_latex t) in
    let () = Printf.printf "This is your substitution \n %s \n" (print_substitution s) in
    let res1 = print_proof_tree p in
    let oc = open_out file in    (* create or truncate file, return channel *)
    Printf.fprintf oc "%s\n" res1;   (* write something *)   
    close_out oc;
    Printf.printf "resultat \n %s \n" res1
  |None -> ()




      
