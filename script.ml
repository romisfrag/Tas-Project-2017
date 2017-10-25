open Js_of_ocaml
open Types
open TypeChecker
open Parser
open Sexplib
open TreePrinter


(* let parse_check_return jstring = *)
(*   let s = Js.to_string jstring in *)
(*   let t = parse_term (Sexp.of_string s) in *)
(*   (match type_check_with_tree t [] with *)
(*   |Some (t,s,p) -> Js.string (print_proof_tree p) *)
(*   |None -> (Js.string "Error")) *)



let affiche_interne s2 =
  let s = Js.to_string s2 in
  let t = parse_term (Sexp.of_string s) in
  let res = (match type_check_with_tree t [] with
  |Some (t,s,p) -> Js.string (print_proof_tree p)
  |None -> (Js.string "Error")) in
  res
  

(* We now export the function *)
let _ =
  Js.export "obj_typeur"
            (object%js
               method affiche x = affiche_interne x
             end)


