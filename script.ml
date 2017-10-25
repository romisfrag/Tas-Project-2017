open Js_of_ocaml
open Types
open TypeChecker
open Parser
open Sexplib
open TreePrinter


let parse_check_return (s : string) : string  =
  let t = parse_term (Sexp.of_string s) in
  match type_check_with_tree t [] with
  |Some (t,s,p) -> print_proof_tree p
  |None -> "Error"





(* We now export the function *)
let _ =
  Js.export "typeur"
            (object%js
               method start_type s = parse_check_return s
             end)


