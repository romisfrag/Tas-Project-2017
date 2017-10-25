open Types
open Sexplib

(* for top level
#require "sexplib" *)
       
let rec parse_term_rec (env : string list) (t : Sexplib.Sexp.t) : term =
  let rec get_number_var_rec (s : string) (n : int) (l : string list) : int =
    (match l with
    | [] -> failwith ("parser variable " ^ s ^ " not declared\n")
    | str :: next -> if s = str then n else get_number_var_rec s (n + 1) next) in
  let get_number (s : string) (l : string list) : int =
    get_number_var_rec s 0 l in
  match t with
  (*Abstraction rule *)
  | Sexp.List [Sexp.Atom "lambda"; Sexp.Atom var; body] ->
     Abs(var,(parse_term_rec (var :: env)) body)
  (* let rule *)
  | Sexp.List [Sexp.Atom "let"; Sexp.Atom var; Sexp.Atom "="; body1; Sexp.Atom "in"; body2] ->
     Let(var,(parse_term_rec env body1),(parse_term_rec (var :: env) body2))
  (* Application rule *)
  | Sexp.List (f :: args) ->
     List.fold_left
       (fun x y -> Appl (x, y))
       (parse_term_rec env f) 
       (List.map (parse_term_rec env) args)
  (* Var rule *)
  | Sexp.Atom var -> Var(get_number var env)
  | _ -> failwith ("Parseur unbound counstruction" ^ Sexp.to_string t ^ "\n")                                                 

let parse_term (t : Sexplib.Sexp.t) : term =
  parse_term_rec [] t
                  
