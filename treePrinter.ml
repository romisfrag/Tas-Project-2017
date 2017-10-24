open Types

(* Here we need to put a "bidon variable" like that it avoid some work *)
let rec term_to_latex_rec (t : term) (contexte : named_contexte) : string =
  match t with
  | Var n -> let (name,ty) = List.nth contexte n in name
  | Abs (name,st) -> "\\lambda " ^ name ^ "." ^ term_to_latex_rec st ((name,TVar "bidon") :: contexte)
  | Appl (st1,st2) -> "(" ^ term_to_latex_rec st1 contexte ^ " \\ " ^ term_to_latex_rec st2 contexte ^ ")"

let term_to_latex (t : term) : string =
  term_to_latex_rec t []

                                                                                               
let rec typ_to_latex (ty : typ) : string =
  match ty with
  | TVar s -> s
  | Arrow (ty1,ty2) -> "(" ^ typ_to_latex ty1 ^ "\\rightarrow " ^ typ_to_latex ty2 ^ ")"


let rec env_to_latex (ctxt : named_contexte) : string =
  match ctxt with
  | [] -> "\\emptyset"
  | (name,ty) :: next -> name ^ ":" ^ typ_to_latex ty ^ ";" ^ env_to_latex next

                                                                              
let print_goal (g : goal) : string =
  env_to_latex g.ctxt ^ "\\vdash " ^ term_to_latex_rec g.ter g.ctxt^ ":" ^ typ_to_latex g.ty

                                                                             
let rec print_proof_tree (p : proofTree) : string =
  match p with
  | Leaf g -> print_goal g
  | Node l  -> let rec print_liste_tree l =
                 match l with
                 | [] -> ""
                 | (g,pt) :: next -> "\\frac{" ^ print_proof_tree pt ^ "}{" ^ print_goal g ^ "} \\quad \\quad"
                                     ^ print_liste_tree next
               in
               print_liste_tree l 



                                                                                      
