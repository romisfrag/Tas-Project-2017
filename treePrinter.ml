open Types

(* Here we need to put a "bidon variable" like that it avoid some work *)
let rec term_to_latex_rec (t : term) (contexte : named_contexte) : string =
  match t with
  | Var n -> let (name,ty) = List.nth contexte n in name
  | Abs (name,st) -> "\\lambda " ^ name ^ "." ^ term_to_latex_rec st ((name,Typ (TVar "bidon")) :: contexte)
  | Appl (st1,st2) -> "(" ^ term_to_latex_rec st1 contexte ^ " \\ " ^ term_to_latex_rec st2 contexte ^ ")"
  | Let(name,st1,st2) -> "Let \\ " ^ name ^ " = " ^ term_to_latex_rec st1 contexte ^
                           " \\ in \\ " ^ term_to_latex_rec st2 ((name,Typ (TVar "bidon")) :: contexte)

let term_to_latex (t : term) : string =
  term_to_latex_rec t []

                                                                                               
let rec typ_to_latex (ty : typ) : string =
  match ty with
  | TVar s -> s
  | Arrow (ty1,ty2) -> "(" ^ typ_to_latex ty1 ^ "\\rightarrow " ^ typ_to_latex ty2 ^ ")"

let rec sigma_to_latex (s : sigma) : string =
  match s with
  | Typ t -> typ_to_latex t
  | Forall(name,ss) -> "\\forall " ^ name ^ ":" ^ sigma_to_latex ss
               
                                                                                       

let rec env_to_latex (ctxt : named_contexte) : string =
  match ctxt with
  | [] -> "\\emptyset"
  | (name,ty) :: next -> name ^ ":" ^ sigma_to_latex ty ^ ";" ^ env_to_latex next

                                                                              
let print_goal (g : goal) : string =
  env_to_latex g.ctxt ^ "\\vdash " ^ term_to_latex_rec g.ter g.ctxt^ ":" ^ sigma_to_latex g.ty

                                                                             
let rec print_proof_tree (p : proofTree) : string =
  match p with
  | Leaf g -> print_goal g
  | Node (g,l)  -> let rec print_liste_tree l =
                 match l with
                 | [] -> ""
                 | pt :: next -> print_proof_tree pt ^ "\\quad \\quad " ^ print_liste_tree next
                                   
                                     
               in
               "\\frac{" ^  print_liste_tree l ^ "}{" ^ print_goal g ^ "}"



                                                                                      
