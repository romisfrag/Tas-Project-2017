open Types

 
let rec term_to_latex_rec (t : term) (varTable : string list) : string =
  match t with
  | Var n -> List.nth varTable n
  | Abs (name,st) -> "\\lambda " ^ name ^ "." ^ term_to_latex_rec st (name :: varTable)
  | Appl (st1,st2) -> "(" ^ term_to_latex_rec st1 varTable ^ " \\ " ^ term_to_latex_rec st2 varTable ^ ")"

let term_to_latex (t : term) : string =
  term_to_latex_rec t []

                                                                                               
let rec typ_to_latex (ty : typ) : string =
  match ty with
  | TVar s -> s
  | Arrow (ty1,ty2) -> "(" ^ typ_to_latex ty1 ^ "\\rightarrow " ^ typ_to_latex ty2 ^ ")"


let rec env_to_latex (env : environment) : string =
  match env with
  | [] -> "\\emptyset"
  | (name,ter) :: next -> name ^ ":" ^ term_to_latex ter ^ ";" ^ env_to_latex next

                                                                              
let print_goal (g : goal) : string =
  env_to_latex g.env ^ "\\vdash" ^ term_to_latex g.ter ^ ":" ^ typ_to_latex g.ty

let print_goals (g : goals) : string =
  match g with
  | One g -> print_goal g
  | Two (g1,g2) -> print_goal g1 ^ "\\quad" ^ print_goal g2
                                                                            
let rec print_proof_tree (p : proofTree) : string =
  match p with
  | Leaf g -> print_goals g
  | Node (g,next) -> "\\frac{" ^ print_goals g ^ "}{" ^ print_proof_tree next ^ "}"

                                                                                      
