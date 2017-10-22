open Types

 
let rec term_to_latex (t : term) (varTable : string list) : string =
  match t with
  | Var n -> List.nth varTable n
  | Abs (name,st) -> "\\lambda " ^ name ^ "." ^ term_to_latex st (name :: varTable)
  | Appl (st1,st2) -> "(" ^ term_to_latex st1 varTable ^ "\\" ^ term_to_latex st2 varTable ^ ")"


let rec typ_to_latex (ty : typ) : string =
  match ty with
  | TVar s -> s
  | Arrow (ty1,ty2) -> "(" ^ typ_to_latex ty1 ^ "\rightarrow " ^ typ_to_latex ty2 ^ ")"


                                                                                      
