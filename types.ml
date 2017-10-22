(* --------------------------- Types definitions -----------------------*)

type typ =
  | TVar of string
  | Arrow of typ * typ
                     
type term =
  | Var of int
  | Abs of string * term
  | Appl of term * term
                     
type contexte =
  typ list

type substitution =
  (string * typ) list

type environment =
  (string * term) list
                 
type goal =
  { env :  environment;
    ter : term ;
    ty : typ }


type goals =
  | One of goal
  | Two of (goal * goal)
    
type proofTree =
  | Leaf of goals
  | Node of (goals * proofTree)

