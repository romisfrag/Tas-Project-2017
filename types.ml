(* --------------------------- Types definitions -----------------------*)

type var_type_name = string

type typ =
  | TVar of var_type_name
  | Arrow of typ * typ

type sigma =
  | Typ of typ
  | Forall of var_type_name * sigma
                     
type term =
  | Var of int
  | Abs of string * term
  | Appl of term * term
  | Let of string * term * term
                     
type contexte =
  sigma list

type named_contexte =
  (string * sigma) list

type substitution =
  (string * typ) list

type substitutionSigma =
  (string * sigma) list

                 
type goal =
  { ctxt :  named_contexte;
    ter : term ;
    ty : typ }


    
type proofTree =
  | Leaf of goal
  | Node of goal*(proofTree list)

