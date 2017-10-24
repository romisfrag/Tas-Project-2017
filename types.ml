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

type named_contexte =
  (string * typ) list

type substitution =
  (string * typ) list

                 
type goal =
  { ctxt :  named_contexte;
    ter : term ;
    ty : typ }


    
type proofTree =
  | Leaf of goal
  | Node of goal*(proofTree list)

