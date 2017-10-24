open Types

(* -------------------------------- Testing terms --------------------------------*)
let t1 = Abs("x",Abs("y",Appl(Var 0,Var 1)))
let ty1 = Arrow(TVar "Alpha", Arrow (Arrow (TVar "alpha", TVar "alpha"), TVar "alpha")) 
let g1 = {env = []; ter = t1; ty = ty1}

let t2 = Abs("y",Appl(Var 0,Var 0))
let ty2 = Arrow (Arrow (TVar "alpha", TVar "alpha"), TVar "alpha")
let g2 = {env = []; ter = t2; ty = ty2}

           
let tree = Node(One g1, Leaf (One g2))



               


               
