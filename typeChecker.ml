open Types
      
(* -------------------------- utility functions ------------------------ *)

(* for a better use of the option monade *)
let bind (t : 'a option) (f : 'a -> 'b) : 'b =
  match t with
  | Some e -> f e
  | None -> None
              
              
let r = ref 0              
let gensym () =
  let () = r := !r + 1 in "V" ^  (string_of_int !r)

let string_equal (s1 : string)  (s2 : string) : bool  =
  String.compare s1 s2 = 0
                                                             
(*--------------------------- Transformation functions ------------------ *)                 
(* map functions over types and terms *)
let rec map_on_Var (t : term) (f : int -> int -> term) (nBounded : int) : term =
  match t with
  | Var n -> f n nBounded
  | Abs (name,st) -> Abs (name,(map_on_Var st f (nBounded + 1)))
  | Appl (st1,st2) -> Appl( (map_on_Var st1 f (nBounded)),(map_on_Var st2 f (nBounded)) )
  | Let (name,st1,st2) -> Let(name,(map_on_Var st1 f (nBounded)),(map_on_Var st2 f (nBounded + 1)))
let rec map_on_Typ (ty : typ) (f : string -> typ) : typ =
  match ty with
  | TVar s -> f s
  | Arrow (ty1,ty2) -> Arrow(map_on_Typ ty1 f,map_on_Typ ty2 f)



(* substitution over types *)
(* let substitute (ty : typ) (s : substitution) : typ = *)
(*   map_on_Typ ty (fun name -> try (List.assoc name s) with _ -> TVar name) *)
             
let rec substitute (ty : typ) (s : substitution) : typ =
  let res = map_on_Typ ty (fun name -> try (List.assoc name s) with _ -> TVar name) in
  if res = ty then res else substitute res s 
          
(* concatenation of substitution *)
(* we first apply all the substitution of s2 over s1 before concatenation *)
let compsubst (s1 : substitution) (s2 : substitution) : substitution =
  (List.map (fun (name,ty) -> (name,substitute ty s2)) s1) @ s2

(* type substitution over sigma *)
let rec substitute_sigma (s : sigma) (sub : substitution) : sigma =
  match s with
  | Typ t -> Typ (substitute t sub)
  | Forall(name,si) -> Forall(name,substitute_sigma si sub)




                             
(* Functions to determine freeVars of a term *)
let rec get_var_type (ty: typ) : string list =
  match ty with
  | TVar n -> [n]
  | Arrow (t1,t2) -> (get_var_type t1) @ (get_var_type t2)
  

let substract l1 l2 =
  List.flatten (List.map (function id -> if (List.mem id l2)
                                         then []
                                         else [id])
                         l1);;    
let free_var_type (ty : typ) (l : string list) : string list =
  substract (get_var_type ty) l
let rec free_vars_sigma_rec (s : sigma) (l : string list) : string list =
  match s with
  | Typ t -> free_var_type t l
  | Forall(name,st) -> free_vars_sigma_rec st (name :: l)
let free_vars_sigma (s : sigma) : string list =
  free_vars_sigma_rec s []
let free_vars_contexte (c : contexte) : string list =
  List.flatten (List.map (fun s -> free_vars_sigma s) c)





(*---------------------------Instanciation - generalization ------------------------- *)
           
(* instanciation definition *)
(* for now i think that instanciate just mean to switch the name of this with new var name *)
let rec instanciate_rec (si : sigma) (sub : substitution) : typ =
  match si with
  | Typ t -> substitute t sub
  | Forall(name,s) -> let freshVar = TVar (gensym()) in instanciate_rec s ((name,freshVar) :: sub)
let instanciate (si : sigma) : typ =
  instanciate_rec si []
 
(* generalization definition *)
let rec generalyze (ty : typ) (c : contexte) : sigma =
  let vars = get_var_type ty in
  let fvarsCtxt = free_vars_contexte c in
  let varToGeneralyze = substract vars fvarsCtxt in
  let rec generalyze_rec (ty : typ) (l : string list) : sigma =
    (match l with
    | [] -> Typ ty
    | s :: next -> Forall(s,generalyze_rec ty next)) in
  generalyze_rec ty varToGeneralyze
    


               


(*--------------------------------Unification ---------------------*)                             
let occur (v : string) (t : typ) : bool = 
  let rec occ_rec (t : typ) : bool =
    match t with 
      TVar b -> b=v
    | Arrow(oT1,oT2) -> occ_rec oT1 || occ_rec oT2
  in occ_rec t
                                                               
(* this function find the substitution betwen two types *)
let rec unify (ty1 : typ) (ty2 : typ) : substitution =
  match (ty1,ty2) with
  | (TVar a, ty2) -> if ty1 = ty2 then []
                   else if occur a ty2 then failwith "Unfiy fail"
                   else [a,ty2]
  | (ty1,TVar a) -> if ty1 = ty2 then []
                   else if occur a ty1 then failwith "Unfiy fail"
                   else [a,ty1]
  | (Arrow (ty11,ty12),Arrow (ty21,ty22)) ->
     let s=unify ty11 ty21  in
          compsubst (unify (substitute ty12 s)
                             (substitute ty22 s)) s
                                                                     
                                                             
let rec type_equal (ty1 : typ) (ty2 : typ) : bool =
  match (ty1,ty2) with
  | (TVar n1,TVar n2) -> n1 = n2
  | (Arrow (ty11,ty12),Arrow(ty21,ty22)) -> type_equal ty11 ty21 && type_equal ty12 ty22
  | _ -> false






  
                  
(* -------------------------- Type checker -------------------- *)

let rec type_check_rec (ter : term) (c : contexte) : (typ*substitution) option =
  match ter with
  (* Only seeking the var in the environment *)
  | Var n -> (try let retTy = instanciate (List.nth c n) in                  
                  Some (retTy, []) with
              | _ -> failwith "typeCheck Error : you must haven't give a close term")
               
  | Abs (name,st) -> let freshType = (TVar (gensym ())) in
                     bind (type_check_rec st (Typ (freshType) :: c))
                          (fun (ty2,sub) ->
                            Some (Arrow (substitute freshType sub, ty2),sub))
                          
  | Appl (st1,st2) -> let freshName = gensym () in
                      bind (type_check_rec st1 c)
                           (fun (ty1,sub) ->
                             let newCtxt = List.map (fun elem -> substitute_sigma elem sub) c in
                             bind (type_check_rec st2 newCtxt)
                                  (fun (ty2,sub2) ->
                                    let unification = unify (Arrow (ty2, TVar(freshName))) (substitute ty1 sub2) in
                                    let resType = try List.assoc freshName unification with _ -> TVar freshName in
                                    Some (resType, (compsubst unification (compsubst sub2 sub)))))
                           
  | Let(name,st1,st2) -> bind (type_check_rec st1 c)
                              (fun (ty1,sub) ->
                                let genTy1 = generalyze ty1 c in
                                type_check_rec st2 (genTy1 :: c))
                                
                                                



                           
let type_check (ter : term) : typ option =
  bind (type_check_rec ter [])
       (fun (ty,sub) -> Some ty)


(* functions that manipulate the tree *)
let substitute_in_goal (g : goal) (s : substitution) : goal =
  let newCtxt = List.map (fun (name,elem) -> (name,substitute_sigma elem s)) g.ctxt in
  {ctxt = newCtxt; ter = g.ter; ty = substitute_sigma g.ty s}

       
let rec substitute_in_tree (t : proofTree) (s : substitution) : proofTree =
  match t with
  | Leaf g -> Leaf (substitute_in_goal g s)
  | Node (g,li) -> let rec sub_rec (l : proofTree list) : proofTree list =
                 (match l with
                 | [] -> []
                 | pT :: next -> substitute_in_tree pT s :: sub_rec next)
                   in let newG = substitute_in_goal g s in
                      Node (newG, sub_rec li)







let rec type_check_with_tree (ter : term) (c : named_contexte) : (typ*substitution*proofTree) option =
  match ter with
  (* Only seeking the var in the environment *)
  | Var n -> (try let (name,retTyG) = List.nth c n in 
                  let retTy = instanciate retTyG in
                  let g = {ctxt = c; ter = Var n; ty = Typ retTy} in
                  let tree = Leaf g in
                  Some (retTy, [],tree) with
              | _ -> failwith "typeCheck Error : you must haven't give a close term")

               
  | Abs (name,st) -> let freshType = (TVar (gensym ())) in
                     bind (type_check_rec st (Typ (freshType) :: c))
                          (fun (ty2,sub) ->
                            Some (Arrow (substitute freshType sub, ty2),sub))
                          
  | Appl (st1,st2) -> let freshName = gensym () in
                      bind (type_check_rec st1 c)
                           (fun (ty1,sub) ->
                             let newCtxt = List.map (fun elem -> substitute_sigma elem sub) c in
                             bind (type_check_rec st2 newCtxt)
                                  (fun (ty2,sub2) ->
                                    let unification = unify (Arrow (ty2, TVar(freshName))) (substitute ty1 sub2) in
                                    let resType = try List.assoc freshName unification with _ -> TVar freshName in
                                    Some (resType, (compsubst unification (compsubst sub2 sub)))))
                           
  | Let(name,st1,st2) -> bind (type_check_rec st1 c)
                              (fun (ty1,sub) ->
                                let genTy1 = generalyze ty1 c in
                                type_check_rec st2 (genTy1 :: c))



                           
                           
let rec type_check_with_tree (ter : term) (c : named_contexte) : (typ*substitution*proofTree) option =
  match ter with
  | Var n -> (try let (name,retTy) = List.nth c n in
                  let g = {ctxt = c; ter = Var n; ty = retTy} in
                  let tree = Leaf g in
                  Some (retTy, [],tree) with
              | _ -> failwith "typeCheck Error : you must haven't give a close term")
  | Abs (name,st) -> let freshType = TVar (gensym ()) in
                     bind (type_check_with_tree st ((name, freshType) :: c))
                          (fun (ty2,sub,retTree) ->
                            let newType = Arrow (substitute freshType sub, ty2) in
                            let g = {ctxt = c; ter = Abs(name,st); ty = newType} in
                            let tree = Node (g,[substitute_in_tree retTree sub]) in
                            Some (newType,sub,tree))
  | Appl(t1,t2) -> let resT1 = type_check_with_tree t1 c in
                   bind (resT1)
                        (fun (typT1,sub1,retTree1) ->
                          let newCtxt = List.map (fun (name,elem) -> (name,substitute elem sub1)) c in
                          let resT2 = type_check_with_tree t2 newCtxt in
                          bind (resT2)
                               (fun (typT2,sub2,retTree2) ->
                                 bind (type_check_with_tree t2 newCtxt)
                                      (fun (typT2,sub2,retTree2) ->
                                        let freshName = gensym () in
                                        let subUnif = unify (Arrow (typT2, TVar(freshName))) (substitute typT1 sub2) in
                                        let resType = try List.assoc freshName subUnif with _ -> TVar freshName in
                                        let newSub = compsubst subUnif (compsubst sub2 sub1) in
                                        (* Now constructing the tree node *)
                                        let g = {ctxt = c; ter = Appl(t1,t2); ty = resType} in
                                        let newTree1 = substitute_in_tree retTree1 newSub in
                                        let newTree2 = substitute_in_tree retTree2 newSub in
                                        let resTree = Node (g,[newTree1;newTree2]) in
                                        Some (resType,newSub,resTree)
                                      )
                               )
                        )

  | _ -> failwith "lol"
       
