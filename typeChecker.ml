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
let rec map_on_Typ (ty : typ) (f : string -> typ) : typ =
  match ty with
  | TVar s -> f s
  | Arrow (ty1,ty2) -> Arrow(map_on_Typ ty1 f,map_on_Typ ty2 f)



                            
let shift (t : term) =
  map_on_Var t (fun v bounded -> Var (if v < bounded then v else v + 1)) 0

(* substitution over types *)
let substitute (ty : typ) (s : substitution) : typ =
  map_on_Typ ty (fun name -> try (List.assoc name s) with _ -> TVar name)

(* concatenation of substitution *)
(* we first apply all the substitution of s2 over s1 before concatenation *)
let compsubst (s1 : substitution) (s2 : substitution) : substitution =
  (List.map (fun (name,ty) -> (name,substitute ty s2)) s1) @ s2
             

(* this function find the substitution betwen two types *)
let rec unify (ty1 : typ) (ty2 : typ) : substitution =
  match (ty1,ty2) with
  | (TVar a, _) -> [(a,ty2)]
  | (_,TVar a) -> [(a,ty1)]
  | (Arrow (ty11,ty12),Arrow (ty21,ty22)) -> unify ty11 ty21 @ unify ty12 ty22

                                                             
let rec type_equal (ty1 : typ) (ty2 : typ) : bool =
  match (ty1,ty2) with
  | (TVar n1,TVar n2) -> n1 = n2
  | (Arrow (ty11,ty12),Arrow(ty21,ty22)) -> type_equal ty11 ty21 && type_equal ty12 ty22
  | _ -> false



           
(* -------------------------- Type checker -------------------- *)

let rec type_check_rec (ter : term) (c : contexte) : (typ*substitution) option =
  match ter with
  (* Only seeking the var in the environment *)
  | Var n -> (try let retTy = List.nth c n in
                  Some (retTy, []) with
              | _ -> failwith "typeCheck Error : you must haven't give a close term")              
  | Abs (name,st) -> let freshType = TVar (gensym ()) in
                     bind (type_check_rec st (freshType :: c))
                          (fun (ty2,sub) -> Some (Arrow (substitute freshType sub, ty2),sub))
                          
  | Appl (st1,st2) -> let freshName = gensym () in
                      bind (type_check_rec st1 c)
                           (fun (ty1,sub) ->
                             let newCtxt = List.map (fun elem -> substitute elem sub) c in
                             bind (type_check_rec st2 newCtxt)
                                  (fun (ty2,sub2) ->
                                    let unification = unify (Arrow (ty2, TVar(freshName))) (substitute ty1 sub2) in
                                    let resType = try List.assoc freshName unification with _ -> TVar freshName in
                                    Some (resType, (compsubst unification (compsubst sub2 sub)))))



                           
let type_check (ter : term) : typ option =
  bind (type_check_rec ter [])
       (fun (ty,sub) -> Some ty)


(* functions that manipulate the tree *)
let substitute_in_goal (g : goal) (s : substitution) : goal =
  {ctxt = g.ctxt; ter = g.ter; ty = substitute g.ty s}

       
let rec substitute_in_tree (t : proofTree) (s : substitution) : proofTree =
  match t with
  | Leaf g -> Leaf (substitute_in_goal g s)                   
  | Node li -> let rec sub_rec (l : (goal * proofTree) list) : (goal * proofTree) list =
                 (match l with
                 | [] -> []
                 | (g,pT) :: next -> (substitute_in_goal g s, substitute_in_tree pT s) :: sub_rec next)
               in Node (sub_rec li)
       
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
                            let g = 
                            let tree =


                              Some (Arrow (substitute freshType sub, ty2),sub))    
  | _ -> failwith "lol"
                        
       
