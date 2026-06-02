(* lambda.ml *)
open Printf;;

type term =
  | LVar of string               
  | LAbs of string * term        
  | LApp of term * term         
 

let rec print_term oc = function
  | LVar x -> fprintf oc "%s" x
  | LAbs (x, e) -> fprintf oc "(λ%s." x; print_term oc e; fprintf oc ")"
  | LApp (e1, e2) -> fprintf oc "("; print_term oc e1; fprintf oc " "; print_term oc e2; fprintf oc ")"


let rec var_libre = function
  | LVar x -> [x]
  | LAbs (x, e) -> let non_liee y = y <> x in
                     List.filter non_liee (var_libre e)
  | LApp (e1, e2) -> var_libre e1 @ var_libre e2


let nb_alias = ref 0

let alias x =
  nb_alias := !nb_alias + 1;
  x ^ string_of_int !nb_alias


let rec subst x v = function
  | LVar y ->
      let is_x = (y = x) in
      if is_x then v else LVar y
  | LApp (e1, e2) -> LApp (subst x v e1, subst x v e2)
  | LAbs (y, e) ->
      if y = x then LAbs (y, e)                       
      else if not (List.mem y (var_libre v)) then
        LAbs (y, subst x v e)
      else                                             
        let y2 = alias y in
        LAbs (y2, subst x v (subst y (LVar y2) e))


let rec reduc = function
  | LVar _ -> None
  | LApp (LAbs (x, e), arg) -> Some (subst x arg e)
  | LApp (e1, e2) ->
      (match reduc e1 with
       | Some e1' -> Some (LApp (e1', e2))
       | None -> match reduc e2 with
                     | Some e2' -> Some (LApp (e1, e2'))
                     | None -> None)
  | LAbs (x, e) ->
      (match reduc e with
       | Some e' -> Some (LAbs (x, e'))
       | None -> None)


let rec normalize e =
  match reduc e with
  | None -> e
  | Some e' -> normalize e'