(* translate.ml *)
open Pcfast
open Lambda


let rec translate_expr = function
  | Ident x -> LVar x
  | Fun(x,e) -> LAbs (x, translate_expr e)
  | App(e1,e2) -> LApp (translate_expr e1, translate_expr e2)
  | Let (x,e1,e2) -> LApp (LAbs (x, translate_expr e2), translate_expr e1)
  | If (a, b, c) -> LApp (LApp (translate_expr a, translate_expr b), translate_expr c)
  | Int n ->
      let rec body k =
        if k = 0 then LVar "x"
        else LApp (LVar "f", body (k - 1))
      in
      LAbs ("f", LAbs ("x", body n))

