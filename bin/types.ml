

(* variable reference as string *)
type var = string

(* types and expressions *)
type ty and exp = 
| Var of var
| FunTy of var * ty * ty
| FunIntro of var * ty * exp
| FunElim of exp * exp
| N
| Zero
| Succ of exp
| ElimNat of exp * exp * exp * exp
| Ty

(* typing context - Gamma in notes *)
type context = (var * ty) list



