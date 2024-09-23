open Types


exception TypeError of string

let type_error (err : string) = 
  raise (TypeError (Printf.sprintf "TypeError: %s" err))

let rec replace_var (x: Types.var) (e: Types.exp) (target : Types.ty) : Types.ty = 
  match target with
  | _ -> type_error "no implement"


let rec infer_type (ctxt: Types.context) (e : Types.exp) : Types.ty =
  match e with
  | Var(x) -> 
    begin
      match List.assoc_opt x ctxt with
      | Some(ty) -> ty
      | None -> type_error "unbound variable"
    end
  | FunTy(x, t1, t2) -> 
      if (infer_type ctxt t1 == Ty) then
        if (infer_type (x, t1)::ctxt t2 == Ty)
        then Ty
        else type_error "function type output needs to be type type"
      else type_error "function type input needs to be type type"
  | FunIntro(x, t1, e1) ->
    if (infer_type ctxt t1 == Ty) then
      let t2 = infer_type (x, t1)::ctxt e1 in
      if (infer_type (x, t1)::ctxt t2 == Ty) then
          FunTy(x, t1, t2)
      else type_error "function lambda output needs to be type type"
    else type_error "function lambda input needs to be type type"
  | FunElim(e1, e2) -> 
    match infer_type ctxt e1 with
      | FunTy(x, t1, t2) -> 
        if (infer_type ctxt e2 == t1)
          then replace_var x e2 t2
        else type_error "expected operand in funelim to have type of arg in funty"
      | _ -> type_error "expected operator in funelim to have type funty"
  | N -> Ty
  | Zero -> N
  | Succ(e1) ->
    begin
      match infer_type ctxt e1 with
      | N -> N
      | _ -> type_error "succ expects natural as argument"
    end
  | ElimNat(e1, e2, e3, e4) ->
    

let rec type_check (ctxt: Types.context) (e : Types.exp) : unit = 
  type_error "no imp"