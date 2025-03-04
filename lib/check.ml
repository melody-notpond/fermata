open Ast

type ty_constraint =
  | CNone
  | CEq of ty

type context =
  { codatatypes: (string * (ident list * codata_method list)) list
  ; datatypes: (string * (ident list * data_constr list)) list
  ; constraints: ty_constraint list
  ; globals: (string * ty) list
  ; vars: (string * ty) list }

type 'a checker = context -> ('a * context, string) result

let (let*) (x: 'a checker) (f: 'a -> 'b checker) : 'b checker =
  fun s ->
  match x s with
  | Ok (v, s') -> f v s'
  | Error e -> Error e

let (>>=) = (let*)

let return (v: 'a) : 'a checker = fun s -> Ok (v, s)
let error (e: string) : 'a checker = fun _ -> Error e

let rec foldM (f: 'b -> 'a -> 'b checker) (acc: 'b) (xs: 'a list)
  : 'b checker =
  match xs with
  | [] -> return acc
  | x :: xs ->
    let* v = f acc x in
    foldM f v xs

let rec mapM (f: 'a -> 'b checker) (xs: 'a list) : 'b list checker =
  match xs with
  | [] -> return []
  | x :: xs ->
    let* v = f x in
    let* vs = mapM f xs in
    return @@ v :: vs

let forM (xs: 'a list) (f: 'a -> unit checker) : unit checker =
  foldM (fun () -> f) () xs

let get : context checker = fun s -> Ok (s, s)
let put (s: context) : unit checker = fun _ -> Ok ((), s)


let new_tyvar : ty checker =
  let* c = get in
  let v = List.length c.constraints in
  let* () = put { c with constraints = c.constraints @ [CNone] } in
  return @@ TyVar v

let rec occurs_check (var: ty) (c: ty) : unit checker =
  match var, c with
  | TyVar _, TyName (_, ts) -> forM ts @@ occurs_check var
  | TyVar i, TyVar j when i = j ->
    error @@ "$" ^ string_of_int i ^ " occurs in itself"
  | _, _ -> return ()

let add_constraint (t: ty) (c: ty_constraint) : unit checker =
  let* () =
    match c with
    | CEq t' -> occurs_check t t'
    | CNone -> return ()
  in match t with
  | TyVar i ->
    let* ctx = get in
    let rec replace xs i v =
      match i, xs with
      | _, [] -> failwith "cant replace nonexistent element"
      | 0, x :: xs -> x, v :: xs
      | _, x :: xs ->
        let (old, xs) = replace xs (i - 1) v in
        old, x :: xs
    in let (old, constraints) = replace ctx.constraints i c in
    begin match old with
    | CNone -> put { ctx with constraints = constraints }
    | _ -> error @@ "cannot overwrite constraint for $" ^ string_of_int i
    end
  | _ -> return ()

let rec initialise_type (map: (ident * ty) list ref) (t: ty) : ty checker =
  match t with
  | TyName (n, ts) ->
    let* ts = mapM (initialise_type map) ts in
    return @@ TyName (n, ts)
  | TyVar i -> return @@ TyVar i
  | TyParam p ->
    match List.assoc_opt p !map with
    | Some t -> return t
    | None ->
      let* v = new_tyvar in
      map := (p, v) :: !map;
      return v

let lookup_constr (cons: string) : (ty * data_constr) checker =
  let* c = get in
  let rec helper ds =
    match ds with
    | [] -> error @@ "could not find constructor " ^ cons
    | (ty, (params, constrs)) :: ds ->
      match List.find_opt
        (fun ({ name; _ }: data_constr) -> name = cons) constrs with
      | Some c ->
        let map = ref [] in
        let* t = initialise_type map @@ TyName
          (ty, List.map (fun p -> TyParam p) params) in
        let* args = mapM (fun (n, t) ->
          let* t = initialise_type map t in return (n, t)) c.args
        in return (t, { c with args })
      | None -> helper ds
  in helper c.datatypes

let lookup_destr (des: string) : (ty * codata_method) checker =
  let* c = get in
  let rec helper ds =
    match ds with
    | [] -> error @@ "could not find destructor " ^ des
    | (ty, (params, destrs)) :: ds ->
      match List.find_opt
        (fun ({ name; _ }: codata_method) -> name = des) destrs with
      | Some c ->
        let map = ref [] in
        let* t = initialise_type map @@ TyName
          (ty, List.map (fun p -> TyParam p) params) in
        let* args = mapM (fun (n, t) ->
          let* t = initialise_type map t in return (n, t)) c.args
        in let* ret_type = initialise_type map c.ret_type in
        return (t, { c with args; ret_type })
      | None -> helper ds
  in helper c.codatatypes

let push_var (x: string) (t: ty) : unit checker =
  let* c = get in
  put { c with vars = (x, t) :: c.vars }

let pop_var : unit checker =
  let* c = get in
  match c.vars with
  | [] -> failwith "tried to pop empty variable"
  | _ :: vars -> put { c with vars }

let lookup_var (x: string) : ty checker =
  let* c = get in
  match List.assoc_opt x c.vars with
  | Some t -> return t
  | None -> error @@ "variable `" ^ x ^ "` does not exist"

let rec lookup_tyvar (t: ty) : ty checker =
  match t with
  | TyVar i ->
    let* c = get in
    begin match List.nth c.constraints i with
    | CEq t' -> lookup_tyvar t'
    | CNone -> return t
    end
  | _ -> return t

let rec unify (t: ty) (t': ty) : ty checker =
  let* t = lookup_tyvar t in
  let* t' = lookup_tyvar t' in
  match t, t' with
  | TyName (n, p), TyName (n', p') when n = n' ->
    let* p = mapM (fun (t, t') -> unify t t') @@ List.combine p p' in
    return @@ TyName (n, p)
  | t, t' when t = t' -> return t
  | t, (TyVar _ as t') | (TyVar _ as t'), t ->
    let* () = add_constraint t' @@ CEq t in
    return t
  | _, _ -> error "types cannot be unified"

let rec check_expr (e: expr) : ty checker =
  match e with
  | EVar x -> lookup_var x
  | ELet { name; value; body } ->
    let* t = check_expr value in
    let* () = push_var name t in
    let* t = check_expr body in
    let* () = pop_var in
    return t
  | EComatch { self = _; typed = _; methods = [] } ->
    error "comatch must have at least one destructor defined"
  | EComatch { self; typed; methods } ->
    failwith "TODO"
  | EMatch { value = _; branches = [] } ->
    error "match must destruct at least one constructor"
  | EMatch { value; branches } ->
    let* t = check_expr value in
    failwith "TODO"
  | EConstr { name; args } ->
    let* (t, constr) = lookup_constr name in
    if List.length args <> List.length constr.args then
      error @@ "constructor `" ^ name ^ "` used with wrong number of arguments"
    else let zipped = List.combine args constr.args in
    let* () = forM zipped @@ fun (e, (_, t)) ->
      let* t' = check_expr e in
      let* _ = unify t' t in
      return ()
    in return t
  | EDestr { value; message; args } ->
    let* t = check_expr value in
    let* (t', destr) = lookup_destr message in
    let* _ = unify t t' in
    if List.length args <> List.length destr.args then
      error @@ "destructor `" ^ message ^
        "` used with wrong number of arguments"
    else let zipped = List.combine args destr.args in
    let* () = forM zipped @@ fun (e, (_, t)) ->
      let* t' = check_expr e in
      let* _ = unify t' t in
      return ()
    in return destr.ret_type

let check = ()
