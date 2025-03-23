type ident = string

type ty =
  | TyName of ident * ty list
  | TyParam of ident
  | TyVar of int

let separated_by (sep: string) (f: 'a -> string) (xs: 'a list) : string =
  match xs with
  | [] -> ""
  | x :: xs ->
    Printf.sprintf "%s%s" (f x) @@
    List.fold_left (fun a x -> Printf.sprintf "%s%s%s" a sep (f x)) "" xs

let id x = x

let rec string_of_ty t =
  match t with
  | TyName (n, []) -> n
  | TyName (n, ts) ->
    Printf.sprintf "%s[%s]" n @@ separated_by ", " string_of_ty ts
  | TyParam p -> "'" ^ p
  | TyVar v -> "$" ^ string_of_int v

type copat =
  { name: ident
  ; args: (ident * ty) list
  ; ret_ty: ty }

let string_of_copat {name; args; ret_ty} =
  Printf.sprintf ".%s(%s) : %s" name
  (separated_by ", " (fun (n, t) ->
    Printf.sprintf "%s: %s" n (string_of_ty t)) args) @@ string_of_ty ret_ty

type pat =
  { name: ident
  ; args: ident list }

let string_of_pat {name; args} =
  Printf.sprintf ".%s(%s)" name @@
  separated_by ", " id args

type expr =
  | EInt of int
  | EVar of ident

  (* let NAME = VALUE in BODY *)
  | ELet of
    { name: ident
    ; value: expr
    ; body: expr }

  (* comatch (SELF (: TYPED)?)? with | COPAT => EXPR* end *)
  | EComatch of
    { self: ident option
    ; typed: ty option
    ; methods: (copat * expr) list }

  (* match VALUE with | PAT => EXPR* end *)
  | EMatch of
    { value: expr
    ; branches: (pat * expr) list }

  (* NAME(ARGS) *)
  | EConstr of
    { name: ident
    ; args: expr list }

  (* VALUE.NAME(ARGS) *)
  | EMethod of
    { value: expr
    ; methd: ident
    ; args: expr list }

let rec string_of_expr e =
  match e with
  | EInt i -> string_of_int i
  | EVar v -> v
  | ELet { name; value; body } ->
    Printf.sprintf "let %s = %s in %s" name
    (string_of_expr value)
    (string_of_expr body)
  | EComatch { self; typed; methods=[] } ->
    Printf.sprintf "comatch %s%s { }"
    (Option.value ~default:"" self) @@
    Option.value ~default:"" (Option.map
      (fun t -> Printf.sprintf ": %s" @@ string_of_ty t) typed)
  | EComatch { self; typed; methods=ms } ->
    Printf.sprintf "comatch %s%s\n{ %s }"
    (Option.value ~default:"" self)
    (Option.value ~default:"" (Option.map
      (fun t -> Printf.sprintf ": %s" @@ string_of_ty t) typed)) @@
    separated_by "\n; " (fun (p, e) ->
      Printf.sprintf "%s => %s" (string_of_copat p) (string_of_expr e)) ms
  | EMatch { value; branches } ->
    Printf.sprintf "match %s with%s\nend" (string_of_expr value) @@
    List.fold_left (fun a (p, e) ->
      Printf.sprintf "%s\n| %s => %s" a (string_of_pat p) (string_of_expr e))
      "" branches
  | EConstr { name; args } ->
    Printf.sprintf "%s(%s)" name @@ separated_by ", " string_of_expr args
  | EMethod { value; methd; args } ->
    Printf.sprintf "%s.%s(%s)" (string_of_expr value) methd @@
    separated_by ", " string_of_expr args

(* | NAME(ARGS) *)
type data_constr =
  { name: ident
  ; args: ty list }

let string_of_data_constr {name; args} =
  Printf.sprintf "%s(%s)" name @@ separated_by ", " string_of_ty args

(* .NAME(ARGS) : RET_TYPE *)
type codata_method =
  { name: ident
  ; args: ty list
  ; ret_type: ty }

let string_of_codata_method {name; args; ret_type} =
  Printf.sprintf ".%s(%s) : %s" name (separated_by ", " string_of_ty args) @@
  string_of_ty ret_type

type top =
  (* data NAME TPARAMS = VARIANTS *)
  | TData of
    { name: ident
    ; tparams: ident list
    ; variants: data_constr list }

  (* codata NAME TPARAMS = { METHODS* } *)
  | TCodata of
    { name: ident
    ; tparams: ident list
    ; methods: codata_method list }

  (* let NAME : TYPED = VALUE *)
  | TLet of
    { name: ident
    ; typed: ty
    ; value: expr }

let string_of_top t =
  match t with
  | TData { name; tparams=[]; variants } ->
    Printf.sprintf "data %s =%s" name @@
    List.fold_left (fun a v ->
      Printf.sprintf "%s\n  | %s" a @@ string_of_data_constr v) "" variants
  | TData { name; tparams=ps; variants } ->
    Printf.sprintf "data %s[%s] =%s" name
    (separated_by ", " id ps) @@
    List.fold_left (fun a v ->
      Printf.sprintf "%s\n  %s" a @@ string_of_data_constr v) "" variants
  | TCodata { name; tparams=[]; methods=[] } ->
    Printf.sprintf "codata %s = { }" name
  | TCodata { name; tparams; methods=[] } ->
    Printf.sprintf "codata %s[%s] = { }" name @@
    separated_by ", " id tparams
  | TCodata { name; tparams=[]; methods } ->
    Printf.sprintf "codata %s =\n{ %s }" name @@
    separated_by "\n; " string_of_codata_method methods
  | TCodata { name; tparams; methods } ->
    Printf.sprintf "codata %s[%s] =\n{ %s }" name
    (separated_by ", " id tparams) @@
    separated_by "\n; " string_of_codata_method methods
  | TLet { name; typed; value } ->
    Printf.sprintf "let %s: %s = %s" name
    (string_of_ty typed) @@
    string_of_expr value

type prog = top list

let string_of_prog p = separated_by "\n\n" string_of_top p
