type ident = string

type ty =
  | TyName of ident * ty list
  | TyParam of ident
  | TyVar of int

type copat =
  { destr: ident
  ; args: (ident * ty) list
  ; ret_ty: ty }

type pat =
  { constr: ident
  ; args: ident list }

and expr =
  | EVar of ident

  (* let NAME = VALUE in BODY *)
  | ELet of
    { name: ident
    ; value: expr
    ; body: expr }

  (* comatch SELF : TYPED with | COPAT => EXPR* end *)
  | EComatch of
    { self: ident
    ; typed: ty
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
  | EDestr of
    { value: expr
    ; message: ident
    ; args: expr list }

(* | NAME(ARGS) *)
type data_constr =
  { name: ident
  ; args: (ident option * ty) list }

(* .NAME(ARGS) : RET_TYPE *)
type codata_method =
  { name: ident
  ; args: (ident option * ty) list
  ; ret_type: ty }

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

type prog = top list
