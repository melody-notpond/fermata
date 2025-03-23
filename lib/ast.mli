type ident = string

type ty =
  | TyName of ident * ty list
  | TyParam of ident
  | TyVar of int

val string_of_ty : ty -> string

type copat =
  { name: ident
  ; args: (ident * ty) list
  ; ret_ty: ty }

val string_of_copat : copat -> string

type pat =
  { name: ident
  ; args: ident list }

val string_of_pat : pat -> string

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

val string_of_expr : expr -> string

(* | NAME(ARGS) *)
type data_constr =
  { name: ident
  ; args: ty list }

(* .NAME(ARGS) : RET_TYPE *)
type codata_method =
  { name: ident
  ; args: ty list
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

val string_of_top : top -> string

type prog = top list

val string_of_prog : prog -> string
