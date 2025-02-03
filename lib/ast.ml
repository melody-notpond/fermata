type ident = string

(* qualified identifiers *)
and qident = mident option * ident

(* qualified module names (includes functor applications) *)
and mident = (ident * mexpr list) list

(* top level constructions *)
and top =
  (* module definitions *)
  | TMod of ident * mexpr

  (* module signature definitions *)
  | TSig of ident * mtype

  (* function definitions *)
  | TFunc of {
    name: ident
  ; generics: ident list
  ; args: (ident * ty) list
  ; ret_type: ty
  ; stats: stat list }

  (* type definitions *)
  | TTypeDef of {
    name: ident
  ; generics: ident list
  ; constrs: constr_def list }

  (* type aliases *)
  | TTypeAlias of {
    name: ident
  ; generics: ident list
  ; equiv: ty }

  (* opening modules *)
  | TOpen of mident

(* definitions of constructors in type definitions *)
and constr_def =
  (* like a c style enum variant *)
  | CName of ident

  (* constructor with unnamed arguments *)
  | CConstr of ident * ty list

  (* record *)
  | CRecord of ident * (ident * ty) list

(* module expressions *)
and mexpr =
  (* name of a module *)
  | MName of mident

  (* module structures *)
  | MStruct of top list

  (* module functors *)
  | MFunctor of (ident * mtype) list * top list

  (* subtype module with a signature *)
  | MSubtype of mexpr * mtype

(* module types *)
and mtype =
  (* module names *)
  | MTyName of mident

  (* module signatures *)
  | MTySig of decl list

  (* module functor types *)
  | MTyFunctor of (ident * mtype) list * mtype

(* types *)
and ty =
  | TyName of qident * ty list
  | TyFunc of ty list * ty

(* declarations in a module signature *)
and decl =
  (* function declarations *)
  | DFunc of {
    name: ident
  ; generics: ident list
  ; args: (ident * ty) list
  ; ret_type: ty }

  (* type definitions *)
  | DTypeDef of {
    name: ident
  ; generics: ident list
  ; constrs: constr_def list }

  (* type aliases *)
  | DTypeAlias of {
    name: ident
  ; generics: ident list
  ; equiv: ty }

  (* type declarations *)
  | DType of {
    name: ident
  ; generics: ident list }

(* literals *)
and lit =
  (* integers *)
  | LInt of int

  (* booleans *)
  | LBool of bool

(* expressions *)
and expr =
  (* literals *)
  | ELit of lit

  (* variables *)
  | EVar of qident

  (* function calls *)
  | ECall of expr * expr list

  (* record construction *)
  | ERecord of qident * (ident * expr) list

  (* record projection *)
  | EAttr of expr * ident

(* statements *)
and stat =
  (* variable definition *)
  | SLet of {
    mut: bool
  ; name: pat
  ; value: expr
  ; elsy: stat option }

  (* variable assignment *)
  | SSet of {
    name: pat
  ; value: expr
  ; elsy: stat option }

  (* pattern matching statements *)
  | SMatch of {
    value: expr
  ; branches: (pat * stat list) list }

  (* if statements *)
  | SIf of {
    value: expr
  ; then_branch: stat list
  ; else_branch: stat list }

  (* function calls *)
  | SCall of {
    func: qident
  ; args: expr list }

  (* return statements *)
  | SReturn of expr

  (* local module opening *)
  | SOpen of mident

(* patterns *)
and pat =
  (* wildcard *)
  | PWildcard

  (* variable pattern *)
  | PVar of ident

  (* constructor pattern *)
  | PConstr of qident * pat list

  (* record pattern *)
  | PRecord of ident list

  (* all fields in a record *)
  | PRecordAll of qident

  (* literal pattern *)
  | PLit of lit

  (* either pattern *)
  | POr of pat * pat

(* the entire program *)
and prog = top list
