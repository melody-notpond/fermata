%{
  open Ast
%}

%token EOF
%token <int> INT
%token <Ast.ident> METHOD IDENT PTYPE
%token LET IN
%token MATCH COMATCH WITH END
%token DATA CODATA
%token EQ BAR THICKARROW COLON COMMA SEMICOLON
%token LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN

%start main
%type <Ast.prog> main
%type <Ast.pat * Ast.expr> pat

%nonassoc LET
%nonassoc METHOD

%%

pat:
  | BAR name=IDENT LPAREN args=separated_list(COMMA, IDENT) RPAREN
      THICKARROW e=expr
    { ({ name; args }, e) }

%inline method_arg_def:
  | n=IDENT COLON t=ty
    { (n, t) }

copat:
  | name=METHOD LPAREN
      args=separated_list(COMMA, method_arg_def) RPAREN
      COLON ret_ty=ty THICKARROW e=expr
    { ({ name; args; ret_ty }, e) }
  | name=METHOD LPAREN
      args=separated_list(COMMA, method_arg_def) RPAREN
      COLON ret_ty=ty THICKARROW p=copat
    { ({ name; args; ret_ty },
      EComatch { self = None; typed = None; methods = [p]}) }

expr:
  | i=INT
    { EInt i }
  | v=IDENT
    { EVar v }
  | LET name=IDENT EQ value=expr IN body=expr
    { ELet { name; value; body } } %prec LET
  | LBRACE methods=separated_list(SEMICOLON, copat) RBRACE
    { EComatch { self = None; typed = None; methods } }
  | COMATCH self=IDENT? LBRACE methods=separated_list(SEMICOLON, copat) RBRACE
    { EComatch { self = self; typed = None; methods } }
  | COMATCH self=IDENT? COLON typed=ty LBRACE
      methods=separated_list(SEMICOLON, copat) RBRACE
    { EComatch { self = self; typed = Some typed; methods } }
  | MATCH value=expr WITH branches=pat* END
    { EMatch { value; branches } }
  | name=IDENT LPAREN args=separated_list(COMMA, expr) RPAREN
    { EConstr { name; args } }
  | value=expr methd=METHOD LPAREN args=separated_list(COMMA, expr) RPAREN
    { EMethod { value; methd; args } }

ty:
  | name=IDENT
    { TyName (name, []) }
  | name=IDENT LBRACK params=separated_list(COMMA, ty) RBRACK
    { TyName (name, params) }
  | param=PTYPE
    { TyParam param }

%inline data_constr:
  | name=IDENT LPAREN args=separated_list(COMMA, ty) RPAREN
    { {name; args} }

%inline codata_method:
  | name=METHOD LPAREN args=separated_list(COMMA, ty) RPAREN
      COLON ret_type=ty
    { { name; args; ret_type } }

top:
  | DATA name=IDENT LBRACK tparams=separated_list(COMMA, PTYPE) RBRACK EQ BAR?
      variants=separated_list(BAR, data_constr)
    { TData { name; tparams; variants } }
  | DATA name=IDENT EQ BAR? variants=separated_list(BAR, data_constr)
    { TData { name; tparams = []; variants } }
  | CODATA name=IDENT EQ LBRACE
      methods=separated_list(SEMICOLON, codata_method) RBRACE
    { TCodata { name; tparams = []; methods } }
  | CODATA name=IDENT LBRACK tparams=separated_list(COMMA, PTYPE) RBRACK EQ
      LBRACE methods=separated_list(SEMICOLON, codata_method) RBRACE
    { TCodata { name; tparams; methods } }
  | LET name=IDENT COLON typed=ty EQ value=expr
    { TLet { name; typed; value } }

main:
  | tops=top* EOF
    { tops }

%%
