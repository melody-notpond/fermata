{
  (* open Lexing *)
  open Parser

  let keyword_table = Hashtbl.create 53
  let () = List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "let", LET
    ; "in", IN
    ; "match", MATCH
    ; "comatch", COMATCH
    ; "with", WITH
    ; "end", END
    ; "data", DATA
    ; "codata", CODATA ]
}

rule lex = parse
  | eof
    { EOF }
  | ' ' | '\t' | '\n' | '\r'
    { lex lexbuf }
  | "(*"
    { comment 1 lexbuf }
  | '{'
    { LBRACE }
  | '}'
    { RBRACE }
  | '['
    { LBRACK }
  | ']'
    { RBRACK }
  | '('
    { LPAREN }
  | ')'
    { RPAREN }
  | '='
    { EQ }
  | ':'
    { COLON }
  | '|'
    { BAR }
  | ';'
    { SEMICOLON }
  | "=>"
    { THICKARROW }
  | ','
    { COMMA }
  | ['0'-'9']+ as i
    { INT (int_of_string i) }
  | '\'' (['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as t)
    { PTYPE t }
  | '.' (['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as m)?
    { match m with
      | Some m -> METHOD m
      | None -> METHOD "" }
  | (['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id)
    { try
        Hashtbl.find keyword_table id
      with Not_found -> IDENT id }
  | _ as c
    { failwith (Printf.sprintf "unexpected character: %C" c) }
and comment level = parse
  | "(*"
    { comment (level + 1) lexbuf }
  | "*)"
    { if level > 1 then
        comment (level - 1) lexbuf
      else lex lexbuf }
  | _
    { comment level lexbuf }
