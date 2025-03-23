module Ast = Ast

let parse_channel c = Parser.main Lexer.lex (Lexing.from_channel c)
let parse_string s = Parser.main Lexer.lex (Lexing.from_string s)
