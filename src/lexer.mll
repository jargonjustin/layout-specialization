{

open Ast
open Parser

exception Invalid_character of char

}

let digits = ['0'-'9']+
let entity = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let member = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let whitespace = [' ' '\t' '\n']

rule grammar = parse
 | "interface"    { INTERFACE }
 | "class"        { CLASS }
 | "def"          { DEF }
  
 | "inherit" whitespace+ '@' (member as attr)
 { ATTR_DECL (InheritDecl attr) }
 | "synthesize" whitespace+ '@' (member as attr)
 { ATTR_DECL (SynthesizeDecl attr)}
 
 | "float"        { FLOAT }
 
 | entity as id   { NAME id }
 | member as id   { IDENT id }
 | (member as field) '@' (member as attribute)
 { ATTR_REF (ChildAttrRef (field, attribute)) }
 | '@' (member as attribute)
 { ATTR_REF (SelfAttrRef attribute) }
 
 | (digits+ '.'?) as fnum           { NUMBER (float_of_string fnum) }
 | (digits* '.' digits+) as fnum    { NUMBER (float_of_string fnum) }
 
 | '{'   { LBRACE }
 | '}'   { RBRACE }
 | '('   { LPAREN }
 | ')'   { RPAREN }
 | ':'   { COLON }
 | ';'   { SEMICOLON }
 | ','   { COMMA }
 | '='   { ASSIGN }
 
 | '+'   { PLUS }
 | '-'   { MINUS }
 | '*'   { MULTIPLY }
 | '/'   { DIVIDE }
 
 | whitespace  { grammar lexbuf }
 | _ as c      { raise (Invalid_character c) }
 | eof         { EOF }
