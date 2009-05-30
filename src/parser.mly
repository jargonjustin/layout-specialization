%{
open Ast

let partition_klass_body body =
   let rec partition body fields children defs =
      match body with
       | `Field f :: tl -> partition tl (f :: fields) children defs
       | `Child c :: tl -> partition tl fields (c :: children) defs
       | `Defin d :: tl -> partition tl fields children (d :: defs)
       | [] -> (fields, children, defs)
   in partition body [] [] []

%}

%token INTERFACE CLASS DEF
%token <Ast.attr_decl> ATTR_DECL
%token FLOAT
%token <string> NAME
%token <string> IDENT
%token <Ast.attr_ref> ATTR_REF
%token <float> NUMBER
%token LBRACE RBRACE LPAREN RPAREN COLON SEMICOLON COMMA ASSIGN
%token PLUS MINUS MULTIPLY DIVIDE
%token EOF

%left PLUS MINUS
%left MULTIPLY DIVIDE

%start grammar
%type <Ast.interface list * Ast.klass list> grammar

%%

/*** Attribute Grammars ***/

grammar     : entities EOF          { $1 }

entities    : entities interface    { let (ifaces, klasses) = $1 in ($2 :: ifaces, klasses) }
            | entities klass        { let (ifaces, klasses) = $1 in (ifaces, $2 :: klasses) }
            | /* empty */           { ([], []) }
            ;

/*** Interfaces ***/

interface   : INTERFACE NAME LBRACE attr_decls RBRACE SEMICOLON
            { Interface ($2, List.rev $4) }
            ;

attr_decls  : attr_decls ATTR_DECL SEMICOLON  { $2 :: $1 }
            | /* empty */                     { [] }
            ;

/*** Classes ***/

klass       : CLASS NAME interfaces LBRACE klass_body RBRACE SEMICOLON
            {
               let (fields, children, defs) = partition_klass_body $5 in
               {
                  name=$2;
                  interfaces=(List.rev $3);
                  fields=fields;
                  children=children;
                  definitions=defs;
               }
            }
            ;

interfaces  : COLON interfaces1        { $2 }
            | /* empty */              { [] }
            ;

interfaces1 : interfaces1 COMMA NAME   { $3 :: $1 }
            | NAME                     { [$1] }
            ;

klass_body  : klass_body body_mem      { $2 :: $1 }
            | /* empty */              { [] }
            ;

body_mem    : NAME IDENT SEMICOLON                 { `Child ($2, $1) }
            | FLOAT IDENT SEMICOLON                { `Field $2 }
            | DEF ATTR_REF ASSIGN expr SEMICOLON   { `Defin ($2, $4) }
            ;

expr        : NUMBER                { Literal $1 }
            | IDENT                 { FieldRef $1 }
            | ATTR_REF              { AttrRef $1 }
            | expr PLUS expr        { BinOp ("+", $1, $3) }
            | expr MINUS expr       { BinOp ("-", $1, $3) }
            | expr MULTIPLY expr    { BinOp ("*", $1, $3) }
            | expr DIVIDE expr      { BinOp ("/", $1, $3) }
            | LPAREN expr RPAREN    { $2 }
            ;
