%{
  open Types
  open Parser_ctx
  open Syntax
  open Util
%}

%token <int> INT
%token <string> STRING
%token SEMICOLON COMMA COLON ARROW DOT AT QUOT
%token SPACE
%token ASSIGN
%token CONS
%token LBRACE RBRACE LPAREN RPAREN LSBRAC RSBRAC LVECT RVECT 
%token PLUS MINUS MULTIPLY DIVIDE REMAINDER
%token EQUAL UNEQUAL LESS MORE MOREEQ LESSEQ
%token AND OR NOT
%token IF THEN ELSE ELIF
%token WHILE BREAK CONTINUE 
%token XORB ORB ANDB INVB SHR SHL
%token <string> IDENT
%token TRUE FALSE 
%token DEF
%token TYPE OF 
%token RET
%token IMPORT EXTERN LITERAL
%token TYPE_FUN TYPE_VOID TYPE_INT TYPE_BOOL TYPE_STR
%token LOCAL
%token EOF

%nonassoc OF
%nonassoc IMPORT 
%nonassoc IF THEN ELSE
%nonassoc LOCAL
%right COMMA SEMICOLON
%right ASSIGN 
%nonassoc WHILE 
%right RET
%nonassoc TRUE FALSE
%right CONS
%left OR
%left AND
%nonassoc EQUAL UNEQUAL LESS MORE MOREEQ LESSEQ
%nonassoc NOT
%left ANDB ORB XORB INVB SHR SHL
%left PLUS MINUS 
%left MULTIPLY DIVIDE REMAINDER
%left NEG	/* negation -- unary minus */
%nonassoc DOT

%start toplevel
%type <Syntax.ast_top> toplevel

%%

toplevel:
    | EOF                               { ast_module [] }
    | definitions                       { ast_module $1 }
    | macro                             { $1 (ast_module []) } 
    | macro toplevel                    { $1 ($2) }

definitions:   func_def                 { $1 :: [] }
             | func_def definitions     { $1 :: $2 }
             | typedef                  { $1 :: [] }
             | typedef definitions      { $1 :: $2 }

func_def: DEF IDENT LPAREN func_args RPAREN func_ret block { ast_func_def $2 $4 $6 $7 }

func_args:                              { [] }
         | func_arg                     { $1 :: [] }
         | func_arg COMMA func_args     { $1 :: $3 }

func_arg: IDENT                         { ($1, TVar(uniq_id ()) ) }
        | IDENT COLON typespec          { ($1, $3) } 

func_ret:                               { TVar(uniq_id ()) }
        | COLON typespec                { $2 }

typedef: record_def                     { $1 }

record_def:
        TYPE IDENT LBRACE
                   record_def_fields
                   RBRACE               {  ast_type_def $2 (TRecord($4)) }

record_def_fields: IDENT COLON typespec       { ($1, $3) :: [] }
                 | IDENT COLON typespec 
                   COMMA 
                   record_def_fields          { ($1, $3) :: $5 }

block: LBRACE statements RBRACE         { ast_block $2 }

statements:                                  { [] }
          | statement SEMICOLON statements   { $3 @ $1 }
          | control statements               { $2 @ $1 }

control: while_stmt                          { $1 :: [] }
         | if_stmt                           { $1 :: [] }

while_stmt: WHILE expression code           { ast_stmt_while $2 $3 }

if_stmt: IF expr THEN code
         elif_part
         else_part                           { ast_stmt_if ($2,$4) $5 $6 }

elif_part:                                   { [] }
        | ELIF expr THEN code elif_part      { ($2, $4) :: $5 }

else_part:                                   { ast_block (ast_stmt_empty :: []) }
       | ELSE code                           { $2  }

code:  statement SEMICOLON                   { ast_block $1 } 
     | block                                 { $1 }


statement:                                   { ast_stmt_empty :: [] }
        | local_stmt                         { $1 }
        | assign_stmt                        { $1 :: [] }
        | void_call_stmt                     { $1 :: [] }
        | ret_stmt                           { $1 :: [] }
        | break_stmt                         { $1 :: [] }
        | continue_stmt                      { $1 :: [] }

assign_stmt: expression ASSIGN expression    { ast_stmt_assign $1 $3 } 

void_call_stmt: call_expr                    { ast_stmt_call $1 }

ret_stmt: RET                                { ast_stmt_ret ENothing }
        | RET expression                     { ast_stmt_ret $2 }

break_stmt:    BREAK                         { ast_stmt_break }
continue_stmt: CONTINUE                      { ast_stmt_continue }

expression: uncallable_exp                   { $1 }
          | callable_exp                     { $1 } 

literal:
        INT                                  { ast_expr_lit (LInt $1) }
      | TRUE                                 { ast_expr_lit (LBool true) }
      | FALSE                                { ast_expr_lit (LBool false) }
      | STRING                               { ast_expr_lit (LString $1) }

uncallable_exp:
              | literal                      { $1 }
              | ariph_expr                   { $1 }
              | bool_expr                    { $1 }
              | cmp_expr                     { $1 }
              | list_expr                    { $1 }
              | pair_expr                    { $1 }
              | record_ctor_expr             { $1 }
              | quot                         { $1 }

callable_exp:
          | record_field                     { $1 }
          | LPAREN expression RPAREN         { $2 }
          | IDENT                            { ast_expr_ident $1 }
          | call_expr                        { $1 }

expr: expression                             { $1 }

ariph_expr:
	| MINUS expr %prec NEG		 { ast_expr_ariph_un  Minus $2    }
	| expr PLUS      expr		 { ast_expr_ariph_bin Plus  $1 $3 }
	| expr MINUS     expr		 { ast_expr_ariph_bin Minus $1 $3 }
	| expr MULTIPLY  expr		 { ast_expr_ariph_bin Mul   $1 $3 } 
	| expr DIVIDE    expr		 { ast_expr_ariph_bin Div   $1 $3 } 
	| expr REMAINDER expr		 { ast_expr_ariph_bin Mod   $1 $3 } 
	| expr ANDB      expr		 { ast_expr_ariph_bin BAnd  $1 $3 } 
	| expr ORB       expr		 { ast_expr_ariph_bin BOr   $1 $3 } 
	| expr XORB      expr		 { ast_expr_ariph_bin BXor  $1 $3 }
	| expr SHR       expr		 { ast_expr_ariph_bin BShr  $1 $3 }
	| expr SHL       expr		 { ast_expr_ariph_bin BShl  $1 $3 }
	| INVB expr          		 { ast_expr_ariph_un  BInv  $2    }

bool_expr:
	| NOT expr                   { ast_expr_bool_un  Not     $2    }
	| expr AND expr              { ast_expr_bool_bin And     $1 $3 }
	| expr OR expr               { ast_expr_bool_bin Or      $1 $3 }

cmp_expr:
	| expr EQUAL     expr		 { ast_expr_cmp Equal   $1 $3 }
	| expr UNEQUAL   expr		 { ast_expr_cmp Unequal $1 $3 }
	| expr MORE      expr		 { ast_expr_cmp More    $1 $3 }
	| expr LESS      expr		 { ast_expr_cmp Less    $1 $3 }
	| expr MOREEQ    expr		 { ast_expr_cmp MoreEq  $1 $3 }
	| expr LESSEQ    expr		 { ast_expr_cmp LessEq  $1 $3 }

record_ctor_expr: LBRACE IDENT COLON record_ctor_fields RBRACE { ast_expr_rec_ctor $2 $4 }

record_ctor_fields:
                  record_ctor_field        { $1 :: [] }
                | record_ctor_field
                  COMMA
                  record_ctor_fields       { $1 :: $3 } 

record_ctor_field: IDENT ASSIGN expr       { ast_expr_rec_field_init ($1, $3) }

record_field: expr DOT IDENT               { ast_expr_rec_field ($1, $3) }

list_expr: LSBRAC RSBRAC         { ast_expr_list_nil }
         | expr CONS expr        { ast_expr_list $1 $3 }

pair_expr: LPAREN expr COMMA expr RPAREN   { ast_expr_pair $2 $4 }

call_expr: callable_exp LPAREN call_args RPAREN { ast_expr_call $1 $3 }

call_args:                                   { [] }
         | expr                              { $1 :: [] }
         | expr COMMA call_args              { $1 :: $3 }

local_stmt: LOCAL local_defs                 { $2 }

local_defs: local_def                        { $1 }
        | local_def COMMA local_defs         { $3 @ $1 }

local_def:
        local_def_assign                     { $1 }

local_def_assign: IDENT ASSIGN expression    { [ ast_stmt_assign (ast_expr_ident $1) $3 ; ast_stmt_local ($1, TVar(uniq_id())) ]  }
                | IDENT COLON typespec 
                  ASSIGN expression          { [ ast_stmt_assign (ast_expr_ident $1) $5 ; ast_stmt_local ($1, $3) ] }

typespec:  TYPE_VOID      { TVoid   }
         | TYPE_INT       { TInt    }
         | TYPE_STR       { TString }
         | TYPE_BOOL      { TBool   }
         | IDENT          { TTypename($1) }
         | LPAREN typespec COMMA typespec RPAREN { TPair($2, $4) }
         | LSBRAC typespec RSBRAC { TList($2) }
         | LVECT  typespec RVECT  { TVect($2) }
         | TYPE_FUN LPAREN typelist RPAREN COLON typespec  { TFun($3, $6) }

typelist:                            { TVoid :: [] }
         | typespec                  { $1 :: [] }
         | typespec COMMA typelist   { $1 :: $3 }

macro: AT macro_code SEMICOLON               { $2 }

macro_code:
        EXTERN INT IDENT LPAREN typelist RPAREN COLON typespec { ast_macro_extern  ($3, TFunNative($2, (unvoid $5), $8)) }
      | LITERAL IDENT literal                                  { ast_macro_literal ($2,$3) }

quot: QUOT IDENT                                               { ast_quotation $2 }

%%

