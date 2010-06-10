{
  open Parser
  open Lexing
  open Escaping 

  let incr_linenum lexbuf =
    let pos = lexbuf.lex_curr_p in 
    lexbuf.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.pos_lnum + 1;
    Lexing.pos_bol = pos.pos_cnum;
    }

}

let ident   = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let digit   = ['0' - '9']
let decimal = digit+
let hexdigit = ['0' - '9' 'a' - 'f' 'A' - 'F']
let hexadecimal = ("0x" | "0X") hexdigit hexdigit*
let space   = [' ' '\t']

rule token = parse
	| "#"  [^'\n']* '\n' { incr_linenum lexbuf; token lexbuf }
	| "%%" [^'\n']* '\n' { incr_linenum lexbuf; token lexbuf }
	| space           { token lexbuf }
	| '\n'            { incr_linenum lexbuf; token lexbuf }
	| "="             { ASSIGN }
	| ";"             { SEMICOLON }
	| ":"             { COLON }
	| ","             { COMMA  }
    | "."             { DOT    }
	| "{"             { LBRACE }
	| "}"             { RBRACE }
	| "("             { LPAREN }
	| ")"             { RPAREN }
    | "["             { LSBRAC }
    | "]"             { RSBRAC }
    | "[<"            { LVECT }
    | ">]"            { RVECT }
    | "::"            { CONS }
	| '+'             { PLUS }
	| '-'             { MINUS }
	| '*'             { MULTIPLY }
	| '/'             { DIVIDE }
	| '%'             { REMAINDER }
	| '|'             { ORB }
	| '&'             { ANDB }
	| '^'             { XORB }
	| '~'             { INVB }
    | '`'             { QUOT }
	| "<<"            { SHL }
	| ">>"            { SHR }
	| "=="            { EQUAL }
	| "!="            { UNEQUAL }
	| "<"             { LESS }
	| ">"             { MORE }
	| "<="            { LESSEQ }
	| ">="            { MOREEQ }
	| "!"             { NOT }
	| "&&"            { AND }
	| "||"            { OR }
	| "true"          { TRUE }
	| "false"         { FALSE }
	| "def"           { DEF }
	| "import"        { IMPORT }
	| "extern"        { EXTERN }
	| "literal"       { LITERAL}
	| "if"            { IF }
	| "then"          { THEN }
	| "else"          { ELSE }
	| "elif"          { ELIF }
	| "while"         { WHILE }
	| "break"         { BREAK }
	| "continue"      { CONTINUE }
	| "ret"           { RET }
	| "local"         { LOCAL }
	| "->"            { ARROW }
    | "@"             { AT }
	| "void"          { TYPE_VOID }
	| "int"           { TYPE_INT }
	| "string"        { TYPE_STR }
	| "bool"          { TYPE_BOOL }
    | "type"          { TYPE }
    | "fun"           { TYPE_FUN }
	| ident           { IDENT (lexeme lexbuf) }
	| decimal         { INT (int_of_string(lexeme lexbuf)) }
	| hexadecimal     { INT (int_of_string(lexeme lexbuf)) }

    (* Char literals *)
    | '\'' ([^'\'']+ as c) '\'' { INT ( Char.code ((unescape c).[0]) ) }

	| '\"' (([^'\"']|'\\' '"')* as s) '\"' { STRING((unescape s)) }
	| eof             { EOF }

{
}

