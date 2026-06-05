
type raw_token = INT of int | FLOAT of float | TRUE | FALSE | NULL
	| STRLIT of string | KLIT of string | U8 of int | LONG of Int64.t
	| ID of string | TID of string | CID of string | DIM of int | T_FN 
	| REFERENCES | MODULE | MODULES | CHAPTER | END
	| STRUCT | ENUM | ENUMP | EXTENDS | ATTRS | UNION | CONST | GLOBALS
	| FN | LIN | VAR | NEW | BY | IF | THEN | ELSIF | ELSE | IS
	| LOOP | WHILE | DO | FOR | IN | RETURN
	| GC_COLLECT | UNDERSCORE
	| EQ | DOT | ELLIP | LPAREN | RPAREN | COMMA | TILDE | QMARK | AT
	| LBRACE | RBRACE | BAR | LBRACK | RBRACK
	| NEQ | LANGLE | RANGLE | LEQ | GEQ | AND | OR | EXCLAM
	| PLUS | DASH | STAR | SLASH | FLDIV | PERC | EXPO
	| PLUS_EQ | SLASH_EQ | EOF

let string_of_raw_token tk = match tk with
	INT i -> string_of_int i
	| FLOAT f -> string_of_float f
	| STRLIT s -> "\"" ^ (String.escaped s) ^ "\""
	| KLIT s -> "^" ^ s
	| U8 b -> (string_of_int b) ^ "b"
	| LONG l -> (Int64.to_string l) ^ "l"
	| DIM i -> (string_of_int i) ^ "d"
	(*| VDIM -> "1v"*)
	| T_FN -> "Fn"
	| TRUE -> "true"
	| FALSE -> "false"
	| NULL -> "null"
	| ID x -> "id:" ^ x
	| TID x -> "tid:" ^ x
	| CID x -> "cid:" ^ x
	| REFERENCES -> "references"
	| MODULE -> "module"
	| MODULES -> "modules"
	| CHAPTER -> "chapter"
	| END -> "end"
	| STRUCT -> "struct"
	| ENUM -> "enum"
	| ENUMP -> "enum+"
	| EXTENDS -> "extends"
	| ATTRS -> "attrs"
	| UNION -> "union"
	| CONST -> "const"
	| GLOBALS -> "globals"
	| FN -> "fn"
	| LIN -> "lin"
	| VAR -> "var"
	| NEW -> "new"
	| BY -> "by"
	| IF -> "if"
	| THEN -> "then"
	| ELSIF -> "elsif"
	| ELSE -> "else"
	| IS -> "is"
	| LOOP -> "loop"
	| WHILE -> "while"
	| DO -> "do"
	| FOR -> "for"
	| IN -> "in"
	| RETURN -> "return"
	| GC_COLLECT -> "gc_collect"
	| UNDERSCORE -> "_"
	| EQ -> "="
	| DOT -> "."
	| ELLIP -> ".."
	| LPAREN -> "("
	| RPAREN -> ")"
	| COMMA -> ","
	| TILDE -> "~"
	| QMARK -> "?"
	| AT -> "@"
	| LBRACE -> "["
	| RBRACE -> "]"
	| BAR -> "|"
	| LBRACK -> "{"
	| RBRACK -> "}"
	| NEQ -> "!="
	| LANGLE -> "<"
	| RANGLE -> ">"
	| LEQ -> "<="
	| GEQ -> ">="
	| AND -> "&&"
	| OR -> "||"
	| EXCLAM -> "!"
	| PLUS -> "+"
	| DASH -> "-"
	| STAR -> "*"
	| SLASH -> "/"
	| FLDIV -> "/."
	| PERC -> "%"
	| EXPO -> "**"
	| PLUS_EQ -> "+="
	| SLASH_EQ -> "/="
	| EOF -> "EOF"

type token = raw_token * Lexing.position

	(* lexer position *)

type l_pos = Lexing.position

let string_of_pos (pos: l_pos) =
	"line " ^ (string_of_int pos.pos_lnum) ^ ", column " ^ (string_of_int (pos.pos_cnum - pos.pos_bol))