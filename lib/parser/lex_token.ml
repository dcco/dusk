
type raw_token = INT of int | FLOAT of float | TRUE | FALSE
	| STRLIT of string | KLIT of string | LONG of Int64.t
	| ID of string | TID of string | DIM of int
	| REFERENCES | MODULE | MODULES | CHAPTER | END
	| STRUCT | CONST
	| FN | LIN | VAR | NEW | BY | IF | THEN | ELSIF | ELSE | IS
	| LOOP | WHILE | DO | FOR | IN | RETURN
	| GC_COLLECT | UNDERSCORE
	| EQ | DOT | ELLIP | LPAREN | RPAREN | COMMA
	| LBRACE | RBRACE | BAR | LBRACK | RBRACK
	| NEQ | LANGLE | RANGLE | LEQ | GEQ | AND | OR | EXCLAM
	| PLUS | DASH | STAR | SLASH | FLDIV | PERC | EXPO | EOF

let string_of_raw_token tk = match tk with
	INT i -> string_of_int i
	| FLOAT f -> string_of_float f
	| STRLIT s -> "\"" ^ (String.escaped s) ^ "\""
	| KLIT s -> "^" ^ s
	| LONG l -> (Int64.to_string l) ^ "l"
	| DIM i -> (string_of_int i) ^ "d"
	| TRUE -> "true"
	| FALSE -> "false"
	| ID x -> "id:" ^ x
	| TID x -> "tid:" ^ x
	| REFERENCES -> "references"
	| MODULE -> "module"
	| MODULES -> "modules"
	| CHAPTER -> "chapter"
	| END -> "end"
	| STRUCT -> "struct"
	| CONST -> "const"
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
	| EOF -> "EOF"

type token = raw_token * Lexing.position

	(* lexer position *)

type l_pos = Lexing.position

let string_of_pos (pos: l_pos) =
	"line " ^ (string_of_int pos.pos_lnum) ^ ", column " ^ (string_of_int (pos.pos_cnum - pos.pos_bol))