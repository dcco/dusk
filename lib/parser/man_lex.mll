{
	open Lexing
	open Lex_token

		(* lexer auxiliary functions *)

	let lexWrap lexbuf t = (t, lexeme_start_p lexbuf)

	let asciiHexLit h = int_of_string ("0" ^ h)

	let stringOfASCII c = String.make 1 (Char.chr c)
}

let idChar = ['a'-'z' 'A'-'Z' '0'-'9']
let numChar = ['0'-'9']
let hexChar = ['a'-'f' 'A'-'F' '0'-'9']

rule token = parse
	| "\r\n" | "\r" | "\n" { new_line lexbuf; token lexbuf }
	| "(*" { comment 1 lexbuf }
	| "--" { comment_line lexbuf }
	| "references" { lexWrap lexbuf REFERENCES }
	| "module" { lexWrap lexbuf MODULE }
	| "modules" { lexWrap lexbuf MODULES }
	| "end" { lexWrap lexbuf END }
	| "fn" { lexWrap lexbuf FN }
	| "var" { lexWrap lexbuf VAR }
	| "new" { lexWrap lexbuf NEW }
	| "by" { lexWrap lexbuf BY }
	| "if" { lexWrap lexbuf IF }
	| "then" { lexWrap lexbuf THEN }
	| "elsif" { lexWrap lexbuf ELSIF }
	| "else" { lexWrap lexbuf ELSE }
	| "is" { lexWrap lexbuf IS }
	| "loop" { lexWrap lexbuf LOOP }
	| "while" { lexWrap lexbuf WHILE }
	| "do" { lexWrap lexbuf DO }
	| "for" { lexWrap lexbuf FOR }
	| "in" { lexWrap lexbuf IN }
	| "return" { lexWrap lexbuf RETURN }
	| "false" { lexWrap lexbuf FALSE }
	| "true" { lexWrap lexbuf TRUE }
	| "\"" { strlit (lexeme_start_p lexbuf) "" lexbuf }
	| ("_"? ['a'-'z'] idChar*) as x { lexWrap lexbuf (ID x) }
	| ("_"? ['A'-'Z'] idChar*) as x { lexWrap lexbuf (TID x) }
	| ((numChar+) as i) "d" { lexWrap lexbuf (DIM (int_of_string i)) }
	| ("-"? numChar+ "." numChar+) as f { lexWrap lexbuf (FLOAT (float_of_string f)) }
	| ("-"? numChar+) as i { lexWrap lexbuf (INT (int_of_string i)) }
	| "_" { lexWrap lexbuf UNDERSCORE }
	| ".." { lexWrap lexbuf ELLIP }
	| "." { lexWrap lexbuf DOT }
	| "=" { lexWrap lexbuf EQ }
	| "!=" { lexWrap lexbuf NEQ }
	| "<" { lexWrap lexbuf LANGLE }
	| ">" { lexWrap lexbuf RANGLE }
	| "<=" { lexWrap lexbuf LEQ }
	| ">=" { lexWrap lexbuf GEQ }
	| "&&" { lexWrap lexbuf AND }
	| "||" { lexWrap lexbuf OR }
	| "!" { lexWrap lexbuf EXCLAM }
	| "+" { lexWrap lexbuf PLUS }
	| "-" { lexWrap lexbuf DASH }
	| "**" { lexWrap lexbuf EXPO }
	| "*" { lexWrap lexbuf STAR }
	| "/" { lexWrap lexbuf SLASH }
	| "%" { lexWrap lexbuf PERC }
	| "(" { lexWrap lexbuf LPAREN }
	| ")" { lexWrap lexbuf RPAREN }
	| "[" { lexWrap lexbuf LBRACE }
	| "]" { lexWrap lexbuf RBRACE }
	| "|" { lexWrap lexbuf BAR }
	| "{" { lexWrap lexbuf LBRACK }
	| "}" { lexWrap lexbuf RBRACK }
	| "," { lexWrap lexbuf COMMA }
	| eof { lexWrap lexbuf EOF }
	| _ { token lexbuf }
and strlit p x = parse
	| "\r\n" | "\r" | "\n" { new_line lexbuf; token lexbuf }
	| "\"" { (STRLIT x, p) }
	| "\\0" { strlit p (x ^ "\x00") lexbuf }
	| "\\a" { strlit p (x ^ "\x07") lexbuf }
	| "\\b" { strlit p (x ^ "\b") lexbuf }
	| "\\t" { strlit p (x ^ "\t") lexbuf }
	| "\\n" { strlit p (x ^ "\n") lexbuf }
	| "\\v" { strlit p (x ^ "\x0B") lexbuf }
	| "\\f" { strlit p (x ^ "\x0C") lexbuf }
	| "\\r" { strlit p (x ^ "\r") lexbuf }
	| "\\\"" { strlit p (x ^ "\"") lexbuf }
	| "\\'" { strlit p (x ^ "'") lexbuf }
	| "\\\\" { strlit p (x ^ "\\") lexbuf }
	| ("\\x" hexChar hexChar) as h { strlit p ((stringOfASCII (asciiHexLit h)) ^ x) lexbuf}
	| _ as c { strlit p (x ^ (String.make 1 c)) lexbuf }
and comment i = parse
	| "\r\n" | "\r" | "\n" { new_line lexbuf; comment i lexbuf }
	| "(*" { comment (i + 1) lexbuf }
	| "*)" { if i = 1 then token lexbuf else comment (i - 1) lexbuf }
	| _ { comment i lexbuf }
and comment_line = parse
	| "\r\n" | "\r" | "\n" { new_line lexbuf; token lexbuf }
	| _ { comment_line lexbuf }