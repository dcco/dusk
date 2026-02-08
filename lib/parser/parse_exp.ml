open Commons.Try_log
open Lex_token
open Dusk_type
open Dusk_ast
open Parse_commons

	(* token check functions *)

let neverEnd (_: raw_token): bool = false

let chkComma (tk: raw_token): bool = match tk with
	COMMA -> true
	| _ -> false
let chkBar (tk: raw_token): bool = match tk with
	BAR -> true
	| _ -> false

let chkBy (tk: raw_token): bool = match tk with
	BY -> true
	| _ -> false

let chkPureEnd (tk: raw_token): bool = match tk with
	END -> true
	| _ -> false

let chkStmtEnd (tk: raw_token): bool = match tk with
	END -> true
	| ELSIF -> true
	| ELSE -> true
	| RBRACK -> true
	| _ -> false

let chkAppEnd (tk: raw_token): bool = match tk with
	LPAREN -> false
	| LBRACE -> false
	| DOT -> false
	| _ -> true

let chkReqEnd (tk: raw_token): bool = match tk with
	REFERENCES -> false
	| _ -> true

let chkUniEnd (tk: raw_token): bool = match tk with
	DASH -> false
	| EXCLAM -> false
	| _ -> true

let readUniOp (tk: raw_token): string option = match tk with
	DASH -> Some "neg"
	| EXCLAM -> Some "not"
	| _ -> None

let readMulOp (tk: raw_token): string option = match tk with
	STAR -> Some "mul"
	| SLASH -> Some "div"
	| PERC -> Some "mod"
	| EXPO -> Some "expo"
	| _ -> None

let readAddOp (tk: raw_token): string option = match tk with
	PLUS -> Some "add"
	| DASH -> Some "sub"
	| _ -> None

let readCompOp (tk: raw_token): string option = match tk with
	EQ -> Some "eq"
	| NEQ -> Some "neq"
	| LANGLE -> Some "lt"
	| RANGLE -> Some "gt"
	| LEQ -> Some "leq"
	| GEQ -> Some "geq" 
	| _ -> None

let readRelOp (tk: raw_token): string option = match tk with
	AND -> Some "and"
	| OR -> Some "or"
	| _ -> None

	(* type parsing *)

let rec parseType: m_type parser = fun tkList -> match tkList with
	(TID x, _) :: tkRem ->
		if List.mem x ["Unit"; "Int"; "Float"; "Bool"; "String"] then Valid (primTy x, tkRem)
		else Valid (NamedTy x, tkRem)
	| (LPAREN, _) :: _ ->
		let* (tau_l, tkRem) = parseParenWrap (parseSepList parseType chkComma) "Tuple Type" tkList in
		Valid (TupleTy tau_l, tkRem)
	| tk :: _ -> Error (BadToken_Err(tk, "Type Name"))
	| _ -> Error (EOF_Err "Type Name")

and parseTypeList: m_type list parser = fun tkList -> 
	parseSepList parseType chkComma tkList

and parseTypeListFull: m_type list parser = fun tkList -> match tkList with
	(RPAREN, _) :: _ | (END, _) :: _ -> Valid ([], tkList)
	| _ -> parseTypeList tkList

and parseRetTy: m_type parser = fun tkList -> match tkList with
	(TID _, _) :: (DOT, _) :: _ -> Valid (unitTy, tkList)
	| (TID _, _) :: _ | (LBRACE, _) :: _ | (LPAREN, _) :: _ | (LANGLE, _) :: _ -> parseType tkList
	| _ -> Valid (unitTy, tkList)

	(* auxiliary application parse objects *)

type appObj =
	TupleIndexApp of int * l_pos
	| ArrayIndexApp of n_exp list * l_pos
	| DirectApp of n_exp list * l_pos
	| IndirectApp of qual_prefix * string * n_exp list * l_pos

let rec foldAppList (e: n_exp) (appList: appObj list): n_exp = match appList with
	[] -> e
	| (TupleIndexApp(i, p)) :: appTail -> foldAppList (AppExp(OpExp(TupleIndexOp i, p), [e], p)) appTail
	| (ArrayIndexApp(ei_l, p)) :: appTail -> foldAppList (AppExp(OpExp(ArrayIndexOp (List.length ei_l), p), e :: ei_l, p)) appTail
	| (DirectApp(el, p)) :: appTail -> foldAppList (AppExp(e, el, p)) appTail
	| (IndirectApp(mo, f, el, p)) :: appTail -> foldAppList (AppExp(VarExp(mo, f, p), e :: el, p)) appTail

	(* auxiliary assignment parse objects *)

type lvalue =
	VarLV of string * l_pos
	| IndexLV of n_exp * n_exp list * l_pos

let asLvalue (e: n_exp): lvalue option = match e with
	VarExp(None, x, p) -> Some (VarLV(x, p))
	| VarExp(Some _, _, _) -> None
	| AppExp(OpExp(ArrayIndexOp _, _), e :: ei_l, p) -> Some (IndexLV(e, ei_l, p))
	| _ -> None

let completeAssign (lv: lvalue) (ev: n_exp): n_stmt = match lv with
	VarLV(x, p) -> AssignStmt(x, ev, p)
	| IndexLV(e, ei_l, p) -> EvalStmt(AppExp(OpExp(UpdateOp, p), [e; ev] @ ei_l, p), p)

	(* auxiliary folds *)

let rec foldOpListU (e: n_exp) (eOpList: (string * l_pos) list): n_exp = match eOpList with
	[] -> e
	| (xOp, p) :: eOpTail ->
		let e' = foldOpListU e eOpTail in AppExp(VarExp(None, xOp, p), [e'], p)

let rec foldOpListL (e: n_exp) (eOpList: (n_exp * string * l_pos) list): n_exp = match eOpList with
	[] -> e
	| (ex, xOp, p) :: eOpTail -> foldOpListL (AppExp(VarExp(None, xOp, p), [e; ex], p)) eOpTail

let rec foldRangeList (rl: (string * range_type * n_exp) list) (body: n_stmt list): n_stmt = match rl with
	[] -> failwith "BUG: Parsed for loop with no ranges"
	| [(x, r, e)] -> ForStmt(x, r, e, body, ann_exp e)
	| (x, r, e) :: rt -> ForStmt(x, r, e, [foldRangeList rt body], ann_exp e)

	(* main expression parsing *)

let rec parseIdAtomExp (prefix: qual_prefix): n_exp parser = fun tkList -> match tkList with
	(ID x, p) :: tkRem -> Valid (VarExp(prefix, x, p), tkRem)
	| (TID m, p) :: tkRem -> (match tkRem with
		(LPAREN, _) :: _ ->
			let* (el, tkRem2) = parseParenWrap (parseSepList parseExp chkComma) "Enum / Union" tkRem in
			Valid (TupleExp(Some (prefix, m), el, p), tkRem2)
		| _ -> Valid (TupleExp(Some (prefix, m), [], p), tkRem)
	)
	| tk :: _ -> Error (BadToken_Err(tk, "Qualified Exp"))
	| _ -> Error (EOF_Err "Qualified Exp")

and parseAtomExp: n_exp parser = fun tkList -> match tkList with
	(INT i, p) :: tkRem -> Valid (ConstExp(IConst i, p), tkRem)
	| (FLOAT f, p) :: tkRem -> Valid (ConstExp(FConst f, p), tkRem)
	| (FALSE, p) :: tkRem -> Valid (ConstExp(BConst false, p), tkRem)
	| (TRUE, p) :: tkRem -> Valid (ConstExp(BConst true, p), tkRem)
	| (STRLIT s, p) :: tkRem -> Valid (ConstExp(SConst s, p), tkRem)
	| (ID x, p) :: tkRem -> Valid (VarExp(None, x, p), tkRem)
	| (TID prefix, _) :: tkRem -> (match tkRem with
		(DOT, _) :: tkRem2 -> parseIdAtomExp (Some prefix) tkRem2
		| _ -> parseIdAtomExp None tkList
	)
	| (NEW, p) :: tkRem -> (match tkRem with
		(DIM i, _) :: tkRem2 ->
			let* ((tau, el), tkRem3) = parseBraceWrap parseArrayInner "Array Initializer" tkRem2 in
			Valid (NewDimExp(i, tau, el, p), tkRem3)
		| tk :: _ -> Error (BadToken_Err(tk, "Heap Memory Initializer"))
		| _ -> Error (EOF_Err "Heap Memory Initializer")
	)
	| (LPAREN, p) :: _ ->
		let* (el, tkRem) = parseParenWrap (parseSepList parseExp chkComma) "Tag Value" tkList in
		if List.length el = 1 then Valid (List.hd el, tkRem)
		else Valid (TupleExp(None, el, p), tkRem)
	| (BAR, p) :: tkRem ->
		let* (e, tkRem2) = parseExp tkRem in
		let* (_, tkRem3) = parseTk BAR "Length Operator" tkRem2 in
		Valid (AppExp(OpExp(LengthOp, p), [e], p), tkRem3)
	| tk :: _ -> Error (BadToken_Err(tk, "Exp"))
	| _ -> Error (EOF_Err "Exp")

and parseArgList: n_exp list parser = fun tkList -> match tkList with
	(RPAREN, _) :: _ -> Valid ([], tkList)
	| _ -> parseSepList parseExp chkComma tkList

and parseArrayInner: (m_type * n_exp list) parser = fun tkList ->
	let* (tau, tkRem) = parseType tkList in
	let* (_, tkRem2) = parseTk ELLIP "Array Initializer" tkRem in
	let* (el, tkRem3) = parseSepList parseExp chkBy tkRem2 in Valid ((tau, el), tkRem3)

and parseAppObj: appObj parser = fun tkList -> match tkList with
	(LPAREN, p) :: _ -> 
		let* (el, tkRem) = parseParenWrap parseArgList "App" tkList in
		Valid (DirectApp(el, p), tkRem)
	| (LBRACE, p) :: _ ->
		let* (el, tkRem) = parseBraceWrap (parseSepList parseExp chkComma) "Array Index" tkList in
		Valid (ArrayIndexApp(el, p), tkRem)
	| (DOT, p) :: tkRem -> (match tkRem with
		(ID f, _) :: tkRem2 ->
			let* (el, tkRem3) = parseParenWrap parseArgList "Function Call" tkRem2 in
			Valid (IndirectApp(None, f, el, p), tkRem3)
		| (INT i, _) :: tkRem2 ->
			Valid (TupleIndexApp(i, p), tkRem2)
		| tk :: _ ->  Error (BadToken_Err(tk, "Property / Function Call"))
		| _ -> Error (EOF_Err "Property / Function Call")
	)
	| _ -> failwith "BUG: Unexpected case while parsing application object."

and parseAppExp: n_exp parser = fun tkList ->
	let* (e, tkRem) = parseAtomExp tkList in
	let* (appList, tkRem2) = parseList parseAppObj chkAppEnd tkRem in
	Valid (foldAppList e appList, tkRem2)

and parseUniExp: n_exp parser = fun tkList ->
	let* (eOpList, tkRem) = parseList (parseTkMulti readUniOp "Unary Operator") chkUniEnd tkList in
	let* (e, tkRem2) = parseAppExp tkRem in
	Valid (foldOpListU e eOpList, tkRem2)

and parseMulExp: n_exp parser = fun tkList ->
	let* ((e0, eOpList), tkRem) = parseSepListFull parseUniExp readMulOp tkList in
	Valid (foldOpListL e0 eOpList, tkRem)

and parseAddExp: n_exp parser = fun tkList ->
	let* ((e0, eOpList), tkRem) = parseSepListFull parseMulExp readAddOp tkList in
	Valid (foldOpListL e0 eOpList, tkRem)

and parseCompExp: n_exp parser = fun tkList ->
	let* ((e0, eOpList), tkRem) = parseSepListFull parseAddExp readCompOp tkList in
	Valid (foldOpListL e0 eOpList, tkRem)

and parseRelExp: n_exp parser = fun tkList ->
	let* ((e0, eOpList), tkRem) = parseSepListFull parseCompExp readRelOp tkList in
	Valid (foldOpListL e0 eOpList, tkRem)

and parseExp tkList = parseRelExp tkList

and parseField: (string * n_exp) parser = fun tkList ->
	let* (x, tkRem) = parseId tkList in
	let* (_, tkRem2) = parseTk EQ "Struct Field" tkRem in
	let* (e, tkRem3) = parseExp tkRem2 in
	Valid ((x, e), tkRem3)

	(* statement parsing *)

let parsePat: string option parser = fun tkList -> match tkList with
	(UNDERSCORE, _) :: tkRem -> Valid (None, tkRem)
	| (ID x, _) :: tkRem -> Valid (Some x, tkRem)
	| tk :: _ -> Error (BadToken_Err(tk, "Pattern"))
	| _ -> Error (EOF_Err "Pattern")

let rec parseStmt: n_stmt parser = fun tkList -> match tkList with
	(VAR, p) :: tkRem ->
		let* (px, tkRem2) = (match tkRem with
			(LPAREN, _) :: _ ->
				let* (xl, tkRem2) = parseParenWrap (parseSepList parsePat chkComma) "Var Pattern" tkRem in
				Valid (ListPat xl, tkRem2)
			| _ -> let* (x, tkRem2) = parseId tkRem in Valid (VarPat x, tkRem2)
		) in
		let* (_, tkRem3) = parseTk EQ "Var Stmt" tkRem2 in
		let* (e, tkRem4) = parseExp tkRem3 in
		Valid (PatStmt(px, e, p), tkRem4)
	| (IF, p) :: tkRem ->
		let* ((c0, b0), tkRem2) = parseCond THEN "If Stmt (Body)" tkRem in
		let* (elseBody, tkRem3) = parseElsif tkRem2 in
		Valid (IfStmt(c0, b0, elseBody, p), tkRem3)
	| (LOOP, p) :: tkRem ->
		let* (body, tkRem2) = parseList parseStmt chkStmtEnd tkRem in
		let* (_, tkRem3) = parseTk END "Loop (End)" tkRem2 in
		Valid (WhileStmt(ConstExp(BConst true, p), body, p), tkRem3)
	| (WHILE, p) :: tkRem ->
		let* ((ec, body), tkRem2) = parseCond DO "While Loop (Body)" tkRem in
		let* (_, tkRem3) = parseTk END "While Loop (End)" tkRem2 in
		Valid (WhileStmt(ec, body, p), tkRem3)
	| (FOR, _) :: tkRem ->
		let* (rl, tkRem2) = parseSepList parseRange chkComma tkRem in
		let* (_, tkRem3) = parseTk DO "For Loop (Body)" tkRem2 in
		let* (body, tkRem4) = parseList parseStmt chkStmtEnd tkRem3 in
		let* (_, tkRem5) = parseTk END "For Loop (End)" tkRem4 in
		Valid (foldRangeList rl body, tkRem5)
	| (RETURN, p) :: tkRem -> (match tkRem with
		(END, _) :: _ -> Valid (ReturnStmt(None, p), tkRem)
		| [] -> Valid (ReturnStmt(None, p), [])
		| _ ->
			let* (e, tkRem2) = parseExp tkRem in
			Valid (ReturnStmt(Some e, p), tkRem2))
	| (tk, p) :: _ ->
		let* (e, tkRem) = parseAddExp tkList in (match tkRem with
		(EQ, _) :: tkRem2 -> (match asLvalue e with
			None -> Error (BadLValue_Err(tk, ann_exp e))
			| Some lv ->
				let* (e, tkRem3) = parseExp tkRem2 in
				Valid (completeAssign lv e, tkRem3))
		| _ -> Valid (EvalStmt(e, p), tkRem))
	| _ -> Error (EOF_Err "Stmt")

and parseCond (sep: raw_token) (x: string): (n_exp * n_stmt list) parser = fun tkList ->
	let* (ec, tkRem) = parseExp tkList in
	let* (_, tkRem2) = parseTk sep x tkRem in
	let* (body, tkRem3) = parseList parseStmt chkStmtEnd tkRem2 in
	Valid ((ec, body), tkRem3)

and parseElsif: n_stmt list parser = fun tkList -> match tkList with 
	(ELSIF, p) :: tkRem ->
		let* ((ce, b), tkRem2) = parseCond THEN "Elsif Stmt (Body)" tkRem in
		let* (elseBody, tkRem3) = parseElsif tkRem2 in
		Valid ([IfStmt(ce, b, elseBody, p)], tkRem3)
	| (ELSE, _) :: tkRem ->
		let* (body, tkRem2) = parseList parseStmt chkStmtEnd tkRem in
		let* (_, tkRem3) = parseTk END "Else Stmt (End)" tkRem2 in
		Valid (body, tkRem3)
	| (END, _) :: tkRem -> Valid ([], tkRem)
	| tk :: _ -> Error (BadToken_Err(tk, "If Stmt (End)"))
	| _ -> Error (EOF_Err "If Stmt (End)")

and parseRangeType: range_type parser = fun tkList -> match tkList with
	(LANGLE, _) :: tkRem -> Valid (LtRange, tkRem)
	| (LEQ, _) :: tkRem -> Valid (LeqRange, tkRem)
	| (IN, _) :: tkRem -> Valid (ListRange, tkRem)
	| tk :: _ -> Error (BadToken_Err(tk, "For Loop Range Type"))
	| _ -> Error (EOF_Err "For Loop Range Type")

and parseRange: (string * range_type * n_exp) parser = fun tkList ->
	let* (x, tkRem) = parseId tkList in
	let* (rt, tkRem2) = parseRangeType tkRem in
	let* (e, tkRem3) = parseExp tkRem2 in
	Valid ((x, rt, e), tkRem3)

	(* method parsing *)

let parseParam: (string * m_type) parser = fun tkList ->
	let* (t, tkRem) = parseType tkList in
	let* (x, tkRem2) = parseId tkRem in
	Valid ((x, t), tkRem2)

let parseParamList: (string * m_type) list parser = fun tkList -> match tkList with
	(RPAREN, _) :: _ -> Valid ([], tkList)
	| _ -> parseSepList parseParam chkComma tkList

let parseFnMain: ((string * m_type) list * m_type * n_stmt list) parser = fun tkList ->
	let* (pl, tkRem) = parseParenWrap parseParamList "Parameter List" tkList in
	let* (tau_r, tkRem2) = parseRetTy tkRem in
	let* (sl, tkRem3) = parseList parseStmt chkStmtEnd tkRem2 in
	let* (_, tkRem4) = parseTk END "Method" tkRem3 in
	Valid ((pl, tau_r, sl), tkRem4)

let parseMet: n_met parser = fun tkList -> match tkList with
	(FN, _) :: tkRem ->
		let* (f, tkRem2) = parseId tkRem in
		let* ((pl, tau_r, body), tkRem3) = parseFnMain tkRem2 in
		Valid (Method(f, pl, tau_r, body), tkRem3)
	| tk :: _ -> Error (BadToken_Err(tk, "Method"))
	| _ -> Error (EOF_Err "Method")

	(* declaration parsing *)

let parseDec: n_dec parser = fun tkList -> match tkList with
	(FN, p) :: _ ->
		let* (m, tkRem) = parseMet tkList in
		Valid (FunDec(m, p), tkRem)
	| tk :: _ -> Error (BadToken_Err(tk, "Dec"))
	| _ -> Error (EOF_Err "Dec")

	(* full file parsing *)

let parseReq: n_req parser = fun tkList -> match tkList with
	(REFERENCES, p) :: tkRem ->
		let* (c, tkRem2) = parseTId tkRem in (match tkRem2 with
			(DOT, _) :: tkRem3 ->
				let* (c2, tkRem4) = parseTId tkRem3 in
				Valid (ShortRefReq([c; c2], p), tkRem4)
			| (MODULES, _) :: tkRem3 ->
				let* (cl, tkRem4) = parseSepList parseTId chkComma tkRem3 in
				Valid (LongRefReq([c], cl, p), tkRem4)
			| _ -> failwith "UNIMPLEMENTED: dusk_parse.ml - other kinds of references"
		)
	| tk :: _ -> Error (BadToken_Err(tk, "Module Requirement"))
	| _ -> Error (EOF_Err "Module Requirement")

let parseMain (tkList: token list): n_section parse_res =
	let* (rList, tkRem) = (parseList parseReq chkReqEnd) tkList in 
	let* (dList, _) = (parseList parseDec neverEnd) tkRem in Valid (Section(rList, dList))