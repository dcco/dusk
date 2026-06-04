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

let chkBrackR (tk: raw_token): bool = match tk with
	RPAREN -> true
	| RBRACE -> true
	| RBRACK -> true
	| _ -> false

let rec chkTypeStart (tkList: token list): bool = match tkList with
	(TID _, _) :: _ -> true
	| (DIM _, _) :: _ -> true
	| (LPAREN, _) :: tkRem -> chkTypeStart tkRem
	| _ -> false

let readUniOp (tk: raw_token): string option = match tk with
	DASH -> Some "neg"
	| EXCLAM -> Some "not"
	| _ -> None

let readMulOp (tk: raw_token): string option = match tk with
	STAR -> Some "mul"
	| SLASH -> Some "div"
	| FLDIV -> Some "flDiv"
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
	(DIM i, _) :: tkRem ->
		let* (tau, tkRem2) = parseBraceWrap parseType "Array Type" tkRem in
		Valid (ArrayTy(i, tau), tkRem2)
	| (LPAREN, _) :: _ ->
		let* (tau_l, tkRem) = parseParenWrap (parseSepList parseType chkComma) "Tuple Type" tkList in
		Valid (TupleTy tau_l, tkRem)
	| (T_FN, _) :: tkRem ->
		let* (tau_pl, tkRem2) = parseParenWrap (
			parseOrEmpty (parseSepList parseType chkComma) chkBrackR) "Function Type" tkRem in
		let* (tau_r, tkRem3) = parseRetTy tkRem2 in
		Valid (FunTy(tau_pl, tau_r), tkRem3)
	| _ ->
		let* (QN(q, x), tkRem) = parseDerefTId "Type Name" tkList in
		if List.mem x ["Unit"; "Int"; "Float"; "Bool"; "String"; "U8"; "U32"; "U64"] then Valid (primTy x, tkRem)
		else if List.mem x ["PRNG"; "Mat4"; "Image"; "FixImage"; "Sprite"; "Mesh"; "RenderData"] then Valid (builtinTy x, tkRem)
		else if x = "BOT" then Valid (BotTy, tkRem)
		else Valid (NamedTy (QN(q, x)), tkRem)

and parseTypeList: m_type list parser = fun tkList -> 
	parseSepList parseType chkComma tkList

and parseTypeListFull: m_type list parser = fun tkList -> match tkList with
	(RPAREN, _) :: _ | (END, _) :: _ -> Valid ([], tkList)
	| _ -> parseTypeList tkList

and parseRetTy: m_type parser = fun tkList -> match tkList with
	(TID _, _) :: (DOT, _) :: _ -> Valid (unitTy, tkList)
	| _ -> if chkTypeStart tkList then parseType tkList else Valid (unitTy, tkList)

	(* auxiliary application parse objects *)

type appObj =
	TupleIndexApp of int * l_pos
	| ArrayIndexApp of m_exp list * l_pos
	| StructFieldApp of string * l_pos
	| TupleTagApp of l_pos
	| EnumRawApp of l_pos
	| DirectApp of m_exp list * l_pos
	| IndirectApp of qual_name * m_exp list * l_pos

let rec foldAppList (e: m_exp) (appList: appObj list): m_exp = match appList with
	[] -> e
	| (TupleIndexApp(i, p)) :: appTail -> foldAppList (AppExp(OpExp(TupleIndexOp i, p), [e], p)) appTail
	| (ArrayIndexApp(ei_l, p)) :: appTail -> foldAppList (AppExp(OpExp(ArrayIndexOp RR, p), e :: ei_l, p)) appTail
	| (StructFieldApp(x, p)) :: appTail -> foldAppList (AppExp(OpExp(StructFieldOp(RR, x), p), [e], p)) appTail
	| (TupleTagApp p) :: appTail -> foldAppList (AppExp(OpExp(TupleTagOp, p), [e], p)) appTail
	| (EnumRawApp p) :: appTail -> foldAppList (AppExp(OpExp(EnumRawOp, p), [e], p)) appTail
	| (DirectApp(el, p)) :: appTail -> foldAppList (AppExp(e, el, p)) appTail
	| (IndirectApp(f, el, p)) :: appTail -> foldAppList (AppExp(VarExp(f, p), e :: el, p)) appTail

	(* auxiliary assignment parse objects *)

type lvalue =
	VarLV of qual_name * l_pos
	| IndexLV of m_exp * m_exp list * l_pos
	| FieldLV of m_exp * string * l_pos

let asLvalue (e: m_exp): lvalue option = match e with
	(*VarExp(QN(None, x), p) -> Some (VarLV(x, p))
	| VarExp(QN(Some _, _), _) -> None*)
	VarExp(x, p) -> Some (VarLV(x, p))
	| AppExp(OpExp(ArrayIndexOp RR, _), e :: ei_l, p) -> Some (IndexLV(e, ei_l, p))
	| AppExp(OpExp(StructFieldOp(RR, x), _), [e], p) -> Some (FieldLV(e, x, p))
	| _ -> None

let completeAssign (lv: lvalue) (ev: m_exp): m_stmt = match lv with
	VarLV(x, p) -> AssignStmt(x, ev, p)
	| IndexLV(e, ei_l, p) -> EvalStmt(AppExp(OpExp(ArrayIndexOp WW, p), [e; ev] @ ei_l, p), p)
	| FieldLV(e, x, p) -> EvalStmt(AppExp(OpExp(StructFieldOp(WW, x), p), [e; ev], p), p)

	(* auxiliary folds *)

let rec foldOpListU (e: m_exp) (eOpList: (string * l_pos) list): m_exp = match eOpList with
	[] -> e
	| (xOp, p) :: eOpTail ->
		let e' = foldOpListU e eOpTail in AppExp(VarExp(qn xOp, p), [e'], p)

let rec foldOpListL (e: m_exp) (eOpList: (m_exp * string * l_pos) list): m_exp = match eOpList with
	[] -> e
	| (ex, xOp, p) :: eOpTail -> foldOpListL (AppExp(VarExp(qn xOp, p), [e; ex], p)) eOpTail

	(*
		- the first loop in a sequence becomes the inner-most loop
			(to mirror how array indexing is performed)
	 *)

let rec foldRangeList (rl: (string * range_type * m_exp) list) (body: m_stmt list): m_stmt = match rl with
	[] -> failwith "BUG: Parsed for loop with no ranges"
	| [(x, r, e)] -> ForStmt(x, r, e, body, ann_exp e)
	| (x, r, e) :: rt -> foldRangeList rt [ForStmt(x, r, e, body, ann_exp e)]

	(* main expression parsing *)

let forceIntList (el: m_exp list): (int list) parse_res = map_try_res (fun e -> match e with
		ConstExp(IConst i, _) -> Valid i
		| e -> Error (NonIntExp_Err (ann_exp e))
	) el

(*let rec parseIdAtomExp (prefix: qual_tag): m_exp parser = fun tkList -> match tkList with
	(ID x, p) :: tkRem -> Valid (VarExp(prefix, x, p), tkRem)
	| (TID m, p) :: tkRem -> (match tkRem with
		(LPAREN, _) :: _ ->
			let* (el, tkRem2) = parseParenWrap (parseSepList parseExp chkComma) "Enum / Union" tkRem in
			Valid (TupleExp(Some (prefix, m), el, p), tkRem2)
		| _ -> Valid (TupleExp(Some (prefix, m), [], p), tkRem)
	)
	| tk :: _ -> Error (BadToken_Err(tk, "Qualified Exp"))
	| _ -> Error (EOF_Err "Qualified Exp")*)

let rec parseAtomExp: m_exp parser = fun tkList -> match tkList with
	(INT i, p) :: tkRem -> Valid (ConstExp(IConst i, p), tkRem)
	| (FLOAT f, p) :: tkRem -> Valid (ConstExp(FConst f, p), tkRem)
	| (FALSE, p) :: tkRem -> Valid (ConstExp(BConst false, p), tkRem)
	| (TRUE, p) :: tkRem -> Valid (ConstExp(BConst true, p), tkRem)
	| (STRLIT s, p) :: tkRem -> Valid (ConstExp(SConst s, p), tkRem)
	| (U8 i, p) :: tkRem -> Valid (ConstExp(U8Const i, p), tkRem)
	| (LONG l, p) :: tkRem -> Valid (ConstExp(LConst l, p), tkRem)
	| (KLIT k, p) :: tkRem -> Valid (ConstExp(KConst k, p), tkRem)
	| (AT, p) :: tkRem ->
		let* (x, tkRem2) = parseDerefTId "@ Enum Syntax" tkRem in Valid (AtCtorExp(x, p), tkRem2)
	| (NEW, p) :: tkRem -> (match tkRem with
		(DIM i, _) :: tkRem2 -> (match tkRem2 with
			(LPAREN, _) :: _ ->
				let* (dim_l, tkRem3) = parseParenWrap (parseSepList parseExp chkComma) "Array Initializer" tkRem2 in
				let* (el, tkRem4) = parseBraceWrap (fun tkList ->
					let* (_, tkRem) = parseTk ELLIP "Array Initializer" tkList in parseExp tkRem
				) "Array Initializer" tkRem3 in
				Valid (FormatArrayExp(i, dim_l, el, p), tkRem4)
			| _ ->
				let* ((tau_ox, el_x), tkRem3) = parseBraceWrap parseArrayInner "Array Initializer" tkRem2 in
				(match tkRem3 with
					(LBRACE, p') :: _ ->
						if tau_ox <> None then Error (BadToken_Err((LBRACE, p'), "Post-Array Initializer"))
						else let* ((tau_o, el), tkRem4) = parseBraceWrap parseArrayInner "Array Initializer" tkRem3 in
							let* il = forceIntList el_x in
							Valid (DataArrayExp(i, tau_o, il, el, p), tkRem4)
					| _ -> Valid (DataArrayExp(i, tau_ox, [List.length el_x], el_x, p), tkRem3)
				)
		)
		| _ ->
			let* (x, tkRem2) = parseDerefTId "Struct Initializer" tkRem in
			let* (fl, tkRem3) = parseBrackWrap
				(parseOrEmpty (parseSepList parseStructInit chkComma) chkBrackR) "Struct Initializer" tkRem2 in
			Valid (NewStructExp(x, fl, p), tkRem3)
			(*
		| (TID x, _) :: tkRem2 ->
		| tk :: _ -> Error (BadToken_Err(tk, "Heap Memory Initializer"))
		| _ -> Error (EOF_Err "Heap Memory Initializer")*)
	)
	(*| (VDIM, p) :: tkRem ->
		let* (el, tkRem2) = parseBraceWrap (parseOrEmpty (parseSepList parseExp chkComma) chkBrackR) "Array Initializer" tkRem in
		Valid (ValueArrayExp(el, p), tkRem2)*)
	| (LPAREN, p) :: _ ->
		let* (el, tkRem) = parseParenWrap (parseSepList parseExp chkComma) "Tag Value" tkList in
		if List.length el = 1 then Valid (List.hd el, tkRem)
		else Valid (TupleExp(None, el, p), tkRem)
	| (BAR, p) :: tkRem ->
		let* (e, tkRem2) = parseExp tkRem in
		let* (_, tkRem3) = parseTk BAR "Measure Operator" tkRem2 in
		Valid (AppExp(OpExp(MeasureOp, p), [e], p), tkRem3)

	| _ -> let* (xx, tkRem) = parseDerefXId "Exp" tkList in (match xx with
		ParseId(x, p) -> (match tkRem with
			(IS, _) :: tkRem2 ->
				let* (y, tkRem3) = parseDerefTId "`is` Expression" tkRem2 in Valid (IsExp(x, y, p), tkRem3)
			| _ -> Valid (VarExp(x, p), tkRem)
		)
		| ParseTId(x, p) -> (match tkRem with
			(LPAREN, _) :: _ ->
				let* (el, tkRem2) = parseParenWrap (parseSepList parseExp chkComma) "Enum/Union" tkRem in
				Valid (TupleExp(Some x, el, p), tkRem2)
			| _ -> Valid (TupleExp(Some x, [], p), tkRem)
		)
	)

	(*| (ID x, p) :: tkRem -> (match tkRem with
		(IS, _) :: tkRem2 -> let* (y, tkRem3) = parseDerefTId tkRem2 in Valid (IsExp(x, y, p), tkRem3)
		| _ -> Valid (VarExp(QN(None, x), p), tkRem)
	| (CID x, p) :: tkRem -> Valid (VarExp(QN(None, x), p), tkRem)
	| (TID prefix, p) :: tkRem -> (match tkRem with
		(DOT, _) :: tkRem2 -> parseIdAtomExp (QT (Some prefix)) tkRem2
		| (LPAREN, _) :: _ ->
			let* (el, tkRem2) = parseParenWrap (parseSepList parseExp chkComma) "Enum" tkRem in
			Valid (TupleExp(Some (QT None, prefix), el, p), tkRem2)
		| _ -> parseIdAtomExp (QT None) tkList
	)*)
	(*
	| tk :: _ -> Error (BadToken_Err(tk, "Exp"))
	| _ -> Error (EOF_Err "Exp")
*)

and parseArgList: m_exp list parser = fun tkList -> match tkList with
	(RPAREN, _) :: _ -> Valid ([], tkList)
	| _ -> parseSepList parseExp chkComma tkList

and parseArrayInner: (m_type option * m_exp list) parser = fun tkList -> match tkList with
	(TILDE, _) :: tkRem ->
		let* (tau, tkRem2) = parseType tkRem in
		Valid ((Some tau, []), tkRem2)
	| _ ->
		let* (el, tkRem) = parseSepList parseExp chkComma tkList in
		Valid ((None, el), tkRem)

(*
and parseArrayInner: (n_exp list * n_exp list * bool) parser = fun tkList ->
	let* (e0, tkRem) = parseExp tkList in (match tkRem with
		(BY, _) :: tkRem2 ->
			let* (dim_l, tkRem3) = parseSepList parseExp chkBy tkRem2 in
			let* (_, tkRem4) = parseTk BAR "Array Initializer (Bar)" tkRem3 in
			let* ((el, b), tkRem5) = parseArrayEnd tkRem4 in
			Valid ((e0 :: dim_l, el, b), tkRem5)
		| (COMMA, _) :: tkRem2 ->
			let* ((el, b), tkRem3) = parseArrayEnd tkRem2 in
			Valid (([], e0 :: el, b), tkRem3)
		| _ -> Valid (([], [e0], false), tkRem)
	)*)

and parseArrayEnd: (m_exp list * bool) parser = fun tkList ->
	let* (el, tkRem) = parseSepList parseExp chkComma tkList in
	let* (ellip, tkRem2) = parseOpt (parseTk ELLIP "Array Initializer (Ellip)") chkBrackR tkRem in
	Valid ((el, ellip <> None), tkRem2)

and parseStructInit: (string * m_exp) parser = fun tkList ->
	let* (x, tkRem) = parseDeclId "Struct Initializer" tkList in
	let* (_, tkRem2) = parseTk EQ "Struct Initializer" tkRem in
	let* (e, tkRem3) = parseExp tkRem2 in Valid ((x, e), tkRem3)

and parseAppObj: appObj parser = fun tkList -> match tkList with
	(LPAREN, p) :: _ -> 
		let* (el, tkRem) = parseParenWrap parseArgList "App" tkList in
		Valid (DirectApp(el, p), tkRem)
	| (LBRACE, p) :: _ ->
		let* (el, tkRem) = parseBraceWrap (parseSepList parseExp chkComma) "Array Index" tkList in
		Valid (ArrayIndexApp(el, p), tkRem)
	| (DOT, p) :: tkRem -> (match tkRem with
		(ID "t", _) :: tkRem2 -> Valid (TupleTagApp p, tkRem2)
		| (ID "i", _) :: tkRem2 -> Valid (EnumRawApp p, tkRem2)
		| (ID f, _) :: tkRem2 -> (match tkRem2 with
			(LPAREN, _) :: _ ->
				let* (el, tkRem3) = parseParenWrap parseArgList "Function Call" tkRem2 in
				Valid (IndirectApp(qn f, el, p), tkRem3)
			| _ -> Valid (StructFieldApp(f, p), tkRem2)
		)
		| (INT i, _) :: tkRem2 ->
			Valid (TupleIndexApp(i, p), tkRem2)
		| tk :: _ ->  Error (BadToken_Err(tk, "Property / Function Call"))
		| _ -> Error (EOF_Err "Property / Function Call")
	)
	| _ -> failwith "BUG: Unexpected case while parsing application object."

and parseAppExp: m_exp parser = fun tkList ->
	let* (e, tkRem) = parseAtomExp tkList in
	let* (appList, tkRem2) = parseList parseAppObj chkAppEnd tkRem in
	Valid (foldAppList e appList, tkRem2)

and parseUniExp: m_exp parser = fun tkList ->
	let* (eOpList, tkRem) = parseList (parseTkMulti readUniOp "Unary Operator") chkUniEnd tkList in
	let* (e, tkRem2) = parseAppExp tkRem in
	Valid (foldOpListU e eOpList, tkRem2)

and parseMulExp: m_exp parser = fun tkList ->
	let* ((e0, eOpList), tkRem) = parseSepListFull parseUniExp readMulOp tkList in
	Valid (foldOpListL e0 eOpList, tkRem)

and parseAddExp: m_exp parser = fun tkList ->
	let* ((e0, eOpList), tkRem) = parseSepListFull parseMulExp readAddOp tkList in
	Valid (foldOpListL e0 eOpList, tkRem)

and parseArrOpExp: m_exp parser = fun tkList ->
	let* (e1, tkRem) = parseAddExp tkList in (match tkRem with
		(PLUS_EQ, p) :: tkRem2 ->
			let* (e2, tkRem3) = parseRelExp tkRem2 in
			Valid (AppExp(OpExp(ArrayAddOp, p), [e1; e2], p), tkRem3)
		| (SLASH_EQ, p) :: tkRem2 ->
			let* (e2, tkRem3) = parseRelExp tkRem2 in
			Valid (AppExp(OpExp(ArrayRemoveOp, p), [e1; e2], p), tkRem3)
		| _ -> Valid (e1, tkRem)
	)

and parseCompExp: m_exp parser = fun tkList ->
	let* ((e0, eOpList), tkRem) = parseSepListFull parseArrOpExp readCompOp tkList in
	Valid (foldOpListL e0 eOpList, tkRem)

and parseRelExp: m_exp parser = fun tkList ->
	let* ((e0, eOpList), tkRem) = parseSepListFull parseCompExp readRelOp tkList in
	Valid (foldOpListL e0 eOpList, tkRem)

and parseExp tkList = parseRelExp tkList

	(* statement parsing *)

let parsePat: string option parser = fun tkList -> match tkList with
	(UNDERSCORE, _) :: tkRem -> Valid (None, tkRem)
	| (ID x, _) :: tkRem -> Valid (Some x, tkRem)
	| tk :: _ -> Error (BadToken_Err(tk, "Pattern"))
	| _ -> Error (EOF_Err "Pattern")

let rec parseStmt: m_stmt parser = fun tkList -> match tkList with
	(VAR, p) :: tkRem ->
		let* (px, tkRem2) = (match tkRem with
			(LPAREN, _) :: _ ->
				let* (xl, tkRem2) = parseParenWrap (parseSepList parsePat chkComma) "Var Pattern" tkRem in
				Valid (ListPat xl, tkRem2)
			| _ -> let* (x, tkRem2) = parseDeclId "Var Stmt" tkRem in Valid (VarPat x, tkRem2)
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
	| (GC_COLLECT, p) :: tkRem ->
		Valid (GCCollectStmt p, tkRem)
	| (tk, p) :: _ ->
		let* (e, tkRem) = parseArrOpExp tkList in (match tkRem with
		(EQ, _) :: tkRem2 -> (match asLvalue e with
			None -> Error (BadLValue_Err(tk, ann_exp e))
			| Some lv ->
				let* (e, tkRem3) = parseExp tkRem2 in
				Valid (completeAssign lv e, tkRem3))
		| _ -> Valid (EvalStmt(e, p), tkRem))
	| _ -> Error (EOF_Err "Stmt")

and parseCond (sep: raw_token) (x: string): (m_exp * m_stmt list) parser = fun tkList ->
	let* (ec, tkRem) = parseExp tkList in
	let* (_, tkRem2) = parseTk sep x tkRem in
	let* (body, tkRem3) = parseList parseStmt chkStmtEnd tkRem2 in
	Valid ((ec, body), tkRem3)

and parseElsif: m_stmt list parser = fun tkList -> match tkList with 
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

and parseRange: (string * range_type * m_exp) parser = fun tkList ->
	let* (x, tkRem) = parseDeclId "For Loop Range" tkList in
	let* (rt, tkRem2) = parseRangeType tkRem in
	let* (e, tkRem3) = parseExp tkRem2 in
	Valid ((x, rt, e), tkRem3)

	(* method parsing *)

let parseParam: (string * m_type) parser = fun tkList ->
	let* (t, tkRem) = parseType tkList in
	let* (x, tkRem2) = parseDeclId "Parameter List" tkRem in
	Valid ((x, t), tkRem2)

let parseParamList: (string * m_type) list parser = fun tkList -> match tkList with
	(RPAREN, _) :: _ -> Valid ([], tkList)
	| _ -> parseSepList parseParam chkComma tkList

let parseFnMain: ((string * m_type) list * m_type * m_stmt list) parser = fun tkList ->
	let* (pl, tkRem) = parseParenWrap parseParamList "Parameter List" tkList in
	let* (tau_r, tkRem2) = parseRetTy tkRem in
	let* (sl, tkRem3) = parseList parseStmt chkStmtEnd tkRem2 in
	let* (_, tkRem4) = parseTk END "Method" tkRem3 in
	Valid ((pl, tau_r, sl), tkRem4)

let parseMet (lf: lin_flag): m_met parser = fun tkList ->
	let* (f, tkRem) = parseDeclId "Function Declaration" tkList in
	let* ((pl, tau_r, body), tkRem2) = parseFnMain tkRem in
	Valid (Method(lf, qn f, pl, tau_r, body), tkRem2)

	(*
		enum / union / enum extension parsing
		- superficially, they are all parsed the same way.
		- at parse-time, attributes are always split off into an enum extension.
	*)

let parseFieldDef (debug: string): (string * m_type) parser = fun tkList ->
	let* (t, tkRem) = parseType tkList in
	let* (x, tkRem2) = parseDeclId debug tkRem in
	Valid ((x, t), tkRem2)

let parseAttrs: m_field_list parser = fun tkList -> match tkList with
	(ATTRS, _) :: tkRem ->
		let* (fl, tkRem2) = parseBrackWrap (parseSepList (parseFieldDef "Enum Attr Definition") chkComma)
			"Enum Attr Definition" tkRem in
		Valid (fl, tkRem2)
	| _ -> Valid ([], tkList)

let parseAttrInit: m_field_init parser = fun tkList -> match tkList with
	(LBRACK, _) :: _ ->
		parseBrackWrap (parseSepList parseStructInit chkComma) "Enum/Union Case Attribute" tkList
	| _ -> Valid ([], tkList)

(*let parseEnumCase: (string * (string * n_exp) list) list parser = fun tkList ->
	let* (ctor, tkRem) = parseTId tkList in
	let* (al, tkRem2) = parseCaseAttr tkRem in Valid ((ctor, al), tkRem2) *)

let parseUnionCase: (qual_name * m_type list * m_field_init) parser = fun tkList ->
	let* (ctor, tkRem) = parseDeclTId "Union Case Definition" tkList in
	let* (tau_l, tkRem2) = (match tkRem with
		(LPAREN, _) :: _ ->
			parseParenWrap (parseSepList parseType chkComma) "Union Case Definition" tkRem
		| _ -> Valid ([], tkRem)
	) in (*Valid ((ctor, tau_l), tkRem2)*)
	let* (al, tkRem3) = parseAttrInit tkRem2 in Valid ((qn ctor, tau_l, al), tkRem3)

let parseUnionDef (p: l_pos): (qual_name * (qual_name union_case) list * m_dec list) parser = fun tkList ->
	let* (x, tkRem) = parseDeclTId "Enum/Union Definition" tkList in
	let* (al, tkRem2) = parseAttrs tkRem in
	(match tkRem2 with
		(EQ, _) :: tkRem3 ->
			let* (cl, tkRem4) = parseSepList parseUnionCase chkBar tkRem3 in
			let cl' = List.map (fun (ctor, tau_l, _) -> (ctor, tau_l, NoEB)) cl in
			let extOpt =
				if List.exists (fun (_, _, al) -> List.length al > 0) cl then
						(* TODO: implement error if cases are defined while extending *)
					let ext_cl' = List.map (fun (ctor, _, al) -> ((ctor, NoEB), al)) cl in
					[ExtendsDec(qn x, al, ext_cl', p)]
				else if List.length al > 0 then
					failwith "UNIMPLEMENTED ERROR: parse_exp.ml - Enum/Union defined with attributes, but without attribute initialization."
				else []
			in Valid ((qn x, cl', extOpt), tkRem4)
		| _ ->
			let extOpt = if List.length al > 0 then [ExtendsDec(qn x, al, [], p)] else [] in
			Valid ((qn x, [], extOpt), tkRem2)
	)

let parseEnumDefFull (extendFlag: bool) (p: l_pos): (m_dec list) parser = fun tkList ->
	let* ((x, cl, extOpt), tkRem) = parseUnionDef p tkList in
		(* TODO: throw error for "empty", non-extensible enum *)
	if List.exists (fun (_, tau_l, _) -> List.length tau_l > 0) cl then
		failwith "UNIMPLEMENTED ERROR: parse_exp.ml - Enum defined with data, try defining a union instead."
	else let cl' = List.map (fun (x, _, eb) -> (x, eb)) cl in
		Valid ((TDefDec(x, EnumTD(extendFlag, cl'), p)) :: extOpt, tkRem)

let parseUnionDefFull (p: l_pos): (m_dec list) parser = fun tkList ->
	let* ((x, cl, extOpt), tkRem) = parseUnionDef p tkList in
		(* TODO: throw error for "empty", non-extensible union *)
	Valid ((TDefDec(x, UnionTD cl, p)) :: extOpt, tkRem)

let parseExtendsOnly (p: l_pos): (m_dec list) parser = fun tkList ->
	let* ((_, _, extOpt), tkRem) = parseUnionDef p tkList in Valid (extOpt, tkRem)

(*
let parseEnumDefFull (extendFlag: bool) (p: l_pos): (n_dec list) parser = fun tkList ->
	let* ((d, attrOpt), tkRem) = parseEnumDef extendFlag p tkList in (match attrOpt with
		None -> Valid ([d], tkRem)
		| Some attr -> Valid ([d; ExtendsDec(attr, [], p)], tkRem)
	)
*)

	(* declaration / type definition parsing *)

let parseGlobArg: qual_name option parser = fun tkList -> match tkList with
	(IN, _) :: tkRem -> let* (x, tkRem2) = parseDeclTId "Global Context" tkRem in Valid (Some (qn x), tkRem2)
	| _ -> Valid (None, tkList)

let parseDec: (m_dec list) parser = fun tkList -> match tkList with
	(FN, p) :: tkRem ->
		let* (m, tkRem2) = parseMet Fn tkRem in
		Valid ([FunDec(m, p)], tkRem2)
	| (LIN, p) :: tkRem ->
		let* (m, tkRem2) = parseMet Lin tkRem in
		Valid ([FunDec(m, p)], tkRem2)
	| (STRUCT, p) :: tkRem ->
		let* (x, tkRem2) = parseDeclTId "Struct Definition" tkRem in
		let* (fl, tkRem3) = parseBrackWrap (parseSepList (parseFieldDef "Struct Definition") chkComma)
			"Struct Definition" tkRem2 in
		Valid ([TDefDec(qn x, StructTD fl, p)], tkRem3)
	| (ENUM, p) :: tkRem ->	parseEnumDefFull false p tkRem
	| (ENUMP, p) :: tkRem -> parseEnumDefFull true p tkRem
	| (EXTENDS, p) :: tkRem ->
		let* (_, tkRem2) = parseTk ENUM "Enum Extension" tkRem in
		parseExtendsOnly p tkRem2
	| (UNION, p) :: tkRem -> parseUnionDefFull p tkRem
		(*let* (x, tkRem2) = parseTId tkRem in
		let* (_, tkRem3) = parseTk EQ "Enum Definition" tkRem2 in
		let* (cl, tkRem4) = parseSepList parseCaseDef chkBar tkRem3 in
		Valid ([TDefDec(x, UnionTD (List.map (fun (x, tl) -> (x, tl, NoEB)) cl), p)], tkRem4)*)
	| (CONST, p) :: tkRem ->
		let* (x, tkRem2) = parseDeclCId "Constant Declaration" tkRem in
		let* (_, tkRem3) = parseTk EQ "Constant Declaration" tkRem2 in
		let* (e, tkRem4) = parseExp tkRem3 in
		Valid ([ConstDec(qn x, e, p)], tkRem4)
	| (GLOBALS, p) :: tkRem ->
		let* (x, tkRem2) = parseDeclTId "Global Declaration" tkRem in
		let* (cont, tkRem3) = parseGlobArg tkRem2 in
		let* (fl, tkRem4) = parseBrackWrap (parseSepList parseStructInit chkComma) "Global Initializer" tkRem3 in
		let fl' = List.map (fun (f, e) -> (qn f, e)) fl in
		Valid ([GlobalsDec(qn x, cont, fl', p)], tkRem4)
	| tk :: _ -> Error (BadToken_Err(tk, "Dec"))
	| _ -> Error (EOF_Err "Dec")

	(* full file parsing *)

let parseReq: m_req parser = fun tkList -> match tkList with
	(REFERENCES, p) :: tkRem ->
		let* (c, tkRem2) = parseDeclTId "Module Requirement" tkRem in (match tkRem2 with
			(DOT, _) :: tkRem3 ->
				let* (c2, tkRem4) = parseDeclTId "Module Requirement" tkRem3 in
				Valid (ShortRefReq([c; c2], p), tkRem4)
			| (MODULES, _) :: tkRem3 ->
				let* (cl, tkRem4) = parseSepList (parseDeclTId "Module Requirement") chkComma tkRem3 in
				Valid (LongRefReq([c], cl, p), tkRem4)
			| _ -> failwith "UNIMPLEMENTED: dusk_parse.ml - other kinds of references"
		)
	| tk :: _ -> Error (BadToken_Err(tk, "Module Requirement"))
	| _ -> Error (EOF_Err "Module Requirement")

let parseMain (tkList: token list): m_section parse_res =
	let* (rList, tkRem) = (parseList parseReq chkReqEnd) tkList in 
	let* (dList, _) = (parseList parseDec neverEnd) tkRem in Valid (Section(rList, List.concat dList))

	(* table of contents parsing *)

let parseChapter: string parser = fun tkList ->
	let* (_, tkRem) = parseTk CHAPTER "Module Chapter" tkList in
	let* (x, tkRem2) = parseDeclTId "Module Chapter" tkRem in Valid (x, tkRem2)

let parseModule: m_mod parser = fun tkList ->
	let* (p, tkRem) = parseTk MODULE "Module" tkList in
	let* (m, tkRem2) = parseDeclTId "Module" tkRem in
	let* (rList, tkRem3) = (parseList parseReq chkReqEnd) tkRem2 in
	let* (xl, tkRem4) = (parseList parseChapter chkPureEnd) tkRem3 in
	let* (_, tkRem5) = parseTk END "Module Chapter" tkRem4 in
	Valid (Module(m, rList, xl, p), tkRem5)

let parseToc (tkList: token list): m_toc parse_res =
	let* (mList, _) = (parseList parseModule neverEnd) tkList in Valid (Toc mList)
