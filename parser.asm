
;; parser.asm
;; quantum_sim.wcodeproj
;;
;; Created by Nicholas Mosier on 06/03/2018
;; parses stream of lexemes

; struct ASTNode<T> {
;	T data;
;	ASTNode *left;
;	ASTNode *right;

;; LUT for parser actions


;; parse
; entry point for parser
; initializes pointers
parse:
	ld (parser_lexP),hl ; save beginning of lex buffer
	call create_ast_appvar
	ld hl,ast_appvar_sym
	bcall(_Mov9ToOP1)
	bcall(_ChkFindSym)
	ld (parser_astVarP),de
	ld a,(de)
	ld l,a
	inc de
	ld a,(de)
	ld h,a	; hl = length of AST appvar
	ld (parser_astLen),hl
	inc de
	ld (parser_curP),de
	
	ret

;; 

; hl = beginning of part of lex stream
; de = (one past) end of lex stream
parse_expr:
	
parse_expr_loop:
	jr 
parse_expr_loop_cond:
	sbc hl,de
	
	jr nc,parse_expr_loop

#define AST_init_size 512

; create_ast_appvar 
create_ast_appvar:
	; first check if AST already exists. If so, resize
	ld hl,ast_appvar_sym
	bcall(_Mov9ToOP1)
	bcall(_ChkFindSym)
	jr c,create_ast_appvar_notfound ; skip ahead if not found
	; otherwise, check memory, insert if needed
	ld a,b
	or a
	jr z,create_ast_appvar_inRAM
	bcall(_Arc_Unarc)
	ld hl,ast_appvar_sym
	bcall(_Mov9ToOP1)
	bcall(_ChkFindSym)
create_ast_appvar_inRAM:
	; de = ptr to begin of AST appvar
	push de
	ld h,d
	ld l,e	; hl <= de
	ld a,(de)
	ld b,a
	inc de
	ld a,(de)
	ld e,b
	ld d,a	; de <= size of AST appvar
	add hl,de	; hl = (one past) end of appvar
	push hl
	ld a,d
	cpl
	ld d,a
	ld a,e
	cpl
	ld e,a
	inc de
	ld hl,AST_init_size
	add hl,de	; hl = # of bytes to insert
	bit 7,h
	jr nz,create_ast_appvar_noresize		; if curSize > sizeNeeded, then
	ld a,h
	or l
	jr z,create_ast_appvar_noresize		; return if 0 bytes need to be inserted
	pop de	; de = end of appvar
	bcall(_insertMem)
	pop hl
	ld de,AST_init_size
	ld (hl),e
	inc hl
	ld (hl),d
	ret
	
create_ast_appvar_noresize:
	pop hl
	pop hl
	ret
	
create_ast_appvar_notfound:
	ld hl,AST_init_size
	bcall(_CreateAppVar)
	ret

ast_appvar_sym:
	.db AppVarObj
	.db "Q_AST",0