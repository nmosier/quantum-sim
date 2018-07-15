;; lexer.asm
;; quantum_sim.wcodeproj
;;
;; Created by Nicholas Mosier on 06/02/2018

;; lexes input at home screen
;; output is array of lex_tokens

#include "lexer.inc"

;; lex_input: lexes input of qtokens
;; inputs: hl = beginning of tok arr
;; outputs: hl = ptr to array of lexemes
lex_input:
	ld de,lexBuffer
	ld (lexBuffer_curP),de
	ld (inputBuffer_lexP),hl
lex_input_mainloop:
	ld hl,(inputBuffer_lexP)
	ld a,(hl)
	call get_lex_tok_transition
	
	ld de,_
	push de
	jp (hl)
	; call (hl)
_	jr c,lex_input_mainloop
lex_done:
	ld hl,lexBuffer
	ret c		; return ptr to string of lexemes if successful
	;if nc, then syntax error
	ld hl,(inputBuffer_curP)
	ret nc		; return ptr to error qtoken if unsuccessful
	
;---------------;	
;; lexer actions
	
lex_none:
	ret

lex_error:
	scf
	ccf	; carry reset (nc)
	ret
	
lex_EOF:
	ld hl,(lexBuffer_curP)
	ld (hl),lexeme_EOF
	pop hl
	scf			;; mark success
	jr lex_done

lex_ket:
lex_bra:
lex_binop:
lex_lparen:
lex_rparen:
lex_unop:
lex_unpostop:
	; 1 byte: lexeme itself
	call tok2lexeme
	; a = lexeme, which contains full info needed for lexeme in this case
	ld hl,(lexBuffer_curP)
	ld (hl),a
	inc hl
	ld (lexBuffer_curP),hl
	ld hl,(inputBuffer_lexP)
	inc hl
	ld (inputBuffer_lexP),hl
	scf						; mark success by setting carry
	ret

lex_number:
	;; most compilated
	;; structure of lexeme entry:
	;; - 1 byte:  lexeme
	;; - 2 bytes: address of beginning
	;; - 1 byte:  length of #
	ld hl,(lexBuffer_curP)
	ld (hl),lexeme_number
	inc hl
	ld (lexBuffer_curP),hl
	ld bc,(inputBuffer_lexP)
	push bc
	xor a
	ld (scrap),a
	; byte (scrap) counts the number of tokens in the number lexeme
lex_number_loop:
	inc bc
	ld hl,scrap
	ld a,$FF
	cp (hl)
	jr z,_			
	inc (hl)		; inc token count iff length of number is < 256 tokens
_	ld a,(bc)
	call tok2lexeme	;; destroys hl,de
	cp lexeme_number	
	jr z,lex_number_loop
	ld (inputBuffer_lexP),bc
lex_number_done:
	pop bc		;; bc = start addr of number
	
	ld hl,(lexBuffer_curP)
	ld (hl),c
	inc hl
	ld (hl),b
	inc hl
	
	cp lexeme_error		; check for error in lexing (a = first non-number token)
	;ccf
	ret z				; note that CARRY is reset (nc) upon return, indicating error	
	ld a,(scrap)
	ld (hl),a
	inc hl
	ld (lexBuffer_curP),hl
	scf					; set CARRY to indicate success (c)
	ret


lex_func:
	; store 2 bytes:
	; - 1 byte: lexeme_func
	; - 1 byte: function token (e.g. qtSin)	
	ld hl,(lexBuffer_curP)
	ld (hl),lexeme_func
	inc hl
	ld (hl),a
	inc hl
	ld (lexBuffer_curP),hl		; update pointer in lexbuffer & pointer in input buffer
	ld hl,(inputBuffer_lexP)
	inc hl
	ld (inputBuffer_lexP),hl
	scf							; successful so set carry flag (c)
	ret

lex_action_LUT:
	.dw lex_EOF			;tqEOF			equ 00h
	.dw lex_binop		;tqPlus 		equ 01h
	.dw lex_binop		;tqMinus		equ 02h
	.dw lex_binop		;tqTimes		equ 03h
	.dw lex_binop		;tqDiv 			equ 04h
	.dw lex_lparen		;tqLeftP		equ 05h
	.dw lex_rparen		;tqRightP		equ 06h
	.dw lex_unop		;tqNeg			equ 07h
	.dw lex_number		;tqPeriod		equ 08h
	.dw lex_number		;tq0			equ 09h
	.dw lex_number		;tq1			equ 0Ah
	.dw lex_number		;tq2			equ 0Bh
	.dw lex_number		;tq3			equ 0Ch
	.dw lex_number		;tq4			equ 0Dh
	.dw lex_number		;tq5			equ 0Fh
	.dw lex_number		;tq6			equ 10h
	.dw lex_number		;tq7			equ 11h
	.dw lex_number		;tq8			equ 12h
	.dw lex_number		;tq9			equ 13h
	.dw lex_func		;qtSin
	.dw lex_func		;qtCos
	.dw lex_func		;qtTan
	.dw lex_unpostop	;qtSquare
	.dw lex_ket			;qtKet
	.dw lex_bra			;qtBra
	fill_word(256-($-lex_action_LUT)/2),lex_error)
	
tok2lexeme_LUT:
	.db lexeme_EOF			;tqEOF			equ 00h
	.db lexeme_binop		;tqPlus 		equ 01h
	.db lexeme_binop		;tqMinus		equ 02h
	.db lexeme_binop		;tqTimes		equ 03h
	.db lexeme_binop		;tqDiv 		equ 04h
	.db lexeme_lparen		;tqLeftP		equ 05h
	.db lexeme_rparen		;tqRightP	equ 06h
	.db lexeme_unop			;tqNeg		equ 07h
	.db lexeme_number		;tqPeriod	equ 08h
	.db lexeme_number		;tq0			equ 09h
	.db lexeme_number		;tq1			equ 0Ah
	.db lexeme_number		;tq2			equ 0Bh
	.db lexeme_number		;tq3			equ 0Ch
	.db lexeme_number		;tq4			equ 0Dh
	.db lexeme_number		;tq5			equ 0Fh
	.db lexeme_number		;tq6			equ 10h
	.db lexeme_number		;tq7			equ 11h
	.db lexeme_number		;tq8			equ 12h
	.db lexeme_number		;tq9			equ 13h
	.db lexeme_func	;qtSin
	.db lexeme_func	;qtCos
	.db lexeme_func	;qtTan
	.db lexeme_unpostop	;qtSquare
	.db lexeme_ket		;qtKet
	.db lexeme_bra		;qtBra
	.fill (256-($-tok2lexeme_LUT)),lexeme_error

;; tok2lexeme: convert qtok to lexeme, if applicable
; input: a = qtoken
; output: a = lexeme
; destroys: hl,de
tok2lexeme:
	ld hl,tok2lexeme_LUT
	ld e,a
	ld d,0
	add hl,de
	ld a,(hl)
	ret

; inputs: a = tok
; outputs: hl = address to jump to
; destroys: de
get_lex_tok_transition:
	ld hl,lex_action_LUT
	ld d,0
	ld e,a
	sla e
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	ex de,hl
	ret

