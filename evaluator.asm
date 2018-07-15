;; evaluator
;; quantum_sim.wcodeproj
;;
;; Created by Nicholas Mosier on 05/13/2018

;; FLAGS: offset from SAFERAM appData
#include "tables.asm"


eval_get_input:
	ld hl,inputBuffer
	ld (inputBuffer_curP),hl ; reset input buffer pointer
	ld (inputBuffer_endP),hl
	ld hl,0
	ld (inputBuffer_offsetE),hl

	bcall(_ClrScrnFull)
	ld hl,0
	ld (curRow),hl
	bcall(_CursorOn)

eval_get_input_loop:
	bcall(_GetKey)
	ld de,input_JT
	ld h,0
	ld l,a
	add hl,hl
	add hl,de
	ld de,eval_get_input_loop
	push de
	ld e,(hl)
	inc hl
	ld h,(hl)
	ld l,e
	jp (hl)

eval_handle_none:
	ret

eval_handle_arrow:
	bcall(_CursorOff)
	ld hl,_
	push hl	; return to end of fn
	cp kLeft
	jr z,eval_move_cursor_left
	cp kRight
	jr z,eval_move_cursor_right
	cp kDown
	jp z,eval_move_cursor_down
	cp kUp
	jp z,eval_move_cursor_up
_	bcall(_CursorOn)
	ret
	
eval_move_cursor_right:
	ld hl,(inputBuffer_offsetE)
	ld a,h
	or l
	ret z	; return if already at end of input
	dec hl
	ld (inputBuffer_offsetE),hl
	;; otherwise, find len of current token
	ld hl,(inputBuffer_curP)
	ld a,(hl)
	inc hl
	ld (inputBuffer_curP),hl
	call tok2len
	ld d,a
	ld e,0
	ld hl,(curRow)
	add hl,de
	call cursor_adjust
	ld (curRow),hl
	ld a,7
	cp l
	call c,eval_scroll_down
	ret

eval_move_cursor_left:
	ld hl,(inputBuffer_curP)
	ld de,inputBuffer
	ld a,d
	cp h
	jr c,_
	ld a,e
	cp l
	ret nc	; check that cursor not at start
_	
	dec hl
	ld (inputBuffer_curP),hl
	ld a,(hl)
	call tok2len
	ld hl,(curRow)
	neg
	add a,h
	ld h,a
	call cursor_adjust
	ld (curRow),hl
	ld de,(inputBuffer_offsetE)
	inc de
	ld (inputBuffer_offsetE),de
	bit 7,l	;; check if row neg
	call nz,eval_scroll_up
	ret

eval_move_cursor_down:
	ld a,(curRow)
	cp 7
	call nc,eval_scroll_down
	ret nc	; return if failure to scroll down. Note if a < 7, then will not call fn or return
	ld hl,(inputBuffer_curP)
	ld de,(inputBuffer_offsetE)
	ld a,16
	call scantoklen_fwd
	;; b = total length of tokens
	;; c = # of tokens
	ld (inputBuffer_curP),hl
	ld (inputBuffer_offsetE),de
	;; now update cursor
	ld hl,(curRow)
	ld c,0
	add hl,bc
	call cursor_adjust
	ld (curRow),hl
	ret
	
	
eval_move_cursor_up:
	ld a,(curRow)
	or a
	scf
	call z,eval_scroll_up
	ret nc	;; if try to scroll & fails, return
	ld hl,(inputBuffer_curP)
	ld de,-inputBuffer
	add hl,de
	ex de,hl	;; de = # of tokens preceding cursor position
	ld hl,(inputBuffer_curP)
	ld a,16
	call scantoklen_rev
	ld (inputBuffer_curP),hl	 ; update cursor position
	ld hl,(inputBuffer_offsetE)
	ld e,c
	ld d,0
	add hl,de
	ld (inputBuffer_offsetE),hl
	;; now update cursor
	ld a,b
	neg
	ld b,a
	ld c,0
	ld hl,(curRow)
	add hl,bc
	call cursor_adjust
	ld (curRow),hl
	ret
	

eval_handle_enter:
	ld hl,inputBuffer
	neg_hl
	ld de,(inputBuffer_endP)
	add hl,de
	ld b,h
	ld c,l	; ld bc,hl
	ld hl,inputBuffer
	call lex_input	;; lex input
	jr nc,_

	pop de	; don't return to eval_screen (yet?)
	
	ld hl,lexBuffer
	ld a,lex_EOF
	call parse
	ret
	
_	bjump(_ErrSyntax)
	ret

eval_handle_insert:
	ld a,(iy+textFlags)
	xor 1<<textInsMode
	ld (iy+textFlags),a
	ret

eval_handle_char:
	; a = GetKey scan code
	ld b,a
	ld de,(inputBuffer_endP)
	ld hl,-inputBuffer_end
	add hl,de
	ld a,h
	or l
	ret z	;; return if already maxed out buffer
	
	ld hl,k2tok_LUT
	ld d,0
	ld e,b
	add hl,de
	ld b,(hl)
	
	ld hl,_
	push hl
	bcall(_CursorOff)	
	ld hl,(inputBuffer_offsetE)
	ld a,h
	or l
	ld a,b
	jp z,eval_append_tok
	bit textInsMode,(iy+textFlags)
	jp z,eval_overwrite_tok
	jp eval_insert_tok
_	bcall(_CursorOn)
	ret

eval_handle_ket:
	
	ret

eval_handle_bra:
	ret

eval_handle_vars:
	ret

eval_handle_gates:
	ret

eval_handle_del:
	bcall(_CursorOff)

	ld bc,(inputBuffer_offsetE)
	ld a,b
	or c
	jr z,_	;; if at end, then nothing to delete
	
	ld hl,(inputBuffer_curP)
	inc hl
	ld de,(inputBuffer_curP)
	ld a,(de)
	ld (scrap+2),a
	ldir	;; overwrite token
	
	ld bc,(inputBuffer_offsetE)
	dec bc
	ld (inputBuffer_offsetE),bc
	ld bc,(inputBuffer_endP)
	dec bc
	ld (inputBuffer_endP),bc		; update offset & endp
	
	call tok2len
	call display_after_withspaces
	
_	bcall(_CursorOn)
	ret
	

eval_handle_clear:
	ld hl,inputBuffer
	ld (hl),0
	ld (inputBuffer_endP),hl
	ld (inputBuffer_curP),hl
	ld hl,0
	ld (inputBuffer_offsetE),hl
	ld (curRow),hl
	bcall(_ClrScrnFull)
	bcall(_CursorOn)
	ret


eval_append_tok:	
	ld hl,(inputBuffer_curP)
	ld (hl),b	; store new token
	inc hl
	ld (inputBuffer_curP),hl
	ld (inputBuffer_endP),hl
	ld (hl),0
	ld a,b
	call tok2str
	push hl
	ld a,(hl)
	ld hl,(curRow)
	add a,h
	ld h,a
	call cursor_adjust
	ld a,l
	cp 8
	ex de,hl					; preserve cursor position in de
	pop hl
	jp c,PutStr					; if row < 8, then just need to put string @ current cursor position
	ld (curRow),de				; if row ≥ 8, then just need to scroll down (which will display the new token, too)
	jp nc,eval_scroll_down		;  but needs updated cursor position

eval_overwrite_tok:
	;; INPUT: a = tok to overwrite with
	;; * assumes inputBuffer_curP < inputBuffer_endP *
	ld (scrap),a	; save new token
	call tok2len
	ld (scrap+1),a	;save new token len
	ld hl,(inputBuffer_curP)
	ld a,(hl)
	call tok2len
	ld hl,scrap+1
	sub (hl)
	ld (hl),a	; scrap+1 contains # of spaces to write at end
	dec hl
	ld a,(hl)	; load new token
	ld hl,(inputBuffer_curP)
	ld (hl),a
	inc hl
	ld (inputBuffer_curP),hl	; update cursor
	ld hl,(inputBuffer_offsetE)
	dec hl
	ld (inputBuffer_offsetE),hl	; update end offset
	
	ld hl,(curRow)
	push hl
	call puttokstr
	ld a,(scrap)
	call tok2len
	pop hl
	add a,h
	ld h,a
	call cursor_adjust
	ld (curRow),hl
	ld a,l
	cp 8
	jp nc,eval_scroll_down	; if overwriting token causes screen overflow, then scroll down instead
	
	ld a,(scrap+1)	; a = # of spaces to write
	bit 7,a
	ld hl,_
	push hl
	jp z,display_after_withspaces
	jp nz,display_after				;; these functions both return to the next line
_	ret


eval_insert_tok:
	; input: a = tok to insert
	; * guaranteed that curP not equal to endP
	ld hl,(inputBuffer_endP)
	ld de,(inputBuffer_endP)
	inc de
	ld (inputBuffer_endP),de
	ld bc,(inputBuffer_offsetE)
	inc bc
	ld (inputBuffer_offsetE),bc
	lddr
	ld (de),a
	
	push af
	call display_after
	pop af
	
	ld bc,(inputBuffer_offsetE)
	dec bc
	ld (inputBuffer_offsetE),bc
	ld hl,(inputBuffer_curP)
	inc hl
	ld (inputBuffer_curP),hl
	
	call tok2len
	ld hl,(curRow)
	ld d,a
	ld e,0
	add hl,de
	call cursor_adjust
	ld (curRow),hl
	ld a,7
	cp l
	call c,eval_scroll_down
	ret


eval_scroll_up:
	;; scrolls up one line
	;; carry flag SET if successful
	;; carry flag RESET if error
	ld a,(curRow)
	cp 7	; nc will be reset if a ≥ 7
	ret z	; if row=7, abort
;	inc a
;	ld (curRow),a
	ld a,16
	ld hl,curCol
	add a,(hl)
	ld de,(inputBuffer_curP)
	ld hl,-inputBuffer
	add hl,de
	ex de,hl
	push af
	call scantoklen_rev
	pop af
	cp b
	jr c,_
	ret nz	; note carry flag reset
_	ld hl,curRow
	inc (hl)
	call display_full
	scf
	ret

	
eval_scroll_down:
	;; scrolls down one line
	;; carry flag SET if successful
	;; carry flag RESET if error
	ld a,(curRow)
	or a	; resets carry flag (nc)
	ret z	; if row=0, abort
	bcall(_ClrScrnFull)
	
	ld hl,curRow
	dec (hl)
	call display_full
	scf
	ret