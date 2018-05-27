;; evaluator
;; quantum_sim.wcodeproj
;;
;; Created by Nicholas Mosier on 05/13/2018

;; FLAGS: offset from SAFERAM appData
#include "tables.asm"

evalFlags .equ appData
ketMode .equ 0 ; if SET, then disp state as ints; RESET, disp as bits

inputBuffer_curP .equ appData+1
inputBuffer_endP .equ appData+3
inputBuffer_offsetE .equ appData+5	; relative from end, i.e. pos = endP - curP
scrap .equ appData+7
scrapEnd .equ appData+13

inputBuffer .equ saveSScreen
inputBuffer_end .equ saveSScreen+767	; reserve 767th byte for null-terminator
inputBuffer_size .equ 767


eval_get_input:
	ld hl,inputBuffer
	ld (inputBuffer_curP),hl ; reset input buffer pointer
	ld (inputBuffer_endP),hl
	ld hl,0
	ld (inputBuffer_offsetE),hl

	bcall(_ClrScrnFull)
	ld hl,0
	ld (curRow),hl
	;set curAble,(iy+curFlags)	; blinking cursor
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
	call tok2titok
	bcall(_GetTokLen)
	ld d,a
	ld e,0
	ld hl,(curRow)
	add hl,de
	call cursor_adjust
	ld (curRow),hl
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
	call tok2titok
	bcall(_GetTokLen)
	ld hl,(curRow)
	neg
	add a,h
	ld h,a
	call cursor_adjust
	ld (curRow),hl
	ld hl,(inputBuffer_offsetE)
	inc hl
	ld (inputBuffer_offsetE),hl
	ret

eval_move_cursor_down:
	ld a,(curRow)
	cp 7
	call z,eval_scroll_down
	ld a,(curRow)
	cp 7
	ret z
	inc a
	ld b,a
	ld hl,(inputBuffer_curP)
	ld de,16
	add hl,de
	ld de,(inputBuffer_endP)
	ld a,d
	cp h
	ret c
	jr nz,_
	ld a,e
	cp l
	ret c
_	ld a,b
	ld (curRow),a
	ld (inputBuffer_curP),hl
	ret
	
eval_move_cursor_up:
	ld a,(curRow)
	or a
	call z,eval_scroll_up
	ld a,(curRow)
	or a
	ret z
	dec a
	ld (curRow),a
	ld hl,(inputBuffer_curP)
	ld de,-16
	add hl,de
	ld (inputBuffer_curP),hl
	ret
	

eval_handle_enter:
	pop de
	ret

eval_handle_insert:
	ld a,(iy+textFlags)
	xor 1<<textInsMode
	ld (iy+textFlags),a
	ret

;CONVERTED
eval_handle_char:
	; a = GetKey scan code
	ld b,a
	ld de,(inputBuffer_curP)
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


eval_handle_vars:
	ret

eval_handle_gates:
	ret

eval_handle_del:
	bcall(_CursorOff)
	;; opposite of insert
	ld hl,(inputBuffer_curP)
	ld bc,(inputBuffer_endP)
	ld a,h
	cpl
	ld h,a
	ld a,l
	cpl
	ld l,a
	inc hl	; 2's complement of hl, i.e. -inputBuffer_curP
	add hl,bc
	ld a,h
	or l
	jr z,_	; if curP=endP, then nothing to delete
	
	ld b,h
	ld c,l
	ld de,(inputBuffer_curP)
	ld hl,(inputBuffer_curP)
	inc hl
	ldir
	
	ld hl,(inputBuffer_endP)
	dec hl
	ld (inputBuffer_endP),hl
	; don't need to store 0 at endP, since lddr copied it
	; also don't need to update curP	
	ld hl,(inputBuffer_curP)
	ld de,(curRow)
	bcall(_PutS)
	ld a,' '
	bcall(_PutMap)	; overwrite last char
	ld (curRow),de
	
_	bcall(_CursorOn)
	ret

eval_handle_clear:
	bcall(_CursorOff)
	ld hl,inputBuffer
	ld (hl),0
	ld (inputBuffer_endP),hl
	ld (inputBuffer_curP),hl
	
	ld hl,0
	ld (curRow),hl
	bcall(_ClrScrnFull)
	bcall(_CursorOn)
	ret

; DONE
eval_append_tok:	
	ld hl,(inputBuffer_curP)
	ld (hl),b	; store new token
	inc hl
	ld (inputBuffer_curP),hl
	ld (inputBuffer_endP),hl
	ld (hl),0
	ld a,b
	call tok2titok	; get ti-tok to print
	bcall(_PutTokString)
	ret

eval_overwrite_tok:
	;; INPUT: a = tok to overwrite with
	;; * assumes inputBuffer_curP < inputBuffer_endP *
	ld (scrap),a	; save new token
	call tok2titok
	bcall(_GetTokLen)
	ld (scrap+1),a	;save new token len
	ld hl,(inputBuffer_curP)
	ld a,(hl)
	call tok2titok
	bcall(_GetTokLen)
	ld hl,scrap+1
	sub (hl)
	ld (hl),a	; scrap+1 contains # of spaces to write at end
	dec hl
	ld a,(hl)	; load new token
	ld hl,(inputBuffer_curP)
	ld (hl),a
	inc hl
	ld (inputBuffer_curP),hl	; update cursor
	call tok2titok
	bcall(_PutTokString)
	ld hl,(inputBuffer_offsetE)
	dec hl
	ld (inputBuffer_offsetE),hl	; update end offset
	ld a,(scrap+1)	; a = # of spaces to write
	or a
	ret z	; if old & new token lengths were same, then done
	;; otherwise need to update rest of display
	;; hl = offset
	ex de,hl	; use de as loop counter, note de > 0
	ld hl,(curRow)
	push hl		; save cursor position
	ld a,d
	or e
	jr z,_
	ld hl,(inputBuffer_curP)
eval_overwrite_tok_disploop:
	push de
	push hl
	ld a,(hl)
	call tok2titok
	bcall(_PutTokString)
	pop hl
	pop de
	inc hl
	dec de
	ld a,d
	or e
	jr nz,eval_overwrite_tok_disploop

_	ld a,(scrap+1)	; # of spaces to write
	bit 7,a			; test if a is negative
	jr nz,_
	ld b,a
eval_overwrite_tok_spaces:
	ld a,' '
	bcall(_PutC)
	djnz eval_overwrite_tok_spaces
	
_	pop hl
	ld (curRow),hl	; restore cursor position
	ret


eval_insert_tok:
	; input: a = tok to insert
	; * guaranteed that curP not equal to endP
	ld b,a
	ld hl,(inputBuffer_endP)
	ld de,inputBuffer_end	;; compare endP to end
	ld a,h
	cp d
	jr c,_
	ld a,l
	cp e
	ret nc	; return if endP ≥ end
_	ld a,b
	ld d,h
	ld e,l
	inc de
	ld (inputBuffer_endP),de
	ld bc,(inputBuffer_offsetE)
	inc bc
	lddr
	ld (de),a
	inc de
	ld (inputBuffer_curP),de
	call tok2titok
	bcall(_PutTokString)	; display new token
	ld hl,(inputBuffer_curP)
	ld bc,(inputBuffer_offsetE)
	ld de,(curRow)
	push de
eval_insert_tok_disploop:
	;; hl ptr to tok
	push hl
	push bc
	ld a,(hl)
	call tok2titok
	bcall(_PutTokString)
	pop bc
	pop hl
	inc hl
	dec bc
	ld a,b
	or c
	jr nz,eval_insert_tok_disploop
	pop de
	ld (curRow),de
	ret

	
eval_scroll_up:
	;; scrolls up one line
	bcall(_ClrScrnFull)
	ld de,(curRow)
	ld a,e
	cp 7
	adc a,0
	ld e,a	; e = min{7,row+1}
	
	ld a,e	; a = max{0,row-1}
	sla a
	sla a
	sla a
	sla a
	add a,d	; a = row*16 + col
	ld b,$FF
	neg
	ld c,a
	jr nz,_
	ld b,$00
_	ld hl,(inputBuffer_curP)
	add hl,bc
	;; comp w/ inputBuffer (begin)
	
	ld a,h
	cp inputBuffer>>8
	jr c,eval_scroll_update
	ld a,l
	cp inputBuffer&$00FF
	jr nc,eval_scroll_update
	ld hl,inputBuffer
	ld de,(curRow)
	jr eval_scroll_update
	
eval_scroll_down:
	;; scrolls down one line
	bcall(_ClrScrnFull)
	ld de,(curRow)
	xor a
	cp e
	jr z,_
	dec e
_	ld a,e	; a = max{0,row-1}
	sla a
	sla a
	sla a
	sla a
	add a,d	; a = row*16 + col
	ld b,$FF
	neg
	ld c,a
	jr nz,_
	ld b,$00
_	ld hl,(inputBuffer_curP)
	add hl,bc	; hl = buffer cursor ptr - (row*16+col)
eval_scroll_update:
	ld bc,0
	ld (curRow),bc
	bcall(_PutS)
	ld (curRow),de
	ret
	
	
	