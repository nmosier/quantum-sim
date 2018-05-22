;; evaluator
;; quantum_sim.wcodeproj
;;
;; Created by Nicholas Mosier on 05/13/2018

;; FLAGS: offset from SAFERAM appData
evalFlags .equ appData
ketMode .equ 0 ; if SET, then disp state as ints; RESET, disp as bits

inputBuffer_curP .equ appData+1
inputBuffer_endP .equ appData+3
scrap .equ appData+5
scrapEnd .equ appData+11

;; define structs
QuditState_nbits .equ 0
QuditState_nstates .equ 1
QuditState_states .equ 2

SBState_size .equ 3
SBState_amp .equ 0
SBState_val .equ 1

;; QuditState layout
;; - nbits: 1 byte
;; - nstates: 1 byte
;; - states: nstates bytes
;; - amplitudes: nstates*18 bytes

;; Qans qubit state used as accumulator

#macro fill_word(count, data)
	#if count > 0
		.dw data
		fill_word(count-1, data)
	#endif
#endmacro

#macro pushw(imm16)
	ld hl,imm16
	push hl
#endmacro



inputBuffer .equ saveSScreen
inputBuffer_end .equ saveSScreen+767	; reserve 767th byte for null-terminator
inputBuffer_size .equ 767


eval_get_input:
	ld hl,inputBuffer
	ld (inputBuffer_curP),hl ; reset input buffer pointer
	ld (inputBuffer_endP),hl

	bcall(_ClrScrnFull)
	ld hl,0
	ld (curRow),hl
	;set curAble,(iy+curFlags)	; blinking cursor
	bcall(_CursorOn)

eval_get_input_loop:
	bcall(_GetCSC)
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

input_JT:
	.dw eval_handle_none	;$00
	.dw eval_handle_arrow	;$01 V
	.dw eval_handle_arrow	;$02 <
	.dw eval_handle_arrow	;$03 >
	.dw eval_handle_arrow	;$04 ^
	fill_word($09-$04-1, eval_handle_none)
	.dw eval_handle_enter	;$09 ENTER
	.dw eval_handle_char	;$0A +
	.dw eval_handle_char	;$0B -
	fill_word($0F-$0B-1, eval_handle_none)
	.dw eval_handle_clear	;$0F CLEAR
	fill_word($11-$0F-1, eval_handle_none)
	.dw eval_handle_char	;$11 -
	.dw eval_handle_char	;$12 3
	.dw eval_handle_char	;$13 6
	.dw eval_handle_char	;$14 9
	.dw eval_handle_char	;$15 )
	fill_word($17-$15-1, eval_handle_none)
	.dw eval_handle_vars	;$17 VARS
	fill_word($19-$17-1, eval_handle_none)
	.dw eval_handle_char	;$19 .
	.dw eval_handle_char	;$1A 2
	.dw eval_handle_char	;$1B 5
	.dw eval_handle_char	;$1C 8
	.dw eval_handle_char	;$1D (
	fill_word($21-$1D-1, eval_handle_none)
	.dw eval_handle_char	;$21 0
	.dw eval_handle_char	;$22 1
	.dw eval_handle_char	;$23 4
	.dw eval_handle_char	;$24 7
	fill_word($28-$24-1, eval_handle_none)
	.dw eval_handle_gates	;$28 6
	fill_word($36-$28-1, eval_handle_none)
	.dw eval_handle_2nd		;$36 2md
	fill_word($38-$36-1, eval_handle_none)
	.dw eval_handle_del		;$38 DEL
	
	
eval_handle_none:
	ret

eval_handle_arrow:
	bcall(_CursorOff)
	ld hl,_
	push hl	; return to end of fn
	cp skLeft
	jr z,eval_move_cursor_left
	cp skRight
	jr z,eval_move_cursor_right
	cp skDown
	jp z,eval_move_cursor_down
	cp skUp
	jp z,eval_move_cursor_up
_	bcall(_CursorOn)
	ret
	
eval_move_cursor_right:
	ld hl,(inputBuffer_endP)
	ld de,(inputBuffer_curP)
	inc de	;; tenative new cursor ptr
	ld a,d
	cp h
	jr c,_
	ld a,l
	cp e
	ret c	; if curP > endP, then abort
_	ld hl,(curRow)
	ld a,h
	cp 15	; curCol = 15?
	jr nz,_
	ld a,l
	cp 7	; curRow = 7?
	call z,eval_scroll_down	;(15,7) -> need to scroll down
	ld hl,(curRow)
	ld a,l
	cp 7
	ret z
	inc l	; curRow++
	ld h,-1	; curCol=-1
_	inc h	; curCol++
	ld (curRow),hl
	ld de,(inputBuffer_curP)
	inc de
	ld (inputBuffer_curP),de
	ret
	
eval_move_cursor_left:
	ld hl,(curRow)
	ld a,h
	or l
	call z,eval_scroll_up
	ld hl,(curRow)
	ld a,h
	or l
	ret z
	ld a,h
	or a
	jr nz,_	; skip if col > 0
	ld h,16
	dec l	; row--
_	dec h	; col--
	ld (curRow),hl
	ld hl,(inputBuffer_curP)
	dec hl
	ld (inputBuffer_curP),hl
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

eval_handle_2nd:
	ld a,(iy+textFlags)
	xor 1<<textInsMode
	ld (iy+textFlags),a
	ret

eval_handle_char:
	; a = CSC scan code
	ld hl,eval_CSC2char_LUT
	ld d,0
	ld e,a
	add hl,de
	ld a,(hl)
	
	ld hl,_
	push hl
	bcall(_CursorOff)	
	bit textInsMode,(iy+textFlags)
	jp z,eval_overwrite_char
	jp eval_insert_char
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

eval_handle_braket:
	ret

eval_CSC2char_LUT:
	.fill $0A-$00,'?'
	.db '+'	; $0A
	.db '-' ; $0B
	.fill $11-$0B-1,'?'
	.db $1A ; $11
	.db '3' ; $12
	.db '6' ; $13
	.db '9' ; $14
	.fill $19-$14-1,'?'
	.db '.' ; $19
	.db '2' ; $1A
	.db '5' ; $1B
	.db '8' ; $1C
	.fill $21-$1C-1,'?'
	.db '0' ; $21
	.db '1' ; $22
	.db '4' ; $23
	.db '7' ; $24
	.fill $38-$24,'?'

eval_overwrite_char:
	; INPUT: a = char to put
	ld b,a
	ld de,(inputBuffer_endP)
	ld hl,-inputBuffer_end
	add hl,de
	ld a,h
	or l
	ret z
	ld a,b
	
	bcall(_PutC)
	;; update buffer
	ld hl,(inputBuffer_curP)
	ld de,(inputBuffer_endP)
	ld (hl),a	; store character into buffer
	inc hl
	ld a,h	; check if curP > endP
	cp d
	jr c,eval_overwrite_char_more
	jr nz,_
	ld a,e
	cp l
	jr nc,eval_overwrite_char_more	; if curP <= endP, skip
_	ld (inputBuffer_endP),hl ;; if hl=de, also update endP
	ld (hl),0	; update end
eval_overwrite_char_more:
	ld (inputBuffer_curP),hl
	;; check cursor position
	ld a,(curRow)
	cp 8
	call z,eval_scroll_down	; if row=8, scroll down
	ret

eval_insert_char:
	; INPUT: a = char to insert
	ld b,a
	ld de,(inputBuffer_endP)
	ld hl,-inputBuffer_end
	add hl,de
	ld a,h
	or l
	ret z
	ld a,b
	
	ld e,a
	
	ld bc,(inputBuffer_curP)
	ld a,$FF
	xor b
	ld b,a
	ld a,$FF
	xor c
	ld c,a
	inc bc ;; bc = -curPtr
	ld hl,(inputBuffer_endP)
	add hl,bc	; inputBuffer_endP - inputBuffer_curP
	ld b,h
	ld c,l
	ld a,h
	or l
	ld a,e
	jr z,eval_overwrite_char
	
	push af
	
	ld de,(inputBuffer_endP)
	inc de
	xor a
	ld (de),a
	dec de
	ld h,d
	ld l,e	; hl = de
	dec hl
	lddr
	
	pop af
	
	ld hl,(inputBuffer_endP)
	inc hl
	ld (inputBuffer_endP),hl
	ld hl,(inputBuffer_curP)
	ld (hl),a
	bcall(_PutC)
	inc hl
	ld (inputBuffer_curP),hl
	ld de,(curRow)	; preserve  cursor loc
	bcall(_PutS)
	ld (curRow),de
	
	;; carry flag reset (nc) if full str didn't display
	;; in which case, recurse IF (curRow != 0)
	ret c
	ld a,(curRow)
	or a
	ret z	; return if row = 0; can't scroll
	jr eval_scroll_down

	
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
	
	
	