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
	bcall(_GetKey)
	ld de,input_JT2
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
	
; uses GetKey codes, not GetCSC
input_JT2:
	.dw eval_handle_none	;$00
	.dw eval_handle_arrow	;$01
	.dw eval_handle_arrow	;$02
	.dw eval_handle_arrow	;$03
	.dw eval_handle_arrow	;$04
	.dw eval_handle_enter	;$05
	fill_word($09-$05-1, eval_handle_none)
	.dw eval_handle_clear	;$09
	.dw eval_handle_del		;$0A
	fill_word($0B-$0A-1, eval_handle_none)
	.dw eval_handle_insert	;$0B
	.dw eval_handle_none	;$0C
	.dw eval_handle_none	;$0D
	.dw eval_handle_none	;$0E
	.dw eval_handle_none	;$0F
	fill_word($2E-$0F-1, eval_handle_none)
	.dw eval_handle_none	;$2E
	.dw eval_handle_none	;$2F
	fill_word($31-$2F-1, eval_handle_none)
	.dw eval_handle_none	;$31
	.dw eval_handle_none	;$32
	fill_word($35-$32-1, eval_handle_none)
	.dw eval_handle_none	;$35
	fill_word($44-$35-1, eval_handle_none)
	.dw eval_handle_none	;$44
	.dw eval_handle_none	;$45
	fill_word($48-$45-1, eval_handle_none)
	.dw eval_handle_none	;$48
	.dw eval_handle_none	;$49
	fill_word($5A-$49-1, eval_handle_none)
	.dw eval_handle_none	;$5A
	fill_word($80-$5A-1, eval_handle_none)
	.dw eval_handle_char	;$80 +
	.dw eval_handle_char	;$81 -
	.dw eval_handle_char	;$82 *
	.dw eval_handle_char	;$83 /
	fill_word($85-$83-1, eval_handle_none)
	.dw eval_handle_char	;$85 (
	.dw eval_handle_char	;$86 )	
	fill_word($8A-$86-1, eval_handle_none)
	.dw eval_handle_none	;$8A
	.dw eval_handle_none	;$8B
	fill_word($97-$8C+1, eval_handle_char)
	fill_word($B4-$97-1, eval_handle_none)
	.dw eval_handle_none	;$B4
	fill_word($B7-$B4-1, eval_handle_none)
	.dw eval_handle_none	;$B7
	fill_word($B9-$B7-1, eval_handle_none)
	.dw eval_handle_none	;$B9
	fill_word($BB-$B9-1, eval_handle_none)
	.dw eval_handle_none	;$BB
	fill_word($BD-$BB-1, eval_handle_none)
	.dw eval_handle_none	;$BD
	.dw eval_handle_none	;$BF
	fill_word($C1-$BF-1, eval_handle_none)
	.dw eval_handle_none	;$C1
	fill_word($FB-$C1, eval_handle_none)





	


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

eval_handle_insert:
eval_handle_2nd:
	ld a,(iy+textFlags)
	xor 1<<textInsMode
	ld (iy+textFlags),a
	ret

eval_handle_char:
	; a = CSC scan code
	ld hl,eval_k2char_LUT
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

eval_k2char_LUT:
	.fill $80,'?'	;$00-$80
	.db '+'			;$80
	.db '-'			;$81
	.db '*'			;$82
	.db '/'			;$83
	.db '?'			;$84
	.db '('			;$85
	.db ')'			;$86
	.fill $8C-$86-1,'?'
	.db '-'			;$8C
	.db '.'			;$8D
	.db '0'			;$8E
	.db '1'			;$8F
	.db '2'			;$90
	.db '3'			;$91
	.db '4'			;$92
	.db '5'			;$93
	.db '6'			;$94
	.db '7'			;$95
	.db '8'			;$96
	.db '9'			;$97
	.fill $FB-$97,'?'

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
	
	
	