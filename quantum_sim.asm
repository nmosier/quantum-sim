;; quantum_sim.asm
;; quantum_sim
;;
;; Created by Nicholas Mosier on 05/12/2018

#include "ti83plus.inc"
#include "app.inc"
#include "macros.inc"
#include "equates.inc"

defpage(0)

main:
	bcall(_CursorOff)
	bcall(_ClrLCDFull)
	
	ld hl,0
	ld (curRow),hl	; set cursor to (0,0)

	call eval_get_input
done:
	bjump(_JForceCmdNoChar)
	
PutKet:
	; void PutKet(byte size, word num)
	; a = size
	; hl = standard basis state, LSB's corresponding; rest of left HL filled with 0's
	
	ld d,h
	ld e,l
	ld c,a ; save params
	
	; draw |
	ld a,'|'
	bcall(_PutC)
	
	ld hl,PutKet_resume
	push hl
	ld hl,evalFlags
	bit ketMode,(hl)
	jr nz,PutKet_ints
	jr PutKet_bits
	
PutKet_resume:
	ld a,'>'
	bcall(_PutC)
	ret

PutKet_bits:
	ld b,8
	ld a,c
	cp b
	jr nc,PutKet_bits_loop
	ld b,a ; b = min(a,8)
PutKet_bits_loop:
	srl e
	jr c,PutKet_bits_loop_1
	ld a,'0'
	jr PutKet_bits_loop_cont
PutKet_bits_loop_1:
	ld a,'1'
PutKet_bits_loop_cont:
	bcall(_PutC)
	djnz PutKet_bits_loop
	ld a,c
	cp 9
	ret c	; return if a < 9
	sub 8
	ld b,a
	ld c,0
	jr PutKet_bits_loop
	
PutKet_ints:
	ld h,d
	ld l,e
	call MyDispHL
	ret
	
#include "evaluator.asm"
#include "lexer.asm"
#include "parser.asm"
#include "util.asm"
#include "Error.asm"