
;; displays HL WITHOUT padding
;; INPUT: hl
;; OUTPUT: (none)
;; DESTROYS: af,b,hl,de
MyDispHL:	
	ld de,scrap
	ld b,5
	xor a
MyDispHL_zero:
	ld (de),a
	inc de
	djnz MyDispHL_zero
	;; now main loop
	ld b,0 ;; count # of digits
MyDispHL_loop:
	bcall(_DivHLBy10)
	add a,'0'
	dec de
	ld (de),a
	inc b
	;; cond
	ld a,h
	or l
	jr nz,MyDispHL_loop
MyDispHL_print:
	ld a,(de)
	bcall(_PutC)
	inc de
	djnz MyDispHL_print
	ret
	
min_hl_de:
	; returns min{hl,de}
	; return val in hl
	ld a,h
	cp d
	ret c	; h<d, so hl is min
	jr nz,_
	ld a,e
	cp l
	ret c	; hl < de if h=d & l<=e
_	ld h,d
	ld l,e
	ret
	
cursor_adjust:
	;; input: hl = (curCol, curRow)
	;; output: hl such that 0 ≤ curCol < 16
	;; destroys: a
	ld a,h
	sra a
	sra a
	sra a
	sra a	;; (h & 0xF0) >> 4
	add a,l
	ld l,a
	ld a,$0F
	and h
	ld h,a
	ret

display_toks:
	;; input:	hl = ptr to beginning of tok array
	;; 			bc = # of toks to display
	;; destroys:
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
	jr nz,display_toks
	ret