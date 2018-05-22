
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