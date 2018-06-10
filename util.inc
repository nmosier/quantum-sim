
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
	ld a,b
	or c
	ret z
display_toks_loop:
	;; input:	hl = ptr to beginning of tok array
	;; 			bc = # of toks to display
	;; destroys:
	push hl
;	push bc
	ld a,(hl)
	;call tok2titok
	;bcall(_PutTokString)
	push de
	call tok2str
	call PutStr
	pop de
;	pop bc
	pop hl
	inc hl
	dec bc
	ret nc	; if string didn't fully display, stop
	ld a,b
	or c
	jr nz,display_toks_loop
	ret
	

;; scantoklen_fwd
; inputs:	hl = pointer to begininning of token array
;			de = max # of tokens to scan
;			a = target length (finds legnth ≥ a)
; outputs:	b = length of tokens summed
;			c = # of tokens summed
; destroys: scrap[0]
scantoklen_fwd:
	ld (scrap),a
	ld bc,0
	or a
	ret z
	jr _
scantoklen_fwd_loop:
	ld a,(hl)
	push hl
	push de
	;call tok2titok
	;bcall(_GetTokLen)
	call tok2len
	;
	pop de
	add a,b
	ld b,a
	dec de
	inc c
	ld hl,scrap
	cp (hl)	; compare to target length
	pop hl	; doesn't affect
	inc hl	; flags, so good
	ret nc
_
	ld a,d
	or e
	jr nz,scantoklen_fwd_loop
	ret
	
;; scantoklen_rev
; inputs:	hl = pointer to one past end of array (like it.end())
;			de = max # of tokens to scan
;			a = target length
; outputs:	b = actual length summed
;			c = actual # tokens summed
; destroys:	scrap[0]
scantoklen_rev:
	ld (scrap),a	; save target
	ld bc,0			; reset length & tok count
	or a
	ret z
	jr _	
scantoklen_rev_loop:
	dec hl
	ld a,(hl)
	push hl
	push de
	call tok2len
	pop de
	add a,b
	ld b,a
	dec de
	inc c	;; inc # of tokens
	ld hl,scrap
	cp (hl)
	pop hl
	ret nc	;; found length
_	ld a,d
	or e
	jr nz,scantoklen_rev_loop	;; any more tokens to sum?
	ret

;; get_cursor_offset
;; inputs:	h = col
;;			l = row
;; outputs: a = offset
get_cursor_offset:
	sla l
	sla l
	sla l
	sla l
	ld a,h
	add a,l
	ret

	
	;;PutStr
	;; void PutStr(char*)
	;; input: hl = ptr to string to display
	;;    string is prefixed by length (not zero-terminated!)
	;; flags: carry flag set if entire string displayed
	
	;;PutStrA
	;; void PutStr(char*, byte n)
	;; input: hl = ptr to one char BEFORE string to display
	;; 		  a = length of string
	;; flags: carry flag set if entire string displayed
PutStr:
	ld a,(hl)
PutStrA:
	or a
	scf	
	ret z
	ld b,a
	ld de,curRow
PutStr_loop:
	inc hl
	ld a,(hl)
	bcall(_PutC)
	ld a,(de)
	cp 8
	ret z	;; carry flag will correctly be reset upon return
	djnz PutStr_loop
	ret