
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


	;; input:		  hl = ptr to beginning of tok array
	;; 				  bc = # of toks to display
	;; output:        (none)
	;; flags:		  carry SET if everything fit & cursor in valid position; RESET otherwise
	;; destroys:	  a,de
	;; end condition: hl = ptr to (one past) final tok displayed
	;;				  bc = # of tokens not displayed
display_toks:
	ld a,b
	or c
	scf	;; just in case function returns
	ret z
display_toks_loop:
	push hl
	ld a,(hl)
	push de
	call tok2str
	call PutStr
	pop de
	pop hl
	inc hl
	dec bc
	ret nc	; if string didn't fully display OR cursor in invalid position, stop
	ld a,b
	or c
	jr nz,display_toks_loop
	scf
	ret

;---------
;; display_full: displays current contents of input buffer
;; inputs: 
display_full:
	call display_before
	call display_after
	ret

;; display_before: displays contents of input buffer BEFORE cursor position
; inputs: (curRow, curCol)
; destroys: ALL
;; flags: see display_toks
display_before:
	ld hl,(curRow)
	ld de,0
	ld (curRow),de
	;push hl
	call get_cursor_offset
	push af			;; preserve offset for later
	ld hl,-inputBuffer
	ld de,(inputBuffer_curP)
	add hl,de
	ex de,hl
	call scantoklen_rev	; upon return, b=length scanned, c=tokens scanned
	pop af
	sub b	; target - actual length, if a<b, then need to disp (end of) prev token
	jr c,_
	jr z,_
	jr display_error	;; i.e. b < a, didn't find enough tokens
_	push af
	push hl
	push bc
	push de
	bcall(_ClrScrnFull)
	pop de
	pop bc
	pop hl
	pop af
	jr z,display_before_fulltokens
	;; display previous token partially on screen
	ld b,a
	push hl
	ld a,(hl)	; a = partially displayed token
	call tok2str
	ld a,b	
	neg		; a = # of chars to exclude from token
	ld d,0
	ld e,a
	ld a,(hl)	; a = length of token
	add a,b		; a = # of tokens to display
	add hl,de
	call PutStrA
	
	pop hl
	inc hl	; move to next token
	dec c	; decrease # of tokens to display	
display_before_fulltokens:
	;; current state:
	;; - hl = next token to display
	;; - c = number of tokens to display
	;; - curRow, curCol in proper position
	ld b,0
	jp display_toks


;; display_after: display tokens after current cursor position
;; inputs: (curRow, curCol)
;; outputs: hl = last cursor position (one past last displayed char)
;; flags: see display_toks
;; destroys: ALL
;; end condition: preserves (curRow, curCol)
display_after:
	ld hl,(curRow)
	push hl
	ld hl,(inputBuffer_curP)
	ld bc,(inputBuffer_offsetE)
	call display_toks
	pop hl
	ld de,(curRow)
	ld (curRow),hl
	ex de,hl
	ret

;; display_after_withspaces: same as display_after but then appends given number of spaces
;;  (used for quick display when token overwritten or deleted)
;; inputs: a = # of spaces to write at end
;;		   (curRow, curCol) specify where to start writing
;; outputs: (none)
;; flags: carry flag SET if everything displayed with room to spare, RESET otherwise
;; destroys: ALL
;; preserves: (curRow, curCol)
display_after_withspaces:
	ld b,a
	push bc
	call display_after
	pop bc
	ret nc	; display full
	ld a,b
	or a
	ret z	;; also carry reset upon return
	ld de,(curRow)
	push de
	ld (curRow),hl
	ld hl,curRow	;; for quick access in loop to check if onscreen
display_after_withspaces_loop:
	ld a,' '
	bcall(_PutC)
	ld a,8
	cp (hl)
	jr z,display_after_withspaces_end	;; carry flag reset
	djnz display_after_withspaces_loop
	ld hl,(curRow)
	scf	;; displayed all
display_after_withspaces_end:
	pop de
	ld (curRow),de
	ret
	

display_error:
	ld hl,display_error_string
	ld de,appErr1
	ld bc,7	; length of string
	ldir
	bjump(_ErrCustom1)

display_error_string: .db "DISPLAY",0
	

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
	;; flags: carry flag set if entire string displayed & cursor not in invalid position
	
	;;PutStrA
	;; void PutStr(char*, byte n)
	;; input: hl = ptr to one char BEFORE string to display
	;; 		  a = length of string
	;; flags: carry flag set if entire string displayed & cursor not in invalid position
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