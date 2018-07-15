;; Error
;; quantum_sim.wcodeproj
;;
;; Created by Nicholas Mosier on 06/10/2018

_DispErrorScreen equ 49DEh
_ErrCustom1 equ 4D41h

;; handle errors
; possible errors: Syntax,
; input: a = error no
; output: a = option chosen
; destroys: ALL 
error_handler:
	ld (errNo),a
	bcall(_DispErrorScreen)
	ld hl,$0001
	ld (curRow),hl
	call error_handler_disp_quit
	ld hl,$0002
	ld (curRow),hl
	call error_handler_disp_goto
	ld b,1	;; b stores the current selected option
error_handler_loop:
	bcall(_GetKey)
	cp k1
	jr z,_
	cp k2
	jr z,_
	jr error_handler_loop
_	ret
	
;; error_handler_disp_quit
;; preconditions: curRow,curCol set to newline to display on
;; inputs: (none)
;; outputs: (none)
;; destroys: 
error_handler_disp_quit:
	set textInverse,(iy+textFlags)
	ld a,'1'
	bcall(_PutC)
	ld a,':'
	bcall(_PutC)
	res textInverse,(iy+textFlags)
	ld hl,strerror_quit
	call PutStr
	ret

error_handler_disp_goto:
	set textInverse,(iy+textFlags)
	ld a,'2'
	bcall(_PutC)
	ld a,':'
	bcall(_PutC)
	res textInverse,(iy+textFlags)
	ld hl,strerror_goto
	call PutStr
	ret
	

strerror_quit:
	.db 4,"Quit"
strerror_goto:
	.db 4,"Goto"