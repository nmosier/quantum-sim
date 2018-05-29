;; tables
;; quantum_sim.wcodeproj
;;
;; Created by Nicholas Mosier on 05/22/2018

; uses GetKey codes, not GetCSC



input_JT:
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
	.dw eval_handle_char	;$B7
	fill_word($B9-$B7-1, eval_handle_none)
	.dw eval_handle_char	;$B9
	fill_word($BB-$B9-1, eval_handle_none)
	.dw eval_handle_char	;$BB
	fill_word($BD-$BB-1, eval_handle_none)
	.dw eval_handle_char	;$BD
	.dw eval_handle_none	;$BF
	fill_word($C1-$BF-1, eval_handle_none)
	.dw eval_handle_none	;$C1
	fill_word($FB-$C1, eval_handle_none)

;-------TOKEN EQUATES-----;
qtEOF		equ $00
qtPlus 		equ $01
qtMinus		equ $02
qtTimes		equ $03
qtDiv 		equ $04
qtLeftP		equ $05
qtRightP	equ $06
qtNeg		equ $07
qtPeriod	equ $08
qt0			equ $09
qt1			equ $0A
qt2			equ $0B
qt3			equ $0C
qt4			equ $0D
qt5			equ $0E
qt6			equ $0F
qt7			equ $10
qt8			equ $11
qt9			equ $12
qtSin		equ $13
qtCos		equ $14
qtTan		equ $15
qtSquare	equ $16
qtUnk 		equ $FF
;--------------------------;

k2tok:
	;; input: a = input GetKey scan code
	;; output: a = qtoken
	ld hl,k2tok_LUT
	ld d,0
	ld e,a
	add hl,de
	ld a,(hl)
	ret

tok2tiTok:
	;; input: a = qtoken
	;; output: de = TI token
	;; destroys: a, hl
	ld hl,tok2tiTok_LUT
	ld d,0
	ld e,a
	add hl,de
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	ret

k2tok_LUT:
	.fill $80,qtUnk	;$00-$79
	.db qtPlus		;$80
	.db qtMinus		;$81
	.db qtTimes		;$82
	.db qtDiv		;$83
	.db qtUnk		;$84
	.db qtLeftP		;$85
	.db qtRightP	;$86
	.fill $8C-$86-1,qtUnk
	.db qtNeg		;$8C
	.db qtPeriod	;$8D
	.db qt0			;$8E
	.db qt1			;$8F
	.db qt2			;$90
	.db qt3			;$91
	.db qt4			;$92
	.db qt5			;$93
	.db qt6			;$94
	.db qt7			;$95
	.db qt8			;$96
	.db qt9			;$97
	.fill $B7-$97-1,qtUnk
	.db qtSin		;$B7
	.db qtUnk		;$B8
	.db qtCos		;$B9
	.db qtUnk		;$BA
	.db qtTan		;$BB
	.db qtUnk		;$BC
	.db qtSquare	;$BD
	.fill $FB-$BD,qtUnk
	
tok2tiTok_LUT:
	.dw 0		;tqEOF			equ 00h
	.dw tAdd	;tqPlus 		equ 01h
	.dw tSub	;tqMinus		equ 02h
	.dw tMul	;tqTimes		equ 03h
	.dw tDiv	;tqDiv 		equ 04h
	.dw tLParen	;tqLeftP		equ 05h
	.dw tRParen	;tqRightP	equ 06h
	.dw tChs	;tqNeg		equ 07h
	.dw tDecPt	;tqPeriod	equ 08h
	.dw t0		;tq0			equ 09h
	.dw t1		;tq1			equ 0Ah
	.dw t2		;tq2			equ 0Bh
	.dw t3		;tq3			equ 0Ch
	.dw t4		;tq4			equ 0Dh
	.dw t5		;tq5			equ 0Fh
	.dw t6		;tq6			equ 10h
	.dw t7		;tq7			equ 11h
	.dw t8		;tq8			equ 12h
	.dw t9		;tq9			equ 13h
	.dw tSin	;qtSin
	.dw tCos	;qtCos
	.dw tTan	;qtTan
	.dw tSqr	;qtSquare
	;qtUnk		equ FFh