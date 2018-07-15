;; lexer.inc
;; quantum_sim.wcodeproj
;;
;; Created by Nicholas Mosier on 06/10/2018

;; lexeme types:
lexeme_EOF 		equ 0
lexeme_binop	equ 1

#comment
lexeme_add		equ 1
lexeme_sub		equ 2
lexeme_mult		equ 3
lexeme_div		equ 4
#endcomment

lexeme_lparen 	equ 2
lexeme_rparen	equ 3
lexeme_unop		equ 4
lexeme_number 	equ 5
lexeme_func		equ 6
lexeme_unpostop	equ 7
lexeme_ket		equ 8
lexeme_bra		equ 9

lexeme_error	equ $FE
lexeme_none		equ $FF