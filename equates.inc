;; evaluator
evalFlags .equ appData
ketMode .equ 0 ; if SET, then disp state as ints; RESET, disp as bits

inputBuffer_curP .equ appData+1
inputBuffer_endP .equ appData+3
inputBuffer_offsetE .equ appData+5	; relative from end, i.e. pos = endP - curP
scrap .equ appData+7
scrapEnd .equ appData+13

inputBuffer .equ saveSScreen
inputBuffer_end .equ saveSScreen+767	; reserve 767th byte for null-terminator
inputBuffer_size .equ 767


;; lexer
lexBuffer equ appBackUpScreen
lexBuffer_curP equ appData+13
inputBuffer_lexP equ appData+15
lexBuffer_end equ appBackUpScreen+768

;; parser
parser_astRootP equ appData+17
parser_curP equ appData+19
parser_lexP equ appData+21
parser_astLen equ appData+23
parser_astVarP equ appData+25