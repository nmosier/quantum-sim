;; macros
;; quantum_sim.wcodeproj
;;
;; Created by Nicholas Mosier on 05/27/2018

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

#macro neg_hl
	
#endmacro