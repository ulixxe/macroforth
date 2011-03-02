\
\ Minimum ARM Cross-Forth compiler -- primitives
\
\ Copyright (C) David Kühling 2007-2009
\
\ This program is free software: you can redistribute it and/or modify
\ it under the terms of the GNU General Public License as published by
\ the Free Software Foundation, either version 3 of the License, or
\ (at your option) any later version.
\
\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\ GNU General Public License for more details.
\
\ As a special exception, you may use this file as part of a free software
\ library without restriction.  Specifically, if other files instantiate
\ templates or use macros or inline functions from this file, or you compile
\ this file and link it with other files to produce an executable, this
\ file does not by itself cause the resulting executable to be covered by
\ the GNU General Public License.  This exception does not however
\ invalidate any other reasons why the executable file might be covered by
\ the GNU General Public License.

target cdata  DECIMAL

target code noop	\ used as a fast (idata) 'next' to branch to
   next,
end-code   ' noop xt>cfa EQU 'next	

target label dovar
   FSP -4 #]!	TOS	STR,
   LR 		TOS	MOV,
   next,
end-label  dovar host TO dovar

target label docon
   FSP -4 #]!	TOS	STR,
   LR 0 #]	TOS	LDR,
   next,
end-label  docon host TO docon

target label dovalue
   FSP -4 #]!	TOS	STR,
   LR 0 #]	TOS	LDR,	\ one more indirection than docon
   TOS 0 #]	TOS	LDR,	\ (since data-field in different section)
   next,
end-label  dovalue host TO dovalue

target label douser
   FSP -4 #]!	TOS	STR,
   LR 0 #]	TOS	LDRH,
   FTP  TOS  	TOS	ADD,	\ FTP = "Forth Task Pointer"
   next,
end-label  douser host TO douser

target label docol
   FRP -4 #]!	FIP	STR,
   LR		FIP	MOV,
   next,
end-label  docol host TO docol

target label dodoes
   FSP -4 #]!		TOS	STR,		\ push data-field address
   FBP  FWP 2 #LSL	R0	ADD,		\ (stored as address after
   R0 4 #]		TOS	LDR,		\ code-field of CREATEd word)
   docol			B,		\ now call via docol
end-label  dodoes host TO dodoes

target code execute
   TOS			FWP	MOV,
   FSP 4 ]#		TOS	LDR,
   FBP	FWP 2 #LSL	PC	ADD,
end-label

\ todo: defer can be optimized by just putting a branch opcode into the
\ code-field, and another branch into the variable section
target label dodefer
   LR 0 #]		R0	LDR,		\ double-indirection
   R0 0 #]		FWP	LDRH,		\ since xt in other section
   FBP	FWP 2 #LSL	PC	ADD,
end-label  dodefer host TO dodefer

target code exit
   FRP 4 ]#	FIP	LDR,
   next,
end-code  ' exit   host TO 'exit

\ target code !sp
\    'stack-bottom #	FSP	MOV,
\    'next			B,
\ end-code

\ if 'stack-bottom cannot be encoded in a MOV, command, replace with
\ following code:
target code !sp
   PC 0 ]#		FSP	LDR,
   'next			B,
   'stack-bottom 		,
end-code


target code sp!
   TOS			FSP	MOV,
   'next			B,
end-code

target code sp@
   FSP -4 #]!		TOS	STR,
   FSP			TOS	MOV,	
   'next			B,
end-code

target code rp!
   TOS			FRP	MOV,
   FSP 4 ]#		TOS	LDR,
   'next			B,
end-code

target code tp!	\ "task pointer"
   TOS			FTP	MOV,
   FSP 4 ]#		TOS	LDR,
   'next			B,
end-code

target code hlit
   FSP -4 #]!	TOS	STR,
   FIP 2 ]#	TOS	LDRSH,
   next,
end-code  ' hlit  host TO 'hlit

target code lit
   FSP -4 #]!		TOS	STR,
   FIP 2 ]#		TOS	LDRH,
   FIP 2 ]#		R0	LDRH,
   TOS  R0 16 #LSL	TOS	ORR,
   'next			B,
end-code  ' lit  host TO 'lit

target code branch
   FIP  2 ]#		R0	LDRSH,
   FIP  R0		FIP	ADD,
   next,
end-code  ' branch host TO 'branch

target code ?branch
   FIP 2 ]#		R0	LDRSH,
   TOS  0 #			TEQ,
   FIP	R0		FIP	EQ ADD,
   FSP 4 ]#		TOS	LDR,
   next,
end-code  ' ?branch host TO '?branch

target code (do)
   FRP DB!	{ FLC FLB }	STM,
   TOS			FLC	MOV,
   FSP 4 ]#		FLB	LDR,	\ can we use LDM here?
   FSP 4 ]#		TOS	LDR,
   'next			B,
end-code  ' (do) host TO '(do)

target code (?do)
   TOS			R2	MOV,
   FSP IA!	{  R0 TOS }	LDM,
   R2  R0			TEQ,
   ' branch xt>cfa		EQ B,
   FRP DB!	{ FLC FLB }	STM,
   R2			FLC	MOV,
   R0			FLB	MOV,
   FIP  2 #		FIP	ADD,
   'next			B,
end-code  ' (?do) host TO '(?do)

target code (loop)
   FLC	1 #		FLC	ADD,
   FLB	FLC			TEQ,
   FIP  2 ]#		R0	LDRSH,		\ similar to conditional branch
   FIP  R0		FIP	NE ADD,
   FIP 2 ]#		FWP	NE LDRH,	\ a conditional next is inlined
   FBP	FWP 2 #LSL	PC	NE ADD,		\ quick in case loop continues

   FRP IA!	{ FLC FLB }	LDM,
   'next			B,
end-code  ' (loop) host TO '(loop)

target code unloop
   FRP IA!	{ FLC FLB }	LDM,
   next,
end-code

target code i
   FSP -4 #]!	TOS	STR,
   FLC		TOS	MOV,
   next,
end-code   

target code drop
   FSP 4 ]#	TOS	LDR,
   next,
end-code

target code nip
   FSP 4 #  	FSP	ADD,
   next,
end-code

target code 2drop
   FSP IA!  	{ R0 TOS }	LDM,
   'next			B,
end-code

target code dup
   FSP -4 #]!	TOS	STR,
   next,
end-code

target code 2dup	( x1 tos -- x1 tos x1 tos )
   FSP 0 #]	R0		LDR,
   FSP DB!	{ R0 TOS }	STM,
   'next			B,
end-code

target code over
   FSP -4 #]!	TOS	STR,
   FSP 4 #]	TOS	LDR,
   next,
end-code

target code swap
   FSP 0 #]  TOS	TOS	SWP, 
   next,
end-code

target code rot   ( S: x2 x1 tos -- x1 tos x2 )
   FSP IA!	{ R2 R3 }	LDM,	\ R2 = x1,  R3 = x2
   FSP DB!	{ TOS R2 }	STM,	
   R3		TOS		MOV,
   next,
end-code

target code -rot   ( S: x2 x1 tos -- tos x2 x1 )
   TOS		R3		MOV,
   FSP IA!	{ TOS R2 }	LDM,	
   FSP DB!	{ R2 R3 }	STM,	
   'next			B,
end-code

target code >R
   FRP -4 #]!		TOS	STR,
   FSP 4 ]#		TOS	LDR,
   next,
end-code

target code 2>R  ( S: x2 x1 tos1 -- tos2  R: -- x1 tos1 )
   FSP IA!	{ R2 R3 }	LDM,	\ R2 = x1,  R3 = x2
   FRP DB!	{ TOS R2 }	STM,
   R3		TOS		MOV,
   'next			B,
end-code

target code R>
   FSP -4 #]!		TOS	STR,
   FRP 4 ]#		TOS	LDR,
   next,
end-code

target code 2R>  ( S: tos1 -- tos1 x2 x1  R: x2 x1 -- )
   TOS		R3		MOV,
   FRP IA!	{ TOS R2 }	LDM,	
   FSP DB!	{ R2 R3 }	STM,
   'next			B,
end-code

target code R@
   FSP -4 #]!		TOS	STR,
   FRP 0 #]		TOS	LDR,
   next,
end-code

target code @
   TOS 0 #]	TOS	LDR,
   next,
end-code

target code c@
   TOS 0 #]	TOS	LDRB,
   next,
end-code

target code h@
   TOS 0 #]	TOS	LDRH,
   next,
end-code

target code sh@
   TOS 0 #]	TOS	LDRSH,
   next,
end-code

target code !
   FSP IA!	{  R0 R2 }	LDM,
   TOS 0 #]	R0		STR,
   R2		TOS		MOV,
   next,
end-code

target code c!
   FSP IA!	{  R0 R2 }	LDM,
   TOS 0 #]	R0		STRB,
   R2		TOS		MOV,
   next,
end-code

target code h!
   FSP IA!	{  R0 R2 }	LDM,
   TOS 0 #]	R0		STRH,
   R2		TOS		MOV,
   next,
end-code

cdata

target code and
   FSP 4 ]#	R0	LDR,
   R0	TOS	TOS	AND,
   next,
end-code

target code or
   FSP 4 ]#	R0	LDR,
   R0	TOS	TOS	ORR,
   next,
end-code

target code xor
   FSP 4 ]#	R0	LDR,
   R0	TOS	TOS	EOR,
   next,
end-code

target code andc
   FSP 4 ]#	R0	LDR,
   R0	TOS	TOS	BIC,
   next,
end-code

target code invert
   TOS		TOS	MVN,
   next,
end-code

target code lshift
   FSP 4 ]#	R0	LDR,
   R0 TOS LSL	TOS	MOV,
   next,
end-code

target code rshift
   FSP 4 ]#	R0	LDR,
   R0 TOS LSR	TOS	MOV,
   next,
end-code

target code 2*
   TOS 1 #LSL	TOS	MOV,
   next,
end-code

target code cells
   TOS 2 #LSL	TOS	MOV,
   next,
end-code

target code 2/
   TOS 1 #ASR	TOS	MOV,
   next,
end-code

target code +
   FSP 4 ]#	R0	LDR,
   R0	TOS	TOS	ADD,
   next,
end-code

target code +!
   FSP IA!	{  R0 R2 }	LDM,
   TOS 0 #]	R3		LDR,
   R3  R0	R3		ADD,
   TOS 0 #]	R3		STR,
   R2		TOS		MOV,
   next,
end-code

target code 1+
   TOS 1 #	TOS	ADD,
   next,
end-code

target code cell+
   TOS 4 #	TOS	ADD,
   next,
end-code

target code -
   FSP 4 ]#	R0	LDR,
   R0	TOS	TOS	SUB,
   next,
end-code

cdata

target code 1-
   TOS 1 #	TOS	SUB,
   next,
end-code

target code negate
   TOS  0 #	TOS	RSB,
   next,
end-code

target code abs
   TOS	0 #		CMP,
   TOS  0 #	TOS	LT RSB,
   next,
end-code

target code min
   FSP 4 ]#	R0	LDR,
   R0  TOS		CMP,
   R0  TOS		LT MOV,
   next,
end-code

target code max
   FSP 4 ]#	R0	LDR,
   R0  TOS		CMP,
   R0  TOS		GT MOV,
   next,
end-code

target code <>
   FSP 4 ]#	R0	LDR,
   R0  TOS	TOS	EORS,
   0 #		TOS	NE MVN,
   next,
end-code

target code =
   FSP 4 ]#	R0	LDR,
   R0  TOS	TOS	EORS,
   0 #		TOS	NE MVN,
   TOS		TOS	MVN,
   next,
end-code

target code >
   FSP 4 ]#	R0	LDR,
   TOS  R0	TOS	SUB,
   TOS 31 #ASR	TOS	MOV,   
   next,
end-code

target code <
   FSP 4 ]#	R0	LDR,
   R0  TOS	TOS	SUB,
   TOS 31 #ASR	TOS	MOV,   
   next,
end-code

target code >=
   FSP 4 ]#	R0	LDR,
   R0  TOS	TOS	SUB,
   TOS 31 #ASR	TOS	MVN,   
   next,
end-code
   
target code <=
   FSP 4 ]#	R0	LDR,
   TOS  R0	TOS	SUB,
   TOS 31 #ASR	TOS	MVN,   
   next,
end-code
   
target code 0<>
   TOS  0 #		TEQ,
   0 #		TOS	NE MVN,
   next,
end-code

target code 0=
   TOS  0 #		TEQ,
   0 #		TOS	NE MVN,
   TOS		TOS	MVN,
   next,
end-code

target code 0<
   TOS 31 #ASR	TOS	MOV,   
   next,
end-code

target code 0>
   TOS  0 #	TOS	RSB,	\ warning: this fails for -80000000h
   TOS 31 #ASR	TOS	MOV,   
   next,
end-code
   
target code u<
   FSP 4 ]#	R0	LDR,
   R0  TOS		CMP,
   R0  R0	TOS	SBC,   
   next,
end-code

target code u>
   FSP 4 ]#	R0	LDR,
   TOS  R0		CMP,
   R0	R0	TOS	SBC,   
   next,
end-code

target code *
   FSP 4 ]#	R0	LDR,
   R0	TOS	TOS	MUL,  \  E0 00 11 90
   'next		B,
end-code

target code M*
   FSP 0 #]	R0	LDR,
   R0	TOS	R0 TOS	SMULL,
   FSP 0 #]	R0	STR,
   'next		B,
end-code

target code fill  ( c-addr1 u1 tos -- )
   FSP IA!	{ R0 R2 R3 }	LDM,	\ R0=u1 R2=c-addr1 R3=new-tos
   BEGIN,
      R0  1 #		R0	SUBS,		\ count down, end reached?
      R2 1 ]#		TOS	HS STRB,	\ if not end: write byte..
   HS UNTIL-NOT,				\ and continue
   R3			TOS	MOV,
   'next			B,
end-code

target code move  ( src dst tos-u -- )
   FSP IA!	{ R0 R2 R3 }	LDM,	\ R0=dst R2=src R3=new-tos
   BEGIN,
      TOS  1 #		TOS	SUBS,	\ count down. end reached?
      R2 1 ]#		R8	HS LDRB,	\ if not: copy one byte..
      R0 1 ]#		R8	HS STRB,
   HS UNTIL-NOT,				\ .. and continue loop
   R3			TOS	MOV,
   'next			B,
end-code

target code compare  ( c-addr1 u1 c-addr2 tos-u2 -- tos-flag )
   FSP IA!	{ R0 R2 R3 }	LDM,	\ R0=c-addr2 R2=u1 R3=c-addr1
   BEGIN,
      TOS  1 #		TOS	SUBS,	  \ count down...
      R2  1 #			HS CMP,  \ and check if one string ended
      R2  1 #		R2	SUB,	
      R2  TOS		R8	LO SUB,	\ if one ended: length difference
      LO WHILE-NOT,
	 R0 1 ]#	R8	LDRB,	\ load one byte from each string
	 R3 1 ]#	R9	LDRB,
	 R9  R8		R8	SUBS,	\ compare two bytes
   EQ REPEAT-UNTIL-NOT,			\ if bytes are equal continue loop
   R8 31 #ASR		TOS	MOV,	\ turn difference in R8 to TOS=+1|0|1
   R8  0 #			CMP,
   1 #			TOS	GT MOV,
   'next		B,
end-code

target code u/mod  ( div tos_d -- rest tos_q )
   FSP 4 ]#	R0	LDR,		\ R0 = div
   0 #		R2	MOV,	\ R2 counts number of shifts below
   BEGIN,			\ first loop: shift divisor left
      TOS		R3	MOV,	
      R2  1 #		R2	ADD,
      TOS 1 #LSL	TOS	MOVS,		\ if divisor > 2^32: Carry
      1 #		TOS	EQ MOVS,	\ protection agains div by zero
      TOS  R0			CC CMP,		\ if divisor > divident : Carry
   LS UNTIL-NOT,		\ until divisor >= divident or divisor > 2^32

   0 #		TOS	MOV,	\ TOS accumulates the quotient
   BEGIN,			\ second loop: compare and substract divisor
      TOS 1 #LSL	TOS	MOV,		\ room for one more result bit
      R0  R3			CMP,		\ compare with divisor
      R0  R3		R0	HS SUB,		\ and substract from remainder
      TOS  1 #		TOS	HS ORR,		\ and write one result-bit
      R3 1 #LSR		R3	MOV,		\ adjust divisor for next cycle
      R2  1 #		R2	SUBS,		
   NE UNTIL-NOT,		\ loop until R2 reaches zero

   FSP -4 #]!		R0	STR,	\ store remainder on stack

   'next			B,
end-code

\ idata initialization code cannot be coded in high-level Forth, since some of
\ the primitives required might reside in IDATA for performance reasons
target label doimage
   LR 4 ]#	R0	LDR,	\ R0 = section start address
   LR 4 ]#	R2	LDR,	\ R2 = first address after end of section
   BEGIN,
      R0	R2		CMP,	\ end-address reached?
      LR	4 ]#	R3	NE LDR,	\ copy one word of data ...
      R0	4 ]#	R3	NE STR,
   NE UNTIL-NOT,		\ ...and continue if end not yet reached
   next,			\ (cannot branch to idata 'next !)
end-label   doimage host TO doimage 

0 [IF]
   Local Variables:
   forth-local-words:
   ((("code" "label")
   non-immediate (font-lock-keyword-face . 2)
   "[ \t\n]" t name (font-lock-function-name-face . 3))
   (("equ")
   non-immediate (font-lock-keyword-face . 2)
   "[ \t\n]" t name (font-lock-variable-name-face . 3))
   )
   forth-local-indent-words:
   ((("label" "code") (0 . 2) (0 . 2))
   (("end-label" "end-code") (-2 . 0) (0 . -2))
   (("begin,") (0 . 2) (0 . 2))
   (("while-not,") (0 . 2) (0 . 2))
   (("until-not," "again,") (-2 . 0) (-2 . -0))
   (("repeat," "repeat-until-not,") (-4 . 0) (-4 . -0)))
   End:
[THEN]
