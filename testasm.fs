

: there  here ;
: t@  @ ;
: t!  ! ;
: t,  , ;

S" ./asm.fs" included

]ASM
4000 #		r7	mov,
r1 ]	r2	r3	swp,
r1 27 #]	r3	strh,
r1 27 #]	r3	ldrh,
r1 r2 +]	r3	ldrh,
r1 r2 2 #lsl +]	r3	ldr,
r1	r2	r3	ne add,
r4 da!	{ R1 R2 R3 }	ldm,
r5 ib	{ R5 R6 R7 }	stm,


r1	r2	r3	mul,
r1	r2	r8 r9	smull,
r1	r2 r3	r5	mla,
r1	r2	r8 r9	smlal,

r1	r2		teq,
NE IF-NOT,
r2	5 #	r2	add,
THEN,

AHEAD,
LABEL constant1
0abcdef ,
THEN,

constant1 [#]	r5	ldr,
80 #		r7	mov,
r5 ]		r6	str,
