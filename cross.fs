\
\ Minimum ARM Cross-Forth compiler -- cross compiler core
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

\ host helper routines
: array  ( n -- ) CREATE CELLS ALLOT  DOES>  SWAP CELLS + ;
: -roll  ( x_0 .. x_n n -- x_n x_0 .. x_n-1 )
   DUP 0 ?DO  DUP >R ROLL  R> LOOP  DROP ; 
: bounds  ( c-addr u1 -- c-addr2 c-addr3 )  OVER + SWAP ;
: ?restore-input  RESTORE-INPUT ABORT" restore-input failed" ;
: parse-number  ( c-addr u -- d +1 | d -1 | d 0 )
   DUP 0= IF  2DROP 0 EXIT THEN
   OVER C@ [CHAR] - = DUP >R 1 AND /STRING
   0 0 2SWAP >NUMBER
   DUP 1 = IF		
      DROP C@ [CHAR] . = 1 and 
   ELSE  NIP 0=  THEN	
   R> IF >R DNEGATE R> THEN ;

\ scopes
VOCABULARY targeting
VOCABULARY interpreting
VOCABULARY compiling
VOCABULARY postponing
VOCABULARY ghosts

0 VALUE scope

\ can this be rewritten in a nicer way?  replice conditional scope updating
\ with custom definition for 'host'?
: scope:  ( xt needs-update? "name" -- )
   CREATE  SWAP , ,  DOES>  DUP TO scope  @ EXECUTE ;
: ?update-scope  ( -- )  \ update scope order if needs update at state-switch
   \ we won't update in host scope, as host scope needs to be able to handle
   \ wordlists itself
   scope CELL+ @ IF  scope @ EXECUTE THEN ;
:NONAME  ( -- )  \ set host scope order
   ONLY FORTH  DEFINITIONS ;		FALSE scope: host
:NONAME  ( -- )  \ target scope order
   \ note: wordlists during compilation handled by 'target-compiler'
   ONLY targeting DEFINITIONS
   ALSO interpreting ;	TRUE scope: target
:NONAME  ( -- )  \ set (update) interpreter scope order
   STATE @ IF
      ONLY FORTH  ALSO interpreting DEFINITIONS
   ELSE
      ONLY interpreting DEFINITIONS  ALSO FORTH
   THEN ;				TRUE scope: interpreter
:NONAME  ( -- )  \ set (update) compiler scope order
   STATE @ IF   ONLY FORTH  ALSO interpreting  ALSO postponing
   ELSE   ONLY FORTH  THEN
   ALSO compiling DEFINITIONS
   PREVIOUS ;				TRUE scope: compiler
: icreate  ( "name" -- ) \ create word in interpreting wordlist
   GET-CURRENT ALSO interpreting DEFINITIONS PREVIOUS
   CREATE  SET-CURRENT ;

\ overload state-changing words to update scope order
host
: :  :   ?update-scope ;
: ;  POSTPONE ;   ?update-scope ; IMMEDIATE
: ]  ]   ?update-scope ; 
: [  POSTPONE [   ?update-scope ; IMMEDIATE

\ Target Addresses
host
: tcells   4 * ;
: tcell+   1 tcells + ;
: tchars   ;
: tchar+   1 tchars + ;
: taligned   3 + 3 INVERT AND ;
: thaligned   1 + 1 INVERT AND ;

\ target memory access (32bit little endian)
: @32le  ( addr -- x )
   0  4 0 ?DO  8 RSHIFT   OVER I CHARS + C@  24 LSHIFT OR LOOP  NIP ;
: !32le  ( x addr -- )
   4 0 ?DO  OVER 255 AND OVER I CHARS + C!  SWAP 8 RSHIFT  SWAP LOOP   2DROP ;
: @16le  ( addr -- x )
   DUP C@   SWAP CHAR+  C@ 8 LSHIFT OR ;
: !16le  ( x addr -- )
   2DUP C!  SWAP 8 RSHIFT  SWAP CHAR+ C! ;

DEFER modified-there	\ hook for kernel to update his idata 'dp' and 'latest'

\ target sections
0 constant #cdata	1 constant #udata	2 constant #idata
3 constant #taskdata	\ non-standard, internally used for USER variables
variable section-type
: section-type!:  CREATE ,  DOES> @ section-type ! ;

interpreter
#cdata section-type!: cdata
#udata section-type!: udata
#idata section-type!: idata
#taskdata section-type!: taskdata
cdata

host
variable last-section  0 last-section !
4  ARRAY current-sections   0 current-sections  4 CELLS ERASE
: current-section  ( -- )   section-type @ current-sections ;
: section  ( taddr1 taddr2 "name" -- )
   icreate   HERE >R		
   section-type @ ,  last-section @ ,  \ store type and link  
   2DUP 2,	 		\ store start and end address
   OVER ,			\ store dp = start-addr
   SWAP -  HERE OVER CHARS ALLOT \ allocate memory for section
   SWAP ERASE			\ and initialize to 0 
   R> last-section !		\ make head of section list
  DOES>
   DUP @  current-sections ! ;
: section-trange  ( section -- taddr1 taddr2 )
   2 cells + 2@ ;
: section-haddr  ( section -- addr )
   5 cells + ;
: find-section  ( taddr -- section )
   last-section @ >R
   BEGIN  R@ WHILE
	 DUP  R@ section-trange WITHIN IF
	    DROP R> EXIT
	 THEN
	 R> cell+ @ >R	\ continue loop with next section
   REPEAT R> DROP  . ABORT" target address not within any target section" ;
: taddr>addr  ( taddr -- addr )
   DUP find-section
   DUP @ #taskdata = ABORT" invalid target address" 
   DUP section-trange DROP >R
   section-haddr  SWAP R> - CHARS + ;      
: save-sections  ( -- n*x )
   3 0 do  I current-sections @ loop  section-type @   4 ;
: restore-sections  ( n*x -- )
   4 <> ABORT" restore-sections: invalid data"
   section-type !  0 2 do  I current-sections ! -1 +loop ;

\ internal 64k pseudo-section used for allocating USER variables
interpreter
HEX  0000 10000 taskdata section (taskdata)
(taskdata)
host DECIMAL

\ section type overrides
: section-override:  ( default-type -- )
   CREATE  section-type @ ,
  DOES>  section-type @  SWAP ! ;

interpreter
udata section-override: variables
idata section-override: deferers
cdata section-override: colon-definitions
cdata section-override: headers
host

: (<section-override)  ( n -- k*x )
   @ >R  save-sections  R> section-type ! ;
: <section-override
   ALSO interpreting ' PREVIOUS
   >BODY POSTPONE LITERAL POSTPONE (<section-override) ; IMMEDIATE
: section-override>  ( k*x -- )
   restore-sections ;

\ access to section's dictionary pointer
: tdp
   current-section @  4 cells + ;
: there  tdp @ ;
: cdata-there  save-sections  [ interpreter ] cdata there [ host ]
   >r restore-sections r> ;
: ?there
   there current-section @
   section-trange WITHIN 0= ABORT" Section out of space"
   modified-there ;

\ options to be controlled by user
host
VARIABLE headless	\ flag: add dictionary headers to target code?
0 VALUE 'xt-base	\ lowest address to be addressable through 16-bit XTs

\ setters for flag-variables a la 'headless'
: on  true swap ! ;
: off  false swap ! ;

\ target dictionary operations
host
: tallot  ( x -- )  tdp +!   ?there ;
: t@  ( taddr -- x )  taddr>addr @32le ;
: t!  ( x taddr -- ) taddr>addr !32le ;
: th@  ( taddr -- x )  taddr>addr @16le ;
: th!  ( x taddr -- ) taddr>addr !16le ;
: tc@  ( taddr -- x )  taddr>addr C@ ;
: tc!  ( x taddr -- )  taddr>addr C! ;
: t,  ( x -- )  there t!  1 tcells tallot ;
: th,  ( x -- )  there th!  2 tchars tallot ;
: tc,  ( x -- ) there tc!  1 tchars tallot ;
: talign  ( x -- )  there taligned tdp ! ?there ;
: thalign  ( x -- )  there thaligned tdp ! ?there ;
: tcfa>xt  ( cfa -- xt )
   'xt-base -   2 RSHIFT
   DUP 0 65536 WITHIN 0= ABORT" out of 16bit code range" ;

\ creation of target words
host

0 VALUE latest-zombie

\ 'link' and 'latest' are the same.  Just if a word has a remote header,
\ 'link' will point to the pointer in the header whereas 'latest' points to
\ the actual code-field
VARIABLE link 0 link !
VARIABLE latest 0 latest !  \ not an xt!  good idea? bad idea

headless on

\ note: in remote-headers we use xt and cfa routines, although there is no
\ code at all.  in that case the "code field" contains only the 2-byte xt of
\ the actual code located in some other section

\ note2: ghosts are used to mirror the target dictionary, usued for compiling
\ target code.  zombies are used to emulate execution of target words, where
\ execution semantics is known (such as CONSTANTs, VARIABLEs etc.)
\ todo: can we merge ghosts with zombies?  should be possible.

: lf-align  ( -- )   \ pad so after link-field we are cell-aligned
   begin		
      there 3 and 2 <> while 0 tc,
   repeat ;
: header-there  ( -- t-addr )
   <section-override headers there >r section-override> r> ;
: header-remote?  ( -- flag )  there header-there <> ;
: tname,  ( c-addr u -- )
   DUP 1 32 within 0= ABORT"  Invalid name length" 
   DUP 128 OR   header-remote? 32 AND OR   	\ parameter&length field
   >R 2>R  <section-override headers
   2R> R> tc,					\ write parameter&length field
   bounds DO  I c@ tc, LOOP			\ write name
   section-override> ;
: tlink,  ( -- )
   <section-override headers 
   lf-align   link @ tcfa>xt th,   there link !  modified-there
   section-override> ;
: ?treference,  ( -- )
   header-remote? IF		\ remote header: points to actual code-field
      align there tcfa>xt >R
      <section-override headers  R> th,  section-override>
   THEN ;			\ (in-place header: adjacent code-field)
: theader,  ( c-addr u -- )
   tname,  tlink,  ?treference, ;
: ?theader  ( "name" -- )
   BL WORD COUNT  headless @ IF  2DROP  ELSE  theader, then ;
: ?latest-zombie  ( -- )
   latest-zombie 0= ABORT" latest word not build using target's CREATE" ;
: zombie-does>  ( "name" -- )
   POSTPONE ?latest-zombie  POSTPONE DOES>  POSTPONE @ ; IMMEDIATE
: kill-zombie
   zombie-does>
   TRUE ABORT" definition's target behaviour not executable on host" ;
: ?kill-zombie
   latest-zombie IF kill-zombie THEN ;
: create-zombie  ( "name" -- )
   icreate  HERE to latest-zombie   there ,   zombie-does> ;
: create-ghost-noname  ( -- xt )
   talign there latest !  0 t,  \ create code-field, pointed to by latest
   0 TO latest-zombie
   latest @ tcfa>xt ;
: create-ghost  ( "name" -- )
   SAVE-INPUT   ?theader  ?restore-input 
   create-ghost-noname  DROP
   GET-CURRENT ALSO targeting DEFINITIONS PREVIOUS
   CREATE latest @ tcfa>xt ,  SET-CURRENT 
  DOES> TRUE ABORT"  Attempt to execute target 'ghost' definition!" ;
: (tcreate)  ( "name" -- )
   SAVE-INPUT create-ghost ?restore-input
   create-zombie ;
: tfind  ( c-addr -- t-xt -1 | xt +1 | c-addr 0 )
   >R  GET-ORDER  R>
   ONLY compiling  FIND IF    1
   ELSE   ONLY targeting  FIND IF   >BODY @  -1
      ELSE 0 THEN
   THEN
   2>R SET-ORDER 2R> ;
: t'  ( "name" -- t-xt )
   \ todo: implement via tfind, see [compile]
   GET-ORDER   ONLY targeting  BL WORD FIND 2>R   SET-ORDER
   2R> 0= ABORT" not a target definition"
   >BODY @ ;
ALSO postponing DEFINITIONS PREVIOUS
: [compile] ( "name" -- )
   BL WORD tfind DUP 0= ABORT" unknown (target) word"
   0< ABORT" not a compiler (immediate) word"
   POSTPONE LITERAL POSTPONE EXECUTE ; IMMEDIATE

\ alias target-memory ops into interpreter scope.  also alias compilation
\ helper words, required in target scope
interpreter
: cells tcells ;
: cell+ tcell+ ;
: chars tchars ;
: char+ tchar+ ;
: aligned taligned ;
: haligned thaligned ;
: ,  t, ;
: h,  th, ;
: c,  tc, ;
: align  talign ;
: halign  thalign ;
: here  there ;
: allot  tallot ;
: org  tdp ! ?there ;
: @  t@ ;
: ! t! ;
: h@  th@ ;
: h! th! ;
: c@  tc@ ;
: c!  tc! ;
: '  t' ;
: cfa>xt  tcfa>xt ;
: find   tfind ;
: \  ['] \  EXECUTE ; IMMEDIATE
: (  ['] (  EXECUTE ; IMMEDIATE
: [IF]  ['] [IF]  EXECUTE ; IMMEDIATE
: [ELSE]  ['] [ELSE]  EXECUTE ; IMMEDIATE
: host  host ;
: target  target ;
: interpreter interpreter ;
: compiler compiler ;
: hex  HEX ;
: decimal DECIMAL ;

\ fallbacks to use from within interpreter scope
host
: @h   @ ;
: c@h   c@ ;
: !h   ! ;
: hconstant  constant ;

\ friendly operations on task storage (USER variables)
interpreter
: uhere   save-sections taskdata here >R restore-sections R> ;
: uallot   >R save-sections taskdata R> allot restore-sections ;
: ualign   save-sections taskdata align restore-sections ;
: uhalign   save-sections taskdata halign restore-sections ;

\ forward references to threaded code handlers.  initialized by prims.fs
host
0 VALUE dovar		
0 VALUE docon		
0 VALUE douser		
0 VALUE dovalue
0 VALUE dodefer
5 VALUE docol
6 VALUE dodoes

\ forward references to primitives needed by compiler.  set by prims.fs
7 VALUE 'lit
8 VALUE 'hlit
9 VALUE 'exit
10 VALUE 'branch
11 VALUE '?branch
12 VALUE '(do)
13 VALUE '(?do)
14 VALUE '(loop)
15 VALUE '(.")
16 VALUE '(s")
17 VALUE '(abort")

\ basic threaded code
host  HEX
: branch!:  icreate ,  
  DOES>  @ >R  ( S: dst-addr src-addr     R :  opcode )
   TUCK -   2 RSHIFT  2 -  0FFFFFF  AND   R> OR  SWAP  t! ;
0EB000000 branch!:  branchl!
0EA000000 branch!:  branch!

interpreter  HEX
: cf!  ( t-addr -- )
   latest @h branchl! ;
: xt>cfa  ( xt -- a-addr )  2 LSHIFT  'xt-base + ;
: cfa>lfa  ( a-addr -- h-addr )  2 - ;
: cfa>nfa  ( a-addr -- c-addr )  cfa>lfa  begin  1- dup c@ 80 and until ;
: xt>dfa  ( xt -- a-addr )  xt>cfa cell+ ;
: >body  ( xt -- a-addr ) xt>dfa @ ;
: compile,  ( xt -- )  h, ;

\ literals
interpreter  HEX
: literal,  ( x -- )
   DUP -8000 8000 WITHIN  IF
      'hlit compile, h,
   ELSE
      'lit compile,   dup 0ffff and h,  10 rshift h,
   THEN ;
: dliteral, ( x -- )
   SWAP literal, literal, ;

\ branching
host  DECIMAL
500000 constant #forward
500001 constant #backward
500002 constant #do

interpreter
: ?pairs  ( n1 n1 -- )  <> ABORT" Control structure mismatch" ;
: mark>  ( -- t-addr '#forward' )  here  #forward 0 h, ;
: <mark  ( -- t-addr '#backward' )  here #backward ;
: resolve>  ( t-addr '#forward' -- )
   #forward ?pairs here over - 2 -  swap h! ;
: <resolve  ( t-addr '#backward' -- )   #backward ?pairs  here - 2 - h, ;
: sliteral,
   DUP C,
   OVER + SWAP ?DO  I C@H C, LOOP
   HALIGN ;
: (sliteral)  ( t-xt 'xxx"' -- )
   compile,  [char] " PARSE sliteral, ;

\ compiling (aka "immediate" words) for target
compiler
: \  ['] \  EXECUTE ; 
: (  ['] (  EXECUTE ; 
: if  '?branch compile, mark> ; 
: ahead  'branch compile,  mark> ; 
: then  resolve> ; 
: else  [compile] ahead  2SWAP  [compile] then ; 
: begin  <mark ; 
: until  '?branch compile,  <resolve ; 
: again  'branch compile,  <resolve ; 
: while  [compile] if ; 
: repeat  2SWAP [compile] again  [compile] then  ; 
: recurse  latest @h ( todo...) cfa>xt xt>dfa compile, ;

: do  '(do) compile,  false <mark #do ; 
: ?do  '(?do) compile,  mark> true   <mark #do ; 
: loop
   #do ?pairs  '(loop) compile,  <resolve  if  resolve> then ; 

: literal  literal, ;
: sliteral  '(s") compile,  sliteral, ;
: [char]   BL WORD COUNT 0= ABORT" Need character" C@H
   [compile] literal ; 
: [']
   BL WORD tfind DUP 0= ABORT" unknown (target) word"
   0> ABORT" compiler word"
   [compile] literal ;
: ."  ( -- )  '(.") (sliteral) ;
: s"  ( -- )  '(s") (sliteral) ;
: abort"  ( -- )  '(abort") (sliteral) ;

\ data defining words
interpreter
: constant  ( x "name" -- )
   >R  save-sections  cdata  (tcreate) R> ,  restore-sections
   docon cf!  zombie-does> @ ;
: create  ( x "name" -- )
   0 constant  align here latest @h cell+ ! ;
: variable  ( "name" -- )
   <section-override variables  create 1 cells allot section-override> ;
: cvariable  ( "name" -- )
   <section-override variables
   here 1 chars allot constant section-override> ;
: reserve  ( n -- t-addr )
   >R save-sections   udata here R> allot  >R restore-sections R>  ;
: buffer:  ( n "name" -- )
   >R save-sections   udata create  R> allot   restore-sections ;
: value  ( x "name" -- )
   >R save-sections  idata create R> ,  restore-sections
   dovalue  cf!   zombie-does> @ @ ;   
: to  ( x "name" -- )
   ' >body  STATE @h IF
      POSTPONE LITERAL  POSTPONE !
   ELSE  !  THEN ;  IMMEDIATE
: defer  ( "name" -- )
   <section-override deferers
   halign  here 2 chars allot constant   section-override>
   dodefer cf! ;
: is  ( t-xt "name" -- )
   ' >body  STATE @h IF
      POSTPONE LITERAL POSTPONE h!
   ELSE  h! THEN ;  IMMEDIATE
: inline-constant  ( x "name" -- )
   >R  GET-CURRENT ALSO compiling DEFINITIONS PREVIOUS
   [ host ] CREATE  R> ,   SET-CURRENT  DOES> @ [ interpreter ] literal, ;
: equ  ( x "name" -- )
   >R SAVE-INPUT  R@ inline-constant   ?restore-input
   GET-CURRENT  ALSO interpreting DEFINITIONS PREVIOUS R> hconstant
   SET-CURRENT ;

\ task-local storage "user variables"
: ucreate  ( "name" -- )
   save-sections  cdata  (tcreate) uhere h,  restore-sections
   douser cf!  kill-zombie ;
: user ( "name" -- )   ualign  ucreate  cell uallot ;
: cuser ( "name" -- )  ucreate  1 chars uallot ;

\ load assembler
host
S" ./asm.fs" INCLUDED

\ support for primitives (machine code definitions)
interpreter
: next,  ( -- )
   [ASM]
   fip 2 ]#		fwp	ldrh,		\ E09560B2
   fbp	fwp 2 #lsl	pc	add,
   [END-ASM] ;
: (code)  75757575  ]asm  ;
: (end-code)  asm[  75757575 <> ABORT" Stack not clean" ;
: code ( "name" -- )
   create-ghost   -1 cells allot   (code) ;
: end-code ( -- )  (end-code) ;
: label  ( "name" -- )  align here equ  (code) ;
: end-label ( -- )  (end-code) ;

\ cross compilation interpreter loop
host DECIMAL
VARIABLE tstate   0 tstate !

: next-word  ( -- c-addr )
   BEGIN  BL WORD  DUP C@ 0= WHILE
	 DROP REFILL 0= ABORT" Unexpected end of input"
   REPEAT ;
: target-compiler  ( -- )
   1 tstate !
   BEGIN
      next-word
      [ interpreter ] 
      tfind CASE
	 1 OF  EXECUTE ENDOF
	 -1 OF  compile,  ENDOF
	 0 OF  COUNT parse-number DUP 0=
	    ABORT" unknown target word"
	    0< IF DROP literal, ELSE dliteral, THEN
	 ENDOF
      ENDCASE
      [ host ]
   tstate @ 0= UNTIL ;

\ cross forth code compilation words
host  HEX
: (tdoes>)  ( t-addr -- )
   [ interpreter ] cf!  [ host ] ?kill-zombie  ;
interpreter
: ]  target-compiler ;
: :  ( "name" -- ) \ TODO: standard compliance: make ghost not before ;?
   <section-override colon-definitions  create-ghost  docol cf!
   [compile] ] ;
: :noname  ( "name" -- )
   <section-override colon-definitions
   create-ghost-noname OVER 1 + -ROLL 
   docol cf!  [compile] ] ;
: does>
   <section-override colon-definitions  align here >R  section-override>
   [ host ]   R> POSTPONE LITERAL  POSTPONE (tdoes>)  POSTPONE ;
   [ interpreter ]
   <section-override colon-definitions dodoes  here 0 ,  branchl!
   [compile] ] ; IMMEDIATE
: toggle  ( c-addr x -- ) OVER c@ XOR  SWAP c! ;
: immediate \ todo: generate error when used on headless code?  ignore?
   link @h cfa>nfa 40  toggle ;

compiler  HEX
: [  0 tstate !h ;
: ;  'exit compile,  [compile] [  section-override> ;

\ support for postpone in 'compiler' definitions
host  ALSO postponing DEFINITIONS PREVIOUS
: postpone ( "name" -- )
   BL WORD tfind DUP 0= ABORT" unknown (target) word"
   SWAP POSTPONE LITERAL
   0> IF POSTPONE EXECUTE
   ELSE [ interpreter ] POSTPONE compile, THEN  [ host ] ; IMMEDIATE

\ conditional compilation on target
host
: (tifdef)   BL WORD tfind NIP POSTPONE [IF] ;
: (tifundef)   BL WORD tfind NIP 0= POSTPONE [IF] ;

interpreter
: [ifdef]   (tifdef) ; IMMEDIATE
: [ifundef]   (tifundef) ; IMMEDIATE
: ?:  ( "name" -- )
   SAVE-INPUT   BL WORD find NIP >R   ?restore-input
   R> IF
      BEGIN   next-word COUNT S" ;?" COMPARE  0= UNTIL
   ELSE  :   THEN ;

compiler
: [ifdef]   (tifdef) ; 
: [ifundef]   (tifundef) ;
: ;?  POSTPONE ;  ;

\ more target memory and section handling code
host  DECIMAL
12 VALUE doimage

interpreter
: dump  ( t-addr n -- )  SWAP taddr>addr SWAP DUMP ; \ todo: reimplement
: erase ( t-addr n -- )   SWAP taddr>addr SWAP ERASE ;
: blank  ( t-addr n -- )  SWAP taddr>addr SWAP BLANK ;
: fill  ( t-addr n c -- )  ROT taddr>addr -ROT FILL ;
: move  ( t-addr1 t-addr2 n -- )  ROT taddr>addr ROT taddr>addr  ROT MOVE ;

: section-start  ( -- x )  current-section @h section-trange DROP ;
: section-end  ( -- x )  current-section @h section-trange NIP ;
: unused  ( -- n )  section-end here - ;
: section-image:  ( "name" -- )
   there section-start 2>R
   save-sections cdata (tcreate)   doimage cf!
   2R> 2DUP , ,
   ?DO I @ , 1 cells +LOOP
   restore-sections ;
: dump-section: ( "filename" -- )
   BL WORD COUNT W/O CREATE-FILE THROW >R
   section-end 1- taddr>addr   section-start taddr>addr 
   TUCK - 1+ R@ WRITE-FILE THROW
   R> CLOSE-FILE THROW ;

host

\ Tell emacs' forth-mode how to handle some of the state-smart words in here.
\ Else auto-indention will malformat this source.
0 [IF]
   Local Variables:
   forth-local-words:
   ((("tpostpone") immediate (font-lock-keyword-face . 1)
   "[ \t\n]" t name (font-lock-function-name-face . 3))
   (("t'") non-immediate (font-lock-keyword-face . 1)
   "[ \t\n]" t name (font-lock-function-name-face . 3)))
   End:
[THEN]
