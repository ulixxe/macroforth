\
\ Minimum ARM Cross-Forth compiler  -- target forth kernel
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

\ constants
target DECIMAL
0 constant false
-1 constant true
4 constant cell
host 'xt-base target EQU 'xt-base

0 constant 0
1 constant 1
2 constant 2
3 constant 3

defer emit
defer key
defer key?
defer abort

\ variables
idata variables
variable latest
variable dp
udata variables

\ During compilation, the cross compiler will use it's own variables to keep
\ track of the dictionary.  In the target system, we need copies of the last
\ values of these variables, to access the dictionary.  The following hook
\ updates the target dictionary (idata) variables whenever the cross compiler
\ adds data or new definitions to the dictionary.
interpreter
: (modified-there)
   save-sections
   udata there dp !
   [ host ] link @ [ interpreter ] latest !
   restore-sections ;
' (modified-there) is modified-there
target

\ more stack ops
?: nip  ( x1 x2 -- x2 )  swap drop ;?
?: -rot  ( x1 x2 x3 -- x3 x1 x2 )  rot rot ;?
?: 2dup  ( x1 x2 -- x1 x2 x1 x2 )  over over ;?
?: 2drop  ( x1 x2 -- )  drop drop ;?
?: 2swap  ( x1 x2 x3 x4 -- x3 x4 x1 x2 )  rot >r rot r> ;?
?: 2>r  r>   rot >r swap >r  >r ;?
?: 2r>  r>   r> r> swap  rot >r ;?
?: tuck  ( x1 x2 -- x2 x1 x2 )   swap over ;?
: depth  ( -- n )  'stack-bottom sp@ -  2/ 2/  2 - ;
: pick  ( n -- )
   dup if  cells sp@ + cell+ @  exit then
   drop dup ;	\ special-case for '0 pick' since TOS in register

\ multi-tasking support
USER 'tos
USER next-task

\ : pause  rp@  sp@  'tos !  next-task @ >r ;
\ : wake  r> tp!  'tos @ sp! rp! ;

\ various helpers
: min  2dup > if swap then drop ;
: max  2dup < if swap then drop ;
: within  ( u1 u2 u3 -- f )  over - >r   -  r> u< ;

\ integer division (ARM does not have divide opcodes)
?: u/mod  ( u1 u2 -- u3-rem u4-quot )
   1 >r      ( S: divident divisor )   ( R: count )
   begin		\ iterate forward to highest result bit
      2dup 2* u< 0=
      over [ 1 31 lshift ] literal u< and
   while   r> 1+ >r  2*  repeat
   0 -rot    ( S: result divident divisor )   
   r>  0 ?do		\ iterate backward and calculate the result 
      2dup u< 0=   dup >r if  tuck - swap then
      rot   2* r> 1 and or  -rot
      1 rshift
   loop
   drop  swap ;?
: u/  ( u1 u2 -- u3 )  u/mod nip ;
: umod  ( u1 u2 -- u3 )  u/mod drop ;

\ string and memory routines
: aligned  3 + 3 andc ;
: haligned  1 +  1 andc ;
: bounds  ( c-addr1 u1 -- c-addr2 c-addr1 ) over + swap ;
: count  ( c-addr1 -- c-addr2 n )  dup c@ swap 1+ swap ;
: /string  ( c-addr1 u1 n -- c-addr2 u2 )  rot over +  -rot - ;
?: move  ( c-addr1 c-addr2 u -- )
   bounds ?do  dup c@ i c!  1+  loop  drop ;?
?: fill ( c-addr u char -- )  -rot bounds ?do  dup i c! loop drop ;?
: erase  ( c-addr1 u -- )  0 fill ;
?: compare  ( c-addr1 u1 c-addr2 u2 -- +1|0|-1 )
   bounds ?do
      dup 0= if  2drop  true unloop exit then
      over c@  i c@  -  dup if  0< 1 or  nip nip  unloop exit then  drop
      1 /string
   loop
   nip  0<> 1 and ;?
: chars ;
: char+ 1+ ;
: @slit  ( -- c-addr u )
   r>
   r> count  2dup + haligned >r
   rot >r ;
: (s")  @slit ;   ' (s") host TO '(s")  target

\ terminal output
32 constant bl

: type  ( c-addr u -- )  \ fails if u==0 !
   bounds ?do  i c@ emit loop ;
: space  ( -- )  bl emit ;
: cr  13 emit 10 emit ;
: .esc  27 emit ;
: spaces   ( u -- )  0 max  0 ?do space loop ;
: (.")  @slit type ;  ' (.") host TO '(.")  target
: (abort")  @slit rot if  type abort exit then  2drop ;
' (abort")  host TO '(abort")  target
: page  .esc ." [2J"  .esc ." [1;1H" ;

: accept1  ( c-addr u1 u2 -- c-addr u1 u3  flag )
   key 
   dup 13 =  over 10 = or if		\ return
      0<> exit
   then
   dup 127 =  over 8 = or if    drop	\ backspace/DEL
      dup if
	 1-
	 8 dup emit space emit
      then
      false exit
   then
   >r 2dup > if			\ normal char
      rot 2dup +  r@ swap c!
      -rot
      1+
      r@ emit
   then
   r> drop   false ;
: accept  ( c-addr u1 -- u2 )
   0  begin accept1 until  nip nip ;
   
\ dictionary
HEX
: here  dp @ ;
: pad  here 0C + ;
: allot   dp +! ;
: ,  here !  cell allot ;
: h,  here h!  2 allot ;
: c,  here c!  1 allot ;
: align  here aligned dp ! ;
: halign   here haligned dp ! ;

\ number output conversion
DECIMAL
variable base
variable hld

: decimal  10 base ! ;
: hex  16 base ! ;
: <#  ( -- )  pad  hld ! ;
: hold  ( char -- )  true hld +!  hld @ c! ;
: sign  ( d1 -- ud2 )  0< if  [char] - hold then ;
: #  ( ud1 -- ud2 )	\ currently only single-cell (32bits) processed
   drop base @ u/mod
   swap dup 9 > if 7 + then  [char] 0 +  hold
   0 ;
: #>  ( ud -- c-addr n )
   2drop  hld @  pad over - ;
: #s  ( d -- 0 0 )
   begin  #  2dup or 0= until ;
: .r  ( n1 n2 -- )
   >r dup abs
   0 <# #s rot sign #>
   r> over - spaces
   type ;
: .  ( n -- )  0 .r space ;
: (dump)  ( c-addr u -- )
   base @ >r hex  
   over 0 <# # # # # # #> type ." : "
   bounds 2dup do
      i c@ 0 <# # # #> type  space 
   loop
   space
   do
      i c@  dup bl 128 within 0= if drop [char] . then  emit
   loop
   r> base ! ;
: dump  ( c-addr u -- )
   begin dup 0> while
	 2dup 16 min cr (dump)
	 16 /string
   repeat 2drop ;
: .s  ( n*x -- )
   depth dup ." <" 0 .r ." > "
   0 ?do   depth i 1+ - pick .   loop ;
   
\ number input conversion
: digit   ( char -- u false | true )
   [char] 0 -  dup 9 > if
      7 -  dup 10 < or
   then
   dup base @  u< 0= dup if  nip then ;
: (>number)  ( u1 c-addr1 u2 -- u3 c-addr2 u3 )
   begin		( S: accu  c-addr1 u1 )
      over c@  digit if	 exit   then
      >r
      1 /string
      rot   base @ *  r> +   -rot
   dup 0= until ;
: >number  ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
   rot drop  (>number)  0 -rot ;

\ word headers and searching

\ note: in remote-headers we use xt and cfa routines, although there is no
\ code at all.  in that case the "code field" contains only the 2-byte xt of
\ the actual code located in some other section

HEX
: xt>cfa  2* 2* 'xt-base + ;
: cfa>xt  'xt-base -  2/ 2/ ;
: cfa>lfa  2 - ;
: nfa>lfa  ( c-addr -- xt )
   count 1f and +   5 + 3 andc 2 - ;
: nfa>xt  ( c-addr -- xt )
   dup  nfa>lfa  2 +
   swap  c@ 20 and if h@	\ resolve remote headers
   else  cfa>xt  then ;
: lfa>nfa  ( a-addr -- c-addr )  begin  1- dup c@ 80 and until ;
: cfa>nfa  ( a-addr -- c-addr )  cfa>lfa lfa>nfa ;
: find  ( c-addr -- t-xt -1 | xt +1 | c-addr 0 )
   latest @  	\ latest must not have a remote header!
   \ not a problem since initialized from host's 'link' variable?
   begin dup while
	 cfa>lfa dup lfa>nfa 2>r 
	 dup count  r@ count 1f and  compare 0= if
	    drop  r@ nfa>xt	\ get xt 
	    2r> nip  c@ 40 and 0= 1 or	\ get immediate-flag
	    exit
	 then
	 2r> drop h@ xt>cfa	\ get pointer to next header
   repeat
   drop 0 ;

\ code generation
HEX
: (branch!)  ( opcode dst-addr src-addr -- )
  >r  tuck -   2 rshift  2 -  0FFFFFF  and   r> or  swap  ! ;
: branchl!  0EB000000 (branch!) ;
: branch!   0EA000000 (branch!) ;
: cf!  ( a-addr -- )
   latest @ branchl! ;
: compile,  h, ;
: literal,   DUP -8000 8000 WITHIN  IF
      ['] hlit compile, h,
   ELSE
      ['] lit compile,   dup 0ffff and h,  10 rshift h,
   THEN ;

\ parser
DECIMAL
variable >in
64 buffer: tib
variable #tib
variable state
: source  tib #tib @ ;
: parse  ( char "xxx<char>" -- c-addr u )
   >in @ >r
   begin
      source >in @ /string   0= if
	 2drop  source r> /string exit
      then
      1 >in +!
      c@ over =
   until
   drop  source drop r@ +  >in @ 1- r> - ;
: parse-word  ( char "<chars>xxx<char"> -- c-addr u )
   begin
      dup
      source >in @ /string
      -rot  c@ = and
   while  1 >in +!  repeat
   parse ;
: word  ( char "<chars>xxx<char"> -- c-addr | 0 )
   parse-word   dup here c!  here -rot  here 1+ swap move ;
: number  ( c-addr u -- n flag )
   over c@ [char] - = dup >r 1 and /string
   0 -rot (>number) nip 0=
   r> if swap negate swap then ;
: interpret ( -- )
   begin
      bl word dup c@ 0<>
   while
	 find dup if
	    state @ = if compile, else execute then
	 else
	    drop  count number  0= abort" unknown"
	    state @ if literal, then
	 then
   repeat  drop ;
: refill
   cr  tib 64 accept  #tib !  0 >in !  space ;
: quit
   begin
      refill
      interpret
      state @ 0= if ." ok" then
   again ;
: (abort)
   !sp  decimal  0 state !
   cr ." MACroForth (C) David Kuehling 2007-2009"
   quit ;
' (abort) is abort
   
\ defining words
HEX

compiler
: compile  ( "name" -- )
   '  postpone literal   postpone compile, ;
: [compile]  ( "name" -- )
   '  compile, ;

target
: toggle  ( c-addr x -- )  over c@ xor  swap c! ;
: (create)  ( "name" -- )
   bl word  dup c@ 1+ allot
   0a0 toggle
   begin here 3 and 2 <> while 0 c, repeat 
   latest @ cfa>xt h,   here latest !
   0 , ;
: reveal  ( -- )   latest @ cfa>nfa  020 toggle ;
: constant  ( x "name" -- )   (create)  docon cf!  ,  reveal ;
: create  ( "name" -- )   0 constant  here latest @ cell+ ! ;
: variable  ( "name" -- )   create cell allot ;
\ todo: 'immediate' currently does not work with headless code.  on the other
\ hand, we won't need a full kernel when doing headless code.  exclude via
\ [IF]?
: ] ( -- ) true state ! ;
: [ ( -- ) false state ! ; immediate
: : ( -- ) (create) docol cf!  ] ;
: ; ( -- ) compile exit [compile] [ reveal ; immediate

\ control flow
DECIMAL
500000 constant #forward
500001 constant #backward
500002 constant #do

: ?pairs  ( n1 n1 -- )  <> ABORT" unstructured" ;
: mark>  ( -- t-addr '#forward' )  here  #forward 0 h, ;
: <mark  ( -- t-addr '#backward' )  here #backward ;
: resolve>  ( t-addr '#forward' -- )
   #forward ?pairs here over - 2 -  swap h! ;
: <resolve  ( t-addr '#backward' -- )   #backward ?pairs  here - 2 - h, ;
: sliteral,  ( c-addr u -- )
   dup c,
   here over allot  swap move   halign ;
: (sliteral)  ( t-xt 'xxx"' -- )
   compile,  [char] " parse sliteral, ;

: if  compile ?branch  mark> ; immediate
: ahead  compile branch  mark> ; immediate
: then  resolve> ; immediate 
: else  [compile] ahead  2swap  [compile] then ; immediate 
: begin  <mark ; immediate 
: until  compile ?branch  <resolve ; immediate 
: again  compile branch   <resolve ; immediate 
: while  [compile] if ; immediate 
: repeat  2swap [compile] again  [compile] then  ; immediate 

: do  compile (do)  false <mark #do ; immediate 
: ?do  compile (?do) mark>  true <mark #do ; immediate 
: loop
   #do ?pairs  compile (loop) <resolve  if resolve> then ; immediate 

: literal  literal, ; immediate
: sliteral  compile (s")  sliteral, ; immediate
: char   bl word count 0= abort" char?" c@ ; 
: [char]   char  [compile] literal ; immediate 
: '  ( "name" -- xt )  bl word find 0= ABORT" unknown" ;
: [']  '  [compile] literal ; immediate 
: ."  ( -- )  ['] (.") (sliteral) ; immediate
: s"  ( -- )  ['] (s") (sliteral) ; immediate
: abort"  ( -- )  ['] (abort") (sliteral) ; immediate

: \  begin  bl word c@ 0=  until ; immediate
: (  begin
      [char] ) parse nip dup 0= if  drop refill drop then
   until ; immediate

0 [IF]
   Local Variables:
   forth-local-indent-words:
   ((("?:") (0 . 2) (0 . 2))
   ((";?") (-2 . 0) (0 . -2)))
   forth-local-words:
   ((("?:") definition-starter (font-lock-keyword-face . 1)
   "[ \t\n]" t name (font-lock-function-name-face . 3))
   ((";?") definition-ender (font-lock-keyword-face . 1)))
   End:
[THEN]