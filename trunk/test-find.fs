target HEX
VARIABLE latest
host link @  target latest !

create "exit"  4 c, char e c, char x c, char i c, char t c,
create "drop"  4 c, char d c, char r c, char o c, char p c,
create "lit"  3 c, char l c, char i c, char t c,
create "mut"  3 c, char m c, char u c, char t c,

interpreter
: andc  invert and ;
: count  ( c-addr1 -- c-addr2 n )  dup c@ swap 1+ swap ;
: /string  ( c-addr1 u1 n -- c-addr2 u2 )  rot over +  -rot - ;
: move  ( c-addr1 c-addr2 u -- )
   bounds ?do  dup c@ i c!  1+  loop  drop ;
: erase  ( c-addr1 u -- )
   bounds ?do  0 i c! loop ;
: compare  ( c-addr1 u1 c-addr2 u2 -- +1|0|-1 )
   bounds ?do
      dup 0= if  2drop  -1 unloop exit then
      over c@  i c@  -  dup if  0< 1 or  nip nip  unloop exit then  drop
      1 /string
   loop
   nip  0<> 1 and ;

: xt>cfa  2* 2* ;
: cfa>xt  2/ 2/ ;
: cfa>lfa  2 - ;
: nfa>lfa  ( c-addr -- xt )
   count 1f and +   5 + 3 andc 2 - ;
: nfa>xt  ( c-addr -- xt )
   dup  nfa>lfa  2 +
   swap  c@ 20 and if h@	\ resolve remote headers
   else  cfa>xt  then ;

: lfa>nfa  ( a-addr -- c-addr )  begin  1- dup c@ 80 and until ;

interpreter
: find  ( c-addr -- t-xt -1 | xt +1 | c-addr 0 )
   latest @  	\ latest must not have a remote header!
   begin dup while
	 cr .s cfa>lfa dup lfa>nfa 2>r 
	 cr .s dup count  r@ count 1f and  compare 0= if
	    cr .s drop  r@ nfa>xt	\ get xt 
	    2r> nip  c@ 40 and 0= 1 or	\ get immediate-flag
	    exit
	 then
	 2r> cr .s drop h@ xt>cfa	\ get pointer to next header
	 cr .s
   repeat
   drop 0 ;

target
cr "exit" find . . .s