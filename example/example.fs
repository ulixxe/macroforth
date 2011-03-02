( * setup cross-compiler for AT91RM9200 * )

include ../cross.fs

\ define memory layout 
interpreter hex
00000000 0000FFFF cdata section flash
00200000 00200FFF idata section iram
00201000 00203FFF udata section uram
flash iram uram

\ range addressible through 16-bit XT's is 'xt-base ... 'xt-base + 256KByte
00000000 TO 'xt-base

\ where to put the stack?  required by prims.fs, kernel.fs
target
00203000  EQU 'stack-bottom

\ the vector table.  later initialize vectors using '<addr> <label> branch!'
target cdata 0 ORG
label b-irq-reset  here B, end-label
label b-irq-undef  here B, end-label
label b-irq-swi	   here B, end-label
label b-irq-pfabrt here B, end-label
label b-irq-dabrt  here B, end-label
label b-irq-unused here B, end-label
label b-irq-irq    here B, end-label
label b-irq-fiq    here B, end-label

\ enable generation of word headers.  we want to run a full forth-interpreter
\ inside the target system!
interpreter
headless off

\ compile asm primitives into cdata space
include ../prims.fs

\ now compile the higher-level code written as forth definitions
include ../kernel.fs

( * now compile our application code * )

target cdata HEX

\ USART serial console code for AT91RM9200
0fffc8000 constant us-cr
0fffc8014 constant us-csr
0fffc8018 constant us-rhr
0fffc801c constant us-thr

: us-init ( -- )  50 us-cr ! ;
: my-emit   begin  us-csr @ 2 and until  us-thr ! ;
: my-key?  us-csr @ 1 and 0<> ;
: my-key  begin my-key? until  us-rhr @ ;

' my-emit is emit
' my-key is key
' my-key? is key?

( * finalize firmware image and add bootup code.  do not touch. * )

\ freeze the IDATA section by copying it into a 'self-extracting' image in
\ CDATA space.  Running the forth-word of the image, restores the IDATA
\ section at run-time
idata IRAM  section-image: idata-image

\ after freezing the image, changes to idata variables do not take effect.
\ this has the side-effect that any words added hereafter, won't be linked
\ into the dictionary (but you can still run the code, if it resides in cdata
\ space)
interpreter headless on  \ headers are a waste of time 

\ forth code entry point.  note: stacks not yet set up
target cdata HEX
: (cold)
   !sp			\ init data stack to 'stack-bottom
   00202000 rp!		\ now init return stack from data stack
   idata-image		\ initialize IRAM (needed to run some primitives!)
   us-init		\ init serial port
   ( ... add some code here to give life-signs for 1st testing... )
   abort ;		\ run the Forth interpreter (over serial cable)

\ a memory location that holds the forth code entry point
label forth-reset-addr  ' (cold) xt>dfa ,  end-label

\ definition for reset interrupt: call into forth interpreter at (cold)
cdata target label reset
   'xt-base # FBP MOV,			\ set forth base pointer
   forth-reset-addr [#] FIP LDR,	\ set forth instruction pointer
   ( .. todo add some asm code to give a life-sign, set leds etc. ... )
   'next B,				\ start the forth interpreter
end-label

\ rewrite reset IRQ entry to point to the above reset code 
reset  b-irq-reset  branch!

( * done.  now the cdata image can be dumped.  see the Makefile * )