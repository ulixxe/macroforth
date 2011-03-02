----------------------------------------------------------------------
MacroForth -- Minimum ARM Cross Forth Compiler 
----------------------------------------------------------------------

Copyright (C) David Kuehling <dvdkhlng TA gmx TOD de> 2007-2011 

License Information
-------------------

You may use MacroForth under the terms of the GNU General Public License
version 3, *no warranty*.  See the file COPYING for details.  The run-time
library routines (supplied in 'prim.fs' and 'kernel.fs') have a license
exception that allow you to use these even for proprietary/commercial
software.

In other words: redistribution of the MacroForth compiler must adhere to
MacroForth's license.  But programs compiled with MacroForth are freed from
any obligations under that license.  For details see license exception at top
of 'prim.fs'.

Installation
------------

Just type 'make install' (as root user).

Reference Documentation
-----------------------

For the Forth programming language, see the ANS Standard:

  http://www.taygeta.com/forth/dpans.html

The cross compiler functionality in MacroForth is modeled after the Cross
Compiler Standard Draft:

  http://www.forth.com/downloads/ANS/XCtext5.pdf
  http://www.forth.com/downloads/ANS/XCapp5.pdf
  http://www.forth.com/downloads/ANS/XCpaper.pdf
  (or: http://www.forth.org.ru/~mlg/std/ANSI-XC/)

Usage
-----

For a usage example see the 'examples' subdirectory, which also contains a
Makefile to compile a minimum program.  Technical details below might be a
little out-of-date compared to the example.

MacroForth is meant to be run using the Gforth interpreter as run-time
environment.  In your Forth program, load the cross compiler, via:

   include macroforth/cross.fs

Then setup the memory layout of your platform, using the 'section' commands.
Note that currently any Forth primitives and threaded code must be located in
address range 0...2^18.  Since some code is currently emitted into idata
space, this restriction applies to both idata and cdata sections, although
this can be easily fixed, see also below.

 Also setup a constant for the bottom of your data stack
via:

  <address> EQU 'stack-bottom

After setting up memory layout, load the Forth run-time support routines
(primitives and kernel) into your program:

  headless off
  \ start code at address 0100 (leaving space for the vector table)
  target cdata 0100 ORG  
  include macroforth/prims.fs
  include macroforth/kernel.fs

After that you're free to add your programm's routines.  MacroForth is
platform independant, so you'll have to add program startup code yourself.  A
simple assembler startup routine should setup the Forth instruction pointer
register (FIP, see 'asm.fs' for details), and the base pointer (should be zero
for now):

  cdata
  label reset  
     0 #		FBP	MOV,	\ set base pointer to zero
     ' (cold) xt>dfa #	FIP	MOV,	\ start forth execution with (cold)
     next,
  end-label

If you run your program without operating system, you'll just have to make the
reset interrupt vector point to your 'reset' assembler routine.  Since on ARM,
interrupt vectors contain branch instructions (instead of just addresses, as
for x86), use the command 'branch!' to write the interurpt vector, ie:

  reset  0 branch!

The Forth entry point '(cold)' must then setup data-stack and return-stack
pointers, before any other code can be executed, ie:

  : (cold)
     !sp           \ initalize stack pointer to value of 'stack-bottom
     1000 rp!	   \ initialise return-stack pointer to 1000 
     ...	   \ now other code can follow
     ;

(of course you must define (cold) before the 'reset' label)

If you are using any IDATA sections, freeze these sections into cdata
(i.e. ROM), and put the corresponding initialization code to (cold):

  idata section-image: iram-image
  : (cold)
     !sp  1000 rp!  iram-image ;

Note that you can only use the kernel's text interpreter and dictionary
routines, if you initialize the IDATA section that was at use when you loaded
the kernel.  If you want to enter the interpreter's terminal at bootup, just
call 'abort':

  : (cold)
     !sp  100 rp!  iram-image  abort ;

After your code has been compiled as described, you need to call
'dump-section:' to write the result into a file:

  cdata dump-section: firmware.img 

Section images are just 1:1 binary version of a single section, usually the
cdata section corresponding to your device's ROM.  Now its up to you to get
the data onto your target device.  Depending on the available firmware
loaders, you may have to somehow convert the data to other formats (hexfile
etc).

Platform Specific Code
----------------------

For your platform, you'll need to define the memory layout via 'section'.
Also any terminal input/output routines must be specified by the user.  to do
so, just define the 'emit', 'key' and 'key?' routines (which are defined as
placeholders via DEFER):

  : my-emit   ... ;
  : my-key   ... ;
  ' my-emit is emit
  ' my-key is key


Extensions to ANS and the Cross-Forth standard draft
----------------------------------------------------

MacroForth supports DEFER and IS, as defined in GNU Forth.  The new word
DEFERERS works like the XCForth word VARIABLES, defining which section
deferers are placed in.  For forward references resolved at compile-time, use
CDATA DEFERERS, for definitions changed at run-time use IDATA DEFERERS.

The code- and data-field of DEFERERS and VARIABLES still resides in CDATA
space.  The data-field then points to the actual address of the data item to
use, and the code-field points to a handler that does the additional
indirection.  (so VARIABLES uses the 'docol' handler).

Other words to control the section used for certain types of definitions:

- COLON-DEFINITIONS

  Set the section used for normal Forth code (typically CDATA).  Note that
  unlike VARIABLES and DEFERERS, COLON-DEFINITIONS affects placement of the
  code- and data-fields of the created definitions.

- HEADERS   

  Set the section used for the dictionary headers (typically CDATA).
  MacroForth supports "remote headers", where the header of a definition
  resides in a different section from the actual code-field and data part of
  the definition.  This is a pretty ugly feature, but it helps to save IDATA
  space, when IDATA has to hold many primitives for performance reasons.

Some more words practical for embedded use are available:

andc  ( x1 x2 -- x3 )   \ like INVERT AN
-rot  ( x1 x2 x3 -- x3 x1 x2 )
h@   \ load halfword
h!   \ store halfword
sh@   \ load and sign-extend halfword
haligned   \ align to halfword
bounds   \ like in GNU Forth

Note that all number conversion words only works on 32-bit numbers, ignoring
the higher-level part in double-precision routines (e.g. #).  Most
double-precision number output routines are not even implemented. 

Threading Model
---------------

MacroForth currently uses direct threading Forth code, with 16-bit execution
tokens.  The rest of the system is completely 32-bit.

ARM instructions must be aligned on a multiple of 4 anyway, and since we use
direct threaded code, every execution token is shifted left by 2 before
jumping to the corresponding address.  So the full address range of executable
code is 4*2^16 = 256KByte, starting at address 0.

The implementation of the threaded code interpreter, NEXT, uses register 'FBP'
(R4) as a base register for threaded code, adding it to the shifted execution
token before execution.  For now whis register must be initialized 0 before
starting up Forth.  With some minor changes to cross.fs, we would be able to
put executable code into any 256K address range, not only the address range
starting at 0.

The list of registers used by the Forth interpreter is (see also asm.fs):

  R13 2CONSTANT FSP	\ forth stack pointer
  R12 2CONSTANT FLC	\ forth loop counter
  R11 2CONSTANT FLB	\ forth loop bound
  R10 2CONSTANT FTP	\ forth task pointer (currently unused?
  R1  2CONSTANT TOS	\ forth top of stack
  R7  2CONSTANT FRP	\ forth return stack pointer
  R6  2CONSTANT FWP	\ "word"/"working" pointer 
  R5  2CONSTANT FIP	\ forth instruction pointer
  R4  2CONSTANT FBP	\ forth code base pointer (must be 0 for now)

R0, R2, R3, R8, R9 are free for temporary use inside primitives.

The implementation of NEXT is:

   FIP 2 ]#		FWP	LDRH,
   FBP	FWP 2 #LSL	PC	ADD,

'prims.fs' puts some often used primitives into IDATA (RAM) as that performs
better on some very cheap ARM SOCs (ARMv4).  If you remove the 'idata'
statements in prims.fs, you might be able to compile code for platforms, where
RAM is not accessible within the lower 256KByte of address space (note: needs
to be double-checked).
