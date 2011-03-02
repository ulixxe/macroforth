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

target cdata HEX

0fffc8000 constant us-cr
0fffc8004 constant us-mr
0fffc8008 constant us-ier
0fffc800c constant us-idr
0fffc8010 constant us-imr
0fffc8014 constant us-csr
0fffc8018 constant us-rhr
0fffc801c constant us-thr
0fffc8020 constant us-brgr
0fffc8024 constant us-rtor
0fffc8028 constant us-ttgr
0fffc8040 constant us-fidi
0fffc8044 constant us-ner
0fffc804c constant us-if

: us-init ( -- )  50 us-cr ! ;

: my-emit   begin  us-csr @ 2 and until  us-thr ! ;
: my-key?  us-csr @ 1 and 0<> ;
: my-key  begin my-key? until  us-rhr @ ;

' my-emit is emit
' my-key is key
' my-key? is key?
