include "variables.asm"

include "macros.asm"

        ORG    BASE

; Split screen between bignums (top) and text (bottom)
IF VISUALIZE
          MODE = 4
         SPLIT = 8
IF MODE = 0
        SCREEN = &3000
          ROWS = 32
          COLS = 80
ELIF MODE = 1
        SCREEN = &3000
          ROWS = 32
          COLS = 40
ELIF MODE = 2
        SCREEN = &3000
          ROWS = 32
          COLS = 20
ELIF MODE = 3
        SCREEN = &4000
          ROWS = 25
          COLS = 80
ELIF MODE = 4
        SCREEN = &5800
          ROWS = 32
          COLS = 40
ELIF MODE = 5
        SCREEN = &5800
          ROWS = 32
          COLS = 20
ELIF MODE = 6
        SCREEN = &6000
          ROWS = 25
          COLS = 40
ELSE
        SCREEN = &7C00
          ROWS = 25
          COLS = 40
ENDIF
          LINE = (&8000-SCREEN) DIV ROWS
    DATA_START = SCREEN
      DATA_END = SCREEN + LINE * SPLIT - 1
ENDIF

.code_start

        JMP    runner

; The first block of code happens to be just under 256 bytes
; so we don't waste much aligning the multiple tables AND
; we still keep the entry point at the start.

; Add a guard so we don't accidentally overflow

        GUARD  BASE + &100

; ==================================================================================
; Allocate the sump and numeratorp buffers for a specified ndigits
;
; Returns C=0 if OK; C=1 if insufficient space
; ==================================================================================

.allocate_bignums
{
; Check ndigits < &40000
        LDA     ndigits+3
        BNE     overflow
        LDA     ndigits+2
        AND     #&FC
        BNE     overflow

; Calculate raw bignum = ndigits / (8 * LOG(2)
        LDA     ndigits
        STA     arg1
        LDA     ndigits+1
        STA     arg1+1
        LDA     ndigits+2
        STA     arg1+2
        LDA     #<(1+&10000/LOG(2)/8)  ;; The +1 is to ensure rounding up
        STA     arg2
        LDA     #>(1+&10000/LOG(2)/8)
        STA     arg2+1
        JSR     multiply_24x16

; If big >= 64K it clearly won't fit
        LDA     arg1+2
        BNE     overflow

; Calculate bignum = raw bignum + 2
        CLC
        LDA     arg1
        ADC     #2
        STA     big
        LDA     arg1+1
        ADC     #0
        STA     big+1

; Calculate sump (little endian)
        LDA     #<DATA_START
        STA     sump
        LDA     #>DATA_START
        STA     sump+1
        LDX     #sump
        JSR     add_pad          ; add two lots of padding to seperate the sum and numerator
        BCS     overflow
        JSR     add_pad          ; (it's clear if you draw a picture that this is necessary)
        BCS     overflow

; Calculate numeratorp (big endian)
        LDA     #<DATA_START
        STA     numeratorp
        LDA     #>DATA_START
        STA     numeratorp+1
        LDX     #numeratorp
        JSR     add_big          ; space for the bignum
        BCS     overflow
        JSR     add_pad          ; space for a few byte above the MSB for rescaling
        BCS     overflow

; Calculate end of used memory

        LDA     sump
        STA     tmp
        LDA     sump+1
        STA     tmp+1
        LDX     #tmp
        JSR     add_big
        BCS     overflow

        LDA     tmp
        CMP     memtop
        LDA     tmp+1
        SBC     memtop+1 ; C=0 if less than memtop
        BCS     overflow

; record the highest page we have used
        LDA     tmp+1
        CMP     lastpage
        BCC     smaller
        STA     lastpage
.smaller
        CLC
        RTS
.overflow
        SEC
        RTS
}

; ==================================================================================
; Print 32-bit decimal number
; the 4-byte number to be printed is in num
; source: https://www.beebwiki.mdfs.net/Number_output_in_6502_machine_code (JGH)
; ==================================================================================

; TODO print FP numbers < 1 with a leading zero, e.g. 0.96 rather than .96

.print_decimal_32
{
        STX     tmp      ; If non-zero output a decimal point between the 100 and 10 digit
        STY     pad
        LDY     #36      ; Offset to powers of ten
.lp1
        LDX     #&FF     ; Start with digit=-1
        SEC
.lp2
        LDA     num+0
        SBC     tens+0,Y
        STA     num+0    ; Subtract current tens
        LDA     num+1
        SBC     tens+1,Y
        STA     num+1
        LDA     num+2
        SBC     tens+2,Y
        STA     num+2
        LDA     num+3
        SBC     tens+3,Y
        STA     num+3
        INX
        BCS     lp2      ; Loop until <0
        LDA     num+0
        ADC     tens+0,Y
        STA     num+0    ; Add current tens back in
        LDA     num+1
        ADC     tens+1,Y
        STA     num+1
        LDA     num+2
        ADC     tens+2,Y
        STA     num+2
        LDA     num+3
        ADC     tens+3,Y
        STA     num+3
        TXA
        BNE     digit    ; Not zero, print it
        CPY     #0
        BEQ     digit    ; last digit, force a 0 to be printed
        LDX     tmp
        BEQ     padit
        CPY     #8       ; test if fixed point, and digit is 100s
        BEQ     digit    ; yes, force a 0 to be printed
.padit
        LDA     pad
        BNE     print
        BEQ     next     ; pad<>0, use it
.digit
        LDX     #'0'
        STX     pad      ; No more zero padding
        ORA     #'0'     ; Print this digit
.print
        JSR     OSWRCH
.next
        LDA     tmp
        BEQ     normal
        CPY     #8       ; test if fixed point, and digit is 100s
        BNE     normal
        LDA     #'.'     ; yes force a . to be printed
        JSR     OSWRCH
.normal
        DEY
        DEY
        DEY
        DEY
        BPL     lp1      ; Loop for next digit
        RTS
}

CLEAR   BASE+&100,BASE+&101

include "spigot-common.6502.asm"

.tens
        EQUD    1
        EQUD    10
        EQUD    100
        EQUD    1000
        EQUD    10000
        EQUD    100000
        EQUD    1000000
        EQUD    10000000
        EQUD    100000000
        EQUD    1000000000

; ==================================================================================
; Main Program
; ==================================================================================

.runner
{
IF VISUALIZE
        JSR     print_string
        EQUB    22,MODE                      ; MODE xxx
        EQUB    28,0,ROWS-1,COLS-1,SPLIT+1   ; define text window
        EQUB    12                           ; CLS
        LDA     #<DATA_START
        STA     tmp
        LDA     #>DATA_START
        STA     tmp+1
        LDY     #&00
        LDA     #&AA
.clear_loop
        STA     (tmp),Y
        INC     tmp
        BNE     clean_skip
        INC     tmp+1
.clean_skip
        LDX     tmp
        CPX     #<(DATA_END+1)
        BNE     clear_loop
        LDX     tmp+1
        CPX     #>(DATA_END+1)
        BNE     clear_loop
ENDIF
        JSR     print_spigot_name

; Get a pointer to the *RUN parameters
; Note, this will always be in I/O memory
        LDA     #&01
        LDX     #tmp
        LDY     #&00
        STY     resultp
        JSR     OSARGS

; Copy the address of the parameter string (in host memory) into an OSWORD 5 block for later reading
        LDA     tmp
        STA     osword_05_block
        LDA     tmp+1
        STA     osword_05_block+1
        LDA     #&FF
        STA     osword_05_block+2
        STA     osword_05_block+3

; Are we running on the HOST?
        LDA     #&EA
        LDX     #&00
        LDY     #&FF
        JSR     OSBYTE
        TXA
        BNE     running_over_tube

; HOST specific actions

; Perform *TAPE
;        LDA     #&8C
;        JSR     OSBYTE

; TODO: Relocate

; Read the current value of HIMEM into memtop
IF VISUALIZE
        LDX     #<DATA_END
        LDY     #>DATA_END
ELSE
        LDA     #&84
        JSR     OSBYTE
ENDIF
        STX     memtop
        STY     memtop+1

        JMP     param_init

.running_over_tube

; Allow use of memory upto &F800
        LDA    #&00
        STA    memtop
        LDA    #&F8
        STA    memtop+1

.param_init

IF DEBUG
        JSR     print_string
        EQUS    "Data start = "
        NOP
        LDA     #<DATA_START
        STA     arg1
        LDA     #>DATA_START
        STA     arg1+1
        LDX     #arg1
        JSR     hex16

        JSR     print_string
        EQUS    "  Data end = "
        NOP
        LDX     #memtop
        JSR     hex16

        ; 16 bit Binary Search
        ;  num = &0000
        ; temp = &8000
        ; while (temp > 0) {
        ;     if (allocate_bignums(num + tmp) is OK) {
        ;         num += temp;
        ;     temp /= 2;
        ; }
        LDA     #&00
        STA     ndigits
        STA     ndigits+1
        STA     ndigits+2
        STA     ndigits+3
        STA     temp
        STA     temp+1
        LDA     #&02
        STA     temp+2
.search_loop
        CLC
        LDA     ndigits
        ADC     temp
        STA     ndigits
        LDA     ndigits+1
        ADC     temp+1
        STA     ndigits+1
        LDA     ndigits+2
        ADC     temp+2
        STA     ndigits+2
        JSR     allocate_bignums
        BCC     search_next
        SEC
        LDA     ndigits
        SBC     temp
        STA     ndigits
        LDA     ndigits+1
        SBC     temp+1
        STA     ndigits+1
        LDA     ndigits+2
        SBC     temp+2
        STA     ndigits+2
.search_next
        LSR     temp+2
        ROR     temp+1
        ROR     temp
        LDA     temp
        ORA     temp+1
        ORA     temp+2
        BNE     search_loop

        LDA     ndigits
        STA     num
        LDA     ndigits+1
        STA     num+1
        LDA     ndigits+2
        STA     num+2
        LDA     ndigits+3
        STA     num+3

        JSR     print_string
        EQUS    "Max Digits = "
        NOP
        LDX     #0
        LDY     #0
        JSR     print_decimal_32
        JSR     OSNEWL
ENDIF

; Initialize lastpage so we know if BASIC has been trampled
        LDA     #&00
        STA     lastpage

; Process the next parameter (as a number of digits)

        LDA     #&20
        PHA

.param_loop
        PLA                     ; restore next param char

        LDY     #&00
        STY     ndigits
        STY     ndigits+1
        STY     ndigits+2
        STY     ndigits+3

.param_char_loop
        CMP     #&0D
        BNE     not_done
        JMP     done
.not_done
        CMP     #' '
        BEQ     skip_it
        CMP     #','
        BNE     param_found
.skip_it
        JSR     param_getchar
        JMP     param_char_loop
.param_found
        CMP     #'0'
        BCC     param_done
        CMP     #'9'+1
        BCS     param_done
        AND     #&0F
        JSR     mult_ndigits_by_10_and_add_A
        JSR     param_getchar
        JMP     param_found
.param_done
        PHA     ; stack the non digit character

; Print the number of digits just parsed
        JSR     print_string
        EQUS    13, "Digits = "
        NOP
        LDA     ndigits
        STA     num
        LDA     ndigits+1
        STA     num+1
        LDA     ndigits+2
        STA     num+2
        LDA     ndigits+3
        STA     num+3
        LDX     #0
        LDY     #0
        JSR     print_decimal_32

        JSR     allocate_bignums
        PHP

; Print the required bignum
        JSR     print_string
        EQUS    " ("
        NOP
        LDA     big
        STA     num
        LDA     big+1
        STA     num+1
        LDX     #0
        LDY     #0
        STY     num+2
        STY     num+3
        JSR     print_decimal_32
        JSR     print_string
        EQUS    " bytes)"
        NOP
        JSR     OSNEWL

        PLP
        BCC     skip_overflow

        JSR     print_string
        EQUS    "Not enough memory", 13
        NOP
        JMP     param_loop

.skip_overflow

; Initialize bignums

        LDX     #sump
        JSR     set_initial_value
        LDX     #numeratorp
        JSR     set_initial_value_bigendian

; Store the number of digits in the results array
        LDX     #ndigits
        LDY     resultp
        JSR     store_result
        STY     resultp

; time1=time
        LDA     #&01
        LDX     #<time1
        LDY     #>time1
        JSR     OSWORD

; Compute pi
        JSR     spigot

; time2=time
        LDA     #&01
        LDX     #<time2
        LDY     #>time2
        JSR     OSWORD

; num = time2 - time1
; 4 bytes should be sufficient: 2^32 centiseconds = 497 days
        SEC
FOR I,0,3
        LDA     time2+I
        SBC     time1+I
        STA     num+I
NEXT

; Store the time in the results array
        LDX     #num
        LDY     resultp
        JSR     store_result
        STY     resultp

; Display the time
        LDX     #1
        LDY     #' '
        JSR     print_decimal_32
        JSR     print_secs

; Loop back for the next digits parmeter

        JMP     param_loop

.done
; Skip summary if there are fewer than two results
        LDY     resultp
        CPY     #16     ; each result is 8 bytes
        BCC     exit

.summary
        JSR     print_string
        EQUS    13, "Summary for "
        NOP
        JSR     print_spigot_name

        LDY     #0
.result_loop
        LDX     #num
        JSR     load_result
        TYA
        PHA
        AND     #&04
        EOR     #&04
        TAX
        LDY     #' '
        JSR     print_decimal_32
        PLA
        TAY
        AND     #&04
        BNE     result_loop
        JSR     print_secs
        CPY     resultp
        BCC     result_loop

.exit
; Have we possibly clobbered the current language?
IF BASE<>&400
        LDA     lastpage
        BMI     exit_by_running_basic
        RTS
ENDIF

; If so, exit with *BASIC
.exit_by_running_basic
        LDX     #<basic
        LDY     #>basic
        JMP     OSCLI

.basic
        EQUS    "BASIC",13

.time1
        SKIP    5

.time2
        SKIP    5
}


.print_spigot_name
{
; Say hello
        JSR     print_string
IF BELLARD
        EQUS    "Bellard Pi Spigot", 13
ELSE
        EQUS    "BBP Pi Spigot", 13
ENDIF
        NOP
        RTS
}

.print_secs
{
        JSR     print_string
        EQUS    " secs",13
        NOP
        RTS
}

; ==================================================================================
; Fetch the *RUN params usimg OSWORD A=&05 so it works over the tube
; ==================================================================================

.param_getchar
{
        LDA    #&05
        LDX    #<osword_05_block
        LDY    #>osword_05_block
        JSR    OSWORD               ; reading the next parameter char
        INC    osword_05_block
        BNE    skip
        INC    osword_05_block+1
.skip
        LDA    osword_05_block+4
        RTS
}

.osword_05_block
        SKIP    5

; ==================================================================================
; Set Initial value to all zero, except for a 4 in element big-1
; ==================================================================================

.set_initial_value
{
        CLC
        LDA    0,X
        ADC    big
        STA    tmp
        LDA    1,X
        ADC    big+1
        STA    tmp+1

        _DEC16 tmp        ; tmp now points to element big-1

        LDY    #PAD
        LDA    #0
.pad
        STA    (tmp),Y
        DEY
        BNE    pad

        LDA    #4
        STA    (tmp),Y

.loop
        _DEC16 tmp

        LDA    #0
        STA    (tmp),Y

        LDA    tmp
        CMP    0,X
        BNE    loop
        LDA    tmp+1
        CMP    1,X
        BNE    loop
        RTS
}

.set_initial_value_bigendian
{
        LDA    0,X
        STA    tmp
        LDA    1,X
        STA    tmp+1

        _SUB16  tmp, tmp, big
        _SUB16C tmp, tmp, PAD ; tmp now points to element big+PAD

        LDY    #PAD
        LDA    #0
.pad
        STA    (tmp),Y
        DEY
        BNE    pad

        _ADD16C tmp, tmp, PAD+1 ; tmp now points to element big-1

        LDA    #4
        STA    (tmp),Y

.loop
        _INC16 tmp

        LDA    #0
        STA    (tmp),Y

;        LDA    tmp
;        CMP    0,X
;        BNE    loop
;        LDA    tmp+1
;        CMP    1,X
;        BNE    loop
        RTS
}

; ==================================================================================
; Print a string embedded in the code, terminated by a negative byte
; ==================================================================================

.print_string
{
        PLA
        STA tmp
        PLA
        STA tmp + 1

        LDX #0
.loop
        INC tmp
        BNE nocarry
        INC tmp + 1
.nocarry
        LDA (tmp, X)
        BMI done
        JSR OSASCI
        JMP loop
.done
        JMP (tmp)
}

; ==================================================================================
; Store result
; ==================================================================================
.store_result
{
FOR I,0,3
        LDA     I,X
        STA     results,Y
        INY
NEXT
        RTS
}
; ==================================================================================
; Load result
; ==================================================================================
.load_result
{
FOR I,0,3
        LDA     results,Y
        STA     I,X
        INY
NEXT
        RTS
}

; ==================================================================================
; Increment the specified (in X) 2-byte pointer by PAD (4)
; ==================================================================================
.add_pad
{
        CLC
        LDA     0,X
        ADC     #<PAD
        STA     0,X
        LDA     1,X
        ADC     #>PAD
        STA     1,X
        RTS
}

; ==================================================================================
; Increment the specified (in X) 2-byte pointer by big
; ==================================================================================

.add_big
{
        CLC
        LDA     0,X
        ADC     big
        STA     0,X
        LDA     1,X
        ADC     big+1
        STA     1,X
        RTS
}

; ==================================================================================
; Multiply 4-byte ndigits by 10 and add the accumulator
; ==================================================================================

.mult_ndigits_by_10_and_add_A
{
        LDX    #0
        STA    tmp+1
 .loop
        LDA    #0
        STA    tmp       ; MSB 8 bits of *10
        LDA    ndigits, X
        ASL    A
        ROL    tmp       ; tmp, A = bignum * 2
        ASL    A
        ROL    tmp       ; tmp, A = bignum * 4
        ADC    ndigits, X
        BCC    nc
        INC    tmp       ; tmp, A = bignum * 5
.nc     ASL    A
        ROL    tmp       ; tmp, A = bignum * 10
        CLC
        ADC    tmp+1
        STA    ndigits, X
        LDA    #0
        ADC    tmp
        STA    tmp+1
        INX
        CPX    #&04
        BNE    loop
        RTS
}


; ==================================================================================
; Unsigned multiply of arg1 (24 bits) x arg2 (16 bits) with
; the 40-bit result being placed in arg1 (MSB) and arg2 (LSB)
; source: http://forum.6502.org/viewtopic.php?p=2846#p2846 (Garth Wilson)
; ==================================================================================

.multiply_24x16
{
        LDA     arg2     ; Get the multiplicand and
        STA     tmp      ; put it in the scratchpad.
        LDA     arg2+1
        STA     tmp+1
        LDA     #0
        STA     arg2     ; Zero-out the original multiplicand area.
        STA     arg2+1

        LDY     #24      ; We'll loop 16 times.
.l1
        ASL     arg2     ; Shift the entire 32 bits over one bit position.
        ROL     arg2+1
        ROL     arg1
        ROL     arg1+1
        ROL     arg1+2
        BCC     l2       ; Skip the adding-in to the result if
                         ; the high bit shifted out was 0.
        CLC              ; Else, add multiplier to intermediate result.
        LDA     tmp
        ADC     arg2
        STA     arg2
        LDA     tmp+1
        ADC     arg2+1
        STA     arg2+1

        LDA     #0       ; If C=1, incr lo byte of hi cell.
        ADC     arg1
        STA     arg1
.l2
        DEY              ; If we haven't done 16 iterations yet,
        BNE     l1       ; then go around again.
        RTS
}
.code_end

IF VISUALIZE
ELSE
.DATA_START
ENDIF

SAVE code_start, code_end, runner
