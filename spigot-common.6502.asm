; ==================================================================================
; Multiplication tables
;
; Bellard uses 5 pages
; BBP uses 4 pages
; ==================================================================================

ALIGN &100

IF BELLARD

.mult250_table
FOR I,0,255
EQUB (I*250) MOD &100
NEXT
FOR I,0,255
EQUB (I*250) DIV &100
NEXT

.mult1000_table
FOR I,0,255
EQUB (I*1000) MOD &100
NEXT
FOR I,0,255
EQUB ((I*1000) DIV &100) MOD &100
NEXT
FOR I,0,255
EQUB (I*1000) DIV &10000
NEXT

ELSE

.mult10_table
FOR I,0,255
EQUB (I*10) MOD &100
NEXT
FOR I,0,255
EQUB (I*10) DIV &100
NEXT

.div16_table_lsb
FOR I,0,255
EQUB (I MOD 16) * 16
NEXT

.div16_table_msb
FOR I,0,255
EQUB (I DIV 16)
NEXT

ENDIF

; ==================================================================================
; Note: Division code comes next, so page alignment doesn't randomly change
; ==================================================================================

IF INCLUDE_DIV40
.divadd40
        CLC
        BCC    div40

.divsub40
        SEC
.div40
        _DIVADDSUB 5
ENDIF

.divadd32
        CLC
        BCC    div32

.divsub32
        SEC
.div32
        _DIVADDSUB 4

IF OPTIMIZE_DIV24

.divadd24
        CLC
        BCC    div24

.divsub24
        SEC
.div24
        _DIVADDSUB 3

IF OPTIMIZE_DIV16

.divadd16
        CLC
        BCC    div16

.divsub16
        SEC
.div16
        _DIVADDSUB 2

ENDIF

ENDIF

; ==================================================================================
; divadd subroutine
; ==================================================================================

.divadd
{
        LDA     divisor+3
IF INCLUDE_DIV40
        BNE     do40
ELSE
        BNE     maths_overflow
ENDIF

IF OPTIMIZE_DIV24
        LDA     divisor+2
        BNE     do32

IF OPTIMIZE_DIV16
        LDA     divisor+1
        BNE     do24
        JSR     divadd16   ; JSR rather than JMP to aid profiling
        RTS
.do24
ENDIF
        JSR     divadd24   ; JSR rather than JMP to aid profiling
        RTS
.do32
ENDIF
        JSR     divadd32   ; JSR rather than JMP to aid profiling
        RTS
IF INCLUDE_DIV40
.do40
        JSR     divadd40   ; JSR rather than JMP to aid profiling
        RTS
ENDIF
}

; ==================================================================================
; divsub subroutine
; ==================================================================================

.divsub
{
        LDA     divisor+3
IF INCLUDE_DIV40
        BNE     do40
ELSE
        BNE     maths_overflow
ENDIF

IF OPTIMIZE_DIV24
        LDA     divisor+2
        BNE     do32

IF OPTIMIZE_DIV16
        LDA     divisor+1
        BNE     do24
        JSR     divsub16   ; JSR rather than JMP to aid profiling
        RTS
.do24
ENDIF
        JSR     divsub24   ; JSR rather than JMP to aid profiling
        RTS
.do32
ENDIF
        JSR     divsub32   ; JSR rather than JMP to aid profiling
        RTS
IF INCLUDE_DIV40
.do40
        JSR     divsub40   ; JSR rather than JMP to aid profiling
        RTS
ENDIF
}

IF NOT(INCLUDE_DIV40)
.maths_overflow
        JSR     print_string
        EQUS    13, "Division overflow", 13
        LDX     saved_sp
        TXS
        RTS
ENDIF

; ==================================================================================
; Shared DIVINIT routine
; ==================================================================================

.divinit
       _DIVINIT

; ==================================================================================
; Main Program
; ==================================================================================

.spigot
        TSX
        STX     saved_sp

; sp_end is a static pointer to the MSB of SumP where digits will appear
        _ADD16  sp_end, sump, big
        _DEC16  sp_end

IF BELLARD
; F%=0
; T%=0
        _ZERO32 f
        _ZERO32 t
ELSE
; K%=0
        _ZERO32 k

ENDIF

; base=0 : L%=base : REM our bignums can get shorter as we go

         LDA    #0
         STA    lsb_fract
        _ZERO16 lsb_index

; M%=big-1 : REM leading zeros index for fast forward division

        _MOV16  msb_index, big
        _DEC16  msb_index

; Index of the first zero element of the numerator
        _MOV16  num_used_index, msb_index

; REPEAT

.spigot_loop

IF BELLARD

;   D%=T%+1
;   IF T% PROCdivaddsub(NOT(OP%))
;   D%=(T%+3)*4
;   PROCdivaddsub(OP%)
;   D%=(T%+5)*64
;   PROCdivaddsub(OP%)
;   D%=(T%+7)*64
;   PROCdivaddsub(OP%)
;   D%=(T%+9)*256
;   PROCdivaddsub(NOT(OP%))
;   D%=(F%+1)*8
;   PROCdivaddsub(OP%)
;   D%=(F%+3)*256
;   PROCdivaddsub(OP%)

        _TST32  t
        BEQ     skipfirst
        _ADD32C divisor, t, 1
        _SHL32  divisor, 0
        ALTERNATE_JSR divadd, divsub, f
.skipfirst
        _ADD32C divisor, t, 3
        _SHL32  divisor, 2
        ALTERNATE_JSR divsub, divadd, f
        _ADD32C divisor, t, 5
        _SHL32  divisor, 6
        ALTERNATE_JSR divsub, divadd, f
        _ADD32C divisor, t, 7
        _SHL32  divisor, 6
        ALTERNATE_JSR divsub, divadd, f
        _ADD32C divisor, t, 9
        _SHL32  divisor, 8
        ALTERNATE_JSR divadd, divsub, f
        _ADD32C divisor, f, 1
        _SHL32  divisor, 3
        ALTERNATE_JSR divsub, divadd, f
        _ADD32C divisor, f, 3
        _SHL32  divisor, 8
        ALTERNATE_JSR divsub, divadd, f

;  REM S is updated, now take three digits
;  IF K% PRINT;RIGHT$("000"+STR$(SumP!(big-1)),3);:ELSEPRINT;SumP!(big-1);
;  PROCmask(SumP) : REM remove those digits from S
;  PROCx1000(SumP)

        LDX     #0
        LDY     #0
        LDA     (sp_end),Y
        STA     temp
        TXA
        STA     (sp_end),Y
        INY
        LDA     (sp_end),Y
        STA     temp+1
        TXA
        STA     (sp_end),Y

;; Supress first two leading zeros
        _TST32  t
        BNE     p1
        LDA     temp
        ORA     #'0'
        JSR     print_digit
        JMP     p2
.p1
        JSR     print_decimal
.p2

        _MOV16  np, sump
        JSR     mult1000_little_endian ; uses np as the argument pointer

;   PROCrescale(NumeratorP)
        _MOV16  np, numeratorp
        JSR     rescale250256_big_endian ; uses np as the argument pointer

ELSE

;   D%=K%+1
;   IF NOT first PROCdivaddsub(TRUE)
;   first=FALSE
;   D%=D%+K%+7
;   PROCdivaddsub(FALSE)
;   D%=D%+2*K%+12
;   PROCdivaddsub(FALSE)
;   D%=D%+4
;   PROCdivaddsub(FALSE)
        _TST32  k
        BEQ     skipfirst
        _ADD32C divisor, k, 1
        _SHL32  divisor, 0
        JSR     divadd
.skipfirst
        _ADD32C divisor, k, 4
        _SHL32  divisor, 1
        JSR     divsub
        _ADD32C divisor, k, 5
        _SHL32  divisor, 2
        JSR     divsub
        _ADD32C divisor, k, 6
        _SHL32  divisor, 2
        JSR     divsub

;   PRINT CHR$(48+FNextract(SumP));
;   PROCmask(SumP) : REM remove that digit from S

        LDY     #0
        LDA     (sp_end),Y
        ORA     #48
        JSR     print_digit
        TYA
        STA     (sp_end),Y

;   PROCx10(SumP)
        _MOV16  np, sump
        JSR     mult10_little_endian ; uses np as the argument pointer

;   PROCrescale(NumeratorP)
        _MOV16  np, numeratorp
        JSR     mult10_big_endian ; uses np as the argument pointer
        _MOV16  np, numeratorp
        JSR     div16_big_endian  ; uses np as the argument pointer

ENDIF

IF VISUALIZE*0  ; update telltales in screen lines below the bignum
        _MOV16C np, DATA_START+LINE*2
        _ADD16  np, np, lsb_index
        LDY     #&00
        LDA     #&00
        STA     (np),Y
        _MOV16C np, DATA_START+LINE*2
        _ADD16  np, np, msb_index
        LDY     #&00
        LDA     #&FF
        STA     (np),Y
        _MOV16C np, DATA_END-LINE*2
        _SUB16  np, np, lsb_index
        LDY     #&00
        LDA     #&00
        STA     (np),Y
        _MOV16C np, DATA_END-LINE*2
        _SUB16  np, np, msb_index
        LDY     #&00
        LDA     #&FF
        STA     (np),Y
ENDIF

;   IF NumeratorP?M%=0 M%=M%-1
        _MOV16  np, numeratorp
        _SUB16  np, np, msb_index
        LDY     #0
        LDA     (np),Y
        BNE     num_not_zero
        _DEC16  msb_index
.num_not_zero

; Maintain a index for how much of the numerator is used (non zero)
        _MOV16  np, numeratorp
        _SUB16  np, np, num_used_index
        LDY     #1
        LDA     (np),Y
        BEQ     num_zero
        _TST16  num_used_index   ; Add a guard to prevent num_used_index going negative
        BEQ     num_zero         ; This can happen when big is very small (< 3)
        _DEC16  num_used_index
        INY
        LDA     #&00
        STA     (np),Y
        INY
        LDA     #&FF
        STA     (np),Y

.num_zero

;   IF base=base+106/256:L%=base

{
       CLC
       LDA      lsb_fract
       ADC      #(LSB_INC MOD 256)
       STA      lsb_fract
       LDA      lsb_index
       ADC      #(LSB_INC DIV 256)
       STA      lsb_index
       BCC      skip
       INC      lsb_index+1
.skip
}

IF BELLARD
;   F%=F%+4
;   T%=T%+10
        _ADD32C f, f, 4
        _ADD32C t, t, 10
ELSE
;   K%=K%+8
        _ADD32C k, k, 8
ENDIF

; UNTIL FALSE
; exit now happens in print_digit
        JMP     spigot_loop

.print_digit
{
IF TEST_MODE
        PHA
ELSE
        JSR     OSWRCH
ENDIF
        _ADD32C ndigits, ndigits, &FFFFFFFF
        _TST32  ndigits
        BNE     return
IF TEST_MODE
        PLA
        JSR     OSWRCH
ELSE
        LDA     #134            ; POS and VPOS
        JSR     OSBYTE
        TXA
        BEQ     skip_nl
        JSR     OSNEWL          ; unwanted if last digit is in right column
.skip_nl
ENDIF
        LDX     saved_sp
        TXS
IF TEST_MODE
        RTS
.return
        PLA
ELSE
.return
ENDIF
        RTS
}

IF BELLARD

.print_decimal
{
        LDY     #2
.Lp1
        LDX     #'0'-1
        SEC
.Lp2
        LDA     temp
        SBC     Tens,Y
        STA     temp
        LDA     temp+1
        SBC     #0
        STA     temp+1
        INX
        BCS     Lp2
        LDA     temp
        ADC     Tens,Y
        STA     temp
        BCC     skip
        INC     temp+1
.skip
        TXA
        JSR     print_digit
        DEY
        BPL     Lp1
        RTS
.Tens
        EQUB    1
        EQUB    10
        EQUB    100
}

carry2 = temp + 2

.mult1000_little_endian
{
        _ADD16  np_end, np, big   ; np_end is one beyond the last element of work
        _ADD16C np_end, np_end, 1 ; one extra bytes beyond the MSB
        _ADD16  np, np, lsb_index ; np is the first element of work
        _CMP16  np, np_end        ; range check up front to be safe
        BCC     ok
        RTS
.ok
        LDY     np      ; use Y as the LSB of the loop
        LDA     #0
        STA     carry   ; force carry byte to zero on first iteration
        STA     carry2
        LDA     np+1
        STA     oplda+2
        STA     opsta+2
        CLC
.loop
.oplda
        LDA     &AA00, Y ; operand is modified dynamically
        TAX
        LDA     mult1000_table, X
        ADC     carry
.opsta
        STA     &AA00, Y ; operand is modified dynamically
        LDA     mult1000_table+&100, X
        ADC     carry2
        STA     carry
        LDA     mult1000_table+&200, X
        ADC     #0
        STA     carry2
        INY
        BNE     compare
        INC     oplda+2
        INC     opsta+2
.compare
        ; An equailty comparison is cheaper, but needs a range check up front
        TYA
        EOR     np_end  ; need to preserve carry, so can't use CPY
        BNE     loop
        LDA     oplda+2
        EOR     np_end+1
        BNE     loop
        RTS
}

.rescale250256_big_endian
{
        _SUB16  np_end, np, big   ; np_end is one beyond the last element of work
        LDX     #np               ; np is the first element of work
        JSR     calc_num_lsb_address
        _CMP16  np_end, np        ; range check up front to be safe
        BCC     ok
        RTS
.ok
        LDY     #0
        LDA     (np), Y
        TAX
        LDA     mult250_table+&100, X
        STA     carry             ; fill the pipeline
        CLC
        LDY     np                ; use Y as the LSB of the loop
        LDA     np+1
        STA     oplda+2
        STA     opsta+2
        _DEC16  np_end
        JMP     next
.loop
.oplda
        LDA     &AA00, Y          ; operand is modified dynamically
        TAX
        LDA     mult250_table, X
        ADC     carry             ; C=1 from this add will be handled next time around
.opsta
        STA     &AA01, Y          ; operand is modified dynamically
        LDA     mult250_table+&100, X
        STA     carry
.next
        TYA
        BNE     compare
        DEC     oplda+2
        DEC     opsta+2
IF PITUBE_JIT_FIX
        DEC     oplda+2
        DEC     opsta+2
        INC     oplda+2
        INC     opsta+2
ENDIF
.compare
        DEY
        TYA
        EOR     np_end            ; need to preserve carry, so can't use CPY
        BNE     loop
        LDA     oplda+2
        EOR     np_end+1
        BNE     loop
        RTS
}

ELSE

.mult10_little_endian
        _MULTIPLY_LITTLE_ENDIAN  mult10_table

.mult10_big_endian
        _MULTIPLY_BIG_ENDIAN  mult10_table

; DEF PROCdiv16(BignumP)
;   LOCAL carry, temp
;   carry=0
;   FOR I%=big-1 TO L% STEP -1
;     temp=BignumP?I%
;     BignumP?I% = (BignumP?I%) DIV 16 + (carry * 16)
;     carry = temp AND 15
;   NEXT
; ENDPROC

.div16_big_endian
{
        LDX     #np_end
        JSR     calc_num_lsb_address
        _INC16  np_end            ; np_end is one beyond the last element of work
        _SUB16  np, np, big
        _INC16  np                ; np is thefirst element of work
        _CMP16  np_end, np        ; range check up front to be safe
        BCS     ok
        RTS
.ok
        LDY     np
        LDA     #0
        STA     carry
        STA     np
.loop
        LDA     (np), Y
        TAX
        LDA     div16_table_msb,X
        ORA     carry
        STA     (np), Y
        LDA     div16_table_lsb,X
        STA     carry
        INY
        BNE     skip
        INC     np+1
.skip
        ; An equailty comparison is cheaper, but needs a range check up front
        CPY     np_end
        BNE     loop
        LDA     np+1
        CMP     np_end+1
        BNE     loop
        RTS
}

ENDIF

; Calculate the address of the LSB of the numerator, using which ever
; of lsb_index or num_used_index is the larger.
; Result written to 0,X and 1,X
;
; Note: num_used_index currently indicates the final non-zero byte of
; the numerator. When returning this, it's important to include the
; follwoing zero, to provide space for the rescale code to extend the
; numerator towards the LSB.
;
; More specifically, this function is used in three places:
; BBP: mult10_big_endian, needs no extra bytes
; BBP: div16_big_endian, needs one extra byte (to extend into)
; BEL: rescale250256_big_endian, needs one extra byte (to extend into)
.calc_num_lsb_address
{
        _MOV16  temp, lsb_index
        _CMP16  lsb_index, num_used_index
        BCS     skip
        _SUB16C temp, num_used_index, 1
.skip
        SEC
        LDA     np
        SBC     temp
        STA     0,X
        LDA     np+1
        SBC     temp+1
        STA     1,X
        RTS
}

IF DEBUG

.hex16
{
        LDA     1, X
        JSR     hexout
        LDA     0, X
        JSR     hexout
        JMP     OSNEWL
}

.hex32
{
        LDA     3, X
        JSR     hexout
        LDA     2, X
        JSR     hexout
        LDA     1, X
        JSR     hexout
        LDA     0, X
        JSR     hexout
        JMP     OSNEWL
}

.hexp
{
        LDY     #3
.loop
        LDA     (np), Y
        JSR     hexout
        DEY
        BPL     loop
        JMP     OSNEWL
}

.hexout
{
        PHA
        LSR     A
        LSR     A
        LSR     A
        LSR     A
        JSR     hex1
        PLA
.hex1
        AND     #&0F
        CMP     #&0A
        BCC     hex2
        ADC     #&06
.hex2
        ADC     #&30
        JMP     OSWRCH
}

ENDIF
