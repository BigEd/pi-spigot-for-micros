OSWRCH = &FFEE
OSNEWL = &FFE7

; Optimization options, which add lots of extra code

; Include versions of DIVADD/SUB with 24-bit maths for small divisors
; Bellard 1000 runs 17.9% faster
;     BBP 1000 runs 28.0% faster
OPTIMIZE_DIV24 = TRUE

; Include versions of DIVADD/SUB with 16-bit maths for very small divisors
; Bellard 1000 runs 1.0% faster
;     BBP 1000 runs 1.0% faster
; (this depends on OPTIMIZE_DIV24 being TRUE and is ignored otherwise)
OPTIMIZE_DIV16 = TRUE

; Optimize implemenation of shifts where the divisor LSB ends up as zero
; Bellard 1000 runs 1.7% faster
;     BBP 1000 runs 1.1% slower
; (this has most benefit when OPTIMIZE_DIV24 and OPTIMIZE_DIV16 are TRUE)
OPTIMIZE_SHIFT = BELLARD

DEBUG = FALSE

; Parameters

numeratorp = &70 ; 2 byte pointer to numerator bignum
      sump = &72 ; 2 byte pointer to sum bignum
       big = &74 ; 2-byte size of bignums
   ndigits = &76 ; 4-byte number of digits (4 bytes to allow for *8)

; Working registers (zero page &50-&6F is unused by BASIC)

        np = &50 ; 2 byte pointer to numerator
    np_end = &52 ; 2 byte pointer to the terminating position in numerator
        sp = &54 ; 2 byte pointer to sum
    sp_end = &56 ; 2 byte pointer to the MSB of sum where a digit is produced
      byte = &58 ; 1 byte
 lsb_fract = &59 ; 1 byte
 lsb_index = &5A ; 2 byte index into numerator/sum
 msb_index = &5C ; 2 byte index into numerator/sum
  saved_sp = &5E ; 1 byte saved BASIC stack pointer
    offset = &5F ; 1 byte offset into SumP array (deals with shift of 8)

   divisor = &60 ; 4 bytes / 32 bit unsigned integer
      temp = &64 ; 4 bytes / 32 bit unsigned integer
IF BELLARD
   LSB_INC = 106*3
         f = &68 ; 4 bytes / 32 bit unsigned integer
         t = &6c ; 4 bytes / 32 bit unsigned integer
ELSE
   LSB_INC = 106
         k = &68 ; 4 bytes / 32 bit unsigned integer
ENDIF

; ==================================================================================
; Code origin
; ==================================================================================

        ORG &6000
        GUARD &7C00

.code_start

; ==================================================================================
; 16-bit MACROS
; ==================================================================================

MACRO _INC16 result
        INC result
        BNE skip
        INC result+1
.skip
ENDMACRO

MACRO _DEC16 result
        LDA result
        BNE skip
        DEC result+1
.skip
        DEC result
ENDMACRO

MACRO _CMP16 arg1, arg2  ; C=1 if arg1 >= arg2
        LDA arg1
        CMP arg2
        LDA arg1+1
        SBC arg2+1
ENDMACRO

MACRO _ZERO16 result
        LDA #0
FOR I,0,1
        STA result+I
NEXT
ENDMACRO

MACRO _MOV16 result, arg1
FOR I,0,1
        LDA arg1+I
        STA result+I
NEXT
ENDMACRO

MACRO _ADD16 result, arg1, arg2
        CLC
FOR I,0,1
        LDA arg1+I
        ADC arg2+I
        STA result+I
NEXT
ENDMACRO

MACRO _ADD16C result, arg1, arg2
        CLC
FOR I,0,1
        LDA arg1+I
        ADC #((arg2 >> (I*8)) AND &FF)
        STA result+I
NEXT
ENDMACRO

; ==================================================================================
; 32-bit MACROS
; ==================================================================================

MACRO _TST32 arg
        LDA arg
        ORA arg+1
        ORA arg+2
        ORA arg+3
ENDMACRO

MACRO _CMP32 arg1, arg2
        LDA arg1
        CMP arg2
FOR I,1,3
        LDA arg1+I
        SBC arg2+I
NEXT
ENDMACRO

MACRO _ZERO32 result
        LDA #0
FOR I,0,3
        STA result+I
NEXT
ENDMACRO

MACRO _ADD32 result, arg1, arg2
        CLC
FOR I,0,3
        LDA arg1+I
        ADC arg2+I
        STA result+I
NEXT
ENDMACRO

MACRO _ADD32C result, arg1, arg2
        CLC
FOR I,0,3
        LDA arg1+I
        ADC #((arg2 >> (I*8)) AND &FF)
        STA result+I
NEXT
ENDMACRO

; After the shift, if the least significant byte of the divisor is
; zero, then we can handle this case more efficiently. This will be
; true if there 8-count zero bits at the least significant end of the
; divisor.

IF OPTIMIZE_SHIFT

MACRO _SHL32 result, count
IF count=0
        LDX #1
ELIF count=8
        LDX #0
ELSE
        LDA #((1<<(8-count))-1)  ; a bit mask of 8-count ones
        BIT result               ; AND with LSB of result
        BNE slow                 ; Z=1 if all bits are zero
        LDX #8-count
.shr_loop
        LSR result+3
        ROR result+2
        ROR result+1
        ROR result
        DEX
        BNE shr_loop
        BEQ done                ; X=0 -> offset
.slow
        LDX #count
.shl_loop
        ASL result
        ROL result+1
        ROL result+2
        ROL result+3
        DEX
        BNE shl_loop
        INX                     ; X=1 -> offset
ENDIF
.done
        STX offset
ENDMACRO

ELSE

MACRO _SHL32 result, count
IF count=0
ELIF count=8
        LDA result+2
        STA result+3
        LDA result+1
        STA result+2
        LDA result+0
        STA result+1
        LDA #0
        STA result+0
ELSE
        LDX #count
.loop
        ASL result+0
        ROL result+1
        ROL result+2
        ROL result+3
        DEX
        BNE loop
ENDIF
ENDMACRO

ENDIF

; ==================================================================================
; TABLE MULTIPLY MACRO
; ==================================================================================

; DEF PROCx10(BignumP)
;   REM bignum multiply by small number
;   REM two byte result from each digit demands a carry
;   LOCAL carry, temp
;   carry=0
;   FOR I%=L% TO big-1
;     temp=(BignumP?I%)*10 + carry
;     BignumP?I% = temp AND 255
;     carry = temp DIV 256
;   NEXT
; ENDPROC

carry = temp + 1

MACRO _MULTIPLY table, extra
        _ADD16  np_end, np, big   ; np_end is one beyond the last element of work
IF extra
        _ADD16C np_end, np_end, extra ; any extra bytes beyond the MSB?
ENDIF
        _ADD16  np, np, lsb_index ; np is the first element of work
        _CMP16  np, np_end        ; range check up front to be safe
        BCC     ok
        RTS
.ok
        LDY     np      ; use Y as the LSB of the loop
        LDA     #0
        STA     carry   ; force carry byte to zero on first iteration
        LDA     np+1
        STA     oplda+2
        STA     opsta+2
        CLC
.loop
.oplda
        LDA     &AA00, Y ; operand is modified dynamically
        TAX
        LDA     table, X
        ADC     carry   ; C=1 from this add will be handled next time around
.opsta
        STA     &AA00, Y ; operand is modified dynamically
        LDA     table+&100, X
        STA     carry
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
ENDMACRO

; ==================================================================================
; DIVADDSUB MACRO
; ==================================================================================

; Macro Parameters:
;   op is TRUE for add, FALSE for subtract
;
; Parameters:
;   numeratorp is a bignum holding the numerator
;   sump is a bignum to add or subtract into
;   divisor (a 32-bit integer) is the number to divide by
;   msb_index (a 16-bit integer) is the MSB index into NumeratorP, SumP
;   lsb_index (a 16-bit integer) is the LSB index into NumeratorP, SumP
;
;
; DEF PROCdivaddsub(C%)

; Factor out divide initialization as a common subroutine to save code space
; (it's a macro that's only instantiated once)

MACRO _DIVINIT

;   T%=0
        _ZERO32 temp

;   FOR I%=M% TO L% STEP -1

        _ADD16  np, numeratorp, msb_index
        _ADD16  np_end, numeratorp, lsb_index

        _CMP16  np, np_end  ; C=1 if arg1 >= arg2
        BCS     work_to_do
        PLA
        PLA
        RTS
.work_to_do
        _ADD16  sp, sump, msb_index
IF OPTIMIZE_SHIFT
        LDA     offset
        BNE     skip_adjust
        _ADD16C sp, sp, &FFFF
.skip_adjust
ENDIF
        LDY     #0
        RTS

ENDMACRO

MACRO _DIVADDSUB bytes,op

; This calls the _DIVINIT code as a subroutine
        JSR     divinit

;   Scary self-modifying code to update the LDX #&xx and SBC #&xx operands
FOR i,bytes-1,1,-1
        LDA     divisor+i-1
        STA     divisor+i
NEXT
        STY     divisor+0    ; Y is a constant 0
FOR j,0,7
        LSR     divisor+bytes-1
FOR i,bytes-2,0,-1
        ROR     divisor+i
NEXT
FOR i,bytes-1,0,-1
        ; Modify the LDX #
        LDA     divisor+i
        STA     modify + j*(bytes*14+5)+(bytes-1-i)*8+1
NEXT
        ; Cunning optimization:
        ;   SBC # literal is divisor-1 which allows an SEC to be
        ;   be eliminated from the inner loop, which is a 2% speed-up
        CLC
FOR i,0,bytes-1
        ; Modify the SBC #
        LDA     divisor+i
        SBC     #0
        STA     modify + j*(bytes*14+5)+i*6+bytes*8+5
NEXT
NEXT

.byte_loop

;       T%=T%*256+NumeratorP?I%
FOR i,bytes-1,1,-1
        LDA     temp+i-1
        STA     temp+i
NEXT
        LDA     (np),Y
        STA     temp+0

;       A will be used to accumulate the 8 result bits
        LDA     #0

.modify

;       Unroll the bit loop
;       FOR J%=0 TO 7
FOR j,0,7

;       IF T%>=D%
FOR i,bytes-1,0,-1
        LDX     #&00         ; operand is modified dynamically
        CPX     temp+i       ; X is used so as not to corrupt A
        BCC     do_subtract
        BNE     bit_loop_next
NEXT
        CLC
;       T%=T%-D%:B%=B%+1
.do_subtract
        TAX                  ; save A
FOR i,0,bytes-1
        LDA     temp+i
        SBC     #&00         ; operand is modified dynamically
        STA     temp+i
NEXT
        TXA                  ; restore A
        ORA     #(&80 >> j)

;       NEXT J%
.bit_loop_next
NEXT

;     IF B%=0 NEXT:ENDPROC
;        CMP     #0
;        BEQ     byte_loop_next

;     IF C% SumP!I%=S%+B%:ELSE SumP!I%=S%-B%

IF (op)
        ; Add byte
        CLC
        ADC     (sp),Y
        STA     (sp),Y
        BCC     byte_loop_next
.cloop
        INY
        LDA     #0
        ADC     (sp),Y
        STA     (sp),Y
        BCS     cloop
ELSE
        ; Subtract byte
        ; SEC not needed because it's already set (assuming no arithmetic overflow)
        STA     byte
        LDA     (sp),Y
        SBC     byte
        STA     (sp),Y
        BCS     byte_loop_next
.cloop
        INY
        LDA     (sp),Y
        SBC     #0
        STA     (sp),Y
        BCC     cloop
ENDIF
        LDY     #0
;       NEXT

.byte_loop_next
        ; Check the LSB first, optimize for branch not taken
        LDA     np
        CMP     np_end
        BEQ     byte_loop_done

.byte_loop_more
        _DEC16  np
        _DEC16  sp
        JMP     byte_loop

.byte_loop_done
        ; Check the MSB
        LDA     np+1
        CMP     np_end+1
        BNE     byte_loop_more
        RTS

ENDMACRO

; =============================================================
; MAIN PROGRAM
; =============================================================

; Start with a jump block to aid testing
JMP spigot ; +0000

; Embed a title into the machine code
IF BELLARD
EQUS "Bellard Pi Spigot"
ELSE
EQUS "BBP Pi Spigot"
ENDIF
EQUB 13

.spigot
        TSX
        STX     saved_sp

; sp_end is a static pointer to the MSB of SumP where digits will appear
        _ADD16  sp_end, sump, big
        _ADD16C sp_end, sp_end, &FFFF

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
        _ADD16C msb_index, msb_index, &FFFF

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
        JSR     divadd
.skipfirst
        _ADD32C divisor, t, 3
        _SHL32  divisor, 2
        JSR     divsub
        _ADD32C divisor, t, 5
        _SHL32  divisor, 6
        JSR     divsub
        _ADD32C divisor, t, 7
        _SHL32  divisor, 6
        JSR     divsub
        _ADD32C divisor, t, 9
        _SHL32  divisor, 8
        JSR     divadd
        _ADD32C divisor, f, 1
        _SHL32  divisor, 3
        JSR     divsub
        _ADD32C divisor, f, 3
        _SHL32  divisor, 8
        JSR     divsub

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
        JSR     mult4 ; uses np as the argument pointer
        _MOV16  np, sump
        JSR     mult250 ; uses np as the argument pointer

;   PROCrescale(NumeratorP)
        _MOV16  np, numeratorp
        JSR     mult250 ; uses np as the argument pointer
        _MOV16  np, numeratorp
        JSR     div256  ; uses np as the argument pointer

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
        JSR     mult10 ; uses np as the argument pointer

;   PROCrescale(NumeratorP)
        _MOV16  np, numeratorp
        JSR     mult10 ; uses np as the argument pointer
        _MOV16  np, numeratorp
        JSR     div16  ; uses np as the argument pointer

ENDIF

;   IF NumeratorP?M%=0 M%=M%-1
        _MOV16  np, numeratorp
        _ADD16  np, np, msb_index
        LDY     #0
        LDA     (np),Y
        BNE     num_not_zero
        _ADD16C msb_index, msb_index, &FFFF
.num_not_zero

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

; ==================================================================================
; DIVADD routines
; ==================================================================================

.divinit
       _DIVINIT

.divadd
IF BELLARD
        LDA     f
        AND     #&04
        BEQ     divadd1
        JMP     divsub1
.divadd1
ENDIF

IF OPTIMIZE_DIV24
        LDA     divisor+2
        BEQ     divadd2
        JMP     divadd32
.divadd2

IF OPTIMIZE_DIV16
        LDA     divisor+1
        BEQ     divadd16
        JMP     divadd24
.divadd16
        _DIVADDSUB 2, TRUE
ENDIF

.divadd24
        _DIVADDSUB 3, TRUE

ENDIF

.divadd32
        _DIVADDSUB 4, TRUE

; ==================================================================================
; DIVSUB routines
; ==================================================================================

.divsub
IF BELLARD
        LDA     f
        AND     #&04
        BEQ     divsub1
        JMP     divadd1
.divsub1
ENDIF

IF OPTIMIZE_DIV24
        LDA     divisor+2
        BEQ     divsub2
        JMP     divsub32
.divsub2

IF OPTIMIZE_DIV16
        LDA     divisor+1
        BEQ     divsub16
        JMP     divsub24

.divsub16
        _DIVADDSUB 2, FALSE
ENDIF

.divsub24
        _DIVADDSUB 3, FALSE
ENDIF

.divsub32
        _DIVADDSUB 4, FALSE

.print_digit
{
        JSR     OSWRCH
        _ADD32C ndigits, ndigits, &FFFFFFFF
        _TST32  ndigits
        BNE     return
        JSR     OSNEWL
        LDX     saved_sp
        TXS
.return
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

.mult4
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
        LDA     np+1
        STA     oplda+2
        STA     opsta+2
        CLC
.loop
.oplda
        LDA     &AA00, Y   ; operand is modified dynamically
        TAX
        ASL     A
        ASL     A
        ORA     carry
.opsta
        STA     &AA00, Y   ; operand is modified dynamically
        LDA     div64_table, X
        STA     carry
        INY
        BNE     compare
        INC     oplda+2
        INC     opsta+2
.compare
        CPY     np_end
        BNE     loop
        LDA     oplda+2
        CMP     np_end+1
        BNE     loop
        RTS

.mult250
        _MULTIPLY mult250_table,1

;  FOR I%=L% TO big-1
;    BignumP?I%=BignumP?(I%+1)
;  NEXT
;  BignumP?big=0

.div256
{
        _ADD16  np_end, np, big
        _ADD16  np, np, lsb_index
        _CMP16  np, np_end        ; range check up front to be safe
        BCC     ok
        RTS
.ok
        LDY     np
        LDA     np+1
        STA     oplda+2
        STA     opsta+2
.loop
.oplda
        LDA     &AA01, Y
.opsta
        STA     &AA00, Y
        INY
        BNE     compare
        INC     oplda+2
        INC     opsta+2
.compare
        CPY     np_end
        BNE     loop
        LDA     oplda+2
        CMP     np_end+1
        BNE     loop
        LDA     #0
        TAY
        STA     (np_end),Y
        RTS
}

ALIGN &100

.div64_table
FOR I,0,255
EQUB I DIV &40
NEXT

.mult250_table
FOR I,0,255
EQUB (I*250) MOD &100
NEXT
FOR I,0,255
EQUB (I*250) DIV &100
NEXT

ELSE

.mult10
        _MULTIPLY  mult10_table, 0

; DEF PROCdiv16(BignumP)
;   LOCAL carry, temp
;   carry=0
;   FOR I%=big-1 TO L% STEP -1
;     temp=BignumP?I%
;     BignumP?I% = (BignumP?I%) DIV 16 + (carry * 16)
;     carry = temp AND 15
;   NEXT
; ENDPROC

.div16
{
        _ADD16  np_end, np, lsb_index
        _DEC16  np_end            ; np_end is one beyond the last element of work
        _ADD16  np, np, big
        _DEC16  np                ; np is thefirst element of work
        _CMP16  np, np_end        ; range check up front to be safe
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
        CPY     #0
        BNE     skip
        DEC     np+1
.skip
        DEY
        ; An equailty comparison is cheaper, but needs a range check up front
        CPY     np_end
        BNE     loop
        LDA     np+1
        CMP     np_end+1
        BNE     loop
        RTS
}

ALIGN &100

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

.code_end

PUTTEXT "boot", "!BOOT", 0000
SAVE "SPIGMC", code_start, code_end
PUTBASIC "spigot-common.6502.basic.txt", "SPIG"

IF BELLARD
PUTBASIC "spigot-bellard.basic.txt", "SPIGBAS"
ELSE
PUTBASIC "spigot-bbp.basic.txt", "SPIGBAS"
ENDIF
