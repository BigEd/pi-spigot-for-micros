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

; self-modify the comparison at the end of the loop
        LDA     np_end
        STA     byte_loop_next+3
;   Scary self-modifying code to update the LDX #&xx and SBC #&xx operands
FOR j,7,0,-1
; Code offset to the Jth division bit slice compare block
COMPARE_J  = j*(bytes*14+5)
; Code offset to the Jth division bit slice subtract block
SUBTRACT_J = COMPARE_J + bytes*8
FOR i,bytes-1,0,-1
        ; Modify the LDX #
        LDA     divisor+i
        STA     modify + COMPARE_J + (bytes-1-i)*8 + 1
NEXT
        ; Cunning optimization:
        ;   SBC # literal is divisor-1 which allows an SEC to be
        ;   be eliminated from the inner loop, which is a 2% speed-up
        CLC
FOR i,0,bytes-1
        ; Modify the SBC #
        LDA     divisor+i
        SBC     #0
        STA     modify + SUBTRACT_J + i*6 + 5
NEXT
        ASL     divisor+0
FOR i,1,bytes-1
        ROL     divisor+i
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
        CMP     #np_end
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

MACRO ALTERNATE_JSR fn1,fn2,c
        LDA     c
        AND     #&04
        BNE     l1
        JSR     fn1
        JMP     l2
.l1
        JSR     fn2
.l2
ENDMACRO
