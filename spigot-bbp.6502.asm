; Globals:
;   NumeratorP, SumP are bignums
;   SumP is a bignum to add or subtract into
;   D% is an integer, the number to divide by
;   M% is the MSB index into NumeratorP
;   L% is the LSB index into NumeratorP
; Parameters:
;   C% is TRUE for add, FALSE for subtract
; Locals:
;   I% byte loop counter (M% to L%)
;   J% bit loop counter (0 TO 7)
;   T% is a 4-byte temporary numerator
;   B% is the byte being accumulated
;
; --------------------------------------------------
;
; DEF PROCdivaddsub(C%)

; Parameters

numeratorp = &70 ; 2 byte pointer to numerator
      sump = &72 ; 2 byte pointer to sum
 msb_index = &74 ; 2 byte index into numerator/sum
 lsb_index = &76 ; 2 byte index into numerator/sum
   divisor = &78 ; 4 bytes / 32 bit unsigned integer

; Working registers

        np = &7C ; 2 byte pointer to numerator
    np_end = &7E ; 2 byte pointer to the terminating position in numerator
        sp = &80 ; 2 byte pointer to sum
      temp = &82 ; 4 bytes / 32 bit unsigned integer
      byte = &86 ; 1 byte

MACRO DIVADDSUB op

;   T%=0
        LDA #0
        STA temp+0
        STA temp+1
        STA temp+2
        STA temp+3


;   FOR I%=M% TO L% STEP -1

        CLC
        LDA numeratorp
        ADC msb_index
        STA np
        LDA numeratorp+1
        ADC msb_index+1
        STA np+1

        CLC
        LDA numeratorp
        ADC lsb_index
        STA np_end
        LDA numeratorp+1
        ADC lsb_index+1
        STA np_end+1

        CLC
        LDA sump
        ADC msb_index
        STA sp
        LDA sump+1
        ADC msb_index+1
        STA sp+1

.byte_loop

;       T%=T%*256+NumeratorP?I%
        LDA temp+2
        STA temp+3
        LDA temp+1
        STA temp+2
        LDA temp+0
        STA temp+1
        LDY #0
        LDA (np),Y
        STA temp+0

;       B%=0
;       not needed as we shift a byte 8 times

;       D%=D%*256
        LDA divisor+2
        STA divisor+3
        LDA divisor+1
        STA divisor+2
        LDA divisor+0
        STA divisor+1
        LDA #0
        STA divisor+0

;       FOR J%=0 TO 7
        LDX #7
.bit_loop

;       B%=B%*2
        ASL byte

;       D%=D% DIV 2
        LSR divisor+3
        ROR divisor+2
        ROR divisor+1
        ROR divisor+0

;       IF T%>=D%
        LDA divisor+3
        CMP temp+3
        BCC do_subtract
        BNE bit_loop_next
        LDA divisor+2
        CMP temp+2
        BCC do_subtract
        BNE bit_loop_next
        LDA divisor+1
        CMP temp+1
        BCC do_subtract
        BNE bit_loop_next
        LDA divisor+0
        CMP temp+0
        BCC do_subtract
        BNE bit_loop_next

;       T%=T%-D%:B%=B%+1
.do_subtract
        SEC
        LDA temp+0
        SBC divisor+0
        STA temp+0
        LDA temp+1
        SBC divisor+1
        STA temp+1
        LDA temp+2
        SBC divisor+2
        STA temp+2
        LDA temp+3
        SBC divisor+3   ; Divisor will not be full 32 bits
        STA temp+3

        INC byte

;       NEXT J%
.bit_loop_next
        DEX
        BPL bit_loop


;     IF B%=0 NEXT:ENDPROC
        LDA byte
        BEQ byte_loop_next

;     IF C% SumP!I%=S%+B%:ELSE SumP!I%=S%-B%

IF (op)
        ; Add byte
        CLC
        LDY #0
        ADC (sp),Y
        STA (sp),Y
        BCC byte_loop_next
.cloop
        INY
        LDA #0
        ADC (sp),Y
        STA (sp),Y
        BCS cloop
ELSE
        ; Subtract byte
        SEC
        LDY #0
        LDA (sp),Y
        SBC byte
        STA (sp),Y
        BCS byte_loop_next
.cloop
        INY
        LDA (sp),Y
        SBC #0
        STA (sp),Y
        BCC cloop
ENDIF

;       NEXT

.byte_loop_next
        LDA np
        CMP np_end
        LDA np+1
        SBC np_end+1
        BCS byte_loop_next2

; ENDPROC
        RTS

.byte_loop_next2

        LDA np
        BNE np_not_zero
        DEC np+1
.np_not_zero
        DEC np

        LDA sp
        BNE sp_not_zero
        DEC sp+1
.sp_not_zero
        DEC sp

        JMP byte_loop

ENDMACRO


        ORG &7000

.code_start

        JMP divadd
        JMP divsub

.divadd
        DIVADDSUB TRUE

.divsub
        DIVADDSUB FALSE

.code_end

SAVE "SPIGMC", code_start, code_end
