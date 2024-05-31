OSWRCH = &FFEE
OSNEWL = &FFE7

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
                 ; 2 bytes spare

   divisor = &60 ; 4 bytes / 32 bit unsigned integer
        k  = &64 ; 4 bytes / 32 bit unsigned integer
      temp = &68 ; 4 bytes / 32 bit unsigned integer

; ==================================================================================
; Code origin
; ==================================================================================

        ORG &7000

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

MACRO _MOV16C result, arg1
FOR I,0,1
        LDA #((arg1 >> (I*8)) AND &FF)
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

MACRO _MOV32C result, arg1
FOR I,0,3
        LDA #((arg1 >> (I*8)) AND &FF)
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

MACRO _DIVADDSUB op

;   T%=0
        _MOV32C temp, 0

;   FOR I%=M% TO L% STEP -1

        _ADD16  np, numeratorp, msb_index
        _ADD16  np_end, numeratorp, lsb_index
        _ADD16  sp, sump, msb_index

.byte_loop

;       T%=T%*256+NumeratorP?I%
        LDA     temp+2
        STA     temp+3
        LDA     temp+1
        STA     temp+2
        LDA     temp+0
        STA     temp+1
        LDY     #0
        LDA     (np),Y
        STA     temp+0

;       B%=0
;       not needed as we shift a byte 8 times

;       D%=D%*256
        LDA     divisor+2
        STA     divisor+3
        LDA     divisor+1
        STA     divisor+2
        LDA     divisor+0
        STA     divisor+1
        LDA     #0
        STA     divisor+0

;       FOR J%=0 TO 7
        LDX     #7
.bit_loop

;       B%=B%*2
        ASL     byte

;       D%=D% DIV 2
        LSR     divisor+3
        ROR     divisor+2
        ROR     divisor+1
        ROR     divisor+0

;       IF T%>=D%
        LDA     divisor+3
        CMP     temp+3
        BCC     do_subtract
        BNE     bit_loop_next
        LDA     divisor+2
        CMP     temp+2
        BCC     do_subtract
        BNE     bit_loop_next
        LDA     divisor+1
        CMP     temp+1
        BCC     do_subtract
        BNE     bit_loop_next
        LDA     divisor+0
        CMP     temp+0
        BCC     do_subtract
        BNE     bit_loop_next

;       T%=T%-D%:B%=B%+1
.do_subtract
        SEC
        LDA     temp+0
        SBC     divisor+0
        STA     temp+0
        LDA     temp+1
        SBC     divisor+1
        STA     temp+1
        LDA     temp+2
        SBC     divisor+2
        STA     temp+2
        LDA     temp+3
        SBC     divisor+3   ; Divisor will not be full 32 bits
        STA     temp+3

        INC     byte

;       NEXT J%
.bit_loop_next
        DEX
        BPL     bit_loop

;     IF B%=0 NEXT:ENDPROC
        LDA     byte
        BEQ     byte_loop_next

;     IF C% SumP!I%=S%+B%:ELSE SumP!I%=S%-B%

IF (op)
        ; Add byte
        CLC
        LDY     #0
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
        SEC
        LDY     #0
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

;       NEXT

.byte_loop_next
        _CMP16  np, np_end  ; C=1 if arg1 >= arg2
        BCS     byte_loop_next2

; ENDPROC
        RTS

.byte_loop_next2

        _DEC16  np
        _DEC16  sp

        JMP     byte_loop

ENDMACRO

; =============================================================
; MAIN PROGRAM
; =============================================================

; Start with a jump block to aid testing

JMP spigot ; +0000
JMP divadd ; +0003
JMP divsub ; +0006
JMP mult10 ; +0009
JMP div16  ; +000C

.spigot
; sp_end is a static pointer to the MSB of SumP where digits will appear
        _ADD16  sp_end, sump, big
        _ADD16C sp_end, sp_end, &FFFF

; K%=0
        _MOV32C k, 0

; base=0 : L%=base : REM our bignums can get shorter as we go

         LDA #0
         STA lsb_fract
        _MOV16C lsb_index, 0

; M%=big-1 : REM leading zeros index for fast forward division

        _MOV16  msb_index, big
        _ADD16C msb_index, msb_index, &FFFF

; ndigits *= 8 to allow direct comparison with K
FOR I,0,2
        ASL     ndigits
        ROL     ndigits+1
        ROL     ndigits+2
        ROL     ndigits+3
NEXT

; REPEAT

.spigot_loop

;   D%=K%+1
;   IF NOT first PROCdivaddsub(TRUE)
;   first=FALSE
;   D%=D%+K%+7
;   PROCdivaddsub(FALSE)
;   D%=D%+2*K%+12
;   PROCdivaddsub(FALSE)
;   D%=D%+4
;   PROCdivaddsub(FALSE)
;   K%=K%+8

        _ADD32C divisor, k, 1
        _TST32  k
        BEQ     skipfirst
        JSR     divadd
.skipfirst
        _ADD32  divisor, divisor, k
        _ADD32C divisor, divisor, 7
        JSR     divsub
        _ADD32  divisor, divisor, k
        _ADD32  divisor, divisor, k
        _ADD32C divisor, divisor, 12
        JSR     divsub
        _ADD32C divisor, divisor, 4
        JSR     divsub
        _ADD32C k, k, 8

;   PRINT CHR$(48+FNextract(SumP));
;   PROCmask(SumP) : REM remove that digit from S

        LDY     #0
        LDA     (sp_end),Y
        ORA     #48
        JSR     OSWRCH
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

;   IF NumeratorP?M%=0 M%=M%-1
        _MOV16  np, numeratorp
        _ADD16  np, np, msb_index
        LDY     #0
        LDA     (np),Y
        BNE     num_not_zero
        _ADD16C msb_index, msb_index, &FFFF
.num_not_zero

;   IF K%>24 base=base+106/256:L%=base

{
       LDA      k+1
       ORA      k+2
       ORA      k+3
       BNE      update
       LDA      k
       CMP      #25
       BCC      skip
.update
       CLC
       LDA      lsb_fract
       ADC      #106
       STA      lsb_fract
       BCC      skip
       _INC16   lsb_index
.skip
}

; UNTIL K%>=8*digits
        _CMP32  k, ndigits ; C=1 if arg1 >= arg2
        BCS     done
        JMP     spigot_loop

.done
        JMP OSNEWL

.divadd
        _DIVADDSUB TRUE

.divsub
        _DIVADDSUB FALSE

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
;
; Called with np pointing to either SumP or NumeratorP

carry = temp + 1

.mult10
{
        _ADD16  np_end, np, big
        _ADD16  np, np, lsb_index

        LDY    #0
        STY    carry   ; force carry byte to zero on first iteration
.loop
        STY    temp    ; MSB 8 bits of *10
        LDA    (np), Y ; bitnum byte
        ASL    A
        ROL    temp    ; temp, A = bignum * 2
        ASL    A
        ROL    temp    ; temp, A = bignum * 4
        ADC    (np), Y
        BCC    nc
        INC    temp    ; temp, A = bignum * 5
.nc     ASL    A
        ROL    temp    ; temp, A = bignum * 10
        CLC
        ADC    carry
        STA    (np),Y  ; store LSB back to bignum
        TYA
        ADC    temp
        STA    carry

        _INC16 np

        LDA    np
        CMP    np_end
        BNE    loop
        LDA    np+1
        CMP    np_end+1
        BNE    loop
        RTS
}


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
        _ADD16  np, np, big
        _DEC16  np

        LDY     #0
        STY     carry
.loop
        LDA     (np), Y
        LSR     A
        ROR     temp
        LSR     A
        ROR     temp
        LSR     A
        ROR     temp
        LSR     A
        ROR     temp
        ORA     carry
        STA     (np), Y
        LDA     temp
        AND     #&F0
        STA     carry
        _DEC16  np
        _CMP16  np, np_end ; C=1 if arg1 >= arg2
        BCS     loop
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
        JMP    OSWRCH
}

ENDIF

.code_end

SAVE "SPIGMC", code_start, code_end
PUTTEXT "boot", "!BOOT", 0000
PUTBASIC "spigot-bbp.6502.basic.txt", "SPIG"
PUTBASIC "spigot-bbp.basic.txt", "SPIGBAS"
