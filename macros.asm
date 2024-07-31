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

MACRO _MOV16C result, arg1
FOR I,0,1
        LDA #((arg1 >> (I*8)) AND &FF)
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

MACRO _SUB16 result, arg1, arg2
        SEC
FOR I,0,1
        LDA arg1+I
        SBC arg2+I
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

MACRO _SUB16C result, arg1, arg2
        SEC
FOR I,0,1
        LDA arg1+I
        SBC #((arg2 >> (I*8)) AND &FF)
        STA result+I
NEXT
ENDMACRO

; ==================================================================================
; 32-bit MACROS
; ==================================================================================

MACRO _MOV32 result, arg1
FOR I,0,3
        LDA arg1+I
        STA result+I
NEXT
ENDMACRO

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

MACRO _ZERO40 result
        LDA #0
FOR I,0,4
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

MACRO _INC32 result
FOR I,0,3
        INC result+I
        BNE done
NEXT
.done
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

MACRO _MULTIPLY_LITTLE_ENDIAN table, extra
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

MACRO _MULTIPLY_BIG_ENDIAN table, extra
        _SUB16  np_end, np, big   ; np_end is one beyond the last element of work
IF extra
        _SUB16C np_end, np_end, extra ; any extra bytes beyond the MSB?
ENDIF
        LDX     #np
        JSR     calc_num_lsb_index
        _CMP16  np_end, np        ; range check up front to be safe
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

;   FOR I%=M% TO L% STEP -1

        _SUB16  np, numeratorp, msb_index
        _SUB16  np_end, numeratorp, lsb_index

        _CMP16  np_end, np  ; C=1 if arg1 >= arg2
        BCS     work_to_do
        PLA
        PLA
        PLA                 ; pop the operation
        RTS
.work_to_do
        _ADD16  sp, sump, msb_index
IF OPTIMIZE_SHIFT
        LDA     offset
        BNE     skip_adjust
        _DEC16  sp
.skip_adjust
ENDIF
        LDY     #0
        RTS

ENDMACRO

MACRO _DIVADDSUB bytes

; The carry indicated the operation (C=0 for Add to Sum, C=1 for Subtract from Sum)
        PHP

; This calls the _DIVINIT code as a subroutine
        JSR     divinit

;   Scary self-modifying code to update the LDX #&xx and SBC #&xx operands

; Bit number threshold for applying the compare BEQ shortcut optimization
IF BELLARD
IF bytes=2
    COMP_OPT_THRESHOLD=7 ; 16-bit division: optimize slice 7
ELIF bytes=3
    COMP_OPT_THRESHOLD=4 ; 24-bit division: optimize slices 4-7
ELIF bytes=4
    COMP_OPT_THRESHOLD=2 ; 32-bit division: optimize slices 2-7
ELSE
    COMP_OPT_THRESHOLD=8 ; don't optimize
ENDIF
ELSE ; different settings for BBP
IF bytes=2
    COMP_OPT_THRESHOLD=7 ; 16-bit division: optimize slice 7
ELIF bytes=3
    COMP_OPT_THRESHOLD=6 ; 24-bit division: optimize slices 6-7
ELIF bytes=4
    COMP_OPT_THRESHOLD=8 ; 32-bit division: don't optimize
ELSE
    COMP_OPT_THRESHOLD=8 ; don't optimize
ENDIF
ENDIF

        STY     divisor+bytes-1  ; Y=0
FOR j,7,0,-1
; Code offset to the Jth division bit slice compare block
COMPARE_J  = j*(bytes*14+5) - (j>COMP_OPT_THRESHOLD)*(j-COMP_OPT_THRESHOLD)*2
; Code offset to the Jth division bit slice subtract block
SUBTRACT_J = COMPARE_J + bytes*8 - (j>=COMP_OPT_THRESHOLD)*2
FOR i,bytes-1,0,-1
        ; Modify the LDX #
        LDA     divisor+i
        STA     modify + COMPARE_J + (bytes-1-i)*8 + 1 - (j>=COMP_OPT_THRESHOLD AND i<bytes-1)*2
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


; Configure the operation
        PLP
        BCS     configure_subtract
.configure_add
        LDA     #&00    ; initial byte value
        STA     modify-1
        LDA     #&18    ; CLC opcode
        STA     modify_clc_sec
        LDA     #&90    ; BCC opcode
        STA     modify_bcc_bcs
        LDA     #&00    ; Constant operand
        STA     modify_lda+1
        LDA     #&B0    ; BCS opcode
        STA     modify_bcs_bcc
        ; In the add case we want the subtract block to end with
        ; ORA #(&80 >> j)
        ; C=0
        LDX     #&09  ; ORA immediate opcode
        LDA     #&80
        BCC     configure_and_or
.configure_subtract
        LDA     #&FF    ; initial byte value
        STA     modify-1
        LDA     #&38    ; SEC opcode
        STA     modify_clc_sec
        LDA     #&B0    ; BCS opcode
        STA     modify_bcc_bcs
        LDA     #&FF    ; Constant operand
        STA     modify_lda+1
        LDA     #&90    ; BCC opcode
        STA     modify_bcs_bcc
        ; In the add case we want the subtract block to end with
        ; AND #(&FF - (&80 >> j))
        ; C=1
        LDX     #&29  ; AND immediate opcode
        LDA     #&7F
.configure_and_or
FOR j,1,8
; Almost identical to COMPARE_J beloe
ORA_AND_J = j*(bytes*14+5) - (j>COMP_OPT_THRESHOLD)*(j-COMP_OPT_THRESHOLD)*2 - 2
        STX     modify + ORA_AND_J
        STA     modify + ORA_AND_J+1
        ROR     A
NEXT

;.byte_loop_full_fat
;     20D0   A6 64      LDX &64     ; unchanged
;     20D2   B1 50      LDA (&50),Y
;     20D4   85 64      STA &64

; self-modify - configure for "full fat" (where we fetch numerator bytes from the bignum)
        LDA     #&B1         ; LDA (zp),y opcode
        STA     byte_loop+2
        LDA     #np
        STA     byte_loop+3
        LDA     #&85         ; STA zp
        STA     byte_loop+4
        LDA     #<byte_loop
        STA     byte_loop_jmp+1
        LDA     #>byte_loop
        STA     byte_loop_jmp+2

; self-modify the comparison at the end of the loop

; compare the LSB and Used Indexes
        _CMP16  lsb_index, num_used_index
        BCC     terminate_on_used_index    ; branch if lsb_index < num_used_index

.terminate_on_lsb_index
        LDA     np_end
        STA     terminal_value_lsb+1
        LDA     np_end+1
        STA     terminal_value_msb+1
        BNE     byte_loop_init
.terminate_on_used_index
        _SUB16  temp, numeratorp, num_used_index
        LDA     temp
        STA     terminal_value_lsb+1
        LDA     temp+1
        STA     terminal_value_msb+1

; TODO try to move this back to DIVINIT

.byte_loop_init
;   T%=0
        _ZERO40 temp

.byte_loop
;       T%=T%*256+NumeratorP?I%
        LDX     temp+0           ; we skip this in the low-fat case
        LDA     (np),Y           ; dynamically modified
        STA     temp+0           ; dynamically modified
IF bytes>2
FOR i,bytes-1,2,-1
        LDA     temp+i-1
        STA     temp+i
NEXT
ENDIF
        STX     temp+1
;       A will be used to accumulate the 8 result bits
        LDA     #0           ; dynmically modified

.modify

;       Unroll the bit loop
;       FOR J%=0 TO 7
FOR j,0,7

;       IF T%>=D%
FOR i,bytes-1,0,-1
        LDX     #&00         ; operand is modified dynamically
        CPX     temp+i       ; X is used so as not to corrupt A
; Shortcut the first BCC/BNE (optimization suggested by profiling)
IF i=bytes-1 AND j>=COMP_OPT_THRESHOLD
        BEQ     skip_br
ENDIF
        BCC     do_subtract
        BNE     bit_loop_next
.skip_br
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
        ORA     #(&80 >> j)  ; dynmically modified (e.g. ORA #&40 for add, AND #&BF for subtract)

;       NEXT J%
.bit_loop_next
NEXT

;     IF B%=0 NEXT:ENDPROC
;        CMP     #0
;        BEQ     byte_loop_next

;     IF C% SumP!I%=S%+B%:ELSE SumP!I%=S%-B%

        ; Add byte
.modify_clc_sec
        CLC                      ; dynamically modified (CLC/SEC)
        ADC     (sp),Y
        STA     (sp),Y
.modify_bcc_bcs
        BCC     byte_loop_next   ; dynamically modified (BCC/BCS)
.cloop
        INY
.modify_lda
        LDA     #0               ; dynamically modified (LDA #&00/LDA #&FF
        ADC     (sp),Y
        STA     (sp),Y
.modify_bcs_bcc
        BCS     cloop            ; dynamically modified (BCS/BCC)
        LDY     #0

; The subtract form looks like
;
;.modify_clc_sec
;        SEC
;        ADC     (sp),Y
;        STA     (sp),Y
;.modify_bcc_bcs
;        BCS     byte_loop_next
;.cloop
;        INY
;.modify_lda
;        LDA     #&FF
;        ADC     (sp),Y
;        STA     (sp),Y
;.modify_bcs_bcc
;        BCC     cloop
;        LDY     #0


;       NEXT

.byte_loop_next
        ; Check the LSB first, optimize for branch not taken
        LDA     np
.terminal_value_lsb
        CMP     #np_end          ; dynamically modified
        BEQ     byte_loop_done

.byte_loop_more
        _INC16  np
        _DEC16  sp
.byte_loop_jmp
        JMP     byte_loop         ; dynamically modified

.byte_loop_done
        ; Check the MSB
        LDA     np+1
.terminal_value_msb
        CMP     #np_end+1         ; dynamically modified
        BNE     byte_loop_more

; Already in low fat phase? then exit
        LDA    byte_loop+2
        CMP    #&A6
        BEQ    exit

; Finish something

; compare the LSB and Used Indexes
        _CMP16  lsb_index, num_used_index
        BCC     modify_phase2    ; branch if lsb_index < num_used_index
.exit
        RTS

.modify_phase2

;.byte_loop_low_fat
;     20D0   A6 64      LDX &64 ; unchanged
;     20D2   A6 64      LDX &64
;     20D4   84 64      STY &64

; TODO: Save 2 cycles by modifying JMP byte_loop (see above)

; self-modify - configure for "low fat" (where the "numerator" bytes are actually zeros)
        LDA     #&A6          ; LDX zp
        STA     byte_loop+2
        LDA     #temp
        STA     byte_loop+3
        LDA     #&84          ; STY zp
        STA     byte_loop+4

        LDA     #<(byte_loop+2)	; we can skip the first instruction for a small speedup
        STA     byte_loop_jmp+1
        LDA     #>(byte_loop+2)
        STA     byte_loop_jmp+2

        LDA     np_end
        STA     terminal_value_lsb+1
        LDA     np_end+1
        STA     terminal_value_msb+1

        BNE     byte_loop_more      ; branch always
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
