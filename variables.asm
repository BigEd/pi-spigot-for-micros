; ==================================================================================
; Optimization options, which add lots of extra code
; ==================================================================================

; Include the 40-bit maths code in the second processor versions
IF BELLARD AND BASE<&E00
INCLUDE_DIV40 =? TRUE
ELSE
INCLUDE_DIV40 =? FALSE
ENDIF

; Include versions of DIVADD/SUB with 24-bit maths for small divisors
; Bellard 1000 runs 17.9% faster
;     BBP 1000 runs 28.0% faster
OPTIMIZE_DIV24 =? TRUE

; Include versions of DIVADD/SUB with 16-bit maths for very small divisors
; Bellard 1000 runs 1.0% faster
;     BBP 1000 runs 1.0% faster
; (this depends on OPTIMIZE_DIV24 being TRUE and is ignored otherwise)
OPTIMIZE_DIV16 =? TRUE

; Optimize implemenation of shifts where the divisor LSB ends up as zero
; Bellard 1000 runs 1.7% faster
;     BBP 1000 runs 1.1% slower
; (this has most benefit when OPTIMIZE_DIV24 and OPTIMIZE_DIV16 are TRUE)
OPTIMIZE_SHIFT =? BELLARD

; Include additiona debug support code
DEBUG          =? FALSE

; Workaround for a bug in the PiTubeDirect JIT Co Pro
PITUBE_JIT_FIX =? FALSE

; Final digits test mode
TEST_MODE =? FALSE

; Number of guard bytes to add to big
GUARD =? 3

; ==================================================================================
; Constants
; ==================================================================================

       PAD = 4

; ==================================================================================
; MOS API Calls
; ==================================================================================

    OSARGS = &FFDA
    OSASCI = &FFE3
    OSNEWL = &FFE7
    OSWRCH = &FFEE
    OSWORD = &FFF1
    OSBYTE = &FFF4
     OSCLI = &FFF7

; ==================================================================================
; Working variables for runner program
; ==================================================================================

    memtop = &80 ; 2 bytes
       num = &82 ; 4 bytes
      arg1 = &86 ; 3 bytes
      arg2 = &89 ; 2 bytes
       tmp = &8B ; 2 bytes
       pad = &8D ; 1 byte
  lastpage = &8E ; 1 byte
   resultp = &8F ; 1 byte

  results = &110 ; results on the stack (8 bytes per entry)

; ==================================================================================
; Parameters for the spigot digit subroutine
; ==================================================================================

numeratorp = &70 ; 2 byte pointer to numerator bignum
      sump = &72 ; 2 byte pointer to sum bignum
       big = &74 ; 2-byte size of bignums
   ndigits = &76 ; 4-byte number of digits (4 bytes to allow for *8)

; ==================================================================================
; Working variables for the spigot digit subroutine
; (zero page &50-&6F is unused by BASIC)
; ==================================================================================

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

num_used_index = &7a ; 2 byte index

   divisor = &60 ; max 5 bytes / 40 bit unsigned integer
      temp = &65 ; max 5 bytes / 40 bit unsigned integer
IF BELLARD
   LSB_INC = 106*3
         f = &6a ; 4 bytes / 32 bit unsigned integer
         t = &7c ; 4 bytes / 32 bit unsigned integer
ELSE
   LSB_INC = 106
         k = &6a ; 4 bytes / 32 bit unsigned integer
ENDIF

IF TEST_MODE
     tcounter = &90 ; test mode digit counter
      tdigits = &94 ; test mode limit
         last = &98 ; last digit in the current run
  second_last = &99 ; last but one digit in the current run
previous_last = &9a ; last digit from the previous run
ENDIF
