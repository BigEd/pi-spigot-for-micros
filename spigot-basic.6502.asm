        ORG &6000
        GUARD &7C00

include "variables.asm"
include "macros.asm"

.code_start

; =============================================================
; MAIN PROGRAM
; =============================================================

; Start with a JSR/RTS to give consistent address to end profiling
JSR spigot ; +0000
RTS

; Embed a title into the machine code
IF BELLARD
EQUS "Bellard Pi Spigot"
EQUB 13
ELSE
EQUS "BBP Pi Spigot"
EQUB 13
EQUB 0,0,0,0 ; Padding so division code starts in same place
ENDIF

include "spigot-common.6502.asm"

.code_end

PUTTEXT "boot", "!BOOT", 0000
SAVE "SPIGMC", code_start, code_end
PUTBASIC "spigot-common.6502.basic.txt", "SPIG"

IF BELLARD
PUTBASIC "spigot-bellard.basic.txt", "SPIGBAS"
ELSE
PUTBASIC "spigot-bbp.basic.txt", "SPIGBAS"
ENDIF
