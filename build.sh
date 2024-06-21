#!/bin/bash

beebasm -D BELLARD=0  -dd -labels     bbp-old.labels -i spigot-basic.6502.asm -v -do     spigot-bbp-old.ssd -title "OLD BBP"     -opt 3 |& tee bbp_old.log
beebasm -D BELLARD=-1 -dd -labels bellard-old.labels -i spigot-basic.6502.asm -v -do spigot-bellard-old.ssd -title "OLD BELLARD" -opt 3 |& tee bellard_old.log

beebasm -D BASE=0x1900 -D BELLARD=0  -dd -labels         bbp.labels -i spigot-runner.6502.asm -v -do        spigot-bbp.ssd -title "SPIG BBP"     -opt 3 |& tee bbp.log
beebasm -D BASE=0x1900 -D BELLARD=-1 -dd -labels     bellard.labels -i spigot-runner.6502.asm -v -do    spigot-bellard.ssd -title "SPIG BELLARD" -opt 3 |& tee bellard.log
