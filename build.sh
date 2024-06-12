#!/bin/bash

beebasm -D BELLARD=0  -dd -labels     bbp.labels -i spigot-common.6502.asm -v -do     spigot-bbp.ssd -title "SPIG BBP"     -opt 3 |& tee bbp.log
beebasm -D BELLARD=-1 -dd -labels bellard.labels -i spigot-common.6502.asm -v -do spigot-bellard.ssd -title "SPIG BELLARD" -opt 3 |& tee bellard.log
