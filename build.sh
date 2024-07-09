#!/bin/bash

# Enable debug output
DEBUG=1

# Build everything in a subdirectory
BUILD=build
mkdir -p ${BUILD}
rm -f ${BUILD}/*

# Prepare a !BOOT file
cat > ${BUILD}/boot <<EOF
*FX11
*RUN BEL19 80,200,400,800,100,1000,3000
EOF

# SSD File name
SSD=spigot.ssd

# Generate a BeebASM file to build the final ssd file
MKSSD=mkssd.asm
echo "PUTTEXT \"${BUILD}/boot\", \"!BOOT\", 0000" >> ${BUILD}/${MKSSD}

# Compile multiple versions
for BELLARD in 0 1
do

    if [ "${BELLARD}" = "1" ]
    then
        STEM=BEL
        echo "PUTBASIC \"spigot-bellard.basic.txt\", \"${STEM}BAS\"" >> ${BUILD}/${MKSSD}
    else
        STEM=BBP
        echo "PUTBASIC \"spigot-bbp.basic.txt\", \"${STEM}BAS\"" >> ${BUILD}/${MKSSD}
    fi

    for BASE in 08 0E 19
    do
        NAME=${STEM}${BASE}

        beebasm -D DEBUG=${DEBUG} -D BELLARD=${BELLARD} -D BASE=0x${BASE}00 -dd -labels ${BUILD}/${NAME}.labels -i spigot-runner.6502.asm -v -o ${BUILD}/${NAME}.bin 2>&1 | tee ${BUILD}/${NAME}.log

        echo "PUTFILE \"${BUILD}/${NAME}.bin\",\"${NAME}\",&${BASE}00,&${BASE}00" >> ${BUILD}/${MKSSD}
    done

done

# Package into an .ssd file
beebasm -i ${BUILD}/${MKSSD} -do ${BUILD}/${SSD} -title "PI SPIG 2024" -opt 3
