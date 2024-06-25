#!/bin/bash

# Build everything in a subdirectory
BUILD=build
mkdir -p ${BUILD}
rm -f ${BUILD}/*

# Prepare a !BOOT file
cat > ${BUILD}/boot <<EOF
*FX11
*RUN PIBEL19 80,200,400,800,100,1000,3000
EOF

# SSD File name
SSD=spigot.ssd

# Generate a BeebASM file to build the final ssd file
MKSSD=mkssd.asm
echo "PUTTEXT \"boot\", \"!BOOT\", 0000" >> ${BUILD}/${MKSSD}

# Compile multiple versions
for BELLARD in 0 1
do

    if [ "${BELLARD}" = "1" ]
    then
        STEM=PIBEL
        echo "PUTBASIC \"../spigot-bellard.basic.txt\", \"${STEM}BA\"" >> ${BUILD}/${MKSSD}
    else
        STEM=PIBBP
        echo "PUTBASIC \"../spigot-bbp.basic.txt\", \"${STEM}BA\"" >> ${BUILD}/${MKSSD}
    fi

    for BASE in 08 0E 19
    do
        NAME=${STEM}${BASE}

        beebasm -D BELLARD=${BELLARD} -D BASE=0x${BASE}00 -dd -labels ${BUILD}/${NAME}.labels -i spigot-runner.6502.asm -v -o ${BUILD}/${NAME}.bin |& tee ${BUILD}/${NAME}.log

        echo "PUTFILE \"${NAME}.bin\",\"${NAME}\",&${BASE}00,&${BASE}00" >> ${BUILD}/${MKSSD}
    done

done

# Package into an .ssd file
pushd ${BUILD}
beebasm -i ${MKSSD} -do ${SSD} -title "PI SPIG 2024" -opt 3
popd
