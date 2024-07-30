#!/bin/bash

# Enable debug output by invoking with:
#    env DEBUG=1 ./build.sh
# or similar
DEBUG=${DEBUG:-0}

VISUALIZE=${VISUALIZE:-0}

MODE=${MODE:-4}

OPTIMIZE_DIV24=${OPTIMIZE_DIV24:-1}

PITUBE_JIT_FIX=${PITUBE_JIT_FIX:-0}

PAGES=${PAGES:-04 08 0E 11 19}

TARGETS=${TARGETS:-80,200,400,800,100,1000,3000}

# Build everything in a subdirectory
BUILD=build
mkdir -p ${BUILD}
rm -f ${BUILD}/*

# Prepare a !BOOT file
cat > ${BUILD}/boot <<EOF
*FX11
CLOSE#0:*RUN BEL11 $TARGETS
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

    for BASE in $PAGES
    do
        NAME=${STEM}${BASE}

        beebasm \
            -D DEBUG=${DEBUG} \
            -D VISUALIZE=${VISUALIZE} \
            -D MODE=${MODE} \
            -D BELLARD=${BELLARD} \
            -D BASE=0x${BASE}00 \
            -D OPTIMIZE_DIV24=${OPTIMIZE_DIV24} \
            -D PITUBE_JIT_FIX=${PITUBE_JIT_FIX} \
            -dd -labels ${BUILD}/${NAME}.labels \
            -v \
            -i spigot-runner.6502.asm \
            -o ${BUILD}/${NAME}.bin 2>&1 | tee ${BUILD}/${NAME}.log

        echo "PUTFILE \"${BUILD}/${NAME}.bin\",\"${NAME}\",&${BASE}00,&${BASE}00" >> ${BUILD}/${MKSSD}
    done

done

# Package into an .ssd file
beebasm -i ${BUILD}/${MKSSD} -do ${BUILD}/${SSD} -title "PI SPIG 2024" -opt 3
