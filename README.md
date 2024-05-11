# pi-spigot-for-micros
Calculating pi digit by digit on 1980s micros - a work in progress

We hope to benchmark against previous work by litwr which uses Rabinowitz' spigot:
- http://litwr2.atspace.eu/pi/pi-spigot-benchmark.html
- https://stardot.org.uk/forums/viewtopic.php?t=11458
- https://github.com/litwr2/rosetta-pi-spigot

We're using the BBP formula for pi calculation, adjusted to produce decimal digits, following the relay computer work presented by The Science Elf
- https://youtu.be/SPTzzSuBFlc

When we've done that, we plan to implement Bellard's improved approach which promises to run faster but should be able to reuse the same machinery
- https://bellard.org/pi/

The algorithm can be written simply in BBC Basic like this
```
REM BBP - see github.com/BigEd/pi-spigot-for-micros
REM 10 digits correct with BBC Basic on 6502
REM 19 digits correct with BBC Basic on x86
S=0:N=1
FOR K=0 TO 8*20 STEP 8
  S=S+4*N/(K+1)
  S=S-2*N/(K+4)
  S=S-N/(K+5)
  S=S-N/(K+6)
  PRINT "digit ";INT(S)
  S=S-INT(S)
  S=S*10
  N=N*10/16
NEXT
```
but to get more than a few digits we will need bignum operations: N and S both need high precision. However, neither of them become very large or very small so fixed point will be fine.  We need to multiply a bignum by a small number, to add and subtract bignums, to scale a bignum by a fraction, and to divide a bignum by a small number.

To take a simple approach we will need one or more temporary bignums which will cost both time and space.  We expect to be able to refine our approach to use only two bignums, and possibly even to share space as one grows while the other one shrinks.

The Bellard algorithm is also simple to capture in BBC Basic:
```
REM Bellard - github.com/BigEd/pi-spigot-for-micros
REM 11 digits correct with BBC Basic on 6502
REM 19 digits correct with BBC Basic on x86
S=0:N=1/64
FOR K=0 TO 7
  F=4*K:T=10*K
  S=S-32*N/(F+1)
  S=S-N/(F+3)
  S=S+256*N/(T+1)
  S=S-64*N/(T+3)
  S=S-4*N/(T+5)
  S=S-4*N/(T+7)
  S=S+1*N/(T+9)
  PRINT "digit ";INT(S)
  S=S-INT(S)
  S=S*1000
  N=-N*1000/1024
NEXT
```
Notice in both cases that most constants are powers of two which should help.
