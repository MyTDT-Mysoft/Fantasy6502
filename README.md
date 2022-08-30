# Fantasy6502

a 6502 toolchain... assembler , emulator , debugger.

##the only emulated hardware I/O is:
* $FE (read a random number)
* $FE (write to sync to next frame of N ms)
* $FF (read to get latest pressed ascii code)

* at $200 you have a 32x32 gfx buffer
with 16 colors (1 byte per pixel)
color palette is fixed (same as C64 color palette)

* at $FE00 you have the default area for text view
for compatibility with some other online emulator

## like this one...
http://6502.cdot.systems/

tested on windows with freebasic 1.09
as well on raspberry Pi
so it should work when compiling on linux too

releases at first will only have windows binaries
and source code for others
