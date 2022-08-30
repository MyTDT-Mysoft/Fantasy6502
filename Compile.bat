@echo off

fbc -dll 6502-view-dll.bas
fbc -dll 6502-gfx-dll.bas
fbc 6502.bas

pause
