;http://www.6502asm.com/
jmp _start
_start:
;color green
lda #$5
sta $19
lda #0
;x=0
sta $29
;y=0
lda #0
sta $2a
;x1
lda #31
sta $2b
lda #31
sta $2c
lda #8
sta $2f
;call pixel
jsr grid
;halt
jsr hlt


;29 x
;2a y
;2b x1
;2c y1
;2d x3
;2e y3
;2f st
grid:
lda $29
and #31
sta $29
lda $2a
and #31
sta $2a
lda $2b
and #31
sta $2b
lda $2c
and #31
sta $2c
lda $29
sta $2e
grid1:
lda $29
sta $1a
lda $2e
sta $1b
lda $2b
sta $1c
;call pixel
jsr hline
lda $2f
clc
adc $2e
sta $2e
cmp $2c
bmi grid1

lda $29
sta $2e
grid2:
lda $2e
sta $1a
lda $2a
sta $1b
lda $2c
sta $1c
;call pixel
jsr vline
lda $2f
clc
adc $2e
sta $2e
cmp $2b
bmi grid2


rts


;$1a, x
;$1b,y
;$1c,x2
;$19,color
hline:
clc 
;check if x and y x2<32
lda $1c
and #31
sta $1c
lda $1a
and #31
sta $1a
lda $1b
and #31
sta $1b
; fill size 1 fill 1 byte
lda $1c
clc
sbc $1a
and #63
cmp #0
bne hline40
lda #1
hline40:
sta $17
;fill size 1 byte fill 1 byte hig byte
lda #$0
sta $18
;load x
ldx $1b
; load 0x200 of y start of the screen
ldy #$2
;if y = 0 escape the * by 0
cpx #$0
beq hline3
hline2:
clc
;multiply y line by 32 20hex
adc #$20
;check for low byte overflow
bcc hline5
clc
;inc carry to y high byte
iny
hline5:
clc
dex
;counter y lines for
cpx #$0
bne hline2
hline3:
;store hig byte to start fill
sty $16
clc 
;add x to low byte
adc $1a
clc
sta $15
;call fill start,count,color
jsr fill
hline12:
rts
;the fill ok
;$15,16,address
;$17,18,counter
;$19,value
fill:
ldx $17
cpx #$0
bne fill11
ldx $18
cpx #$0
beq fill10
fill11:
ldx #$0
lda $19
sta  ($15,x)
ldx $15
inx
cpx #$0
bne fill2
stx $15
ldx $16
inx
stx $16
ldx $15
fill2:
stx $15
ldx $17
dex
stx $17
cpx #$ff
bne fill22
ldx $18
dex
stx $18
fill22:
ldx $17
cpx #$0
bne fill11
ldx $18
cpx #$0
bne fill11
fill10:
rts
hlt:
jmp hlt






;$1a, x
;$1b,y
;$1c,x2
;$19,color
vline:
clc 
;check if x and y y2<32
lda $1c
and #31
sta $1c
lda $1a
and #31
sta $1a
lda $1b
and #31
sta $1b
; fill size 1 fill 1 byte
lda $1c
clc
sbc $1b
and #63
cmp #0
bne vline40
lda #1
vline40:
sta $17
;fill size 1 byte fill 1 byte hig byte
lda #$0
sta $18
;load x
ldx $1b
; load 0x200 of y start of the screen
ldy #$2
;if y = 0 escape the * by 0
cpx #$0
beq vline3
vline2:
clc
;multiply y line by 32 20hex
adc #$20
;check for low byte overflow
bcc vline5
clc
;inc carry to y high byte
iny
vline5:
clc
dex
;counter y lines for
cpx #$0
bne vline2
vline3:
;store hig byte to start fill
sty $16
clc 
;add x to low byte
adc $1a
clc
sta $15
lda #32
sta $14
;call fill start,count,color
jsr fillps
vline12:
rts
;the fillps ok
;14 add
;$15,16,address
;$17,18,counter
;$19,value
fillps:
ldx $17
cpx #$0
bne fillps11
ldx $18
cpx #$0
beq fillps10
fillps11:
ldx #$0
lda $19
sta  ($15,x)
lda $15
clc
adc $14
bcc fillps2
sta $15
ldx $16
inx
stx $16
lda $15
fillps2:
sta $15
ldx $17
dex
stx $17
cpx #$ff
bne fillps22
ldx $18
dex
stx $18
fillps22:
ldx $17
cpx #$0
bne fillps11
ldx $18
cpx #$0
bne fillps11
fillps10:
rts
hlt:
jmp hlt
