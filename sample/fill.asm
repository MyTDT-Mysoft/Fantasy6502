;http://www.6502asm.com/
jmp _start
_start:
;color green
lda #$5
sta $19
lda #$0
;x=0
sta $20
;y=1
lda #$1
sta $21
;x2 #31
lda #31
sta $22
;y2
lda #10
sta $23
;call pixel
jsr box
;halt
jsr hlt
 
;20 x
;21 y
;22 x2
;23 y2
;19 color
box:
lda $23
and #31
sta $23
lda $21
and #31
sta $21
box2:
lda $20
sta $1a
lda $21
sta $1b
lda $22
sta $1c
jsr hline
lda #31
cmp $21
beq box5
ldx $21
inx
stx $21
lda $21
cmp $23
bne box2
box5:
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
 