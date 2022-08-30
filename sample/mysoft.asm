; $0 = color ???
; $1 = X
; $2 = Y
; 
; *** init for keypress display ***
start: lda #$FF     ;reset previous key pressed  
  sta $30
  lda #$6F     ;dot in middle
  sta $31      
  lda #1       ;initial color
  sta $32
; *** init for counter ***
  lda #9       ;start from 999999 (rolling over to 000000)
  ldx #6       ;initialize 6 digits to 9
_initnum:
  sta $20,x
  dex
  bne _initnum

  ldx #32
  lda #14
_horzline:
  dex
  sta $3E0,x
  sta $500,x
  bne _horzline

  jmp _begin2


; *** show newest key pressed ***
jmp_add:
jmp _add;

_begin:  
  lda $FE      ;draw dot as position
  ldx $31      ;with random color
  sta $400,x

  lda $FF      ;get newest key pressed
  beq jmp_add  ;yes? then no need to update
  sta $30      ;store into previous key  
_begin2:
  ldx #0
  stx $FF
  lda $FE
  ora #1
  sta $0       ;color will be random
  lda #1
  sta $1       ;col position
  lda #26
  sta $2       ;row position
  lda $30      ;ascii to draw
  jsr drawnum  ;draw number in A  

;*** erase till 3 digits ***
  tax          ;A have the last X position draw
  lda #0       ;clearing with black
_nxerase:
  sta $0540,x  ;row 26
  sta $0560,x  ;row 27
  sta $0580,x  ;row 28
  sta $05A0,x  ;row 29
  sta $05C0,x  ;row 30
  inx
  cpx #14      ;reached column 14?
  bne _nxerase ;no? keep erasing

;*** draw dot ***
  ldx $31      ;position
  lda $32      ;color
  sta $400,x   ;draw
  
;*** check keys ***
  lda $30      ;last key pressed
  ora #$20     ;case insensitive  
  cmp #119 ;'w'
  beq k_up
  cmp #115 ;'s'
  beq k_down
  cmp #97  ;'a'
  beq k_left
  cmp #100 ;'d'
  beq k_right
  sta $32      ;if other key that becomes new color
  jmp _begin
k_up:
  clc
  lda $31
  adc #224
  sta $31
  jmp _begin
k_down:
  clc
  lda $31
  adc #32  
  sta $31
  jmp _begin
k_left:
  clc
  lda $31
  pha
  and #224
  sta $31
  pla
  adc #255
  and #31
  ora $31
  sta $31
  jmp _begin
k_right:
  clc
  clc
  lda $31
  pha
  and #224
  sta $31
  pla
  adc #1
  and #31
  ora $31
  sta $31
  jmp _begin

;*** displayer number of code passes ***
_add:  
  lda #10
  sta $FE       ;10ms frames
  lda #3
  sta $0        ; color will be 3 (cyan)
  sta $2        ; row position will be 3
  lda #24       ; digit X position (right to left)
  sta $1
  lda #6        ; max 6 digits
  sta $1F    
_nxadd:  
  ldx $1F       ; digit position
  inc $20,X     ; inc digit
  lda $20,X     ; load digit
  cmp #10       ; it overflows '9'
  beq _moredig  ; yes? then process next
  jsr drawchar  ; draw digit on A
  jmp _begin    ; and this number is done
_moredig:
  lda #0        ; reset digit
  sta $20,X     ; store new digit
  jsr drawchar  ; draw digit on A
  lda $1        ; \
  sbc #3        ; | PosX -= 4
  sta $1        ; /  
  dec $1F       ; advance to next digit
  bne _nxadd    ; go add next
jmp _begin

; ==========================================
; ============= RUNTIME LIBRARY ============
; ==========================================

drawchar:
  sta $E  ;
  asl     ;
  adc $E  ; X = A*3
  tax     ;
  lda #0
  sta $E  ;Lo Offset
  lda $2  ;get Y  
  lsr     ;|
  ror $E  ;|
  lsr     ;| 16bit
  ror $E  ;| << 5
  lsr     ;|
  ror $E  ;| guarantees C=0
  adc #2  ;+2 ($200)
  sta $F  ;Hi Offset
  lda $1  ;get X
  adc $E  ;Lo Offset + X
  sta $E  ;store it
  bcc drawchar_nc1
  inc $F  ;carry to Hi offset
drawchar_nc1:
  ldy #0        ;output offset
  lda #3
  sta $12       ;$12 = (3 bytes per char)
drawchar_nextrows:
  lda font,x  
  sta $10     ;$10 = current byte
  lda #$11
  sta $11     ;$11 = 4|4! (rot counter)
drawchar_nextpix:
  lda #$0
  asl $10     ;get pixel  
  sbc #0
  and $0      ;color or 0
  sta ($E),y
  iny
  asl $11     ;1 bit done
  bcc drawchar_nextpix
  lda $11     ;why??
  beq drawchar_rowsdone
  tya 
  adc #27     ;(32-5)-1 (carry) (next row)
  tay  
  jmp drawchar_nextpix
drawchar_rowsdone:  
  tya 
  adc #27     ;(32-5)-1 (carry) (next row)
  tay
  inx
  dec $12     ;2 rows done, theres more?  
  bne drawchar_nextrows  
  rts

drawnum:
  tax
  ldx $1
  stx $13
  cmp #100
  bcs drawnum_do_100
  cmp #10
  bcs drawnum_do_10
  jmp drawnum_do_1
drawnum_do_100:
  ldx #$FF
drawnum_100:  
  inx 
  sec 
  sbc #100
  bcs drawnum_100
  adc #100
  pha
  txa
  jsr drawchar
  lda $1             ;read X
  clc
  adc #4
  sta $1
  pla
drawnum_do_10:
  ldx #$FF
drawnum_10:
  inx  
  sec
  sbc #10
  bcs drawnum_10
  adc #10
  pha
  txa
  jsr drawchar
  lda $1             ;read X
  clc
  adc #4
  sta $1
  pla
drawnum_do_1:
  jsr drawchar
  lda $1
  clc
  adc #4
  ldx $13
  stx $1
  rts
  

font:
  dcb $15,$55,$1F ;0
  dcb $B3,$BB,$1F ;1
  dcb $1D,$17,$1F ;2
  dcb $1D,$9D,$1F ;3
  dcb $55,$1D,$DF ;4
  dcb $17,$1D,$1F ;5
  dcb $17,$15,$1F ;6
  dcb $1D,$BB,$BF ;7
  dcb $15,$15,$1F ;8
  dcb $15,$1D,$1F ;9
  