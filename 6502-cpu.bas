namespace cpu

  type CpuFlags
    union
      type
        Carry     : 1 as ubyte
        Zero      : 1 as ubyte
        Interrupt : 1 as ubyte
        Decimal   : 1 as ubyte
        Break     : 1 as ubyte
        One       : 1 as ubyte
        oVerflow  : 1 as ubyte
        Negative  : 1 as ubyte
      end type
      bFlags as ubyte
    end union
  end type 
  type CpuCore
    as ubyte  RegA,RegX,RegY,RegSP
    as CpuFlags RegP
    as ubyte  bOpcode 
    as ushort RegPC , wMemAddr
    as ubyte bBreak , bDone , bDebug , bRun , bSleep
    as long iCyclesMS = 2^10
  end type
  
  static shared as CpuCore g_tCpu
  
  #define _Begin with g_tCpu
  #define _End end with
  
  sub ResetCPU( tCpu as CpuCore )
    with tCpu
      .RegA = 0 : .RegX = 0 : .RegY = 0
      .RegSP = &hFF : .RegPC = &h600 
      .RegP = type(0,0,0,1,0,1,0,0)    
    end with
  end sub
  sub ShowCPU( tCpu as CpuCore )
    var iRow = csrlin() , iCol = pos()
    var iConW = width(), iConH = hiword(iConW) 
    iConW = loword(iConW)
    view print : color 14,1 : locate 1,1
    with tCpu 
      #define f(_f,_c) iif(.RegP._f,asc(_c)-32,asc(_c))
      dim as zstring*512 zCpu = any
      var iLen = sprintf(zCpu,"A=$%02X  X=$%02X  Y=$%02X  SP=$01%02X  PC=$%04X  P=$%02X (%c%c%c1%c%c%c%c) %s", _
      .RegA , .RegX , .RegY , .RegSP , .RegPC , .RegP.bFlags , f(Negative,"n") , f(oVerflow,"v") , _
      f(Break,"b") , f(Decimal,"d") ,  f(Interrupt,"i") , f(Zero,"z") , f(Carry,"c") , str(uTotalCycles2) )    
      (@zCpu)[iLen] = space(loword(width())-iLen)
      puts(zCpu)
    end with  
    view print 3 to iConH-1
    locate iRow,iCol : color 7,0  
  end sub
  
  #ifdef CpuNotifyRead
    #define NotifyRead8(_uaddr) g_bMemChg(_uaddr) = -2
    #define NotifyRead16(_uaddr) *cptr(ushort ptr,@g_bMemChg(_uAddr)) = &hFEFE
  #else
    #define NotifyRead8(_uaddr) rem
    #define NotifyRead16(_uaddr) rem
  #endif
  #ifdef CpuNotifyWrite
    #define NotifyWrite8(_uaddr) g_bMemChg(_uaddr) = 2
    #define NotifyWrite16(_uaddr) *cptr(ushort ptr,@g_bMemChg(_uAddr)) = &h0202
  #else
    #define NotifyWrite8(_uaddr) rem
    #define NotifyWrite16(_uaddr) rem
  #endif    

  function ReadByte( uAddr as long ) as ubyte  
    if uAddr = &hFE then return rand()    
    NotifyRead8(uAddr)
    return g_bMemory(uAddr)
  end function
  #if 0
  function ReadWord( uAddr as long ) as ulong
    if uAddr = &hFE then return (rand() and &hFF)+(g_bMemory(uAddr+1) shl 8)
    return *cptr(ushort ptr,@g_bMemory(uAddr))
  end function
  #endif
  
  #ifdef CpuNotifyRead  
    function ReadWord( uAddr as long ) as ulong
      NotifyRead16(uAddr)
      return culng(*cptr(ushort ptr,@g_bMemory(uAddr)))
    end function    
    #define ReadByteFast ReadByte
  #else
    #define ReadWord( _uAddr ) culng(*cptr(ushort ptr,@g_bMemory(_uAddr)))
    #define ReadByteFast( _uAddr ) g_bMemory(_uAddr) 
  #endif
    
  sub WriteByte( uAddr as long , bValue as ubyte )
    if uAddr = &hFE then 'align to next X ms
      static as double dDelay 
      if g_tCpu.bDebug then
        dDelay = timer
      else        
        if abs(timer-dDelay) > ((bValue*2)/1000) then
          dDelay = timer
        else
          while (timer-dDelay) < (bValue/1000)
            sleep 1, 1
          wend
          dDelay += (bValue/1000)
        end if
        g_tCpu.bBreak = 1
        g_tCpu.bSleep = 1
      end if
    end if
    g_bMemory(uAddr) = bValue
    NotifyWrite8(uAddr)
  end sub
  #define WriteWord( _uAddr , _wValue ) *cptr(ushort ptr,@g_bMemory(_uAddr)) = (_wValue) : NotifyWrite16(_uAddr)
  #define WriteByteFast( _uAddr , _uVal ) g_bMemory(_uAddr) = (_uVal) : NotifyWrite8(_uAddr)
  
  #define TestFlag( _Name ) .RegP._Name
  #define GetFlag( _Name ) .RegP._Name
  #define SetFlag( _Name )   .RegP._Name = 1
  #define ClearFlag( _Name ) .RegP._Name = 0
  #define CalcFlag_Negative( _Val ) .RegP.Negative = (((_Val) shr 7) and 1)
  #define CalcFlag_Zero( _Val )     .RegP.Zero     = (cubyte(_Val)=0)
  #define CalcFlag_Carry( _Val )    .RegP.Carry    = ((_Val)>255)
  #define CalcFlag_XCarry( _Val )   .RegP.Carry    = ((_Val)<=255)
  '#define CalcFlag_oVerFlow( _Val1 , _Val2 ) .RegP.oVerflow = (((_Val1) shr 7) and 1) xor (((_Val2) shr 7) and 1)
  #define CalcFlag_oVerFlow( _Val ) .RegP.oVerflow = (clng(_Val)<-128) or (clng(_Val)>127)
  #define CalcFlag_oVerFlow6( _Val ) .RegP.oVerflow = (((_Val) shr 6) and 1)
  
  #macro CalcFlags_NZ(_Val) 
    CalcFlag_Negative( _Val )
    CalcFlag_Zero( _Val )
  #endmacro
  #macro CalcFlags_NZC(_Val) 
    CalcFlag_Negative( _Val )
    CalcFlag_Zero( _Val )
    CalcFlag_Carry( _Val )
  #endmacro
  #macro CalcFlags_NZCV(_Val) 
    CalcFlag_Negative( _Val )
    CalcFlag_Zero( _Val )
    CalcFlag_Carry( _Val )
    CalcFlag_oVerFlow( _Val )
  #endmacro
  
  '------------- ADDRESSING -------------
  sub fnAddr_Unimplemented()
    _Begin    
      color 12 : print "Unimplemented Address: " & hex(.bOpcode,2) & " > '"+g_zDism(.bOpcode)+"'"
      .bBreak = 1
    _End
  end sub
  sub fnAddr__x_()
    _Begin  
      color 12 : print "Addressing from illegal opcode: " & hex(.bOpcode,2)  
      .bBreak = 1
    _End
  end sub
  sub fnAddr_imm8()   'immediate byte right after opcode
    _Begin
      .wMemAddr = .RegPC : .RegPC += 1
    _End
  end sub
  sub fnAddr_z8()     'zero page (imm 8bit) address right after opcode
    _Begin
      .wMemAddr = ReadByte(.RegPC) : .RegPC += 1
    _End
  end sub
  sub fnAddr_i8X()   'zero page (imm 8bit) address plus X (unsigned, wrap 8)
    _Begin
      .wMemAddr = (ReadByte(.RegPC)+.RegX) and &hFF
      .RegPC += 1
    _End
  end sub
  sub fnAddr_i8Y()   'zero page (imm 8bit) address plus Y (unsigned, wrap 8)
    _Begin
      .wMemAddr = (ReadByte(.RegPC)+.RegY) and &hFF
      .RegPC += 1
    _End
  end sub
  sub fnAddr_i16X()   '(imm 16bit) address plus X (unsigned)
    _Begin
      .wMemAddr = (ReadWord(.RegPC)+.RegX) and &hFFFF 
      .RegPC += 2
    _End
  end sub
  sub fnAddr_i16Y()   '(imm 16bit) address plus Y (unsigned)
    _Begin
      .wMemAddr = (ReadWord(.RegPC)+.RegY) and &hFFFF 
      .RegPC += 2
    _End
  end sub
  sub fnAddr_r16()    'PC plus imm8 (signed)
    _Begin
      .wMemAddr = (1+.RegPC+cbyte(ReadByte(.RegPC))) and &hFFFF 
      .RegPC += 1
    _End
  end sub
  sub fnAddr_ind8X()  'indirect (16bit address at zero page (imm 8bit) plus X) (unsigned)
    _Begin
      .wMemAddr = ReadWord(((ReadByte(.RegPC)+.RegX) and &hFF))
      .RegPC += 1
    _End
  end sub
  sub fnAddr_ind8Y()  'indirect (16bit address at zero page (imm 8bit)) plus Y (unsigned)
    _Begin
      .wMemAddr = (ReadWord(ReadByte(.RegPC))+.RegY) and &hFFFF 
      .RegPC += 1
    _End
  end sub
  sub fnAddr_ind16()  'indirect (16bit address) (unsigned)
    _Begin
      .wMemAddr = ReadWord(ReadByte(.RegPC))
      .RegPC += 2
    _End
  end sub
  sub fnAddr_a16()    '(imm 16bit) address
    _Begin
      .wMemAddr = ReadWord(.RegPC)
      .RegPC += 2
    _End
  end sub
  
  #define u(_op,_n) @fnAddr_Unimplemented
  #define f(_op,_n) @fnAddr##_n
  static shared as sub() g_fnAddr(255) = { _
    _'   0             1              2            3         4             5             6            7           
    _'   8             9              A            B         C             D             E            F           
    u(BRK,_none) ,f(ORA,_ind8X) ,f(z,__x_)    ,f(z,__x_),f(z,__x_)    ,f(ORA,_z8)   ,f(ASL,_z8)   ,f(z,__x_),_ '00  
    u(PHP,_none) ,f(ORA,_imm8)  ,u(ASL,_Acc)  ,f(z,__x_),f(z,__x_)    ,f(ORA,_a16)  ,f(ASL,_a16)  ,f(z,__x_),_ '08
    f(BPL,_r16)  ,f(ORA,_ind8Y) ,f(z,__x_)    ,f(z,__x_),f(z,__x_)    ,f(ORA,_i8X)  ,f(ASL,_i8X)  ,f(z,__x_),_ '10
    u(CLC,_none) ,f(ORA,_i16Y)  ,f(z,__x_)    ,f(z,__x_),f(z,__x_)    ,f(ORA,_i16X) ,f(ASL,_i16X) ,f(z,__x_),_ '18
    f(JSR,_a16)  ,f(AND,_ind8X) ,f(z,__x_)    ,f(z,__x_),f(BIT,_z8)   ,f(AND,_z8)   ,f(ROL,_z8)   ,f(z,__x_),_ '20
    u(PLP,_none) ,f(AND,_imm8)  ,u(ROL,_Acc)  ,f(z,__x_),f(BIT,_a16)  ,f(AND,_a16)  ,f(ROL,_a16)  ,f(z,__x_),_ '28
    f(BMI,_r16)  ,f(AND,_ind8Y) ,f(z,__x_)    ,f(z,__x_),f(z,__x_)    ,f(AND,_i8X)  ,f(ROL,_i8X)  ,f(z,__x_),_ '30
    u(SEC,_none) ,f(AND,_i16Y)  ,f(z,__x_)    ,f(z,__x_),f(z,__x_)    ,f(AND,_i16X) ,f(ROL,_i16X) ,f(z,__x_),_ '38
    u(RTI,_none) ,f(EOR,_ind8X) ,f(z,__x_)    ,f(z,__x_),f(z,__x_)    ,f(EOR,_z8)   ,f(LSR,_z8)   ,f(z,__x_),_ '40
    u(PHA,_none) ,f(EOR,_imm8)  ,u(LSR,_Acc)  ,f(z,__x_),f(JMP,_a16)  ,f(EOR,_a16)  ,f(LSR,_a16)  ,f(z,__x_),_ '48
    f(BVC,_r16)  ,f(EOR,_ind8Y) ,f(z,__x_)    ,f(z,__x_),f(z,__x_)    ,f(EOR,_i8X)  ,f(LSR,_i8X)  ,f(z,__x_),_ '50
    u(CLI,_none) ,f(EOR,_i16Y)  ,f(z,__x_)    ,f(z,__x_),f(z,__x_)    ,f(EOR,_i16X) ,f(LSR,_i16X) ,f(z,__x_),_ '58
    u(RTS,_none) ,f(ADC,_ind8X) ,f(z,__x_)    ,f(z,__x_),f(z,__x_)    ,f(ADC,_z8)   ,f(ROR,_z8)   ,f(z,__x_),_ '60
    u(PLA,_none) ,f(ADC,_imm8)  ,u(ROR,_Acc)  ,f(z,__x_),f(JMP,_ind16),f(ADC,_a16)  ,f(ROR,_a16)  ,f(z,__x_),_ '68
    f(BVS,_r16)  ,f(ADC,_ind8Y) ,f(z,__x_)    ,f(z,__x_),f(z,__x_)    ,f(ADC,_i8X)  ,f(ROR,_i8X)  ,f(z,__x_),_ '70
    u(SEI,_none) ,f(ADC,_i16Y)  ,f(z,__x_)    ,f(z,__x_),f(z,__x_)    ,f(ADC,_i16X) ,f(ROR,_i16X) ,f(z,__x_),_ '78
    f(z,__x_)    ,f(STA,_ind8X) ,f(z,__x_)    ,f(z,__x_),f(STY,_z8)   ,f(STA,_z8)   ,f(STX,_z8)   ,f(z,__x_),_ '80
    u(DEY,_none) ,f(z,__x_)     ,u(TXA,_none) ,f(z,__x_),f(STY,_a16)  ,f(STA,_a16)  ,f(STX,_a16)  ,f(z,__x_),_ '88
    f(BCC,_r16)  ,f(STA,_ind8Y) ,f(z,__x_)    ,f(z,__x_),f(STY,_i8X)  ,f(STA,_i8X)  ,f(STX,_i8Y)  ,f(z,__x_),_ '90
    u(TYA,_none) ,f(STA,_i16Y)  ,u(TXS,_none) ,f(z,__x_),f(z,__x_)    ,f(STA,_i16X) ,f(z,__x_)    ,f(z,__x_),_ '98
    f(LDY,_imm8) ,f(LDA,_ind8X) ,f(LDX,_imm8) ,f(z,__x_),f(LDY,_z8)   ,f(LDA,_z8)   ,f(LDX,_z8)   ,f(z,__x_),_ 'A0
    u(TAY,_none) ,f(LDA,_imm8)  ,u(TAX,_none) ,f(z,__x_),f(LDY,_a16)  ,f(LDA,_a16)  ,f(LDX,_a16)  ,f(z,__x_),_ 'A8
    f(BCS,_r16)  ,f(LDA,_ind8Y) ,f(z,__x_)    ,f(z,__x_),f(LDY,_i8X)  ,f(LDA,_i8X)  ,f(LDX,_i8Y)  ,f(z,__x_),_ 'B0
    u(CLV,_none) ,f(LDA,_i16Y)  ,u(TSX,_none) ,f(z,__x_),f(LDY,_i16X) ,f(LDA,_i16X) ,f(LDX,_i16Y) ,f(z,__x_),_ 'B8
    f(CPY,_imm8) ,f(CMP,_ind8X) ,f(z,__x_)    ,f(z,__x_),f(CPY,_z8)   ,f(CMP,_z8)   ,f(DEC,_z8)   ,f(z,__x_),_ 'C0
    u(INY,_none) ,f(CMP,_imm8)  ,u(DEX,_none) ,f(z,__x_),f(CPY,_a16)  ,f(CMP,_a16)  ,f(DEC,_a16)  ,f(z,__x_),_ 'C8
    f(BNE,_r16)  ,f(CMP,_ind8Y) ,f(z,__x_)    ,f(z,__x_),f(z,__x_)    ,f(CMP,_i8X)  ,f(DEC,_i8X)  ,f(z,__x_),_ 'D0
    u(CLD,_none) ,f(CMP,_i16Y)  ,f(z,__x_)    ,f(z,__x_),f(z,__x_)    ,f(CMP,_i16X) ,f(DEC,_i16X) ,f(z,__x_),_ 'D8
    f(CPX,_imm8) ,f(SBC,_ind8X) ,f(z,__x_)    ,f(z,__x_),f(CPX,_z8)   ,f(SBC,_z8)   ,f(INC,_z8)   ,f(z,__x_),_ 'E0
    u(INX,_none) ,f(SBC,_imm8)  ,u(NOP,_none) ,f(z,__x_),f(CPX,_a16)  ,f(SBC,_a16)  ,f(INC,_a16)  ,f(z,__x_),_ 'E8
    f(BEQ,_r16)  ,f(SBC,_ind8Y) ,f(z,__x_)    ,f(z,__x_),f(z,__x_)    ,f(SBC,_i8X)  ,f(INC,_i8X)  ,f(z,__x_),_ 'F0
    u(SED,_none) ,f(SBC,_i16Y)  ,f(z,__x_)    ,f(z,__x_),f(z,__x_)    ,f(SBC,_i16X) ,f(INC,_i16X) ,f(z,__x_) } 'F8
  rem ------------------------------------------------------------------------------------------------------------
  #undef u
  #undef f
  
  '--------------- OPCODES --------------
  sub fnOpcode_Unimplemented()
    _Begin  
      color 12 : print "Unimplemented opcode: " & hex(.bOpcode,2) & " > '"+g_zDism(.bOpcode)+"'"
      .bBreak = 1
    _End
  end sub
  sub fnOpcode__x_()
    _Begin      
      color 12 : print "Illegal opcode: " & hex(.bOpcode,2)
      .bBreak = 1
    _End
  end sub
  
  rem ---------------- flags ------------
    sub fnOpcode_CLC() 'Clear Carry
      
      'Flag (Processor Status) Instructions
      'Affect Flags: as noted  
      'These instructions are implied mode, have a length of one byte and require two machine cycles.  
      
      'MNEMONIC                       HEX
      'CLC (CLear Carry)              $18
      'SEC (SEt Carry)                $38
      'CLI (CLear Interrupt)          $58
      'SEI (SEt Interrupt)            $78
      'CLV (CLear oVerflow)           $B8
      'CLD (CLear Decimal)            $D8
      'SED (SEt Decimal)              $F8
      
      _Begin    
        ClearFlag(Carry)    
      _End
      
    end sub
    sub fnOpcode_SEC() 'Set Carry
      
      'Flag (Processor Status) Instructions
      'Affect Flags: as noted  
      'These instructions are implied mode, have a length of one byte and require two machine cycles.  
      
      'MNEMONIC                       HEX
      'SEC (SEt Carry)                $38
      
      _Begin    
        SetFlag(Carry)    
      _End
      
    end sub
    sub fnOpcode_CLI() 'Clear Interrupt
      
      'Flag (Processor Status) Instructions
      'Affect Flags: as noted  
      'These instructions are implied mode, have a length of one byte and require two machine cycles.  
      
      'MNEMONIC                       HEX
      'CLI (CLear Interrupt)          $58
      
      _Begin    
        ClearFlag(Interrupt)    
      _End
      
    end sub
    sub fnOpcode_SEI() 'Set Carry
      
      'Flag (Processor Status) Instructions
      'Affect Flags: as noted  
      'These instructions are implied mode, have a length of one byte and require two machine cycles.  
      
      'MNEMONIC                       HEX
      'SEI (SEt Interrupt)            $78
      
      _Begin    
        SetFlag(Interrupt)    
      _End
      
    end sub
    sub fnOpcode_CLV() 'Clear Overflow
      
      'Flag (Processor Status) Instructions
      'Affect Flags: as noted  
      'These instructions are implied mode, have a length of one byte and require two machine cycles.  
      
      'MNEMONIC                       HEX
      'CLV (CLear oVerflow)           $B8
      
      _Begin    
        ClearFlag(oVerflow)    
      _End
      
    end sub
    sub fnOpcode_CLD() 'Clear Decimal
      
      'Flag (Processor Status) Instructions
      'Affect Flags: as noted  
      'These instructions are implied mode, have a length of one byte and require two machine cycles.  
      
      'MNEMONIC                       HEX
      'CLD (CLear Decimal)            $D8
      
      _Begin    
        ClearFlag(Decimal)    
      _End
      
    end sub
    sub fnOpcode_SED() 'Set Decimal
      
      'Flag (Processor Status) Instructions
      'Affect Flags: as noted  
      'These instructions are implied mode, have a length of one byte and require two machine cycles.  
      
      'MNEMONIC                       HEX
      'SED (SEt Decimal)              $F8
      
      _Begin    
        SetFlag(Decimal)    
      _End
      
    end sub
  rem ---------------- load -------------
    sub fnOpcode_LDA() 'load A from immediate or memory
      
      'LDA (LoaD Accumulator)
      'Affects Flags: N Z
      
      'MODE           SYNTAX       HEX LEN TIM
      'Immediate     LDA #$44      $A9  2   2
      'Zero Page     LDA $44       $A5  2   3
      'Zero Page,X   LDA $44,X     $B5  2   4
      'Absolute      LDA $4400     $AD  3   4
      'Absolute,X    LDA $4400,X   $BD  3   4+
      'Absolute,Y    LDA $4400,Y   $B9  3   4+
      'Indirect,X    LDA ($44,X)   $A1  2   6
      'Indirect,Y    LDA ($44),Y   $B1  2   5+
      
      '+ add 1 cycle if page boundary crossed
      
      _Begin    
        g_fnAddr(.bOpcode)()    
        .RegA = ReadByte(.wMemAddr)    
        CalcFlags_NZ( .RegA )
      _End
      
    end sub
    sub fnOpcode_LDX() 'load X from immediate or memory
      
      'LDX (LoaD X register)
      'Affects Flags: N Z
      
      'MODE           SYNTAX       HEX LEN TIM
      'Immediate     LDX #$44      $A2  2   2
      'Zero Page     LDX $44       $A6  2   3
      'Zero Page,Y   LDX $44,Y     $B6  2   4
      'Absolute      LDX $4400     $AE  3   4
      'Absolute,Y    LDX $4400,Y   $BE  3   4+
      
      '+ add 1 cycle if page boundary crossed
      
      _Begin    
        g_fnAddr(.bOpcode)()    
        .RegX = ReadByte(.wMemAddr)    
        CalcFlags_NZ( .RegX )
      _End
      
    end sub
    sub fnOpcode_LDY() 'load Y from immediate or memory
      
      'LDY (LoaD Y register)
      'Affects Flags: N Z
      
      'MODE           SYNTAX       HEX LEN TIM
      'Immediate     LDY #$44      $A0  2   2
      'Zero Page     LDY $44       $A4  2   3
      'Zero Page,X   LDY $44,X     $B4  2   4
      'Absolute      LDY $4400     $AC  3   4
      'Absolute,X    LDY $4400,X   $BC  3   4+
      
      '+ add 1 cycle if page boundary crossed
      
      _Begin    
        g_fnAddr(.bOpcode)()    
        .RegY = ReadByte(.wMemAddr)    
        CalcFlags_NZ( .RegY )
      _End
      
    end sub
  rem ---------------- save -------------
    sub fnOpcode_STA() 'store A to memory
      
      'STA (STore Accumulator)
      'Affects Flags: none
    
      'MODE           SYNTAX       HEX LEN TIM
      'Zero Page     STA $44       $85  2   3
      'Zero Page,X   STA $44,X     $95  2   4
      'Absolute      STA $4400     $8D  3   4
      'Absolute,X    STA $4400,X   $9D  3   5
      'Absolute,Y    STA $4400,Y   $99  3   5
      'Indirect,X    STA ($44,X)   $81  2   6
      'Indirect,Y    STA ($44),Y   $91  2   6
      
      _Begin    
        g_fnAddr(.bOpcode)()    
        WriteByte(.wMemAddr,.RegA)        
      _End
      
    end sub
    sub fnOpcode_STX() 'store X to memory
      
      'STX (STore X register)
      'Affects Flags: none
      
      'MODE           SYNTAX       HEX LEN TIM
      'Zero Page     STX $44       $86  2   3
      'Zero Page,Y   STX $44,Y     $96  2   4
      'Absolute      STX $4400     $8E  3   4
      
      _Begin    
        g_fnAddr(.bOpcode)()    
        WriteByte(.wMemAddr,.RegX)        
      _End
      
    end sub
    sub fnOpcode_STY() 'store Y to memory
      
      'STY (STore Y register)
      'Affects Flags: none
      
      'MODE           SYNTAX       HEX LEN TIM
      'Zero Page     STY $44       $84  2   3
      'Zero Page,X   STY $44,X     $94  2   4
      'Absolute      STY $4400     $8C  3   4
      
      _Begin    
        g_fnAddr(.bOpcode)()    
        WriteByte(.wMemAddr,.RegY)        
      _End
      
    end sub
  rem -------------- register -----------
    sub fnOpcode_TAX() 'transfer A to X
      
      'Register Instructions
      'Affect Flags: N Z
      
      'These instructions are implied mode, have a length of one byte and require two machine cycles.
      
      'MNEMONIC                 HEX
      'TAX (Transfer A to X)    $AA
      'TXA (Transfer X to A)    $8A
      'DEX (DEcrement X)        $CA
      'INX (INcrement X)        $E8
      'TAY (Transfer A to Y)    $A8
      'TYA (Transfer Y to A)    $98
      'DEY (DEcrement Y)        $88
      'INY (INcrement Y)        $C8
      
      _Begin          
        .RegX = .RegA
        CalcFlags_NZ( .RegX )
      _End
      
    end sub
    sub fnOpcode_TXA() 'transfer X to A
      
      'Register Instructions
      'Affect Flags: N Z
      
      'These instructions are implied mode, have a length of one byte and require two machine cycles.
      
      'MNEMONIC                 HEX
      'TXA (Transfer X to A)    $8A
      
      _Begin          
        .RegA = .RegX
        CalcFlags_NZ( .RegA )
      _End
      
    end sub
    sub fnOpcode_DEX() 'DEcrement X
      
      'Register Instructions
      'Affect Flags: N Z
      
      'These instructions are implied mode, have a length of one byte and require two machine cycles.
      
      'MNEMONIC                 HEX
      'DEX (DEcrement X)        $CA
          
      _Begin          
        .RegX -= 1
        CalcFlags_NZ( .RegX )
      _End
      
    end sub
    sub fnOpcode_INX() 'INcrement X
      
      'Register Instructions
      'Affect Flags: N Z
      
      'These instructions are implied mode, have a length of one byte and require two machine cycles.
      
      'MNEMONIC                 HEX
      'INX (INcrement X)        $E8
      
      _Begin          
        .RegX += 1
        CalcFlags_NZ( .RegX )
      _End
      
    end sub
    sub fnOpcode_TAY() 'transfer A to Y
      
      'Register Instructions
      'Affect Flags: N Z
      
      'These instructions are implied mode, have a length of one byte and require two machine cycles.
      
      'MNEMONIC                 HEX
      'TAY (Transfer A to Y)    $A8    
      
      _Begin          
        .RegY = .RegA
        CalcFlags_NZ( .RegY )
      _End
      
    end sub
    sub fnOpcode_TYA() 'transfer Y to A
      
      'Register Instructions
      'Affect Flags: N Z
      
      'These instructions are implied mode, have a length of one byte and require two machine cycles.
      
      'MNEMONIC                 HEX
      'TYA (Transfer Y to A)    $98
      
      _Begin          
        .RegA = .RegY
        CalcFlags_NZ( .RegA )
      _End
      
    end sub
    sub fnOpcode_DEY() 'DEcrement Y
      
      'Register Instructions
      'Affect Flags: N Z
      
      'These instructions are implied mode, have a length of one byte and require two machine cycles.
      
      'MNEMONIC                 HEX
      'DEY (DEcrement Y)        $88    
      
      _Begin          
        .RegY -= 1
        CalcFlags_NZ( .RegY )
      _End
      
    end sub
    sub fnOpcode_INY() 'INcrement Y
      
      'Register Instructions
      'Affect Flags: N Z
      
      'These instructions are implied mode, have a length of one byte and require two machine cycles.
      
      'MNEMONIC                 HEX
      'INY (INcrement Y)        $C8
      
      _Begin          
        .RegY += 1
        CalcFlags_NZ( .RegY )
      _End
      
    end sub
  rem --------------  memory ------------
    sub fnOpcode_INC() 'Increment memory
      
      'INC (INCrement memory)
      'Affects Flags: N Z
      
      'MODE           SYNTAX       HEX LEN TIM
      'Zero Page     INC $44       $E6  2   5
      'Zero Page,X   INC $44,X     $F6  2   6
      'Absolute      INC $4400     $EE  3   6
      'Absolute,X    INC $4400,X   $FE  3   7
      
      _Begin    
        g_fnAddr(.bOpcode)()
        dim as ubyte Resu = ReadByte(.wMemAddr)+1
        WriteByte(.wMemAddr,Resu)
        CalcFlags_NZ(Resu)
      _End
      
    end sub
    sub fnOpcode_DEC() 'Increment memory
      
      'INC (INCrement memory)
      'Affects Flags: N Z
      
      'MODE           SYNTAX       HEX LEN TIM
      'Zero Page     INC $44       $E6  2   5
      'Zero Page,X   INC $44,X     $F6  2   6
      'Absolute      INC $4400     $EE  3   6
      'Absolute,X    INC $4400,X   $FE  3   7
      
      _Begin    
        g_fnAddr(.bOpcode)()
        dim as ubyte Resu = ReadByte(.wMemAddr)-1
        WriteByte(.wMemAddr,Resu)
        CalcFlags_NZ(Resu)
      _End
      
    end sub
    sub fnOpcode_BIT() 'Bit Test memory
      
      'BIT (test BITs)
      'Affects Flags: N V Z

      'MODE           SYNTAX       HEX LEN TIM
      'Zero Page     BIT $44       $24  2   3
      'Absolute      BIT $4400     $2C  3   4

      'BIT sets the Z flag as though the value in the address tested were ANDed with the accumulator. 
      'The N and V flags are set to match bits 7 and 6 respectively in the value stored at the tested address.
      
      _Begin    
        g_fnAddr(.bOpcode)()
        var bResu = ReadByte(.wMemAddr)
        CalcFlag_Zero(.RegA and bResu)
        CalcFlag_Negative(bResu)        
        CalcFlag_Overflow6(bResu)
      _End
      
    end sub
  rem ---------------- math -------------
    sub fnOpcode_ADC() 'ADC A with memory or imm
      
      'ADC (ADd with Carry)
      'Affects Flags: N V Z C
      
      'MODE           SYNTAX       HEX LEN TIM
      'Immediate     ADC #$44      $69  2   2
      'Zero Page     ADC $44       $65  2   3
      'Zero Page,X   ADC $44,X     $75  2   4
      'Absolute      ADC $4400     $6D  3   4
      'Absolute,X    ADC $4400,X   $7D  3   4+
      'Absolute,Y    ADC $4400,Y   $79  3   4+
      'Indirect,X    ADC ($44,X)   $61  2   6
      'Indirect,Y    ADC ($44),Y   $71  2   5+
      
      '+ add 1 cycle if page boundary crossed
      
      _Begin    
        g_fnAddr(.bOpcode)()
        dim as ulong Resu = .RegA + ReadByte(.wMemAddr) + GetFlag(Carry)      
        CalcFlags_NZCV(Resu)
        'CalcFlag_oVerflow( .RegA , Resu )
        .RegA = Resu
      _End
      
    end sub
    sub fnOpcode_SBC() 'SBC A with memory or imm
      
      'SBC (SuBtract with Carry)
      'Affects Flags: N V Z C
      
      'MODE           SYNTAX       HEX LEN TIM
      'Immediate     SBC #$44      $E9  2   2
      'Zero Page     SBC $44       $E5  2   3
      'Zero Page,X   SBC $44,X     $F5  2   4
      'Absolute      SBC $4400     $ED  3   4
      'Absolute,X    SBC $4400,X   $FD  3   4+
      'Absolute,Y    SBC $4400,Y   $F9  3   4+
      'Indirect,X    SBC ($44,X)   $E1  2   6
      'Indirect,Y    SBC ($44),Y   $F1  2   5+
      
      '+ add 1 cycle if page boundary crossed
      
      _Begin    
        g_fnAddr(.bOpcode)()
        dim as ulong Resu = .RegA - (ReadByte(.wMemAddr) + (1-GetFlag(Carry)))
        CalcFlags_NZ(Resu)
        CalcFlag_XCarry(Resu)
        CalcFlag_oVerflow( Resu )
        'CalcFlag_oVerflow( .RegA , Resu )
        .RegA = Resu
      _End
      
    end sub
    sub fnOpcode_CMP() 'Compare A with memory or imm
      
      'CMP (CoMPare accumulator)
      'Affects Flags: N Z C
      
      'MODE           SYNTAX       HEX LEN TIM
      'Immediate     CMP #$44      $C9  2   2
      'Zero Page     CMP $44       $C5  2   3
      'Zero Page,X   CMP $44,X     $D5  2   4
      'Absolute      CMP $4400     $CD  3   4
      'Absolute,X    CMP $4400,X   $DD  3   4+
      'Absolute,Y    CMP $4400,Y   $D9  3   4+
      'Indirect,X    CMP ($44,X)   $C1  2   6
      'Indirect,Y    CMP ($44),Y   $D1  2   5+
      
      '+ add 1 cycle if page boundary crossed
      
      _Begin    
        g_fnAddr(.bOpcode)()
        dim as ulong Resu = .RegA - ReadByte(.wMemAddr)
        CalcFlags_NZ(Resu)
        CalcFlag_XCarry(Resu)
      _End
      
    end sub
    sub fnOpcode_CPX() 'Compare X with memory or imm
      
      'CPX (ComPare X register)
      'Affects Flags: N Z C
      
      'MODE           SYNTAX       HEX LEN TIM
      'Immediate     CPX #$44      $E0  2   2
      'Zero Page     CPX $44       $E4  2   3
      'Absolute      CPX $4400     $EC  3   4
      
      _Begin    
        g_fnAddr(.bOpcode)()
        dim as ulong Resu = .RegX - ReadByte(.wMemAddr)
        CalcFlags_NZ(Resu)
        CalcFlag_XCarry(Resu)
      _End
      
    end sub
    sub fnOpcode_CPY() 'Compare Y with memory or imm
      
      'CPY (ComPare Y register)
      'Affects Flags: N Z C
      
      'MODE           SYNTAX       HEX LEN TIM
      'Immediate     CPY #$44      $C0  2   2
      'Zero Page     CPY $44       $C4  2   3
      'Absolute      CPY $4400     $CC  3   4
      
      _Begin    
        g_fnAddr(.bOpcode)()
        dim as ulong Resu = .RegY - ReadByte(.wMemAddr)
        CalcFlags_NZ(Resu)
        CalcFlag_XCarry(Resu)
      _End
      
    end sub
    sub fnOpcode_AND() 'AND with memory or imm
      
      'AND (bitwise AND with accumulator)
      'Affects Flags: N Z
      
      'MODE           SYNTAX       HEX LEN TIM
      'Immediate     AND #$44      $29  2   2
      'Zero Page     AND $44       $25  2   3
      'Zero Page,X   AND $44,X     $35  2   4
      'Absolute      AND $4400     $2D  3   4
      'Absolute,X    AND $4400,X   $3D  3   4+
      'Absolute,Y    AND $4400,Y   $39  3   4+
      'Indirect,X    AND ($44,X)   $21  2   6
      'Indirect,Y    AND ($44),Y   $31  2   5+
      
      '+ add 1 cycle if page boundary crossed
      
      _Begin    
        g_fnAddr(.bOpcode)()      
        .RegA = ReadByte(.wMemAddr) and .RegA
        CalcFlags_NZ(.RegA)
      _End
      
    end sub
    sub fnOpcode_ORA() 'ORA with memory or imm
      
      'ORA (bitwise OR with Accumulator)
      'Affects Flags: N Z
      
      'MODE           SYNTAX       HEX LEN TIM
      'Immediate     ORA #$44      $09  2   2
      'Zero Page     ORA $44       $05  2   3
      'Zero Page,X   ORA $44,X     $15  2   4
      'Absolute      ORA $4400     $0D  3   4
      'Absolute,X    ORA $4400,X   $1D  3   4+
      'Absolute,Y    ORA $4400,Y   $19  3   4+
      'Indirect,X    ORA ($44,X)   $01  2   6
      'Indirect,Y    ORA ($44),Y   $11  2   5+
      
      '+ add 1 cycle if page boundary crossed
      
      _Begin    
        g_fnAddr(.bOpcode)()
        .RegA = ReadByte(.wMemAddr) or .RegA      
        CalcFlags_NZ(.RegA)
      _End
      
    end sub
    sub fnOpcode_EOR() 'XOR with memory or imm
      
      'EOR (bitwise Exclusive OR)
      'Affects Flags: N Z
      
      'MODE           SYNTAX       HEX LEN TIM
      'Immediate     EOR #$44      $49  2   2
      'Zero Page     EOR $44       $45  2   3
      'Zero Page,X   EOR $44,X     $55  2   4
      'Absolute      EOR $4400     $4D  3   4
      'Absolute,X    EOR $4400,X   $5D  3   4+
      'Absolute,Y    EOR $4400,Y   $59  3   4+
      'Indirect,X    EOR ($44,X)   $41  2   6
      'Indirect,Y    EOR ($44),Y   $51  2   5+
      
      '+ add 1 cycle if page boundary crossed
      
      _Begin    
        g_fnAddr(.bOpcode)()
        .RegA = ReadByte(.wMemAddr) xor .RegA      
        CalcFlags_NZ(.RegA)
      _End
      
    end sub
  rem --------------- branch ------------
    sub fnOpcode_BPL() 'Branch if Negative Clear (plus)
      
      'Branches are dependant on the status of the flag bits when the op code is encountered.
      'A branch not taken requires two machine cycles. Add one if the branch is taken 
      'and add one more if the branch crosses a page boundary.
    
      'MNEMONIC                       HEX
      'BPL (Branch on PLus)           $10
      'BMI (Branch on MInus)          $30
      'BVC (Branch on oVerflow Clear) $50
      'BVS (Branch on oVerflow Set)   $70
      'BCC (Branch on Carry Clear)    $90
      'BCS (Branch on Carry Set)      $B0
      'BNE (Branch on Not Equal)      $D0
      'BEQ (Branch on EQual)          $F0
      
      _Begin
        g_fnAddr(.bOpcode)()    
        if TestFlag(Negative)=0 then .RegPC = .wMemAddr
      _End
      
    end sub
    sub fnOpcode_BMI() 'Branch if Negative Set (minus)
      
      'Branches are dependant on the status of the flag bits when the op code is encountered.
      'A branch not taken requires two machine cycles. Add one if the branch is taken 
      'and add one more if the branch crosses a page boundary.
    
      'MNEMONIC                       HEX
      'BMI (Branch on MInus)          $30
      
      _Begin
        g_fnAddr(.bOpcode)()    
        if TestFlag(Negative) then .RegPC = .wMemAddr
      _End
      
    end sub
    sub fnOpcode_BVC() 'Branch if oVerflow Clear
      
      'Branches are dependant on the status of the flag bits when the op code is encountered.
      'A branch not taken requires two machine cycles. Add one if the branch is taken 
      'and add one more if the branch crosses a page boundary.
    
      'MNEMONIC                       HEX
      'BVC (Branch on oVerflow Clear) $50
      
      _Begin
        g_fnAddr(.bOpcode)()    
        if TestFlag(Overflow)=0 then .RegPC = .wMemAddr
      _End
      
    end sub
    sub fnOpcode_BVS() 'Branch if oVerflow Set
      
      'Branches are dependant on the status of the flag bits when the op code is encountered.
      'A branch not taken requires two machine cycles. Add one if the branch is taken 
      'and add one more if the branch crosses a page boundary.
    
      'MNEMONIC                       HEX
      'BVS (Branch on oVerflow Set)   $70
      
      _Begin
        g_fnAddr(.bOpcode)()    
        if TestFlag(oVerflow) then .RegPC = .wMemAddr
      _End
      
    end sub
    sub fnOpcode_BCC() 'Branch if Carry Clear
      
      'Branches are dependant on the status of the flag bits when the op code is encountered.
      'A branch not taken requires two machine cycles. Add one if the branch is taken 
      'and add one more if the branch crosses a page boundary.
    
      'MNEMONIC                       HEX
      'BCC (Branch on Carry Clear)    $90
      
      _Begin
        g_fnAddr(.bOpcode)()    
        if TestFlag(Carry)=0 then .RegPC = .wMemAddr
      _End
      
    end sub
    sub fnOpcode_BCS() 'Branch if Carry Set
      
      'Branches are dependant on the status of the flag bits when the op code is encountered.
      'A branch not taken requires two machine cycles. Add one if the branch is taken 
      'and add one more if the branch crosses a page boundary.
    
      'MNEMONIC                       HEX
      'BCS (Branch on Carry Set)      $B0
      
      _Begin
        g_fnAddr(.bOpcode)()    
        if TestFlag(Carry) then .RegPC = .wMemAddr
      _End
      
    end sub
    sub fnOpcode_BNE() 'Branch if Zero Clear (Not Equal)
      
      'Branches are dependant on the status of the flag bits when the op code is encountered.
      'A branch not taken requires two machine cycles. Add one if the branch is taken 
      'and add one more if the branch crosses a page boundary.
    
      'MNEMONIC                       HEX
      'BNE (Branch on Not Equal)      $D0
      
      _Begin
        g_fnAddr(.bOpcode)()    
        if TestFlag(Zero)=0 then .RegPC = .wMemAddr
      _End
      
    end sub
    sub fnOpcode_BEQ() 'Branch if Zero Set  (Equal)
      
      'Branches are dependant on the status of the flag bits when the op code is encountered.
      'A branch not taken requires two machine cycles. Add one if the branch is taken 
      'and add one more if the branch crosses a page boundary.
    
      'MNEMONIC                       HEX
      'BEQ (Branch on EQual)          $F0
      
      _Begin
        g_fnAddr(.bOpcode)()    
        if TestFlag(Zero) then .RegPC = .wMemAddr
      _End
      
    end sub  
    sub fnOpcode_JMP() 'Jump to location
      
      'JMP (JuMP)
      'Affects Flags: none
      
      'MODE           SYNTAX       HEX LEN TIM
      'Absolute      JMP $5597     $4C  3   3
      'Indirect      JMP ($5597)   $6C  3   5
      
      _Begin
        g_fnAddr(.bOpcode)()
        .RegPC = .wMemAddr
      _End
      
    end sub  
    sub fnOpcode_JSR() 'Jump to Subroutine
      
      'JSR (Jump to SubRoutine)
      'Affects Flags: none
      
      'MODE           SYNTAX       HEX LEN TIM
      'Absolute      JSR $5597     $20  3   6
      
      _Begin          
        g_fnAddr(.bOpcode)()
        .RegSP -= 2
        WriteWord((&h100+.RegSP),.RegPC-1)
        .RegPC = .wMemAddr
      _End    
      
    end sub
    sub fnOpcode_RTS() 'Return from Subroutine
      
      'Affects Flags: none
  
      'MODE           SYNTAX       HEX LEN TIM
      'Implied       RTS           $60  1   6
      
      _Begin
        if .RegSP = &hFF then
          color 14 : print "Stack is empty!"
          .bBreak = 1 : exit sub
        end if
        .RegPC = ReadWord((&h100+.RegSP))+1
        .RegSP += 2      
      _End    
      
    end sub
    
  rem --------------- shift -------------
    sub fnOpcodeASLA() 'Arithmetic Shift Left Accumulator
      
      'ASL (Arithmetic Shift Left)
      'Affects Flags: N Z C
      
      'MODE           SYNTAX       HEX LEN TIM
      'Accumulator   ASL A         $0A  1   2
      
      'ASL shifts all bits left one position. 0 is shifted into bit 0 and the original bit 7 is shifted into the Carry.
      
      _Begin
        var Resu = culng(.RegA) shl 1
        .RegA = Resu
        CalcFlags_NZC(Resu)
      _End
      
    end sub
    sub fnOpcode_ASL() 'Arithmetic Shift Left Memory
      
      'ASL (Arithmetic Shift Left)
      'Affects Flags: N Z C
      
      'MODE           SYNTAX       HEX LEN TIM
      'Zero Page     ASL $44       $06  2   5
      'Zero Page,X   ASL $44,X     $16  2   6
      'Absolute      ASL $4400     $0E  3   6
      'Absolute,X    ASL $4400,X   $1E  3   7
      
      'ASL shifts all bits left one position. 0 is shifted into bit 0 and the original bit 7 is shifted into the Carry.
      
      _Begin    
        g_fnAddr(.bOpcode)()    
        var Resu = culng(ReadByte(.wMemAddr)) shl 1
        WriteByte(.wMemAddr,Resu)
        CalcFlags_NZC(Resu)
      _End
      
    end sub    
    sub fnOpcodeLSRA() 'Shift Right Accumulator
      
      'LSR (Logical Shift Right)
      'Affects Flags: N Z C
      
      'MODE           SYNTAX       HEX LEN TIM
      'Accumulator   LSR A         $4A  1   2
      
      'LSR shifts all bits right one position. 0 is shifted into bit 7 
      'and the original bit 0 is shifted into the Carry.
      
      _Begin      
        GetFlag(Carry) = (.RegA and 1)
        .RegA = (.RegA shr 1)      
        CalcFlags_NZ(.RegA)
      _End
      
    end sub
    sub fnOpcode_LSR() 'Shift Right
      
      'LSR (Logical Shift Right)
      'Affects Flags: N Z C
      
      'MODE           SYNTAX       HEX LEN TIM
      'Zero Page     LSR $44       $46  2   5
      'Zero Page,X   LSR $44,X     $56  2   6
      'Absolute      LSR $4400     $4E  3   6
      'Absolute,X    LSR $4400,X   $5E  3   7
      
      'LSR shifts all bits right one position. 0 is shifted into bit 7 
      'and the original bit 0 is shifted into the Carry.
      
      _Begin    
        g_fnAddr(.bOpcode)()    
        var Resu = ReadByte(.wMemAddr)
        GetFlag(Carry) = (Resu and 1)
        Resu = (Resu shr 1)
        WriteByte(.wMemAddr,Resu)      
        CalcFlags_NZ(Resu)
      _End
      
    end sub    
    sub fnOpcodeRORA() 'Rotate Right (trough carry) Accumulator
      
      'ROR (ROtate Right)
      'Affects Flags: N Z C
      
      'MODE           SYNTAX       HEX LEN TIM
      'Accumulator   ROR A         $6A  1   2
      
      'ROR shifts all bits right one position. The Carry is shifted into bit 7 
      'and the original bit 0 is shifted into the Carry.
      
      _Begin
        var Src = .RegA
        .RegA = (.RegA shr 1) or (GetFlag(Carry) shl 7)      
        GetFlag(Carry) = (Src and 1)
        CalcFlags_NZ(.RegA)
      _End
      
    end sub
    sub fnOpcode_ROR() 'Rotate Right (trough carry)
      
      'ROR (ROtate Right)
      'Affects Flags: N Z C
      
      'MODE           SYNTAX       HEX LEN TIM
      'Zero Page     ROR $44       $66  2   5
      'Zero Page,X   ROR $44,X     $76  2   6
      'Absolute      ROR $4400     $6E  3   6
      'Absolute,X    ROR $4400,X   $7E  3   7
      
      'ROR shifts all bits right one position. The Carry is shifted into bit 7 
      'and the original bit 0 is shifted into the Carry.
      
      _Begin    
        g_fnAddr(.bOpcode)()    
        var Src = ReadByte(.wMemAddr)
        var Resu = (Src shr 1) or (GetFlag(Carry) shl 7)
        WriteByte(.wMemAddr,Resu)
        GetFlag(Carry) = (Src and 1)
        CalcFlags_NZ(Resu)
      _End
      
    end sub
    sub fnOpcodeROLA() 'Rotate Left (trough carry) accumulator
      
      'ROL (ROtate Left)
      'Affects Flags: N Z C
  
      'MODE           SYNTAX       HEX LEN TIM
      'Accumulator   ROL A         $2A  1   2
  
      'ROL shifts all bits left one position. The Carry is shifted into bit 0 
      'and the original bit 7 is shifted into the Carry.
      
      _Begin
        var Resu = (culng(.RegA) shl 1) or GetFlag(Carry)
        .RegA = Resu
        CalcFlags_NZC(Resu)
      _End
      
    end sub
    sub fnOpcode_ROL() 'Rotate Left (trough carry)
      
      'ROL (ROtate Left)
      'Affects Flags: N Z C
  
      'MODE           SYNTAX       HEX LEN TIM
      'Zero Page     ROL $44       $26  2   5
      'Zero Page,X   ROL $44,X     $36  2   6
      'Absolute      ROL $4400     $2E  3   6
      'Absolute,X    ROL $4400,X   $3E  3   7
  
      'ROL shifts all bits left one position. The Carry is shifted into bit 0 
      'and the original bit 7 is shifted into the Carry.
      
      _Begin    
        g_fnAddr(.bOpcode)()    
        var Resu = (culng(ReadByte(.wMemAddr)) shl 1) or GetFlag(Carry)
        WriteByte(.wMemAddr,Resu)
        CalcFlags_NZC(Resu)
      _End
      
    end sub
  rem --------------- stack -------------
    sub fnOpcode_TXS() 'transfer X to SP
      
      'Stack Instructions
      'These instructions are implied mode, have a length of one byte 
      'and require machine cycles as indicated. 
      'The "PuLl" operations are known as "POP" on most other microprocessors.
      'With the 6502, the stack is always on page one ($100-$1FF) and works top down.
  
      'MNEMONIC                        HEX TIM
      'TXS (Transfer X to Stack ptr)   $9A  2
      'TSX (Transfer Stack ptr to X)   $BA  2
      'PHA (PusH Accumulator)          $48  3
      'PLA (PuLl Accumulator)          $68  4
      'PHP (PusH Processor status)     $08  3
      'PLP (PuLl Processor status)     $28  4
      
      _Begin          
        .RegSP = .RegX
      _End
      
    end sub
    sub fnOpcode_TSX() 'transfer SP to X
      
      'Stack Instructions
      'These instructions are implied mode, have a length of one byte 
      'and require machine cycles as indicated. 
      'The "PuLl" operations are known as "POP" on most other microprocessors.
      'With the 6502, the stack is always on page one ($100-$1FF) and works top down.
  
      'MNEMONIC                        HEX TIM
      'TSX (Transfer Stack ptr to X)   $BA  2
      
      _Begin          
        .RegX = .RegSP
      _End
      
    end sub
    sub fnOpcode_PHA() 'Push Accumulator 
      
      'Stack Instructions
      'These instructions are implied mode, have a length of one byte 
      'and require machine cycles as indicated. 
      'The "PuLl" operations are known as "POP" on most other microprocessors.
      'With the 6502, the stack is always on page one ($100-$1FF) and works top down.
  
      'MNEMONIC                        HEX TIM
      'PHA (PusH Accumulator)          $48  3
      'PLA (PuLl Accumulator)          $68  4
      'PHP (PusH Processor status)     $08  3
      'PLP (PuLl Processor status)     $28  4
      
      _Begin          
        .RegSP -= 1
        WriteByteFast((&h100+.RegSP),.RegA)
      _End    
      
    end sub
    sub fnOpcode_PLA() 'Pull Accumulator 
      
      'Stack Instructions
      'These instructions are implied mode, have a length of one byte 
      'and require machine cycles as indicated. 
      'The "PuLl" operations are known as "POP" on most other microprocessors.
      'With the 6502, the stack is always on page one ($100-$1FF) and works top down.
  
      'MNEMONIC                        HEX TIM
      'PLA (PuLl Accumulator)          $68  4
      
      _Begin          
        .RegA = ReadByteFast((&h100+.RegSP))
        .RegSP += 1      
      _End    
      
    end sub
    sub fnOpcode_PHP() 'Push Processor status (flags)
      
      'Stack Instructions
      'These instructions are implied mode, have a length of one byte 
      'and require machine cycles as indicated. 
      'The "PuLl" operations are known as "POP" on most other microprocessors.
      'With the 6502, the stack is always on page one ($100-$1FF) and works top down.
  
      'MNEMONIC                        HEX TIM
      'PHP (PusH Processor status)     $08  3
      
      
      _Begin          
        .RegSP -= 1
        WriteByteFast((&h100+.RegSP),.RegP.bFlags)
      _End    
      
    end sub
    sub fnOpcode_PLP() 'Pull Processor status (flags)
      
      'Stack Instructions
      'These instructions are implied mode, have a length of one byte 
      'and require machine cycles as indicated. 
      'The "PuLl" operations are known as "POP" on most other microprocessors.
      'With the 6502, the stack is always on page one ($100-$1FF) and works top down.
  
      'MNEMONIC                        HEX TIM
      'PLP (PuLl Processor status)     $28  4
      
      _Begin          
        .RegP.bFlags = ReadByteFast((&h100+.RegSP))
        .RegSP += 1      
      _End    
      
    end sub
  rem --------------- other -------------
    sub fnOpcode_BRK()
      _Begin        
        if 0 then '.bDebug then 
          if g_bBreakPoint = .RegPC-1 then 
            .bBreak = 1 : g_bBreakPoint = -1
          else
            g_bBreakPoint = .RegPC-1 : .RegPC -= 1       
          end if
        else
          .bBreak = 1
        end if        
      _End
    end sub
    sub fnOpcode_NOP()
      rem nope...
    end sub
  rem -----------------------------------
  
  #define u(_n_) @fnOpcode_Unimplemented
  #define f(_n_) @fnOpcode##_n_
  static shared as sub() g_fnOpcode(255) = { _
    _'  0       1       2       3       4       5       6       7        
    _'  8       9       A       B       C       D       E       F        
    f(_BRK),f(_ORA),f(__x_),f(__x_),f(__x_),f(_ORA),f(_ASL),f(__x_),_ '00
    f(_PHP),f(_ORA),f(ASLA),f(__x_),f(__x_),f(_ORA),f(_ASL),f(__x_),_ '08
    f(_BPL),f(_ORA),f(__x_),f(__x_),f(__x_),f(_ORA),f(_ASL),f(__x_),_ '10
    f(_CLC),f(_ORA),f(__x_),f(__x_),f(__x_),f(_ORA),f(_ASL),f(__x_),_ '18
    f(_JSR),f(_AND),f(__x_),f(__x_),f(_BIT),f(_AND),f(_ROL),f(__x_),_ '20
    f(_PLP),f(_AND),f(ROLA),f(__x_),f(_BIT),f(_AND),f(_ROL),f(__x_),_ '28
    f(_BMI),f(_AND),f(__x_),f(__x_),f(__x_),f(_AND),f(_ROL),f(__x_),_ '30
    f(_SEC),f(_AND),f(__x_),f(__x_),f(__x_),f(_AND),f(_ROL),f(__x_),_ '38
    u(_RTI),f(_EOR),f(__x_),f(__x_),f(__x_),f(_EOR),f(_LSR),f(__x_),_ '40
    f(_PHA),f(_EOR),f(LSRA),f(__x_),f(_JMP),f(_EOR),f(_LSR),f(__x_),_ '48
    f(_BVC),f(_EOR),f(__x_),f(__x_),f(__x_),f(_EOR),f(_LSR),f(__x_),_ '50
    f(_CLI),f(_EOR),f(__x_),f(__x_),f(__x_),f(_EOR),f(_LSR),f(__x_),_ '58
    f(_RTS),f(_ADC),f(__x_),f(__x_),f(__x_),f(_ADC),f(_ROR),f(__x_),_ '60
    f(_PLA),f(_ADC),f(RORA),f(__x_),f(_JMP),f(_ADC),f(_ROR),f(__x_),_ '68
    f(_BVS),f(_ADC),f(__x_),f(__x_),f(__x_),f(_ADC),f(_ROR),f(__x_),_ '70
    f(_SEI),f(_ADC),f(__x_),f(__x_),f(__x_),f(_ADC),f(_ROR),f(__x_),_ '78
    f(__x_),f(_STA),f(__x_),f(__x_),f(_STY),f(_STA),f(_STX),f(__x_),_ '80
    f(_DEY),f(__x_),f(_TXA),f(__x_),f(_STY),f(_STA),f(_STX),f(__x_),_ '88
    f(_BCC),f(_STA),f(__x_),f(__x_),f(_STY),f(_STA),f(_STX),f(__x_),_ '90
    f(_TYA),f(_STA),f(_TXS),f(__x_),f(__x_),f(_STA),f(__x_),f(__x_),_ '98
    f(_LDY),f(_LDA),f(_LDX),f(__x_),f(_LDY),f(_LDA),f(_LDX),f(__x_),_ 'A0
    f(_TAY),f(_LDA),f(_TAX),f(__x_),f(_LDY),f(_LDA),f(_LDX),f(__x_),_ 'A8
    f(_BCS),f(_LDA),f(__x_),f(__x_),f(_LDY),f(_LDA),f(_LDX),f(__x_),_ 'B0
    f(_CLV),f(_LDA),f(_TSX),f(__x_),f(_LDY),f(_LDA),f(_LDX),f(__x_),_ 'B8
    f(_CPY),f(_CMP),f(__x_),f(__x_),f(_CPY),f(_CMP),f(_DEC),f(__x_),_ 'C0
    f(_INY),f(_CMP),f(_DEX),f(__x_),f(_CPY),f(_CMP),f(_DEC),f(__x_),_ 'C8
    f(_BNE),f(_CMP),f(__x_),f(__x_),f(__x_),f(_CMP),f(_DEC),f(__x_),_ 'D0
    f(_CLD),f(_CMP),f(__x_),f(__x_),f(__x_),f(_CMP),f(_DEC),f(__x_),_ 'D8
    f(_CPX),f(_SBC),f(__x_),f(__x_),f(_CPX),f(_SBC),f(_INC),f(__x_),_ 'E0
    f(_INX),f(_SBC),f(_NOP),f(__x_),f(_CPX),f(_SBC),f(_INC),f(__x_),_ 'E8
    f(_BEQ),f(_SBC),f(__x_),f(__x_),f(__x_),f(_SBC),f(_INC),f(__x_),_ 'F0
    f(_SED),f(_SBC),f(__x_),f(__x_),f(__x_),f(_SBC),f(_INC),f(__x_) } 'F8
  rem -------------------------------------------------------------------
  #undef u
  #undef f
  
end namespace
