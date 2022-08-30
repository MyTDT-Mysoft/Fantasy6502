static shared as byte g_LenDisasm(255) = { _
 _'0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F      
   1, 2,-1,-1,-1, 2, 2,-1, 1, 2, 1,-1,-1, 3, 3,-1, _ '0
   2, 2,-1,-1,-1, 2, 2,-1, 1, 3,-1,-1,-1, 3, 3,-1, _ '1
   3, 2,-1,-1, 2, 2, 2,-1, 1, 2, 1,-1, 3, 3, 3,-1, _ '2
   2, 2,-1,-1,-1, 2, 2,-1, 1, 3,-1,-1,-1, 3, 3,-1, _ '3
   1, 2,-1,-1,-1, 2, 2,-1, 1, 2, 1,-1, 3, 3, 3,-1, _ '4
   2, 2,-1,-1,-1, 2, 2,-1, 1, 3,-1,-1,-1, 3, 3,-1, _ '5
   1, 2,-1,-1,-1, 2, 2,-1, 1, 2, 1,-1, 3, 3, 3,-1, _ '6
   2, 2,-1,-1,-1, 2, 2,-1, 1, 3,-1,-1,-1, 3, 3,-1, _ '7
   -1,2,-1,-1, 2, 2, 2,-1, 1,-1, 1,-1, 3, 3, 3,-1, _ '8
   2, 2,-1,-1, 2, 2, 2,-1, 1, 3, 1,-1,-1, 3,-1,-1, _ '9
   2, 2, 2,-1, 2, 2, 2,-1, 1, 2, 1,-1, 3, 3, 3,-1, _ 'A
   2, 2,-1,-1, 2, 2, 2,-1, 1, 3, 1,-1, 3, 3, 3,-1, _ 'B
   2, 2,-1,-1, 2, 2, 2,-1, 1, 2, 1,-1, 3, 3, 3,-1, _ 'C
   2, 2,-1,-1,-1, 2, 2,-1, 1, 3,-1,-1,-1, 3, 3,-1, _ 'D
   2, 2,-1,-1, 2, 2, 2,-1, 1, 2, 1,-1, 3, 3, 3,-1, _ 'E
   2, 2,-1,-1,-1, 2, 2,-1, 1, 3,-1,-1,-1, 3, 3,-1 }  'F
rem ------------------------------------------------------------------------------------------

static shared as zstring*12 g_zDism(255) = { _
  _'  0           1              2      3      4             5             6          7       
  _'  8           9              A      B      C             D             E          F       
  "BRK"      ,"ORA ($xx,X)",""        ,"" ,""           ,"ORA $xx"    ,"ASL $xx"    ,"", _ '00
  "PHP"      ,"ORA #$xx"   ,"ASL  A"  ,"" ,""           ,"ORA $xxxx"  ,"ASL $xxxx"  ,"", _ '08
  "BPL $xx@@","ORA ($xx),Y",""        ,"" ,""           ,"ORA $xx,X"  ,"ASL $xx,X"  ,"", _ '10
  "CLC"      ,"ORA $xxxx,Y",""        ,"" ,""           ,"ORA $xxxx,X","ASL $xxxx,X","", _ '18
  "JSR $xxxx","AND ($xx,X)",""        ,"" ,"BIT $xx"    ,"AND $xx"    ,"ROL $xx"    ,"", _ '20
  "PLP"      ,"AND #$xx"   ,"ROL  A"  ,"" ,"BIT $xxxx"  ,"AND $xxxx"  ,"ROL $xxxx"  ,"", _ '28
  "BMI $xx@@","AND ($xx),Y",""        ,"" ,""           ,"AND $xx,X"  ,"ROL $xx,X"  ,"", _ '30
  "SEC"      ,"AND $xxxx,Y",""        ,"" ,""           ,"AND $xxxx,X","ROL $xxxx,X","", _ '38
  "RTI"      ,"EOR ($xx,X)",""        ,"" ,""           ,"EOR $xx"    ,"LSR $xx"    ,"", _ '40
  "PHA"      ,"EOR #$xx"   ,"LSR  A"  ,"" ,"JMP $xxxx"  ,"EOR $xxxx"  ,"LSR $xxxx"  ,"", _ '48
  "BVC $xx@@","EOR ($xx),Y",""        ,"" ,""           ,"EOR $xx,X"  ,"LSR $xx,X"  ,"", _ '50
  "CLI"      ,"EOR $xxxx,Y",""        ,"" ,""           ,"EOR $xxxx,X","LSR $xxxx,X","", _ '58
  "RTS"      ,"ADC ($xx,X)",""        ,"" ,""           ,"ADC $xx"    ,"ROR $xx"    ,"", _ '60
  "PLA"      ,"ADC #$xx"   ,"ROR  A"  ,"" ,"JMP ($xxxx)","ADC $xxxx"  ,"ROR $xxxx"  ,"", _ '68
  "BVS $xx@@","ADC ($xx),Y",""        ,"" ,""           ,"ADC $xx,X"  ,"ROR $xx,X"  ,"", _ '70
  "SEI"      ,"ADC $xxxx,Y",""        ,"" ,""           ,"ADC $xxxx,X","ROR $xxxx,X","", _ '78
  ""         ,"STA ($xx,X)",""        ,"" ,"STY $xx"    ,"STA $xx"    ,"STX $xx"    ,"", _ '80
  "DEY"      ,""           ,"TXA"     ,"" ,"STY $xxxx"  ,"STA $xxxx"  ,"STX $xxxx"  ,"", _ '88
  "BCC $xx@@","STA ($xx),Y",""        ,"" ,"STY $xx,X"  ,"STA $xx,X"  ,"STX $xx,Y"  ,"", _ '90
  "TYA"      ,"STA $xxxx,Y","TXS"     ,"" ,""           ,"STA $xxxx,X",""           ,"", _ '98
  "LDY #$xx" ,"LDA ($xx,X)","LDX #$xx","" ,"LDY $xx"    ,"LDA $xx"    ,"LDX $xx"    ,"", _ 'A0
  "TAY"      ,"LDA #$xx"   ,"TAX"     ,"" ,"LDY $xxxx"  ,"LDA $xxxx"  ,"LDX $xxxx"  ,"", _ 'A8
  "BCS $xx@@","LDA ($xx),Y",""        ,"" ,"LDY $xx,X"  ,"LDA $xx,X"  ,"LDX $xx,Y"  ,"", _ 'B0
  "CLV"      ,"LDA $xxxx,Y","TSX"     ,"" ,"LDY $xxxx,X","LDA $xxxx,X","LDX $xxxx,Y","", _ 'B8
  "CPY #$xx" ,"CMP ($xx,X)",""        ,"" ,"CPY $xx"    ,"CMP $xx"    ,"DEC $xx"    ,"", _ 'C0
  "INY"      ,"CMP #$xx"   ,"DEX"     ,"" ,"CPY $xxxx"  ,"CMP $xxxx"  ,"DEC $xxxx"  ,"", _ 'C8
  "BNE $xx@@","CMP ($xx),Y",""        ,"" ,""           ,"CMP $xx,X"  ,"DEC $xx,X"  ,"", _ 'D0
  "CLD"      ,"CMP $xxxx,Y",""        ,"" ,""           ,"CMP $xxxx,X","DEC $xxxx,X","", _ 'D8
  "CPX #$xx" ,"SBC ($xx,X)",""        ,"" ,"CPX $xx"    ,"SBC $xx"    ,"INC $xx"    ,"", _ 'E0
  "INX"      ,"SBC #$xx"   ,"NOP"     ,"" ,"CPX $xxxx"  ,"SBC $xxxx"  ,"INC $xxxx"  ,"", _ 'E8
  "BEQ $xx@@","SBC ($xx),Y",""        ,"" ,""           ,"SBC $xx,X"  ,"INC $xx,X"  ,"", _ 'F0
  "SED"      ,"SBC $xxxx,Y",""        ,"" ,""           ,"SBC $xxxx,X","INC $xxxx,X","" }  'F8
rem ------------------------------------------------------------------------------------------

function DisasmInstruction( byref uAddr as long ) as bool
  var bOP =  g_bMemory(uAddr)
  dim as zstring*16 sOpcode = g_zDism( bOP )
  dim as long iLen = len(sOpcode) : sOpcode += "      "      
    
  color 7 : print hex(uAddr,4);": ";
  color 8 : print hex( bOP ,2);" ";
  
  uAddr += 1 
  if iLen=0 then 
    locate ,19 : color 12 : print "ILLEGAL"
    return false
  end if
  
  dim as byte Offset=5 , bDig = 2 , bSz = 1
  dim as ushort uVal = uAddr
  var uOp = sOpcode[5]+(sOpcode[6] shl 8)+(sOpcode[7] shl 16)+(sOpcode[8] shl 24)      
  select case uOp
  case cvl("    ") 'No Parameter
    bDig = 0 : bSz = 0
  case cvl("A   ") 'Accumulator
    bDig = 0 : bSz = 0
  case cvl("xx  ") 'Zero Page
    uVal = g_bMemory(uAddr)
  case cvl("$xx ") 'Immediate 8bit
    Offset += 1
  case cvl("xx@@") 'Relative 8bit
    uVal += cbyte(g_bMemory(uAddr))+1 : bDig = 4
  case cvl("$xx,") 'ZP , X indirect [8Bit+X]
    Offset += 1
  case cvl("$xx)") 'ZP , Y indirect [8bit]+Y
    Offset += 1
  case cvl("xx,X") 'ZP , X 
  case cvl("xx,Y") 'ZP , Y 
  case cvl("$xxx") '16bit indirect
    Offset += 1 : bDig = 4 : bSz = 2
  case cvl("xxxx") '16bit absolute (direct or indexed)
    bDig = 4 : bSz = 2
  case else        '>>> Error <<<        
    color 12 : print "{"+sOpcode+"}",hex(uOp,8): sleep
  end select
  
  if bSz=1 then 
    print hex( g_bMemory(uAddr) , 2 );
    if bDig = 2 then uVal = g_bMemory(uAddr)
  end if
  if bSz=2 then 
    print hex( g_bMemory(uAddr+0) , 2 );" ";
    print hex( g_bMemory(uAddr+1) , 2 );
    uVal = *cptr(ushort ptr,@g_bMemory(uAddr))
  end if
  uAddr += bSz      
  for N as long = bDig-1 to 0 step -1
    var uDig = uVal and &hF
    if uDig < 10 then uDig += asc("0") else uDig += asc("A")-10
    sOpcode[Offset+N] = uDig : uVal = uVal shr 4
  next N
        
  locate ,19 : color 10 : print left(sOpcode,4);      
  color iif((bSz=1 andalso bDig=4) orelse sOpcode[0]=asc("J") , 14 , 15 )
  print mid(sOpcode,5)
  
  return true
  
end function

#ifdef g_tParser
  #define g_tDisasm g_tParser
  sub Disassemble()
    for N as long = 0 to g_iBlockCount-1
      dim as ulong uAddr = g_tBlock(N).uBegin , uAddrEnd = g_tBlock(N).uEnd , uBytes
      #if 0
        while uAddr < uAddrEnd
          DisasmInstruction( uAddr )
        wend
      #else
        do
          var bOP =  g_bMemory(uAddr)
          dim as zstring*16 sOpcode = g_zDism( bOP )
          dim as long iLen = len(sOpcode)
          if uBytes >= 4 orelse uAddr >= uAddrEnd orelse iLen then
            sOpcode += "      "
            if uBytes then        
              color 7 : print hex(uAddr,4);": "; : color 8
              for N1 as long = uAddr-uBytes to uAddr-1
                print hex(g_bMemory(N1),2);" ";
              next N1
              locate , 19 : color 11 : print "dcb ";
              for N1 as long = uAddr-uBytes to uAddr-1
                color 7 : print g_bMemory(N1);
                if N1 <> (uAddr-1) then color 15 : print ",";
              next N1
              print : uBytes = 0 
            end if      
            if uAddr >= uAddrEnd then exit do
            uAddr += 1 : if iLen=0 then uBytes += 1 : continue do    
            
            color 7 : print hex(uAddr-1,4);": ";
            color 8 : print hex( bOP , 2 );" ";
            
            dim as byte Offset=5 , bDig = 2 , bSz = 1
            dim as ushort uVal = uAddr
            var uOp = sOpcode[5]+(sOpcode[6] shl 8)+(sOpcode[7] shl 16)+(sOpcode[8] shl 24)      
            select case uOp
            case cvl("    ") 'No Parameter
              bDig = 0 : bSz = 0
            case cvl("A   ") 'Accumulator
              bDig = 0 : bSz = 0
            case cvl("xx  ") 'Zero Page
              uVal = g_bMemory(uAddr)
            case cvl("$xx ") 'Immediate 8bit
              Offset += 1
            case cvl("xx@@") 'Relative 8bit
              uVal += g_bMemory(uAddr)+1 : bDig = 4
            case cvl("$xx,") 'ZP , X indirect [8Bit+X]
              Offset += 1
            case cvl("$xx)") 'ZP , Y indirect [8bit]+Y
              Offset += 1
            case cvl("xx,X") 'ZP , X 
            case cvl("xx,Y") 'ZP , Y 
            case cvl("$xxx") '16bit indirect
              Offset += 1 : bDig = 4 : bSz = 2
            case cvl("xxxx") '16bit absolute (direct or indexed)
              bDig = 4 : bSz = 2
            case else        '>>> Error <<<        
              color 12 : print "{"+sOpcode+"}",hex(uOp,8): sleep
            end select
            
            if bSz=1 then 
              print hex( g_bMemory(uAddr) , 2 );
              if bDig = 2 then uVal = g_bMemory(uAddr)
            end if
            if bSz=2 then 
              print hex( g_bMemory(uAddr+0) , 2 );" ";
              print hex( g_bMemory(uAddr+1) , 2 );
              uVal = *cptr(ushort ptr,@g_bMemory(uAddr))
            end if
            uAddr += bSz      
            for N as long = bDig-1 to 0 step -1
              var uDig = uVal and &hF
              if uDig < 10 then uDig += asc("0") else uDig += asc("A")-10
              sOpcode[Offset+N] = uDig : uVal = uVal shr 4
            next N
                  
            locate ,19 : color 10 : print left(sOpcode,4);      
            color iif((bSz=1 andalso bDig=4) orelse sOpcode[0]=asc("J") , 14 , 15 )
            print mid(sOpcode,5)
            
          else      
            uBytes += 1 : uAddr += 1
          end if
        loop    
      #endif
    next N
  end sub
#endif
