'#cmdline "-dll -s gui"
#cmdline "-dll -s gui -gen gcc -O 3"

#undef bool
type bool as byte

#include "fbgfx.bi"

const ViewWid=56,ViewHei=16
const Title = "6502 - "

namespace viewer

  enum ViewMode
    vmNone
    vmHelp
    vmHex
    vmSource
    vmDisasm
    vmSymbols
    vmText
    vmLast
  end enum

  type LabelStruct 
    sName as string
    uAddr as long
    b16   as ubyte
  end type

  
  static shared as ViewMode g_ViewMode = vmNone
  static shared as ubyte bScreenOn
  
  static shared as ubyte ptr pbMemory 
  #define g_bMemory(_uAddr) pbMemory[_uAddr]
  #include "6502-dis.bas"

  sub OpenScreen(bOnTop as byte) export
    screenres ViewWid*8,ViewHei*16,8,1,iif(bOnTop,fb.GFX_ALWAYS_ON_TOP,0)    
    width ViewWid,ViewHei
    WindowTitle( Title+"Initializing..." )
    g_ViewMode = vmNone : bScreenOn = 1
  end sub
  
  function IsScreenOpen() as long export
    return bScreenOn andalso screenptr<>0
  end function
  
  sub ScreenPos( iX as integer , iY as integer ) export
    screencontrol fb.SET_WINDOW_POS,iX,iY
  end sub

  sub CloseScreen() export
    if IsScreenOpen()=0 then exit sub
    screenres 32,32,8,1,fb.gfx_null
    bScreenOn = 0
  end sub

  function ProcessKeys() as long export
    if IsScreenOpen() = 0 then return 1
    do      
      var sKey = inkey()
      if len(sKey)=0 then return 1
      dim as long iKey = sKey[0]
      if iKey=255 then iKey = -sKey[1]      
      select case iKey
      case -asc("k") : return 0      
      end select
    loop
  end function

  sub UpdateViewHelp( iPage as long ) export
    if IsScreenOpen() = 0 then exit sub
    if g_ViewMode <> vmHelp then
      WindowTitle( Title+"Help View" )
      g_ViewMode = vmHelp
    else exit sub
    end if
    
    screenlock
    cls
    color 10
    print " -- 6502 emulator --"
    print 
    color 11
    print " I/O:"
    print
    color 7
    print " $FE - read random number"
    print " $FE - write to sync to next frame of N ms"
    print " $FF - read last key code"
    print
    color 11
    print " GFX:"
    print
    color 7
    print " $200  - 32x32 (16 color / 1 byte per pixel)"
    print " $FE00 - 40x16 (text viewer)"
    screenunlock
    
  end sub
  sub UpdateViewHex( pbMem as ubyte ptr , pbChanges as byte ptr , uAddress as long , bForce as byte = 0 ) export
    
    if IsScreenOpen() = 0 then exit sub
    
    if g_ViewMode <> vmHex then
      WindowTitle( Title+"Hex View" )
      g_ViewMode = vmHex
    end if
    
    if bForce then screenlock : cls
    
    color 14
    for N as long = 0 to ViewHei-1
      locate N+1 , 3 : print hex(uAddress+N*16,4);
    next N
    
    var uEnd = ViewHei*16-1
    if bForce then
      for N as long = 0 to uEnd        
        var uAddr = uAddress+N
        if pbChanges[uAddr] > 0 then 
          color 12         
        elseif pbChanges[uAddr] < 0 then 
          color 10
        else
          color 7
        end if
        locate 1+(N\16) , 8+(N and 15)*3
        print hex(pbMem[uAddr],2);              
        if (uAddr) >= 65535 then exit for
      next N
    else
      for N as long = 0 to uEnd
        var uAddr = uAddress+N
        if pbChanges[uAddr] then        
          if pbChanges[uAddr] > 1 then 
            color 12 : pbChanges[uAddr] -= 1
          elseif pbChanges[uAddr] > 0 then
            color 7 : pbChanges[uAddr] -= 1
          elseif pbChanges[uAddr] < -1 then 
            color 10 : pbChanges[uAddr] += 1
          elseif pbChanges[uAddr] < 0 then
            color 7 : pbChanges[uAddr] += 1
          end if
          locate 1+(N\16) , 8+(N and 15)*3
          print hex(pbMem[uAddr],2);        
        end if
        if uAddr >= 65535 then exit for
      next N
    end if
    
    if bForce then screenunlock
    
  end sub  
  sub UpdateViewSource( pbSource as zstring ptr , plIndex as ulong ptr , pwAddr as ushort ptr , uLineCount as ulong , uViewLine as long ) export
    
    if IsScreenOpen() = 0 then exit sub
    
    if g_ViewMode <> vmSource then
      WindowTitle( Title+"Source View" )
      g_ViewMode = vmSource
    end if
    
    var uOutLine = ViewHei\2 , uLine = uViewLine
    'if uViewLine > uOutLine then uOutLine
    do
      if uLine = 0 then exit do
      uLine -= 1 : uOutLine -= 1
    loop until uOutLine=1    
    
    screenlock
    cls : locate uOutLine,1
    for N as long = uOutLine to ViewHei-1      
      color 7: print right("     " & uLine+1,4);" ";
      color 11: print hex(pwAddr[uLine],4);: color 10
      if uLine = (uViewLine-1) then print " > "; else print "   ";
      color 15: print left(pbSource[plIndex[uLine]],ViewWid-13)
      uLine += 1 : if uLine > = uLineCount then exit for
    next N
    screenunlock
    
  end sub
  sub UpdateViewDisasm( pbMem as ubyte ptr , uAddress as long ) export
    if IsScreenOpen() = 0 then exit sub
    
    static as long uAddressPrev = -1
    pbMemory = pbMem
    
    if g_ViewMode <> vmDisasm then
      WindowTitle( Title+"Disassembler View" )
      g_ViewMode = vmDisasm : uAddressPrev = -1
    elseif uAddressPrev = uAddress then
      exit sub
    end if
    uAddressPrev = uAddress
    screenlock
    cls
    for N as long = 1 to ViewHei-1
      if uAddress >= 65535 then exit for
      locate ,2
      DisasmInstruction(uAddress)
    next N
    screenunlock
  end sub
  sub UpdateViewSymbols( _ptLabel as any ptr , iLabelCount as long , iLabelView as long ) export
    if IsScreenOpen() = 0 then exit sub    
    var ptLabel = cast(LabelStruct ptr,_ptLabel)
    static as long iLabelViewPrev , iBigLabel
    if g_ViewMode <> vmSymbols then
      WindowTitle( Title+"Symbols View" )
      g_ViewMode = vmSymbols : iBigLabel = -1
    elseif iLabelViewPrev = iLabelView then
      exit sub
    end if
    iLabelViewPrev = iLabelView    
    
    if iBigLabel = -1 then
      iBigLabel = 4
      for N as long = 0 to iLabelCount-1
        with ptLabel[N]
          if len(.sName) > iBigLabel then iBigLabel = len(.sName)
        end with
      next N
      if iBigLabel > ViewWid-14 then iBigLabel = ViewWid-14
    end if
    
    screenlock
    cls : color 10
    locate 1,2          : print "Name";
    locate 1,iBigLabel+3: print "Value";
    locate 1,iBigLabel+9: print "16bit";
    
    if iLabelCount = 0 then
      locate 3,2 : color 13
      print "No Labels."
    end if
    
    for N as long = 2 to ViewHei
      var uLab = N+iLabelView-2          
      if uLab >= iLabelCount then exit for
      with ptLabel[ulab]
        locate N, 2         : color 14 
        print left(.sName,ViewWid-14);
        locate N,iBigLabel+3 : color 7  
        if .b16 then print "$"+hex(.uAddr,4); else print "$"+hex(.uAddr,2);
        locate N,iBigLabel+9 : color 15 
        print iif(.b16,"Yes","No");
      end with
    next N  
    screenunlock
  end sub
  sub UpdateViewText( pbMem as ubyte ptr , uAddress as long ) export
    
    if IsScreenOpen() = 0 then exit sub
    
    if g_ViewMode <> vmText then
      WindowTitle( Title+"Text View" )
      g_ViewMode = vmText
    end if
    
    screenlock
    cls
    
    color 14
    for N as long = 0 to ViewHei-1
      locate N+1 , 3 : print hex(uAddress+N*40,4);
    next N
    
    var uEnd = ViewHei*40-1    
    
    type fbStr
      pzData as zstring ptr
      iLen   as integer
      iSize  as integer
    end type
    
    dim as string sRow
    var ptStr = cast(fbStr ptr,@sRow)
    with *ptStr
      .pzData = @pbMem[uAddress]
      .iLen = 40 : .iSize = 40
      for Y as long = 0 to ViewHei-1              
        draw string (8*8,Y*16),sRow
        .pzData += 40 : uAddress += 40
        if uAddress >= 65535 then exit for        
      next Y
      .pzData = 0 : .iSize = 0 : .iLen = 0
    end with
    
    screenunlock
    
  end sub  
  
end namespace
