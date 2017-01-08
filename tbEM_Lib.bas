  #If 0
  =============================================================================
   Program Name: tbEM_Lib.bas
   Author      : Michael Hartlef
   Version     : 1.04 (released March 23th, 2008)
   Description : Dll To manage events
  =============================================================================
  'COPYRIGHT AND PERMISSION NOTICE
  '============================================================================
  Copyright (c) 2008, Michael Hartlef, <mike@fantomgl.com>
   
  All rights reserved.
   
  Permission To use this software Is granted only For the purpose To develop
  thinBasic language modules both For commercial Or non commercial purpose.
  In Any Case the above copyright notice And this permission notice must appear
  In All copies.
   
  THE SOFTWARE Is PROVIDED "AS IS", WITHOUT WARRANTY Of Any KIND, EXPRESS Or
  IMPLIED, INCLUDING BUT Not LIMITED To THE WARRANTIES Of MERCHANTABILITY,
  FITNESS For A PARTICULAR PURPOSE And NONINFRINGEMENT Of THIRD PARTY RIGHTS.
  In NO EVENT SHALL THE AUTHORS Or COPYRIGHT HOLDERS BE LIABLE For Any CLAIM,
  DAMAGES Or OTHER LIABILITY, WHETHER In AN ACTION Of CONTRACT, TORT Or
  OTHERWISE, ARISING From, Out Of Or In CONNECTION With THE SOFTWARE Or THE
  USE Or OTHER DEALINGS In THE SOFTWARE.          
  
  
  'Changes For Version 1.11:

  'Fix xCheckEvents(TBEM_RUN): Removed check for time factors

  'Changes For Version 1.10:
  
  'Fix EVM_SetRepeat: Didn't accept the 4th parameter
  'Change EVN_AddEvent: When no starttime is given, it sets it to current tickcount
  'Change type tEvent-> funcname definition to ASCIIZ
  'Change RunEvents to reflect the new funcname definition
  'Add: All zone related commands 
  =============================================================================
  #EndIf

  #Compile Dll
  #Register None
  #Dim All
  %USEMACROS = 1  
                     
  '---Resource file. 
  #Resource "tbEM_Lib.PBR"    
  #Include "win32api.inc"

  '---Every used defined thinBasic module must include this file
  #Include "thinCore.inc"

'----------------------------------------------------
'equates

  '%ID_KEYB  = 101


'----------------------------------------------------
'Types and datas     

  Type tZone
	  ID              As Double
	  X1              As Double
	  y1              As Double
	  z1              As Double
	  x2              As Double
	  y2              As Double
	  z2              As Double
	  activeflag      As Long
    zoneData1       As Long
    zoneData2       As Long
	  triggerType     As Long
    preZone         As tZone Ptr
    postZone        As tZone Ptr
  End Type

  Type tTrigger
    triggerID       As Long
    triggerType     As Long
    triggerdata1    As Long
    triggerdata2    As Long
    time            As Dword
    postTrigger     As tTrigger Ptr
  End Type



  Type tEvent
    eventID       As Long
    eventGroup    As Long
    eventType     As Long
    startTime     As Dword
    intervalCount As Dword 
    intervalTime  As Dword
    endTime       As Dword
    repeatflag    As Dword
    activeflag    As Dword
    runflag       As Dword
    'funcName      As String * 128               Michael Hartlef 20081009
    funcName      As Asciiz * 64
    pdata         As Long
    preEvent      As tEvent Ptr
    postEvent     As tEvent Ptr
    triggerdata1  As Long
    triggerdata2  As Long
  End Type       
              
  Global retstr As String
  Global eventCount   As Long 
  Global triggerCount As Long   
  Global zoneCount As Long   
    
  Global newevent     As tEvent Ptr
  Global firstevent   As tEvent Ptr
  Global nextevent    As tEvent Ptr
  Global lastevent    As tEvent Ptr
  Global currevent    As tEvent Ptr
  Global delevent     As tEvent Ptr

  Global newtrigger   As tTrigger Ptr
  Global firsttrigger As tTrigger Ptr
  Global nexttrigger  As tTrigger Ptr
  Global lasttrigger  As tTrigger Ptr
  Global currtrigger  As tTrigger Ptr
  Global deltrigger   As tTrigger Ptr
  
  Global newzone     As tZone Ptr
  Global firstzone   As tZone Ptr
  Global nextzone    As tZone Ptr
  Global lastzone    As tZone Ptr
  Global currzone    As tZone Ptr
  Global delzone     As tZone Ptr

'----------------------------------------------------
'Function declarations  
  Function MemFree(ByRef pMem As Dword) As Long
    If pMem Then
      Function = HeapFree( GetProcessHeap(), 0, ByVal pMem)
      pMem = 0
    End If
  End Function
  
  Function MemAlloc(ByVal nSize As Long) As Dword
    If nSize > 0 Then
      Function = HeapAlloc( GetProcessHeap(), %HEAP_ZERO_MEMORY, nSize)
    End If
  End Function
 

  '*****************************************************************
  ' Module functions
  '*****************************************************************


  '*****************************************************************
  Function xCheckEvents(typ As Long, group As Long, time As Dword) As Long
  '*****************************************************************
    Local cev, ctr As Dword
    If eventcount > 0 Then
      cev = currEvent
      ctr = currTrigger
      'MsgBox("Run events") 
      currevent = firstevent
      'MsgBox ("Run Event "+Str$(@currevent.eventid)+": "+Trim$(@currevent.funcName)+".")
'HARTLEF 20090429 >>>>>>>>>>      
'      IF @currevent.eventGroup = group _
'           AND @currevent.eventtype  = typ _
'           AND @currevent.endtime   >= time _
'           AND @currevent.starttime <= time _
'           AND @currevent.activeflag = %TRUE THEN 

      IF @currevent.eventGroup = group _
           AND @currevent.eventtype  = typ _
           AND @currevent.activeflag = %TRUE THEN 
'HARTLEF 20090429 <<<<<<<<<<

        @currEvent.runFlag = %TRUE
        If currtrigger <> 0 Then  
          @currevent.triggerdata1 = @currtrigger.triggerdata1
          @currevent.triggerdata2 = @currtrigger.triggerdata2
        End If
      End If
      While @currevent.postevent <> 0
        nextevent = @currevent.postevent 
        currevent = nextevent 
'HARTLEF 20090429 >>>>>>>>>>
'        If @currevent.eventGroup = group _
'             AND @currevent.eventtype  = typ _
'             AND @currevent.endtime   >= time _
'             AND @currevent.starttime <= time _
'             AND @currevent.activeflag = %TRUE THEN 
        If @currevent.eventGroup = group _
             AND @currevent.eventtype  = typ _
             AND @currevent.activeflag = %TRUE THEN 
'HARTLEF 20090429 <<<<<<<<<<

          @currEvent.runFlag = %TRUE
          If currtrigger <> 0 Then
            @currevent.triggerdata1 = @currtrigger.triggerdata1
            @currevent.triggerdata2 = @currtrigger.triggerdata2
          End If
        End If      
      Wend 
      currEvent = cev
      currTrigger = ctr
      Function = %TRUE
    Else
      Function = %FALSE
    End If  
  End Function

  '*****************************************************************
  Function xRunEvents(time As Dword) As Long
  '*****************************************************************
    Local cev, ctr As Dword 
    Local retExt As Ext      
    'Local stimold, stimnew As Dword
    
    If eventcount > 0 Then  
      cev = currEvent  
      ctr = currTrigger
      currevent = firstevent
      If @currEvent.runFlag = %TRUE _
         And @currEvent.startTime <= time _
         And @currEvent.endTime >= time Then 
         
        'MsgBox("function call:"& Trim$(@currevent.funcName)+$CrLf+"Returnvalue:"+Str$(retext)+$CrLf+"Time:"+Str$(time)+$CrLf+"Event:"+Str$(currevent)) 
        'thinBasic_FunctionSimpleCall(Trim$(@currevent.funcName), VarPtr (retExt) )                     Michael Hartlef 20081009
        thinBasic_FunctionSimpleCall(@currevent.funcName, VarPtr (retExt) )   
        'MsgBox("Back from function call:"& Trim$(@currevent.funcName)+$CrLf+"Returnvalue:"+Str$(retext)+$CrLf+"Time:"+Str$(time)+$CrLf+"Event:"+Str$(currevent)) 
        'MsgBox("intervalcount1:"+Str$(@currEvent.intervalcount))
        If (@currEvent.repeatFlag = %FALSE) Or (@currEvent.endtime <= time) Then 
          'MsgBox("Runflag must be set to FALSE")
          @currEvent.runFlag = %FALSE  
          'MsgBox("Runflag set to FALSE")
        Else
          'MsgBox("Calculate new starttime for "+Trim$(@currevent.funcName))
          'stimold = @currEvent.starttime
          If @currEvent.intervalcount <> 0 Then
            While @currEvent.starttime <= time
              @currEvent.starttime = time + @currEvent.intervaltime
            Wend
          End If                          
          If @currEvent.intervalcount > 0 Then
            @currEvent.intervalcount = @currEvent.intervalcount - 1
            If @currEvent.intervalcount = 0 Then @currEvent.repeatFlag = %FALSE
          End If        
          'stimnew = @currEvent.starttime
          'MsgBox("Calculate new starttime for "+Trim$(@currevent.funcName)+$CrLf+"current time: "+Str$(time)+$CrLf+"Old startime: "+Str$(stimold)+$CrLf+"New startime: "+Str$(stimnew)+$CrLf+"End time: "+Str$(@currevent.endtime))
        End If
        'MsgBox("intervalcount2:"+Str$(@currEvent.intervalcount))
      End If       
      'MsgBox("finish run first event...")
      While @currevent.postevent <> 0
        'MsgBox("run next event...")
        nextevent = @currevent.postevent 
        currevent = nextevent           
        If @currEvent.runFlag = %TRUE _
           And @currEvent.startTime <= time _
           And @currEvent.endTime >= time Then    
           'MsgBox("function call:"& Trim$(@currevent.funcName)+$CrLf+"Returnvalue:"+Str$(retext)+$CrLf+"Time:"+Str$(time)+$CrLf+"Event:"+Str$(currevent)) 
          'thinBasic_FunctionSimpleCall(Trim$(@currevent.funcName), VarPtr (retExt) )                     Michael Hartlef 20081009   
          thinBasic_FunctionSimpleCall(@currevent.funcName, VarPtr (retExt) )   
          
          If @currEvent.repeatFlag = %FALSE Or @currEvent.endtime <= time Then 
            @currEvent.runFlag = %FALSE
          Else            
            'stimold = @currEvent.starttime
            If @currEvent.intervalcount <> 0 Then
              While @currEvent.starttime <= time
                @currEvent.starttime = @currEvent.starttime + @currEvent.intervaltime
              Wend
            End If
            If @currEvent.intervalcount > 0 Then
              @currEvent.intervalcount = @currEvent.intervalcount - 1
              If @currEvent.intervalcount = 0 Then @currEvent.repeatFlag = %FALSE
            End If
            'stimnew = @currEvent.starttime
            'MsgBox("Calculate new starttime for "+Trim$(@currevent.funcName)+$CrLf+"current time: "+Str$(time)+$CrLf+"Old startime: "+Str$(stimold)+$CrLf+"New startime: "+Str$(stimnew)+$CrLf+"End time: "+Str$(@currevent.endtime))
          End If
        End If
      Wend
      currEvent = cev
      currTrigger = ctr
      Function = %TRUE
    Else
      Function = %FALSE
    End If  
  End Function

  '*****************************************************************
  Function EVM_AddZone() As Ext  
  'TBEM_AddZone ( triggerType, x1, y1, z1, x2, y2, z2[, activeflag] ) As zoneID 
  '*****************************************************************
    Local nParsed As Long
    Local fname As String    
    Local activeFlag As Ext  
    Local triggerType As Ext
    Local x1,x2,y1,y2,z1,z2 As Ext   
    Local czo As Dword
    
    activeFlag = %True
    
    nParsed = thinBasic_ParseXNumbers(7,8, x1, y1, z1, x2, y2, z2, triggerType, activeFlag)
    czo = currZone
    newzone = memalloc(SizeOf(tZone))
            
    @newzone.ID       = newzone
    @newzone.x1       = x1
    @newzone.y1       = y1
    @newzone.z1       = z1
    @newzone.x2       = x2
    @newzone.y2       = y2
    @newzone.z2       = z2     
    @newzone.zoneData1 = 0
    @newzone.zoneData2 = 0
    @newzone.activeflag    = activeFlag
    @newzone.triggerType   = triggerType
    
    currzone = newzone
    If zonecount = 0 Then
      firstzone = newzone
    Else
      @lastzone.postzone = newzone
    End If                 
    zonecount = zonecount + 1
    lastzone = newzone
    
    currzone = czo
    Function = @newzone.ID
  End Function

  '*****************************************************************
  Function EVM_Run() As Ext
  ' TBEM_Run ( [ eventGroup] )
  '*****************************************************************
    Local nParsed As Long
    Local eventGroup As Ext 
    Local evgrp As Long
    Local retExt As Ext
    Local tim As Dword 
    Local cev, ctr As Dword  
    Local tTime As Dword
    eventGroup = 0
    
    nParsed = thinBasic_ParseXNumbers(0,1,eventGroup)     
    
    evgrp = eventGroup
    cev = currEvent
    ctr = currTrigger
    
    If triggerCount > 0 Then
      currTrigger = firstTrigger 
      tTime = @currTrigger.time
      retExt = xCheckEvents(@currTrigger.triggerType, evgrp, tTime)
      delTrigger = currtrigger
      While @currTrigger.posttrigger <> 0
        currtrigger = @currtrigger.posttrigger
        tTime = @currTrigger.time
        memfree(deltrigger) 
        triggerCount = triggerCount - 1
        retExt = xCheckEvents(@currTrigger.triggerType, evgrp, ttime)
        delTrigger = currtrigger
      Wend                       
      memfree(deltrigger)              
      triggerCount = triggerCount - 1
      
      tim = GetTickCount()
      xRunEvents(tim)
      currtrigger = ctr
      currevent = cev
      Function = %TRUE
    Else
      tim = GetTickCount()          
      xRunEvents(tim)
      currtrigger = ctr
      currevent = cev
      Function = %FALSE
    End If  
  End Function 



  '*****************************************************************
  Function EVM_AddEvent() As Ext  
  'TBEM_AddEvent ( functionName [, eventType [,startTime [,eventGroup [, activeFlag ]]]]) As eventID 
  '*****************************************************************
    Local nParsed As Long
    Local fname As String    
    Local startTime As Ext    
    Local eventType As Ext
    Local eventGroup As Ext
    Local activeFlag As Ext   
    Local cev As Dword
    
    'startTime  = 0                                                       Michael Hartlef  20081009
    startTime  = GetTickCount()
    eventType  = 0  
    eventGroup = 0
    activeFlag = %True
    
    nParsed = thinBasic_Parse1StringXNumbers(0,4,fname,eventType,startTime,eventGroup,activeFlag)
    cev = currEvent
    newevent = memalloc(SizeOf(tEvent))
            
    @newevent.eventID       = newevent
    @newevent.eventGroup    = eventGroup
    @newevent.eventType     = eventType
    @newevent.startTime     = startTime
    @newevent.intervalCount = 0
    @newevent.intervalTime  = 0
    @newevent.endTime       = &H0FFFFFFFF
    @newevent.repeatflag    = %FALSE
    @newevent.activeflag    = activeFlag
    @newevent.runflag       = %FALSE
    @newevent.funcName      = fname
    @newevent.pdata         = 0
    @newevent.preEvent      = lastevent
    @newevent.postEvent     = 0              
    @newevent.triggerdata1  = 0
    @newevent.triggerdata2  = 0
    
    currevent = newevent
    If eventcount = 0 Then
      firstevent = newevent
    Else
      @lastevent.postevent = newevent
    End If                 
    eventcount = eventcount + 1
    lastevent = newevent
    'MsgBox ("Event "+Str$(@newevent.eventid)+": "+Trim$(@newevent.funcName)+" added. Size:"+Str$(SizeOf(tEvent))+"pre:"+str$(@newevent.preevent))
    
    currevent = cev
    Function = @newevent.eventID
  End Function



  '*****************************************************************
  Function xFireTrigger(triggerType As Long, triggerdata1 As Dword, triggerdata2 As Dword) As Dword  
  '*****************************************************************
    Local tmpTime As Dword 
    Local ctr As Dword
    
    tmpTime = GetTickCount()
   
    ctr = currTrigger
    newTrigger = memalloc(SizeOf(tTrigger))

    @newtrigger.triggerID     = newTrigger
    @newtrigger.triggerType   = triggerType
    @newtrigger.triggerdata1  = triggerData1
    @newtrigger.triggerdata2  = triggerData2
    @newtrigger.time          = tmpTime
    @newtrigger.postTrigger   = 0      
    
    currTrigger = newTrigger
    If Triggercount = 0 Then
      firstTrigger = newTrigger
    Else
      @lastTrigger.postTrigger = newTrigger
    End If                 
    Triggercount = Triggercount + 1
    lastTrigger = newTrigger
    
    currtrigger = ctr
    Function = @newtrigger.TriggerID
  End Function


  '*****************************************************************
  Function EVM_AddTrigger() As Ext  
   'TBEM_AddTrigger ( [triggerType [, triggerdata1 [, triggerdata2 ]]] ) as long
  '*****************************************************************
    Local nParsed As Long
    Local triggerType As Ext
    Local triggerdata1 As Ext
    Local triggerdata2 As Ext
    Local tmpTime As Dword 
    Local ctr As Dword
    
    tmpTime = GetTickCount()
    triggerType  = 0   
    triggerdata1  = 0   
    triggerdata2  = 0   
   
    nParsed = thinBasic_ParseXNumbers(0,3,triggerType, triggerdata1, triggerdata2)
    ctr = currTrigger
    newTrigger = memalloc(SizeOf(tTrigger))

    @newtrigger.triggerID     = newTrigger
    @newtrigger.triggerType   = triggerType
    @newtrigger.triggerdata1  = triggerData1
    @newtrigger.triggerdata2  = triggerData2
    @newtrigger.time          = tmpTime
    @newtrigger.postTrigger   = 0      
    
    currTrigger = newTrigger
    If Triggercount = 0 Then
      firstTrigger = newTrigger
    Else
      @lastTrigger.postTrigger = newTrigger
    End If                 
    Triggercount = Triggercount + 1
    lastTrigger = newTrigger
    
    currtrigger = ctr
    Function = @newtrigger.TriggerID
  End Function

  '*****************************************************************
  Function EVM_DeleteEvent() As Ext
  'TBEM_DeleteEvent ( eventID ) as long
  '*****************************************************************
    Local nParsed As Long 
    Local eventID As Ext  
    Local ret As Ext
    Local cev As Dword
    
    nParsed = thinBasic_ParseXNumbers(1,1,eventID)
    cev = currEvent 
    
    If eventcount > 0 Then 
      ret = %FALSE
      currevent = firstevent
      If @currevent.eventID = eventID Then
        delevent = currevent
        firstevent = @currevent.postevent
        eventcount = eventcount - 1
        memfree(delevent)
        ret = %TRUE
        currEvent = firstEvent
      Else  
        While @currevent.postevent <> 0
          lastevent = currevent
          nextevent = @currevent.postevent 
          currevent = nextevent
          If @currevent.eventID = eventID Then
            delevent = currevent
            eventcount = eventcount - 1
            @lastevent.postevent = nextevent
            memfree(delevent)
            ret = %TRUE
            Exit Loop
          End If  
        Wend
      End If         
      If cev <> delEvent Then
        currEvent = cev
      Else
        currEvent = firstevent
      End If
    Else              
      ret = %FALSE
    End If  

    Function = ret   
  End Function        
  
  '*****************************************************************
  Function EVM_DeleteZone() As Ext
  'TBEM_DeleteZone ( zoneID ) as long
  '*****************************************************************
    Local nParsed As Long 
    Local zoneID As Ext  
    Local ret As Ext
    Local czo As Dword
    
    nParsed = thinBasic_ParseXNumbers(1,1,zoneID)
    czo = currzone 
    
    If zonecount > 0 Then 
      ret = %FALSE
      currzone = firstzone
      If @currzone.ID = zoneID Then
        delzone = currzone
        firstzone = @currzone.postzone
        zonecount = zonecount - 1
        memfree(delzone)
        ret = %TRUE
        currzone = firstzone
      Else  
        While @currzone.postzone <> 0
          lastzone = currzone
          nextzone = @currzone.postzone 
          currzone = nextzone
          If @currzone.ID = zoneID Then
            delzone = currzone
            zonecount = zonecount - 1
            @lastzone.postzone = nextzone
            memfree(delzone)
            ret = %TRUE
            Exit Loop
          End If  
        Wend
      End If         
      If czo <> delzone Then
        currzone = czo
      Else
        currzone = firstzone
      End If
    Else              
      ret = %FALSE
    End If  

    Function = ret   
  End Function        
  
  
   
  '*****************************************************************
  Function EVM_SetRepeat() As Ext
  'TBEM_SetRepeat ( eventID, repeatFlag [, intervalTime [, intervalCount]] ) as long
  '*****************************************************************
    Local nParsed As Long 
    Local eventID As Ext 
    Local repeatFlag As Ext
    Local intervalCount As Ext
    Local intervalTime As Ext 
    Local ret As Ext
    Local cev As Dword
    
    intervalTime  = -1
    intervalCount = -1
    
    'nParsed = thinBasic_ParseXNumbers(2,3,eventID, repeatFlag, intervalTime, intervalCount)        Michael Hartlef  2008109
    nParsed = thinBasic_ParseXNumbers(2,4,eventID, repeatFlag, intervalTime, intervalCount)
    
    If eventcount > 0 Then 
      cev = currEvent
      ret = %FALSE
      currevent = firstevent
      If @currevent.eventID = eventID Then
        @currevent.repeatFlag = repeatFlag
        @currevent.intervalTime = intervalTime
        @currevent.intervalCount = intervalCount
        ret = %TRUE
      Else  
        While @currevent.postevent <> 0
          nextevent = @currevent.postevent 
          currevent = nextevent
          If @currevent.eventID = eventID Then
            @currevent.repeatFlag = repeatFlag
            @currevent.intervalTime = intervalTime
            @currevent.intervalCount = intervalCount
            ret = %TRUE
            Exit Loop
          End If  
        Wend
      End If
      currEvent = cev      
    Else              
      ret = %FALSE

    End If       
    Function = ret   
  End Function
 

    

  '*****************************************************************
  Function EVM_SetTime() As Ext
  'TBEM_SetTime ( eventID , starttime [, endTime ] ) as long
  '*****************************************************************
    Local nParsed As Long 
    Local eventID As Ext 
    Local startTime As Ext
    Local EndTime As Ext 
    Local ret As Ext
    Local cev As Dword
    
    EndTime       = &H0FFFFFFFF
    
    nParsed = thinBasic_ParseXNumbers(2,3,eventID, startTime, EndTime)
    If eventcount > 0 Then 
      cev = currEvent
      ret = %FALSE
      currevent = firstevent
      If @currevent.eventID = eventID Then
        @currevent.starttime = starttime
        @currevent.endtime = endtime
        ret = %TRUE
      Else  
        While @currevent.postevent <> 0
          nextevent = @currevent.postevent 
          currevent = nextevent
          If @currevent.eventID = eventID Then
            @currevent.starttime = starttime
            @currevent.endtime = endtime
            ret = %TRUE
            Exit Loop
          End If  
        Wend
      End If
      currEvent = cev
    Else              
      ret = %FALSE
    End If        
    
    Function = ret   
  End Function
                      
                      
                      
                      
  '*****************************************************************
  Function EVM_SetActive() As Ext
  'TBEM_SetActive ( eventID ,activeFlag ) as long
  '*****************************************************************
    Local nParsed As Long 
    Local eventID As Ext 
    Local active As Ext 
    Local ret As Ext
    Local cev As Dword
    
    nParsed = thinBasic_ParseXNumbers(2,2,eventID, active)

    If eventcount > 0 Then 
      cev = currEvent
      ret = %FALSE
      currevent = firstevent
      If @currevent.eventID = eventID Then
        @currevent.activeflag = active
        ret = %TRUE
      Else  
        While @currevent.postevent <> 0
          nextevent = @currevent.postevent 
          currevent = nextevent
          If @currevent.eventID = eventID Then
            @currevent.activeflag = active
            ret = %TRUE
            Exit Loop
          End If  
        Wend
      End If
      currEvent = cev
    Else              
      ret = %FALSE
    End If       
     
    Function = ret   
  End Function
  
  '*****************************************************************
  Function EVM_SetZoneActive() As Ext
  'TBEM_SetZoneActive ( zoneID ,activeFlag ) as long
  '*****************************************************************
    Local nParsed As Long 
    Local zoneID As Ext 
    Local active As Ext 
    Local ret As Ext
    Local czo As Dword
    
    nParsed = thinBasic_ParseXNumbers(2,2,zoneID, active)

    If zonecount > 0 Then 
      czo = currzone
      ret = %FALSE
      currzone = firstzone
      If @currzone.ID = zoneID Then
        @currzone.activeflag = active
        ret = %TRUE
      Else  
        While @currzone.postzone <> 0
          nextzone = @currzone.postzone 
          currzone = nextzone
          If @currzone.ID = zoneID Then
            @currzone.activeflag = active
            ret = %TRUE
            Exit Loop
          End If  
        Wend
      End If
      currzone = czo
    Else              
      ret = %FALSE
    End If       
     
    Function = ret   
  End Function
  
  '*****************************************************************
  Function EVM_SetEventFunc() As Ext
  'TBEM_SetEventFunc ( eventID ,funcname ) as long
  '*****************************************************************
    Local eventID As Ext 
    Local funcnam As String 
    Local ret As Ext  
    Local cev As Dword
    
    If thinBasic_CheckOpenParens Then
      thinBasic_ParseNumber eventID
      If thinBasic_CheckComma Then
        thinBasic_ParseString funcnam
        If thinBasic_CheckCloseParens Then
          
          If eventcount > 0 Then 
            cev = currevent
            ret = %FALSE
            currevent = firstevent
            If @currevent.eventID = eventID Then
              @currevent.funcname = Trim$(funcnam)
              ret = %TRUE
            Else  
              While @currevent.postevent <> 0
                nextevent = @currevent.postevent 
                currevent = nextevent
                If @currevent.eventID = eventID Then
                  @currevent.funcname = Trim$(funcnam)
                  ret = %TRUE
                  Exit Loop
                End If  
              Wend
            End If
            currEvent = cev
          Else              
            ret = %FALSE
          End If  
          
        End If
      End If
    End If       
     
    Function = ret   
  End Function
  
  '*****************************************************************
  Function EVM_SetZoneTrigger() As Ext
  'TBEM_SetZonetrigger ( ZoneID, triggerType ) as long
  '*****************************************************************
    Local nParsed As Long
    Local ZoneID As Ext 
    Local triggerType As Ext 
    Local ret As Ext  
    Local czo As Dword
    
    nParsed = thinBasic_ParseXNumbers(0,2,ZoneID, triggerType)         
    If Zonecount > 0 Then 
      czo = currZone
      ret = %FALSE
      currZone = firstZone
      If @currZone.ID = ZoneID Then
        @currZone.triggerType = triggerType
        ret = %TRUE
      Else  
        While @currZone.postZone <> 0
          nextZone = @currZone.postZone 
          currZone = nextZone
          If @currZone.ID = ZoneID Then
            @currZone.triggerType = triggerType
            ret = %TRUE
            Exit Loop
          End If  
        Wend
      End If
      currZone = czo
    Else              
      ret = %FALSE
    End If  
    
     
    Function = ret   
  End Function
  
  '*****************************************************************
  Function EVM_SetData() As Ext
  'TBEM_SetData ( eventID, pData ) As Long
  '*****************************************************************
    Local nParsed As Long 
    Local eventID As Ext 
    Local pData As Ext 
    Local ret As Ext
    Local cev As Dword
    
    nParsed = thinBasic_ParseXNumbers(2,2,eventID, pData)

    If eventcount > 0 Then 
      cev = currEvent
      ret = %FALSE
      currevent = firstevent
      If @currevent.eventID = eventID Then
        @currevent.pdata = pdata
        ret = %TRUE
      Else  
        While @currevent.postevent <> 0
          nextevent = @currevent.postevent 
          currevent = nextevent
          If @currevent.eventID = eventID Then
            @currevent.pdata = pdata
            ret = %TRUE
            Exit Loop
          End If  
        Wend
      End If
      currEvent = cev
    Else              
      ret = %FALSE
    End If       
     
    Function = ret   
  End Function

  '*****************************************************************
  Function EVM_SetZoneCoords() As Ext
  'TBEM_SetZoneCoords( zoneID, x1, y1, z1, x2, y2, z2 ) As Long
  '*****************************************************************
    Local nParsed As Long 
    Local zoneID As Ext 
    Local ret As Ext
    Local czo As Dword 
    Local x1,x2,y1,y2,z1,z2 As Ext   

    nParsed = thinBasic_ParseXNumbers(7,7,zoneID, x1, y1, z1, x2, y2, z2)

    If zonecount > 0 Then 
      czo = currzone
      ret = %FALSE
      currzone = firstzone
      If @currzone.ID = zoneID Then
        @currzone.x1 = x1
        @currzone.y1 = y1
        @currzone.z1 = z1
        @currzone.x2 = x2
        @currzone.y2 = y2
        @currzone.z2 = z2
        ret = %TRUE
      Else  
        While @currzone.postzone <> 0
          nextzone = @currzone.postzone 
          currzone = nextzone
          If @currzone.ID = zoneID Then
            @currzone.x1 = x1
            @currzone.y1 = y1
            @currzone.z1 = z1
            @currzone.x2 = x2
            @currzone.y2 = y2
            @currzone.z2 = z2
            ret = %TRUE
            Exit Loop
          End If  
        Wend
      End If
      currzone = czo
    Else              
      ret = %FALSE
    End If       
     
    Function = ret   
  End Function

  '*****************************************************************
  Function EVM_SetZoneData() As Ext
  'TBEM_SetZoneData ( zoneID, zoneData1 [, zoneData2] ) As Long
  '*****************************************************************
    Local nParsed As Long 
    Local zoneID As Ext 
    Local zoneData1 As Ext 
    Local zoneData2 As Ext 
    Local ret As Ext
    Local czo As Dword 
    
    zoneData2 = 0
    nParsed = thinBasic_ParseXNumbers(1,3,zoneID, zoneData1, zoneData2)

    If zonecount > 0 Then 
      czo = currzone
      ret = %FALSE
      currzone = firstzone
      If @currzone.ID = zoneID Then
        @currzone.zoneData1 = zoneData1
        @currzone.zoneData2 = zoneData2
        ret = %TRUE
      Else  
        While @currzone.postzone <> 0
          nextzone = @currzone.postzone 
          currzone = nextzone
          If @currzone.ID = zoneID Then
            @currzone.zoneData1 = zoneData1
            @currzone.zoneData2 = zoneData2
            ret = %TRUE
            Exit Loop
          End If  
        Wend
      End If
      currzone = czo
    Else              
      ret = %FALSE
    End If       
     
    Function = ret   
  End Function

  '*****************************************************************
  Function EVM_GetRepeatCount() As Ext
  'TBEM_GetRepeatCount ( eventID ) as long
  '*****************************************************************
    Local nParsed As Long 
    Local eventID As Ext 
    Local ret As Ext
    Local cev As Dword
    
    nParsed = thinBasic_ParseXNumbers(1,1,eventID)

    If eventcount > 0 Then 
      cev = currEvent
      ret = %FALSE
      currevent = firstevent
      If @currevent.eventID = eventID Then
        ret = @currevent.intervalcount
      Else  
        While @currevent.postevent <> 0
          nextevent = @currevent.postevent 
          currevent = nextevent
          If @currevent.eventID = eventID Then
            ret = @currevent.intervalcount
            Exit Loop
          End If  
        Wend
      End If
      currEvent = cev
    Else              
      ret = %FALSE
    End If       
      
    Function = ret   
  End Function

  '*****************************************************************
  Function EVM_GetRepeatTime() As Ext
  'TBEM_GetRepeatTime ( eventID ) As long
  '*****************************************************************
    Local nParsed As Long 
    Local eventID As Ext 
    Local ret As Ext
    Local cev As Dword
    
    nParsed = thinBasic_ParseXNumbers(1,1,eventID)

    If eventcount > 0 Then 
      cev = currEvent
      ret = %FALSE
      currevent = firstevent
      If @currevent.eventID = eventID Then
        ret = @currevent.intervaltime
      Else  
        While @currevent.postevent <> 0
          nextevent = @currevent.postevent 
          currevent = nextevent
          If @currevent.eventID = eventID Then
            ret = @currevent.intervaltime
            Exit Loop
          End If  
        Wend
      End If
      currEvent = cev
    Else              
      ret = %FALSE
    End If             
    Function = ret
 
  End Function
  
  
  '*****************************************************************
  Function EVM_GetData() As Ext
  'TBEM_GetData ( eventID ) as long
  '*****************************************************************
    Local nParsed As Long 
    Local eventID As Ext 
    Local ret As Ext
    Local cev As Dword
    
    nParsed = thinBasic_ParseXNumbers(1,1,eventID)

    If eventcount > 0 Then 
      cev = currEvent
      ret = %FALSE
      currevent = firstevent
      If @currevent.eventID = eventID Then
        ret = @currevent.pdata
      Else  
        While @currevent.postevent <> 0
          nextevent = @currevent.postevent 
          currevent = nextevent
          If @currevent.eventID = eventID Then
            ret = @currevent.pdata
            Exit Loop
          End If  
        Wend
      End If
      currEvent = cev
    Else              
      ret = %FALSE
    End If        
     
    Function = ret   
  End Function

 
  '*****************************************************************
  Function EVM_GetEventID() As Ext
  'TBEM_GetEventID( eventIndex ) As Long
  '*****************************************************************
    Local nParsed As Long 
    Local index As Ext 
    Local ret As Ext
    Local eCount As Long
    Local cev As Dword
    
    nParsed = thinBasic_ParseXNumbers(1,1,index)
    ecount = 0                                                  
    
    If eventcount > 0 And index > 0 And index <= eventcount Then 
      cev = currEvent
      ret = %FALSE
      currevent = firstevent
      ecount = 1
      If ecount = index Then
        ret = @currevent.eventID
      Else  
        While @currevent.postevent <> 0
          nextevent = @currevent.postevent 
          currevent = nextevent
          ecount = ecount + 1
          If ecount = index Then
            ret = @currevent.eventID
            Exit Loop
          End If  
        Wend
      End If
      currEvent = cev
    Else              
      ret = %FALSE
    End If       
     
    Function = ret   
  End Function

  '*****************************************************************
  Function EVM_GetZoneID() As Ext
  'TBEM_GetZoneID( zoneIndex ) As Long
  '*****************************************************************
    Local nParsed As Long 
    Local index As Ext 
    Local ret As Ext
    Local zCount As Long
    Local czo As Dword
    
    nParsed = thinBasic_ParseXNumbers(1,1,index)
    zcount = 0                                                  
    
    If zonecount > 0 And index > 0 And index <= zonecount Then 
      czo = currzone
      ret = %FALSE
      currzone = firstzone
      zcount = 1
      If zcount = index Then
        ret = @currzone.ID
      Else  
        While @currzone.postzone <> 0
          nextzone = @currzone.postzone 
          currzone = nextzone
          zcount = zcount + 1
          If zcount = index Then
            ret = @currzone.ID
            Exit Loop
          End If  
        Wend
      End If
      currzone = czo
    Else              
      ret = %FALSE
    End If       
     
    Function = ret   
  End Function

  '*****************************************************************
  Function EVM_GetEventType() As Ext
  'TBEM_GetEventType( eventID ) As Long
  '*****************************************************************
    Local nParsed As Long 
    Local eventID As Ext 
    Local ret As Ext
    Local cev As Dword
    
    nParsed = thinBasic_ParseXNumbers(1,1,eventID)

    If eventcount > 0 Then 
      cev = currEvent
      ret = %FALSE
      currevent = firstevent
      If @currevent.eventID = eventID Then
        ret = @currevent.eventType
      Else  
        While @currevent.postevent <> 0
          nextevent = @currevent.postevent 
          currevent = nextevent
          If @currevent.eventID = eventID Then
            ret = @currevent.eventType
            Exit Loop
          End If  
        Wend
      End If
      currEvent = cev
    Else              
      ret = %FALSE
    End If       
     
    Function = ret   
  End Function

  '*****************************************************************
  Function EVM_GetEventSTime() As Ext
  'TBEM_GetEventSTime( eventID ) As Long
  '*****************************************************************
    Local nParsed As Long 
    Local eventID As Ext 
    Local ret As Ext
    Local cev As Dword
    
    nParsed = thinBasic_ParseXNumbers(1,1,eventID)

    If eventcount > 0 Then 
      cev = currEvent
      ret = %FALSE
      currevent = firstevent
      If @currevent.eventID = eventID Then
        ret = @currevent.startTime
      Else  
        While @currevent.postevent <> 0
          nextevent = @currevent.postevent 
          currevent = nextevent
          If @currevent.eventID = eventID Then
            ret = @currevent.startTime
            Exit Loop
          End If  
        Wend
      End If
      currEvent = cev
    Else              
      ret = %FALSE
    End If       
     
    Function = ret   
  End Function

  '*****************************************************************
  Function EVM_GetEventETime() As Ext
  'TBEM_GetEventETime( eventID ) As Long
  '*****************************************************************
    Local nParsed As Long 
    Local eventID As Ext 
    Local ret As Ext
    Local cev As Dword
    
    nParsed = thinBasic_ParseXNumbers(1,1,eventID)

    If eventcount > 0 Then 
      cev = currEvent
      ret = %FALSE
      currevent = firstevent
      If @currevent.eventID = eventID Then
        ret = @currevent.endTime
      Else  
        While @currevent.postevent <> 0
          nextevent = @currevent.postevent 
          currevent = nextevent
          If @currevent.eventID = eventID Then
            ret = @currevent.endTime
            Exit Loop
          End If  
        Wend
      End If
      currEvent = cev
    Else              
      ret = %FALSE
    End If       
     
    Function = ret   
  End Function

  '*****************************************************************
  Function EVM_GetEventRepeatFlag() As Ext
  'TBEM_GetEventRepeatFlag( eventID ) As Long
  '*****************************************************************
    Local nParsed As Long 
    Local eventID As Ext 
    Local ret As Ext
    Local cev As Dword
    
    nParsed = thinBasic_ParseXNumbers(1,1,eventID)

    If eventcount > 0 Then 
      cev = currEvent
      ret = %FALSE
      currevent = firstevent
      If @currevent.eventID = eventID Then
        ret = @currevent.repeatflag
      Else  
        While @currevent.postevent <> 0
          nextevent = @currevent.postevent 
          currevent = nextevent
          If @currevent.eventID = eventID Then
            ret = @currevent.repeatflag
            Exit Loop
          End If  
        Wend
      End If
      currEvent = cev
    Else              
      ret = %FALSE
    End If       
     
    Function = ret   
  End Function

  '*****************************************************************
  Function EVM_CheckZones() As Ext
  'TBEM_CheckZones( x, y, z ) As Long
  '*****************************************************************
    Local nParsed As Long 
    Local ret As Ext 
    Local retExt As Ext
    Local x,y,z As Ext
    Local czo As Dword
    Local mx,my,mz As Double
    nParsed = thinBasic_ParseXNumbers(3,3,x,y,z)

    If zonecount > 0 Then 
      mx = x
      my = y
      mz = z
      czo = currZone
      ret = %FALSE
      currzone = firstzone
      If @currzone.activeflag = %TRUE Then
					If mx >= @currzone.x1 And mx <= @currzone.x2 Then
						If my >= @currzone.y1 And my <= @currzone.y2 Then
						  If mz >= @currzone.z1 And mz <= @currzone.z2 Then
						    xFireTrigger(@currZone.triggerType, @currZone.zoneData1, @currZone.zoneData2)     
							  ret =  %True
						  End If
						End If
					End If
      End If
      While @currzone.postzone <> 0
        nextzone = @currzone.postzone 
        currzone = nextzone
        If @currzone.activeflag = %TRUE Then
  					If mx >= @currzone.x1 And mx <= @currzone.x2 Then
  						If my >= @currzone.y1 And my <= @currzone.y2 Then
  						  If mz >= @currzone.z1 And mz <= @currzone.z2 Then
  						    xFireTrigger(@currZone.triggerType, @currZone.zoneData1, @currZone.zoneData2)     
  							  ret =  %True
  						  End If
  						End If
  					End If
        End If  
      Wend
      currzone = czo
    Else              
      ret = %FALSE
    End If       
     
    Function = ret   
  End Function

  '*****************************************************************
  Function EVM_GetZoneActive() As Ext
  'TBEM_GetZoneActive( zoneID ) As Long
  '*****************************************************************
    Local nParsed As Long 
    Local zoneID As Ext 
    Local ret As Ext
    Local czo As Dword
    
    nParsed = thinBasic_ParseXNumbers(1,1,zoneID)

    If zonecount > 0 Then 
      czo = currZone
      ret = %FALSE
      currzone = firstzone
      If @currzone.ID = zoneID Then
        ret = @currzone.activeflag
      Else  
        While @currzone.postzone <> 0
          nextzone = @currzone.postzone 
          currzone = nextzone
          If @currzone.ID = zoneID Then
            ret = @currzone.activeflag
            Exit Loop
          End If  
        Wend
      End If
      currzone = czo
    Else              
      ret = %FALSE
    End If       
     
    Function = ret   
  End Function

  '*****************************************************************
  Function EVM_GetZoneData1() As Ext
  'TBEM_GetZoneData1( zoneID ) As Long
  '*****************************************************************
    Local nParsed As Long 
    Local zoneID As Ext 
    Local ret As Ext
    Local czo As Dword
    
    nParsed = thinBasic_ParseXNumbers(1,1,zoneID)

    If zonecount > 0 Then 
      czo = currZone
      ret = 0
      currzone = firstzone
      If @currzone.ID = zoneID Then
        ret = @currzone.zoneData1
      Else  
        While @currzone.postzone <> 0
          nextzone = @currzone.postzone 
          currzone = nextzone
          If @currzone.ID = zoneID Then
            ret = @currzone.zoneData1
            Exit Loop
          End If  
        Wend
      End If
      currzone = czo
    Else              
      ret = 0
    End If       
     
    Function = ret   
  End Function

  '*****************************************************************
  Function EVM_GetZoneData2() As Ext
  'TBEM_GetZoneData2( zoneID ) As Long
  '*****************************************************************
    Local nParsed As Long 
    Local zoneID As Ext 
    Local ret As Ext
    Local czo As Dword
    
    nParsed = thinBasic_ParseXNumbers(1,1,zoneID)

    If zonecount > 0 Then 
      czo = currZone
      ret = 0
      currzone = firstzone
      If @currzone.ID = zoneID Then
        ret = @currzone.zoneData1
      Else  
        While @currzone.postzone <> 0
          nextzone = @currzone.postzone 
          currzone = nextzone
          If @currzone.ID = zoneID Then
            ret = @currzone.zoneData2
            Exit Loop
          End If  
        Wend
      End If
      currzone = czo
    Else              
      ret = 0
    End If       
     
    Function = ret   
  End Function

  '*****************************************************************
  Function EVM_GetEventGroup() As Ext
  'TBEM_GetEventGroup( eventID ) As Long
  '*****************************************************************
    Local nParsed As Long 
    Local eventID As Ext 
    Local ret As Ext
    Local cev As Dword
    
    nParsed = thinBasic_ParseXNumbers(1,1,eventID)

    If eventcount > 0 Then
      cev = currEvent 
      ret = %FALSE
      currevent = firstevent
      If @currevent.eventID = eventID Then
        ret = @currevent.eventGroup
      Else  
        While @currevent.postevent <> 0
          nextevent = @currevent.postevent 
          currevent = nextevent
          If @currevent.eventID = eventID Then
            ret = @currevent.eventGroup
            Exit Loop
          End If  
        Wend
      End If
      currEvent = cev
    Else              
      ret = %FALSE
    End If       
      
    Function = ret   
  End Function

  '*****************************************************************
  Function EVM_GetEventFunc() As String
  'TBEM_GetEventFunc( eventID ) As String
  '*****************************************************************
    Local nParsed As Long 
    Local eventID As Ext
    Local cev As Dword 
    
    nParsed = thinBasic_ParseXNumbers(1,1,eventID)
    
    If eventcount > 0 Then 
    
      cev = currEvent
      retstr = ""
      currevent = firstevent
      If @currevent.eventID = eventID Then
        retstr = Trim$(@currevent.funcname)
      Else  
        While @currevent.postevent <> 0
          nextevent = @currevent.postevent 
          currevent = nextevent
          If @currevent.eventID = eventID Then
            retstr = Trim$(@currevent.funcname)
            Exit Loop
          End If  
        Wend
      End If
      currevent = cev      
    Else              
      retstr = ""
    End If       
      
    Function = retstr
  End Function

  '*****************************************************************
  Function EVM_GetCurrEventID() As Ext
  'TBEM_GetCurrEventID as long
  '***************************************************************** 
    If currevent <> 0 Then
      Function = @currevent.eventID
    Else
      Function = 0
    End If
  End Function

  '*****************************************************************
  Function EVM_GetCurrTriggerData1() As Ext
  'TBEM_GetCurrTriggerData1 as long
  '***************************************************************** 
    If currevent <> 0 Then
      Function = @currevent.triggerdata1
    Else
      Function = 0
    End If
  End Function

  '*****************************************************************
  Function EVM_GetCurrTriggerData2() As Ext
  'TBEM_GetCurrTriggerData2 as long
  '***************************************************************** 
    If currevent <> 0 Then
      Function = @currevent.triggerdata2
    Else
      Function = 0
    End If
  End Function



  '*****************************************************************
  Sub EVM_DeleteAllEvents()
  'TBEM_DeleteAllEvents
  '*****************************************************************
    If eventcount > 0 Then 
      currevent = firstevent
      While eventcount > 0
        nextevent = @currevent.postevent 
        'MsgBox ("Remove Event: "+ Trim$(@currevent.funcName))        
        memfree(currevent)
        currevent = nextevent    
        eventcount = eventcount - 1
      Wend           
      
    End If  
                     
    currevent = 0
    newevent = 0
    lastevent = 0
    firstevent = 0
    nextevent = 0    
    
  End Sub
    
  '*****************************************************************
  Sub EVM_DeleteAllZones()
  'TBEM_DeleteAllZones
  '*****************************************************************
    If zonecount > 0 Then 
      currzone = firstzone
      While zonecount > 0
        nextzone = @currzone.postzone 
        memfree(currzone)
        currzone = nextzone    
        zonecount = zonecount - 1
      Wend           
      
    End If  
                     
    currzone = 0
    newzone = 0
    lastzone = 0
    firstzone = 0
    nextzone = 0    
    
  End Sub
    

  '*****************************************************************
  Sub EVM_DeleteAllTrigger()
  'TBEM_DeleteAllTrigger
  '*****************************************************************
    If triggerCount > 0 Then 
      currtrigger = firsttrigger
      While eventcount > 0
        nexttrigger = @currtrigger.postTrigger 
        memfree(currtrigger)
        currtrigger = nexttrigger    
        triggerCount = triggerCount - 1
      Wend
    End If  
                     
    triggerCount = 0
    currtrigger = 0
    newtrigger = 0
    lasttrigger = 0
    firsttrigger = 0
    nexttrigger = 0

  End Sub
    
  '*****************************************************************
  Function EVM_GetEventCount() As Ext  
  'TBEM_GetEventCount as LONG
  '*****************************************************************
    Function = eventCount
  End Function

  '*****************************************************************
  Function EVM_GetTriggerCount() As Ext 
  'TBEM_GetTriggerCount as LONG 
  '*****************************************************************
    Function = triggerCount
  End Function

  '*****************************************************************
  Function EVM_GetZoneCount() As Ext  
  'TBEM_GetZoneCount as LONG
  '*****************************************************************
    Function = zoneCount
  End Function


  '*****************************************************************
  Sub EVM_Init()
  '*****************************************************************
    'MsgBox ("Eventmanager loaded")

    eventCount = 0
    currevent = 0
    newevent = 0
    lastevent = 0
    firstevent = 0
    nextevent = 0
    
    zoneCount = 0
    currzone = 0
    newzone = 0
    lastzone = 0
    firstzone = 0
    nextzone = 0
    
    triggerCount = 0
    currtrigger = 0
    newtrigger = 0
    lasttrigger = 0
    firsttrigger = 0
    nexttrigger = 0
    
  End Sub
                      
  '*****************************************************************
  Sub EVM_Close()
  '*****************************************************************
    If eventcount > 0 Then 
      currevent = firstevent
      While eventcount > 0
        nextevent = @currevent.postevent 
        'MsgBox ("Remove Event: "+ Trim$(@currevent.funcName))        
        memfree(currevent)
        currevent = nextevent    
        eventcount = eventcount - 1
      Wend
    End If  
                     
    currevent = 0
    newevent = 0
    lastevent = 0
    firstevent = 0
    nextevent = 0    
    
    If zonecount > 0 Then 
      currzone = firstzone
      While zonecount > 0
        nextzone = @currzone.postzone 
        memfree(currzone)
        currzone = nextzone    
        zonecount = zonecount - 1
      Wend
    End If  
                     
    currzone = 0
    newzone = 0
    lastzone = 0
    firstzone = 0
    nextzone = 0    
    
    If triggerCount > 0 Then 
      currtrigger = firsttrigger
      While eventcount > 0
        nexttrigger = @currtrigger.postTrigger 
        memfree(currtrigger)
        currtrigger = nexttrigger    
        triggerCount = triggerCount - 1
      Wend
    End If  
                     
    triggerCount = 0
    currtrigger = 0
    newtrigger = 0
    lasttrigger = 0
    firsttrigger = 0
    nexttrigger = 0
    
    'MsgBox ("Eventmanager unloaded")
  End Sub
    
    


  '----------------------------------------------------------------------------
  Function LoadLocalSymbols Alias "LoadLocalSymbols" (Optional ByVal sPath As String) Export As Long
  ' This function is automatically called by thinCore whenever this DLL is loaded.
  ' This function MUST be present in every external DLL you want to use
  ' with thinBasic
  ' Use this function to initialize every variable you need and for loading the
  ' new symbol (read Keyword) you have created.
  '----------------------------------------------------------------------------

    '---
    'Add here Initialization code if needed
    '---

     
    '---
    'Here define/add new thinBasic keywords
    '---                                     
    thinBasic_LoadSymbol "TBEM_SetData",            %thinBasic_ReturnNumber,      CodePtr(EVM_SetData),               %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_SetActive",          %thinBasic_ReturnNumber,      CodePtr(EVM_SetActive),             %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_SetTime",            %thinBasic_ReturnNumber,      CodePtr(EVM_SetTime),               %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_SetRepeat",          %thinBasic_ReturnNumber,      CodePtr(EVM_SetRepeat),             %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_SetEventFunc",       %thinBasic_ReturnNumber,      CodePtr(EVM_SetEventFunc),          %thinBasic_ForceOverWrite

    thinBasic_LoadSymbol "TBEM_GetData",            %thinBasic_ReturnNumber,      CodePtr(EVM_GetData),               %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetRepeatCount",     %thinBasic_ReturnNumber,      CodePtr(EVM_GetRepeatCount),        %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetRepeatTime",      %thinBasic_ReturnNumber,      CodePtr(EVM_GetRepeatTime),         %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetEventType",       %thinBasic_ReturnNumber,      CodePtr(EVM_GetEventType),          %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetEventGroup",      %thinBasic_ReturnNumber,      CodePtr(EVM_GetEventGroup),         %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetEventFunc",       %thinBasic_ReturnString,      CodePtr(EVM_GetEventFunc),          %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetEventSTime",      %thinBasic_ReturnNumber,      CodePtr(EVM_GetEventSTime),         %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetEventETime",      %thinBasic_ReturnNumber,      CodePtr(EVM_GetEventETime),         %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetEventRepeatFlag", %thinBasic_ReturnNumber,      CodePtr(EVM_GetEventRepeatFlag),    %thinBasic_ForceOverWrite

    thinBasic_LoadSymbol "TBEM_GetEventID",         %thinBasic_ReturnNumber,      CodePtr(EVM_GetEventID),            %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetCurrEventID",     %thinBasic_ReturnNumber,      CodePtr(EVM_GetCurrEventID),        %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetCurrTriggerData1",%thinBasic_ReturnNumber,      CodePtr(EVM_GetCurrTriggerData1),   %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetCurrTriggerData2",%thinBasic_ReturnNumber,      CodePtr(EVM_GetCurrTriggerData2),   %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetTriggerCount",    %thinBasic_ReturnNumber,      CodePtr(EVM_GetTriggerCount),       %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetEventCount",      %thinBasic_ReturnNumber,      CodePtr(EVM_GetEventCount),         %thinBasic_ForceOverWrite

    thinBasic_LoadSymbol "TBEM_DeleteAllTrigger",   %thinBasic_ReturnNone,        CodePtr(EVM_DeleteAllTrigger),      %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_DeleteAllEvents",    %thinBasic_ReturnNone,        CodePtr(EVM_DeleteAllEvents),       %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_DeleteEvent",        %thinBasic_ReturnNumber,      CodePtr(EVM_DeleteEvent),           %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_AddEvent",           %thinBasic_ReturnNumber,      CodePtr(EVM_AddEvent),              %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_AddTrigger",         %thinBasic_ReturnNumber,      CodePtr(EVM_AddTrigger),            %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_Run",                %thinBasic_ReturnNumber,      CodePtr(EVM_Run),                   %thinBasic_ForceOverWrite
    
    thinBasic_LoadSymbol "TBEM_AddZone",            %thinBasic_ReturnNumber,      CodePtr(EVM_AddZone),               %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_DeleteZone",         %thinBasic_ReturnNumber,      CodePtr(EVM_DeleteZone),            %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_DeleteAllZones",     %thinBasic_ReturnNone,        CodePtr(EVM_DeleteAllZones),        %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_SetZoneActive",      %thinBasic_ReturnNumber,      CodePtr(EVM_SetZoneActive),         %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_SetZoneTrigger",     %thinBasic_ReturnNumber,      CodePtr(EVM_SetZoneTrigger),        %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetZoneID",          %thinBasic_ReturnNumber,      CodePtr(EVM_GetZoneID),             %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetZoneCount",       %thinBasic_ReturnNumber,      CodePtr(EVM_GetZoneCount),          %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_SetZoneData",        %thinBasic_ReturnNumber,      CodePtr(EVM_SetZoneData),           %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetZoneData1",       %thinBasic_ReturnNumber,      CodePtr(EVM_GetZoneData1),          %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetZoneData2",       %thinBasic_ReturnNumber,      CodePtr(EVM_GetZoneData2),          %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_SetZoneCoords",      %thinBasic_ReturnNumber,      CodePtr(EVM_SetZoneCoords),         %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_GetZoneActive",      %thinBasic_ReturnNumber,      CodePtr(EVM_GetZoneActive),         %thinBasic_ForceOverWrite
    thinBasic_LoadSymbol "TBEM_CheckZones",         %thinBasic_ReturnNumber,      CodePtr(EVM_CheckZones),            %thinBasic_ForceOverWrite






    '---
    'Here define/add new thinBasic equates
    '---
    'thinBasic_AddEquate  "%MyNumericEquate",             ""       , 1
    'thinBasic_AddEquate  "$MyStringEquate",              "ABC"    , 0


    EVM_INIT()
  End Function

  '----------------------------------------------------------------------------
  Function UnLoadLocalSymbols Alias "UnLoadLocalSymbols" () Export As Long
  ' This function is automatically called by thinCore whenever this DLL is unloaded.
  ' This function CAN be present but it is not necessary. If present, this function
  ' will be executed by thinBasic core when module will be released.
  ' Use this function to perform uninitialize process, if needed.
  '----------------------------------------------------------------------------
    EVM_Close() 

    '---
    'Add here DeInitialization code if needed
    '---
    Function = 0&
  End Function


  Function LibMain Alias "LibMain" (ByVal hInstance   As Long, _
                                    ByVal fwdReason   As Long, _
                                    ByVal lpvReserved As Long) Export As Long
    Select Case fwdReason
      Case %DLL_PROCESS_ATTACH  
        'dInstance = hInstance    
        'Dim effectmem(8,%effectmemsize) As Global Long 
        
        Function = 1
        Exit Function
      Case %DLL_PROCESS_DETACH   
        
        Function = 1
        Exit Function
      Case %DLL_THREAD_ATTACH
      
        Function = 1
        Exit Function
      Case %DLL_THREAD_DETACH
      
        Function = 1
        Exit Function
    End Select 
    
  End Function
