Attribute VB_Name = "modGameplayTimer"
Private Declare Function cppticks_get Lib "cppticks.dll" () As Long

Private cyclesInt As Boolean
Private cyclesFin As Boolean
Private cyclesCurrent As Long
Private cyclesTotal As Long

Private realTimeStart As Long
Private realTimeStop As Long
Private realTimeCurrentStart As Long
Private realTimeCurrentStop As Long

Public g_enableGameplayTimer As Boolean
Private doPrintTimerDiffs As Boolean

Public Sub gpTimerInit()
    doPrintTimerDiffs = False

    cyclesInt = False
    cyclesFin = False
    cyclesCurrent = 0
    cyclesTotal = 0
    realTimeStart = 0
    realTimeStop = 0
    realTimeCurrentStart = 0
    realTimeCurrentStop = 0
End Sub

Public Sub gpTimerResetTotal()
    cyclesInt = False
    cyclesFin = False
    cyclesCurrent = 0
    cyclesTotal = 0
    realTimeStart = cppticks_get()
    realTimeStop = 0
    realTimeCurrentStart = cppticks_get()
    realTimeCurrentStop = 0
End Sub

Public Sub gpTimerResetCurrent()
    cyclesCurrent = 0
    realTimeCurrentStart = cppticks_get()
    realTimeCurrentStop = 0
End Sub

Public Sub gpTimerTick()
    Dim in_leveltest_restart_screen As Boolean
    Dim in_normal_level_play As Boolean

    If Not g_enableGameplayTimer Or LevelEditor Or WorldEditor Or GameMenu Or GameOutro Or BattleMode Then
        Exit Sub
    End If

    If Not cyclesInt Then
        cyclesInt = True
        cyclesCurrent = 0
        cyclesTotal = 0
        cyclesFin = False
        realTimeStart = cppticks_get()
    End If

    in_leveltest_restart_screen = GamePaused And LevelBeatCode < 0
    in_normal_level_play = Not LevelSelect And LevelMacro = 0

    If Not in_leveltest_restart_screen And (LevelSelect Or in_normal_level_play) Then
        cyclesCurrent = cyclesCurrent + 1
    ElseIf realTimeCurrentStop = 0 Then
        realTimeCurrentStop = cppticks_get()
    End If

    If Not cyclesFin Then
        cyclesTotal = cyclesTotal + 1
    End If
End Sub

Public Sub gpTimerBossDead()
    cyclesFin = True
End Sub

Private Function gpFormatTimeTicks(t As Long, Optional ByVal isRealTime As Boolean = False) As String
    Dim realMilliseconds As Long
    Dim miliseconds As Long
    Dim realSeconds As Long
    Dim seconds As Long
    Dim realMinutes As Long
    Dim minutes As Long
    Dim realHours As Long
    Dim hours As Long
    Dim days As Long

    If isRealTime Then
        realMilliseconds = t
    Else
        realMilliseconds = CLng(CDbl(t) * 15.6)
    End If
    miliseconds = realMilliseconds Mod 1000
    realSeconds = CLng(CDbl(realMilliseconds) / 1000)
    seconds = realSeconds Mod 60
    realMinutes = CLng(CDbl(realSeconds) / 60)
    minutes = realMinutes Mod 60
    realHours = CLng(CDbl(realMinutes) / 60)
    hours = realHours Mod 24
    days = CLng(realHours / 24)

    If days >= 1 Then
        gpFormatTimeTicks = Format(days, "00") & ":" & Format(hours, "00") & ":" & Format(minutes, "00") & ":" & Format(seconds, "00") & "." & Format(miliseconds, "000")
    ElseIf realHours >= 1 Then
        gpFormatTimeTicks = Format(hours, "00") & ":" & Format(minutes, "00") & ":" & Format(seconds, "00") & "." & Format(miliseconds, "000")
    ElseIf realHours < 1 Then
        gpFormatTimeTicks = Format(minutes, "00") & ":" & Format(seconds, "00") & "." & Format(miliseconds, "000")
    End If
End Function

Public Sub gpTimerDoPrintDiffs()
    doPrintTimerDiffs = True
End Sub

Public Sub gpTimerPrint()
    Dim timeDiff As Long
    Dim timeDiffTotal As Long
    Dim totalLines As Long

    If Not g_enableGameplayTimer Or LevelEditor Or WorldEditor Or GameMenu Or GameOutro Or BattleMode Then
        Exit Sub
    End If

    If realTimeCurrentStop = 0 Then
        timeDiff = cppticks_get() - realTimeCurrentStart
    Else
        timeDiff = realTimeCurrentStop - realTimeCurrentStart
    End If

    timeDiffTotal = cppticks_get() - realTimeStart

    totalLines = 6

    SuperPrint "--- Current ---", 5, 10, ScreenH - (18 * totalLines)
    totalLines = totalLines - 1
    
    SuperPrint "In-game: " & gpFormatTimeTicks(cyclesCurrent) & " (" & CStr(cyclesCurrent) & " ticks)", 5, 10, ScreenH - (18 * totalLines)
    totalLines = totalLines - 1
    
    SuperPrint "Realtime:" & gpFormatTimeTicks(CLng(timeDiff), True) & " (" & CStr(timeDiff) & " ms)", 5, 10, ScreenH - (18 * totalLines)
    totalLines = totalLines - 1

    SuperPrint "--- Total ---", 5, 10, ScreenH - (18 * totalLines)
    totalLines = totalLines - 1
    
    SuperPrint "In-game: " & gpFormatTimeTicks(cyclesTotal) & " (" & CStr(cyclesTotal) & " ticks)", 5, 10, ScreenH - (18 * totalLines)
    totalLines = totalLines - 1

    SuperPrint "Realtime:" & gpFormatTimeTicks(CLng(timeDiffTotal), True) & " (" & CStr(timeDiffTotal) & " ms)", 5, 10, ScreenH - (18 * totalLines)
    totalLines = totalLines - 1

    If doPrintTimerDiffs Then
        If g_oldKeyholeTimer Then
            SuperPrint "--- Old keyhole ticks ---", 5, 190, 200
        Else
            SuperPrint "--- New keyhole ticks ---", 5, 190, 200
        End If

        totalLines = 11

        SuperPrint "--- Difference ---", 5, 100, ScreenH - (18 * totalLines)
        totalLines = totalLines - 1

        SuperPrint "IG: " & CStr(cyclesTotal - cyclesCurrent) & " ticks", 5, 100, ScreenH - (18 * totalLines)
        totalLines = totalLines - 1

        SuperPrint "IG: " & CStr(CLng(CDbl(cyclesTotal - cyclesCurrent) * 15.6)) & " ms counted", 5, 100, ScreenH - (18 * totalLines)
        totalLines = totalLines - 1

        SuperPrint "RT: " & CStr(timeDiffTotal - timeDiff) & " ms de-facto", 5, 100, ScreenH - (18 * totalLines)
        totalLines = totalLines - 1
        doPrintTimerDiffs = False
    End If
End Sub
