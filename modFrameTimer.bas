Attribute VB_Name = "modFrameTimer"
Private Declare Sub cppticks_init Lib "cppticks.dll" ()
Private Declare Sub cppticks_quit Lib "cppticks.dll" ()
Private Declare Function cppticks_get Lib "cppticks.dll" () As Long

Public Enum LoopScene
    SceneMainMenu
    SceneWorldMap
    SceneWorldPathWait
    SceneLevel
    SceneOutro
    SceneEditor
    SceneKeyHole
    ScenePauseMenu
End Enum

Private Const useNewTimer As Boolean = True

Private Const sleepDelay As Long = 1

' Old Timer's data
Private Const frameRate As Double = 15 'for controlling game speed
Private cycleCount As Integer
Private fpsTime As Double
Private fpsCount As Double
Private goalTime As Double
Private overTime As Double
Private gameTime As Double

' New Timer's data
Private currentTicks As Double
Private Const frameRateNano As Double = 1000# / 64.1025

Private oldTime As Double
Private overHead As Double
Private startProcessing As Double
Private stopProcessing As Double
Private doUpdate As Double

Private Const TimeStoreSize As Integer = 4
Private TimeStorePos As Integer
Private TimeStoreSum As Double
Private TimeStoreItems(0 To 3) As Double

' TimeStore
Public Sub InitFrameTimer()
    cppticks_init

    currentTicks = 0
    oldTime = 0
    overHead = 0
    startProcessing = 0
    stopProcessing = 0
    doUpdate = 0

    TimeStorePos = 0
    TimeStoreSum = 0
    TimeStoreItems(0) = 0
    TimeStoreItems(1) = 0
    TimeStoreItems(2) = 0
    TimeStoreItems(3) = 0
End Sub

Public Sub QuitFrameTimer()
    cppticks_quit
End Sub

Private Sub TimeStoreAdd(ByVal item As Double)
    TimeStoreSum = TimeStoreSum - TimeStoreItems(TimeStorePos)
    TimeStoreSum = TimeStoreSum + item
    TimeStoreItems(TimeStorePos) = item
    TimeStorePos = TimeStorePos + 1
    If TimeStorePos >= TimeStoreSize Then
        TimeStorePos = 0
    End If
End Sub

Private Function TimeStoreAverage() As Double
    TimeStoreAverage = TimeStoreSum / TimeStoreSize
End Function

Private Function getElapsedTime(ByVal oldTime As Double) As Double
    getElapsedTime = cppticks_get() - oldTime
End Function

Private Function getSleepTime(ByVal oldTime As Double, ByVal target As Double) As Double
    getSleepTime = target - getElapsedTime(oldTime)
End Function

Private Sub xtech_nanosleep(ByVal sleepTime As Double)
    Dim doSleepTime As Long
    If sleepTime <= 0 Then Exit Sub
    doSleepTime = CLng(CDbl(CLng(sleepTime + 0.5)))
    Sleep doSleepTime
End Sub

Public Sub cycleNextInc()
    cycleCount = cycleCount + 1
End Sub

Public Sub frameNextInc()
    fpsCount = fpsCount + 1
End Sub

Private Sub computeFrameTime1Real_old()
    If fpsCount >= 32000 Then fpsCount = 0 'Fixes Overflow bug
    If cycleCount >= 32000 Then cycleCount = 0 'Fixes Overflow bug

    overTime = overTime + (currentTicks - (gameTime + frameRate))

    If gameTime = 0 Then overTime = 0

    If overTime <= 1 Then
        overTime = 0
    ElseIf overTime > 1000 Then
        overTime = 1000
    End If

    gameTime = currentTicks - overTime
    overTime = (overTime - (currentTicks - gameTime))
End Sub

Private Sub computeFrameTime2Real_old()
    If cppticks_get > fpsTime Then
        If cycleCount >= 65 Then
            overTime = 0
            gameTime = currentTicks
        End If
        cycleCount = 0
        fpsTime = cppticks_get + 1000
        goalTime = fpsTime
        If Debugger = True Then frmLevelDebugger.lblFPS = fpsCount
        If ShowFPS = True Then
            PrintFPS = fpsCount
        End If
        fpsCount = 0
    End If
End Sub

Private Sub computeFrameTime1Real_new()
    If fpsCount >= 32000 Then fpsCount = 0 'Fixes Overflow bug
    If cycleCount >= 32000 Then cycleCount = 0 'Fixes Overflow bug
End Sub

Private Sub computeFrameTime2Real_new()
    Dim Start As Double
    Dim sleepTime As Double
    Dim adjustedSleepTime As Double
    Dim E As Double
    Dim overslept As Double

    If doUpdate > 0 Then
        doUpdate = doUpdate - frameRateNano
    End If
    startProcessing = 0
    stopProcessing = 0

    If cppticks_get > fpsTime Then
        If cycleCount >= 65 Then
            overTime = 0
            gameTime = currentTicks
        End If

        cycleCount = 0
        fpsTime = cppticks_get + 1000
        goalTime = fpsTime

        If ShowFPS Then
            PrintFPS = fpsCount
        End If
        fpsCount = 0
    End If

    If Not MaxFPS Then
        Start = cppticks_get
        sleepTime = getSleepTime(oldTime, frameRateNano)
        overHead = TimeStoreAverage()

        If sleepTime > overHead Then
            adjustedSleepTime = sleepTime - overHead
            If adjustedSleepTime > 500 Then
                frmDebugLog.AddMsg "frame_timer: Adjusted sleep time got a too big value: " & CStr(adjustedSleepTime)
                adjustedSleepTime = 500
            End If

            xtech_nanosleep adjustedSleepTime

            E = getElapsedTime(Start)
            overslept = E - adjustedSleepTime
            If overslept < 0 Then
                TimeStoreAdd 0
            ElseIf overslept < frameRateNano Then
                TimeStoreAdd overslept
            End If
        End If
    End If

    oldTime = cppticks_get
End Sub

Public Sub computeFrameTime1()
    If useNewTimer Then
        computeFrameTime1Real_new
    Else
        computeFrameTime1Real_old
    End If
End Sub

Public Sub computeFrameTime2()
    If useNewTimer Then
        computeFrameTime2Real_new
    Else
        computeFrameTime2Real_old
    End If
End Sub

Public Function frameSkipNeeded() As Boolean
    If useNewTimer Then
        frameSkipNeeded = doUpdate > 0
    Else
        If cppticks_get + Int(1000 * (1 - (cycleCount / 63))) > goalTime Then
            frameSkipNeeded = True
        Else
            frameSkipNeeded = False
        End If
    End If
End Function

Private Function canProcessFrameCond() As Boolean
    Dim ret As Boolean

    ret = currentTicks >= gameTime + frameRate Or currentTicks < gameTime Or MaxFPS = True
    If ret = True And doUpdate <= 0 Then
        startProcessing = cppticks_get
    End If

    canProcessFrameCond = ret
End Function

Public Function canProceedFrame() As Boolean
    currentTicks = cppticks_get
    canProceedFrame = canProcessFrameCond
End Function

Public Sub resetFrameTimer()
    overTime = 0
    fpsCount = 0
    fpsTime = 0
    cycleCount = 0
    gameTime = 0

    If useNewTimer Then
        goalTime = 0
    Else
        goalTime = cppticks_get + 1000
    End If

    doUpdate = 0
End Sub

Public Sub resetTimeBuffer()
    currentTicks = 0
End Sub

Public Sub frameRenderStart()
    If useNewTimer Then
        If doUpdate <= 0 Then
            startProcessing = cppticks_get
        End If
    End If
End Sub

Public Sub frameRenderEnd()
    Dim newTime As Double

    If useNewTimer Then
        If doUpdate <= 0 Then
            stopProcessing = cppticks_get
            newTime = IIf(FrameSkip, stopProcessing - startProcessing, 0)
            If newTime > frameRateNano * 25 Then
                frmDebugLog.AddMsg "Overloading detected: " & CStr(newTime / frameRateNano) & " frames to skip (" & CStr(newTime) & " milliseconds delay)"
                newTime = frameRateNano * 25
            End If
            doUpdate = doUpdate + newTime
            goalTime = CDbl(cppticks_get) + newTime
        End If
    End If
End Sub


Public Sub runSceneLoop(ByVal doScene As LoopScene)
    Dim C As Integer

    resetFrameTimer

    Do
        DoEvents
        currentTicks = cppticks_get

        Select Case doScene
            Case SceneWorldMap
                FreezeNPCs = False

            Case SceneOutro
                ScreenType = 0
                SetupScreens
        End Select

        If canProcessFrameCond() Then
            CheckActive

            ' Run loop (pre)
            Select Case doScene
                Case SceneMainMenu
                    MenuLoop    'Run the menu loop

                Case SceneWorldPathWait
                    gpTimerTick
                    UpdateGraphics2
                    UpdateSound
                    C = C + 1

                Case SceneEditor
                    EditorLoop  'Do the editor loop

                Case SceneOutro
                    OutroLoop
            End Select

            computeFrameTime1

            ' Run loop (post)
            Select Case doScene
                Case SceneWorldMap
                    WorldLoop
                Case SceneLevel
                    GameLoop    'Run the game loop
            End Select

            DoEvents

            computeFrameTime2

            Select Case doScene
                Case SceneLevel
                    If LivingPlayers = False Then
                        EveryonesDead
                    End If
                Case ScenePauseMenu
                    PauseGameLoop
            End Select

        End If

        Select Case doScene
            Case ScenePauseMenu
                If qScreen = True Then stopPause = False
        End Select

        If Not MaxFPS Then Sleep sleepDelay

        ' Quit loop condition
        Select Case doScene
            Case SceneMainMenu
                If Not GameMenu Then Exit Do
            Case SceneWorldMap
                If Not LevelSelect Then Exit Do
            Case SceneWorldPathWait
                If C >= 24 Then Exit Do
            Case SceneLevel
                If LevelSelect Or GameMenu Then Exit Do
            Case SceneEditor
                If Not LevelEditor Then Exit Do
            Case SceneOutro
                If Not GameOutro Then Exit Do
            Case ScenePauseMenu
                If PauseGameStopped() Then Exit Do
        End Select
    Loop

End Sub

