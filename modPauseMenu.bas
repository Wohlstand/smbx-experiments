Attribute VB_Name = "modPauseMenu"
Private stopPause As Boolean
Private noButtons As Boolean
Private pauseCurPlr As Integer

Private Sub PaseGameInit()
    stopPause = False
    noButtons = False
End Sub

Public Function PauseGameStopped() As Boolean
    PauseGameStopped = stopPause
End Function

Public Sub PauseGameLoop()
    Dim A As Integer
    Dim B As Integer

    DoEvents
    CheckActive

    gpTimerTick

    If LevelSelect = True Then
        UpdateGraphics2
    Else
        UpdateGraphics
    End If
    UpdateControls
    UpdateSound
    BlockFrames
    UpdateEffects

    If SingleCoop > 0 Or numPlayers > 2 Then
        For A = 1 To numPlayers
            Player(A).Controls = Player(1).Controls
        Next A
    End If

    With Player(pauseCurPlr).Controls
        If MessageText = "" Then
            If noButtons = False Then
                If .Down = False And .Up = False And .Run = False And .Jump = False And .Start = False Then
                    If (GetKeyState(vbKeyEscape) And KEY_PRESSED) Or (GetKeyState(vbKeySpace) And KEY_PRESSED) Or (GetKeyState(vbKeyReturn) And KEY_PRESSED) Or (GetKeyState(vbKeyDown) And KEY_PRESSED) Or (GetKeyState(vbKeyUp) And KEY_PRESSED) Then
                    Else
                        noButtons = True
                    End If
                End If
            Else
                If (GetKeyState(vbKeyEscape) And KEY_PRESSED) Then
                    If LevelSelect = True And Cheater = False Then
                        If MenuCursor <> 2 Then PlaySound 26
                        MenuCursor = 2
                    Else
                        If MenuCursor <> 1 Then PlaySound 26
                        MenuCursor = 1
                    End If
                    noButtons = False
                ElseIf .Start = True Then
                    stopPause = True
                End If
                If .Up = True Or (GetKeyState(vbKeyUp) And KEY_PRESSED) Then
                    PlaySound 26
                    MenuCursor = MenuCursor - 1
                    noButtons = False
                ElseIf .Down = True Or (GetKeyState(vbKeyDown) And KEY_PRESSED) Then
                    PlaySound 26
                    MenuCursor = MenuCursor + 1
                    noButtons = False
                End If

                If LevelSelect = True Then
                    If Player(A).Character = 1 Or Player(A).Character = 2 Then Player(A).Hearts = 0
                    For A = 1 To numPlayers
                        If Player(A).RunRelease = False Then
                            If Player(A).Controls.Left = False And Player(A).Controls.Right = False Then Player(A).RunRelease = True


                        ElseIf Player(A).Controls.Left = True Or Player(A).Controls.Right = True Then

                            AllCharBlock = 0
                            For B = 1 To numCharacters
                                If blockCharacter(B) = False Then
                                    If AllCharBlock = 0 Then
                                        AllCharBlock = B
                                    Else
                                        AllCharBlock = 0
                                        Exit For
                                    End If
                                End If
                            Next B
                            If AllCharBlock = 0 Then
                                PlaySound 26
                                Player(A).RunRelease = False
                                If A = 1 Then
                                    B = 2
                                Else
                                    B = 1
                                End If
                                If numPlayers = 1 Then B = 0
                                Player(0).Character = 0
                                If Player(A).Controls.Left = True Then
                                    Do
                                        Player(A).Character = Player(A).Character - 1
                                        If Player(A).Character <= 0 Then Player(A).Character = 5
                                    Loop While Player(A).Character = Player(B).Character Or blockCharacter(Player(A).Character) = True
                                Else
                                    Do
                                        Player(A).Character = Player(A).Character + 1
                                        If Player(A).Character >= 6 Then Player(A).Character = 1
                                    Loop While Player(A).Character = Player(B).Character Or blockCharacter(Player(A).Character) = True
                                End If
                                Player(A) = SavedChar(Player(A).Character)
                                SetupPlayers
                            End If
                        End If


                    Next A
                End If

                If .Jump = True Or (GetKeyState(vbKeySpace) And KEY_PRESSED) Or (GetKeyState(vbKeyReturn) And KEY_PRESSED) Then
                    If MenuCursor = 0 Then
                        stopPause = True
                    ElseIf MenuCursor = 1 And (LevelSelect = True Or (StartLevel = FileName And NoMap = True)) And Cheater = False Then
                        SaveGame
                        stopPause = True
                    Else
                        If Cheater = False And (LevelSelect = True Or (StartLevel = FileName And NoMap = True)) Then SaveGame
                        stopPause = True
                        GameMenu = True
                        MenuMode = 0
                        MenuCursor = 0
                        If LevelSelect = False Then
                            LevelSelect = True
                            EndLevel = True
                        Else
                            LevelSelect = False
                        End If
                        BitBlt myBackBuffer, 0, 0, 800, 600, 0, 0, 0, vbWhiteness
                        BitBlt frmMain.hdc, 0, 0, frmMain.ScaleWidth, frmMain.ScaleHeight, 0, 0, 0, vbWhiteness
                        StopMusic
                        DoEvents
                        Sleep 500
                    End If
                End If
                If Cheater = True Or Not (LevelSelect = True Or (StartLevel = FileName And NoMap = True)) Then
                    If MenuCursor > 1 Then MenuCursor = 0
                    If MenuCursor < 0 Then MenuCursor = 1
                Else
                    If MenuCursor > 2 Then MenuCursor = 0
                    If MenuCursor < 0 Then MenuCursor = 2
                End If
            End If
        Else
            If noButtons = False Then
                If .Down = False And .Up = False And .Run = False And .Jump = False And .Start = False Then
                    If (GetKeyState(vbKeyEscape) And KEY_PRESSED) Or (GetKeyState(vbKeySpace) And KEY_PRESSED) Or (GetKeyState(vbKeyReturn) And KEY_PRESSED) Or (GetKeyState(vbKeyDown) And KEY_PRESSED) Or (GetKeyState(vbKeyUp) And KEY_PRESSED) Then
                    Else
                        noButtons = True
                    End If
                End If
            Else
                If (GetKeyState(vbKeyEscape) And KEY_PRESSED) Or .Jump = True Or .Run = True Or .Start = True Or (GetKeyState(vbKeySpace) And KEY_PRESSED) Or (GetKeyState(vbKeyReturn) And KEY_PRESSED) Then
                    stopPause = True
                End If
            End If
        End If
    End With
    ' LOOP CODE END
End Sub

Public Sub PauseGame(plr As Integer)
    ' Dim stopPause As Boolean
    Dim A As Integer
    Dim B As Integer
    ' Dim noButtons
    ' Dim fpsTime As Double
    ' Dim fpsCount As Integer

    For A = numPlayers To 1 Step -1
        SavedChar(Player(A).Character) = Player(A)
    Next A

    If TestLevel = True And MessageText = "" Then Exit Sub
    If MessageText = "" Then
        PlaySound 30
    Else
        SoundPause(47) = 0
        PlaySound 47
    End If

    GamePaused = True
    MenuCursor = 0
    MenuCursorCanMove = False
    
    If PSwitchTime > 0 Then
        'If noSound = False Then mciSendString "pause smusic", 0, 0, 0
        If noSound = False Then SoundPauseAll
    End If

    PaseGameInit
    pauseCurPlr = plr

    runSceneLoop (ScenePauseMenu)

    ' overTime = 0
    ' GoalTime = GetTickCount + 1000
    ' fpsCount = 0
    ' fpsTime = 0
    ' cycleCount = 0
    ' gameTime = 0
    ' Do
        ' tempTime = GetTickCount
        ' If tempTime >= gameTime + frameRate Or tempTime < gameTime Or MaxFPS = True Then
            ' If fpsCount >= 32000 Then fpsCount = 0 'Fixes Overflow bug
            ' If cycleCount >= 32000 Then cycleCount = 0 'Fixes Overflow bug
            ' overTime = overTime + (tempTime - (gameTime + frameRate))
            ' If gameTime = 0 Then overTime = 0
            ' If overTime <= 1 Then
                ' overTime = 0
            ' ElseIf overTime > 1000 Then
                ' overTime = 1000
            ' End If
            ' gameTime = tempTime - overTime
            ' overTime = (overTime - (tempTime - gameTime))
            ' If GetTickCount > fpsTime Then
                ' If cycleCount >= 65 Then
                    ' overTime = 0
                    ' gameTime = tempTime
                ' End If
                ' cycleCount = 0
                ' fpsTime = GetTickCount + 1000
                ' GoalTime = fpsTime
                ' If Debugger = True Then frmLevelDebugger.lblFPS = fpsCount
                ' If ShowFPS = True Then
                    ' PrintFPS = fpsCount
                ' End If
                ' fpsCount = 0
            ' End If

            ' ' LOOP CODE START
            ' PauseGameLoop
            ' ' LOOP CODE END
        ' End If

        ' If qScreen = True Then stopPause = False

        ' Sleep sleepDelay
    ' Loop Until stopPause = True

    resetFrameTimer

    GamePaused = False
    Player(plr).UnStart = False
    Player(plr).CanJump = False
    If MessageText = "" Then PlaySound 30
    If PSwitchTime > 0 Then
        ' If noSound = False Then mciSendString "resume smusic", 0, 0, 0
        If noSound = False Then SoundResumeAll
    End If
    MessageText = ""

    resetFrameTimer
    'overTime = 0
    'GoalTime = GetTickCount + 1000
    'fpsCount = 0
    'cycleCount = 0
    'gameTime = 0
    'fpsTime = 0

End Sub

