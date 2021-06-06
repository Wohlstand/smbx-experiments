Attribute VB_Name = "modRecord"
'this module handles particular the control recording and playback functions
' and the gameplay stats recording functions

Public g_recordControlReplay As Boolean
Public g_recordControlRecord As Boolean
Public g_recordReplayId As Integer

Public g_recordNumRenderedNPCs As Integer
Public g_recordNumRenderedBlocks As Integer
Public g_recordNumRenderedBGOs As Integer

Private s_recordControlFD As Integer
Private s_recordGameplayFD As Integer
Private in_level As Boolean
Private frame_no As Long
Private next_delta_frame As Long
Private last_controls(1 To maxPlayers) As Controls

Public Sub record_init()
    in_level = True
    frame_no = 0
    next_delta_frame = -1
    g_recordNumRenderedNPCs = 0
    g_recordNumRenderedBlocks = 0
    g_recordNumRenderedBGOs = 0

    If g_recordControlRecord Or g_recordControlReplay Then
        Call random_seed(310)
    End If

    Dim nextrun As Integer
    Dim idStr As String
    Dim lastId As String

    nextrun = -1
    Do
        nextrun = nextrun + 1
        idStr = Mid(Str(nextrun), 2)
    Loop While Dir(FullFileName + "." + idStr + ".c.txt") <> "" Or Dir(FullFileName + "." + idStr + ".g.txt") <> ""
    If g_recordReplayId <> -1 Then
        nextrun = g_recordReplayId + 1
    End If
    lastId = Mid(Str(nextrun - 1), 2)

    If g_recordControlRecord = True And g_recordControlReplay = False Then
        If s_recordControlFD = 0 Then
            s_recordControlFD = 25
            Open FullFileName + "." + idStr + ".c.txt" For Output As #s_recordControlFD
        End If
        If s_recordGameplayFD = 0 Then
            s_recordGameplayFD = 30
            Open FullFileName + "." + idStr + ".g.txt" For Output As #s_recordGameplayFD
        End If
    ElseIf g_recordControlReplay = True Then
        If s_recordControlFD = 0 And Dir(FullFileName + "." + lastId + ".c.txt") <> "" Then
            s_recordControlFD = 25
            Open FullFileName + "." + lastId + ".c.txt" For Input As #s_recordControlFD
        End If
        If s_recordGameplayFD = 0 And Dir(FullFileName + "." + lastId + ".g.txt") <> "" Then
            s_recordGameplayFD = 29
            Open FullFileName + "." + lastId + ".g.txt" For Input As #s_recordGameplayFD
            record_readstate
            Close #s_recordGameplayFD
            s_recordGameplayFD = 0
        End If
        If Not s_recordControlFD = 0 Then
            If s_recordGameplayFD = 0 Then
                s_recordGameplayFD = 30
                Open FullFileName + "." + lastId + ".r.smbx.txt" For Output As #s_recordGameplayFD
            End If
        End If
    End If

    If Not s_recordGameplayFD = 0 Then
        record_writestate
    End If
    
    For i = 1 To numPlayers
        last_controls(i).Up = False
        last_controls(i).Down = False
        last_controls(i).Left = False
        last_controls(i).Right = False
        last_controls(i).Jump = False
        last_controls(i).Run = False
        last_controls(i).AltJump = False
        last_controls(i).AltRun = False
        last_controls(i).Start = False
        last_controls(i).Drop = False
    Next i
End Sub

Public Sub record_writestate()
    ' write all necessary state variables!
    Print #s_recordGameplayFD, "Init"
    Print #s_recordGameplayFD, "SMBX" ' game version / commit
    Print #s_recordGameplayFD, 3 ' compatibility mode
    Print #s_recordGameplayFD, FileName ' level that was played
    If Checkpoint = FullFileName Then
        Print #s_recordGameplayFD, 1
    Else
        Print #s_recordGameplayFD, 0
    End If
    Print #s_recordGameplayFD, StartWarp
    Print #s_recordGameplayFD, ReturnWarp
    Print #s_recordGameplayFD, Lives
    Print #s_recordGameplayFD, Coins
    Print #s_recordGameplayFD, Score
    Print #s_recordGameplayFD, numStars
    For A = 1 To numStars
        Print #s_recordGameplayFD, "Star"
        Print #s_recordGameplayFD, Star(A).level
        Print #s_recordGameplayFD, Star(A).Section
    Next A
    Print #s_recordGameplayFD, numPlayers
    For A = 1 To numPlayers
        Print #s_recordGameplayFD, "Player"
        Print #s_recordGameplayFD, Player(A).Character
        Print #s_recordGameplayFD, Player(A).State
        Print #s_recordGameplayFD, Player(A).MountType
        Print #s_recordGameplayFD, Player(A).HeldBonus
    Next A
End Sub

Public Sub record_readstate()
    ' read all necessary state variables!
    Dim S As String
    Dim N As Integer
    Input #s_recordGameplayFD, S ' "Init"
    Input #s_recordGameplayFD, S ' game version / commit
    Input #s_recordGameplayFD, N ' compatibility mode
    If N <> 3 Then
        MsgBox ("Warning: recording not made in compatibility mode 3. Do not expect identical results.")
    End If
    Input #s_recordGameplayFD, S ' level that was played
    If S <> FileName Then
        MsgBox ("Warning: FileName does not match.")
    End If
    Input #s_recordGameplayFD, N ' is there a checkpoint?
    If N <> 0 Then
        Checkpoint = FullFileName
    Else
        Checkpoint = ""
    End If
    Input #s_recordGameplayFD, StartWarp
    Input #s_recordGameplayFD, ReturnWarp
    Input #s_recordGameplayFD, Lives
    Input #s_recordGameplayFD, Coins
    Input #s_recordGameplayFD, Score
    Input #s_recordGameplayFD, numStars
    For A = 1 To numStars
        Input #s_recordGameplayFD, S ' "Star"
        Input #s_recordGameplayFD, Star(A).level
        Input #s_recordGameplayFD, Star(A).Section
    Next A
    Input #s_recordGameplayFD, numPlayers
    For A = 1 To numPlayers
        Input #s_recordGameplayFD, S ' "Player"
        Input #s_recordGameplayFD, Player(A).Character
        Input #s_recordGameplayFD, Player(A).State
        Input #s_recordGameplayFD, Player(A).MountType
        Input #s_recordGameplayFD, Player(A).HeldBonus
    Next A
    ' stars must appear if and only if they originally did
    For A = 1 To numNPCs
        If NPC(A).Type = 97 Or NPC(A).Type = 196 Then
            NPC(A).Special = 0
            NPC(A).DefaultSpecial = 0
            NPC(A).Killed = 0
            For B = 1 To numStars
                If Star(B).level = FileName And (Star(B).Section = NPC(A).Section Or Star(B).Section = -1) Then
                    NPC(A).Special = 1
                    NPC(A).DefaultSpecial = 1
                    If NPC(A).Type = 196 Then NPC(A).Killed = 9
                    Exit For
                End If
            Next B
        End If
    Next A
End Sub

Public Sub record_finish()
    in_level = False
    
    If Not s_recordGameplayFD = 0 Then
        ' how did the level end?
        Print #s_recordGameplayFD, "End"
        Print #s_recordGameplayFD, frame_no
        Print #s_recordGameplayFD, LevelBeatCode
        Close #s_recordGameplayFD
        s_recordGameplayFD = 0
    End If

    If Not s_recordControlFD = 0 Then
        Close #s_recordControlFD
        s_recordControlFD = 0
    End If
End Sub

Public Sub record_sync()
    If Not in_level Then
        Exit Sub
    End If
        
    If s_recordControlFD = 0 And s_recordGameplayFD = 0 Then
        Exit Sub
    End If

    If g_recordControlReplay = True And Not s_recordControlFD = 0 Then
        If next_delta_frame = -1 And Not s_recordControlFD = 0 Then
            If EOF(s_recordControlFD) Then
                Close #s_recordControlFD
                s_recordControlFD = 0
            Else
                Input #s_recordControlFD, next_delta_frame
            End If
        End If

        Do Until Not next_delta_frame = frame_no Or s_recordControlFD = 0
            Dim command As String
            Dim is_on As Boolean
            Dim key As String
            Dim i As Integer

            If EOF(s_recordControlFD) Then
                Close #s_recordControlFD
                s_recordControlFD = 0
                Exit Do
            Else
                Input #s_recordControlFD, command
            End If

            If Mid(command, 1, 1) = "+" Then
                is_on = True
            Else
                is_on = False
            End If

            i = CInt(Mid(command, 2, 1))
            key = Mid(command, 3, 1)
            If key = "U" Then
                last_controls(i).Up = is_on
            ElseIf key = "D" Then
                last_controls(i).Down = is_on
            ElseIf key = "L" Then
                last_controls(i).Left = is_on
            ElseIf key = "R" Then
                last_controls(i).Right = is_on
            ElseIf key = "S" Then
                last_controls(i).Start = is_on
            ElseIf key = "I" Then
                last_controls(i).Drop = is_on
            ElseIf key = "A" Then
                last_controls(i).Jump = is_on
            ElseIf key = "B" Then
                last_controls(i).Run = is_on
            ElseIf key = "X" Then
                last_controls(i).AltJump = is_on
            ElseIf key = "Y" Then
                last_controls(i).AltRun = is_on
            End If

            If EOF(s_recordControlFD) Then
                Close #s_recordControlFD
                s_recordControlFD = 0
                Exit Do
            Else
                Input #s_recordControlFD, next_delta_frame
            End If
        Loop
        For i = 1 To numPlayers
            Player(i).Controls = last_controls(i)
        Next i
    ElseIf g_recordControlRecord And Not s_recordControlFD = 0 Then
        For i = 1 To numPlayers
            Dim iStr As String
            iStr = Mid(Str(i), 2, 1) ' get the actual numerical character...
            If Not last_controls(i).Up And Player(i).Controls.Up Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "+" + iStr + "U"
            ElseIf last_controls(i).Up And Not Player(i).Controls.Up Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "-" + iStr + "U"
            End If
            If Not last_controls(i).Down And Player(i).Controls.Down Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "+" + iStr + "D"
            ElseIf last_controls(i).Down And Not Player(i).Controls.Down Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "-" + iStr + "D"
            End If
            If Not last_controls(i).Left And Player(i).Controls.Left Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "+" + iStr + "L"
            ElseIf last_controls(i).Left And Not Player(i).Controls.Left Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "-" + iStr + "L"
            End If
            If Not last_controls(i).Right And Player(i).Controls.Right Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "+" + iStr + "R"
            ElseIf last_controls(i).Right And Not Player(i).Controls.Right Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "-" + iStr + "R"
            End If
            If Not last_controls(i).Jump And Player(i).Controls.Jump Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "+" + iStr + "A"
            ElseIf last_controls(i).Jump And Not Player(i).Controls.Jump Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "-" + iStr + "A"
            End If
            If Not last_controls(i).Run And Player(i).Controls.Run Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "+" + iStr + "B"
            ElseIf last_controls(i).Run And Not Player(i).Controls.Run Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "-" + iStr + "B"
            End If
            If Not last_controls(i).AltJump And Player(i).Controls.AltJump Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "+" + iStr + "X"
            ElseIf last_controls(i).AltJump And Not Player(i).Controls.AltJump Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "-" + iStr + "X"
            End If
            If Not last_controls(i).AltRun And Player(i).Controls.AltRun Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "+" + iStr + "Y"
            ElseIf last_controls(i).AltRun And Not Player(i).Controls.AltRun Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "-" + iStr + "Y"
            End If
            If Not last_controls(i).Start And Player(i).Controls.Start Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "+" + iStr + "S"
            ElseIf last_controls(i).Start And Not Player(i).Controls.Start Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "-" + iStr + "S"
            End If
            If Not last_controls(i).Drop And Player(i).Controls.Drop Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "+" + iStr + "I"
            ElseIf last_controls(i).Drop And Not Player(i).Controls.Drop Then
                Print #s_recordControlFD, frame_no
                Print #s_recordControlFD, "-" + iStr + "I"
            End If
            last_controls(i) = Player(i).Controls
        Next i
    End If

    If Not s_recordGameplayFD = 0 And frame_no Mod 60 = 0 Then
        Print #s_recordGameplayFD, "Frame"
        Print #s_recordGameplayFD, frame_no
        Print #s_recordGameplayFD, Score
        Print #s_recordGameplayFD, numNPCs
        Dim numActiveNPCs As Integer
        numActiveNPCs = 0
        If frame_no <> 0 Then
            For i = 1 To numNPCs
                If NPC(i).Active Then
                    numActiveNPCs = numActiveNPCs + 1
                End If
            Next i
        End If
        Print #s_recordGameplayFD, numActiveNPCs
        Print #s_recordGameplayFD, g_recordNumRenderedNPCs
        Print #s_recordGameplayFD, g_recordNumRenderedBlocks
        Print #s_recordGameplayFD, g_recordNumRenderedBGOs
        For i = 1 To numPlayers
            Print #s_recordGameplayFD, Player(i).Location.X
            Print #s_recordGameplayFD, Player(i).Location.Y
        Next i
    End If
    frame_no = frame_no + 1
End Sub
