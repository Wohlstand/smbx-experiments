Attribute VB_Name = "modRecord"
'this module handles particular the control recording and playback functions
' and the gameplay stats recording functions

Public g_recordEnabled As Boolean

Public g_recordNumRenderedNPCs As Integer
Public g_recordNumRenderedBlocks As Integer
Public g_recordNumRenderedBGOs As Integer

' Version of record file: Increase this once new fields got added!
Private Const c_recordFileVersion As Integer = 2

Private s_recordFD As Integer
Private s_replayFD As Integer
Private in_level As Boolean
Private frame_no As Long
Private last_controls(1 To maxPlayers) As Controls
Private last_status_tick As Long

Public Sub record_init()
    If Not g_recordEnabled Then
        Exit Sub
    End If

    in_level = True
    last_status_tick = GetTickCount
    frame_no = 0
    g_recordNumRenderedNPCs = 0
    g_recordNumRenderedBlocks = 0
    g_recordNumRenderedBGOs = 0

    If s_recordFD = 0 Then
        s_recordFD = 25
        If Dir(App.Path + "\gameplay-records\", vbDirectory) = "" Then
            MkDir (App.Path + "\gameplay-records\")
        End If
        Open App.Path + "\gameplay-records\" + Replace(FileName, ".lvl", "") + "_SMBX-64_" + Format(Now, "yyyy-mm-dd_hh-mm-ss") + ".rec" For Output As #s_recordFD
    End If

    If Not s_recordFD = 0 Then
        Call record_write_header
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

    FrameSkip = False
    g_compatMode = True
End Sub

Private Sub record_write_header()
    ' write all necessary state variables!
    Print #s_recordFD, "Header"
    Print #s_recordFD, "RecordVersion " + Str(c_recordFileVersion) ' Version of record file
    Print #s_recordFD, "Version SMBX-64" ' game version / commit
    Print #s_recordFD, "CompatLevel 3" ' compatibility mode
    Print #s_recordFD, Replace(Replace(FullFileName, App.Path + "\", ""), "\", "/") ' level that was played
    Print #s_recordFD, "SumMD5 " + DigestFileToHexStr(FullFileName) ' Since version 2: md5 Hash sum of recorded level file

    Dim seed As Integer
    seed = random_int(32767)
    random_seed (seed)
    Print #s_recordFD, "Seed " + Str(seed) ' random seed (THINK ABOUT THIS)
    If Checkpoint = FullFileName Then
        Print #s_recordFD, "Checkpoint 1"
    Else
        Print #s_recordFD, "Checkpoint 0"
    End If
    Print #s_recordFD, "StartWarp " + Str(StartWarp)
    Print #s_recordFD, "ReturnWarp " + Str(ReturnWarp)
    Print #s_recordFD, "Lives " + Str(Lives)
    Print #s_recordFD, "Coins " + Str(Coins)
    Print #s_recordFD, "Score " + Str(Score)
    Print #s_recordFD, "Stars " + Str(numStars)
    For A = 1 To numStars
        Print #s_recordFD, "Star"
        Print #s_recordFD, Star(A).level
        Print #s_recordFD, "Section " + Str(Star(A).Section)
    Next A
    Print #s_recordFD, "Players " + Str(numPlayers)
    For A = 1 To numPlayers
        Print #s_recordFD, "Player"
        Print #s_recordFD, "Char " + Str(Player(A).Character)
        Print #s_recordFD, "State " + Str(Player(A).State)
        Print #s_recordFD, "MountType " + Str(Player(A).MountType)
        Print #s_recordFD, "HeldBonus " + Str(Player(A).HeldBonus)
    Next A
End Sub

' Private Sub record_read_header()
'     ' read all necessary state variables!
'     Dim S As String
'     Dim N As Integer
'     Input #s_replayFD, S ' "Init"
'     Input #s_replayFD, S ' game version / commit
'     Input #s_replayFD, N ' compatibility mode
'     If N <> 3 Then
'         MsgBox ("Warning: recording not made in compatibility mode 3. Do not expect identical results.")
'     End If
'     ' do the following smartly
'     Input #s_replayFD, S ' level that was played
'     If S <> FileName Then
'         MsgBox ("Warning: FileName does not match.")
'     End If
'     Input #s_replayFD, N ' random seed
'     Call random_seed(N)
'     Input #s_replayFD, N ' is there a checkpoint?
'     If N <> 0 Then
'         Checkpoint = FullFileName
'     Else
'         Checkpoint = ""
'     End If
'     Input #s_replayFD, StartWarp
'     Input #s_replayFD, ReturnWarp
'     Input #s_replayFD, Lives
'     Input #s_replayFD, Coins
'     Input #s_replayFD, Score
'     Input #s_replayFD, numStars
'     For A = 1 To numStars
'         Input #s_replayFD, S ' "Star"
'         Input #s_replayFD, Star(A).level
'         Input #s_replayFD, Star(A).Section
'     Next A
'     Input #s_replayFD, numPlayers
'     For A = 1 To numPlayers
'         Input #s_replayFD, S ' "Player"
'         Input #s_replayFD, Player(A).Character
'         Input #s_replayFD, Player(A).State
'         Input #s_replayFD, Player(A).MountType
'         Input #s_replayFD, Player(A).HeldBonus
'     Next A
'     ' stars must appear if and only if they originally did
'     For A = 1 To numNPCs
'         If NPC(A).Type = 97 Or NPC(A).Type = 196 Then
'             NPC(A).Special = 0
'             NPC(A).DefaultSpecial = 0
'             NPC(A).Killed = 0
'             For B = 1 To numStars
'                 If Star(B).level = FileName And (Star(B).Section = NPC(A).Section Or Star(B).Section = -1) Then
'                     NPC(A).Special = 1
'                     NPC(A).DefaultSpecial = 1
'                     If NPC(A).Type = 196 Then NPC(A).Killed = 9
'                     Exit For
'                 End If
'             Next B
'         End If
'     Next A
' End Sub

Private Sub record_write_control()
    For i = 1 To numPlayers
        Dim iStr As String
        iStr = Mid(Str(i), 2, 1) ' get the actual numerical character...
        If Not last_controls(i).Up And Player(i).Controls.Up Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C+" + iStr + "U"
        ElseIf last_controls(i).Up And Not Player(i).Controls.Up Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C-" + iStr + "U"
        End If
        If Not last_controls(i).Down And Player(i).Controls.Down Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C+" + iStr + "D"
        ElseIf last_controls(i).Down And Not Player(i).Controls.Down Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C-" + iStr + "D"
        End If
        If Not last_controls(i).Left And Player(i).Controls.Left Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C+" + iStr + "L"
        ElseIf last_controls(i).Left And Not Player(i).Controls.Left Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C-" + iStr + "L"
        End If
        If Not last_controls(i).Right And Player(i).Controls.Right Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C+" + iStr + "R"
        ElseIf last_controls(i).Right And Not Player(i).Controls.Right Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C-" + iStr + "R"
        End If
        If Not last_controls(i).Jump And Player(i).Controls.Jump Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C+" + iStr + "A"
        ElseIf last_controls(i).Jump And Not Player(i).Controls.Jump Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C-" + iStr + "A"
        End If
        If Not last_controls(i).Run And Player(i).Controls.Run Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C+" + iStr + "B"
        ElseIf last_controls(i).Run And Not Player(i).Controls.Run Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C-" + iStr + "B"
        End If
        If Not last_controls(i).AltJump And Player(i).Controls.AltJump Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C+" + iStr + "X"
        ElseIf last_controls(i).AltJump And Not Player(i).Controls.AltJump Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C-" + iStr + "X"
        End If
        If Not last_controls(i).AltRun And Player(i).Controls.AltRun Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C+" + iStr + "Y"
        ElseIf last_controls(i).AltRun And Not Player(i).Controls.AltRun Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C-" + iStr + "Y"
        End If
        If Not last_controls(i).Start And Player(i).Controls.Start Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C+" + iStr + "S"
        ElseIf last_controls(i).Start And Not Player(i).Controls.Start Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C-" + iStr + "S"
        End If
        If Not last_controls(i).Drop And Player(i).Controls.Drop Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C+" + iStr + "I"
        ElseIf last_controls(i).Drop And Not Player(i).Controls.Drop Then
            Print #s_recordFD, frame_no
            Print #s_recordFD, "C-" + iStr + "I"
        End If
        last_controls(i) = Player(i).Controls
    Next i
End Sub

Private Sub record_read_control(command)
    Dim is_on As Boolean
    Dim key As String
    Dim i As Integer

    If Mid(command, 2, 1) = "+" Then
        is_on = True
    Else
        is_on = False
    End If

    i = CInt(Mid(command, 3, 1))
    key = Mid(command, 4, 1)

    With last_controls(i)
        If key = "U" Then
            .Up = is_on
        ElseIf key = "D" Then
            .Down = is_on
        ElseIf key = "L" Then
            .Left = is_on
        ElseIf key = "R" Then
            .Right = is_on
        ElseIf key = "S" Then
            .Start = is_on
        ElseIf key = "I" Then
            .Drop = is_on
        ElseIf key = "A" Then
            .Jump = is_on
        ElseIf key = "B" Then
            .Run = is_on
        ElseIf key = "X" Then
            .AltJump = is_on
        ElseIf key = "Y" Then
            .AltRun = is_on
        End If
    End With

    If Not s_recordFD = 0 Then
        Print #s_recordFD, frame_no
        Print #s_recordFD, command
    End If
End Sub

Public Sub record_finish()
    in_level = False

    If Not s_recordFD = 0 Then
        ' when did the level end?
        Print #s_recordFD, frame_no + 1
        ' how did the level end?
        Print #s_recordFD, "End"
        Print #s_recordFD, "LevelBeatCode " + Str(LevelBeatCode)
        Close #s_recordFD
        s_recordFD = 0
    End If
End Sub

Public Sub record_write_status()
    Print #s_recordFD, frame_no
    Print #s_recordFD, "Status"
    Dim status_tick As Long
    status_tick = GetTickCount
    Print #s_recordFD, "Ticks " + Str(status_tick - last_status_tick)
    last_status_tick = status_tick
    Print #s_recordFD, "randCalls " + Str(random_ncalls)
    Print #s_recordFD, "Score " + Str(Score)
    Print #s_recordFD, "numNPCs " + Str(numNPCs)
    Dim numActiveNPCs As Integer
    numActiveNPCs = 0

    If frame_no <> 0 Then
        For i = 1 To numNPCs
            If NPC(i).Active Then
                numActiveNPCs = numActiveNPCs + 1
            End If
        Next i
    End If

    Print #s_recordFD, "numActiveNPCs " + Str(numActiveNPCs)
    Print #s_recordFD, "numRenderNPCs " + Str(g_recordNumRenderedNPCs)
    Print #s_recordFD, "numRenderBlocks " + Str(g_recordNumRenderedBlocks)
    Print #s_recordFD, "numRenderBGOs " + Str(g_recordNumRenderedBGOs)

    For i = 1 To numPlayers
        Print #s_recordFD, "p" + Str(i) + "x " + Str(Player(i).Location.X)
        Print #s_recordFD, "p" + Str(i) + "y " + Str(Player(i).Location.Y)
    Next i
End Sub

Public Sub record_write_NPCs()
    Print #s_recordFD, frame_no
    Print #s_recordFD, "NPCs"
    Print #s_recordFD, "numNPCs " + Str(numNPCs)

    For i = 1 To numNPCs
        Print #s_recordFD, "NPC " + Str(i)
        With NPC(i)
            Print #s_recordFD, "Type " + Str(.Type)
            If .Active Then
                Print #s_recordFD, "Active 1"
            Else
                Print #s_recordFD, "Active 0"
            End If
            Print #s_recordFD, "Dir " + Str(.Direction)
            Print #s_recordFD, "XYWH " + Str(.Location.X) + " " + Str(.Location.Y) + " " + Str(.Location.Width) + " " + Str(.Location.Height)
            Print #s_recordFD, "S " + Str(.Special) + " " + Str(.Special2) + " " + Str(.Special3) + " " + Str(.Special4) + " " + Str(.Special5) + " " + Str(.Special6)
        End With
    Next i
End Sub

Public Sub record_sync()
    If Not in_level Then
        Exit Sub
    End If

    If Not s_replayFD = 0 Then
        If next_record_frame = -1 Then
            If EOF(s_replayFD) Then
                Close #s_replayFD
                s_replayFD = 0
            Else
                Input #s_replayFD, next_record_frame
            End If
        End If

        Do Until Not next_record_frame = frame_no Or s_replayFD = 0
            Dim command As String

            If EOF(s_replayFD) Then
                Close #s_replayFD
                s_replayFD = 0
                Exit Do
            Else
                Input #s_replayFD, command
            End If

            If Mid(command, 1, 1) = "C" Then
                Call record_read_control(command)
            End If

            If EOF(s_replayFD) Then
                Close #s_replayFD
                s_replayFD = 0
                Exit Do
            Else
                Input #s_replayFD, next_record_frame
            End If
        Loop
        For i = 1 To numPlayers
            Player(i).Controls = last_controls(i)
        Next i
    End If
    If Not s_recordFD = 0 Then
        Call record_write_control
        If frame_no Mod 60 = 0 Then
            Call record_write_status
        End If
        If frame_no Mod 900 = 0 Then
            Call record_write_NPCs
        End If
    End If

    frame_no = frame_no + 1
End Sub
