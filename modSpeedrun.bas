Attribute VB_Name = "modSpeedrun"
'this module handles "speedrun" functions, in particular the control recording and playback functions

Private last_controls As Controls
Private frame_no As Long
Private next_delta_frame As Long
Public g_speedRunnerControlFile As Integer
Public g_speedRunnerGameplayLog As Integer

Public Sub speedRun_syncControlKeys()
    If g_speedRunnerControlFile = 0 And g_speedRunnerGameplayLog = 0 Then
        Exit Sub
    End If

    If Not g_speedRunnerControlFile = 0 Then
        If next_delta_frame = -1 And Not g_speedRunnerControlFile = 0 Then
            If EOF(g_speedRunnerControlFile) Then
                Close #g_speedRunnerControlFile
                g_speedRunnerControlFile = 0
            Else
                Input #g_speedRunnerControlFile, next_delta_frame
            End If
        End If

        Do Until Not next_delta_frame = frame_no Or g_speedRunnerControlFile = 0
            Dim command As String
            Dim is_on As Boolean
            Dim key As String

            If EOF(g_speedRunnerControlFile) Then
                Close #g_speedRunnerControlFile
                g_speedRunnerControlFile = 0
                Exit Do
            Else
                Input #g_speedRunnerControlFile, command
            End If

            If Mid(command, 0, 1) = "-" Then
                is_on = True
            Else
                is_on = False
            End If

            key = Mid(command, 1, 1)
            If key = "U" Then
                last_controls.Up = is_on
            ElseIf key = "U" Then
                last_controls.Down = is_on
            ElseIf key = "L" Then
                last_controls.Left = is_on
            ElseIf key = "R" Then
                last_controls.Right = is_on
            ElseIf key = "S" Then
                last_controls.Start = is_on
            ElseIf key = "I" Then
                last_controls.Drop = is_on
            ElseIf key = "A" Then
                last_controls.Jump = is_on
            ElseIf key = "B" Then
                last_controls.Run = is_on
            ElseIf key = "X" Then
                last_controls.AltJump = is_on
            ElseIf key = "Y" Then
                last_controls.AltRun = is_on
            End If

            If EOF(g_speedRunnerControlFile) Then
                Close #g_speedRunnerControlFile
                g_speedRunnerControlFile = 0
                Exit Do
            Else
                Input #g_speedRunnerControlFile, next_delta_frame
            End If
        Loop
        Player(1).Controls = last_controls
    End If

    If Not g_speedRunnerGameplayLog = 0 And frame_no Mod 60 = 0 Then
        Print #g_speedRunnerGameplayLog, "Frame"
        Print #g_speedRunnerGameplayLog, frame_no
        Print #g_speedRunnerGameplayLog, Player(1).Location.X
        Print #g_speedRunnerGameplayLog, Player(1).Location.Y
        Print #g_speedRunnerGameplayLog, Score
    End If

    If Not g_speedRunnerGameplayLog = 0 And g_speedRunnerControlFile = 0 Then
        KillIt
    End If
End Sub
