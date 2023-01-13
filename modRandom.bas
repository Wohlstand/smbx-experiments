Attribute VB_Name = "modRandom"
Private use_cpp As Boolean
Private n_calls As Long

Private g_lastIntRandRes As Long
Private g_lastIntRandResMax As Integer
Private g_lastRoundIntRandRes As Long
Private g_lastRoundIntRandResMax As Integer
Private g_lastDubRandRes As Double

Private g_trackRandom As Boolean
Private s_randTrackerFD As Integer

Private Declare Sub cpprand_seed Lib "cpprand.dll" (ByVal seed As Long)
Private Declare Function cpprand_double Lib "cpprand.dll" () As Double
Private Declare Function cpprand_int32 Lib "cpprand.dll" (ByVal max As Long) As Long
Private Declare Function cpprand_int32_round Lib "cpprand.dll" (ByVal max As Long) As Long

Private Declare Sub cpprand_seed_sec Lib "cpprand.dll" (ByVal seed As Long)
Private Declare Function cpprand_double_sec Lib "cpprand.dll" () As Double
Private Declare Function cpprand_int32_sec Lib "cpprand.dll" (ByVal max As Long) As Long
Private Declare Function cpprand_int32_round_sec Lib "cpprand.dll" (ByVal max As Long) As Long

Public Sub random_init()
    Randomize Timer
    use_cpp = False
End Sub


Public Function random_seed(seed As Long)
    cpprand_seed (seed)
    cpprand_seed_sec (seed)
    use_cpp = True
    n_calls = 0
End Function

Public Sub start_rand_track(outFile As String)
    If s_randTrackerFD = 0 Then
        s_randTrackerFD = 42
        Open outFile For Output As #s_randTrackerFD
        If s_randTrackerFD = 0 Then
            MsgBox "Damn!"
        End If
    End If
    g_trackRandom = True
End Sub

Public Sub stop_rand_track()
    g_trackRandom = False
    Close #s_randTrackerFD
    s_randTrackerFD = 0
End Sub

Private Sub dump_rand_track_i()
    If g_trackRandom And s_randTrackerFD <> 0 Then
        Print #s_randTrackerFD, Trim(Str(GetRecordFrameNo)) + ": (" + Trim(Str(n_calls)) + ") I=" + Trim(Str(g_lastIntRandRes)) + " (max=" + Trim(Str(g_lastIntRandResMax)) + ")"
    End If
End Sub

Private Sub dump_rand_track_r()
    If g_trackRandom And s_randTrackerFD <> 0 Then
        Print #s_randTrackerFD, Trim(Str(GetRecordFrameNo)) + ": (" + Trim(Str(n_calls)) + ") R=" + Trim(Str(g_lastRoundIntRandRes)) + " (max=" + Trim(Str(g_lastRoundIntRandResMax)) + ")"
    End If
End Sub

Private Sub dump_rand_track_d()
    If g_trackRandom And s_randTrackerFD <> 0 Then
        Print #s_randTrackerFD, Trim(Str(GetRecordFrameNo)) + ": (" + Trim(Str(n_calls)) + ") D=" + Trim(Str(Int(g_lastDubRandRes * 1000000)))
    End If
End Sub

Public Function random_double() As Double
    If use_cpp = False Then
        random_double = Rnd()
    Else
        g_lastDubRandRes = cpprand_double()
        n_calls = n_calls + 1
        random_double = g_lastDubRandRes
        dump_rand_track_d
    End If
End Function

Public Function random_double_sec() As Double
    If use_cpp = False Then
        random_double_sec = Rnd()
    Else
        random_double_sec = random_double() ' cpprand_double_sec()
    End If
End Function

Public Function random_int(max As Integer) As Integer
    If use_cpp = False Then
        random_int = Int(Rnd() * max)
    Else
        g_lastIntRandResMax = max
        g_lastIntRandRes = cpprand_int32(max)
        n_calls = n_calls + 1
        random_int = g_lastIntRandRes
        dump_rand_track_i
    End If
End Function

Public Function random_int_sec(max As Integer) As Integer
    If use_cpp = False Then
        random_int_sec = Int(Rnd() * max)
    Else
        random_int_sec = random_int(max) ' cpprand_int32_sec(max)
    End If
End Function

Public Function random_int_round(max As Integer) As Integer
    If use_cpp = False Then
        random_int_round = Rnd() * max
    Else
        g_lastRoundIntRandResMax = max
        g_lastRoundIntRandRes = cpprand_int32_round(max)
        n_calls = n_calls + 1
        random_int_round = g_lastRoundIntRandRes
        dump_rand_track_r
    End If
End Function

Public Function random_int_round_sec(max As Integer) As Integer
    If use_cpp = False Then
        random_int_round_sec = Rnd() * max
    Else
        random_int_round_sec = random_int_round(max) ' cpprand_int32_round_sec(max)
    End If
End Function

Public Function random_ncalls() As Long
    random_ncalls = n_calls
End Function
