Attribute VB_Name = "modRandom"
Private use_cpp As Boolean
Private n_calls As Long

Private Declare Sub cpprand_seed Lib "cpprand.dll" (ByVal seed As Long)
Private Declare Function cpprand_double Lib "cpprand.dll" () As Double
Private Declare Function cpprand_int32 Lib "cpprand.dll" (ByVal max As Long) As Long

Public Sub random_init()
    Randomize Timer
    use_cpp = False
End Sub


Public Function random_seed(seed As Long)
    cpprand_seed (seed)
    use_cpp = True
    n_calls = 0
End Function

Public Function random_double() As Double
    If use_cpp = False Then
        random_double = Rnd()
    Else
        random_double = cpprand_double()
        n_calls = n_calls + 1
    End If
End Function

Public Function random_int(max As Integer) As Integer
    If use_cpp = False Then
        random_int = Int(Rnd() * max)
    Else
        random_int = cpprand_int32(max)
        n_calls = n_calls + 1
    End If
End Function

Public Function random_int_round(max As Integer) As Integer
    If use_cpp = False Then
        random_int_round = Rnd() * max
    Else
        Dim i As Long
        i = cpprand_int32(max * 2)
        If i = 0 Then
            random_int_round = max
        Else
            random_int_round = i \ 2
        End If
        n_calls = n_calls + 1
    End If
End Function

Public Function random_ncalls() As Long
    random_ncalls = n_calls
End Function
