Attribute VB_Name = "modRandom"
Private use_cpp As Boolean
Private n_calls As Long

Private Declare Sub cpprand_seed Lib "cpprand.dll" (ByVal seed As Integer)
Private Declare Function cpprand_double Lib "cpprand.dll" () As Double


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
        'Print #7, "r", n_calls
        n_calls = n_calls + 1
    End If
End Function

Public Function random_int(max As Integer) As Integer
    random_int = Int(random_double() * max)
End Function

