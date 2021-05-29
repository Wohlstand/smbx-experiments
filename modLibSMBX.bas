Attribute VB_Name = "modLibSMBX"
Public Declare Sub SMBX_Init Lib "smbx.dll" Alias "_SMBX_Init@0" ()
Public Declare Sub SMBX_Quit Lib "smbx.dll" Alias "_SMBX_Quit@0" ()
Public Declare Sub SMBX_initPlayers Lib "smbx.dll" Alias "_SMBX_initPlayers@4" (ByVal playersCount As Integer)
Public Declare Sub SMBX_setPlayer Lib "smbx.dll" Alias "_SMBX_setPlayer@8" (ByRef Player As Player, ByVal playerId As Integer)
Public Declare Sub SMBX_printPlayerLocationIntoFile Lib "smbx.dll" Alias "_SMBX_printPlayerLocationIntoFile@4" (ByVal playerId As Integer)

Private Declare Function VarPtrArray Lib "msvbvm60.dll" Alias "VarPtr" (ByRef Ptr() As Any) As Long

Public Sub libSMBX_InitArrays()
    Dim A As Integer

    Call SMBX_initPlayers(maxPlayers)
    For A = 0 To maxPlayers
        Call SMBX_setPlayer(Player(A), A)
    Next A
End Sub
