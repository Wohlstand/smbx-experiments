Attribute VB_Name = "modFileDialogue"
Option Explicit

Private Declare Function GetOpenFileNameA Lib "comdlg32.dll" (ByRef unnamedParam1 As OPENFILENAMEA) As Long
Private Declare Function GetSaveFileNameA Lib "comdlg32.dll" (ByRef unnamedParam1 As OPENFILENAMEA) As Long
Private Declare Function CommDlgExtendedError Lib "comdlg32.dll" () As Long
Private Declare Sub CopyMemory Lib "kernel32.dll" Alias "RtlMoveMemory" (Destination As Any, Source As Any, ByVal Length As Long)
Private Declare Function lstrlenA Lib "kernel32.dll" (ByVal lpString As Long) As Long

Public Const OFN_ALLOWMULTISELECT As Long = &H200
Public Const OFN_CREATEPROMPT As Long = &H2000
Public Const OFN_DONTADDTORECENT As Long = &H2000000
Public Const OFN_ENABLEHOOK As Long = &H20
Public Const OFN_ENABLEINCLUDENOTIFY As Long = &H400000
Public Const OFN_ENABLESIZING As Long = &H800000
Public Const OFN_ENABLETEMPLATE As Long = &H40
Public Const OFN_ENABLETEMPLATEHANDLE As Long = &H80
Public Const OFN_EXPLORER As Long = &H80000
Public Const OFN_EXTENSIONDIFFERENT As Long = &H400
Public Const OFN_FILEMUSTEXIST As Long = &H1000
Public Const OFN_FORCESHOWHIDDEN As Long = &H10000000
Public Const OFN_HIDEREADONLY As Long = &H4
Public Const OFN_LONGNAMES As Long = &H200000
Public Const OFN_NOCHANGEDIR As Long = &H8
Public Const OFN_NODEREFERENCELINKS As Long = &H100000
Public Const OFN_NOLONGNAMES As Long = &H40000
Public Const OFN_NONETWORKBUTTON As Long = &H20000
Public Const OFN_NOREADONLYRETURN As Long = &H8000
Public Const OFN_NOTESTFILECREATE As Long = &H10000
Public Const OFN_NOVALIDATE As Long = &H100
Public Const OFN_OVERWRITEPROMPT As Long = &H2
Public Const OFN_PATHMUSTEXIST As Long = &H1000
Public Const OFN_READONLY As Long = &H1
Public Const OFN_SHAREAWARE As Long = &H4000
Public Const OFN_SHOWHELP As Long = &H10

Public Type OPENFILENAMEA
    lStructSize As Long
    hwndOwner As Long
    hInstance As Long
    lpstrFilter As Long ' StrPtr
    lpstrCustomFilter As Long 'StrPtr
    nMaxCustFilter As Long
    nFilterIndex As Long
    lpstrFile As Long 'StrPtr
    nMaxFile As Long 'StrPtr
    lpstrFileTitle As Long 'StrPtr
    nMaxFileTitle As Long
    lpstrInitialDir As Long 'StrPtr
    lpstrTitle As Long 'StrPtr
    Flags As Long
    nFileOffset As Integer
    nFileExtension As Integer
    lpstrDefExt As Long 'StrPtr
    lCustData As Long
    lpfnHook As Long
    lpTemplateName As Long 'StrPtr
    ' lpEditInfo As Long
    ' lpstrPrompt As Long 'StrPtr
    pvReserved As Long
    dwReserved As Long
    FlagsEx As Long
End Type

Public Function GetOpenFile(ByVal hWnd As Long, _
    ByVal initDir As String, ByVal FileName As String, _
    ByVal filter As String, ByVal filterIndex As Long, _
    ByVal title As String) As String

    Dim ofn As OPENFILENAMEA
    Dim outFile(0 To 256) As Byte
    Dim outFileLen As Long
    Dim outFileN() As Byte
    Dim outFileU As String
    Dim ret As Long

    GetOpenFile = ""
    outFile(0) = 0

    title = StrConv(title, vbFromUnicode)
    initDir = StrConv(initDir, vbFromUnicode)
    filter = StrConv(filter, vbFromUnicode)
    If FileName <> "" Then
        outFileN = StrConv(FileName, vbFromUnicode)
        Call CopyMemory(outFile(0), outFileN(0), Len(FileName))
    End If

    ofn.lStructSize = LenB(ofn)
    ofn.hwndOwner = hWnd
    ofn.hInstance = App.hInstance
    ofn.lpstrFilter = StrPtr(filter)
    ofn.nFilterIndex = filterIndex
    ofn.lpstrFile = VarPtr(outFile(0))
    ofn.nMaxFile = 260
    ofn.lpstrFileTitle = 0
    ofn.nMaxFileTitle = 0
    ofn.lpstrTitle = StrPtr(title)
    ofn.lpstrInitialDir = StrPtr(initDir)
    ofn.Flags = OFN_PATHMUSTEXIST Or OFN_FILEMUSTEXIST Or OFN_READONLY Or OFN_HIDEREADONLY Or OFN_EXPLORER
    ofn.nFileOffset = 0
    ofn.nFileExtension = 0
    ofn.FlagsEx = 0

    ret = GetOpenFileNameA(ofn)

    If ret = 1 Then
        outFileLen = lstrlenA(VarPtr(outFile(0)))
        ReDim outFileN(0 To outFileLen - 1) As Byte
        Call CopyMemory(outFileN(0), outFile(0), outFileLen)
        outFileU = StrConv(outFileN, vbUnicode)
        GetOpenFile = outFileU
    End If

End Function

Public Function GetSaveFile(ByVal hWnd As Long, _
    ByVal initDir As String, ByVal FileName As String, _
    ByVal filter As String, ByVal filterIndex As Long, _
    ByVal title As String) As String

    Dim ofn As OPENFILENAMEA
    Dim outFileLen As Long
    Dim outFile(0 To 256) As Byte
    Dim outFileN() As Byte
    Dim outFileU As String
    Dim ret As Long

    GetSaveFile = ""

    title = StrConv(title, vbFromUnicode)
    initDir = StrConv(initDir, vbFromUnicode)
    filter = StrConv(filter, vbFromUnicode)
    If FileName <> "" Then
        outFileN = StrConv(FileName, vbFromUnicode)
        Call CopyMemory(outFile(0), outFileN(0), Len(FileName))
    End If

    ofn.lStructSize = LenB(ofn)
    ofn.hwndOwner = hWnd
    ofn.hInstance = App.hInstance
    ofn.lpstrFilter = StrPtr(filter)
    ofn.nFilterIndex = filterIndex
    ofn.lpstrFile = VarPtr(outFile(0))
    ofn.nMaxFile = 260
    ofn.lpstrFileTitle = 0
    ofn.nMaxFileTitle = 0
    ofn.lpstrTitle = StrPtr(title)
    ofn.lpstrInitialDir = StrPtr(initDir)
    ofn.Flags = OFN_EXPLORER
    ofn.nFileOffset = 0
    ofn.nFileExtension = 0
    ofn.FlagsEx = 0

    ret = GetSaveFileNameA(ofn)

    If ret = 1 Then
        outFileLen = lstrlenA(VarPtr(outFile(0)))
        ReDim outFileN(0 To outFileLen - 1) As Byte
        Call CopyMemory(outFileN(0), outFile(0), outFileLen)
        outFileU = StrConv(outFileN, vbUnicode)
        GetSaveFile = outFileU
    End If
End Function

