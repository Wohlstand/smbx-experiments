VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "ComDlg32.OCX"
Begin VB.MDIForm frmLevelEditor 
   AutoShowChildren=   0   'False
   BackColor       =   &H8000000C&
   Caption         =   "<title defined at Load() call>"
   ClientHeight    =   11700
   ClientLeft      =   120
   ClientTop       =   750
   ClientWidth     =   14025
   Icon            =   "frmLevelEditor.frx":0000
   LinkTopic       =   "MDIForm1"
   OLEDropMode     =   1  'Manual
   StartUpPosition =   2  'CenterScreen
   WindowState     =   2  'Maximized
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   120
      Top             =   120
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.PictureBox picWorld 
      Align           =   2  'Align Bottom
      BorderStyle     =   0  'None
      Height          =   500
      Left            =   0
      ScaleHeight     =   495
      ScaleWidth      =   14025
      TabIndex        =   7
      Top             =   10710
      Visible         =   0   'False
      Width           =   14025
      Begin VB.CheckBox chkAlign2 
         Caption         =   "Auto-Align"
         Height          =   375
         Left            =   12600
         TabIndex        =   18
         Top             =   80
         Value           =   1  'Checked
         Width           =   1095
      End
      Begin VB.OptionButton optCursor 
         Caption         =   "Selection"
         Height          =   375
         Index           =   14
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   16
         Top             =   60
         Value           =   -1  'True
         Width           =   1500
      End
      Begin VB.OptionButton optCursor 
         Caption         =   "Settings"
         Height          =   375
         Index           =   12
         Left            =   11040
         Style           =   1  'Graphical
         TabIndex        =   14
         Top             =   60
         Width           =   1500
      End
      Begin VB.OptionButton optCursor 
         Caption         =   "Music"
         Height          =   375
         Index           =   11
         Left            =   9480
         Style           =   1  'Graphical
         TabIndex        =   13
         Top             =   60
         Width           =   1500
      End
      Begin VB.OptionButton optCursor 
         Caption         =   "Paths"
         Height          =   375
         Index           =   10
         Left            =   7920
         Style           =   1  'Graphical
         TabIndex        =   12
         Top             =   60
         Width           =   1500
      End
      Begin VB.OptionButton optCursor 
         Caption         =   "Levels"
         Height          =   375
         Index           =   9
         Left            =   6360
         Style           =   1  'Graphical
         TabIndex        =   11
         Top             =   60
         Width           =   1500
      End
      Begin VB.OptionButton optCursor 
         Caption         =   "Scenery"
         Height          =   375
         Index           =   8
         Left            =   4800
         Style           =   1  'Graphical
         TabIndex        =   10
         Top             =   60
         Width           =   1500
      End
      Begin VB.OptionButton optCursor 
         Caption         =   "Tiles"
         Height          =   375
         Index           =   7
         Left            =   3240
         Style           =   1  'Graphical
         TabIndex        =   9
         Top             =   60
         Width           =   1500
      End
      Begin VB.OptionButton optCursor 
         Caption         =   "Eraser"
         Height          =   375
         Index           =   6
         Left            =   1680
         Style           =   1  'Graphical
         TabIndex        =   8
         Top             =   60
         Width           =   1500
      End
   End
   Begin VB.PictureBox picLevel 
      Align           =   2  'Align Bottom
      BorderStyle     =   0  'None
      Height          =   500
      Left            =   0
      ScaleHeight     =   495
      ScaleWidth      =   14025
      TabIndex        =   0
      Top             =   11205
      Width           =   14025
      Begin VB.OptionButton optCursor 
         Caption         =   "Water"
         Height          =   375
         Index           =   15
         Left            =   11040
         Style           =   1  'Graphical
         TabIndex        =   19
         Top             =   60
         Width           =   1500
      End
      Begin VB.CheckBox chkAlign 
         Caption         =   "Auto-Align"
         Height          =   375
         Left            =   12600
         TabIndex        =   17
         Top             =   80
         Value           =   1  'Checked
         Width           =   1095
      End
      Begin VB.OptionButton optCursor 
         Caption         =   "Selection"
         Height          =   375
         Index           =   13
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   15
         Top             =   60
         Value           =   -1  'True
         Width           =   1500
      End
      Begin VB.OptionButton optCursor 
         Caption         =   "Warps and Doors"
         Height          =   375
         Index           =   5
         Left            =   9480
         Style           =   1  'Graphical
         TabIndex        =   6
         Top             =   60
         Width           =   1500
      End
      Begin VB.OptionButton optCursor 
         Caption         =   "NPCs"
         Height          =   375
         Index           =   4
         Left            =   6360
         Style           =   1  'Graphical
         TabIndex        =   5
         Top             =   60
         Width           =   1500
      End
      Begin VB.OptionButton optCursor 
         Caption         =   "Backgrounds"
         Height          =   375
         Index           =   3
         Left            =   4800
         Style           =   1  'Graphical
         TabIndex        =   4
         Top             =   60
         Width           =   1500
      End
      Begin VB.OptionButton optCursor 
         Caption         =   "Level Settings"
         Height          =   375
         Index           =   2
         Left            =   7920
         Style           =   1  'Graphical
         TabIndex        =   3
         Top             =   60
         Width           =   1500
      End
      Begin VB.OptionButton optCursor 
         Caption         =   "Eraser"
         Height          =   375
         Index           =   0
         Left            =   1680
         Style           =   1  'Graphical
         TabIndex        =   2
         Top             =   60
         Width           =   1500
      End
      Begin VB.OptionButton optCursor 
         Caption         =   "Blocks and Tiles"
         Height          =   375
         Index           =   1
         Left            =   3240
         Style           =   1  'Graphical
         TabIndex        =   1
         Top             =   60
         Width           =   1500
      End
   End
   Begin VB.Menu menuFile 
      Caption         =   "&File"
      Begin VB.Menu menuFileNew 
         Caption         =   "&New"
         Shortcut        =   ^N
      End
      Begin VB.Menu mnuFile1 
         Caption         =   "-"
      End
      Begin VB.Menu menuFileSave 
         Caption         =   "&Save"
         Shortcut        =   ^S
      End
      Begin VB.Menu menuFileSaveAs 
         Caption         =   "Sav&e As..."
         Shortcut        =   +{F3}
      End
      Begin VB.Menu menufileOpen 
         Caption         =   "&Open"
         Shortcut        =   ^O
      End
      Begin VB.Menu mnuFil3 
         Caption         =   "-"
      End
      Begin VB.Menu menuCurDirOpen 
         Caption         =   "Open episode directory..."
      End
      Begin VB.Menu menuCurLevelDataOpen 
         Caption         =   "Open level data directory..."
      End
      Begin VB.Menu mnuFile2 
         Caption         =   "-"
      End
      Begin VB.Menu menuFileExit 
         Caption         =   "E&xit"
         Shortcut        =   ^Q
      End
   End
   Begin VB.Menu mnuMode 
      Caption         =   "&Mode"
      Begin VB.Menu mnuLevelEditor 
         Caption         =   "&Level Editor"
         Shortcut        =   ^V
      End
      Begin VB.Menu mnuWorldEditor 
         Caption         =   "&World Editor"
         Shortcut        =   ^W
      End
   End
   Begin VB.Menu MenuTest 
      Caption         =   "&Test Level"
      Begin VB.Menu MenuTestLevel 
         Caption         =   "Test Level &1up"
         Shortcut        =   {F5}
      End
      Begin VB.Menu MenuTestLevel2 
         Caption         =   "Test Level &2up"
         Shortcut        =   {F6}
      End
      Begin VB.Menu MenuTestLevelBattle 
         Caption         =   "Test Level &Battle Game"
         Shortcut        =   {F7}
      End
      Begin VB.Menu mnuTestLevel 
         Caption         =   "-"
      End
      Begin VB.Menu mnuTestSetting 
         Caption         =   "&Test Settings"
         Shortcut        =   ^T
      End
      Begin VB.Menu menureset 
         Caption         =   "&Reset Game"
         Shortcut        =   {F8}
      End
      Begin VB.Menu mnuTestLevel2 
         Caption         =   "-"
      End
      Begin VB.Menu menuRecordGameplay 
         Caption         =   "Record gameplay data"
         Checked         =   -1  'True
      End
      Begin VB.Menu menuCompatMode 
         Caption         =   "Strict compatibility mode"
         Checked         =   -1  'True
      End
      Begin VB.Menu menuNoFrameSkip 
         Caption         =   "Disable frame skip"
      End
      Begin VB.Menu menuEnableGameplayTimer 
         Caption         =   "Enable gameplay timer"
      End
      Begin VB.Menu menuUseOldKeyHoleTime 
         Caption         =   "Use old keyhole time"
      End
      Begin VB.Menu MenuTestLeveld 
         Caption         =   "-"
         Visible         =   0   'False
      End
      Begin VB.Menu menuTestLevelTwo 
         Caption         =   "Super Mario 2 (Single Player Co-op)"
         Visible         =   0   'False
      End
      Begin VB.Menu menuTestLevel4 
         Caption         =   "Super Mario 4"
         Visible         =   0   'False
      End
      Begin VB.Menu menuTestLevel8 
         Caption         =   "Super Mario 8"
         Visible         =   0   'False
      End
      Begin VB.Menu menuTestLevel16 
         Caption         =   "Super Mario 16"
         Visible         =   0   'False
      End
      Begin VB.Menu menuTestLevel32 
         Caption         =   "Super Mario 32"
         Visible         =   0   'False
      End
      Begin VB.Menu menuTestLevel64 
         Caption         =   "Super Mario 64"
         Visible         =   0   'False
      End
      Begin VB.Menu menuTestLevel128 
         Caption         =   "Super Mario 128"
         Visible         =   0   'False
      End
   End
   Begin VB.Menu menuView 
      Caption         =   "&View"
      Begin VB.Menu menuLayers 
         Caption         =   "&Layers"
         Shortcut        =   ^L
      End
      Begin VB.Menu menuEvents 
         Caption         =   "&Events"
         Shortcut        =   ^E
      End
      Begin VB.Menu menuloasdl 
         Caption         =   "-"
      End
      Begin VB.Menu menuLevelDebugger 
         Caption         =   "&Debugger"
         Shortcut        =   ^D
      End
   End
   Begin VB.Menu mnuOnline 
      Caption         =   "&Online"
      Enabled         =   0   'False
      Begin VB.Menu mnuChat 
         Caption         =   "&Chat Window"
         Shortcut        =   ^K
      End
      Begin VB.Menu menuonlineline 
         Caption         =   "-"
      End
      Begin VB.Menu menuNetplay 
         Caption         =   "&Start"
         Shortcut        =   ^I
      End
   End
   Begin VB.Menu menuhelp 
      Caption         =   "&Help"
      Begin VB.Menu menHelp 
         Caption         =   "&Editor Help"
         Shortcut        =   {F1}
      End
      Begin VB.Menu menuGameplay 
         Caption         =   "&Gameplay Manual"
         Shortcut        =   {F2}
      End
      Begin VB.Menu menuhelpline 
         Caption         =   "-"
      End
      Begin VB.Menu menChangeLog 
         Caption         =   "&Changelog"
      End
      Begin VB.Menu menGetSrc 
         Caption         =   "Get &source code"
      End
      Begin VB.Menu menAbout 
         Caption         =   "&About..."
      End
   End
End
Attribute VB_Name = "frmLevelEditor"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub chkAlign_Click()
    chkAlign2.Value = chkAlign.Value
End Sub

Private Sub chkAlign2_Click()
    chkAlign.Value = chkAlign2.Value
End Sub

Private Sub MDIForm_Load()
    On Error Resume Next
    Dim A As Integer

    Me.Caption = "Super Mario Bros. X - Level Editor - Version " & App.Major & "." & App.Minor & "." & App.Revision & " [Wohlstand's Edition] - https://wohlsoft.ru/"

    testPlayer(1).Character = 1
    testPlayer(2).Character = 2
    Load frmLevelWindow
    frmLevelWindow.Left = 0
    frmLevelWindow.Top = 0
    Load frmBlocks
    frmBlocks.Hide
    frmBlocks.Top = frmLevelWindow.Height
    frmBlocks.Left = 0
    Load frmLevelSettings
    frmLevelSettings.Hide
    frmLevelSettings.Top = frmLevelWindow.Height
    frmLevelSettings.Left = 0
    Load frmBackgrounds
    frmBackgrounds.Hide
    frmBackgrounds.Top = frmLevelWindow.Height
    frmBackgrounds.Left = 0
    Load frmChat
    frmChat.Hide
    frmChat.Top = 0
    frmChat.Left = frmLevelWindow.Width
    Load frmNetStatus
    frmNetStatus.Hide
    frmNetStatus.Top = frmChat.Height
    frmNetStatus.Left = frmLevelWindow.Width
    Load frmNetplay
    frmNetplay.Hide
    frmNetplay.Top = frmChat.Height
    frmNetplay.Left = frmLevelWindow.Width
    Load frmWarp
    frmWarp.Hide
    frmWarp.Top = frmLevelWindow.Height
    frmWarp.Left = 0
    Load frmNPCs
    frmNPCs.Hide
    frmNPCs.Top = frmLevelWindow.Height
    frmNPCs.Left = 0
    Load frmTiles
    frmTiles.Hide
    frmTiles.Top = frmLevelWindow.Height
    frmTiles.Left = 0
    Load frmScene
    frmScene.Hide
    frmScene.Top = frmLevelWindow.Height
    frmScene.Left = 0
    Load frmLevels
    frmLevels.Hide
    frmLevels.Top = frmLevelWindow.Height
    frmLevels.Left = 0
    Load frmPaths
    frmPaths.Hide
    frmPaths.Top = frmLevelWindow.Height
    frmPaths.Left = 0
    Load frmMusic
    frmMusic.Hide
    frmMusic.Top = frmLevelWindow.Height
    frmMusic.Left = 0
    frmWorld.Hide
    frmWorld.Top = frmLevelWindow.Height
    frmWorld.Left = 0
    Load frmWater
    frmWater.Hide
    frmWater.Top = frmLevelWindow.Height
    frmWater.Left = 0
    
    Load frmNPCAdvanced
    frmNPCAdvanced.Hide
    frmNPCAdvanced.Top = frmNPCs.Top - frmNPCAdvanced.Height
    frmNPCAdvanced.Left = frmLevelWindow.Width
    
    Load frmGenerator
    frmGenerator.Hide
    frmGenerator.Top = frmNPCAdvanced.Top - frmGenerator.Height
    frmGenerator.Left = frmLevelWindow.Width
    
    Load frmAdvanced
    frmAdvanced.Hide
    frmAdvanced.Top = frmGenerator.Top - frmAdvanced.Height
    frmAdvanced.Left = frmLevelWindow.Width

    frmAdvancedBlock.Hide
    frmAdvancedBlock.Top = 0
    frmAdvancedBlock.Left = frmLevelWindow.Width
    Load frmLayers
    frmLayers.Hide
    frmLayers.Top = 0
    frmLayers.Left = frmLevelWindow.Width
    Load frmEvents
    frmEvents.Hide
    frmEvents.Top = 0
    frmEvents.Left = frmLevelWindow.Width
    frmEvents.RefreshEvents

    menuTestSync

    For A = 0 To maxSections
        bgColor(A) = GFX.BackgroundColor(1).BackColor
    Next A
    numWarps = 1
    ClearLevel
    EditorCursor.Mode = 13
    optCursor(13).Value = True
    DoEvents
    frmLevelEditor.Show
    frmLevelWindow.Show
    frmLevelWindow.SetFocus
    frmSplash.Hide
    PlaySound 28
End Sub

Private Sub MDIForm_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    KillIt
End Sub

Private Sub MDIForm_Unload(Cancel As Integer)
    KillIt
End Sub

Private Sub menAbout_Click()
    frmAbout.Show vbModal, Me
End Sub

Private Sub menChangeLog_Click()
    On Error GoTo Bugs
    Shell "notepad " & Chr(34) & App.Path & "\changelog.txt" & Chr(34), vbNormalFocus
    Exit Sub
Bugs:
    MsgBox "The help file 'changelog.txt' is missing from the working directory.", vbOKOnly, "Super Mario Bros. X - Error"
End Sub

Private Sub menGetSrc_Click()
    ShellExecuteA Me.hWnd, "open", urlSourceCode, "", "", 4
End Sub

Private Sub menHelp_Click()
    On Error GoTo Bugs
    Shell "write " & Chr(34) & App.Path & "\SMBx Editor Help.rtf" & Chr(34), vbNormalFocus
    Exit Sub
Bugs:
    MsgBox "The help file 'SMBx Editor Help.rtf' is missing from the working directory.", vbOKOnly, "Super Mario Bros. X - Error"
End Sub

Private Sub menuCurDirOpen_Click()
    If FileNamePath <> "" Then
        OpenDirectoryX Me.hWnd, Left(FileNamePath, Len(FileNamePath) - 1)
    End If
End Sub

Private Sub menuCurLevelDataOpen_Click()
    Dim sLevelDataDir As String
    sLevelDataDir = FileNamePath & Left(FileName, Len(FileName) - 4)
    If FileNamePath <> "" Then
        If Dir(sLevelDataDir, vbDirectory) = "" Then
            MkDir sLevelDataDir
        End If
        OpenDirectoryX Me.hWnd, sLevelDataDir
    End If
End Sub

Private Sub menuGameplay_Click()
    On Error GoTo Bugs
    Shell "write " & Chr(34) & App.Path & "\SMBx Gameplay Manual.rtf" & Chr(34), vbNormalFocus
    Exit Sub
Bugs:
    MsgBox "The help file 'SMBx Gameplay Manual.rtf' is missing from the working directory.", vbOKOnly, "Super Mario Bros. X - Error"
End Sub

Private Sub menuLevelDebugger_Click()
    frmLevelDebugger.Show
End Sub

Private Sub menuEvents_Click()
    On Error Resume Next
    frmEvents.Show
    DoEvents
    frmEvents.SetFocus
End Sub

Private Sub menuFileExit_Click()
    KillIt
End Sub

Private Sub menuFileNew_Click()
    If WorldEditor = True Then
        ClearWorld
    Else
        ClearLevel
    End If
    FileName = ""
    FullFileName = ""
    PlaySound 36
    If nPlay.Online = True Then
        Netplay.sendData "j" & LB & "p36"
        If nPlay.Mode = 1 Then
            Netplay.sendData "d" & LocalNick & " cleared the level." & LB
            frmChat.txtChat = frmChat.txtChat & LocalNick & " cleared the level." & LB
            frmChat.txtChat.SelStart = Len(frmChat.txtChat.Text)
        End If
    End If
End Sub

Private Sub menuFileOpen_Click()
    'frmOpen.Show vbModal, Me
    CommonDialog1.FileName = ""
    
    If FileNamePath = "" Then
        CommonDialog1.InitDir = App.Path & "\worlds"
    Else
        CommonDialog1.InitDir = FileNamePath
    End If

    If WorldEditor Then
        CommonDialog1.Filter = "SMBX64 World files (*.wld)|*.wld|All files (*.*)|*.*"
        CommonDialog1.DefaultExt = "wld"
        CommonDialog1.DialogTitle = "Open the SMBX64 World file"
        CommonDialog1.ShowOpen

        If CommonDialog1.FileName <> "" Then
            frmLevelEditor.optCursor(14).Value = True
            OpenWorld CommonDialog1.FileName
        End If

    Else
        CommonDialog1.Filter = "SMBX64 Level files (*.lvl)|*.lvl|All files (*.*)|*.*"
        CommonDialog1.DefaultExt = "lvl"
        CommonDialog1.DialogTitle = "Open the SMBX64 Level file"
        CommonDialog1.ShowOpen

        If CommonDialog1.FileName <> "" Then
            frmLevelEditor.optCursor(13).Value = True
            ClearLevel
            OpenLevel CommonDialog1.FileName
        End If
    End If

    ' Hack: keep the app path being a working directory
    ChDir App.Path
End Sub

Private Sub DoSaveFile(FilePath As String)
    Dim tempName As String

    ChDir App.Path

    If WorldEditor = True Then
        If Right(FilePath, 4) = ".wld" Then
            tempName = FilePath
        Else
            tempName = FilePath & ".wld"
        End If
        SaveWorld tempName
    Else
        If Right(FilePath, 4) = ".lvl" Then
            tempName = FilePath
        Else
            tempName = FilePath & ".lvl"
        End If
        SaveLevel tempName
    End If
End Sub

Private Sub menuFileSave_Click()
    If FullFileName = "" Then
        menuFileSaveAs_Click
    Else
        DoSaveFile FullFileName
    End If
End Sub

Private Sub menuFileSaveAs_Click()
    ' frmSave.Show vbModal, Me
    CommonDialog1.FileName = ""

    If FileNamePath = "" Then
        CommonDialog1.InitDir = App.Path & "\worlds"
    Else
        CommonDialog1.InitDir = FileNamePath
    End If

    If WorldEditor Then
        CommonDialog1.Filter = "SMBX64 World files (*.wld)|*.wld"
        CommonDialog1.DefaultExt = "wld"
        CommonDialog1.DialogTitle = "Save the SMBX64 World file"
        CommonDialog1.ShowSave

        If CommonDialog1.FileName <> "" Then
            DoSaveFile CommonDialog1.FileName
        End If
    Else
        CommonDialog1.Filter = "SMBX64 Level files (*.lvl)|*.lvl"
        CommonDialog1.DefaultExt = "lvl"
        CommonDialog1.DialogTitle = "Save the SMBX64 Level file"
        CommonDialog1.ShowSave

        If CommonDialog1.FileName <> "" Then
            DoSaveFile CommonDialog1.FileName
        End If
    End If

    ' Hack: keep the app path being a working directory
    ChDir App.Path
End Sub

Private Sub menuLayers_Click()
    On Error Resume Next
    frmLayers.Show
    DoEvents
    frmLayers.SetFocus
End Sub

Private Sub menuNetplay_Click()
    If OnlineDisc = False Then
        If MsgBox("Online functionality is still in the beta stage and is currently unsupported.  The developers of Super Mario Bros. X take no responsibility for any problems encountered while playing this game online.", vbOKCancel, "Disclaimer") = 1 Then
            frmNetplay.Show
            OnlineDisc = True
        End If
    Else
        frmNetplay.Show
    End If
End Sub

Private Sub menureset_Click()
    PlaySound 22
    numStars = 0
    Checkpoint = ""
    numSavedEvents = 0
    BlockSwitch(1) = False
    BlockSwitch(2) = False
    BlockSwitch(3) = False
    BlockSwitch(4) = False
End Sub

Private Sub menuTestSync()
    menuRecordGameplay.Checked = g_recordEnabled
    menuCompatMode.Checked = g_compatMode
    menuNoFrameSkip.Checked = Not FrameSkip
    menuEnableGameplayTimer.Checked = g_enableGameplayTimer
    menuUseOldKeyHoleTime.Checked = g_oldKeyholeTimer

    menuCompatMode.Enabled = Not g_recordEnabled
    If g_recordEnabled Then
        menuCompatMode.Checked = True
        g_compatMode = True
    End If

    menuNoFrameSkip.Enabled = Not g_recordEnabled
    If g_recordEnabled Then
        menuNoFrameSkip.Checked = True
        FrameSkip = False
    End If
End Sub

Private Sub menuRecordGameplay_Click()
    menuRecordGameplay.Checked = Not menuRecordGameplay.Checked
    g_recordEnabled = menuRecordGameplay.Checked
    menuTestSync
End Sub

Private Sub menuCompatMode_Click()
    menuCompatMode.Checked = Not menuCompatMode.Checked
    g_compatMode = menuCompatMode.Checked
End Sub

Private Sub menuNoFrameSkip_Click()
    menuNoFrameSkip.Checked = Not menuNoFrameSkip.Checked
    FrameSkip = Not menuNoFrameSkip.Checked
End Sub

Private Sub menuEnableGameplayTimer_Click()
    menuEnableGameplayTimer.Checked = Not menuEnableGameplayTimer.Checked
    g_enableGameplayTimer = menuEnableGameplayTimer.Checked
End Sub

Private Sub menuUseOldKeyHoleTime_Click()
    menuUseOldKeyHoleTime.Checked = Not menuUseOldKeyHoleTime.Checked
    g_oldKeyholeTimer = menuUseOldKeyHoleTime.Checked
End Sub

Private Sub menuStatus_Click()
    frmNetStatus.Show
End Sub

Private Sub menuTestLevel_Click()
    BattleMode = False
    BattleIntro = 0
    BattleOutro = 0
    If nPlay.Online = True Then
        Netplay.sendData "H1" & LB
        zTestLevel
    Else
        numPlayers = 1
        zTestLevel
    End If
End Sub

Private Sub menuTestLevel128_Click()
    numPlayers = 128
    zTestLevel
End Sub

Private Sub menuTestLevel16_Click()
    numPlayers = 16
    zTestLevel
End Sub

Private Sub menuTestLevel2_Click()
    BattleMode = False
    BattleIntro = 0
    BattleOutro = 0
    If nPlay.Online = True Then
        Netplay.sendData "H1" & LB
        zTestLevel
    Else
        numPlayers = 2
        zTestLevel
    End If
End Sub

Private Sub menuTestLevel32_Click()
    numPlayers = 32
    zTestLevel
End Sub

Private Sub menuTestLevel4_Click()
    numPlayers = 4
    zTestLevel
End Sub

Private Sub menuTestLevel64_Click()
    numPlayers = 64
    zTestLevel
End Sub

Private Sub menuTestLevel8_Click()
    numPlayers = 8
    zTestLevel
End Sub

Private Sub MenuTestLevelBattle_Click()
    numPlayers = 2
    BattleMode = True
    BattleIntro = 150
    zTestLevel
End Sub

Private Sub menuTestLevelTwo_Click()
    numPlayers = 2
    SingleCoop = 1
    zTestLevel
End Sub


Private Sub mnuChat_Click()
    frmChat.Show
End Sub

Public Sub toggleLevelEdit()
    menuLevelDebugger.Visible = True
    WorldEditor = False
    For A = 0 To optCursor.Count - 1
        optCursor(A).Value = False
    Next A
    optCursor(13).Value = True
    EditorCursor.Mode = 13
    FileName = ""
    FullFileName = ""
    vScreenX(1) = 0
    vScreenY(1) = 0
    menuView.Enabled = True
    MenuTestLevel.Enabled = True
    ClearWorld
    ClearLevel
    mnuOnline.Enabled = True
    frmChat.Hide
    frmNetplay.Hide
End Sub

Public Sub toggleWorldEdit()
    mnuOnline.Enabled = False
    Unload frmLevelDebugger
    menuLevelDebugger.Visible = False
    WorldEditor = True
    ClearLevel
    ClearWorld
    For A = 0 To optCursor.Count - 1
        optCursor(A).Value = False
    Next A
    optCursor(14).Value = True
    EditorCursor.Mode = 14
    FileName = ""
    FullFileName = ""
    vScreenX(1) = 0
    vScreenY(1) = 0
    ClearLevel
    ClearWorld
    frmEvents.Hide
    frmLayers.Hide
    frmTestSettings.Hide
    frmChat.Hide
    frmNetplay.Hide
End Sub

Private Sub mnuLevelEditor_Click()
    Dim A As Integer

    A = MsgBox("All unsaved changes will be lost, proceed?", vbYesNo, "Proceed?")
    If A = 6 Then
        toggleLevelEdit
        PlaySound 32
    End If
End Sub

Private Sub mnuTestSetting_Click()
    frmTestSettings.Show
End Sub

Private Sub mnuWorldEditor_Click()
    Dim A As Integer
    A = MsgBox("All unsaved changes will be lost, proceed?", vbYesNo, "Proceed?")
    If A = 6 Then
        toggleWorldEdit
        PlaySound 32
    End If
End Sub

Private Sub optCursor_Click(Index As Integer)
    On Error Resume Next
    Dim A As Integer
    EditorCursor.Selected = 0
    EditorCursor.SelectedMode = 0
    For A = 1 To optCursor.Count
        If Index <> A Then optCursor(A).Value = False
    Next A
    If Index = 1 Then
        frmBlocks.Show
        frmBlocks.FocusNinja.SetFocus
    Else
        frmBlocks.Hide
        frmAdvancedBlock.Hide
    End If
    If Index = 2 Then
        frmLevelSettings.optLevel(0).Value = False
        frmLevelSettings.optLevel(1).Value = False
        frmLevelSettings.optLevel(2).Value = False
        frmLevelSettings.optLevel(3).Value = False
        frmLevelSettings.optLevel(4).Value = False
        frmLevelSettings.optLevel(5).Value = False
        frmLevelSettings.Show
        frmLevelSettings.FocusNinja.SetFocus
    Else
        frmLevelSettings.Hide
    End If
    If Index = 3 Then
        frmBackgrounds.Show
        frmBackgrounds.FocusNinja.SetFocus
    Else
        frmBackgrounds.Hide
    End If
    If Index = 4 Then
        frmNPCs.Show
        frmNPCs.FocusNinja.SetFocus
    Else
        frmNPCs.Hide
        Unload frmGenerator
        Unload frmAdvanced
        Unload frmNPCAdvanced
    End If
    If Index = 5 Then
        frmWarp.Show
        frmWarp.FocusNinja.SetFocus
    Else
        frmWarp.Hide
    End If
    If Index = 7 Then
        frmTiles.Show
        frmTiles.FocusNinja.SetFocus
    Else
        frmTiles.Hide
    End If
    If Index = 8 Then
        frmScene.Show
        frmScene.FocusNinja.SetFocus
    Else
        frmScene.Hide
    End If
    If Index = 9 Then
        frmLevels.Show
        frmLevels.FocusNinja.SetFocus
    Else
        frmLevels.Hide
    End If
    If Index = 10 Then
        frmPaths.Show
        frmPaths.FocusNinja.SetFocus
    Else
        frmPaths.Hide
    End If
    If Index = 11 Then
        frmMusic.Show
        frmMusic.FocusNinja.SetFocus
    Else
        frmMusic.Hide
    End If
    If Index = 12 Then
        frmWorld.Show
        frmWorld.FocusNinja.SetFocus
    Else
        frmWorld.Hide
    End If
    If Index = 15 Then
        frmWater.Show
        'frmWater.FocusNinja.SetFocus
    Else
        frmWater.Hide
    End If
    If Index = 0 Or Index = 6 Or Index = 13 Or Index = 14 Then
        frmLevelWindow.SetFocus
    End If
End Sub

Private Sub MDIForm_OLEDragDrop(Data As DataObject, Effect As Long, Button As Integer, Shift As Integer, X As Single, Y As Single)
    Dim intFile As Integer
    Dim fileToOpen As String

    With Data
        For intFile = 1 To .Files.Count
            fileToOpen = Data.Files.item(intFile)
        Next intFile
    End With 'Data

    If LCase(Right(fileToOpen, 4)) = ".lvl" Then
        If WorldEditor Then
            toggleLevelEdit
        End If

        SetActiveWindow (hWnd)
        frmLevelEditor.optCursor(13).Value = True
        ClearLevel
        OpenLevel fileToOpen
    ElseIf LCase(Right(fileToOpen, 4)) = ".wld" Then
        If Not WorldEditor Then
            toggleWorldEdit
        End If

        SetActiveWindow (hWnd)
        frmLevelEditor.optCursor(14).Value = True
        OpenWorld fileToOpen
    End If

End Sub
