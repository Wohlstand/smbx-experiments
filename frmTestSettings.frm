VERSION 5.00
Begin VB.Form frmTestSettings 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Level Test Settings"
   ClientHeight    =   4455
   ClientLeft      =   45
   ClientTop       =   345
   ClientWidth     =   10110
   Icon            =   "frmTestSettings.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   ScaleHeight     =   4455
   ScaleWidth      =   10110
   Begin VB.CommandButton cmdClose 
      Caption         =   "Close"
      Height          =   375
      Left            =   120
      TabIndex        =   24
      Top             =   1800
      Width           =   1455
   End
   Begin VB.Frame framePlayer 
      Caption         =   "Player 2"
      Height          =   4215
      Index           =   2
      Left            =   5880
      TabIndex        =   29
      Top             =   120
      Width           =   4095
      Begin VB.Frame Frame2 
         Caption         =   "Character"
         Height          =   615
         Left            =   120
         TabIndex        =   35
         Top             =   240
         Width           =   3855
         Begin VB.OptionButton optChrLink 
            Caption         =   "Link"
            Height          =   255
            Index           =   2
            Left            =   3120
            TabIndex        =   60
            Top             =   240
            Width           =   615
         End
         Begin VB.OptionButton optChrToad 
            Caption         =   "Toad"
            Height          =   255
            Index           =   2
            Left            =   2400
            TabIndex        =   58
            Top             =   240
            Width           =   735
         End
         Begin VB.OptionButton optChrPeach 
            Caption         =   "Peach"
            Height          =   255
            Index           =   2
            Left            =   1560
            TabIndex        =   56
            Top             =   240
            Width           =   855
         End
         Begin VB.OptionButton optChrLuigi 
            Caption         =   "Luigi"
            Height          =   255
            Index           =   2
            Left            =   840
            TabIndex        =   37
            Top             =   240
            Value           =   -1  'True
            Width           =   735
         End
         Begin VB.OptionButton optChrMario 
            Caption         =   "Mario"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   36
            Top             =   240
            Width           =   735
         End
      End
      Begin VB.Frame frameState 
         Caption         =   "State"
         Height          =   2055
         Index           =   2
         Left            =   120
         TabIndex        =   31
         Top             =   840
         Width           =   1935
         Begin VB.OptionButton optIceMario 
            Caption         =   "Ice Luigi"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   61
            Top             =   960
            Width           =   1455
         End
         Begin VB.OptionButton optHammerMario 
            Caption         =   "Hammer Luigi"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   41
            Top             =   1680
            Width           =   1695
         End
         Begin VB.OptionButton optTanookiMario 
            Caption         =   "Tanooki Luigi"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   39
            Top             =   1440
            Width           =   1455
         End
         Begin VB.OptionButton optMario 
            Caption         =   "Luigi"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   15
            Top             =   240
            Value           =   -1  'True
            Width           =   1455
         End
         Begin VB.OptionButton optSuperMario 
            Caption         =   "Super Luigi"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   16
            Top             =   480
            Width           =   1455
         End
         Begin VB.OptionButton optFireMario 
            Caption         =   "Fire Luigi"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   17
            Top             =   720
            Width           =   1455
         End
         Begin VB.OptionButton optRacoonMario 
            Caption         =   "Racoon Luigi"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   18
            Top             =   1200
            Width           =   1455
         End
      End
      Begin VB.Frame frameMount 
         Caption         =   "Mount"
         Height          =   3255
         Index           =   2
         Left            =   2160
         TabIndex        =   30
         Top             =   840
         Width           =   1815
         Begin VB.OptionButton optBlueBoot 
            Caption         =   "Lakitu's Shoe"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   54
            Top             =   2880
            Width           =   1335
         End
         Begin VB.OptionButton optIceYoshi 
            Caption         =   "Ice Yoshi"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   53
            Top             =   2160
            Width           =   1215
         End
         Begin VB.OptionButton optPinkYoshi 
            Caption         =   "Pink Yoshi"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   52
            Top             =   1920
            Width           =   1215
         End
         Begin VB.OptionButton optPurpleYoshi 
            Caption         =   "Purple Yoshi"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   51
            Top             =   1680
            Width           =   1215
         End
         Begin VB.OptionButton optBlackYoshi 
            Caption         =   "Black Yoshi"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   50
            Top             =   1440
            Width           =   1215
         End
         Begin VB.OptionButton optRedBoot 
            Caption         =   "Podoboo's Shoe"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   49
            Top             =   2640
            Width           =   1575
         End
         Begin VB.OptionButton optBoot 
            Caption         =   "Kuribo's Shoe"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   48
            Top             =   2400
            Width           =   1335
         End
         Begin VB.OptionButton optGreenYoshi 
            Caption         =   "Green Yoshi"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   20
            Top             =   480
            Width           =   1215
         End
         Begin VB.OptionButton optBlueYoshi 
            Caption         =   "Blue Yoshi"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   21
            Top             =   720
            Width           =   1215
         End
         Begin VB.OptionButton optYellowYoshi 
            Caption         =   "Yellow Yoshi"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   22
            Top             =   960
            Width           =   1215
         End
         Begin VB.OptionButton optRedYoshi 
            Caption         =   "Red Yoshi"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   23
            Top             =   1200
            Width           =   1215
         End
         Begin VB.OptionButton optNoMount 
            Caption         =   "None"
            Height          =   255
            Index           =   2
            Left            =   120
            TabIndex        =   19
            Top             =   240
            Value           =   -1  'True
            Width           =   1575
         End
      End
   End
   Begin VB.Frame framePlayer 
      Caption         =   "Player 1"
      Height          =   4215
      Index           =   1
      Left            =   1680
      TabIndex        =   26
      Top             =   120
      Width           =   4095
      Begin VB.Frame Frame1 
         Caption         =   "Character"
         Height          =   615
         Left            =   120
         TabIndex        =   32
         Top             =   240
         Width           =   3855
         Begin VB.OptionButton optChrLink 
            Caption         =   "Link"
            Height          =   255
            Index           =   1
            Left            =   3120
            TabIndex        =   59
            Top             =   240
            Width           =   615
         End
         Begin VB.OptionButton optChrToad 
            Caption         =   "Toad"
            Height          =   255
            Index           =   1
            Left            =   2400
            TabIndex        =   57
            Top             =   240
            Width           =   735
         End
         Begin VB.OptionButton optChrPeach 
            Caption         =   "Peach"
            Height          =   255
            Index           =   1
            Left            =   1560
            TabIndex        =   55
            Top             =   240
            Width           =   855
         End
         Begin VB.OptionButton optChrLuigi 
            Caption         =   "Luigi"
            Height          =   255
            Index           =   1
            Left            =   840
            TabIndex        =   34
            Top             =   240
            Width           =   735
         End
         Begin VB.OptionButton optChrMario 
            Caption         =   "Mario"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   33
            Top             =   240
            Value           =   -1  'True
            Width           =   735
         End
      End
      Begin VB.Frame frameState 
         Caption         =   "State"
         Height          =   2055
         Index           =   1
         Left            =   120
         TabIndex        =   28
         Top             =   840
         Width           =   1935
         Begin VB.OptionButton optIceMario 
            Caption         =   "Ice Mario"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   62
            Top             =   960
            Width           =   1455
         End
         Begin VB.OptionButton optHammerMario 
            Caption         =   "Hammer Mario"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   40
            Top             =   1680
            Width           =   1695
         End
         Begin VB.OptionButton optTanookiMario 
            Caption         =   "Tanooki Mario"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   38
            Top             =   1440
            Width           =   1455
         End
         Begin VB.OptionButton optMario 
            Caption         =   "Mario"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   5
            Top             =   240
            Value           =   -1  'True
            Width           =   1455
         End
         Begin VB.OptionButton optSuperMario 
            Caption         =   "Super Mario"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   6
            Top             =   480
            Width           =   1455
         End
         Begin VB.OptionButton optFireMario 
            Caption         =   "Fire Mario"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   7
            Top             =   720
            Width           =   1455
         End
         Begin VB.OptionButton optRacoonMario 
            Caption         =   "Racoon Mario"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   8
            Top             =   1200
            Width           =   1455
         End
      End
      Begin VB.Frame frameMount 
         Caption         =   "Mount"
         Height          =   3255
         Index           =   1
         Left            =   2160
         TabIndex        =   27
         Top             =   840
         Width           =   1815
         Begin VB.OptionButton optIceYoshi 
            Caption         =   "Ice Yoshi"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   47
            Top             =   2160
            Width           =   1215
         End
         Begin VB.OptionButton optPinkYoshi 
            Caption         =   "Pink Yoshi"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   46
            Top             =   1920
            Width           =   1215
         End
         Begin VB.OptionButton optPurpleYoshi 
            Caption         =   "Purple Yoshi"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   45
            Top             =   1680
            Width           =   1215
         End
         Begin VB.OptionButton optBlackYoshi 
            Caption         =   "Black Yoshi"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   44
            Top             =   1440
            Width           =   1215
         End
         Begin VB.OptionButton optBlueBoot 
            Caption         =   "Lakitu's Shoe"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   43
            Top             =   2880
            Width           =   1335
         End
         Begin VB.OptionButton optRedBoot 
            Caption         =   "Podoboo's Shoe"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   42
            Top             =   2640
            Width           =   1575
         End
         Begin VB.OptionButton optGreenYoshi 
            Caption         =   "Green Yoshi"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   10
            Top             =   480
            Width           =   1215
         End
         Begin VB.OptionButton optBlueYoshi 
            Caption         =   "Blue Yoshi"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   11
            Top             =   720
            Width           =   1215
         End
         Begin VB.OptionButton optYellowYoshi 
            Caption         =   "Yellow Yoshi"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   12
            Top             =   960
            Width           =   1215
         End
         Begin VB.OptionButton optRedYoshi 
            Caption         =   "Red Yoshi"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   13
            Top             =   1200
            Width           =   1215
         End
         Begin VB.OptionButton optBoot 
            Caption         =   "Kuribo's Shoe"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   14
            Top             =   2400
            Width           =   1335
         End
         Begin VB.OptionButton optNoMount 
            Caption         =   "None"
            Height          =   255
            Index           =   1
            Left            =   120
            TabIndex        =   9
            Top             =   240
            Value           =   -1  'True
            Width           =   1575
         End
      End
   End
   Begin VB.Frame Frame 
      Caption         =   "General Settings"
      Height          =   1575
      Left            =   120
      TabIndex        =   25
      Top             =   120
      Width           =   1455
      Begin VB.CheckBox chkFullscreen 
         Caption         =   "Fullscreen"
         Height          =   255
         Left            =   120
         TabIndex        =   4
         Top             =   1200
         Width           =   1215
      End
      Begin VB.CheckBox chkMaxFPS 
         Caption         =   "Max FPS"
         Height          =   255
         Left            =   120
         TabIndex        =   3
         Top             =   960
         Width           =   1215
      End
      Begin VB.CheckBox chkShowFPS 
         Caption         =   "Show FPS"
         Height          =   255
         Left            =   120
         TabIndex        =   2
         Top             =   720
         Width           =   1215
      End
      Begin VB.CheckBox chkGodMode 
         Caption         =   "God Mode"
         Height          =   255
         Left            =   120
         TabIndex        =   1
         Top             =   480
         Width           =   1215
      End
      Begin VB.CheckBox chkGrabAll 
         Caption         =   "Grab All"
         Height          =   255
         Left            =   120
         TabIndex        =   0
         Top             =   240
         Width           =   975
      End
   End
End
Attribute VB_Name = "frmTestSettings"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub chkFullscreen_Click()
    TestFullscreen = chkFullscreen.Value
End Sub

Private Sub chkGodMode_Click()
    GodMode = chkGodMode.Value
End Sub

Private Sub chkGrabAll_Click()
    GrabAll = chkGrabAll.Value
End Sub

Private Sub chkMaxFPS_Click()
    MaxFPS = chkMaxFPS.Value
End Sub

Private Sub chkShowFPS_Click()
    ShowFPS = chkShowFPS.Value
End Sub

Private Sub cmdClose_Click()
    Me.Hide
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Cancel = 1
    Me.Hide
End Sub

Private Sub optBlackYoshi_Click(index As Integer)
    testPlayer(index).Mount = 3
    testPlayer(index).MountType = 5
End Sub

Private Sub optBlueBoot_Click(index As Integer)
    testPlayer(index).Mount = 1
    testPlayer(index).MountType = 3
End Sub

Private Sub optBlueYoshi_Click(index As Integer)
    testPlayer(index).Mount = 3
    testPlayer(index).MountType = 2
End Sub

Private Sub optBoot_Click(index As Integer)
    testPlayer(index).Mount = 1
    testPlayer(index).MountType = 1
End Sub

Private Sub optChrLink_Click(index As Integer)
    testPlayer(index).Character = 5
    optMario(index).Caption = "Link (1 HP)"
    optSuperMario(index).Caption = "Link (2 HP)"
    optFireMario(index).Caption = "Fire Link"
    optRacoonMario(index).Caption = "Fairy Link"
    optTanookiMario(index).Caption = "Gold Link"
    optHammerMario(index).Caption = "Shadow Link"
    optIceMario(index).Caption = "Ice Link"
    
    ShowAll index
    'optMario(Index).Value = True
    'optSuperMario(Index).Visible = False
    'optRacoonMario(Index).Visible = False
    'optTanookiMario(Index).Visible = False
    'optHammerMario(Index).Visible = False
    
    optNoMount(index).Value = True
    optGreenYoshi(index).Visible = False
    optRedYoshi(index).Visible = False
    optYellowYoshi(index).Visible = False
    optBlueYoshi(index).Visible = False
    optBlackYoshi(index).Visible = False
    optPinkYoshi(index).Visible = False
    optPurpleYoshi(index).Visible = False
    optIceYoshi(index).Visible = False
    optBoot(index).Visible = False
    optRedBoot(index).Visible = False
    optBlueBoot(index).Visible = False
End Sub

Private Sub optChrLuigi_Click(index As Integer)
    testPlayer(index).Character = 2
    optMario(index).Caption = "Luigi"
    optSuperMario(index).Caption = "Super Luigi"
    optFireMario(index).Caption = "Fire Luigi"
    optRacoonMario(index).Caption = "Racoon Luigi"
    optTanookiMario(index).Caption = "Tanooki Luigi"
    optHammerMario(index).Caption = "Hammer Luigi"
    optIceMario(index).Caption = "Ice Luigi"
    
    ShowAll index
    optMario(index).Value = True
    optNoMount(index).Value = True
End Sub

Private Sub optChrMario_Click(index As Integer)
    testPlayer(index).Character = 1
    optMario(index).Caption = "Mario"
    optSuperMario(index).Caption = "Super Mario"
    optFireMario(index).Caption = "Fire Mario"
    optRacoonMario(index).Caption = "Racoon Mario"
    optTanookiMario(index).Caption = "Tanooki Mario"
    optHammerMario(index).Caption = "Hammer Mario"
    optIceMario(index).Caption = "Ice Mario"
    
    ShowAll index
    optMario(index).Value = True
    optNoMount(index).Value = True
End Sub

Private Sub optChrPeach_Click(index As Integer)
    testPlayer(index).Character = 3
    optMario(index).Caption = "Peach"
    optSuperMario(index).Caption = "Super Peach"
    optFireMario(index).Caption = "Fire Peach"
    optRacoonMario(index).Caption = "Racoon Peach"
    optTanookiMario(index).Caption = "Tanooki Peach"
    optHammerMario(index).Caption = "Bomb Peach"
    optIceMario(index).Caption = "Ice Peach"
    
    ShowAll index
    'optMario(Index).Value = True
    'optRacoonMario(Index).Visible = False
    'optTanookiMario(Index).Visible = False
    'optHammerMario(Index).Visible = False
    optNoMount(index).Value = True
    optGreenYoshi(index).Visible = False
    optRedYoshi(index).Visible = False
    optYellowYoshi(index).Visible = False
    optBlueYoshi(index).Visible = False
    optBlackYoshi(index).Visible = False
    optPinkYoshi(index).Visible = False
    optPurpleYoshi(index).Visible = False
    optIceYoshi(index).Visible = False
End Sub

Private Sub optChrToad_Click(index As Integer)
    testPlayer(index).Character = 4
    optMario(index).Caption = "Toad"
    optSuperMario(index).Caption = "Super Toad"
    optFireMario(index).Caption = "Fire Toad"
    optRacoonMario(index).Caption = "Racoon Toad"
    optTanookiMario(index).Caption = "Tanooki Toad"
    optHammerMario(index).Caption = "Boomerang Toad"
    optIceMario(index).Caption = "Ice Toad"
    
    ShowAll index
    'optMario(Index).Value = True
    'optRacoonMario(Index).Visible = False
    'optTanookiMario(Index).Visible = False
    'optHammerMario(Index).Visible = False
    optNoMount(index).Value = True
    optGreenYoshi(index).Visible = False
    optRedYoshi(index).Visible = False
    optYellowYoshi(index).Visible = False
    optBlueYoshi(index).Visible = False
    optBlackYoshi(index).Visible = False
    optPinkYoshi(index).Visible = False
    optPurpleYoshi(index).Visible = False
    optIceYoshi(index).Visible = False
End Sub

Private Sub optFireMario_Click(index As Integer)
    testPlayer(index).State = 3
End Sub

Private Sub optGreenYoshi_Click(index As Integer)
    testPlayer(index).Mount = 3
    testPlayer(index).MountType = 1
End Sub

Private Sub optKoopaClownCar_Click(index As Integer)
    testPlayer(index).Mount = 2
End Sub

Private Sub optHammerMario_Click(index As Integer)
    testPlayer(index).State = 6
End Sub

Private Sub optIceMario_Click(index As Integer)
    testPlayer(index).State = 7
End Sub

Private Sub optIceYoshi_Click(index As Integer)
    testPlayer(index).Mount = 3
    testPlayer(index).MountType = 8
End Sub

Private Sub optMario_Click(index As Integer)
    testPlayer(index).State = 1
End Sub

Private Sub optMarioC_Click(index As Integer)
    testPlayer(1).Character = index
End Sub

Private Sub optNoMount_Click(index As Integer)
    testPlayer(index).Mount = 0
End Sub

Private Sub optPinkYoshi_Click(index As Integer)
    testPlayer(index).Mount = 3
    testPlayer(index).MountType = 7
End Sub

Private Sub optPurpleYoshi_Click(index As Integer)
    testPlayer(index).Mount = 3
    testPlayer(index).MountType = 6
End Sub

Private Sub optRacoonMario_Click(index As Integer)
    testPlayer(index).State = 4
End Sub

Private Sub optRedBoot_Click(index As Integer)
    testPlayer(index).Mount = 1
    testPlayer(index).MountType = 2
End Sub

Private Sub optRedYoshi_Click(index As Integer)
    testPlayer(index).Mount = 3
    testPlayer(index).MountType = 4
End Sub

Private Sub optSuperMario_Click(index As Integer)
    testPlayer(index).State = 2
End Sub

Private Sub optTanookiMario_Click(index As Integer)
    testPlayer(index).State = 5
End Sub

Private Sub optYellowYoshi_Click(index As Integer)
    testPlayer(index).Mount = 3
    testPlayer(index).MountType = 3
End Sub

Private Sub ShowAll(index As Integer)
    optMario(index).Value = True
    optMario(index).Visible = True
    optSuperMario(index).Visible = True
    optFireMario(index).Visible = True
    optRacoonMario(index).Visible = True
    optTanookiMario(index).Visible = True
    optHammerMario(index).Visible = True
    optGreenYoshi(index).Visible = True
    optRedYoshi(index).Visible = True
    optYellowYoshi(index).Visible = True
    optBlueYoshi(index).Visible = True
    optBlackYoshi(index).Visible = True
    optPinkYoshi(index).Visible = True
    optPurpleYoshi(index).Visible = True
    optIceYoshi(index).Visible = True
    optBoot(index).Visible = True
    optRedBoot(index).Visible = True
    optBlueBoot(index).Visible = True
    optNoMount(index).Visible = True
End Sub
