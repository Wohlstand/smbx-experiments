VERSION 5.00
Begin VB.Form frmLoader 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "<Title defined at Load()>"
   ClientHeight    =   4845
   ClientLeft      =   45
   ClientTop       =   375
   ClientWidth     =   4890
   Icon            =   "frmLoader.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4845
   ScaleWidth      =   4890
   StartUpPosition =   2  'CenterScreen
   Begin VB.CheckBox chkRecord 
      Caption         =   "Record Gameplay Data"
      Height          =   255
      Left            =   240
      TabIndex        =   10
      Top             =   1560
      Value           =   1  'Checked
      Width           =   2175
   End
   Begin VB.CheckBox chkFrameskip 
      Caption         =   "Disable Frameskip"
      Height          =   255
      Left            =   240
      TabIndex        =   4
      Top             =   1200
      Width           =   1695
   End
   Begin VB.CheckBox chkSound 
      Caption         =   "Disable Sound"
      Height          =   255
      Left            =   1920
      TabIndex        =   3
      Top             =   1200
      Width           =   1455
   End
   Begin VB.CommandButton cmdExit 
      Caption         =   "Exit"
      Height          =   375
      Left            =   1680
      TabIndex        =   2
      Top             =   4320
      Width           =   1575
   End
   Begin VB.CommandButton cmdEditor 
      Caption         =   "Start Editor"
      Height          =   375
      Left            =   3360
      TabIndex        =   1
      Top             =   360
      Width           =   1335
   End
   Begin VB.CommandButton cmdGame 
      Caption         =   "Start Game"
      Height          =   375
      Left            =   240
      TabIndex        =   0
      Top             =   360
      Width           =   1335
   End
   Begin VB.Frame Frame1 
      Caption         =   "What to do?"
      Height          =   735
      Left            =   120
      TabIndex        =   5
      Top             =   120
      Width           =   4695
   End
   Begin VB.Frame Frame2 
      Caption         =   "Settings"
      Height          =   1335
      Left            =   120
      TabIndex        =   6
      Top             =   960
      Width           =   4695
      Begin VB.CommandButton showLog 
         Caption         =   "Show debug log"
         Height          =   255
         Left            =   3240
         TabIndex        =   9
         Top             =   240
         Width           =   1335
      End
   End
   Begin VB.Label Label2 
      Caption         =   $"frmLoader.frx":628A
      ForeColor       =   &H000000FF&
      Height          =   855
      Left            =   120
      TabIndex        =   8
      Top             =   3360
      Width           =   4695
   End
   Begin VB.Label Label1 
      Caption         =   $"frmLoader.frx":6364
      Height          =   855
      Left            =   120
      TabIndex        =   7
      Top             =   2400
      Width           =   4695
   End
End
Attribute VB_Name = "frmLoader"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub cmdEditor_Click()
    DebugMsg "Starting Editor..."
    LevelEditor = True
    StartMenu = True
End Sub

Private Sub cmdExit_Click()
    KillIt
End Sub

Private Sub cmdGame_Click()
    DebugMsg "Starting game..."
    StartMenu = True
End Sub

Private Sub Form_Load()
    ' No more needed. Wohlstand
    ' Splash.Navigate "http://www.supermariobrothers.org/splash/"
    Me.Caption = "Super Mario Bros. X - Version " & App.Major & "." & App.Minor & "." & App.Revision & " [Wohlstand's Edition]"
    chkFrameskip.Value = IIf(FrameSkip, 0, 1)
    chkSound.Value = IIf(noSound, 1, 0)
    chkRecord.Value = IIf(g_recordControlRecord, 1, 0)
End Sub

Private Sub Form_Unload(Cancel As Integer)
    If StartMenu = False Then KillIt
End Sub

Private Sub showLog_Click()
    frmDebugLog.Show
End Sub
