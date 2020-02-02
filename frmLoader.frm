VERSION 5.00
Begin VB.Form frmLoader 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "<Research by Wohlstand> Super Mario Bros. X - Version 1.3"
   ClientHeight    =   3255
   ClientLeft      =   45
   ClientTop       =   375
   ClientWidth     =   4260
   Icon            =   "frmLoader.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3255
   ScaleWidth      =   4260
   StartUpPosition =   2  'CenterScreen
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
      Left            =   2520
      TabIndex        =   3
      Top             =   1200
      Width           =   1455
   End
   Begin VB.CommandButton cmdExit 
      Caption         =   "Exit"
      Height          =   375
      Left            =   1320
      TabIndex        =   2
      Top             =   2760
      Width           =   1455
   End
   Begin VB.CommandButton cmdEditor 
      Caption         =   "Level Editor"
      Height          =   375
      Left            =   2520
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
      Width           =   3975
   End
   Begin VB.Frame Frame2 
      Caption         =   "Settings"
      Height          =   615
      Left            =   120
      TabIndex        =   6
      Top             =   960
      Width           =   3975
   End
   Begin VB.Label Label1 
      Caption         =   $"frmLoader.frx":628A
      Height          =   975
      Left            =   120
      TabIndex        =   7
      Top             =   1680
      Width           =   3975
   End
End
Attribute VB_Name = "frmLoader"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub cmdEditor_Click()
    LevelEditor = True
    StartMenu = True
End Sub

Private Sub cmdExit_Click()
    KillIt
End Sub

Private Sub cmdGame_Click()
    StartMenu = True
End Sub

Private Sub Form_Load()
    ' No more needed. Wohlstand
    ' Splash.Navigate "http://www.supermariobrothers.org/splash/"
End Sub

Private Sub Form_Unload(Cancel As Integer)
    If StartMenu = False Then KillIt
End Sub
