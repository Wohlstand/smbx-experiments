VERSION 5.00
Begin VB.Form frmDebugLog 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Debug log"
   ClientHeight    =   6465
   ClientLeft      =   195
   ClientTop       =   585
   ClientWidth     =   9345
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6465
   ScaleWidth      =   9345
   Begin VB.CommandButton CleanLog 
      Caption         =   "Clear"
      Height          =   375
      Left            =   7920
      TabIndex        =   1
      Top             =   6000
      Width           =   1335
   End
   Begin VB.TextBox debugOutput 
      BackColor       =   &H00000000&
      BeginProperty Font 
         Name            =   "Courier New"
         Size            =   8.25
         Charset         =   204
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   5895
      Left            =   0
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   0
      Top             =   0
      Width           =   9375
   End
End
Attribute VB_Name = "frmDebugLog"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit

Private Sub CleanLog_Click()
    Me.debugOutput.Text = ""
End Sub

Private Sub Form_Load()
    Me.Icon = frmMain.Icon
End Sub

Public Sub AddMsg(Text As String)
    Me.debugOutput.Text = Me.debugOutput.Text & vbCrLf & Text
    Me.debugOutput.SelStart = Len(Me.debugOutput.Text)
End Sub

