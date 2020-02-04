VERSION 5.00
Begin VB.Form frmDebugLog 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Debug log"
   ClientHeight    =   6075
   ClientLeft      =   195
   ClientTop       =   585
   ClientWidth     =   9330
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6075
   ScaleWidth      =   9330
   Begin VB.TextBox debugOutput 
      BeginProperty Font 
         Name            =   "Courier New"
         Size            =   8.25
         Charset         =   204
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   5895
      Left            =   120
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   0
      Top             =   120
      Width           =   9135
   End
End
Attribute VB_Name = "frmDebugLog"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit

Private Sub Form_Load()
    Me.Icon = frmMain.Icon
End Sub

Public Sub AddMsg(Text As String)
    Me.debugOutput.Text = Me.debugOutput.Text & vbCrLf & Text
    Me.debugOutput.SelStart = Len(Me.debugOutput.Text)
End Sub

