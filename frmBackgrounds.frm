VERSION 5.00
Begin VB.Form frmBackgrounds 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Background Objects"
   ClientHeight    =   3810
   ClientLeft      =   -19950
   ClientTop       =   330
   ClientWidth     =   15210
   Icon            =   "frmBackgrounds.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   ScaleHeight     =   3810
   ScaleWidth      =   15210
   Visible         =   0   'False
   Begin VB.OptionButton Background 
      BackColor       =   &H00000000&
      Height          =   540
      Index           =   160
      Left            =   17880
      Picture         =   "frmBackgrounds.frx":0B3A
      Style           =   1  'Graphical
      TabIndex        =   198
      Top             =   480
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.Frame Game 
      Caption         =   "Misc."
      Height          =   2535
      Index           =   4
      Left            =   9120
      TabIndex        =   146
      Top             =   10200
      Visible         =   0   'False
      Width           =   5655
      Begin VB.Frame Frame6 
         Caption         =   "Metroid"
         Height          =   2175
         Left            =   3480
         TabIndex        =   184
         Top             =   240
         Width           =   2055
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   157
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":0F0A
            Style           =   1  'Graphical
            TabIndex        =   191
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   156
            Left            =   720
            Picture         =   "frmBackgrounds.frx":12C0
            Style           =   1  'Graphical
            TabIndex        =   190
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   155
            Left            =   720
            Picture         =   "frmBackgrounds.frx":167F
            Style           =   1  'Graphical
            TabIndex        =   189
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   154
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":1A3D
            Style           =   1  'Graphical
            TabIndex        =   188
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   153
            Left            =   120
            Picture         =   "frmBackgrounds.frx":1DF9
            Style           =   1  'Graphical
            TabIndex        =   187
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   152
            Left            =   120
            Picture         =   "frmBackgrounds.frx":2234
            Style           =   1  'Graphical
            TabIndex        =   186
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   151
            Left            =   120
            Picture         =   "frmBackgrounds.frx":264C
            Style           =   1  'Graphical
            TabIndex        =   185
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame Frame5 
         Caption         =   "Zelda 2"
         Height          =   2175
         Left            =   120
         TabIndex        =   171
         Top             =   240
         Width           =   3255
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   115
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":2A5B
            Style           =   1  'Graphical
            TabIndex        =   183
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   116
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":2E25
            Style           =   1  'Graphical
            TabIndex        =   182
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   117
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":31BD
            Style           =   1  'Graphical
            TabIndex        =   181
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   118
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":358B
            Style           =   1  'Graphical
            TabIndex        =   180
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   119
            Left            =   720
            Picture         =   "frmBackgrounds.frx":3990
            Style           =   1  'Graphical
            TabIndex        =   179
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            Height          =   540
            Index           =   120
            Left            =   720
            Picture         =   "frmBackgrounds.frx":3D93
            Style           =   1  'Graphical
            TabIndex        =   178
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   121
            Left            =   720
            Picture         =   "frmBackgrounds.frx":419D
            Style           =   1  'Graphical
            TabIndex        =   177
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   122
            Left            =   120
            Picture         =   "frmBackgrounds.frx":45EB
            Style           =   1  'Graphical
            TabIndex        =   176
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   123
            Left            =   120
            Picture         =   "frmBackgrounds.frx":4A08
            Style           =   1  'Graphical
            TabIndex        =   175
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   124
            Left            =   120
            Picture         =   "frmBackgrounds.frx":4E0B
            Style           =   1  'Graphical
            TabIndex        =   174
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   125
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":5227
            Style           =   1  'Graphical
            TabIndex        =   173
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   126
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":5604
            Style           =   1  'Graphical
            TabIndex        =   172
            Top             =   840
            Width           =   540
         End
      End
   End
   Begin VB.OptionButton Background 
      BackColor       =   &H00000000&
      Height          =   540
      Index           =   98
      Left            =   17280
      Picture         =   "frmBackgrounds.frx":5AF3
      Style           =   1  'Graphical
      TabIndex        =   10
      Top             =   480
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.PictureBox FocusNinja 
      Height          =   375
      Left            =   19080
      ScaleHeight     =   315
      ScaleWidth      =   315
      TabIndex        =   0
      Top             =   840
      Width           =   375
   End
   Begin VB.Frame Game 
      Caption         =   "Super Mario Bros. 2"
      Height          =   1935
      Index           =   3
      Left            =   1920
      TabIndex        =   9
      Top             =   10560
      Visible         =   0   'False
      Width           =   6015
      Begin VB.Frame Frame 
         Caption         =   "Water"
         Height          =   1575
         Index           =   22
         Left            =   4080
         TabIndex        =   192
         Top             =   240
         Width           =   855
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   159
            Left            =   120
            Picture         =   "frmBackgrounds.frx":5EAE
            Style           =   1  'Graphical
            TabIndex        =   194
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   158
            Left            =   120
            Picture         =   "frmBackgrounds.frx":5FD4
            Style           =   1  'Graphical
            TabIndex        =   193
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Misc."
         Height          =   1575
         Index           =   19
         Left            =   120
         TabIndex        =   106
         Top             =   240
         Width           =   3855
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   148
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":63F8
            Style           =   1  'Graphical
            TabIndex        =   197
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   149
            Left            =   3120
            Picture         =   "frmBackgrounds.frx":681C
            Style           =   1  'Graphical
            TabIndex        =   196
            Top             =   840
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   150
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":6C54
            Style           =   1  'Graphical
            TabIndex        =   195
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   112
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":7085
            Style           =   1  'Graphical
            TabIndex        =   143
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   111
            Left            =   720
            Picture         =   "frmBackgrounds.frx":74A9
            Style           =   1  'Graphical
            TabIndex        =   142
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   113
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":78E9
            Style           =   1  'Graphical
            TabIndex        =   141
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   110
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":7D16
            Style           =   1  'Graphical
            TabIndex        =   140
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   108
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":8177
            Style           =   1  'Graphical
            TabIndex        =   139
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   109
            Left            =   720
            Picture         =   "frmBackgrounds.frx":85C9
            Style           =   1  'Graphical
            TabIndex        =   138
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   63
            Left            =   120
            Picture         =   "frmBackgrounds.frx":8A1B
            Style           =   1  'Graphical
            TabIndex        =   108
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   62
            Left            =   120
            Picture         =   "frmBackgrounds.frx":8DF4
            Style           =   1  'Graphical
            TabIndex        =   107
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Doors"
         Height          =   1575
         Index           =   20
         Left            =   5040
         TabIndex        =   103
         Top             =   240
         Width           =   855
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   87
            Left            =   120
            Picture         =   "frmBackgrounds.frx":9246
            Style           =   1  'Graphical
            TabIndex        =   105
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   88
            Left            =   120
            Picture         =   "frmBackgrounds.frx":9616
            Style           =   1  'Graphical
            TabIndex        =   104
            Top             =   840
            Width           =   540
         End
      End
   End
   Begin VB.Frame Game 
      Caption         =   "Super Mario World"
      Height          =   3375
      Index           =   2
      Left            =   120
      TabIndex        =   7
      Top             =   3960
      Visible         =   0   'False
      Width           =   13095
      Begin VB.Frame Frame4 
         Caption         =   "Fence"
         Height          =   2055
         Left            =   9720
         TabIndex        =   213
         Top             =   240
         Width           =   3255
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   186
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":9A1F
            Style           =   1  'Graphical
            TabIndex        =   226
            Top             =   1440
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   185
            Left            =   720
            Picture         =   "frmBackgrounds.frx":9E2A
            Style           =   1  'Graphical
            TabIndex        =   225
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   184
            Left            =   120
            Picture         =   "frmBackgrounds.frx":A203
            Style           =   1  'Graphical
            TabIndex        =   224
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   183
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":A610
            Style           =   1  'Graphical
            TabIndex        =   223
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   182
            Left            =   720
            Picture         =   "frmBackgrounds.frx":AA0D
            Style           =   1  'Graphical
            TabIndex        =   222
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   181
            Left            =   120
            Picture         =   "frmBackgrounds.frx":ADDC
            Style           =   1  'Graphical
            TabIndex        =   221
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   180
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":B1DD
            Style           =   1  'Graphical
            TabIndex        =   220
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   179
            Left            =   720
            Picture         =   "frmBackgrounds.frx":B5ED
            Style           =   1  'Graphical
            TabIndex        =   219
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   178
            Left            =   120
            Picture         =   "frmBackgrounds.frx":B9C7
            Style           =   1  'Graphical
            TabIndex        =   218
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   177
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":BDD3
            Style           =   1  'Graphical
            TabIndex        =   217
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   176
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":C1BE
            Style           =   1  'Graphical
            TabIndex        =   216
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   175
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":C5A8
            Style           =   1  'Graphical
            TabIndex        =   215
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   174
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":C992
            Style           =   1  'Graphical
            TabIndex        =   214
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Water"
         Height          =   855
         Index           =   23
         Left            =   6480
         TabIndex        =   204
         Top             =   2400
         Width           =   2055
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   173
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":CD7C
            Style           =   1  'Graphical
            TabIndex        =   212
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   167
            Left            =   720
            Picture         =   "frmBackgrounds.frx":D207
            Style           =   1  'Graphical
            TabIndex        =   206
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   166
            Left            =   120
            Picture         =   "frmBackgrounds.frx":D56B
            Style           =   1  'Graphical
            TabIndex        =   205
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame3 
         Caption         =   "Checkpoint / Exit"
         Height          =   1455
         Left            =   6480
         TabIndex        =   155
         Top             =   960
         Width           =   3135
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   145
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":D904
            Style           =   1  'Graphical
            TabIndex        =   167
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   144
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":DD20
            Style           =   1  'Graphical
            TabIndex        =   166
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   143
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":E13C
            Style           =   1  'Graphical
            TabIndex        =   165
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   142
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":E551
            Style           =   1  'Graphical
            TabIndex        =   164
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   138
            Left            =   720
            Picture         =   "frmBackgrounds.frx":E97B
            Style           =   1  'Graphical
            TabIndex        =   160
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   137
            Left            =   720
            Picture         =   "frmBackgrounds.frx":ED57
            Style           =   1  'Graphical
            TabIndex        =   159
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   136
            Left            =   120
            Picture         =   "frmBackgrounds.frx":F13F
            Style           =   1  'Graphical
            TabIndex        =   158
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   135
            Left            =   120
            Picture         =   "frmBackgrounds.frx":F543
            Style           =   1  'Graphical
            TabIndex        =   157
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   134
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":F957
            Style           =   1  'Graphical
            TabIndex        =   156
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Ghost House"
         Height          =   1455
         Index           =   16
         Left            =   2880
         TabIndex        =   95
         Top             =   120
         Width           =   3255
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   146
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":FD65
            Style           =   1  'Graphical
            TabIndex        =   168
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   140
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":10248
            Style           =   1  'Graphical
            TabIndex        =   162
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   139
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":106DE
            Style           =   1  'Graphical
            TabIndex        =   161
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   43
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":10BC3
            Style           =   1  'Graphical
            TabIndex        =   102
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   44
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":11004
            Style           =   1  'Graphical
            TabIndex        =   101
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   47
            Left            =   120
            Picture         =   "frmBackgrounds.frx":1144E
            Style           =   1  'Graphical
            TabIndex        =   100
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   48
            Left            =   720
            Picture         =   "frmBackgrounds.frx":11A04
            Style           =   1  'Graphical
            TabIndex        =   99
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   49
            Left            =   120
            Picture         =   "frmBackgrounds.frx":11EF2
            Style           =   1  'Graphical
            TabIndex        =   98
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   50
            Left            =   720
            Picture         =   "frmBackgrounds.frx":12271
            Style           =   1  'Graphical
            TabIndex        =   97
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   51
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":1260E
            Style           =   1  'Graphical
            TabIndex        =   96
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Misc"
         Height          =   855
         Index           =   13
         Left            =   2880
         TabIndex        =   93
         Top             =   2400
         Width           =   2055
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   133
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":1298B
            Style           =   1  'Graphical
            TabIndex        =   154
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   132
            Left            =   720
            Picture         =   "frmBackgrounds.frx":12D22
            Style           =   1  'Graphical
            TabIndex        =   153
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   29
            Left            =   120
            Picture         =   "frmBackgrounds.frx":130CF
            Style           =   1  'Graphical
            TabIndex        =   94
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Trees"
         Height          =   1575
         Index           =   17
         Left            =   120
         TabIndex        =   84
         Top             =   1320
         Width           =   2655
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   59
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":134C4
            Style           =   1  'Graphical
            TabIndex        =   92
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   52
            Left            =   720
            Picture         =   "frmBackgrounds.frx":1384F
            Style           =   1  'Graphical
            TabIndex        =   91
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   53
            Left            =   720
            Picture         =   "frmBackgrounds.frx":13CE4
            Style           =   1  'Graphical
            TabIndex        =   90
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   54
            Left            =   120
            Picture         =   "frmBackgrounds.frx":140CB
            Style           =   1  'Graphical
            TabIndex        =   89
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   55
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":14458
            Style           =   1  'Graphical
            TabIndex        =   88
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   56
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":147E9
            Style           =   1  'Graphical
            TabIndex        =   87
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            Height          =   540
            Index           =   57
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":14B4D
            Style           =   1  'Graphical
            TabIndex        =   86
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   58
            Left            =   120
            Picture         =   "frmBackgrounds.frx":14F48
            Style           =   1  'Graphical
            TabIndex        =   85
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Bushes"
         Height          =   975
         Index           =   11
         Left            =   120
         TabIndex        =   79
         Top             =   240
         Width           =   2655
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   32
            Left            =   720
            Picture         =   "frmBackgrounds.frx":15343
            Style           =   1  'Graphical
            TabIndex        =   83
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   31
            Left            =   120
            Picture         =   "frmBackgrounds.frx":1573D
            Style           =   1  'Graphical
            TabIndex        =   82
            Top             =   240
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   28
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":15B3B
            Style           =   1  'Graphical
            TabIndex        =   81
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   27
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":15EE3
            Style           =   1  'Graphical
            TabIndex        =   80
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Block Tiles"
         Height          =   855
         Index           =   14
         Left            =   2880
         TabIndex        =   75
         Top             =   1560
         Width           =   2055
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   42
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":162E8
            Style           =   1  'Graphical
            TabIndex        =   78
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   41
            Left            =   720
            Picture         =   "frmBackgrounds.frx":16688
            Style           =   1  'Graphical
            TabIndex        =   77
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   30
            Left            =   120
            Picture         =   "frmBackgrounds.frx":16A3D
            Style           =   1  'Graphical
            TabIndex        =   76
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Platform"
         Height          =   855
         Index           =   12
         Left            =   6240
         TabIndex        =   69
         Top             =   120
         Width           =   3255
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   70
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":16DF8
            Style           =   1  'Graphical
            TabIndex        =   74
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   71
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":17234
            Style           =   1  'Graphical
            TabIndex        =   73
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   72
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":175A5
            Style           =   1  'Graphical
            TabIndex        =   72
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   73
            Left            =   720
            Picture         =   "frmBackgrounds.frx":1793A
            Style           =   1  'Graphical
            TabIndex        =   71
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   74
            Left            =   120
            Picture         =   "frmBackgrounds.frx":17CCE
            Style           =   1  'Graphical
            TabIndex        =   70
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Doors"
         Height          =   1455
         Index           =   18
         Left            =   5040
         TabIndex        =   67
         Top             =   1680
         Width           =   1335
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   141
            Left            =   120
            Picture         =   "frmBackgrounds.frx":18062
            Style           =   1  'Graphical
            TabIndex        =   163
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   104
            Left            =   720
            Picture         =   "frmBackgrounds.frx":18529
            Style           =   1  'Graphical
            TabIndex        =   128
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   105
            Left            =   720
            Picture         =   "frmBackgrounds.frx":189DA
            Style           =   1  'Graphical
            TabIndex        =   127
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   92
            Left            =   120
            Picture         =   "frmBackgrounds.frx":18D9C
            Style           =   1  'Graphical
            TabIndex        =   68
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Keyhole"
         Height          =   855
         Index           =   15
         Left            =   8640
         TabIndex        =   65
         Top             =   2400
         Width           =   855
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   35
            Left            =   120
            Picture         =   "frmBackgrounds.frx":19156
            Style           =   1  'Graphical
            TabIndex        =   66
            Top             =   240
            Width           =   540
         End
      End
   End
   Begin VB.Frame Game 
      Caption         =   "Super Mario Bros."
      Height          =   2535
      Index           =   1
      Left            =   120
      TabIndex        =   4
      Top             =   9840
      Visible         =   0   'False
      Width           =   7575
      Begin VB.Frame Frame 
         Caption         =   "Misc."
         Height          =   1095
         Index           =   21
         Left            =   5040
         TabIndex        =   169
         Top             =   240
         Width           =   855
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   147
            Left            =   120
            Picture         =   "frmBackgrounds.frx":19D98
            Style           =   1  'Graphical
            TabIndex        =   170
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Misc."
         Height          =   2175
         Index           =   8
         Left            =   120
         TabIndex        =   118
         Top             =   240
         Width           =   3255
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   161
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":1A147
            Style           =   1  'Graphical
            TabIndex        =   199
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   131
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":1A53A
            Style           =   1  'Graphical
            TabIndex        =   152
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   130
            Left            =   720
            Picture         =   "frmBackgrounds.frx":1A963
            Style           =   1  'Graphical
            TabIndex        =   151
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   129
            Left            =   120
            Picture         =   "frmBackgrounds.frx":1AD87
            Style           =   1  'Graphical
            TabIndex        =   150
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   128
            Left            =   720
            Picture         =   "frmBackgrounds.frx":1B1A6
            Style           =   1  'Graphical
            TabIndex        =   149
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   127
            Left            =   120
            Picture         =   "frmBackgrounds.frx":1B570
            Style           =   1  'Graphical
            TabIndex        =   148
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   86
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":1B959
            Style           =   1  'Graphical
            TabIndex        =   126
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   84
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":1BD81
            Style           =   1  'Graphical
            TabIndex        =   125
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   85
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":1C167
            Style           =   1  'Graphical
            TabIndex        =   124
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   25
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":1C589
            Style           =   1  'Graphical
            TabIndex        =   123
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   23
            Left            =   120
            Picture         =   "frmBackgrounds.frx":1D1CB
            Style           =   1  'Graphical
            TabIndex        =   122
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   24
            Left            =   720
            Picture         =   "frmBackgrounds.frx":1DE0D
            Style           =   1  'Graphical
            TabIndex        =   121
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   22
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":1EA4F
            Style           =   1  'Graphical
            TabIndex        =   120
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   21
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":1F691
            Style           =   1  'Graphical
            TabIndex        =   119
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Grass"
         Height          =   1095
         Index           =   7
         Left            =   3480
         TabIndex        =   114
         Top             =   1320
         Width           =   2415
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   20
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":202D3
            Style           =   1  'Graphical
            TabIndex        =   117
            Top             =   240
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   18
            Left            =   120
            Picture         =   "frmBackgrounds.frx":207B2
            Style           =   1  'Graphical
            TabIndex        =   116
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   19
            Left            =   720
            Picture         =   "frmBackgrounds.frx":20BD6
            Style           =   1  'Graphical
            TabIndex        =   115
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Water"
         Height          =   2175
         Index           =   10
         Left            =   6000
         TabIndex        =   112
         Top             =   240
         Width           =   1455
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   169
            Left            =   720
            Picture         =   "frmBackgrounds.frx":21041
            Style           =   1  'Graphical
            TabIndex        =   208
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   168
            Left            =   720
            Picture         =   "frmBackgrounds.frx":213D9
            Style           =   1  'Graphical
            TabIndex        =   207
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   164
            Left            =   120
            Picture         =   "frmBackgrounds.frx":217A1
            Style           =   1  'Graphical
            TabIndex        =   203
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   26
            Left            =   120
            Picture         =   "frmBackgrounds.frx":21B05
            Style           =   1  'Graphical
            TabIndex        =   113
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Castle"
         Height          =   975
         Index           =   9
         Left            =   3480
         TabIndex        =   109
         Top             =   240
         Width           =   1455
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   16
            Left            =   120
            Picture         =   "frmBackgrounds.frx":21E84
            Style           =   1  'Graphical
            TabIndex        =   111
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   17
            Left            =   720
            Picture         =   "frmBackgrounds.frx":224E3
            Style           =   1  'Graphical
            TabIndex        =   110
            Top             =   240
            Width           =   540
         End
      End
   End
   Begin VB.Frame Frame 
      Caption         =   "Game Selection"
      Height          =   615
      Index           =   0
      Left            =   120
      TabIndex        =   2
      Top             =   0
      Width           =   10935
      Begin VB.OptionButton optGame 
         Caption         =   "Misc."
         Height          =   255
         Index           =   4
         Left            =   8760
         Style           =   1  'Graphical
         TabIndex        =   145
         Top             =   240
         Width           =   2055
      End
      Begin VB.OptionButton optGame 
         Caption         =   "Super Mario Bros. 2"
         Height          =   255
         Index           =   3
         Left            =   2280
         Style           =   1  'Graphical
         TabIndex        =   8
         Top             =   240
         Width           =   2055
      End
      Begin VB.OptionButton optGame 
         Caption         =   "Super Mario World"
         Height          =   255
         Index           =   2
         Left            =   6600
         Style           =   1  'Graphical
         TabIndex        =   6
         Top             =   240
         Width           =   2055
      End
      Begin VB.OptionButton optGame 
         Caption         =   "Super Mario Bros."
         Height          =   255
         Index           =   1
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   5
         Top             =   240
         Width           =   2055
      End
      Begin VB.OptionButton optGame 
         Caption         =   "Super Mario Bros. 3"
         Height          =   255
         Index           =   0
         Left            =   4440
         Style           =   1  'Graphical
         TabIndex        =   3
         Top             =   240
         Value           =   -1  'True
         Width           =   2055
      End
   End
   Begin VB.Frame Game 
      Caption         =   "Super Mario Bros. 3"
      Height          =   3135
      Index           =   0
      Left            =   120
      TabIndex        =   1
      Top             =   600
      Width           =   15015
      Begin VB.Frame Frame7 
         Caption         =   "Quick Sand"
         Height          =   2775
         Left            =   13200
         TabIndex        =   227
         Top             =   120
         Width           =   1695
         Begin VB.Frame Frame9 
            Caption         =   "Back"
            Height          =   1455
            Left            =   840
            TabIndex        =   229
            Top             =   240
            Width           =   735
            Begin VB.OptionButton Background 
               BackColor       =   &H00000000&
               Height          =   540
               Index           =   190
               Left            =   120
               Picture         =   "frmBackgrounds.frx":22B2E
               Style           =   1  'Graphical
               TabIndex        =   233
               Top             =   840
               Width           =   540
            End
            Begin VB.OptionButton Background 
               BackColor       =   &H00000000&
               Height          =   540
               Index           =   189
               Left            =   120
               Picture         =   "frmBackgrounds.frx":23063
               Style           =   1  'Graphical
               TabIndex        =   232
               Top             =   240
               Width           =   540
            End
         End
         Begin VB.Frame Frame8 
            Caption         =   "Fore"
            Height          =   1455
            Left            =   120
            TabIndex        =   228
            Top             =   240
            Width           =   735
            Begin VB.OptionButton Background 
               BackColor       =   &H00000000&
               Height          =   540
               Index           =   187
               Left            =   120
               Picture         =   "frmBackgrounds.frx":23460
               Style           =   1  'Graphical
               TabIndex        =   231
               Top             =   840
               Width           =   540
            End
            Begin VB.OptionButton Background 
               BackColor       =   &H00000000&
               Height          =   540
               Index           =   188
               Left            =   120
               Picture         =   "frmBackgrounds.frx":23995
               Style           =   1  'Graphical
               TabIndex        =   230
               Top             =   240
               Width           =   540
            End
         End
      End
      Begin VB.Frame Frame2 
         Caption         =   "Platform"
         Height          =   975
         Left            =   7200
         TabIndex        =   136
         Top             =   1800
         Width           =   735
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   100
            Left            =   120
            Picture         =   "frmBackgrounds.frx":23D92
            Style           =   1  'Graphical
            TabIndex        =   137
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame1 
         Caption         =   "Doors"
         Height          =   975
         Left            =   7920
         TabIndex        =   129
         Top             =   1800
         Width           =   735
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   107
            Left            =   120
            Picture         =   "frmBackgrounds.frx":241A6
            Style           =   1  'Graphical
            TabIndex        =   130
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Misc."
         Height          =   2775
         Index           =   2
         Left            =   8760
         TabIndex        =   43
         Top             =   120
         Width           =   4335
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   163
            Left            =   3720
            Picture         =   "frmBackgrounds.frx":2457D
            Style           =   1  'Graphical
            TabIndex        =   201
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   162
            Left            =   3120
            Picture         =   "frmBackgrounds.frx":24921
            Style           =   1  'Graphical
            TabIndex        =   200
            Top             =   2040
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   103
            Left            =   3120
            Picture         =   "frmBackgrounds.frx":24CCA
            Style           =   1  'Graphical
            TabIndex        =   135
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   102
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":250D8
            Style           =   1  'Graphical
            TabIndex        =   134
            Top             =   2040
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   101
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":254B1
            Style           =   1  'Graphical
            TabIndex        =   133
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   99
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":258B2
            Style           =   1  'Graphical
            TabIndex        =   132
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   106
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":25CCD
            Style           =   1  'Graphical
            TabIndex        =   131
            Top             =   2040
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   67
            Left            =   720
            Picture         =   "frmBackgrounds.frx":26078
            Style           =   1  'Graphical
            TabIndex        =   61
            Top             =   2040
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   80
            Left            =   120
            Picture         =   "frmBackgrounds.frx":26423
            Style           =   1  'Graphical
            TabIndex        =   60
            Top             =   2040
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   81
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":267CB
            Style           =   1  'Graphical
            TabIndex        =   59
            Top             =   2040
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   37
            Left            =   3120
            Picture         =   "frmBackgrounds.frx":26B72
            Style           =   1  'Graphical
            TabIndex        =   58
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   38
            Left            =   3120
            Picture         =   "frmBackgrounds.frx":26F97
            Style           =   1  'Graphical
            TabIndex        =   57
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   36
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":27393
            Style           =   1  'Graphical
            TabIndex        =   56
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   68
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":2776B
            Style           =   1  'Graphical
            TabIndex        =   55
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   89
            Left            =   120
            Picture         =   "frmBackgrounds.frx":27BC5
            Style           =   1  'Graphical
            TabIndex        =   54
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   90
            Left            =   120
            Picture         =   "frmBackgrounds.frx":28006
            Style           =   1  'Graphical
            TabIndex        =   53
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   91
            Left            =   120
            Picture         =   "frmBackgrounds.frx":2844B
            Style           =   1  'Graphical
            TabIndex        =   52
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   69
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":28866
            Style           =   1  'Graphical
            TabIndex        =   51
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   97
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":28C23
            Style           =   1  'Graphical
            TabIndex        =   50
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   93
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":29030
            Style           =   1  'Graphical
            TabIndex        =   49
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   45
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":2942B
            Style           =   1  'Graphical
            TabIndex        =   48
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   46
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":297E3
            Style           =   1  'Graphical
            TabIndex        =   47
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   94
            Left            =   720
            Picture         =   "frmBackgrounds.frx":29B81
            Style           =   1  'Graphical
            TabIndex        =   46
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   95
            Left            =   720
            Picture         =   "frmBackgrounds.frx":29F2C
            Style           =   1  'Graphical
            TabIndex        =   45
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   96
            Left            =   720
            Picture         =   "frmBackgrounds.frx":2A2D0
            Style           =   1  'Graphical
            TabIndex        =   44
            Top             =   1440
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Water"
         Height          =   1575
         Index           =   4
         Left            =   6000
         TabIndex        =   38
         Top             =   240
         Width           =   2655
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   172
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":2A702
            Style           =   1  'Graphical
            TabIndex        =   211
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   171
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":2AB18
            Style           =   1  'Graphical
            TabIndex        =   210
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   170
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":2AF8F
            Style           =   1  'Graphical
            TabIndex        =   209
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   165
            Left            =   120
            Picture         =   "frmBackgrounds.frx":2B3F3
            Style           =   1  'Graphical
            TabIndex        =   202
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   65
            Left            =   120
            Picture         =   "frmBackgrounds.frx":2B757
            Style           =   1  'Graphical
            TabIndex        =   42
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   66
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":2BAE6
            Style           =   1  'Graphical
            TabIndex        =   41
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   82
            Left            =   720
            Picture         =   "frmBackgrounds.frx":2BEF5
            Style           =   1  'Graphical
            TabIndex        =   40
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   83
            Left            =   720
            Picture         =   "frmBackgrounds.frx":2C2BD
            Style           =   1  'Graphical
            TabIndex        =   39
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Background Tiles"
         Height          =   975
         Index           =   5
         Left            =   4560
         TabIndex        =   33
         Top             =   1800
         Width           =   2535
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   75
            Left            =   120
            Picture         =   "frmBackgrounds.frx":2C621
            Style           =   1  'Graphical
            TabIndex        =   37
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   76
            Left            =   720
            Picture         =   "frmBackgrounds.frx":2CC10
            Style           =   1  'Graphical
            TabIndex        =   36
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   77
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":2D08A
            Style           =   1  'Graphical
            TabIndex        =   35
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   78
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":2D67D
            Style           =   1  'Graphical
            TabIndex        =   34
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Black And Exit"
         Height          =   975
         Index           =   6
         Left            =   120
         TabIndex        =   28
         Top             =   1800
         Width           =   4335
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   33
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":2DAE5
            Style           =   1  'Graphical
            TabIndex        =   64
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   34
            Left            =   3120
            Picture         =   "frmBackgrounds.frx":2DEBA
            Style           =   1  'Graphical
            TabIndex        =   63
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   13
            Left            =   3720
            Picture         =   "frmBackgrounds.frx":2E404
            Style           =   1  'Graphical
            TabIndex        =   62
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00707070&
            Height          =   540
            Index           =   61
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":2E80E
            Style           =   1  'Graphical
            TabIndex        =   32
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00707070&
            Height          =   540
            Index           =   60
            Left            =   120
            Picture         =   "frmBackgrounds.frx":2EB85
            Style           =   1  'Graphical
            TabIndex        =   31
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00707070&
            Height          =   540
            Index           =   12
            Left            =   720
            Picture         =   "frmBackgrounds.frx":2EF1D
            Style           =   1  'Graphical
            TabIndex        =   30
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   11
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":2F2A6
            Style           =   1  'Graphical
            TabIndex        =   29
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Block Tiles"
         Height          =   1575
         Index           =   3
         Left            =   3960
         TabIndex        =   22
         Top             =   240
         Width           =   1935
         Begin VB.OptionButton Background 
            Height          =   540
            Index           =   79
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":2F63C
            Style           =   1  'Graphical
            TabIndex        =   147
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            Height          =   540
            Index           =   40
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":2FA6F
            Style           =   1  'Graphical
            TabIndex        =   27
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            Height          =   540
            Index           =   39
            Left            =   720
            Picture         =   "frmBackgrounds.frx":2FE27
            Style           =   1  'Graphical
            TabIndex        =   26
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            Height          =   540
            Index           =   64
            Left            =   120
            Picture         =   "frmBackgrounds.frx":30248
            Style           =   1  'Graphical
            TabIndex        =   25
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            Height          =   540
            Index           =   15
            Left            =   720
            Picture         =   "frmBackgrounds.frx":30627
            Style           =   1  'Graphical
            TabIndex        =   24
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            Height          =   540
            Index           =   14
            Left            =   120
            Picture         =   "frmBackgrounds.frx":30A7C
            Style           =   1  'Graphical
            TabIndex        =   23
            Top             =   840
            Value           =   -1  'True
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Bushes"
         Height          =   1575
         Index           =   1
         Left            =   120
         TabIndex        =   11
         Top             =   240
         Width           =   3735
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   114
            Left            =   3120
            Picture         =   "frmBackgrounds.frx":30ED8
            Style           =   1  'Graphical
            TabIndex        =   144
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   1
            Left            =   120
            Picture         =   "frmBackgrounds.frx":31292
            Style           =   1  'Graphical
            TabIndex        =   21
            Top             =   240
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   9
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":316BF
            Style           =   1  'Graphical
            TabIndex        =   20
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   10
            Left            =   2520
            Picture         =   "frmBackgrounds.frx":31AB0
            Style           =   1  'Graphical
            TabIndex        =   19
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   4
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":31ECF
            Style           =   1  'Graphical
            TabIndex        =   18
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   5
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":3229E
            Style           =   1  'Graphical
            TabIndex        =   17
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   6
            Left            =   1920
            Picture         =   "frmBackgrounds.frx":32638
            Style           =   1  'Graphical
            TabIndex        =   16
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   2
            Left            =   1320
            Picture         =   "frmBackgrounds.frx":32A4A
            Style           =   1  'Graphical
            TabIndex        =   15
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   8
            Left            =   720
            Picture         =   "frmBackgrounds.frx":32E61
            Style           =   1  'Graphical
            TabIndex        =   14
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   7
            Left            =   720
            Picture         =   "frmBackgrounds.frx":3325E
            Style           =   1  'Graphical
            TabIndex        =   13
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Background 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   3
            Left            =   120
            Picture         =   "frmBackgrounds.frx":33632
            Style           =   1  'Graphical
            TabIndex        =   12
            Top             =   840
            Width           =   540
         End
      End
   End
End
Attribute VB_Name = "frmBackgrounds"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Background_Click(Index As Integer)
    Dim A As Integer
    On Error Resume Next
    For A = 1 To Background.Count '- 1
        If Index <> A Then Background(A).Value = False
    Next A
    If Me.Visible = True Then FocusNinja.SetFocus
End Sub

Private Sub Form_Load()
    Dim A As Integer
    For A = 0 To optGame.Count - 1
        Game(A).Top = Game(0).Top
        Game(A).Left = Game(0).Left
    Next A
    Background(1).Value = True
    For A = 2 To Background.Count
        Background(A).Value = False
    Next A
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Cancel = 1
    frmLevelEditor.optCursor(13).Value = True
End Sub

Private Sub optGame_Click(Index As Integer)
    Dim A As Integer
    On Error Resume Next
    If Me.Visible = True Then FocusNinja.SetFocus
    For A = 0 To Game.Count - 1
        If A = Index Then
            Game(A).Visible = True
        Else
            Game(A).Visible = False
        End If
    Next A
End Sub
