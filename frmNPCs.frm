VERSION 5.00
Begin VB.Form frmNPCs 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Non-Player Characters"
   ClientHeight    =   4050
   ClientLeft      =   1.00050e5
   ClientTop       =   330
   ClientWidth     =   16155
   Icon            =   "frmNPCs.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   ScaleHeight     =   4050
   ScaleWidth      =   16155
   Visible         =   0   'False
   Begin VB.Frame Frame 
      Caption         =   "Lakitu"
      Height          =   550
      Index           =   28
      Left            =   120
      TabIndex        =   358
      Top             =   3420
      Width           =   1095
      Begin VB.CommandButton Lakitu 
         Caption         =   "No"
         Height          =   255
         Left            =   120
         TabIndex        =   359
         Top             =   220
         Width           =   855
      End
   End
   Begin VB.Frame Frame 
      Caption         =   "Bubble"
      Height          =   550
      Index           =   1
      Left            =   120
      TabIndex        =   356
      Top             =   2830
      Width           =   1095
      Begin VB.CommandButton Bubble 
         Caption         =   "No"
         Height          =   255
         Left            =   120
         TabIndex        =   357
         Top             =   220
         Width           =   855
      End
   End
   Begin VB.Frame Frame19 
      Caption         =   "Events"
      Height          =   615
      Left            =   14400
      TabIndex        =   287
      Top             =   0
      Width           =   1095
      Begin VB.CommandButton cmdEvents 
         Caption         =   "Show"
         Height          =   255
         Left            =   120
         TabIndex        =   288
         Top             =   240
         Width           =   855
      End
   End
   Begin VB.Frame Frame18 
      Caption         =   "Generator"
      Height          =   615
      Left            =   13200
      TabIndex        =   285
      Top             =   0
      Width           =   1095
      Begin VB.CommandButton cmdGenerator 
         Caption         =   "Show"
         Height          =   255
         Left            =   120
         TabIndex        =   286
         Top             =   240
         Width           =   855
      End
   End
   Begin VB.Frame Game 
      Caption         =   "Misc."
      Height          =   2775
      Index           =   4
      Left            =   -360
      TabIndex        =   132
      Top             =   9960
      Visible         =   0   'False
      Width           =   8055
      Begin VB.Frame Frame4 
         Caption         =   "Switch"
         Height          =   855
         Left            =   3480
         TabIndex        =   309
         Top             =   1800
         Width           =   2535
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   65
            Left            =   1920
            Picture         =   "frmNPCs.frx":0B3A
            Style           =   1  'Graphical
            TabIndex        =   313
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   63
            Left            =   1320
            Picture         =   "frmNPCs.frx":0F7A
            Style           =   1  'Graphical
            TabIndex        =   312
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   61
            Left            =   720
            Picture         =   "frmNPCs.frx":13BA
            Style           =   1  'Graphical
            TabIndex        =   311
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   555
            Index           =   59
            Left            =   120
            Picture         =   "frmNPCs.frx":17FA
            Style           =   1  'Graphical
            TabIndex        =   310
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Boss"
         Height          =   1575
         Index           =   27
         Left            =   7080
         TabIndex        =   260
         Top             =   240
         Width           =   855
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   209
            Left            =   120
            Picture         =   "frmNPCs.frx":1C3A
            Style           =   1  'Graphical
            TabIndex        =   262
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   208
            Left            =   120
            Picture         =   "frmNPCs.frx":2107
            Style           =   1  'Graphical
            TabIndex        =   261
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame7 
         Caption         =   "Items"
         Height          =   1575
         Left            =   3480
         TabIndex        =   193
         Top             =   240
         Width           =   2535
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   255
            Left            =   1920
            Picture         =   "frmNPCs.frx":2547
            Style           =   1  'Graphical
            TabIndex        =   326
            Top             =   840
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   254
            Left            =   1920
            Picture         =   "frmNPCs.frx":2977
            Style           =   1  'Graphical
            TabIndex        =   325
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   253
            Left            =   1320
            Picture         =   "frmNPCs.frx":2D9B
            Style           =   1  'Graphical
            TabIndex        =   324
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   252
            Left            =   1320
            Picture         =   "frmNPCs.frx":3153
            Style           =   1  'Graphical
            TabIndex        =   323
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   251
            Left            =   720
            Picture         =   "frmNPCs.frx":350B
            Style           =   1  'Graphical
            TabIndex        =   322
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   250
            Left            =   720
            Picture         =   "frmNPCs.frx":38C3
            Style           =   1  'Graphical
            TabIndex        =   321
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   158
            Left            =   120
            Picture         =   "frmNPCs.frx":3CE2
            Style           =   1  'Graphical
            TabIndex        =   201
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   152
            Left            =   120
            Picture         =   "frmNPCs.frx":40CC
            Style           =   1  'Graphical
            TabIndex        =   194
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame6 
         Caption         =   "Enemies"
         Height          =   2055
         Left            =   120
         TabIndex        =   160
         Top             =   240
         Width           =   3255
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   257
            Left            =   1320
            Picture         =   "frmNPCs.frx":44E0
            Style           =   1  'Graphical
            TabIndex        =   328
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   256
            Left            =   720
            Picture         =   "frmNPCs.frx":49B5
            Style           =   1  'Graphical
            TabIndex        =   327
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   243
            Left            =   720
            Picture         =   "frmNPCs.frx":4E87
            Style           =   1  'Graphical
            TabIndex        =   308
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   242
            Left            =   120
            Picture         =   "frmNPCs.frx":5380
            Style           =   1  'Graphical
            TabIndex        =   307
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   211
            Left            =   2520
            Picture         =   "frmNPCs.frx":57BC
            Style           =   1  'Graphical
            TabIndex        =   264
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   205
            Left            =   2520
            Picture         =   "frmNPCs.frx":5C0A
            Style           =   1  'Graphical
            TabIndex        =   257
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   204
            Left            =   1920
            Picture         =   "frmNPCs.frx":607D
            Style           =   1  'Graphical
            TabIndex        =   256
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   203
            Left            =   1320
            Picture         =   "frmNPCs.frx":64E0
            Style           =   1  'Graphical
            TabIndex        =   255
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   168
            Left            =   120
            Picture         =   "frmNPCs.frx":6A1F
            Style           =   1  'Graphical
            TabIndex        =   210
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   128
            Left            =   1320
            Picture         =   "frmNPCs.frx":6DF7
            Style           =   1  'Graphical
            TabIndex        =   164
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   127
            Left            =   120
            Picture         =   "frmNPCs.frx":7206
            Style           =   1  'Graphical
            TabIndex        =   163
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   126
            Left            =   720
            Picture         =   "frmNPCs.frx":7615
            Style           =   1  'Graphical
            TabIndex        =   162
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   125
            Left            =   1920
            Picture         =   "frmNPCs.frx":7A24
            Style           =   1  'Graphical
            TabIndex        =   161
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "People"
         Height          =   1575
         Index           =   26
         Left            =   6120
         TabIndex        =   133
         Top             =   240
         Width           =   855
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   107
            Left            =   120
            Picture         =   "frmNPCs.frx":7E2B
            Style           =   1  'Graphical
            TabIndex        =   141
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   102
            Left            =   120
            Picture         =   "frmNPCs.frx":8256
            Style           =   1  'Graphical
            TabIndex        =   134
            Top             =   240
            Width           =   540
         End
      End
   End
   Begin VB.Frame Frame1 
      Caption         =   "Advanced"
      Height          =   615
      Left            =   12000
      TabIndex        =   128
      Top             =   0
      Width           =   1095
      Begin VB.CommandButton cmdAdvanced 
         Caption         =   "Show"
         Height          =   255
         Left            =   120
         TabIndex        =   129
         Top             =   240
         Width           =   855
      End
   End
   Begin VB.Frame Lak 
      Caption         =   "The Lakitus are throwing Lakitus!"
      Height          =   1575
      Left            =   1320
      TabIndex        =   92
      Top             =   7560
      Visible         =   0   'False
      Width           =   9255
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   292
         Left            =   1320
         Picture         =   "frmNPCs.frx":8663
         Style           =   1  'Graphical
         TabIndex        =   371
         Top             =   960
         Value           =   -1  'True
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   291
         Left            =   1320
         Picture         =   "frmNPCs.frx":8A5A
         Style           =   1  'Graphical
         TabIndex        =   370
         Top             =   360
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   290
         Left            =   8520
         Picture         =   "frmNPCs.frx":8E3A
         Style           =   1  'Graphical
         TabIndex        =   369
         Top             =   360
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   284
         Left            =   7320
         Picture         =   "frmNPCs.frx":923C
         Style           =   1  'Graphical
         TabIndex        =   361
         Top             =   960
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   283
         Left            =   3120
         Picture         =   "frmNPCs.frx":963D
         Style           =   1  'Graphical
         TabIndex        =   360
         Top             =   360
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   282
         Left            =   4320
         Picture         =   "frmNPCs.frx":9A31
         Style           =   1  'Graphical
         TabIndex        =   355
         Top             =   960
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   281
         Left            =   7920
         Picture         =   "frmNPCs.frx":9E16
         Style           =   1  'Graphical
         TabIndex        =   354
         Top             =   960
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   276
         Left            =   4920
         Picture         =   "frmNPCs.frx":A21B
         Style           =   1  'Graphical
         TabIndex        =   347
         Top             =   960
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   269
         Left            =   5520
         Picture         =   "frmNPCs.frx":A5BE
         Style           =   1  'Graphical
         TabIndex        =   339
         Top             =   960
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   268
         Left            =   7920
         Picture         =   "frmNPCs.frx":A995
         Style           =   1  'Graphical
         TabIndex        =   338
         Top             =   360
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   265
         Left            =   120
         Picture         =   "frmNPCs.frx":ADF4
         Style           =   1  'Graphical
         TabIndex        =   337
         Top             =   960
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   266
         Left            =   720
         Picture         =   "frmNPCs.frx":B1AC
         Style           =   1  'Graphical
         TabIndex        =   336
         Top             =   360
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   263
         Left            =   2520
         Picture         =   "frmNPCs.frx":B531
         Style           =   1  'Graphical
         TabIndex        =   334
         Top             =   360
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   246
         Left            =   4920
         Picture         =   "frmNPCs.frx":BB3A
         Style           =   1  'Graphical
         TabIndex        =   316
         Top             =   360
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   237
         Left            =   1920
         Picture         =   "frmNPCs.frx":BEF0
         Style           =   1  'Graphical
         TabIndex        =   302
         Top             =   360
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   210
         Left            =   5520
         Picture         =   "frmNPCs.frx":C335
         Style           =   1  'Graphical
         TabIndex        =   263
         Top             =   360
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   202
         Left            =   6120
         Picture         =   "frmNPCs.frx":C77F
         Style           =   1  'Graphical
         TabIndex        =   254
         Top             =   360
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   171
         Left            =   720
         Picture         =   "frmNPCs.frx":CB8E
         Style           =   1  'Graphical
         TabIndex        =   227
         Top             =   960
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   159
         Left            =   3720
         Picture         =   "frmNPCs.frx":CF74
         Style           =   1  'Graphical
         TabIndex        =   212
         Top             =   960
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   108
         Left            =   1920
         Picture         =   "frmNPCs.frx":D3D2
         Style           =   1  'Graphical
         TabIndex        =   192
         Top             =   960
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   133
         Left            =   6720
         Picture         =   "frmNPCs.frx":D7D5
         Style           =   1  'Graphical
         TabIndex        =   169
         Top             =   360
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   55
         Left            =   7320
         Picture         =   "frmNPCs.frx":DB61
         Style           =   1  'Graphical
         TabIndex        =   155
         Top             =   360
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   30
         Left            =   6120
         Picture         =   "frmNPCs.frx":DF73
         Style           =   1  'Graphical
         TabIndex        =   99
         Top             =   960
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   40
         Left            =   6720
         Picture         =   "frmNPCs.frx":E35F
         Style           =   1  'Graphical
         TabIndex        =   98
         Top             =   960
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   50
         Left            =   2520
         Picture         =   "frmNPCs.frx":E73E
         Style           =   1  'Graphical
         TabIndex        =   97
         Top             =   960
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   85
         Left            =   4320
         Picture         =   "frmNPCs.frx":EB80
         Style           =   1  'Graphical
         TabIndex        =   96
         Top             =   360
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   87
         Left            =   3720
         Picture         =   "frmNPCs.frx":EF71
         Style           =   1  'Graphical
         TabIndex        =   95
         Top             =   360
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   91
         Left            =   3120
         Picture         =   "frmNPCs.frx":F374
         Style           =   1  'Graphical
         TabIndex        =   94
         Top             =   960
         Width           =   540
      End
      Begin VB.OptionButton NPC 
         BackColor       =   &H00000000&
         Height          =   540
         Index           =   13
         Left            =   120
         Picture         =   "frmNPCs.frx":F750
         Style           =   1  'Graphical
         TabIndex        =   93
         Top             =   360
         Width           =   540
      End
   End
   Begin VB.Frame Frame 
      Caption         =   "Egg"
      Height          =   550
      Index           =   4
      Left            =   120
      TabIndex        =   89
      Top             =   2280
      Width           =   1095
      Begin VB.CommandButton Egg 
         Caption         =   "No"
         Height          =   255
         Left            =   120
         TabIndex        =   90
         Top             =   220
         Width           =   855
      End
   End
   Begin VB.Frame Frame 
      Caption         =   "Don't Move"
      Height          =   550
      Index           =   6
      Left            =   120
      TabIndex        =   87
      Top             =   600
      Width           =   1095
      Begin VB.CommandButton DontMove 
         Caption         =   "No"
         Height          =   255
         Left            =   120
         TabIndex        =   88
         Top             =   220
         Width           =   855
      End
   End
   Begin VB.Frame Frame 
      Caption         =   "Friendly"
      Height          =   550
      Index           =   5
      Left            =   120
      TabIndex        =   85
      Top             =   1150
      Width           =   1095
      Begin VB.CommandButton Friendly 
         Caption         =   "No"
         Height          =   255
         Left            =   120
         TabIndex        =   86
         Top             =   220
         Width           =   855
      End
   End
   Begin VB.Frame Frame 
      Caption         =   "Message"
      Height          =   615
      Index           =   7
      Left            =   7680
      TabIndex        =   83
      Top             =   0
      Width           =   4215
      Begin VB.CheckBox chkMessage 
         Caption         =   "Test"
         Height          =   255
         Left            =   3600
         Style           =   1  'Graphical
         TabIndex        =   364
         Top             =   240
         Width           =   495
      End
      Begin VB.TextBox NPCText 
         Height          =   285
         Left            =   120
         TabIndex        =   84
         Top             =   240
         Width           =   3375
      End
   End
   Begin VB.Frame Frame 
      Caption         =   "Buried"
      Height          =   550
      Index           =   3
      Left            =   120
      TabIndex        =   79
      Top             =   1700
      Width           =   1095
      Begin VB.CommandButton Buried 
         Caption         =   "No"
         Height          =   255
         Left            =   120
         TabIndex        =   80
         Top             =   220
         Width           =   855
      End
   End
   Begin VB.PictureBox FocusNinja 
      Height          =   375
      Left            =   0
      ScaleHeight     =   315
      ScaleWidth      =   315
      TabIndex        =   0
      Top             =   12480
      Width           =   375
   End
   Begin VB.Frame Game 
      Caption         =   "Super Mario Bros."
      Height          =   3015
      Index           =   3
      Left            =   8760
      TabIndex        =   31
      Top             =   10920
      Visible         =   0   'False
      Width           =   7455
      Begin VB.Frame Frame22 
         Caption         =   "Sushi"
         Height          =   2295
         Left            =   6120
         TabIndex        =   297
         Top             =   240
         Width           =   855
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   555
            Index           =   28
            Left            =   120
            Picture         =   "frmNPCs.frx":FB06
            Style           =   1  'Graphical
            TabIndex        =   300
            Top             =   1560
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   233
            Left            =   120
            Picture         =   "frmNPCs.frx":10748
            Style           =   1  'Graphical
            TabIndex        =   299
            Top             =   960
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   235
            Left            =   120
            Picture         =   "frmNPCs.frx":10B92
            Style           =   1  'Graphical
            TabIndex        =   298
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame16 
         Caption         =   "Vines"
         Height          =   1695
         Left            =   4440
         TabIndex        =   270
         Top             =   1200
         Width           =   855
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   223
            Left            =   120
            Picture         =   "frmNPCs.frx":10FA0
            Style           =   1  'Graphical
            TabIndex        =   280
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   222
            Left            =   120
            Picture         =   "frmNPCs.frx":11376
            Style           =   1  'Graphical
            TabIndex        =   279
            Top             =   960
            Value           =   -1  'True
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Boss"
         Height          =   975
         Index           =   0
         Left            =   4440
         TabIndex        =   251
         Top             =   240
         Width           =   855
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   200
            Left            =   120
            Picture         =   "frmNPCs.frx":1178F
            Style           =   1  'Graphical
            TabIndex        =   252
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame12 
         Caption         =   "Shells"
         Height          =   1695
         Left            =   3480
         TabIndex        =   224
         Top             =   1200
         Width           =   855
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   555
            Index           =   174
            Left            =   120
            Picture         =   "frmNPCs.frx":11C2A
            Style           =   1  'Graphical
            TabIndex        =   226
            Top             =   960
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   555
            Index           =   172
            Left            =   120
            Picture         =   "frmNPCs.frx":12038
            Style           =   1  'Graphical
            TabIndex        =   225
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame3 
         Caption         =   "Platforms"
         Height          =   975
         Left            =   3480
         TabIndex        =   138
         Top             =   240
         Width           =   855
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   106
            Left            =   120
            Picture         =   "frmNPCs.frx":12446
            Style           =   1  'Graphical
            TabIndex        =   140
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Items"
         Height          =   975
         Index           =   25
         Left            =   120
         TabIndex        =   121
         Top             =   1920
         Width           =   3255
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   186
            Left            =   2520
            Picture         =   "frmNPCs.frx":127A5
            Style           =   1  'Graphical
            TabIndex        =   231
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   182
            Left            =   720
            Picture         =   "frmNPCs.frx":12BE5
            Style           =   1  'Graphical
            TabIndex        =   230
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   184
            Left            =   120
            Picture         =   "frmNPCs.frx":1301C
            Style           =   1  'Graphical
            TabIndex        =   229
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   178
            Left            =   1920
            Picture         =   "frmNPCs.frx":1345A
            Style           =   1  'Graphical
            TabIndex        =   228
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   88
            Left            =   1320
            Picture         =   "frmNPCs.frx":13887
            Style           =   1  'Graphical
            TabIndex        =   122
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Enemies"
         Height          =   1575
         Index           =   24
         Left            =   120
         TabIndex        =   32
         Top             =   240
         Width           =   3255
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   260
            Left            =   2520
            Picture         =   "frmNPCs.frx":13C5B
            Style           =   1  'Graphical
            TabIndex        =   331
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   555
            Index           =   177
            Left            =   720
            Picture         =   "frmNPCs.frx":13FDE
            Style           =   1  'Graphical
            TabIndex        =   223
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   555
            Index           =   176
            Left            =   1920
            Picture         =   "frmNPCs.frx":1441B
            Style           =   1  'Graphical
            TabIndex        =   222
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   555
            Index           =   175
            Left            =   120
            Picture         =   "frmNPCs.frx":14858
            Style           =   1  'Graphical
            TabIndex        =   221
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   555
            Index           =   173
            Left            =   1320
            Picture         =   "frmNPCs.frx":14C6E
            Style           =   1  'Graphical
            TabIndex        =   220
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   555
            Index           =   153
            Left            =   1920
            Picture         =   "frmNPCs.frx":15094
            Style           =   1  'Graphical
            TabIndex        =   195
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   93
            Left            =   2520
            Picture         =   "frmNPCs.frx":154E1
            Style           =   1  'Graphical
            TabIndex        =   81
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   555
            Index           =   89
            Left            =   120
            Picture         =   "frmNPCs.frx":15915
            Style           =   1  'Graphical
            TabIndex        =   76
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   29
            Left            =   1320
            Picture         =   "frmNPCs.frx":15D80
            Style           =   1  'Graphical
            TabIndex        =   37
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   555
            Index           =   27
            Left            =   720
            Picture         =   "frmNPCs.frx":161B9
            Style           =   1  'Graphical
            TabIndex        =   33
            Top             =   240
            Width           =   540
         End
      End
   End
   Begin VB.Frame Game 
      Caption         =   "Super Mario World"
      Height          =   3375
      Index           =   2
      Left            =   1320
      TabIndex        =   25
      Top             =   4080
      Visible         =   0   'False
      Width           =   14775
      Begin VB.Frame Frame23 
         Caption         =   "Boss"
         Height          =   855
         Left            =   13920
         TabIndex        =   352
         Top             =   240
         Width           =   735
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   280
            Left            =   120
            Picture         =   "frmNPCs.frx":16DFB
            Style           =   1  'Graphical
            TabIndex        =   353
            Top             =   240
            Value           =   -1  'True
            Width           =   540
         End
      End
      Begin VB.Frame Frame21 
         Caption         =   "Sushi"
         Height          =   2055
         Left            =   13200
         TabIndex        =   294
         Top             =   240
         Width           =   735
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   236
            Left            =   120
            Picture         =   "frmNPCs.frx":1723F
            Style           =   1  'Graphical
            TabIndex        =   301
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   232
            Left            =   120
            Picture         =   "frmNPCs.frx":17667
            Style           =   1  'Graphical
            TabIndex        =   296
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   234
            Left            =   120
            Picture         =   "frmNPCs.frx":17A82
            Style           =   1  'Graphical
            TabIndex        =   295
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame15 
         Caption         =   "Vines"
         Height          =   1575
         Left            =   12480
         TabIndex        =   269
         Top             =   240
         Width           =   735
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   227
            Left            =   120
            Picture         =   "frmNPCs.frx":17E87
            Style           =   1  'Graphical
            TabIndex        =   284
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   224
            Left            =   120
            Picture         =   "frmNPCs.frx":182A3
            Style           =   1  'Graphical
            TabIndex        =   278
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame Frame13 
         Caption         =   "Check"
         Height          =   1095
         Left            =   11520
         TabIndex        =   243
         Top             =   1320
         Width           =   855
         Begin VB.OptionButton NPC 
            Height          =   540
            Index           =   192
            Left            =   120
            Picture         =   "frmNPCs.frx":18698
            Style           =   1  'Graphical
            TabIndex        =   244
            Top             =   360
            Width           =   540
         End
      End
      Begin VB.Frame Frame9 
         Caption         =   "Signs"
         Height          =   1095
         Left            =   10680
         TabIndex        =   190
         Top             =   1320
         Width           =   735
         Begin VB.OptionButton NPC 
            Height          =   540
            Index           =   151
            Left            =   120
            Picture         =   "frmNPCs.frx":18A09
            Style           =   1  'Graphical
            TabIndex        =   191
            Top             =   360
            Width           =   540
         End
      End
      Begin VB.Frame Frame5 
         Caption         =   "Koopa Troopas"
         Height          =   2175
         Left            =   5160
         TabIndex        =   142
         Top             =   240
         Width           =   3735
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   194
            Left            =   2520
            Picture         =   "frmNPCs.frx":18E12
            Style           =   1  'Graphical
            TabIndex        =   246
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   124
            Left            =   3120
            Picture         =   "frmNPCs.frx":19247
            Style           =   1  'Graphical
            TabIndex        =   159
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   123
            Left            =   3120
            Picture         =   "frmNPCs.frx":19687
            Style           =   1  'Graphical
            TabIndex        =   158
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   122
            Left            =   2520
            Picture         =   "frmNPCs.frx":19AC0
            Style           =   1  'Graphical
            TabIndex        =   157
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   121
            Left            =   2520
            Picture         =   "frmNPCs.frx":19F00
            Style           =   1  'Graphical
            TabIndex        =   156
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   120
            Left            =   1920
            Picture         =   "frmNPCs.frx":1A340
            Style           =   1  'Graphical
            TabIndex        =   154
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   119
            Left            =   1320
            Picture         =   "frmNPCs.frx":1A755
            Style           =   1  'Graphical
            TabIndex        =   153
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   118
            Left            =   720
            Picture         =   "frmNPCs.frx":1AB6A
            Style           =   1  'Graphical
            TabIndex        =   152
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   117
            Left            =   120
            Picture         =   "frmNPCs.frx":1AF7F
            Style           =   1  'Graphical
            TabIndex        =   151
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   116
            Left            =   1920
            Picture         =   "frmNPCs.frx":1B394
            Style           =   1  'Graphical
            TabIndex        =   150
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   115
            Left            =   1320
            Picture         =   "frmNPCs.frx":1B7A3
            Style           =   1  'Graphical
            TabIndex        =   149
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   114
            Left            =   720
            Picture         =   "frmNPCs.frx":1BBB2
            Style           =   1  'Graphical
            TabIndex        =   148
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   113
            Left            =   120
            Picture         =   "frmNPCs.frx":1BFC1
            Style           =   1  'Graphical
            TabIndex        =   147
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   112
            Left            =   1920
            Picture         =   "frmNPCs.frx":1C3D0
            Style           =   1  'Graphical
            TabIndex        =   146
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   111
            Left            =   1320
            Picture         =   "frmNPCs.frx":1C7BE
            Style           =   1  'Graphical
            TabIndex        =   145
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   110
            Left            =   720
            Picture         =   "frmNPCs.frx":1CBAC
            Style           =   1  'Graphical
            TabIndex        =   144
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   109
            Left            =   120
            Picture         =   "frmNPCs.frx":1CF9A
            Style           =   1  'Graphical
            TabIndex        =   143
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Yoshi"
         Height          =   855
         Index           =   18
         Left            =   120
         TabIndex        =   123
         Top             =   2400
         Width           =   4935
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   228
            Left            =   4320
            Picture         =   "frmNPCs.frx":1D388
            Style           =   1  'Graphical
            TabIndex        =   289
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   150
            Left            =   3720
            Picture         =   "frmNPCs.frx":1D781
            Style           =   1  'Graphical
            TabIndex        =   189
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   149
            Left            =   3120
            Picture         =   "frmNPCs.frx":1DB8B
            Style           =   1  'Graphical
            TabIndex        =   188
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   148
            Left            =   2520
            Picture         =   "frmNPCs.frx":1DF95
            Style           =   1  'Graphical
            TabIndex        =   187
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   95
            Left            =   120
            Picture         =   "frmNPCs.frx":1E39F
            Style           =   1  'Graphical
            TabIndex        =   127
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   98
            Left            =   720
            Picture         =   "frmNPCs.frx":1E7A9
            Style           =   1  'Graphical
            TabIndex        =   126
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   99
            Left            =   1320
            Picture         =   "frmNPCs.frx":1EBB3
            Style           =   1  'Graphical
            TabIndex        =   125
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   100
            Left            =   1920
            Picture         =   "frmNPCs.frx":1EFBD
            Style           =   1  'Graphical
            TabIndex        =   124
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Platforms"
         Height          =   2175
         Index           =   19
         Left            =   9000
         TabIndex        =   60
         Top             =   240
         Width           =   1335
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   190
            Left            =   720
            Picture         =   "frmNPCs.frx":1F3C7
            Style           =   1  'Graphical
            TabIndex        =   240
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   105
            Left            =   120
            Picture         =   "frmNPCs.frx":1F7D2
            Style           =   1  'Graphical
            TabIndex        =   139
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   66
            Left            =   720
            Picture         =   "frmNPCs.frx":1FB55
            Style           =   1  'Graphical
            TabIndex        =   64
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   64
            Left            =   720
            Picture         =   "frmNPCs.frx":1FF6C
            Style           =   1  'Graphical
            TabIndex        =   63
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   62
            Left            =   120
            Picture         =   "frmNPCs.frx":20382
            Style           =   1  'Graphical
            TabIndex        =   62
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   60
            Left            =   120
            Picture         =   "frmNPCs.frx":20799
            Style           =   1  'Graphical
            TabIndex        =   61
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame10 
         Caption         =   "Exit"
         Height          =   975
         Left            =   10440
         TabIndex        =   38
         Top             =   240
         Width           =   1935
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   196
            Left            =   1320
            Picture         =   "frmNPCs.frx":20BAB
            Style           =   1  'Graphical
            TabIndex        =   350
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   197
            Left            =   720
            Picture         =   "frmNPCs.frx":20F65
            Style           =   1  'Graphical
            TabIndex        =   248
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            Height          =   540
            Index           =   31
            Left            =   120
            Picture         =   "frmNPCs.frx":212E3
            Style           =   1  'Graphical
            TabIndex        =   39
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Items"
         Height          =   855
         Index           =   17
         Left            =   5040
         TabIndex        =   35
         Top             =   2400
         Width           =   9615
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   279
            Left            =   9000
            Picture         =   "frmNPCs.frx":21F25
            Style           =   1  'Graphical
            TabIndex        =   351
            Top             =   240
            Value           =   -1  'True
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   278
            Left            =   8400
            Picture         =   "frmNPCs.frx":2232F
            Style           =   1  'Graphical
            TabIndex        =   349
            Top             =   240
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   277
            Left            =   7800
            Picture         =   "frmNPCs.frx":227E6
            Style           =   1  'Graphical
            TabIndex        =   348
            Top             =   240
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   274
            Left            =   7200
            Picture         =   "frmNPCs.frx":22BE1
            Style           =   1  'Graphical
            TabIndex        =   345
            Top             =   240
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   258
            Left            =   3000
            Picture         =   "frmNPCs.frx":230F7
            Style           =   1  'Graphical
            TabIndex        =   329
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   239
            Left            =   6600
            Picture         =   "frmNPCs.frx":234F0
            Style           =   1  'Graphical
            TabIndex        =   303
            Top             =   240
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   195
            Left            =   5400
            Picture         =   "frmNPCs.frx":238FC
            Style           =   1  'Graphical
            TabIndex        =   247
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   188
            Left            =   1800
            Picture         =   "frmNPCs.frx":23D2F
            Style           =   1  'Graphical
            TabIndex        =   235
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   187
            Left            =   1200
            Picture         =   "frmNPCs.frx":240E7
            Style           =   1  'Graphical
            TabIndex        =   234
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   183
            Left            =   600
            Picture         =   "frmNPCs.frx":244E8
            Style           =   1  'Graphical
            TabIndex        =   233
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   185
            Left            =   0
            Picture         =   "frmNPCs.frx":248E3
            Style           =   1  'Graphical
            TabIndex        =   232
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   96
            Left            =   3600
            Picture         =   "frmNPCs.frx":24CE4
            Style           =   1  'Graphical
            TabIndex        =   91
            Top             =   240
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   56
            Left            =   4800
            Picture         =   "frmNPCs.frx":250D6
            Style           =   1  'Graphical
            TabIndex        =   59
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   33
            Left            =   2400
            Picture         =   "frmNPCs.frx":2553D
            Style           =   1  'Graphical
            TabIndex        =   41
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   32
            Left            =   6000
            Picture         =   "frmNPCs.frx":25936
            Style           =   1  'Graphical
            TabIndex        =   40
            Top             =   240
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   26
            Left            =   4200
            Picture         =   "frmNPCs.frx":25D52
            Style           =   1  'Graphical
            TabIndex        =   36
            Top             =   240
            Width           =   555
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Enemies"
         Height          =   2175
         Index           =   16
         Left            =   120
         TabIndex        =   26
         Top             =   240
         Width           =   4935
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   286
            Left            =   3720
            Picture         =   "frmNPCs.frx":26994
            Style           =   1  'Graphical
            TabIndex        =   363
            Top             =   240
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   285
            Left            =   3720
            Picture         =   "frmNPCs.frx":26D9F
            Style           =   1  'Graphical
            TabIndex        =   362
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   275
            Left            =   3720
            Picture         =   "frmNPCs.frx":271FE
            Style           =   1  'Graphical
            TabIndex        =   346
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   271
            Left            =   4320
            Picture         =   "frmNPCs.frx":275F3
            Style           =   1  'Graphical
            TabIndex        =   341
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   270
            Left            =   3120
            Picture         =   "frmNPCs.frx":27A46
            Style           =   1  'Graphical
            TabIndex        =   340
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   207
            Left            =   3120
            Picture         =   "frmNPCs.frx":27F90
            Style           =   1  'Graphical
            TabIndex        =   259
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   199
            Left            =   3120
            Picture         =   "frmNPCs.frx":283B9
            Style           =   1  'Graphical
            TabIndex        =   250
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   189
            Left            =   2520
            Picture         =   "frmNPCs.frx":288DE
            Style           =   1  'Graphical
            TabIndex        =   239
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   179
            Left            =   2520
            Picture         =   "frmNPCs.frx":28CF3
            Style           =   1  'Graphical
            TabIndex        =   238
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   181
            Left            =   2520
            Picture         =   "frmNPCs.frx":2911D
            Style           =   1  'Graphical
            TabIndex        =   237
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   180
            Left            =   1920
            Picture         =   "frmNPCs.frx":2954A
            Style           =   1  'Graphical
            TabIndex        =   236
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   167
            Left            =   120
            Picture         =   "frmNPCs.frx":29995
            Style           =   1  'Graphical
            TabIndex        =   209
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   166
            Left            =   120
            Picture         =   "frmNPCs.frx":29DAC
            Style           =   1  'Graphical
            TabIndex        =   208
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   165
            Left            =   120
            Picture         =   "frmNPCs.frx":2A1CC
            Style           =   1  'Graphical
            TabIndex        =   207
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   164
            Left            =   1920
            Picture         =   "frmNPCs.frx":2A5E4
            Style           =   1  'Graphical
            TabIndex        =   206
            Top             =   840
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   163
            Left            =   720
            Picture         =   "frmNPCs.frx":2AA0B
            Style           =   1  'Graphical
            TabIndex        =   205
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   162
            Left            =   720
            Picture         =   "frmNPCs.frx":2AE43
            Style           =   1  'Graphical
            TabIndex        =   204
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   77
            Left            =   720
            Picture         =   "frmNPCs.frx":2B23A
            Style           =   1  'Graphical
            TabIndex        =   74
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   43
            Left            =   1320
            Picture         =   "frmNPCs.frx":2B61A
            Style           =   1  'Graphical
            TabIndex        =   54
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   42
            Left            =   1320
            Picture         =   "frmNPCs.frx":2BA2F
            Style           =   1  'Graphical
            TabIndex        =   53
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   44
            Left            =   1320
            Picture         =   "frmNPCs.frx":2BE55
            Style           =   1  'Graphical
            TabIndex        =   52
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   18
            Left            =   1920
            Picture         =   "frmNPCs.frx":2C28D
            Style           =   1  'Graphical
            TabIndex        =   27
            Top             =   240
            Width           =   540
         End
      End
   End
   Begin VB.Frame Game 
      Caption         =   "Super Mario Brothers 2"
      Height          =   2775
      Index           =   1
      Left            =   10680
      TabIndex        =   21
      Top             =   7560
      Visible         =   0   'False
      Width           =   10695
      Begin VB.Frame Frame24 
         Caption         =   "Warps"
         Height          =   975
         Left            =   9240
         TabIndex        =   366
         Top             =   1680
         Width           =   1335
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   289
            Left            =   720
            Picture         =   "frmNPCs.frx":2CECF
            Style           =   1  'Graphical
            TabIndex        =   368
            Top             =   240
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   288
            Left            =   120
            Picture         =   "frmNPCs.frx":2D31A
            Style           =   1  'Graphical
            TabIndex        =   367
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame17 
         Caption         =   "Vines"
         Height          =   1455
         Left            =   7440
         TabIndex        =   271
         Top             =   240
         Width           =   2655
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   221
            Left            =   1920
            Picture         =   "frmNPCs.frx":2D42F
            Style           =   1  'Graphical
            TabIndex        =   281
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   216
            Left            =   1320
            MaskColor       =   &H00808080&
            Picture         =   "frmNPCs.frx":2D831
            Style           =   1  'Graphical
            TabIndex        =   277
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   219
            Left            =   1320
            Picture         =   "frmNPCs.frx":2DC45
            Style           =   1  'Graphical
            TabIndex        =   276
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   215
            Left            =   720
            Picture         =   "frmNPCs.frx":2E06D
            Style           =   1  'Graphical
            TabIndex        =   275
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   218
            Left            =   720
            Picture         =   "frmNPCs.frx":2E481
            Style           =   1  'Graphical
            TabIndex        =   274
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   217
            Left            =   120
            Picture         =   "frmNPCs.frx":2E8A9
            Style           =   1  'Graphical
            TabIndex        =   273
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   220
            Left            =   120
            Picture         =   "frmNPCs.frx":2ECBD
            Style           =   1  'Graphical
            TabIndex        =   272
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame Frame11 
         Caption         =   "Blocks"
         Height          =   975
         Left            =   120
         TabIndex        =   196
         Top             =   1680
         Width           =   2655
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   157
            Left            =   1920
            Picture         =   "frmNPCs.frx":2F0E5
            Style           =   1  'Graphical
            TabIndex        =   200
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   156
            Left            =   1320
            Picture         =   "frmNPCs.frx":2F516
            Style           =   1  'Graphical
            TabIndex        =   199
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   155
            Left            =   720
            Picture         =   "frmNPCs.frx":2F948
            Style           =   1  'Graphical
            TabIndex        =   198
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   154
            Left            =   120
            Picture         =   "frmNPCs.frx":2FD67
            Style           =   1  'Graphical
            TabIndex        =   197
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame8 
         Caption         =   "Veggies"
         Height          =   1455
         Left            =   4080
         TabIndex        =   175
         Top             =   240
         Width           =   3255
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   147
            Left            =   120
            Picture         =   "frmNPCs.frx":30192
            Style           =   1  'Graphical
            TabIndex        =   185
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   146
            Left            =   2520
            Picture         =   "frmNPCs.frx":3056A
            Style           =   1  'Graphical
            TabIndex        =   184
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   145
            Left            =   120
            Picture         =   "frmNPCs.frx":30979
            Style           =   1  'Graphical
            TabIndex        =   183
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   144
            Left            =   720
            Picture         =   "frmNPCs.frx":30D61
            Style           =   1  'Graphical
            TabIndex        =   182
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   143
            Left            =   1920
            Picture         =   "frmNPCs.frx":3114A
            Style           =   1  'Graphical
            TabIndex        =   181
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   141
            Left            =   1320
            Picture         =   "frmNPCs.frx":314F5
            Style           =   1  'Graphical
            TabIndex        =   180
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   140
            Left            =   1320
            Picture         =   "frmNPCs.frx":31905
            Style           =   1  'Graphical
            TabIndex        =   179
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   139
            Left            =   2520
            Picture         =   "frmNPCs.frx":31D3E
            Style           =   1  'Graphical
            TabIndex        =   178
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   142
            Left            =   1920
            Picture         =   "frmNPCs.frx":32154
            Style           =   1  'Graphical
            TabIndex        =   177
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   92
            Left            =   720
            Picture         =   "frmNPCs.frx":32570
            Style           =   1  'Graphical
            TabIndex        =   176
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Items"
         Height          =   975
         Index           =   23
         Left            =   2880
         TabIndex        =   78
         Top             =   1680
         Width           =   3135
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   249
            Left            =   2520
            Picture         =   "frmNPCs.frx":329D2
            Style           =   1  'Graphical
            TabIndex        =   320
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   241
            Left            =   1320
            Picture         =   "frmNPCs.frx":32E0E
            Style           =   1  'Graphical
            TabIndex        =   305
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   240
            Left            =   1920
            Picture         =   "frmNPCs.frx":3321E
            Style           =   1  'Graphical
            TabIndex        =   304
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   138
            Left            =   120
            Picture         =   "frmNPCs.frx":3363D
            Style           =   1  'Graphical
            TabIndex        =   174
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   134
            Left            =   720
            Picture         =   "frmNPCs.frx":33A5F
            Style           =   1  'Graphical
            TabIndex        =   170
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Exit"
         Height          =   975
         Index           =   20
         Left            =   6120
         TabIndex        =   50
         Top             =   1680
         Width           =   855
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   41
            Left            =   120
            Picture         =   "frmNPCs.frx":33E3B
            Style           =   1  'Graphical
            TabIndex        =   51
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Boss"
         Height          =   975
         Index           =   21
         Left            =   7080
         TabIndex        =   48
         Top             =   1680
         Width           =   2055
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   262
            Left            =   1320
            Picture         =   "frmNPCs.frx":34254
            Style           =   1  'Graphical
            TabIndex        =   333
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   201
            Left            =   720
            Picture         =   "frmNPCs.frx":34871
            Style           =   1  'Graphical
            TabIndex        =   253
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   39
            Left            =   120
            Picture         =   "frmNPCs.frx":34ECE
            Style           =   1  'Graphical
            TabIndex        =   49
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Enemies"
         Height          =   1455
         Index           =   22
         Left            =   120
         TabIndex        =   22
         Top             =   240
         Width           =   3855
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   272
            Left            =   3120
            Picture         =   "frmNPCs.frx":352E1
            Style           =   1  'Graphical
            TabIndex        =   342
            Top             =   240
            Value           =   -1  'True
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   247
            Left            =   2520
            Picture         =   "frmNPCs.frx":35777
            Style           =   1  'Graphical
            TabIndex        =   315
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   206
            Left            =   2520
            Picture         =   "frmNPCs.frx":35BBD
            Style           =   1  'Graphical
            TabIndex        =   258
            Top             =   240
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   135
            Left            =   1920
            Picture         =   "frmNPCs.frx":35FE2
            Style           =   1  'Graphical
            TabIndex        =   171
            Top             =   840
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   132
            Left            =   1920
            Picture         =   "frmNPCs.frx":36455
            Style           =   1  'Graphical
            TabIndex        =   168
            Top             =   240
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   131
            Left            =   1320
            Picture         =   "frmNPCs.frx":368A5
            Style           =   1  'Graphical
            TabIndex        =   167
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   130
            Left            =   1320
            Picture         =   "frmNPCs.frx":36CF5
            Style           =   1  'Graphical
            TabIndex        =   166
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   129
            Left            =   720
            Picture         =   "frmNPCs.frx":37145
            Style           =   1  'Graphical
            TabIndex        =   165
            Top             =   840
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   25
            Left            =   720
            Picture         =   "frmNPCs.frx":375B6
            Style           =   1  'Graphical
            TabIndex        =   30
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   19
            Left            =   120
            Picture         =   "frmNPCs.frx":381F8
            Style           =   1  'Graphical
            TabIndex        =   24
            Top             =   840
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   20
            Left            =   120
            Picture         =   "frmNPCs.frx":38D7A
            Style           =   1  'Graphical
            TabIndex        =   23
            Top             =   240
            Width           =   540
         End
      End
   End
   Begin VB.Frame Game 
      Caption         =   "Super Mario Brothers 3"
      Height          =   3375
      Index           =   0
      Left            =   1320
      TabIndex        =   1
      Top             =   600
      Width           =   14775
      Begin VB.Frame Frame20 
         Caption         =   "Sushi"
         Height          =   2055
         Left            =   12480
         TabIndex        =   290
         Top             =   240
         Width           =   735
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   231
            Left            =   120
            Picture         =   "frmNPCs.frx":398FC
            Style           =   1  'Graphical
            TabIndex        =   293
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   230
            Left            =   120
            Picture         =   "frmNPCs.frx":39D3D
            Style           =   1  'Graphical
            TabIndex        =   292
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   229
            Left            =   120
            Picture         =   "frmNPCs.frx":39E87
            Style           =   1  'Graphical
            TabIndex        =   291
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame14 
         Caption         =   "Vines"
         Height          =   855
         Left            =   5040
         TabIndex        =   266
         Top             =   1560
         Width           =   2535
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   226
            Left            =   120
            Picture         =   "frmNPCs.frx":3A2D6
            Style           =   1  'Graphical
            TabIndex        =   283
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   225
            Left            =   1320
            Picture         =   "frmNPCs.frx":3A432
            Style           =   1  'Graphical
            TabIndex        =   282
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   214
            Left            =   1920
            Picture         =   "frmNPCs.frx":3A895
            Style           =   1  'Graphical
            TabIndex        =   268
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   213
            Left            =   720
            Picture         =   "frmNPCs.frx":3AE73
            Style           =   1  'Graphical
            TabIndex        =   267
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Exit"
         Height          =   855
         Index           =   15
         Left            =   11280
         TabIndex        =   214
         Top             =   2400
         Width           =   1935
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   11
            Left            =   120
            Picture         =   "frmNPCs.frx":3B28C
            Style           =   1  'Graphical
            TabIndex        =   217
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   16
            Left            =   720
            Picture         =   "frmNPCs.frx":3B70E
            Style           =   1  'Graphical
            TabIndex        =   216
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   97
            Left            =   1320
            Picture         =   "frmNPCs.frx":3C350
            Style           =   1  'Graphical
            TabIndex        =   215
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame2 
         Caption         =   "Platforms"
         Height          =   1455
         Left            =   13320
         TabIndex        =   136
         Top             =   1800
         Width           =   1335
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   212
            Left            =   720
            Picture         =   "frmNPCs.frx":3CF92
            Style           =   1  'Graphical
            TabIndex        =   265
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   57
            Left            =   720
            Picture         =   "frmNPCs.frx":3D385
            Style           =   1  'Graphical
            TabIndex        =   219
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   46
            Left            =   120
            Picture         =   "frmNPCs.frx":3D75E
            Style           =   1  'Graphical
            TabIndex        =   218
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   104
            Left            =   120
            Picture         =   "frmNPCs.frx":3DB51
            Style           =   1  'Graphical
            TabIndex        =   137
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "People"
         Height          =   1455
         Index           =   13
         Left            =   13320
         TabIndex        =   117
         Top             =   240
         Width           =   1335
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   198
            Left            =   720
            Picture         =   "frmNPCs.frx":3DF15
            Style           =   1  'Graphical
            TabIndex        =   249
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   101
            Left            =   720
            Picture         =   "frmNPCs.frx":3E308
            Style           =   1  'Graphical
            TabIndex        =   130
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   75
            Left            =   120
            Picture         =   "frmNPCs.frx":3E78C
            Style           =   1  'Graphical
            TabIndex        =   119
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   94
            Left            =   120
            Picture         =   "frmNPCs.frx":3ECF8
            Style           =   1  'Graphical
            TabIndex        =   118
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Plants"
         Height          =   2055
         Index           =   10
         Left            =   11040
         TabIndex        =   105
         Top             =   240
         Width           =   1335
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   261
            Left            =   720
            Picture         =   "frmNPCs.frx":3F269
            Style           =   1  'Graphical
            TabIndex        =   332
            Top             =   1440
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   245
            Left            =   120
            Picture         =   "frmNPCs.frx":3F6B7
            Style           =   1  'Graphical
            TabIndex        =   314
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   74
            Left            =   720
            Picture         =   "frmNPCs.frx":3FACE
            Style           =   1  'Graphical
            TabIndex        =   120
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   52
            Left            =   120
            Picture         =   "frmNPCs.frx":40117
            Style           =   1  'Graphical
            TabIndex        =   108
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   8
            Left            =   120
            Picture         =   "frmNPCs.frx":40658
            Style           =   1  'Graphical
            TabIndex        =   107
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   51
            Left            =   720
            Picture         =   "frmNPCs.frx":40C15
            Style           =   1  'Graphical
            TabIndex        =   106
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Shells"
         Height          =   1455
         Index           =   14
         Left            =   6360
         TabIndex        =   100
         Top             =   120
         Width           =   1335
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   5
            Left            =   720
            Picture         =   "frmNPCs.frx":41032
            Style           =   1  'Graphical
            TabIndex        =   104
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   7
            Left            =   120
            Picture         =   "frmNPCs.frx":41C74
            Style           =   1  'Graphical
            TabIndex        =   103
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   24
            Left            =   720
            Picture         =   "frmNPCs.frx":428B6
            Style           =   1  'Graphical
            TabIndex        =   102
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   73
            Left            =   120
            Picture         =   "frmNPCs.frx":434F8
            Style           =   1  'Graphical
            TabIndex        =   101
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "NPC Blocks"
         Height          =   2175
         Index           =   11
         Left            =   7800
         TabIndex        =   65
         Top             =   240
         Width           =   3135
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   45
            Left            =   2520
            Picture         =   "frmNPCs.frx":43AED
            Style           =   1  'Graphical
            TabIndex        =   241
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   160
            Left            =   120
            Picture         =   "frmNPCs.frx":43EEF
            Style           =   1  'Graphical
            TabIndex        =   202
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   84
            Left            =   720
            Picture         =   "frmNPCs.frx":4427E
            Style           =   1  'Graphical
            TabIndex        =   116
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   21
            Left            =   120
            Picture         =   "frmNPCs.frx":4467A
            Style           =   1  'Graphical
            TabIndex        =   115
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   79
            Left            =   1320
            Picture         =   "frmNPCs.frx":452BC
            Style           =   1  'Graphical
            TabIndex        =   114
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   80
            Left            =   720
            Picture         =   "frmNPCs.frx":45696
            Style           =   1  'Graphical
            TabIndex        =   113
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   83
            Left            =   720
            Picture         =   "frmNPCs.frx":45A75
            Style           =   1  'Graphical
            TabIndex        =   112
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   82
            Left            =   1320
            Picture         =   "frmNPCs.frx":45E83
            Style           =   1  'Graphical
            TabIndex        =   111
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   78
            Left            =   1920
            Picture         =   "frmNPCs.frx":46260
            Style           =   1  'Graphical
            TabIndex        =   110
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   81
            Left            =   120
            Picture         =   "frmNPCs.frx":46614
            Style           =   1  'Graphical
            TabIndex        =   109
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   70
            Left            =   2520
            Picture         =   "frmNPCs.frx":469F6
            Style           =   1  'Graphical
            TabIndex        =   70
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   69
            Left            =   1920
            Picture         =   "frmNPCs.frx":46D55
            Style           =   1  'Graphical
            TabIndex        =   69
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   68
            Left            =   2520
            Picture         =   "frmNPCs.frx":470ED
            Style           =   1  'Graphical
            TabIndex        =   68
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   58
            Left            =   1320
            Picture         =   "frmNPCs.frx":47450
            Style           =   1  'Graphical
            TabIndex        =   67
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   67
            Left            =   1920
            Picture         =   "frmNPCs.frx":4783E
            Style           =   1  'Graphical
            TabIndex        =   66
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Boss"
         Height          =   1455
         Index           =   9
         Left            =   5040
         TabIndex        =   46
         Top             =   120
         Width           =   1245
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   267
            Left            =   600
            Picture         =   "frmNPCs.frx":47BE1
            Style           =   1  'Graphical
            TabIndex        =   343
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   86
            Left            =   40
            Picture         =   "frmNPCs.frx":480A9
            Style           =   1  'Graphical
            TabIndex        =   75
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   15
            Left            =   40
            Picture         =   "frmNPCs.frx":48650
            Style           =   1  'Graphical
            TabIndex        =   47
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Items"
         Height          =   855
         Index           =   12
         Left            =   120
         TabIndex        =   13
         Top             =   2400
         Width           =   10935
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   287
            Left            =   10200
            Picture         =   "frmNPCs.frx":48C06
            Style           =   1  'Graphical
            TabIndex        =   365
            Top             =   240
            Value           =   -1  'True
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   273
            Left            =   9600
            Picture         =   "frmNPCs.frx":48FD3
            Style           =   1  'Graphical
            TabIndex        =   344
            Top             =   240
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            Height          =   540
            Index           =   264
            Left            =   3000
            Picture         =   "frmNPCs.frx":49401
            Style           =   1  'Graphical
            TabIndex        =   335
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   238
            Left            =   9000
            Picture         =   "frmNPCs.frx":49838
            Style           =   1  'Graphical
            TabIndex        =   319
            Top             =   240
            Width           =   555
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   248
            Left            =   8400
            Picture         =   "frmNPCs.frx":49C54
            Style           =   1  'Graphical
            TabIndex        =   318
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   49
            Left            =   7800
            Picture         =   "frmNPCs.frx":4A07F
            Style           =   1  'Graphical
            TabIndex        =   317
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   193
            Left            =   6600
            Picture         =   "frmNPCs.frx":4A49D
            Style           =   1  'Graphical
            TabIndex        =   245
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   191
            Left            =   6000
            Picture         =   "frmNPCs.frx":4A8AD
            Style           =   1  'Graphical
            TabIndex        =   242
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   170
            Left            =   2400
            Picture         =   "frmNPCs.frx":4ACBD
            Style           =   1  'Graphical
            TabIndex        =   213
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   169
            Left            =   1800
            Picture         =   "frmNPCs.frx":4B0DE
            Style           =   1  'Graphical
            TabIndex        =   211
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   103
            Left            =   4800
            Picture         =   "frmNPCs.frx":4B52E
            Style           =   1  'Graphical
            TabIndex        =   135
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   34
            Left            =   1320
            Picture         =   "frmNPCs.frx":4B926
            Style           =   1  'Graphical
            TabIndex        =   82
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   90
            Left            =   3600
            Picture         =   "frmNPCs.frx":4BD29
            Style           =   1  'Graphical
            TabIndex        =   77
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   35
            Left            =   5400
            Picture         =   "frmNPCs.frx":4C167
            Style           =   1  'Graphical
            TabIndex        =   42
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   22
            Left            =   7200
            Picture         =   "frmNPCs.frx":4C577
            Style           =   1  'Graphical
            TabIndex        =   28
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   14
            Left            =   720
            Picture         =   "frmNPCs.frx":4D1B9
            Style           =   1  'Graphical
            TabIndex        =   17
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   10
            Left            =   4200
            Picture         =   "frmNPCs.frx":4DDFB
            Style           =   1  'Graphical
            TabIndex        =   15
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   9
            Left            =   120
            Picture         =   "frmNPCs.frx":4E8BD
            Style           =   1  'Graphical
            TabIndex        =   14
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame 
         Caption         =   "Enemies"
         Height          =   2175
         Index           =   8
         Left            =   120
         TabIndex        =   7
         Top             =   240
         Width           =   4900
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   259
            Left            =   4320
            Picture         =   "frmNPCs.frx":4F4FF
            Style           =   1  'Graphical
            TabIndex        =   330
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   244
            Left            =   1920
            Picture         =   "frmNPCs.frx":4F8F3
            Style           =   1  'Graphical
            TabIndex        =   306
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   161
            Left            =   1920
            Picture         =   "frmNPCs.frx":4FEB4
            Style           =   1  'Graphical
            TabIndex        =   203
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   137
            Left            =   1920
            Picture         =   "frmNPCs.frx":50431
            Style           =   1  'Graphical
            TabIndex        =   173
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   136
            Left            =   1320
            MaskColor       =   &H00808080&
            Picture         =   "frmNPCs.frx":5084A
            Style           =   1  'Graphical
            TabIndex        =   172
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   76
            Left            =   1320
            Picture         =   "frmNPCs.frx":50C94
            Style           =   1  'Graphical
            TabIndex        =   73
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   72
            Left            =   2520
            Picture         =   "frmNPCs.frx":51210
            Style           =   1  'Graphical
            TabIndex        =   72
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   71
            Left            =   2520
            Picture         =   "frmNPCs.frx":517EA
            Style           =   1  'Graphical
            TabIndex        =   71
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   54
            Left            =   3120
            Picture         =   "frmNPCs.frx":51DE4
            Style           =   1  'Graphical
            TabIndex        =   58
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   53
            Left            =   2520
            Picture         =   "frmNPCs.frx":52251
            Style           =   1  'Graphical
            TabIndex        =   57
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   48
            Left            =   3120
            Picture         =   "frmNPCs.frx":5269A
            Style           =   1  'Graphical
            TabIndex        =   56
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   47
            Left            =   3720
            Picture         =   "frmNPCs.frx":52AC3
            Style           =   1  'Graphical
            TabIndex        =   55
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   38
            Left            =   3720
            Picture         =   "frmNPCs.frx":52FD9
            Style           =   1  'Graphical
            TabIndex        =   45
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   37
            Left            =   3120
            Picture         =   "frmNPCs.frx":53419
            Style           =   1  'Graphical
            TabIndex        =   44
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   36
            Left            =   3720
            Picture         =   "frmNPCs.frx":538B9
            Style           =   1  'Graphical
            TabIndex        =   43
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   23
            Left            =   120
            Picture         =   "frmNPCs.frx":53D1D
            Style           =   1  'Graphical
            TabIndex        =   29
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   17
            Left            =   720
            Picture         =   "frmNPCs.frx":5495F
            Style           =   1  'Graphical
            TabIndex        =   18
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   12
            Left            =   4320
            Picture         =   "frmNPCs.frx":55421
            Style           =   1  'Graphical
            TabIndex        =   16
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   6
            Left            =   120
            Picture         =   "frmNPCs.frx":55EE3
            Style           =   1  'Graphical
            TabIndex        =   12
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   2
            Left            =   720
            Picture         =   "frmNPCs.frx":56403
            Style           =   1  'Graphical
            TabIndex        =   11
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   1
            Left            =   120
            Picture         =   "frmNPCs.frx":57045
            Style           =   1  'Graphical
            TabIndex        =   10
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   3
            Left            =   1320
            Picture         =   "frmNPCs.frx":57C87
            Style           =   1  'Graphical
            TabIndex        =   9
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton NPC 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   4
            Left            =   720
            Picture         =   "frmNPCs.frx":5824C
            Style           =   1  'Graphical
            TabIndex        =   8
            Top             =   840
            Width           =   540
         End
      End
   End
   Begin VB.Frame Frame 
      Caption         =   "Game Selection"
      Height          =   615
      Index           =   2
      Left            =   3240
      TabIndex        =   5
      Top             =   0
      Width           =   4335
      Begin VB.OptionButton optGame 
         Caption         =   "Misc."
         Height          =   255
         Index           =   4
         Left            =   3480
         Style           =   1  'Graphical
         TabIndex        =   131
         Top             =   240
         Width           =   765
      End
      Begin VB.OptionButton optGame 
         Caption         =   "SMB"
         Height          =   255
         Index           =   3
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   34
         Top             =   240
         Width           =   765
      End
      Begin VB.OptionButton optGame 
         Caption         =   "SMW"
         Height          =   255
         Index           =   2
         Left            =   2640
         Style           =   1  'Graphical
         TabIndex        =   20
         Top             =   240
         Width           =   765
      End
      Begin VB.OptionButton optGame 
         Caption         =   "SMB2"
         Height          =   255
         Index           =   1
         Left            =   960
         Style           =   1  'Graphical
         TabIndex        =   19
         Top             =   240
         Width           =   765
      End
      Begin VB.OptionButton optGame 
         Caption         =   "SMB3"
         Height          =   255
         Index           =   0
         Left            =   1800
         Style           =   1  'Graphical
         TabIndex        =   6
         Top             =   240
         Value           =   -1  'True
         Width           =   765
      End
   End
   Begin VB.Frame frmDirection 
      Caption         =   "Direction"
      Height          =   615
      Left            =   120
      TabIndex        =   2
      Top             =   0
      Width           =   3015
      Begin VB.OptionButton optNPCDirection 
         Caption         =   "Random"
         Height          =   255
         Index           =   1
         Left            =   1080
         Style           =   1  'Graphical
         TabIndex        =   186
         Top             =   240
         Width           =   855
      End
      Begin VB.OptionButton optNPCDirection 
         Caption         =   "Right"
         Height          =   255
         Index           =   2
         Left            =   2040
         Style           =   1  'Graphical
         TabIndex        =   4
         Top             =   240
         Width           =   855
      End
      Begin VB.OptionButton optNPCDirection 
         Caption         =   "Left"
         Height          =   255
         Index           =   0
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   3
         Top             =   240
         Value           =   -1  'True
         Width           =   855
      End
   End
End
Attribute VB_Name = "frmNPCs"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub Bubble_Click()
    On Error Resume Next
    ResetNPC EditorCursor.NPC.Type
    If Me.Visible = True Then FocusNinja.SetFocus
    If Bubble.Caption = "No" Then
        Bubble.Caption = "Yes"
        Egg.Caption = "No"
        Buried.Caption = "No"
        Lakitu.Caption = "No"
        optNPCDirection(1).Caption = "None"
    Else
        Bubble.Caption = "No"
    End If
End Sub

Private Sub Buried_Click()
    On Error Resume Next
    ResetNPC EditorCursor.NPC.Type
    If Me.Visible = True Then FocusNinja.SetFocus
    If Buried.Caption = "No" Then
        Buried.Caption = "Yes"
        Egg.Caption = "No"
        If Bubble.Caption = "Yes" Then frmNPCs.optNPCDirection(1).Caption = "Random"
        Bubble.Caption = "No"
        Lakitu.Caption = "No"
    Else
        Buried.Caption = "No"
    End If
End Sub

Private Sub chkMessage_Click()
    On Error Resume Next
    If Me.Visible = True Then FocusNinja.SetFocus
    If LevelEditor = True Then
        If chkMessage.Value = 0 Then
            MessageText = ""
        Else
            MessageText = NPCText.Text
        End If
    End If
End Sub

Private Sub cmdAdvanced_Click()
    On Error Resume Next
    frmNPCAdvanced.Show
    DoEvents
    frmNPCAdvanced.SetFocus
End Sub

Private Sub cmdEvents_Click()
    On Error Resume Next
    frmAdvanced.Show
    DoEvents
    frmAdvanced.SetFocus
End Sub

Private Sub cmdGenerator_Click()
    On Error Resume Next
    frmGenerator.Show
    DoEvents
    frmGenerator.SetFocus
End Sub

Private Sub DontMove_Click()
    If DontMove.Caption = "No" Then
        DontMove.Caption = "Yes"
    Else
        DontMove.Caption = "No"
    End If
    On Error Resume Next
    If Me.Visible = True Then FocusNinja.SetFocus
End Sub

Private Sub Egg_Click()
    On Error Resume Next
    ResetNPC EditorCursor.NPC.Type
    If Me.Visible = True Then FocusNinja.SetFocus
    If Egg.Caption = "No" Then
        Egg.Caption = "Yes"
        Buried.Caption = "No"
        If Bubble.Caption = "Yes" Then frmNPCs.optNPCDirection(1).Caption = "Random"
        Bubble.Caption = "No"
        Lakitu.Caption = "No"
    Else
        Egg.Caption = "No"
    End If
End Sub

Private Sub Form_Load()
    Dim A As Integer
    For A = 0 To optGame.Count - 1
        Game(A).Top = Game(0).Top
        Game(A).Left = Game(0).Left
    Next A
    Lak.Top = Game(0).Top
    Lak.Left = Game(0).Left
    NPC(1).Value = True
    For A = 2 To NPC.Count
        NPC(A).Value = False
    Next A
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Cancel = 1
    frmLevelEditor.optCursor(13).Value = True
End Sub

Private Sub Friendly_Click()
    On Error Resume Next
    If Me.Visible = True Then FocusNinja.SetFocus
    If Friendly.Caption = "No" Then
        Friendly.Caption = "Yes"
    Else
        Friendly.Caption = "No"
    End If
End Sub

Private Sub Lakitu_Click()
    On Error Resume Next
    ResetNPC EditorCursor.NPC.Type
    If Me.Visible = True Then FocusNinja.SetFocus
    If Lakitu.Caption = "No" Then
        Lakitu.Caption = "Yes"
        Buried.Caption = "No"
        Egg.Caption = "No"
        If Bubble.Caption = "Yes" Then frmNPCs.optNPCDirection(1).Caption = "Random"
        Bubble.Caption = "No"
    Else
        Lakitu.Caption = "No"
    End If
End Sub

Private Sub NPC_Click(Index As Integer)
    Dim A As Integer
    Dim W As Single
    Dim H As Single
    On Error Resume Next
    ResetNPC Index
    If Me.Visible = True Then FocusNinja.SetFocus
    For A = 1 To NPC.Count
        If Index <> A Then NPC(A).Value = False
    Next A
    If NPCHeight(Index) > 36 Or NPCWidth(Index) > 36 Or NPCWidthGFX(Index) > 36 Or NPCHeightGFX(Index) > 36 Then
        W = NPCWidth(Index)
        H = NPCHeight(Index)
        If NPCWidthGFX(Index) > W Then W = NPCWidthGFX(Index)
        If NPCHeightGFX(Index) > H Then H = NPCHeightGFX(Index)
        If (W <= 32 And H <= 54) Or (H <= 32 And W <= 54) Or Index = 134 Then
            Bubble.Enabled = True
            Frame(1).Enabled = True
        Else
            Bubble.Enabled = False
            Frame(1).Enabled = False
        End If
    Else
        Bubble.Enabled = True
        Frame(1).Enabled = True
    End If
    If EditorCursor.Mode <> 13 Then
        NPCText.Text = ""
        Buried.Caption = "No"
        frmGenerator.Spawn.Caption = "No"
        frmGenerator.CheckSpawn
        Egg.Caption = "No"
        Friendly.Caption = "No"
        DontMove.Caption = "No"
        Lakitu.Caption = "No"
        Bubble.Caption = "No"
        frmNPCAdvanced.Legacy.Caption = "No"
        frmAdvanced.TriggerActivate.Text = ""
        frmAdvanced.TriggerDeath.Text = ""
        frmAdvanced.TriggerTalk.Text = ""
        frmAdvanced.AttLayer.Text = ""
        frmNPCAdvanced.cmbPara.ListIndex = 1
        frmNPCAdvanced.cmbCheep.ListIndex = 0
    End If
    optNPCDirection(1).Caption = "Random"
    If Index = 60 Or Index = 62 Or Index = 64 Or Index = 66 Or Index = 104 Then
        frmDirection.Caption = "Active"
        optNPCDirection(0).Caption = "Off"
        optNPCDirection(2).Caption = "On"
    ElseIf Index = 106 Or (NPCIsAParaTroopa(Index) = True And frmNPCAdvanced.cmbPara.ListIndex = 3) Then
        frmDirection.Caption = "Direction"
        optNPCDirection(0).Caption = "Up"
        optNPCDirection(2).Caption = "Down"
    ElseIf Index = 259 Or Index = 260 Then
        frmDirection.Caption = "Direction"
        optNPCDirection(0).Caption = "Bottom"
        optNPCDirection(2).Caption = "Top"
    Else
        frmDirection.Caption = "Direction"
        optNPCDirection(0).Caption = "Left"
        optNPCDirection(2).Caption = "Right"
    End If
    
    If Index = 57 Or Index = 60 Or Index = 62 Or Index = 64 Or Index = 66 Then ' no random direction
        optNPCDirection(1).Enabled = False
        If optNPCDirection(1).Value = True Then
            optNPCDirection(0).Value = True
        End If
    Else
        optNPCDirection(1).Enabled = True
    End If
    If Index = 288 Or Index = 289 Then
        frmNPCAdvanced.WarpFrame.Enabled = True
        frmNPCAdvanced.WarpSection.Enabled = True
    Else
        frmNPCAdvanced.WarpSection.Text = ""
        frmNPCAdvanced.WarpFrame.Enabled = False
        frmNPCAdvanced.WarpSection.Enabled = False
    End If
    If Index = 15 Or Index = 39 Or Index = 86 Then
        frmNPCAdvanced.LegacyFrame.Enabled = True
        frmNPCAdvanced.Legacy.Enabled = True
    Else
        frmNPCAdvanced.LegacyFrame.Enabled = False
        frmNPCAdvanced.Legacy.Enabled = False
    End If
    If NPCIsAParaTroopa(Index) = True Then
        frmNPCAdvanced.ParaFrame.Enabled = True
        frmNPCAdvanced.cmbPara.Enabled = True
    Else
        frmNPCAdvanced.ParaFrame.Enabled = False
        frmNPCAdvanced.cmbPara.Enabled = False
    End If
    If NPCIsCheep(Index) = True Then
        frmNPCAdvanced.CheepFrame.Enabled = True
        frmNPCAdvanced.cmbCheep.Enabled = True
    Else
        frmNPCAdvanced.CheepFrame.Enabled = False
        frmNPCAdvanced.cmbCheep.Enabled = False
    End If
    If Index = 151 Then
        frmNPCs.Friendly.Value = True
        frmNPCs.Friendly.Enabled = False
    Else
        frmNPCs.Friendly.Enabled = True
    End If
    If Index = 260 Then
        frmNPCAdvanced.FireFrame.Enabled = True
        frmNPCAdvanced.scrFire.Enabled = True
        frmNPCAdvanced.lbFire.Enabled = True
    Else
        frmNPCAdvanced.FireFrame.Enabled = False
        frmNPCAdvanced.scrFire.Enabled = False
        frmNPCAdvanced.lbFire.Enabled = False
    End If
End Sub

Private Sub NPCText_Change()
    Friendly.Caption = "Yes"
End Sub

Private Sub optEffect_Click(Index As Integer)
    On Error Resume Next
    If Me.Visible = True Then FocusNinja.SetFocus
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
    Lak.Visible = False
    For A = 1 To NPC.Count
        If NPC(A).Visible = True And NPC(A).Value = True Then
            ResetNPC A
        End If
    Next A
End Sub

Private Sub optNPCDirection_Click(Index As Integer)
    On Error Resume Next
    ResetNPC EditorCursor.NPC.Type
    If Me.Visible = True Then FocusNinja.SetFocus
End Sub

Public Sub ShowLak()
    Dim A As Integer
    For A = 0 To Game.Count - 1
        optGame(A).Value = False
        Game(A).Visible = False
    Next A
    Lak.Visible = True
End Sub
