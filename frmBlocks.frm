VERSION 5.00
Begin VB.Form frmBlocks 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Blocks and Tiles"
   ClientHeight    =   3690
   ClientLeft      =   1.00050e5
   ClientTop       =   80430
   ClientWidth     =   12045
   Icon            =   "frmBlocks.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   ScaleHeight     =   3690
   ScaleWidth      =   12045
   ShowInTaskbar   =   0   'False
   Visible         =   0   'False
   Begin VB.CheckBox chkFill 
      Caption         =   "Fill"
      Height          =   375
      Left            =   11160
      Style           =   1  'Graphical
      TabIndex        =   775
      Top             =   240
      Width           =   735
   End
   Begin VB.Frame Frame 
      Caption         =   "Slippery"
      Height          =   615
      Index           =   0
      Left            =   1680
      TabIndex        =   771
      Top             =   2400
      Width           =   975
      Begin VB.CommandButton cmdSlip 
         Caption         =   "No"
         Height          =   255
         Left            =   120
         TabIndex        =   772
         Top             =   240
         Width           =   735
      End
   End
   Begin VB.Frame Frame 
      Caption         =   "Block Size"
      Height          =   1095
      Index           =   33
      Left            =   1680
      TabIndex        =   430
      Top             =   720
      Width           =   975
      Begin VB.CommandButton BlockR 
         Height          =   255
         Left            =   360
         TabIndex        =   433
         Top             =   480
         Width           =   255
      End
      Begin VB.HScrollBar BlockW 
         Height          =   255
         Left            =   120
         Max             =   99
         Min             =   2
         TabIndex        =   432
         Top             =   480
         Value           =   2
         Width           =   735
      End
      Begin VB.VScrollBar BlockH 
         Height          =   735
         Left            =   360
         Max             =   99
         Min             =   2
         TabIndex        =   431
         Top             =   240
         Value           =   2
         Width           =   255
      End
   End
   Begin VB.Frame Game 
      Caption         =   "Misc."
      Height          =   2535
      Index           =   4
      Left            =   8640
      TabIndex        =   349
      Top             =   9720
      Visible         =   0   'False
      Width           =   8535
      Begin VB.Frame Misc 
         Caption         =   "Players"
         Height          =   1575
         Index           =   3
         Left            =   1560
         TabIndex        =   758
         Top             =   4800
         Width           =   3255
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   632
            Left            =   2520
            Picture         =   "frmBlocks.frx":0B3A
            Style           =   1  'Graphical
            TabIndex        =   770
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   631
            Left            =   2520
            Picture         =   "frmBlocks.frx":0F41
            Style           =   1  'Graphical
            TabIndex        =   769
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   629
            Left            =   1920
            Picture         =   "frmBlocks.frx":133F
            Style           =   1  'Graphical
            TabIndex        =   767
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   628
            Left            =   1320
            Picture         =   "frmBlocks.frx":1746
            Style           =   1  'Graphical
            TabIndex        =   766
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   627
            Left            =   720
            Picture         =   "frmBlocks.frx":1B4D
            Style           =   1  'Graphical
            TabIndex        =   765
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   626
            Left            =   120
            Picture         =   "frmBlocks.frx":1F54
            Style           =   1  'Graphical
            TabIndex        =   764
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   625
            Left            =   1920
            Picture         =   "frmBlocks.frx":235B
            Style           =   1  'Graphical
            TabIndex        =   763
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   624
            Left            =   1320
            Picture         =   "frmBlocks.frx":275C
            Style           =   1  'Graphical
            TabIndex        =   762
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   623
            Left            =   720
            Picture         =   "frmBlocks.frx":2B62
            Style           =   1  'Graphical
            TabIndex        =   761
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   622
            Left            =   120
            Picture         =   "frmBlocks.frx":2F60
            Style           =   1  'Graphical
            TabIndex        =   759
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Misc 
         Caption         =   "Metroid (B)"
         Height          =   2175
         Index           =   2
         Left            =   1560
         TabIndex        =   677
         Top             =   2520
         Visible         =   0   'False
         Width           =   5055
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   567
            Left            =   720
            Picture         =   "frmBlocks.frx":3374
            Style           =   1  'Graphical
            TabIndex        =   700
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   566
            Left            =   1920
            Picture         =   "frmBlocks.frx":3795
            Style           =   1  'Graphical
            TabIndex        =   699
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   565
            Left            =   120
            Picture         =   "frmBlocks.frx":3B7C
            Style           =   1  'Graphical
            TabIndex        =   698
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   564
            Left            =   2520
            Picture         =   "frmBlocks.frx":3F99
            Style           =   1  'Graphical
            TabIndex        =   697
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   563
            Left            =   1320
            Picture         =   "frmBlocks.frx":43DA
            Style           =   1  'Graphical
            TabIndex        =   696
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   562
            Left            =   3120
            Picture         =   "frmBlocks.frx":47E3
            Style           =   1  'Graphical
            TabIndex        =   695
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   561
            Left            =   1320
            Picture         =   "frmBlocks.frx":4C10
            Style           =   1  'Graphical
            TabIndex        =   694
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   560
            Left            =   3720
            Picture         =   "frmBlocks.frx":5029
            Style           =   1  'Graphical
            TabIndex        =   693
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   559
            Left            =   3120
            Picture         =   "frmBlocks.frx":546D
            Style           =   1  'Graphical
            TabIndex        =   692
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   558
            Left            =   1920
            Picture         =   "frmBlocks.frx":58A8
            Style           =   1  'Graphical
            TabIndex        =   691
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   557
            Left            =   1920
            Picture         =   "frmBlocks.frx":5C76
            Style           =   1  'Graphical
            TabIndex        =   690
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   556
            Left            =   720
            Picture         =   "frmBlocks.frx":6064
            Style           =   1  'Graphical
            TabIndex        =   689
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   555
            Left            =   2520
            Picture         =   "frmBlocks.frx":647C
            Style           =   1  'Graphical
            TabIndex        =   688
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   554
            Left            =   720
            Picture         =   "frmBlocks.frx":68A5
            Style           =   1  'Graphical
            TabIndex        =   687
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   553
            Left            =   120
            Picture         =   "frmBlocks.frx":6CD7
            Style           =   1  'Graphical
            TabIndex        =   686
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   552
            Left            =   2520
            Picture         =   "frmBlocks.frx":70FC
            Style           =   1  'Graphical
            TabIndex        =   685
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   551
            Left            =   1320
            Picture         =   "frmBlocks.frx":7527
            Style           =   1  'Graphical
            TabIndex        =   684
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   550
            Left            =   120
            Picture         =   "frmBlocks.frx":78D4
            Style           =   1  'Graphical
            TabIndex        =   683
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   549
            Left            =   4320
            Picture         =   "frmBlocks.frx":7CDE
            Style           =   1  'Graphical
            TabIndex        =   682
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   548
            Left            =   4320
            Picture         =   "frmBlocks.frx":811A
            Style           =   1  'Graphical
            TabIndex        =   681
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   547
            Left            =   3720
            Picture         =   "frmBlocks.frx":851C
            Style           =   1  'Graphical
            TabIndex        =   680
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   546
            Left            =   3720
            Picture         =   "frmBlocks.frx":8949
            Style           =   1  'Graphical
            TabIndex        =   679
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   545
            Left            =   3120
            Picture         =   "frmBlocks.frx":8D57
            Style           =   1  'Graphical
            TabIndex        =   678
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame Misc 
         Caption         =   "Metroid (A)"
         Height          =   2175
         Index           =   1
         Left            =   1560
         TabIndex        =   643
         Top             =   240
         Width           =   6855
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   512
            Left            =   4920
            Picture         =   "frmBlocks.frx":917C
            Style           =   1  'Graphical
            TabIndex        =   676
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   513
            Left            =   3720
            Picture         =   "frmBlocks.frx":959E
            Style           =   1  'Graphical
            TabIndex        =   675
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   514
            Left            =   3720
            Picture         =   "frmBlocks.frx":9969
            Style           =   1  'Graphical
            TabIndex        =   674
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   515
            Left            =   3120
            Picture         =   "frmBlocks.frx":9D69
            Style           =   1  'Graphical
            TabIndex        =   673
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   516
            Left            =   2520
            Picture         =   "frmBlocks.frx":A13F
            Style           =   1  'Graphical
            TabIndex        =   672
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   517
            Left            =   3120
            Picture         =   "frmBlocks.frx":A56D
            Style           =   1  'Graphical
            TabIndex        =   671
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   518
            Left            =   4320
            Picture         =   "frmBlocks.frx":A97C
            Style           =   1  'Graphical
            TabIndex        =   670
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   519
            Left            =   120
            Picture         =   "frmBlocks.frx":AD8E
            Style           =   1  'Graphical
            TabIndex        =   669
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   520
            Left            =   120
            Picture         =   "frmBlocks.frx":B1C3
            Style           =   1  'Graphical
            TabIndex        =   668
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   521
            Left            =   1320
            Picture         =   "frmBlocks.frx":B5D7
            Style           =   1  'Graphical
            TabIndex        =   667
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   522
            Left            =   120
            Picture         =   "frmBlocks.frx":BA38
            Style           =   1  'Graphical
            TabIndex        =   666
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   523
            Left            =   1320
            Picture         =   "frmBlocks.frx":BE8A
            Style           =   1  'Graphical
            TabIndex        =   665
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   524
            Left            =   720
            Picture         =   "frmBlocks.frx":C29C
            Style           =   1  'Graphical
            TabIndex        =   664
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   525
            Left            =   1320
            Picture         =   "frmBlocks.frx":C650
            Style           =   1  'Graphical
            TabIndex        =   663
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   526
            Left            =   720
            Picture         =   "frmBlocks.frx":CA65
            Style           =   1  'Graphical
            TabIndex        =   662
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   527
            Left            =   1920
            Picture         =   "frmBlocks.frx":CEBF
            Style           =   1  'Graphical
            TabIndex        =   661
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   528
            Left            =   720
            Picture         =   "frmBlocks.frx":D30B
            Style           =   1  'Graphical
            TabIndex        =   660
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   529
            Left            =   1920
            Picture         =   "frmBlocks.frx":D750
            Style           =   1  'Graphical
            TabIndex        =   659
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   530
            Left            =   1920
            Picture         =   "frmBlocks.frx":DB36
            Style           =   1  'Graphical
            TabIndex        =   658
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   531
            Left            =   2520
            Picture         =   "frmBlocks.frx":E005
            Style           =   1  'Graphical
            TabIndex        =   657
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   532
            Left            =   2520
            Picture         =   "frmBlocks.frx":E433
            Style           =   1  'Graphical
            TabIndex        =   656
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   533
            Left            =   4320
            Picture         =   "frmBlocks.frx":E847
            Style           =   1  'Graphical
            TabIndex        =   655
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   534
            Left            =   5520
            Picture         =   "frmBlocks.frx":EC18
            Style           =   1  'Graphical
            TabIndex        =   654
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   535
            Left            =   6120
            Picture         =   "frmBlocks.frx":EFE2
            Style           =   1  'Graphical
            TabIndex        =   653
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   536
            Left            =   5520
            Picture         =   "frmBlocks.frx":F3B0
            Style           =   1  'Graphical
            TabIndex        =   652
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   537
            Left            =   6120
            Picture         =   "frmBlocks.frx":F77D
            Style           =   1  'Graphical
            TabIndex        =   651
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   538
            Left            =   3120
            Picture         =   "frmBlocks.frx":FB30
            Style           =   1  'Graphical
            TabIndex        =   650
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   539
            Left            =   4920
            Picture         =   "frmBlocks.frx":FF1B
            Style           =   1  'Graphical
            TabIndex        =   649
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   540
            Left            =   6120
            Picture         =   "frmBlocks.frx":1037A
            Style           =   1  'Graphical
            TabIndex        =   648
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   541
            Left            =   5520
            Picture         =   "frmBlocks.frx":1076C
            Style           =   1  'Graphical
            TabIndex        =   647
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   542
            Left            =   4920
            Picture         =   "frmBlocks.frx":10BA4
            Style           =   1  'Graphical
            TabIndex        =   646
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   543
            Left            =   4320
            Picture         =   "frmBlocks.frx":10FE1
            Style           =   1  'Graphical
            TabIndex        =   645
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   544
            Left            =   3720
            Picture         =   "frmBlocks.frx":113FD
            Style           =   1  'Graphical
            TabIndex        =   644
            Top             =   1440
            Width           =   540
         End
      End
      Begin VB.Frame Frame4 
         Caption         =   "Tile Set"
         Height          =   1335
         Left            =   120
         TabIndex        =   639
         Top             =   240
         Width           =   1335
         Begin VB.OptionButton optMisc 
            Caption         =   "Players"
            Height          =   255
            Index           =   3
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   760
            Top             =   960
            Width           =   1095
         End
         Begin VB.OptionButton optMisc 
            Caption         =   "Metroid (B)"
            Height          =   255
            Index           =   2
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   642
            Top             =   720
            Width           =   1095
         End
         Begin VB.OptionButton optMisc 
            Caption         =   "Metroid (A)"
            Height          =   255
            Index           =   1
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   641
            Top             =   480
            Width           =   1095
         End
         Begin VB.OptionButton optMisc 
            Caption         =   "Zelda"
            Height          =   255
            Index           =   0
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   640
            Top             =   240
            Value           =   -1  'True
            Width           =   1095
         End
      End
      Begin VB.Frame Misc 
         Caption         =   "Zelda 2"
         Height          =   1575
         Index           =   0
         Left            =   6720
         TabIndex        =   636
         Top             =   2520
         Width           =   855
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   291
            Left            =   120
            Picture         =   "frmBlocks.frx":11828
            Style           =   1  'Graphical
            TabIndex        =   638
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   292
            Left            =   120
            Picture         =   "frmBlocks.frx":11C2D
            Style           =   1  'Graphical
            TabIndex        =   637
            Top             =   840
            Width           =   540
         End
      End
   End
   Begin VB.OptionButton Block 
      Height          =   540
      Index           =   183
      Left            =   20640
      Picture         =   "frmBlocks.frx":1200E
      Style           =   1  'Graphical
      TabIndex        =   339
      Top             =   12840
      Visible         =   0   'False
      Width           =   540
   End
   Begin VB.Frame Frame1 
      Caption         =   "Events"
      Height          =   615
      Left            =   1680
      TabIndex        =   334
      Top             =   3000
      Width           =   975
      Begin VB.CommandButton cmdAdvanced 
         Caption         =   "Show"
         Height          =   255
         Left            =   120
         TabIndex        =   335
         Top             =   240
         Width           =   735
      End
   End
   Begin VB.Frame Frame 
      Caption         =   "Block Contents"
      Height          =   3495
      Index           =   30
      Left            =   120
      TabIndex        =   170
      Top             =   120
      Width           =   1455
      Begin VB.CommandButton qBlockS 
         Caption         =   "Yoshi"
         Height          =   255
         Index           =   95
         Left            =   120
         TabIndex        =   480
         Top             =   3120
         Width           =   1215
      End
      Begin VB.CommandButton qBlockS 
         Caption         =   "1-Up"
         Height          =   255
         Index           =   90
         Left            =   120
         TabIndex        =   479
         Top             =   2880
         Width           =   1215
      End
      Begin VB.CommandButton qBlockS 
         Caption         =   "Tanooki Suit"
         Height          =   255
         Index           =   169
         Left            =   120
         TabIndex        =   478
         Top             =   2640
         Width           =   1215
      End
      Begin VB.CommandButton qBlockS 
         Caption         =   "Leaf"
         Height          =   255
         Index           =   34
         Left            =   120
         TabIndex        =   415
         Top             =   2400
         Width           =   1215
      End
      Begin VB.CommandButton qBlockS 
         Caption         =   "Fire Flower"
         Height          =   255
         Index           =   14
         Left            =   120
         TabIndex        =   414
         Top             =   2160
         Width           =   1215
      End
      Begin VB.CommandButton qBlockS 
         Caption         =   "Mushroom"
         Height          =   255
         Index           =   9
         Left            =   120
         TabIndex        =   413
         Top             =   1920
         Width           =   1215
      End
      Begin VB.CommandButton qBlockS 
         Caption         =   "None"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   173
         Top             =   1680
         Width           =   1215
      End
      Begin VB.PictureBox PicNPC 
         AutoRedraw      =   -1  'True
         BackColor       =   &H00000000&
         Height          =   1215
         Left            =   120
         ScaleHeight     =   77
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   77
         TabIndex        =   172
         Top             =   240
         Width           =   1215
         Begin VB.Label lblCoins 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "1 coin"
            BeginProperty Font 
               Name            =   "Times New Roman"
               Size            =   9.75
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H00FFFFFF&
            Height          =   225
            Left            =   0
            TabIndex        =   174
            Top             =   960
            Visible         =   0   'False
            Width           =   495
         End
      End
      Begin VB.HScrollBar optBlockS 
         Height          =   255
         Left            =   120
         Max             =   1000
         Min             =   -99
         TabIndex        =   171
         Top             =   1440
         Width           =   1215
      End
   End
   Begin VB.PictureBox FocusNinja 
      Height          =   375
      Left            =   120
      ScaleHeight     =   315
      ScaleWidth      =   315
      TabIndex        =   0
      Top             =   13560
      Width           =   375
   End
   Begin VB.Frame Game 
      Caption         =   "Super Mario World"
      Height          =   2535
      Index           =   3
      Left            =   2760
      TabIndex        =   78
      Top             =   3720
      Visible         =   0   'False
      Width           =   9135
      Begin VB.Frame SMW 
         Caption         =   "Lava"
         Height          =   2175
         Index           =   12
         Left            =   2160
         TabIndex        =   578
         Top             =   2520
         Width           =   6855
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   404
            Left            =   4320
            Picture         =   "frmBlocks.frx":12407
            Style           =   1  'Graphical
            TabIndex        =   609
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   420
            Left            =   4920
            Picture         =   "frmBlocks.frx":12813
            Style           =   1  'Graphical
            TabIndex        =   608
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   487
            Left            =   6120
            Picture         =   "frmBlocks.frx":12B77
            Style           =   1  'Graphical
            TabIndex        =   607
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   484
            Left            =   5520
            Picture         =   "frmBlocks.frx":12F50
            Style           =   1  'Graphical
            TabIndex        =   606
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   483
            Left            =   5520
            Picture         =   "frmBlocks.frx":1331B
            Style           =   1  'Graphical
            TabIndex        =   605
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   481
            Left            =   6120
            Picture         =   "frmBlocks.frx":136FC
            Style           =   1  'Graphical
            TabIndex        =   604
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   473
            Left            =   1920
            Picture         =   "frmBlocks.frx":13ACC
            Style           =   1  'Graphical
            TabIndex        =   603
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   475
            Left            =   2520
            Picture         =   "frmBlocks.frx":13EAB
            Style           =   1  'Graphical
            TabIndex        =   602
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   477
            Left            =   3120
            Picture         =   "frmBlocks.frx":14265
            Style           =   1  'Graphical
            TabIndex        =   601
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   478
            Left            =   3720
            Picture         =   "frmBlocks.frx":1463C
            Style           =   1  'Graphical
            TabIndex        =   600
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   486
            Left            =   4320
            Picture         =   "frmBlocks.frx":149EE
            Style           =   1  'Graphical
            TabIndex        =   599
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   485
            Left            =   4920
            Picture         =   "frmBlocks.frx":14E01
            Style           =   1  'Graphical
            TabIndex        =   598
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   482
            Left            =   4920
            Picture         =   "frmBlocks.frx":15219
            Style           =   1  'Graphical
            TabIndex        =   597
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   472
            Left            =   3120
            Picture         =   "frmBlocks.frx":15623
            Style           =   1  'Graphical
            TabIndex        =   596
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   474
            Left            =   3720
            Picture         =   "frmBlocks.frx":159E2
            Style           =   1  'Graphical
            TabIndex        =   595
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   476
            Left            =   3120
            Picture         =   "frmBlocks.frx":15DA9
            Style           =   1  'Graphical
            TabIndex        =   594
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   479
            Left            =   3720
            Picture         =   "frmBlocks.frx":1616B
            Style           =   1  'Graphical
            TabIndex        =   593
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   480
            Left            =   4320
            Picture         =   "frmBlocks.frx":16533
            Style           =   1  'Graphical
            TabIndex        =   592
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   459
            Left            =   720
            Picture         =   "frmBlocks.frx":16948
            Style           =   1  'Graphical
            TabIndex        =   591
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   460
            Left            =   1320
            Picture         =   "frmBlocks.frx":16D42
            Style           =   1  'Graphical
            TabIndex        =   590
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   461
            Left            =   1320
            Picture         =   "frmBlocks.frx":1717A
            Style           =   1  'Graphical
            TabIndex        =   589
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   462
            Left            =   720
            Picture         =   "frmBlocks.frx":17576
            Style           =   1  'Graphical
            TabIndex        =   588
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   463
            Left            =   120
            Picture         =   "frmBlocks.frx":1796B
            Style           =   1  'Graphical
            TabIndex        =   587
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   464
            Left            =   1320
            Picture         =   "frmBlocks.frx":17D69
            Style           =   1  'Graphical
            TabIndex        =   586
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   465
            Left            =   120
            Picture         =   "frmBlocks.frx":181A0
            Style           =   1  'Graphical
            TabIndex        =   585
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   466
            Left            =   120
            Picture         =   "frmBlocks.frx":185DA
            Style           =   1  'Graphical
            TabIndex        =   584
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   467
            Left            =   720
            Picture         =   "frmBlocks.frx":18A12
            Style           =   1  'Graphical
            TabIndex        =   583
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   468
            Left            =   2520
            Picture         =   "frmBlocks.frx":18D76
            Style           =   1  'Graphical
            TabIndex        =   582
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   469
            Left            =   2520
            Picture         =   "frmBlocks.frx":19143
            Style           =   1  'Graphical
            TabIndex        =   581
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   470
            Left            =   1920
            Picture         =   "frmBlocks.frx":1950A
            Style           =   1  'Graphical
            TabIndex        =   580
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   471
            Left            =   1920
            Picture         =   "frmBlocks.frx":198D0
            Style           =   1  'Graphical
            TabIndex        =   579
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMW 
         Caption         =   "Hurts"
         Height          =   2175
         Index           =   11
         Left            =   12720
         TabIndex        =   559
         Top             =   2640
         Width           =   2055
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   431
            Left            =   720
            Picture         =   "frmBlocks.frx":19CA0
            Style           =   1  'Graphical
            TabIndex        =   566
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   430
            Left            =   720
            Picture         =   "frmBlocks.frx":1A088
            Style           =   1  'Graphical
            TabIndex        =   565
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   429
            Left            =   720
            Picture         =   "frmBlocks.frx":1A471
            Style           =   1  'Graphical
            TabIndex        =   564
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   428
            Left            =   1320
            Picture         =   "frmBlocks.frx":1A871
            Style           =   1  'Graphical
            TabIndex        =   563
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   409
            Left            =   120
            Picture         =   "frmBlocks.frx":1AC72
            Style           =   1  'Graphical
            TabIndex        =   562
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   408
            Left            =   120
            Picture         =   "frmBlocks.frx":1B06A
            Style           =   1  'Graphical
            TabIndex        =   561
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   407
            Left            =   120
            Picture         =   "frmBlocks.frx":1B463
            Style           =   1  'Graphical
            TabIndex        =   560
            Top             =   1440
            Width           =   540
         End
      End
      Begin VB.Frame SMW 
         Caption         =   "Castle"
         Height          =   2175
         Index           =   10
         Left            =   14400
         TabIndex        =   529
         Top             =   7800
         Width           =   6255
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   454
            Left            =   3720
            Picture         =   "frmBlocks.frx":1B85B
            Style           =   1  'Graphical
            TabIndex        =   574
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   455
            Left            =   5520
            Picture         =   "frmBlocks.frx":1BCAD
            Style           =   1  'Graphical
            TabIndex        =   573
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   456
            Left            =   4320
            Picture         =   "frmBlocks.frx":1C0FA
            Style           =   1  'Graphical
            TabIndex        =   572
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   453
            Left            =   4920
            Picture         =   "frmBlocks.frx":1C55D
            Style           =   1  'Graphical
            TabIndex        =   571
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   452
            Left            =   4920
            Picture         =   "frmBlocks.frx":1C9BF
            Style           =   1  'Graphical
            TabIndex        =   555
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   451
            Left            =   5520
            Picture         =   "frmBlocks.frx":1CD8A
            Style           =   1  'Graphical
            TabIndex        =   554
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   450
            Left            =   4920
            Picture         =   "frmBlocks.frx":1D15B
            Style           =   1  'Graphical
            TabIndex        =   553
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   449
            Left            =   5520
            Picture         =   "frmBlocks.frx":1D590
            Style           =   1  'Graphical
            TabIndex        =   552
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   418
            Left            =   3720
            Picture         =   "frmBlocks.frx":1D9C9
            Style           =   1  'Graphical
            TabIndex        =   551
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   417
            Left            =   4320
            Picture         =   "frmBlocks.frx":1DE01
            Style           =   1  'Graphical
            TabIndex        =   550
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   416
            Left            =   3720
            Picture         =   "frmBlocks.frx":1E236
            Style           =   1  'Graphical
            TabIndex        =   549
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   415
            Left            =   4320
            Picture         =   "frmBlocks.frx":1E66E
            Style           =   1  'Graphical
            TabIndex        =   548
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   436
            Left            =   3120
            Picture         =   "frmBlocks.frx":1EAA3
            Style           =   1  'Graphical
            TabIndex        =   547
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   427
            Left            =   1920
            Picture         =   "frmBlocks.frx":1EF10
            Style           =   1  'Graphical
            TabIndex        =   546
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   426
            Left            =   3120
            Picture         =   "frmBlocks.frx":1F385
            Style           =   1  'Graphical
            TabIndex        =   545
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   425
            Left            =   1920
            Picture         =   "frmBlocks.frx":1F7E9
            Style           =   1  'Graphical
            TabIndex        =   544
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   424
            Left            =   2520
            Picture         =   "frmBlocks.frx":1FC60
            Style           =   1  'Graphical
            TabIndex        =   543
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   423
            Left            =   1920
            Picture         =   "frmBlocks.frx":200B0
            Style           =   1  'Graphical
            TabIndex        =   542
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   422
            Left            =   2520
            Picture         =   "frmBlocks.frx":20514
            Style           =   1  'Graphical
            TabIndex        =   541
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   421
            Left            =   3120
            Picture         =   "frmBlocks.frx":20932
            Style           =   1  'Graphical
            TabIndex        =   540
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   419
            Left            =   2520
            Picture         =   "frmBlocks.frx":20D9B
            Style           =   1  'Graphical
            TabIndex        =   539
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   435
            Left            =   120
            Picture         =   "frmBlocks.frx":211E7
            Style           =   1  'Graphical
            TabIndex        =   538
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   434
            Left            =   1320
            Picture         =   "frmBlocks.frx":215FA
            Style           =   1  'Graphical
            TabIndex        =   537
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   433
            Left            =   120
            Picture         =   "frmBlocks.frx":219FB
            Style           =   1  'Graphical
            TabIndex        =   536
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   432
            Left            =   1320
            Picture         =   "frmBlocks.frx":21DF7
            Style           =   1  'Graphical
            TabIndex        =   535
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   414
            Left            =   720
            Picture         =   "frmBlocks.frx":221DF
            Style           =   1  'Graphical
            TabIndex        =   534
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   413
            Left            =   720
            Picture         =   "frmBlocks.frx":225BC
            Style           =   1  'Graphical
            TabIndex        =   533
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   412
            Left            =   120
            Picture         =   "frmBlocks.frx":22964
            Style           =   1  'Graphical
            TabIndex        =   532
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   411
            Left            =   1320
            Picture         =   "frmBlocks.frx":22D56
            Style           =   1  'Graphical
            TabIndex        =   531
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   410
            Left            =   720
            Picture         =   "frmBlocks.frx":23133
            Style           =   1  'Graphical
            TabIndex        =   530
            Top             =   1440
            Width           =   540
         End
      End
      Begin VB.Frame Frame2 
         Caption         =   "Tile Set"
         Height          =   2175
         Left            =   120
         TabIndex        =   467
         Top             =   240
         Width           =   1935
         Begin VB.OptionButton optSMW 
            Caption         =   "Lava"
            Height          =   255
            Index           =   12
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   577
            Top             =   1680
            Width           =   855
         End
         Begin VB.OptionButton optSMW 
            Caption         =   "Hurts"
            Height          =   255
            Index           =   11
            Left            =   960
            Style           =   1  'Graphical
            TabIndex        =   568
            Top             =   1440
            Width           =   855
         End
         Begin VB.OptionButton optSMW 
            Caption         =   "Castle"
            Height          =   255
            Index           =   10
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   567
            Top             =   1440
            Width           =   855
         End
         Begin VB.OptionButton optSMW 
            Caption         =   "Special"
            Height          =   255
            Index           =   9
            Left            =   960
            Style           =   1  'Graphical
            TabIndex        =   477
            Top             =   1200
            Width           =   855
         End
         Begin VB.OptionButton optSMW 
            Caption         =   "Misc."
            Height          =   255
            Index           =   8
            Left            =   960
            Style           =   1  'Graphical
            TabIndex        =   476
            Top             =   960
            Width           =   855
         End
         Begin VB.OptionButton optSMW 
            Caption         =   "Sizable"
            Height          =   255
            Index           =   7
            Left            =   960
            Style           =   1  'Graphical
            TabIndex        =   475
            Top             =   720
            Width           =   855
         End
         Begin VB.OptionButton optSMW 
            Caption         =   "Bonus"
            Height          =   255
            Index           =   6
            Left            =   960
            Style           =   1  'Graphical
            TabIndex        =   474
            Top             =   480
            Width           =   855
         End
         Begin VB.OptionButton optSMW 
            Caption         =   "Switch"
            Height          =   255
            Index           =   5
            Left            =   960
            Style           =   1  'Graphical
            TabIndex        =   473
            Top             =   240
            Width           =   855
         End
         Begin VB.OptionButton optSMW 
            Caption         =   "Ghost"
            Height          =   255
            Index           =   4
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   472
            Top             =   1200
            Width           =   855
         End
         Begin VB.OptionButton optSMW 
            Caption         =   "Pipes"
            Height          =   255
            Index           =   3
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   471
            Top             =   960
            Width           =   855
         End
         Begin VB.OptionButton optSMW 
            Caption         =   "Woods"
            Height          =   255
            Index           =   2
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   470
            Top             =   720
            Width           =   855
         End
         Begin VB.OptionButton optSMW 
            Caption         =   "Cave"
            Height          =   255
            Index           =   1
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   469
            Top             =   480
            Width           =   855
         End
         Begin VB.OptionButton optSMW 
            Caption         =   "Grass"
            Height          =   255
            Index           =   0
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   468
            Top             =   240
            Width           =   855
         End
      End
      Begin VB.Frame SMW 
         Caption         =   "Misc."
         Height          =   975
         Index           =   8
         Left            =   13680
         TabIndex        =   297
         Top             =   5640
         Width           =   4455
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   448
            Left            =   3720
            Picture         =   "frmBlocks.frx":234FA
            Style           =   1  'Graphical
            TabIndex        =   558
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   447
            Left            =   2520
            Picture         =   "frmBlocks.frx":2390C
            Style           =   1  'Graphical
            TabIndex        =   557
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   446
            Left            =   3120
            Picture         =   "frmBlocks.frx":23CDA
            Style           =   1  'Graphical
            TabIndex        =   556
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   115
            Left            =   1920
            Picture         =   "frmBlocks.frx":2411B
            Style           =   1  'Graphical
            TabIndex        =   301
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   135
            Left            =   1320
            Picture         =   "frmBlocks.frx":24515
            Style           =   1  'Graphical
            TabIndex        =   300
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   134
            Left            =   720
            Picture         =   "frmBlocks.frx":24933
            Style           =   1  'Graphical
            TabIndex        =   299
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   133
            Left            =   120
            Picture         =   "frmBlocks.frx":24D0C
            Style           =   1  'Graphical
            TabIndex        =   298
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMW 
         Caption         =   "Bonus"
         Height          =   2175
         Index           =   6
         Left            =   6960
         TabIndex        =   283
         Top             =   4920
         Width           =   3255
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   227
            Left            =   120
            Picture         =   "frmBlocks.frx":25123
            Style           =   1  'Graphical
            TabIndex        =   296
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   228
            Left            =   720
            Picture         =   "frmBlocks.frx":25527
            Style           =   1  'Graphical
            TabIndex        =   295
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   229
            Left            =   1320
            Picture         =   "frmBlocks.frx":2590A
            Style           =   1  'Graphical
            TabIndex        =   294
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   230
            Left            =   120
            Picture         =   "frmBlocks.frx":25D0D
            Style           =   1  'Graphical
            TabIndex        =   293
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   231
            Left            =   720
            Picture         =   "frmBlocks.frx":26118
            Style           =   1  'Graphical
            TabIndex        =   292
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   232
            Left            =   1320
            Picture         =   "frmBlocks.frx":264EB
            Style           =   1  'Graphical
            TabIndex        =   291
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   233
            Left            =   120
            Picture         =   "frmBlocks.frx":268F9
            Style           =   1  'Graphical
            TabIndex        =   290
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   234
            Left            =   720
            Picture         =   "frmBlocks.frx":26D05
            Style           =   1  'Graphical
            TabIndex        =   289
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   235
            Left            =   1320
            Picture         =   "frmBlocks.frx":270E8
            Style           =   1  'Graphical
            TabIndex        =   288
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   236
            Left            =   1920
            Picture         =   "frmBlocks.frx":274F4
            Style           =   1  'Graphical
            TabIndex        =   287
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   237
            Left            =   2520
            Picture         =   "frmBlocks.frx":278E4
            Style           =   1  'Graphical
            TabIndex        =   286
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   238
            Left            =   2520
            Picture         =   "frmBlocks.frx":27CCD
            Style           =   1  'Graphical
            TabIndex        =   285
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   239
            Left            =   1920
            Picture         =   "frmBlocks.frx":280B0
            Style           =   1  'Graphical
            TabIndex        =   284
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame SMW 
         Caption         =   "Cave"
         Height          =   2175
         Index           =   1
         Left            =   10320
         TabIndex        =   268
         Top             =   360
         Width           =   5655
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   322
            Left            =   3720
            Picture         =   "frmBlocks.frx":28499
            Style           =   1  'Graphical
            TabIndex        =   466
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   319
            Left            =   3120
            Picture         =   "frmBlocks.frx":2882D
            Style           =   1  'Graphical
            TabIndex        =   465
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   320
            Left            =   4320
            Picture         =   "frmBlocks.frx":28BEB
            Style           =   1  'Graphical
            TabIndex        =   464
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   321
            Left            =   2520
            Picture         =   "frmBlocks.frx":28F7B
            Style           =   1  'Graphical
            TabIndex        =   463
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   315
            Left            =   3720
            Picture         =   "frmBlocks.frx":2933A
            Style           =   1  'Graphical
            TabIndex        =   462
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   316
            Left            =   3120
            Picture         =   "frmBlocks.frx":29448
            Style           =   1  'Graphical
            TabIndex        =   461
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   317
            Left            =   3120
            Picture         =   "frmBlocks.frx":29557
            Style           =   1  'Graphical
            TabIndex        =   460
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   318
            Left            =   3720
            Picture         =   "frmBlocks.frx":29667
            Style           =   1  'Graphical
            TabIndex        =   459
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   368
            Left            =   4320
            Picture         =   "frmBlocks.frx":29775
            Style           =   1  'Graphical
            TabIndex        =   458
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   365
            Left            =   4320
            Picture         =   "frmBlocks.frx":29B5B
            Style           =   1  'Graphical
            TabIndex        =   457
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   366
            Left            =   4920
            Picture         =   "frmBlocks.frx":29F44
            Style           =   1  'Graphical
            TabIndex        =   456
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   367
            Left            =   4920
            Picture         =   "frmBlocks.frx":2A32C
            Style           =   1  'Graphical
            TabIndex        =   455
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   246
            Left            =   120
            Picture         =   "frmBlocks.frx":2A711
            Style           =   1  'Graphical
            TabIndex        =   282
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   247
            Left            =   1320
            Picture         =   "frmBlocks.frx":2AB3E
            Style           =   1  'Graphical
            TabIndex        =   281
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   248
            Left            =   120
            Picture         =   "frmBlocks.frx":2AF5E
            Style           =   1  'Graphical
            TabIndex        =   280
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   249
            Left            =   1320
            Picture         =   "frmBlocks.frx":2B386
            Style           =   1  'Graphical
            TabIndex        =   279
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   250
            Left            =   720
            Picture         =   "frmBlocks.frx":2B7AF
            Style           =   1  'Graphical
            TabIndex        =   278
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   251
            Left            =   720
            Picture         =   "frmBlocks.frx":2BBA6
            Style           =   1  'Graphical
            TabIndex        =   277
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   252
            Left            =   120
            Picture         =   "frmBlocks.frx":2BF41
            Style           =   1  'Graphical
            TabIndex        =   276
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   253
            Left            =   1320
            Picture         =   "frmBlocks.frx":2C351
            Style           =   1  'Graphical
            TabIndex        =   275
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   254
            Left            =   720
            Picture         =   "frmBlocks.frx":2C765
            Style           =   1  'Graphical
            TabIndex        =   274
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   262
            Left            =   1920
            Picture         =   "frmBlocks.frx":2CB60
            Style           =   1  'Graphical
            TabIndex        =   273
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   255
            Left            =   2520
            Picture         =   "frmBlocks.frx":2CF12
            Style           =   1  'Graphical
            TabIndex        =   272
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   256
            Left            =   1920
            Picture         =   "frmBlocks.frx":2D2E0
            Style           =   1  'Graphical
            TabIndex        =   271
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   257
            Left            =   1920
            Picture         =   "frmBlocks.frx":2D6AC
            Style           =   1  'Graphical
            TabIndex        =   270
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   258
            Left            =   2520
            Picture         =   "frmBlocks.frx":2DA7D
            Style           =   1  'Graphical
            TabIndex        =   269
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame SMW 
         Caption         =   "Sizable Blocks"
         Height          =   2175
         Index           =   7
         Left            =   3480
         TabIndex        =   261
         Top             =   7560
         Width           =   3255
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   445
            Left            =   2520
            Picture         =   "frmBlocks.frx":2DE4D
            Style           =   1  'Graphical
            TabIndex        =   528
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   444
            Left            =   2520
            Picture         =   "frmBlocks.frx":2E363
            Style           =   1  'Graphical
            TabIndex        =   527
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   443
            Left            =   2520
            Picture         =   "frmBlocks.frx":2E88A
            Style           =   1  'Graphical
            TabIndex        =   526
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   442
            Left            =   1920
            Picture         =   "frmBlocks.frx":2EDB3
            Style           =   1  'Graphical
            TabIndex        =   525
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   441
            Left            =   1920
            Picture         =   "frmBlocks.frx":2F32B
            Style           =   1  'Graphical
            TabIndex        =   524
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   440
            Left            =   1920
            Picture         =   "frmBlocks.frx":2F8C0
            Style           =   1  'Graphical
            TabIndex        =   523
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   439
            Left            =   1320
            Picture         =   "frmBlocks.frx":2FF0B
            Style           =   1  'Graphical
            TabIndex        =   522
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   438
            Left            =   1320
            Picture         =   "frmBlocks.frx":30553
            Style           =   1  'Graphical
            TabIndex        =   521
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   437
            Left            =   1320
            Picture         =   "frmBlocks.frx":30BAB
            Style           =   1  'Graphical
            TabIndex        =   520
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   161
            Left            =   120
            Picture         =   "frmBlocks.frx":311DF
            Style           =   1  'Graphical
            TabIndex        =   267
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   79
            Left            =   120
            Picture         =   "frmBlocks.frx":316B6
            Style           =   1  'Graphical
            TabIndex        =   266
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   244
            Left            =   720
            Picture         =   "frmBlocks.frx":31B8A
            Style           =   1  'Graphical
            TabIndex        =   265
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   259
            Left            =   720
            Picture         =   "frmBlocks.frx":32127
            Style           =   1  'Graphical
            TabIndex        =   264
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   245
            Left            =   120
            Picture         =   "frmBlocks.frx":326C7
            Style           =   1  'Graphical
            TabIndex        =   263
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   261
            Left            =   720
            Picture         =   "frmBlocks.frx":32BA7
            Style           =   1  'Graphical
            TabIndex        =   262
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame SMW 
         Caption         =   "Switch"
         Height          =   2175
         Index           =   5
         Left            =   480
         TabIndex        =   144
         Top             =   4560
         Width           =   2655
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   181
            Left            =   1920
            Picture         =   "frmBlocks.frx":33146
            Style           =   1  'Graphical
            TabIndex        =   156
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   180
            Left            =   1920
            Picture         =   "frmBlocks.frx":334F0
            Style           =   1  'Graphical
            TabIndex        =   155
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   179
            Left            =   1920
            Picture         =   "frmBlocks.frx":338F3
            Style           =   1  'Graphical
            TabIndex        =   154
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   178
            Left            =   1320
            Picture         =   "frmBlocks.frx":33D09
            Style           =   1  'Graphical
            TabIndex        =   153
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   177
            Left            =   1320
            Picture         =   "frmBlocks.frx":340B3
            Style           =   1  'Graphical
            TabIndex        =   152
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   176
            Left            =   1320
            Picture         =   "frmBlocks.frx":344B6
            Style           =   1  'Graphical
            TabIndex        =   151
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   175
            Left            =   720
            Picture         =   "frmBlocks.frx":348CC
            Style           =   1  'Graphical
            TabIndex        =   150
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   174
            Left            =   720
            Picture         =   "frmBlocks.frx":34C76
            Style           =   1  'Graphical
            TabIndex        =   149
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   173
            Left            =   720
            Picture         =   "frmBlocks.frx":35079
            Style           =   1  'Graphical
            TabIndex        =   148
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   171
            Left            =   120
            Picture         =   "frmBlocks.frx":3548F
            Style           =   1  'Graphical
            TabIndex        =   147
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   170
            Left            =   120
            Picture         =   "frmBlocks.frx":35892
            Style           =   1  'Graphical
            TabIndex        =   146
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   172
            Left            =   120
            Picture         =   "frmBlocks.frx":35CA8
            Style           =   1  'Graphical
            TabIndex        =   145
            Top             =   1440
            Width           =   540
         End
      End
      Begin VB.Frame SMW 
         Caption         =   "Ghost House"
         Height          =   975
         Index           =   4
         Left            =   16560
         TabIndex        =   125
         Top             =   4440
         Width           =   3255
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   121
            Left            =   120
            Picture         =   "frmBlocks.frx":36052
            Style           =   1  'Graphical
            TabIndex        =   130
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   122
            Left            =   720
            Picture         =   "frmBlocks.frx":3645E
            Style           =   1  'Graphical
            TabIndex        =   129
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   123
            Left            =   1320
            Picture         =   "frmBlocks.frx":3685A
            Style           =   1  'Graphical
            TabIndex        =   128
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   124
            Left            =   2520
            Picture         =   "frmBlocks.frx":36C66
            Style           =   1  'Graphical
            TabIndex        =   127
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   125
            Left            =   1920
            Picture         =   "frmBlocks.frx":37083
            Style           =   1  'Graphical
            TabIndex        =   126
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMW 
         Caption         =   "Woods"
         Height          =   1575
         Index           =   2
         Left            =   10320
         TabIndex        =   111
         Top             =   4920
         Width           =   2055
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   116
            Left            =   120
            Picture         =   "frmBlocks.frx":374C4
            Style           =   1  'Graphical
            TabIndex        =   124
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   117
            Left            =   1320
            Picture         =   "frmBlocks.frx":378EC
            Style           =   1  'Graphical
            TabIndex        =   123
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   118
            Left            =   1320
            Picture         =   "frmBlocks.frx":37D16
            Style           =   1  'Graphical
            TabIndex        =   122
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   119
            Left            =   720
            Picture         =   "frmBlocks.frx":38117
            Style           =   1  'Graphical
            TabIndex        =   121
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   120
            Left            =   120
            Picture         =   "frmBlocks.frx":38514
            Style           =   1  'Graphical
            TabIndex        =   120
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame SMW 
         Caption         =   "Grass"
         Height          =   2175
         Index           =   0
         Left            =   2160
         TabIndex        =   110
         Top             =   240
         Width           =   5655
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   619
            Left            =   4320
            Picture         =   "frmBlocks.frx":38914
            Style           =   1  'Graphical
            TabIndex        =   755
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   618
            Left            =   4920
            Picture         =   "frmBlocks.frx":38CE5
            Style           =   1  'Graphical
            TabIndex        =   754
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   617
            Left            =   3720
            Picture         =   "frmBlocks.frx":390B3
            Style           =   1  'Graphical
            TabIndex        =   753
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   616
            Left            =   3120
            Picture         =   "frmBlocks.frx":39457
            Style           =   1  'Graphical
            TabIndex        =   752
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   489
            Left            =   2520
            Picture         =   "frmBlocks.frx":397FD
            Style           =   1  'Graphical
            TabIndex        =   611
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   488
            Left            =   1920
            Picture         =   "frmBlocks.frx":39B94
            Style           =   1  'Graphical
            TabIndex        =   610
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   300
            Left            =   3720
            Picture         =   "frmBlocks.frx":39F92
            Style           =   1  'Graphical
            TabIndex        =   454
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   299
            Left            =   3120
            Picture         =   "frmBlocks.frx":3A37C
            Style           =   1  'Graphical
            TabIndex        =   453
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   304
            Left            =   4320
            Picture         =   "frmBlocks.frx":3A764
            Style           =   1  'Graphical
            TabIndex        =   452
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   301
            Left            =   4920
            Picture         =   "frmBlocks.frx":3AAF8
            Style           =   1  'Graphical
            TabIndex        =   451
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   302
            Left            =   4320
            Picture         =   "frmBlocks.frx":3AE90
            Style           =   1  'Graphical
            TabIndex        =   450
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   303
            Left            =   4920
            Picture         =   "frmBlocks.frx":3B22B
            Style           =   1  'Graphical
            TabIndex        =   449
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   310
            Left            =   3720
            Picture         =   "frmBlocks.frx":3B5C1
            Style           =   1  'Graphical
            TabIndex        =   448
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   309
            Left            =   3120
            Picture         =   "frmBlocks.frx":3B9AA
            Style           =   1  'Graphical
            TabIndex        =   447
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   266
            Left            =   1320
            Picture         =   "frmBlocks.frx":3BD92
            Style           =   1  'Graphical
            TabIndex        =   309
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   265
            Left            =   120
            Picture         =   "frmBlocks.frx":3C1AE
            Style           =   1  'Graphical
            TabIndex        =   308
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   264
            Left            =   720
            Picture         =   "frmBlocks.frx":3C5C1
            Style           =   1  'Graphical
            TabIndex        =   307
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   263
            Left            =   2520
            Picture         =   "frmBlocks.frx":3C99D
            Style           =   1  'Graphical
            TabIndex        =   306
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   273
            Left            =   1920
            Picture         =   "frmBlocks.frx":3CD7A
            Style           =   1  'Graphical
            TabIndex        =   305
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   80
            Left            =   120
            Picture         =   "frmBlocks.frx":3D154
            Style           =   1  'Graphical
            TabIndex        =   119
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   81
            Left            =   720
            Picture         =   "frmBlocks.frx":3DD96
            Style           =   1  'Graphical
            TabIndex        =   118
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   82
            Left            =   1320
            Picture         =   "frmBlocks.frx":3E9D8
            Style           =   1  'Graphical
            TabIndex        =   117
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   83
            Left            =   120
            Picture         =   "frmBlocks.frx":3F61A
            Style           =   1  'Graphical
            TabIndex        =   116
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   84
            Left            =   1320
            Picture         =   "frmBlocks.frx":4025C
            Style           =   1  'Graphical
            TabIndex        =   115
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   85
            Left            =   1920
            Picture         =   "frmBlocks.frx":40E9E
            Style           =   1  'Graphical
            TabIndex        =   114
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   86
            Left            =   2520
            Picture         =   "frmBlocks.frx":41AE0
            Style           =   1  'Graphical
            TabIndex        =   113
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   87
            Left            =   720
            Picture         =   "frmBlocks.frx":42722
            Style           =   1  'Graphical
            TabIndex        =   112
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame SMW 
         Caption         =   "Special Blocks"
         Height          =   975
         Index           =   9
         Left            =   7560
         TabIndex        =   106
         Top             =   7200
         Width           =   5055
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   615
            Left            =   4320
            Picture         =   "frmBlocks.frx":43364
            Style           =   1  'Graphical
            TabIndex        =   751
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   283
            Left            =   3720
            Picture         =   "frmBlocks.frx":4371D
            Style           =   1  'Graphical
            TabIndex        =   338
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   282
            Left            =   3120
            Picture         =   "frmBlocks.frx":43B3A
            Style           =   1  'Graphical
            TabIndex        =   337
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   281
            Left            =   2520
            Picture         =   "frmBlocks.frx":43F3B
            Style           =   1  'Graphical
            TabIndex        =   336
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   169
            Left            =   1920
            Picture         =   "frmBlocks.frx":44384
            Style           =   1  'Graphical
            TabIndex        =   143
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   89
            Left            =   720
            Picture         =   "frmBlocks.frx":44797
            Style           =   1  'Graphical
            TabIndex        =   109
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   88
            Left            =   120
            Picture         =   "frmBlocks.frx":453D9
            Style           =   1  'Graphical
            TabIndex        =   108
            Top             =   240
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   90
            Left            =   1320
            Picture         =   "frmBlocks.frx":4601B
            Style           =   1  'Graphical
            TabIndex        =   107
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMW 
         Caption         =   "Pipes"
         Height          =   2175
         Index           =   3
         Left            =   7680
         TabIndex        =   103
         Top             =   7920
         Width           =   5055
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   139
            Left            =   1920
            Picture         =   "frmBlocks.frx":46C5D
            Style           =   1  'Graphical
            TabIndex        =   321
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   140
            Left            =   2520
            Picture         =   "frmBlocks.frx":4701B
            Style           =   1  'Graphical
            TabIndex        =   320
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   141
            Left            =   3120
            Picture         =   "frmBlocks.frx":473D9
            Style           =   1  'Graphical
            TabIndex        =   319
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   144
            Left            =   1920
            Picture         =   "frmBlocks.frx":47797
            Style           =   1  'Graphical
            TabIndex        =   318
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   145
            Left            =   2520
            Picture         =   "frmBlocks.frx":47B5E
            Style           =   1  'Graphical
            TabIndex        =   317
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   146
            Left            =   3120
            Picture         =   "frmBlocks.frx":47F25
            Style           =   1  'Graphical
            TabIndex        =   316
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   148
            Left            =   3720
            Picture         =   "frmBlocks.frx":482EC
            Style           =   1  'Graphical
            TabIndex        =   315
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   149
            Left            =   3720
            Picture         =   "frmBlocks.frx":486A7
            Style           =   1  'Graphical
            TabIndex        =   314
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   150
            Left            =   3720
            Picture         =   "frmBlocks.frx":48A62
            Style           =   1  'Graphical
            TabIndex        =   313
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   154
            Left            =   4320
            Picture         =   "frmBlocks.frx":48E1D
            Style           =   1  'Graphical
            TabIndex        =   312
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   155
            Left            =   4320
            Picture         =   "frmBlocks.frx":491A4
            Style           =   1  'Graphical
            TabIndex        =   311
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   156
            Left            =   4320
            Picture         =   "frmBlocks.frx":4952B
            Style           =   1  'Graphical
            TabIndex        =   310
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   158
            Left            =   720
            Picture         =   "frmBlocks.frx":498B2
            Style           =   1  'Graphical
            TabIndex        =   140
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   157
            Left            =   1920
            Picture         =   "frmBlocks.frx":49C39
            Style           =   1  'Graphical
            TabIndex        =   139
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   153
            Left            =   3120
            Picture         =   "frmBlocks.frx":49FC0
            Style           =   1  'Graphical
            TabIndex        =   138
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   152
            Left            =   120
            Picture         =   "frmBlocks.frx":4A347
            Style           =   1  'Graphical
            TabIndex        =   137
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   151
            Left            =   1320
            Picture         =   "frmBlocks.frx":4A702
            Style           =   1  'Graphical
            TabIndex        =   136
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   147
            Left            =   2520
            Picture         =   "frmBlocks.frx":4AABD
            Style           =   1  'Graphical
            TabIndex        =   135
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   143
            Left            =   1320
            Picture         =   "frmBlocks.frx":4AE78
            Style           =   1  'Graphical
            TabIndex        =   134
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   142
            Left            =   720
            Picture         =   "frmBlocks.frx":4B23F
            Style           =   1  'Graphical
            TabIndex        =   133
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   138
            Left            =   1320
            Picture         =   "frmBlocks.frx":4B606
            Style           =   1  'Graphical
            TabIndex        =   132
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   137
            Left            =   720
            Picture         =   "frmBlocks.frx":4B9C4
            Style           =   1  'Graphical
            TabIndex        =   131
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   113
            Left            =   120
            Picture         =   "frmBlocks.frx":4BD82
            Style           =   1  'Graphical
            TabIndex        =   105
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   114
            Left            =   120
            Picture         =   "frmBlocks.frx":4C140
            Style           =   1  'Graphical
            TabIndex        =   104
            Top             =   840
            Width           =   540
         End
      End
   End
   Begin VB.Frame Game 
      Caption         =   "Super Mario Bros."
      Height          =   2655
      Index           =   2
      Left            =   14040
      TabIndex        =   65
      Top             =   9240
      Visible         =   0   'False
      Width           =   6135
      Begin VB.Frame SMB1 
         Caption         =   "Clouds"
         Height          =   1935
         Index           =   7
         Left            =   9840
         TabIndex        =   501
         Top             =   5400
         Width           =   4935
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   372
            Left            =   1920
            Picture         =   "frmBlocks.frx":4C507
            Style           =   1  'Graphical
            TabIndex        =   509
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   382
            Left            =   1920
            Picture         =   "frmBlocks.frx":4C91F
            Style           =   1  'Graphical
            TabIndex        =   508
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   381
            Left            =   1320
            Picture         =   "frmBlocks.frx":4CD23
            Style           =   1  'Graphical
            TabIndex        =   507
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   380
            Left            =   720
            Picture         =   "frmBlocks.frx":4D10D
            Style           =   1  'Graphical
            TabIndex        =   506
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   379
            Left            =   120
            Picture         =   "frmBlocks.frx":4D500
            Style           =   1  'Graphical
            TabIndex        =   505
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   375
            Left            =   1320
            Picture         =   "frmBlocks.frx":4D905
            Style           =   1  'Graphical
            TabIndex        =   504
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   374
            Left            =   720
            Picture         =   "frmBlocks.frx":4DCF3
            Style           =   1  'Graphical
            TabIndex        =   503
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   373
            Left            =   120
            Picture         =   "frmBlocks.frx":4E0D2
            Style           =   1  'Graphical
            TabIndex        =   502
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMB1 
         Caption         =   "Dungeon"
         Height          =   2175
         Index           =   6
         Left            =   1560
         TabIndex        =   481
         Top             =   2760
         Width           =   4455
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   405
            Left            =   3120
            Picture         =   "frmBlocks.frx":4E4D4
            Style           =   1  'Graphical
            TabIndex        =   569
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   371
            Left            =   3120
            Picture         =   "frmBlocks.frx":4E838
            Style           =   1  'Graphical
            TabIndex        =   499
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   403
            Left            =   2520
            Picture         =   "frmBlocks.frx":4EC39
            Style           =   1  'Graphical
            TabIndex        =   498
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   402
            Left            =   1320
            Picture         =   "frmBlocks.frx":4F042
            Style           =   1  'Graphical
            TabIndex        =   497
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   401
            Left            =   3720
            Picture         =   "frmBlocks.frx":4F484
            Style           =   1  'Graphical
            TabIndex        =   496
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   400
            Left            =   3120
            Picture         =   "frmBlocks.frx":4F89C
            Style           =   1  'Graphical
            TabIndex        =   495
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   399
            Left            =   1920
            Picture         =   "frmBlocks.frx":4FC9E
            Style           =   1  'Graphical
            TabIndex        =   494
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   398
            Left            =   2520
            Picture         =   "frmBlocks.frx":5009B
            Style           =   1  'Graphical
            TabIndex        =   493
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   397
            Left            =   720
            Picture         =   "frmBlocks.frx":5048E
            Style           =   1  'Graphical
            TabIndex        =   492
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   396
            Left            =   120
            Picture         =   "frmBlocks.frx":508BB
            Style           =   1  'Graphical
            TabIndex        =   491
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   395
            Left            =   120
            Picture         =   "frmBlocks.frx":50CEF
            Style           =   1  'Graphical
            TabIndex        =   490
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   394
            Left            =   720
            Picture         =   "frmBlocks.frx":51136
            Style           =   1  'Graphical
            TabIndex        =   489
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   393
            Left            =   1320
            Picture         =   "frmBlocks.frx":51580
            Style           =   1  'Graphical
            TabIndex        =   488
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   390
            Left            =   1320
            Picture         =   "frmBlocks.frx":519C9
            Style           =   1  'Graphical
            TabIndex        =   487
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   387
            Left            =   720
            Picture         =   "frmBlocks.frx":51E18
            Style           =   1  'Graphical
            TabIndex        =   486
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   386
            Left            =   120
            Picture         =   "frmBlocks.frx":52239
            Style           =   1  'Graphical
            TabIndex        =   485
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   385
            Left            =   1920
            Picture         =   "frmBlocks.frx":52682
            Style           =   1  'Graphical
            TabIndex        =   484
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   384
            Left            =   1920
            Picture         =   "frmBlocks.frx":52A70
            Style           =   1  'Graphical
            TabIndex        =   483
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   383
            Left            =   2520
            Picture         =   "frmBlocks.frx":52E5B
            Style           =   1  'Graphical
            TabIndex        =   482
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame Frame3 
         Caption         =   "Tile Set"
         Height          =   2295
         Left            =   120
         TabIndex        =   440
         Top             =   240
         Width           =   1335
         Begin VB.OptionButton optSMB1 
            Caption         =   "Clouds"
            Height          =   255
            Index           =   7
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   516
            Top             =   1920
            Width           =   1095
         End
         Begin VB.OptionButton optSMB1 
            Caption         =   "Dungeon"
            Height          =   255
            Index           =   6
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   515
            Top             =   1680
            Width           =   1095
         End
         Begin VB.OptionButton optSMB1 
            Caption         =   "Pipes"
            Height          =   255
            Index           =   5
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   446
            Top             =   1440
            Width           =   1095
         End
         Begin VB.OptionButton optSMB1 
            Caption         =   "Misc"
            Height          =   255
            Index           =   4
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   445
            Top             =   1200
            Width           =   1095
         End
         Begin VB.OptionButton optSMB1 
            Caption         =   "Special"
            Height          =   255
            Index           =   3
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   444
            Top             =   960
            Width           =   1095
         End
         Begin VB.OptionButton optSMB1 
            Caption         =   "Sizable"
            Height          =   255
            Index           =   2
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   443
            Top             =   720
            Width           =   1095
         End
         Begin VB.OptionButton optSMB1 
            Caption         =   "Underground"
            Height          =   255
            Index           =   1
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   442
            Top             =   480
            Width           =   1095
         End
         Begin VB.OptionButton optSMB1 
            Caption         =   "Overworld"
            Height          =   255
            Index           =   0
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   441
            Top             =   240
            Width           =   1095
         End
      End
      Begin VB.Frame SMB1 
         Caption         =   "Sizable"
         Height          =   975
         Index           =   2
         Left            =   8400
         TabIndex        =   344
         Top             =   2640
         Width           =   855
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   287
            Left            =   120
            Picture         =   "frmBlocks.frx":53250
            Style           =   1  'Graphical
            TabIndex        =   345
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMB1 
         Caption         =   "Pipes"
         Height          =   2175
         Index           =   5
         Left            =   9960
         TabIndex        =   241
         Top             =   1320
         Width           =   2055
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   376
            Left            =   120
            Picture         =   "frmBlocks.frx":5385D
            Style           =   1  'Graphical
            TabIndex        =   514
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   378
            Left            =   1320
            Picture         =   "frmBlocks.frx":53C22
            Style           =   1  'Graphical
            TabIndex        =   513
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   197
            Left            =   720
            Picture         =   "frmBlocks.frx":54072
            Style           =   1  'Graphical
            TabIndex        =   512
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   196
            Left            =   720
            Picture         =   "frmBlocks.frx":54447
            Style           =   1  'Graphical
            TabIndex        =   511
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   377
            Left            =   720
            Picture         =   "frmBlocks.frx":54818
            Style           =   1  'Graphical
            TabIndex        =   510
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   194
            Left            =   120
            Picture         =   "frmBlocks.frx":54BB5
            Style           =   1  'Graphical
            TabIndex        =   243
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   195
            Left            =   120
            Picture         =   "frmBlocks.frx":54F86
            Style           =   1  'Graphical
            TabIndex        =   242
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame SMB1 
         Caption         =   "Misc."
         Height          =   1575
         Index           =   4
         Left            =   6360
         TabIndex        =   240
         Top             =   3720
         Width           =   3855
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   577
            Left            =   720
            Picture         =   "frmBlocks.frx":5535B
            Style           =   1  'Graphical
            TabIndex        =   710
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   576
            Left            =   120
            Picture         =   "frmBlocks.frx":55779
            Style           =   1  'Graphical
            TabIndex        =   709
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   388
            Left            =   3120
            Picture         =   "frmBlocks.frx":55BAC
            Style           =   1  'Graphical
            TabIndex        =   500
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   369
            Left            =   2520
            Picture         =   "frmBlocks.frx":56005
            Style           =   1  'Graphical
            TabIndex        =   400
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   69
            Left            =   1920
            Picture         =   "frmBlocks.frx":5643F
            Style           =   1  'Graphical
            TabIndex        =   247
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   66
            Left            =   120
            Picture         =   "frmBlocks.frx":57081
            Style           =   1  'Graphical
            TabIndex        =   246
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   67
            Left            =   720
            Picture         =   "frmBlocks.frx":57CC3
            Style           =   1  'Graphical
            TabIndex        =   245
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   68
            Left            =   1320
            Picture         =   "frmBlocks.frx":58905
            Style           =   1  'Graphical
            TabIndex        =   244
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMB1 
         Caption         =   "Undergound"
         Height          =   2175
         Index           =   1
         Left            =   6240
         TabIndex        =   239
         Top             =   240
         Width           =   3855
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   331
            Left            =   120
            Picture         =   "frmBlocks.frx":59547
            Style           =   1  'Graphical
            TabIndex        =   365
            Top             =   840
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   330
            Left            =   1320
            Picture         =   "frmBlocks.frx":59670
            Style           =   1  'Graphical
            TabIndex        =   364
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   329
            Left            =   1920
            Picture         =   "frmBlocks.frx":5979E
            Style           =   1  'Graphical
            TabIndex        =   363
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   328
            Left            =   2520
            Picture         =   "frmBlocks.frx":59B92
            Style           =   1  'Graphical
            TabIndex        =   362
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   327
            Left            =   2520
            Picture         =   "frmBlocks.frx":59F84
            Style           =   1  'Graphical
            TabIndex        =   361
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   326
            Left            =   1920
            Picture         =   "frmBlocks.frx":5A388
            Style           =   1  'Graphical
            TabIndex        =   360
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   325
            Left            =   720
            Picture         =   "frmBlocks.frx":5A772
            Style           =   1  'Graphical
            TabIndex        =   359
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   324
            Left            =   120
            Picture         =   "frmBlocks.frx":5AB47
            Style           =   1  'Graphical
            TabIndex        =   358
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   323
            Left            =   720
            Picture         =   "frmBlocks.frx":5AF1C
            Style           =   1  'Graphical
            TabIndex        =   357
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   56
            Left            =   120
            Picture         =   "frmBlocks.frx":5B026
            Style           =   1  'Graphical
            TabIndex        =   260
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   57
            Left            =   720
            Picture         =   "frmBlocks.frx":5BC68
            Style           =   1  'Graphical
            TabIndex        =   259
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   58
            Left            =   1320
            Picture         =   "frmBlocks.frx":5C8AA
            Style           =   1  'Graphical
            TabIndex        =   258
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   59
            Left            =   3120
            Picture         =   "frmBlocks.frx":5D4EC
            Style           =   1  'Graphical
            TabIndex        =   257
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   61
            Left            =   3120
            Picture         =   "frmBlocks.frx":5E12E
            Style           =   1  'Graphical
            TabIndex        =   256
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMB1 
         Caption         =   "Overworld"
         Height          =   1575
         Index           =   0
         Left            =   1560
         TabIndex        =   238
         Top             =   240
         Width           =   3855
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   335
            Left            =   1920
            Picture         =   "frmBlocks.frx":5E599
            Style           =   1  'Graphical
            TabIndex        =   369
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   334
            Left            =   2520
            Picture         =   "frmBlocks.frx":5E98D
            Style           =   1  'Graphical
            TabIndex        =   368
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   333
            Left            =   2520
            Picture         =   "frmBlocks.frx":5ED7F
            Style           =   1  'Graphical
            TabIndex        =   367
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   332
            Left            =   1920
            Picture         =   "frmBlocks.frx":5F183
            Style           =   1  'Graphical
            TabIndex        =   366
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   64
            Left            =   1320
            Picture         =   "frmBlocks.frx":5F56D
            Style           =   1  'Graphical
            TabIndex        =   255
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   62
            Left            =   120
            Picture         =   "frmBlocks.frx":601AF
            Style           =   1  'Graphical
            TabIndex        =   254
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   63
            Left            =   720
            Picture         =   "frmBlocks.frx":60DF1
            Style           =   1  'Graphical
            TabIndex        =   253
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   65
            Left            =   3120
            Picture         =   "frmBlocks.frx":61A33
            Style           =   1  'Graphical
            TabIndex        =   252
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   93
            Left            =   3120
            Picture         =   "frmBlocks.frx":62675
            Style           =   1  'Graphical
            TabIndex        =   251
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   189
            Left            =   120
            Picture         =   "frmBlocks.frx":62A89
            Style           =   1  'Graphical
            TabIndex        =   250
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   190
            Left            =   720
            Picture         =   "frmBlocks.frx":62EBE
            Style           =   1  'Graphical
            TabIndex        =   249
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   191
            Left            =   1320
            Picture         =   "frmBlocks.frx":632C0
            Style           =   1  'Graphical
            TabIndex        =   248
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame SMB1 
         Caption         =   "Special Blocks"
         Height          =   975
         Index           =   3
         Left            =   6360
         TabIndex        =   160
         Top             =   2640
         Width           =   2655
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   192
            Left            =   720
            Picture         =   "frmBlocks.frx":636EE
            Style           =   1  'Graphical
            TabIndex        =   164
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   60
            Left            =   1920
            Picture         =   "frmBlocks.frx":63ACC
            Style           =   1  'Graphical
            TabIndex        =   163
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   188
            Left            =   1320
            Picture         =   "frmBlocks.frx":6470E
            Style           =   1  'Graphical
            TabIndex        =   162
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   193
            Left            =   120
            Picture         =   "frmBlocks.frx":64AFF
            Style           =   1  'Graphical
            TabIndex        =   161
            Top             =   240
            Value           =   -1  'True
            Width           =   540
         End
      End
   End
   Begin VB.Frame Frame 
      Caption         =   "Invisible"
      Height          =   615
      Index           =   32
      Left            =   1680
      TabIndex        =   63
      Top             =   1800
      Width           =   975
      Begin VB.CommandButton cmdInvis 
         Caption         =   "No"
         Height          =   255
         Left            =   120
         TabIndex        =   64
         Top             =   240
         Width           =   735
      End
   End
   Begin VB.Frame Game 
      Caption         =   "Super Mario Brothers 2"
      Height          =   2535
      Index           =   1
      Left            =   960
      TabIndex        =   51
      Top             =   8760
      Visible         =   0   'False
      Width           =   6735
      Begin VB.Frame SMB2 
         Caption         =   "Castle"
         Height          =   2175
         Index           =   5
         Left            =   1560
         TabIndex        =   613
         Top             =   5040
         Width           =   4455
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   511
            Left            =   3720
            Picture         =   "frmBlocks.frx":64F12
            Style           =   1  'Graphical
            TabIndex        =   634
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   510
            Left            =   3720
            Picture         =   "frmBlocks.frx":65324
            Style           =   1  'Graphical
            TabIndex        =   633
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   508
            Left            =   1320
            Picture         =   "frmBlocks.frx":65741
            Style           =   1  'Graphical
            TabIndex        =   632
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   507
            Left            =   120
            Picture         =   "frmBlocks.frx":65B3B
            Style           =   1  'Graphical
            TabIndex        =   631
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   506
            Left            =   720
            Picture         =   "frmBlocks.frx":65F35
            Style           =   1  'Graphical
            TabIndex        =   630
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   504
            Left            =   3120
            Picture         =   "frmBlocks.frx":663C5
            Style           =   1  'Graphical
            TabIndex        =   629
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   503
            Left            =   1320
            Picture         =   "frmBlocks.frx":667F5
            Style           =   1  'Graphical
            TabIndex        =   628
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   502
            Left            =   2520
            Picture         =   "frmBlocks.frx":66C19
            Style           =   1  'Graphical
            TabIndex        =   627
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   501
            Left            =   1320
            Picture         =   "frmBlocks.frx":67023
            Style           =   1  'Graphical
            TabIndex        =   626
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   500
            Left            =   1920
            Picture         =   "frmBlocks.frx":6743A
            Style           =   1  'Graphical
            TabIndex        =   625
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   499
            Left            =   120
            Picture         =   "frmBlocks.frx":6784F
            Style           =   1  'Graphical
            TabIndex        =   624
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   498
            Left            =   3120
            Picture         =   "frmBlocks.frx":67C73
            Style           =   1  'Graphical
            TabIndex        =   623
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   497
            Left            =   1920
            Picture         =   "frmBlocks.frx":6808C
            Style           =   1  'Graphical
            TabIndex        =   622
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   496
            Left            =   720
            Picture         =   "frmBlocks.frx":684A5
            Style           =   1  'Graphical
            TabIndex        =   621
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   495
            Left            =   720
            Picture         =   "frmBlocks.frx":688BA
            Style           =   1  'Graphical
            TabIndex        =   620
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   494
            Left            =   2520
            Picture         =   "frmBlocks.frx":68CD3
            Style           =   1  'Graphical
            TabIndex        =   619
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   493
            Left            =   2520
            Picture         =   "frmBlocks.frx":690E7
            Style           =   1  'Graphical
            TabIndex        =   618
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   492
            Left            =   120
            Picture         =   "frmBlocks.frx":694FE
            Style           =   1  'Graphical
            TabIndex        =   617
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   491
            Left            =   1920
            Picture         =   "frmBlocks.frx":69915
            Style           =   1  'Graphical
            TabIndex        =   616
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   490
            Left            =   3120
            Picture         =   "frmBlocks.frx":69D3F
            Style           =   1  'Graphical
            TabIndex        =   615
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame6 
         Caption         =   "Tile Set"
         Height          =   1815
         Left            =   120
         TabIndex        =   434
         Top             =   240
         Width           =   1335
         Begin VB.OptionButton optSMB2 
            Caption         =   "Castle"
            Height          =   255
            Index           =   5
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   635
            Top             =   1440
            Width           =   1095
         End
         Begin VB.OptionButton optSMB2 
            Caption         =   "Misc"
            Height          =   255
            Index           =   4
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   439
            Top             =   1200
            Width           =   1095
         End
         Begin VB.OptionButton optSMB2 
            Caption         =   "Special"
            Height          =   255
            Index           =   3
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   438
            Top             =   960
            Width           =   1095
         End
         Begin VB.OptionButton optSMB2 
            Caption         =   "Sizable"
            Height          =   255
            Index           =   2
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   437
            Top             =   720
            Width           =   1095
         End
         Begin VB.OptionButton optSMB2 
            Caption         =   "Underground"
            Height          =   255
            Index           =   1
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   436
            Top             =   480
            Width           =   1095
         End
         Begin VB.OptionButton optSMB2 
            Caption         =   "Overworld"
            Height          =   255
            Index           =   0
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   435
            Top             =   240
            Width           =   1095
         End
      End
      Begin VB.Frame SMB2 
         Caption         =   "Special"
         Height          =   1575
         Index           =   3
         Left            =   4920
         TabIndex        =   353
         Top             =   2760
         Width           =   855
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   370
            Left            =   120
            Picture         =   "frmBlocks.frx":6A154
            Style           =   1  'Graphical
            TabIndex        =   401
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   293
            Left            =   120
            Picture         =   "frmBlocks.frx":6A5B2
            Style           =   1  'Graphical
            TabIndex        =   354
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMB2 
         Caption         =   "Overworld"
         Height          =   2175
         Index           =   0
         Left            =   1560
         TabIndex        =   213
         Top             =   240
         Width           =   5055
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   343
            Left            =   4320
            Picture         =   "frmBlocks.frx":6A9F6
            Style           =   1  'Graphical
            TabIndex        =   377
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   342
            Left            =   3120
            Picture         =   "frmBlocks.frx":6ADF0
            Style           =   1  'Graphical
            TabIndex        =   376
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   341
            Left            =   3720
            Picture         =   "frmBlocks.frx":6B1AC
            Style           =   1  'Graphical
            TabIndex        =   375
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   340
            Left            =   2520
            Picture         =   "frmBlocks.frx":6B5A4
            Style           =   1  'Graphical
            TabIndex        =   374
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   339
            Left            =   4320
            Picture         =   "frmBlocks.frx":6B961
            Style           =   1  'Graphical
            TabIndex        =   373
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   338
            Left            =   3120
            Picture         =   "frmBlocks.frx":6BADE
            Style           =   1  'Graphical
            TabIndex        =   372
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   337
            Left            =   3720
            Picture         =   "frmBlocks.frx":6BEF5
            Style           =   1  'Graphical
            TabIndex        =   371
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   336
            Left            =   2520
            Picture         =   "frmBlocks.frx":6C07A
            Style           =   1  'Graphical
            TabIndex        =   370
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   44
            Left            =   1320
            Picture         =   "frmBlocks.frx":6C492
            Style           =   1  'Graphical
            TabIndex        =   237
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   43
            Left            =   720
            Picture         =   "frmBlocks.frx":6D0D4
            Style           =   1  'Graphical
            TabIndex        =   236
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   42
            Left            =   120
            Picture         =   "frmBlocks.frx":6DD16
            Style           =   1  'Graphical
            TabIndex        =   235
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   46
            Left            =   1920
            Picture         =   "frmBlocks.frx":6E958
            Style           =   1  'Graphical
            TabIndex        =   234
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   45
            Left            =   1920
            Picture         =   "frmBlocks.frx":6F59A
            Style           =   1  'Graphical
            TabIndex        =   233
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   41
            Left            =   1320
            Picture         =   "frmBlocks.frx":701DC
            Style           =   1  'Graphical
            TabIndex        =   232
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   39
            Left            =   120
            Picture         =   "frmBlocks.frx":70E1E
            Style           =   1  'Graphical
            TabIndex        =   231
            Top             =   240
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   40
            Left            =   720
            Picture         =   "frmBlocks.frx":71A60
            Style           =   1  'Graphical
            TabIndex        =   230
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   198
            Left            =   120
            Picture         =   "frmBlocks.frx":726A2
            Style           =   1  'Graphical
            TabIndex        =   229
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   199
            Left            =   1320
            Picture         =   "frmBlocks.frx":72AFB
            Style           =   1  'Graphical
            TabIndex        =   228
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   200
            Left            =   720
            Picture         =   "frmBlocks.frx":72F6E
            Style           =   1  'Graphical
            TabIndex        =   227
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame SMB2 
         Caption         =   "Underground"
         Height          =   2175
         Index           =   1
         Left            =   1560
         TabIndex        =   212
         Top             =   2640
         Width           =   3255
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   206
            Left            =   1920
            Picture         =   "frmBlocks.frx":733C8
            Style           =   1  'Graphical
            TabIndex        =   226
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   201
            Left            =   720
            Picture         =   "frmBlocks.frx":73863
            Style           =   1  'Graphical
            TabIndex        =   225
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   203
            Left            =   120
            Picture         =   "frmBlocks.frx":73CA8
            Style           =   1  'Graphical
            TabIndex        =   224
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   204
            Left            =   1320
            Picture         =   "frmBlocks.frx":74104
            Style           =   1  'Graphical
            TabIndex        =   223
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   205
            Left            =   720
            Picture         =   "frmBlocks.frx":74572
            Style           =   1  'Graphical
            TabIndex        =   222
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   207
            Left            =   120
            Picture         =   "frmBlocks.frx":74987
            Style           =   1  'Graphical
            TabIndex        =   221
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   208
            Left            =   1320
            Picture         =   "frmBlocks.frx":74DF2
            Style           =   1  'Graphical
            TabIndex        =   220
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   211
            Left            =   1920
            Picture         =   "frmBlocks.frx":7526E
            Style           =   1  'Graphical
            TabIndex        =   219
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   212
            Left            =   2520
            Picture         =   "frmBlocks.frx":7567E
            Style           =   1  'Graphical
            TabIndex        =   218
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   213
            Left            =   720
            Picture         =   "frmBlocks.frx":75A83
            Style           =   1  'Graphical
            TabIndex        =   217
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   214
            Left            =   120
            Picture         =   "frmBlocks.frx":75ED1
            Style           =   1  'Graphical
            TabIndex        =   216
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   215
            Left            =   1320
            Picture         =   "frmBlocks.frx":7633C
            Style           =   1  'Graphical
            TabIndex        =   215
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   216
            Left            =   1920
            Picture         =   "frmBlocks.frx":76780
            Style           =   1  'Graphical
            TabIndex        =   214
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame SMB2 
         Caption         =   "Misc."
         Height          =   2175
         Index           =   4
         Left            =   6360
         TabIndex        =   207
         Top             =   3720
         Width           =   3255
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   574
            Left            =   2520
            Picture         =   "frmBlocks.frx":76BB4
            Style           =   1  'Graphical
            TabIndex        =   702
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   573
            Left            =   2520
            Picture         =   "frmBlocks.frx":76FDB
            Style           =   1  'Graphical
            TabIndex        =   701
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   505
            Left            =   1920
            Picture         =   "frmBlocks.frx":7740B
            Style           =   1  'Graphical
            TabIndex        =   614
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   509
            Left            =   1320
            Picture         =   "frmBlocks.frx":777FB
            Style           =   1  'Graphical
            TabIndex        =   612
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   392
            Left            =   1320
            Picture         =   "frmBlocks.frx":77C20
            Style           =   1  'Graphical
            TabIndex        =   519
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   391
            Left            =   720
            Picture         =   "frmBlocks.frx":78018
            Style           =   1  'Graphical
            TabIndex        =   518
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   389
            Left            =   1920
            Picture         =   "frmBlocks.frx":78433
            Style           =   1  'Graphical
            TabIndex        =   517
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   298
            Left            =   720
            Picture         =   "frmBlocks.frx":78855
            Style           =   1  'Graphical
            TabIndex        =   356
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   297
            Left            =   720
            Picture         =   "frmBlocks.frx":78CAC
            Style           =   1  'Graphical
            TabIndex        =   355
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   131
            Left            =   1320
            Picture         =   "frmBlocks.frx":79101
            Style           =   1  'Graphical
            TabIndex        =   211
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   202
            Left            =   120
            Picture         =   "frmBlocks.frx":79518
            Style           =   1  'Graphical
            TabIndex        =   210
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   209
            Left            =   120
            Picture         =   "frmBlocks.frx":79972
            Style           =   1  'Graphical
            TabIndex        =   209
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   210
            Left            =   120
            Picture         =   "frmBlocks.frx":79DC2
            Style           =   1  'Graphical
            TabIndex        =   208
            Top             =   1440
            Width           =   540
         End
      End
      Begin VB.Frame SMB2 
         Caption         =   "Sizable Blocks"
         Height          =   975
         Index           =   2
         Left            =   6840
         TabIndex        =   203
         Top             =   240
         Width           =   2655
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   288
            Left            =   1920
            Picture         =   "frmBlocks.frx":7A219
            Style           =   1  'Graphical
            TabIndex        =   343
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00000000&
            BeginProperty Font 
               Name            =   "Arial Black"
               Size            =   15.75
               Charset         =   0
               Weight          =   900
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   540
            Index           =   243
            Left            =   1320
            MaskColor       =   &H00000000&
            Picture         =   "frmBlocks.frx":7A818
            Style           =   1  'Graphical
            TabIndex        =   206
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00000000&
            BeginProperty Font 
               Name            =   "Arial Black"
               Size            =   15.75
               Charset         =   0
               Weight          =   900
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   540
            Index           =   242
            Left            =   720
            MaskColor       =   &H00000000&
            Picture         =   "frmBlocks.frx":7AE42
            Style           =   1  'Graphical
            TabIndex        =   205
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00000000&
            BeginProperty Font 
               Name            =   "Arial Black"
               Size            =   15.75
               Charset         =   0
               Weight          =   900
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   540
            Index           =   38
            Left            =   120
            MaskColor       =   &H00000000&
            Picture         =   "frmBlocks.frx":7B472
            Style           =   1  'Graphical
            TabIndex        =   204
            Top             =   240
            Width           =   540
         End
      End
   End
   Begin VB.Frame Game 
      Caption         =   "Super Mario Bros. 3"
      Height          =   2655
      Index           =   0
      Left            =   2760
      TabIndex        =   1
      Top             =   840
      Width           =   8535
      Begin VB.Frame SMB3 
         Caption         =   "Water"
         Height          =   2175
         Index           =   14
         Left            =   6240
         TabIndex        =   715
         Top             =   2640
         Width           =   3855
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   599
            Left            =   1320
            Picture         =   "frmBlocks.frx":7BA92
            Style           =   1  'Graphical
            TabIndex        =   735
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   598
            Left            =   120
            Picture         =   "frmBlocks.frx":7BEDE
            Style           =   1  'Graphical
            TabIndex        =   733
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   578
            Left            =   720
            Picture         =   "frmBlocks.frx":7C31C
            Style           =   1  'Graphical
            TabIndex        =   731
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   594
            Left            =   3120
            Picture         =   "frmBlocks.frx":7C738
            Style           =   1  'Graphical
            TabIndex        =   730
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   593
            Left            =   2520
            Picture         =   "frmBlocks.frx":7CB03
            Style           =   1  'Graphical
            TabIndex        =   729
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   592
            Left            =   1920
            Picture         =   "frmBlocks.frx":7CE9A
            Style           =   1  'Graphical
            TabIndex        =   728
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   591
            Left            =   3120
            Picture         =   "frmBlocks.frx":7D270
            Style           =   1  'Graphical
            TabIndex        =   727
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   590
            Left            =   2520
            Picture         =   "frmBlocks.frx":7D617
            Style           =   1  'Graphical
            TabIndex        =   726
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   589
            Left            =   1920
            Picture         =   "frmBlocks.frx":7D97B
            Style           =   1  'Graphical
            TabIndex        =   725
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   588
            Left            =   3120
            Picture         =   "frmBlocks.frx":7DD1D
            Style           =   1  'Graphical
            TabIndex        =   724
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   587
            Left            =   2520
            Picture         =   "frmBlocks.frx":7E0DE
            Style           =   1  'Graphical
            TabIndex        =   723
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   586
            Left            =   1920
            Picture         =   "frmBlocks.frx":7E45C
            Style           =   1  'Graphical
            TabIndex        =   722
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   585
            Left            =   1320
            Picture         =   "frmBlocks.frx":7E80D
            Style           =   1  'Graphical
            TabIndex        =   721
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   584
            Left            =   720
            Picture         =   "frmBlocks.frx":7EC59
            Style           =   1  'Graphical
            TabIndex        =   720
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   583
            Left            =   120
            Picture         =   "frmBlocks.frx":7F096
            Style           =   1  'Graphical
            TabIndex        =   719
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   582
            Left            =   1320
            Picture         =   "frmBlocks.frx":7F4DC
            Style           =   1  'Graphical
            TabIndex        =   718
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   581
            Left            =   720
            Picture         =   "frmBlocks.frx":7F919
            Style           =   1  'Graphical
            TabIndex        =   717
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   580
            Left            =   120
            Picture         =   "frmBlocks.frx":7FD51
            Style           =   1  'Graphical
            TabIndex        =   716
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame Frame5 
         Caption         =   "Tile Set"
         Height          =   2295
         Left            =   120
         TabIndex        =   412
         Top             =   240
         Width           =   1935
         Begin VB.OptionButton optSMB3 
            Caption         =   "Water"
            Height          =   255
            Index           =   14
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   714
            Top             =   1920
            Width           =   855
         End
         Begin VB.OptionButton optSMB3 
            Caption         =   "Special"
            Height          =   255
            Index           =   6
            Left            =   960
            Style           =   1  'Graphical
            TabIndex        =   429
            Top             =   1680
            Width           =   855
         End
         Begin VB.OptionButton optSMB3 
            Caption         =   "Misc."
            Height          =   255
            Index           =   11
            Left            =   960
            Style           =   1  'Graphical
            TabIndex        =   428
            Top             =   1440
            Width           =   855
         End
         Begin VB.OptionButton optSMB3 
            Caption         =   "Sizable"
            Height          =   255
            Index           =   10
            Left            =   960
            Style           =   1  'Graphical
            TabIndex        =   427
            Top             =   1200
            Width           =   855
         End
         Begin VB.OptionButton optSMB3 
            Caption         =   "Pipes"
            Height          =   255
            Index           =   9
            Left            =   960
            Style           =   1  'Graphical
            TabIndex        =   426
            Top             =   960
            Width           =   855
         End
         Begin VB.OptionButton optSMB3 
            Caption         =   "Hurts"
            Height          =   255
            Index           =   12
            Left            =   960
            Style           =   1  'Graphical
            TabIndex        =   425
            Top             =   720
            Width           =   855
         End
         Begin VB.OptionButton optSMB3 
            Caption         =   "Airship"
            Height          =   255
            Index           =   13
            Left            =   960
            Style           =   1  'Graphical
            TabIndex        =   424
            Top             =   480
            Width           =   855
         End
         Begin VB.OptionButton optSMB3 
            Caption         =   "Snow"
            Height          =   255
            Index           =   8
            Left            =   960
            Style           =   1  'Graphical
            TabIndex        =   423
            Top             =   240
            Width           =   855
         End
         Begin VB.OptionButton optSMB3 
            Caption         =   "Dungeon"
            Height          =   255
            Index           =   7
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   422
            Top             =   1680
            Width           =   855
         End
         Begin VB.OptionButton optSMB3 
            Caption         =   "House"
            Height          =   255
            Index           =   5
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   421
            Top             =   1440
            Width           =   855
         End
         Begin VB.OptionButton optSMB3 
            Caption         =   "Bonus"
            Height          =   255
            Index           =   4
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   420
            Top             =   1200
            Width           =   855
         End
         Begin VB.OptionButton optSMB3 
            Caption         =   "Desert"
            Height          =   255
            Index           =   3
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   419
            Top             =   960
            Width           =   855
         End
         Begin VB.OptionButton optSMB3 
            Caption         =   "Cave"
            Height          =   255
            Index           =   2
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   418
            Top             =   720
            Width           =   855
         End
         Begin VB.OptionButton optSMB3 
            Caption         =   "Grass"
            Height          =   255
            Index           =   1
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   417
            Top             =   480
            Width           =   855
         End
         Begin VB.OptionButton optSMB3 
            Caption         =   "Wood"
            Height          =   255
            Index           =   0
            Left            =   120
            Style           =   1  'Graphical
            TabIndex        =   416
            Top             =   240
            Value           =   -1  'True
            Width           =   855
         End
      End
      Begin VB.Frame SMB3 
         Caption         =   "Cave"
         Height          =   2175
         Index           =   2
         Left            =   2160
         TabIndex        =   378
         Top             =   9240
         Width           =   5655
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   364
            Left            =   4920
            Picture         =   "frmBlocks.frx":80191
            Style           =   1  'Graphical
            TabIndex        =   399
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   363
            Left            =   3720
            Picture         =   "frmBlocks.frx":80556
            Style           =   1  'Graphical
            TabIndex        =   398
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   362
            Left            =   3120
            Picture         =   "frmBlocks.frx":8097B
            Style           =   1  'Graphical
            TabIndex        =   397
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   361
            Left            =   4320
            Picture         =   "frmBlocks.frx":80D86
            Style           =   1  'Graphical
            TabIndex        =   396
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   360
            Left            =   4920
            Picture         =   "frmBlocks.frx":81148
            Style           =   1  'Graphical
            TabIndex        =   395
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   359
            Left            =   3720
            Picture         =   "frmBlocks.frx":81519
            Style           =   1  'Graphical
            TabIndex        =   394
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   358
            Left            =   3120
            Picture         =   "frmBlocks.frx":81924
            Style           =   1  'Graphical
            TabIndex        =   393
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   357
            Left            =   4320
            Picture         =   "frmBlocks.frx":81D42
            Style           =   1  'Graphical
            TabIndex        =   392
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   356
            Left            =   2520
            Picture         =   "frmBlocks.frx":82110
            Style           =   1  'Graphical
            TabIndex        =   391
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   355
            Left            =   1920
            Picture         =   "frmBlocks.frx":8253E
            Style           =   1  'Graphical
            TabIndex        =   390
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   354
            Left            =   2520
            Picture         =   "frmBlocks.frx":82973
            Style           =   1  'Graphical
            TabIndex        =   389
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   353
            Left            =   1920
            Picture         =   "frmBlocks.frx":82DA0
            Style           =   1  'Graphical
            TabIndex        =   388
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   352
            Left            =   1320
            Picture         =   "frmBlocks.frx":831C5
            Style           =   1  'Graphical
            TabIndex        =   387
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   351
            Left            =   720
            Picture         =   "frmBlocks.frx":83626
            Style           =   1  'Graphical
            TabIndex        =   386
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   350
            Left            =   120
            Picture         =   "frmBlocks.frx":83A62
            Style           =   1  'Graphical
            TabIndex        =   385
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   349
            Left            =   1320
            Picture         =   "frmBlocks.frx":83EBC
            Style           =   1  'Graphical
            TabIndex        =   384
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   348
            Left            =   720
            Picture         =   "frmBlocks.frx":842FC
            Style           =   1  'Graphical
            TabIndex        =   383
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   347
            Left            =   120
            Picture         =   "frmBlocks.frx":8470E
            Style           =   1  'Graphical
            TabIndex        =   382
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   346
            Left            =   1320
            Picture         =   "frmBlocks.frx":84B54
            Style           =   1  'Graphical
            TabIndex        =   381
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   345
            Left            =   720
            Picture         =   "frmBlocks.frx":84F9B
            Style           =   1  'Graphical
            TabIndex        =   380
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   344
            Left            =   120
            Picture         =   "frmBlocks.frx":853E2
            Style           =   1  'Graphical
            TabIndex        =   379
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMB3 
         Caption         =   "Snow"
         Height          =   855
         Index           =   8
         Left            =   9720
         TabIndex        =   322
         Top             =   7200
         Width           =   3135
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   634
            Left            =   2520
            Picture         =   "frmBlocks.frx":8582E
            Style           =   1  'Graphical
            TabIndex        =   774
            Top             =   240
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   633
            Left            =   1920
            Picture         =   "frmBlocks.frx":85CC2
            Style           =   1  'Graphical
            TabIndex        =   773
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   270
            Left            =   120
            Picture         =   "frmBlocks.frx":86107
            Style           =   1  'Graphical
            TabIndex        =   325
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   271
            Left            =   1320
            Picture         =   "frmBlocks.frx":86534
            Style           =   1  'Graphical
            TabIndex        =   324
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   272
            Left            =   720
            Picture         =   "frmBlocks.frx":86962
            Style           =   1  'Graphical
            TabIndex        =   323
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMB3 
         Caption         =   "House"
         Height          =   2175
         Index           =   5
         Left            =   240
         TabIndex        =   192
         Top             =   3600
         Width           =   1455
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   221
            Left            =   120
            Picture         =   "frmBlocks.frx":86D66
            Style           =   1  'Graphical
            TabIndex        =   198
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   220
            Left            =   720
            Picture         =   "frmBlocks.frx":8716C
            Style           =   1  'Graphical
            TabIndex        =   197
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   219
            Left            =   120
            Picture         =   "frmBlocks.frx":87572
            Style           =   1  'Graphical
            TabIndex        =   196
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   218
            Left            =   720
            Picture         =   "frmBlocks.frx":87976
            Style           =   1  'Graphical
            TabIndex        =   195
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   217
            Left            =   120
            Picture         =   "frmBlocks.frx":87D2B
            Style           =   1  'Graphical
            TabIndex        =   194
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   222
            Left            =   720
            Picture         =   "frmBlocks.frx":88135
            Style           =   1  'Graphical
            TabIndex        =   193
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame SMB3 
         Caption         =   "Hurts"
         Height          =   2175
         Index           =   12
         Left            =   7320
         TabIndex        =   99
         Top             =   6960
         Width           =   2055
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   406
            Left            =   720
            Picture         =   "frmBlocks.frx":8853C
            Style           =   1  'Graphical
            TabIndex        =   570
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   267
            Left            =   720
            Picture         =   "frmBlocks.frx":888A0
            Style           =   1  'Graphical
            TabIndex        =   304
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   268
            Left            =   120
            Picture         =   "frmBlocks.frx":88C90
            Style           =   1  'Graphical
            TabIndex        =   303
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   269
            Left            =   120
            Picture         =   "frmBlocks.frx":890C1
            Style           =   1  'Graphical
            TabIndex        =   302
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   30
            Left            =   720
            Picture         =   "frmBlocks.frx":894B0
            Style           =   1  'Graphical
            TabIndex        =   181
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   110
            Left            =   120
            Picture         =   "frmBlocks.frx":8A0F2
            Style           =   1  'Graphical
            TabIndex        =   101
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   109
            Left            =   1320
            Picture         =   "frmBlocks.frx":8A51B
            Style           =   1  'Graphical
            TabIndex        =   100
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMB3 
         Caption         =   "Misc."
         Height          =   2175
         Index           =   11
         Left            =   2160
         TabIndex        =   92
         Top             =   2640
         Width           =   3855
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   132
            Left            =   3120
            Picture         =   "frmBlocks.frx":8A943
            Style           =   1  'Graphical
            TabIndex        =   734
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   572
            Left            =   3120
            Picture         =   "frmBlocks.frx":8AD63
            Style           =   1  'Graphical
            TabIndex        =   708
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   457
            Left            =   3120
            Picture         =   "frmBlocks.frx":8B15E
            Style           =   1  'Graphical
            TabIndex        =   576
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   458
            Left            =   2520
            Picture         =   "frmBlocks.frx":8B280
            Style           =   1  'Graphical
            TabIndex        =   575
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   168
            Left            =   1320
            Picture         =   "frmBlocks.frx":8B38F
            Style           =   1  'Graphical
            TabIndex        =   411
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   8
            Left            =   120
            Picture         =   "frmBlocks.frx":8B73E
            Style           =   1  'Graphical
            TabIndex        =   410
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   290
            Left            =   120
            Picture         =   "frmBlocks.frx":8C380
            Style           =   1  'Graphical
            TabIndex        =   347
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   289
            Left            =   1320
            Picture         =   "frmBlocks.frx":8C772
            Style           =   1  'Graphical
            TabIndex        =   346
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   280
            Left            =   720
            Picture         =   "frmBlocks.frx":8CB5C
            Style           =   1  'Graphical
            TabIndex        =   333
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   107
            Left            =   720
            Picture         =   "frmBlocks.frx":8CF46
            Style           =   1  'Graphical
            TabIndex        =   202
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   106
            Left            =   120
            Picture         =   "frmBlocks.frx":8D35B
            Style           =   1  'Graphical
            TabIndex        =   201
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   105
            Left            =   720
            Picture         =   "frmBlocks.frx":8D708
            Style           =   1  'Graphical
            TabIndex        =   200
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   184
            Left            =   1920
            Picture         =   "frmBlocks.frx":8DB4B
            Style           =   1  'Graphical
            TabIndex        =   167
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   223
            Left            =   2520
            Picture         =   "frmBlocks.frx":8DF8E
            Style           =   1  'Graphical
            TabIndex        =   166
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   187
            Left            =   1320
            Picture         =   "frmBlocks.frx":8E3B7
            Style           =   1  'Graphical
            TabIndex        =   159
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   160
            Left            =   2520
            Picture         =   "frmBlocks.frx":8E7D3
            Style           =   1  'Graphical
            TabIndex        =   142
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   112
            Left            =   1920
            Picture         =   "frmBlocks.frx":8EBDA
            Style           =   1  'Graphical
            TabIndex        =   94
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   111
            Left            =   1920
            Picture         =   "frmBlocks.frx":8EFF3
            Style           =   1  'Graphical
            TabIndex        =   93
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame SMB3 
         Caption         =   "Desert"
         Height          =   2175
         Index           =   3
         Left            =   9480
         TabIndex        =   81
         Top             =   4680
         Width           =   4935
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   638
            Left            =   4320
            Picture         =   "frmBlocks.frx":8F399
            Style           =   1  'Graphical
            TabIndex        =   779
            Top             =   840
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   637
            Left            =   4320
            Picture         =   "frmBlocks.frx":8F46E
            Style           =   1  'Graphical
            TabIndex        =   778
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   636
            Left            =   3720
            Picture         =   "frmBlocks.frx":8F56C
            Style           =   1  'Graphical
            TabIndex        =   777
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   635
            Left            =   3720
            Picture         =   "frmBlocks.frx":8F643
            Style           =   1  'Graphical
            TabIndex        =   776
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   286
            Left            =   1920
            Picture         =   "frmBlocks.frx":8F742
            Style           =   1  'Graphical
            TabIndex        =   342
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   285
            Left            =   2520
            Picture         =   "frmBlocks.frx":8FB76
            Style           =   1  'Graphical
            TabIndex        =   341
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   284
            Left            =   3120
            Picture         =   "frmBlocks.frx":8FF79
            Style           =   1  'Graphical
            TabIndex        =   340
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   167
            Left            =   3120
            Picture         =   "frmBlocks.frx":90396
            Style           =   1  'Graphical
            TabIndex        =   190
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   162
            Left            =   1920
            Picture         =   "frmBlocks.frx":907C9
            Style           =   1  'Graphical
            TabIndex        =   189
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   163
            Left            =   2520
            Picture         =   "frmBlocks.frx":90BE6
            Style           =   1  'Graphical
            TabIndex        =   188
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   164
            Left            =   3120
            Picture         =   "frmBlocks.frx":90FE9
            Style           =   1  'Graphical
            TabIndex        =   187
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   165
            Left            =   1920
            Picture         =   "frmBlocks.frx":91413
            Style           =   1  'Graphical
            TabIndex        =   186
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   166
            Left            =   2520
            Picture         =   "frmBlocks.frx":91835
            Style           =   1  'Graphical
            TabIndex        =   185
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   102
            Left            =   1320
            Picture         =   "frmBlocks.frx":91C31
            Style           =   1  'Graphical
            TabIndex        =   90
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   101
            Left            =   720
            Picture         =   "frmBlocks.frx":9205E
            Style           =   1  'Graphical
            TabIndex        =   89
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   100
            Left            =   120
            Picture         =   "frmBlocks.frx":9247D
            Style           =   1  'Graphical
            TabIndex        =   88
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   99
            Left            =   1320
            Picture         =   "frmBlocks.frx":928BD
            Style           =   1  'Graphical
            TabIndex        =   87
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   98
            Left            =   720
            Picture         =   "frmBlocks.frx":92CEA
            Style           =   1  'Graphical
            TabIndex        =   86
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   97
            Left            =   120
            Picture         =   "frmBlocks.frx":930F8
            Style           =   1  'Graphical
            TabIndex        =   85
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   96
            Left            =   1320
            Picture         =   "frmBlocks.frx":9351D
            Style           =   1  'Graphical
            TabIndex        =   84
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   95
            Left            =   720
            Picture         =   "frmBlocks.frx":93950
            Style           =   1  'Graphical
            TabIndex        =   83
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Height          =   540
            Index           =   94
            Left            =   120
            Picture         =   "frmBlocks.frx":93D5C
            Style           =   1  'Graphical
            TabIndex        =   82
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMB3 
         Caption         =   "Airship / Tank"
         Height          =   2175
         Index           =   13
         Left            =   6720
         TabIndex        =   67
         Top             =   4680
         Width           =   2655
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   614
            Left            =   1320
            Picture         =   "frmBlocks.frx":94187
            Style           =   1  'Graphical
            TabIndex        =   750
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   613
            Left            =   720
            Picture         =   "frmBlocks.frx":9458E
            Style           =   1  'Graphical
            TabIndex        =   749
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   185
            Left            =   1920
            Picture         =   "frmBlocks.frx":94948
            Style           =   1  'Graphical
            TabIndex        =   199
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   77
            Left            =   1320
            Picture         =   "frmBlocks.frx":94D43
            Style           =   1  'Graphical
            TabIndex        =   76
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   76
            Left            =   1920
            Picture         =   "frmBlocks.frx":95985
            Style           =   1  'Graphical
            TabIndex        =   75
            Top             =   1440
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   75
            Left            =   120
            Picture         =   "frmBlocks.frx":965C7
            Style           =   1  'Graphical
            TabIndex        =   74
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   74
            Left            =   120
            Picture         =   "frmBlocks.frx":97209
            Style           =   1  'Graphical
            TabIndex        =   73
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   73
            Left            =   120
            Picture         =   "frmBlocks.frx":97E4B
            Style           =   1  'Graphical
            TabIndex        =   72
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   72
            Left            =   1920
            Picture         =   "frmBlocks.frx":98A8D
            Style           =   1  'Graphical
            TabIndex        =   71
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   71
            Left            =   1320
            Picture         =   "frmBlocks.frx":996CF
            Style           =   1  'Graphical
            TabIndex        =   70
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   70
            Left            =   720
            Picture         =   "frmBlocks.frx":9A311
            Style           =   1  'Graphical
            TabIndex        =   69
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   78
            Left            =   720
            Picture         =   "frmBlocks.frx":9AF53
            Style           =   1  'Graphical
            TabIndex        =   68
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame SMB3 
         Caption         =   "Bonus Tiles"
         Height          =   2055
         Index           =   4
         Left            =   13440
         TabIndex        =   52
         Top             =   480
         Width           =   3495
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   612
            Left            =   2520
            Picture         =   "frmBlocks.frx":9B310
            Style           =   1  'Graphical
            TabIndex        =   748
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   611
            Left            =   1920
            Picture         =   "frmBlocks.frx":9B6FB
            Style           =   1  'Graphical
            TabIndex        =   747
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   610
            Left            =   1320
            Picture         =   "frmBlocks.frx":9BAEB
            Style           =   1  'Graphical
            TabIndex        =   746
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   609
            Left            =   1320
            Picture         =   "frmBlocks.frx":9BF03
            Style           =   1  'Graphical
            TabIndex        =   745
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   608
            Left            =   120
            Picture         =   "frmBlocks.frx":9C30A
            Style           =   1  'Graphical
            TabIndex        =   744
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   54
            Left            =   720
            Picture         =   "frmBlocks.frx":9C71E
            Style           =   1  'Graphical
            TabIndex        =   60
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   53
            Left            =   2520
            Picture         =   "frmBlocks.frx":9D360
            Style           =   1  'Graphical
            TabIndex        =   59
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   52
            Left            =   720
            Picture         =   "frmBlocks.frx":9DFA2
            Style           =   1  'Graphical
            TabIndex        =   58
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   51
            Left            =   1920
            Picture         =   "frmBlocks.frx":9EBE4
            Style           =   1  'Graphical
            TabIndex        =   57
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   50
            Left            =   120
            Picture         =   "frmBlocks.frx":9F826
            Style           =   1  'Graphical
            TabIndex        =   56
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   49
            Left            =   120
            Picture         =   "frmBlocks.frx":A0468
            Style           =   1  'Graphical
            TabIndex        =   55
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   48
            Left            =   720
            Picture         =   "frmBlocks.frx":A10AA
            Style           =   1  'Graphical
            TabIndex        =   54
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   47
            Left            =   1320
            Picture         =   "frmBlocks.frx":A1CEC
            Style           =   1  'Graphical
            TabIndex        =   53
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame SMB3 
         Caption         =   "Dungeon"
         Height          =   2175
         Index           =   7
         Left            =   10200
         TabIndex        =   40
         Top             =   2520
         Width           =   1455
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   630
            Left            =   720
            Picture         =   "frmBlocks.frx":A292E
            Style           =   1  'Graphical
            TabIndex        =   768
            Top             =   840
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   91
            Left            =   720
            Picture         =   "frmBlocks.frx":A2D63
            Style           =   1  'Graphical
            TabIndex        =   332
            Top             =   240
            Width           =   585
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   136
            Left            =   120
            Picture         =   "frmBlocks.frx":A3115
            Style           =   1  'Graphical
            TabIndex        =   102
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   126
            Left            =   120
            Picture         =   "frmBlocks.frx":A3548
            Style           =   1  'Graphical
            TabIndex        =   98
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   29
            Left            =   120
            Picture         =   "frmBlocks.frx":A397B
            Style           =   1  'Graphical
            TabIndex        =   41
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMB3 
         Caption         =   "Sizable Blocks"
         Height          =   2175
         Index           =   10
         Left            =   4320
         TabIndex        =   35
         Top             =   7080
         Width           =   2895
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   579
            Left            =   1920
            Picture         =   "frmBlocks.frx":A45BD
            Style           =   1  'Graphical
            TabIndex        =   732
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   575
            Left            =   1920
            Picture         =   "frmBlocks.frx":A49D2
            Style           =   1  'Graphical
            TabIndex        =   704
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   568
            Left            =   1920
            Picture         =   "frmBlocks.frx":A4DB1
            Style           =   1  'Graphical
            TabIndex        =   703
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   130
            Left            =   720
            Picture         =   "frmBlocks.frx":A51AB
            Style           =   1  'Graphical
            TabIndex        =   180
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   108
            Left            =   120
            Picture         =   "frmBlocks.frx":A5641
            Style           =   1  'Graphical
            TabIndex        =   179
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00000000&
            BeginProperty Font 
               Name            =   "Arial Black"
               Size            =   15.75
               Charset         =   0
               Weight          =   900
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   540
            Index           =   260
            Left            =   1320
            MaskColor       =   &H00000000&
            Picture         =   "frmBlocks.frx":A5A44
            Style           =   1  'Graphical
            TabIndex        =   177
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00000000&
            BeginProperty Font 
               Name            =   "Arial Black"
               Size            =   15.75
               Charset         =   0
               Weight          =   900
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   540
            Index           =   240
            Left            =   1320
            MaskColor       =   &H00000000&
            Picture         =   "frmBlocks.frx":A60B9
            Style           =   1  'Graphical
            TabIndex        =   176
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00000000&
            BeginProperty Font 
               Name            =   "Arial Black"
               Size            =   15.75
               Charset         =   0
               Weight          =   900
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   540
            Index           =   241
            Left            =   1320
            MaskColor       =   &H00000000&
            Picture         =   "frmBlocks.frx":A6674
            Style           =   1  'Graphical
            TabIndex        =   175
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   26
            Left            =   720
            Picture         =   "frmBlocks.frx":A6CB0
            Style           =   1  'Graphical
            TabIndex        =   39
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00000000&
            BeginProperty Font 
               Name            =   "Arial Black"
               Size            =   15.75
               Charset         =   0
               Weight          =   900
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   540
            Index           =   25
            Left            =   120
            MaskColor       =   &H00000000&
            Picture         =   "frmBlocks.frx":A716C
            Style           =   1  'Graphical
            TabIndex        =   38
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   27
            Left            =   120
            Picture         =   "frmBlocks.frx":A762C
            Style           =   1  'Graphical
            TabIndex        =   37
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   28
            Left            =   720
            Picture         =   "frmBlocks.frx":A7AF1
            Style           =   1  'Graphical
            TabIndex        =   36
            Top             =   840
            Width           =   540
         End
      End
      Begin VB.Frame SMB3 
         Caption         =   "Special Blocks"
         Height          =   2175
         Index           =   6
         Left            =   1440
         TabIndex        =   31
         Top             =   6960
         Width           =   2655
         Begin VB.OptionButton Block 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00000000&
            BeginProperty Font 
               Name            =   "Arial Black"
               Size            =   15.75
               Charset         =   0
               Weight          =   900
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   540
            Index           =   621
            Left            =   1920
            MaskColor       =   &H00000000&
            Picture         =   "frmBlocks.frx":A7F7D
            Style           =   1  'Graphical
            TabIndex        =   757
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   620
            Left            =   1920
            Picture         =   "frmBlocks.frx":A83D4
            Style           =   1  'Graphical
            TabIndex        =   756
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   186
            Left            =   720
            Picture         =   "frmBlocks.frx":A8847
            Style           =   1  'Graphical
            TabIndex        =   191
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   225
            Left            =   720
            Picture         =   "frmBlocks.frx":A8C70
            Style           =   1  'Graphical
            TabIndex        =   184
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   224
            Left            =   120
            Picture         =   "frmBlocks.frx":A904C
            Style           =   1  'Graphical
            TabIndex        =   183
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   226
            Left            =   1320
            Picture         =   "frmBlocks.frx":A945C
            Style           =   1  'Graphical
            TabIndex        =   182
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   159
            Left            =   1320
            Picture         =   "frmBlocks.frx":A9879
            Style           =   1  'Graphical
            TabIndex        =   141
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   55
            Left            =   120
            Picture         =   "frmBlocks.frx":A9CB0
            Style           =   1  'Graphical
            TabIndex        =   62
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   2
            Left            =   720
            Picture         =   "frmBlocks.frx":AA8F2
            Style           =   1  'Graphical
            TabIndex        =   34
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00000000&
            BeginProperty Font 
               Name            =   "Arial Black"
               Size            =   15.75
               Charset         =   0
               Weight          =   900
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   540
            Index           =   4
            Left            =   1320
            MaskColor       =   &H00000000&
            Picture         =   "frmBlocks.frx":AB534
            Style           =   1  'Graphical
            TabIndex        =   33
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   5
            Left            =   120
            Picture         =   "frmBlocks.frx":AC176
            Style           =   1  'Graphical
            TabIndex        =   32
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMB3 
         Caption         =   "Pipes"
         Height          =   2175
         Index           =   9
         Left            =   2160
         TabIndex        =   26
         Top             =   4680
         Width           =   4455
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   571
            Left            =   3720
            Picture         =   "frmBlocks.frx":ACDB8
            Style           =   1  'Graphical
            TabIndex        =   707
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   570
            Left            =   3720
            Picture         =   "frmBlocks.frx":AD1E0
            Style           =   1  'Graphical
            TabIndex        =   706
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   569
            Left            =   3120
            Picture         =   "frmBlocks.frx":AD570
            Style           =   1  'Graphical
            TabIndex        =   705
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   296
            Left            =   2520
            Picture         =   "frmBlocks.frx":AD92B
            Style           =   1  'Graphical
            TabIndex        =   352
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   295
            Left            =   2520
            Picture         =   "frmBlocks.frx":ADD53
            Style           =   1  'Graphical
            TabIndex        =   351
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   294
            Left            =   3120
            Picture         =   "frmBlocks.frx":AE167
            Style           =   1  'Graphical
            TabIndex        =   350
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   182
            Left            =   1920
            Picture         =   "frmBlocks.frx":AE58B
            Style           =   1  'Graphical
            TabIndex        =   168
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   103
            Left            =   1920
            Picture         =   "frmBlocks.frx":AE9CB
            Style           =   1  'Graphical
            TabIndex        =   158
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   104
            Left            =   1920
            Picture         =   "frmBlocks.frx":AED9A
            Style           =   1  'Graphical
            TabIndex        =   157
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   36
            Left            =   1320
            Picture         =   "frmBlocks.frx":AF16D
            Style           =   1  'Graphical
            TabIndex        =   49
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   35
            Left            =   720
            Picture         =   "frmBlocks.frx":AF53C
            Style           =   1  'Graphical
            TabIndex        =   48
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   34
            Left            =   720
            Picture         =   "frmBlocks.frx":AF90F
            Style           =   1  'Graphical
            TabIndex        =   47
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   37
            Left            =   1320
            Picture         =   "frmBlocks.frx":AFCDE
            Style           =   1  'Graphical
            TabIndex        =   46
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   32
            Left            =   720
            Picture         =   "frmBlocks.frx":B00B1
            Style           =   1  'Graphical
            TabIndex        =   45
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   31
            Left            =   120
            Picture         =   "frmBlocks.frx":B0CF3
            Style           =   1  'Graphical
            TabIndex        =   44
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   33
            Left            =   1320
            Picture         =   "frmBlocks.frx":B1935
            Style           =   1  'Graphical
            TabIndex        =   43
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   24
            Left            =   3120
            Picture         =   "frmBlocks.frx":B2577
            Style           =   1  'Graphical
            TabIndex        =   30
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   23
            Left            =   2520
            Picture         =   "frmBlocks.frx":B2907
            Style           =   1  'Graphical
            TabIndex        =   29
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   22
            Left            =   120
            Picture         =   "frmBlocks.frx":B2CBD
            Style           =   1  'Graphical
            TabIndex        =   28
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   21
            Left            =   120
            Picture         =   "frmBlocks.frx":B3090
            Style           =   1  'Graphical
            TabIndex        =   27
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMB3 
         Caption         =   "Wood"
         Height          =   2175
         Index           =   0
         Left            =   2160
         TabIndex        =   19
         Top             =   240
         Width           =   6255
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   607
            Left            =   5520
            Picture         =   "frmBlocks.frx":B345F
            Style           =   1  'Graphical
            TabIndex        =   743
            Top             =   840
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   606
            Left            =   4920
            Picture         =   "frmBlocks.frx":B3556
            Style           =   1  'Graphical
            TabIndex        =   742
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   605
            Left            =   5520
            Picture         =   "frmBlocks.frx":B3647
            Style           =   1  'Graphical
            TabIndex        =   741
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   604
            Left            =   4920
            Picture         =   "frmBlocks.frx":B3709
            Style           =   1  'Graphical
            TabIndex        =   740
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   603
            Left            =   4320
            Picture         =   "frmBlocks.frx":B37CB
            Style           =   1  'Graphical
            TabIndex        =   739
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   602
            Left            =   3720
            Picture         =   "frmBlocks.frx":B38BF
            Style           =   1  'Graphical
            TabIndex        =   738
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   601
            Left            =   4320
            Picture         =   "frmBlocks.frx":B39B4
            Style           =   1  'Graphical
            TabIndex        =   737
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   600
            Left            =   3720
            Picture         =   "frmBlocks.frx":B3AA5
            Style           =   1  'Graphical
            TabIndex        =   736
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   596
            Left            =   2520
            Picture         =   "frmBlocks.frx":B3B91
            Style           =   1  'Graphical
            TabIndex        =   713
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   595
            Left            =   1920
            Picture         =   "frmBlocks.frx":B3F56
            Style           =   1  'Graphical
            TabIndex        =   712
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   597
            Left            =   3120
            Picture         =   "frmBlocks.frx":B4339
            Style           =   1  'Graphical
            TabIndex        =   711
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   275
            Left            =   1320
            Picture         =   "frmBlocks.frx":B4713
            Style           =   1  'Graphical
            TabIndex        =   328
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   274
            Left            =   120
            Picture         =   "frmBlocks.frx":B4B13
            Style           =   1  'Graphical
            TabIndex        =   327
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   276
            Left            =   720
            Picture         =   "frmBlocks.frx":B4F23
            Style           =   1  'Graphical
            TabIndex        =   326
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   1
            Left            =   1920
            Picture         =   "frmBlocks.frx":B5301
            Style           =   1  'Graphical
            TabIndex        =   178
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   129
            Left            =   3120
            Picture         =   "frmBlocks.frx":B5F43
            Style           =   1  'Graphical
            TabIndex        =   97
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   128
            Left            =   1920
            Picture         =   "frmBlocks.frx":B637A
            Style           =   1  'Graphical
            TabIndex        =   96
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   127
            Left            =   2520
            Picture         =   "frmBlocks.frx":B67AF
            Style           =   1  'Graphical
            TabIndex        =   95
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   92
            Left            =   2520
            Picture         =   "frmBlocks.frx":B6BB8
            Style           =   1  'Graphical
            TabIndex        =   79
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            Alignment       =   1  'Right Justify
            BackColor       =   &H00000000&
            BeginProperty Font 
               Name            =   "Arial Black"
               Size            =   15.75
               Charset         =   0
               Weight          =   900
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   540
            Index           =   17
            Left            =   1320
            MaskColor       =   &H00000000&
            Picture         =   "frmBlocks.frx":B6FFF
            Style           =   1  'Graphical
            TabIndex        =   25
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   16
            Left            =   720
            Picture         =   "frmBlocks.frx":B7C41
            Style           =   1  'Graphical
            TabIndex        =   24
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   15
            Left            =   120
            Picture         =   "frmBlocks.frx":B8883
            Style           =   1  'Graphical
            TabIndex        =   23
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   3
            Left            =   720
            Picture         =   "frmBlocks.frx":B94C5
            Style           =   1  'Graphical
            TabIndex        =   22
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   6
            Left            =   1320
            Picture         =   "frmBlocks.frx":BA107
            Style           =   1  'Graphical
            TabIndex        =   21
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   7
            Left            =   120
            Picture         =   "frmBlocks.frx":BAD49
            Style           =   1  'Graphical
            TabIndex        =   20
            Top             =   240
            Width           =   540
         End
      End
      Begin VB.Frame SMB3 
         Caption         =   "Grass"
         Height          =   2175
         Index           =   1
         Left            =   8640
         TabIndex        =   9
         Top             =   240
         Width           =   4455
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   305
            Left            =   1920
            Picture         =   "frmBlocks.frx":BB98B
            Style           =   1  'Graphical
            TabIndex        =   409
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   306
            Left            =   3120
            Picture         =   "frmBlocks.frx":BBD87
            Style           =   1  'Graphical
            TabIndex        =   408
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   307
            Left            =   2520
            Picture         =   "frmBlocks.frx":BC142
            Style           =   1  'Graphical
            TabIndex        =   407
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   308
            Left            =   3720
            Picture         =   "frmBlocks.frx":BC53D
            Style           =   1  'Graphical
            TabIndex        =   406
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   314
            Left            =   3720
            Picture         =   "frmBlocks.frx":BC8F8
            Style           =   1  'Graphical
            TabIndex        =   405
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   311
            Left            =   1920
            Picture         =   "frmBlocks.frx":BCCB5
            Style           =   1  'Graphical
            TabIndex        =   404
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   312
            Left            =   3120
            Picture         =   "frmBlocks.frx":BD0B6
            Style           =   1  'Graphical
            TabIndex        =   403
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   313
            Left            =   2520
            Picture         =   "frmBlocks.frx":BD471
            Style           =   1  'Graphical
            TabIndex        =   402
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   278
            Left            =   720
            Picture         =   "frmBlocks.frx":BD86F
            Style           =   1  'Graphical
            TabIndex        =   331
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   277
            Left            =   1320
            Picture         =   "frmBlocks.frx":BDC7D
            Style           =   1  'Graphical
            TabIndex        =   330
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   279
            Left            =   120
            Picture         =   "frmBlocks.frx":BE09A
            Style           =   1  'Graphical
            TabIndex        =   329
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   20
            Left            =   1320
            Picture         =   "frmBlocks.frx":BE4CE
            Style           =   1  'Graphical
            TabIndex        =   18
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   19
            Left            =   720
            Picture         =   "frmBlocks.frx":BF110
            Style           =   1  'Graphical
            TabIndex        =   17
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   18
            Left            =   120
            Picture         =   "frmBlocks.frx":BFD52
            Style           =   1  'Graphical
            TabIndex        =   16
            Top             =   840
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   14
            Left            =   3120
            Picture         =   "frmBlocks.frx":C0994
            Style           =   1  'Graphical
            TabIndex        =   15
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   13
            Left            =   2520
            Picture         =   "frmBlocks.frx":C15D6
            Style           =   1  'Graphical
            TabIndex        =   14
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   12
            Left            =   1920
            Picture         =   "frmBlocks.frx":C2218
            Style           =   1  'Graphical
            TabIndex        =   13
            Top             =   1440
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   11
            Left            =   1320
            Picture         =   "frmBlocks.frx":C2E5A
            Style           =   1  'Graphical
            TabIndex        =   12
            Top             =   240
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   9
            Left            =   120
            Picture         =   "frmBlocks.frx":C3A9C
            Style           =   1  'Graphical
            TabIndex        =   11
            Top             =   240
            Value           =   -1  'True
            Width           =   540
         End
         Begin VB.OptionButton Block 
            BackColor       =   &H00000000&
            Height          =   540
            Index           =   10
            Left            =   720
            Picture         =   "frmBlocks.frx":C46DE
            Style           =   1  'Graphical
            TabIndex        =   10
            Top             =   240
            Width           =   540
         End
      End
   End
   Begin VB.Frame Frame 
      Caption         =   "Game Selection"
      Height          =   615
      Index           =   34
      Left            =   1680
      TabIndex        =   7
      Top             =   120
      Width           =   9375
      Begin VB.OptionButton optGame 
         Caption         =   "Misc."
         Height          =   255
         Index           =   4
         Left            =   7320
         Style           =   1  'Graphical
         TabIndex        =   348
         Top             =   240
         Width           =   1695
      End
      Begin VB.OptionButton optGame 
         Caption         =   "Super Mario World"
         Height          =   255
         Index           =   3
         Left            =   5520
         Style           =   1  'Graphical
         TabIndex        =   77
         Top             =   240
         Width           =   1695
      End
      Begin VB.OptionButton optGame 
         Caption         =   "Super Mario Bros."
         Height          =   255
         Index           =   2
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   66
         Top             =   240
         Width           =   1695
      End
      Begin VB.OptionButton optGame 
         Caption         =   "Super Mario Bros. 2"
         Height          =   255
         Index           =   1
         Left            =   1920
         Style           =   1  'Graphical
         TabIndex        =   50
         Top             =   240
         Width           =   1695
      End
      Begin VB.OptionButton optGame 
         Caption         =   "Super Mario Bros. 3"
         Height          =   255
         Index           =   0
         Left            =   3720
         Style           =   1  'Graphical
         TabIndex        =   8
         Top             =   240
         Value           =   -1  'True
         Width           =   1695
      End
   End
   Begin VB.Frame Frame 
      Caption         =   "Block Special"
      Height          =   5055
      Index           =   31
      Left            =   -360
      TabIndex        =   2
      Top             =   13800
      Width           =   1455
      Begin VB.OptionButton optBlockSpecial 
         Caption         =   "Yoshi"
         Height          =   375
         Index           =   9
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   169
         Top             =   4560
         Width           =   1215
      End
      Begin VB.OptionButton optBlockSpecial 
         Caption         =   "1 Up"
         Height          =   375
         Index           =   8
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   165
         Top             =   4080
         Width           =   1215
      End
      Begin VB.OptionButton optBlockSpecial 
         Caption         =   "Shoe"
         Height          =   375
         Index           =   7
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   91
         Top             =   3120
         Width           =   1215
      End
      Begin VB.OptionButton optBlockSpecial 
         Caption         =   "Leaf"
         Height          =   375
         Index           =   6
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   80
         Top             =   2640
         Width           =   1215
      End
      Begin VB.OptionButton optBlockSpecial 
         Caption         =   "Fire Flower"
         Height          =   375
         Index           =   5
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   61
         Top             =   2160
         Width           =   1215
      End
      Begin VB.OptionButton optBlockSpecial 
         Caption         =   "Goomba"
         Height          =   375
         Index           =   4
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   42
         Top             =   3600
         Width           =   1215
      End
      Begin VB.OptionButton optBlockSpecial 
         Caption         =   "Mushroom"
         Height          =   375
         Index           =   1
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   6
         Top             =   1680
         Width           =   1215
      End
      Begin VB.OptionButton optBlockSpecial 
         Caption         =   "None"
         Height          =   375
         Index           =   0
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   5
         Top             =   240
         Value           =   -1  'True
         Width           =   1215
      End
      Begin VB.OptionButton optBlockSpecial 
         Caption         =   "One Coin"
         Height          =   375
         Index           =   2
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   4
         Top             =   720
         Width           =   1215
      End
      Begin VB.OptionButton optBlockSpecial 
         Caption         =   "Ten Coins"
         Height          =   375
         Index           =   3
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   3
         Top             =   1200
         Width           =   1215
      End
   End
End
Attribute VB_Name = "frmBlocks"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim BlockSD As Integer
Dim StopIt As Boolean

Private Sub Block_Click(Index As Integer)
    Dim A As Integer
    On Error Resume Next
    If Me.Visible = True Then FocusNinja.SetFocus
    If BlockIsSizable(Index) Then
        chkFill.Enabled = False
    Else
        chkFill.Enabled = True
    End If
    For A = 1 To Block.Count
        If Index <> A Then Block(A).Value = False
    Next A
    If Index = 5 Or Index = 88 Or Index = 193 Or Index = 224 Then
        optBlockS.Value = -1
    Else
        optBlockS.Value = 0
    End If
    CheckBlock
    chkFill.Value = 0
    cmdInvis.Caption = "No"
    cmdSlip.Caption = "No"
    If Index = 189 Or Index = 190 Or Index = 191 Or Index = 270 Or Index = 271 Or Index = 272 Or Index = 620 Or Index = 621 Or Index = 633 Or Index = 634 Or Index = 241 Or Index = 242 Then
        cmdSlip.Caption = "Yes"
    End If
End Sub

Private Sub BlockH_Change()
    On Error Resume Next
    If Me.Visible = True Then FocusNinja.SetFocus
End Sub

Private Sub BlockR_Click()
    On Error Resume Next
    If Me.Visible = True Then FocusNinja.SetFocus
    BlockH.Value = 2
    BlockW.Value = 2
End Sub

Private Sub BlockW_Change()
    On Error Resume Next
    If Me.Visible = True Then FocusNinja.SetFocus
End Sub

Private Sub cmdAdvanced_Click()
    On Error Resume Next
    frmAdvancedBlock.Show
    DoEvents
    frmAdvancedBlock.SetFocus
End Sub

Private Sub cmdInvis_Click()
    If cmdInvis.Caption = "No" Then
        cmdInvis.Caption = "Yes"
    Else
        cmdInvis.Caption = "No"
    End If
End Sub

Private Sub cmdSlip_Click()
    If cmdSlip.Caption = "No" Then
        cmdSlip.Caption = "Yes"
    Else
        cmdSlip.Caption = "No"
    End If
End Sub

Private Sub Form_Load()
    Dim A As Integer
    For A = 1 To Block.Count
        Block(A).Value = False
    Next A
    optBlockS.max = frmNPCs.NPC.Count
    Block(1).Value = True
    For A = 0 To Game.Count - 1
        Game(A).Top = Game(0).Top
        Game(A).Left = Game(0).Left
    Next A
    For A = 1 To SMB3.Count - 1
        SMB3(A).Top = SMB3(0).Top
        SMB3(A).Left = SMB3(0).Left
        SMB3(A).Visible = False
    Next A
    For A = 1 To SMB2.Count - 1
        SMB2(A).Top = SMB2(0).Top
        SMB2(A).Left = SMB2(0).Left
        SMB2(A).Visible = False
    Next A
    For A = 1 To SMB1.Count - 1
        SMB1(A).Top = SMB1(0).Top
        SMB1(A).Left = SMB1(0).Left
        SMB1(A).Visible = False
    Next A
    For A = 1 To SMW.Count - 1
        SMW(A).Top = SMW(0).Top
        SMW(A).Left = SMW(0).Left
        SMW(A).Visible = False
    Next A
    For A = 0 To Misc.Count - 1
        If A <> 1 Then
            Misc(A).Top = Misc(1).Top
            Misc(A).Left = Misc(1).Left
        End If
        If A <> 0 Then Misc(A).Visible = False
    Next A
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Cancel = 1
    frmLevelEditor.optCursor(13).Value = True
End Sub

Private Sub optBlockS_Change()
    CheckBlock
    On Error Resume Next
    If Me.Visible = True Then FocusNinja.SetFocus
End Sub

Private Sub optBlockS_Scroll()
    CheckBlock
    On Error Resume Next
    If Me.Visible = True Then FocusNinja.SetFocus
End Sub

Private Sub optBlockSpecial_Click(Index As Integer)
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
End Sub

Private Sub optMisc_Click(Index As Integer)
    On Error Resume Next
    Dim A As Integer
    For A = 0 To Misc.Count - 1
        If A = Index Then
            Misc(A).Visible = True
        Else
            Misc(A).Visible = False
        End If
    Next A
    If Me.Visible = True Then FocusNinja.SetFocus
End Sub

Private Sub optSMW_Click(Index As Integer)
    On Error Resume Next
    Dim A As Integer
    For A = 0 To SMW.Count - 1
        If A = Index Then
            SMW(A).Visible = True
        Else
            SMW(A).Visible = False
        End If
    Next A
    If Me.Visible = True Then FocusNinja.SetFocus
End Sub

Private Sub optSMB3_Click(Index As Integer)
    On Error Resume Next
    Dim A As Integer
    For A = 0 To SMB3.Count - 1
        If A = Index Then
            SMB3(A).Visible = True
        Else
            SMB3(A).Visible = False
        End If
    Next A
    If Me.Visible = True Then FocusNinja.SetFocus
End Sub

Private Sub optSMB2_Click(Index As Integer)
    On Error Resume Next
    Dim A As Integer
    For A = 0 To SMB2.Count - 1
        If A = Index Then
            SMB2(A).Visible = True
        Else
            SMB2(A).Visible = False
        End If
    Next A
    If Me.Visible = True Then FocusNinja.SetFocus
End Sub

Private Sub optSMB1_Click(Index As Integer)
    On Error Resume Next
    Dim A As Integer
    For A = 0 To SMB1.Count - 1
        If A = Index Then
            SMB1(A).Visible = True
        Else
            SMB1(A).Visible = False
        End If
    Next A
    If Me.Visible = True Then FocusNinja.SetFocus
End Sub


Private Sub qBlockS_Click(Index As Integer)
    On Error Resume Next
    optBlockS.Value = Index
    If Me.Visible = True Then FocusNinja.SetFocus
End Sub

Private Sub qCoin_Click(Index As Integer)
    optBlockS.Value = -Index
End Sub

Public Sub CheckBlock()
    On Error Resume Next
    Dim A As Integer
    Dim D As Integer
    Dim W As Single
    Dim gW As Single
    Dim H As Single
    Dim gH As Single
    Dim B As Integer
    If StopIt = True Then Exit Sub
    StopIt = True
    A = optBlockS.Value
    If A > 0 Then
        lblCoins.Visible = False
        If A > BlockSD Then
            D = 1
        Else
            D = -1
        End If
        Do While NPCIsACoin(A) Or NPCIsAnExit(A) Or A = 8 Or A = 12 Or A = 13 Or A = 17 Or A = 18 Or A = 28 Or A = 30 Or A = 37 Or A = 40 Or A = 44 Or A = 46 Or A = 50 Or A = 51 Or A = 52 Or (A >= 56 And A <= 70) Or A = 74 Or (A >= 78 And A <= 83) Or A = 85 Or A = 87 Or A = 91 Or A = 93 Or A = 104 Or A = 105 Or A = 106 Or A = 108 Or A = 133 Or (NPCWidthGFX(A) <> 0 And Not NPCIsYoshi(A)) Or A = 38 Or A = 42 Or A = 43 Or A = 44 Or A = 151 Or A = 152 Or A = 159 Or A = 160 Or A = 164 Or A = 171 Or A = 178 Or A = 179 Or A = 180 Or A = 181 Or A = 190 Or A = 196 Or A = 197 Or A = 199 Or A = 202 Or A = 208 Or A = 210 Or A = 211 Or A = 212 Or A = 209 Or A = 200 Or A = 201 Or A = 192 Or A = 237 Or A = 49 Or A = 246 Or A = 255 Or A = 256 Or A = 257 Or A = 263 Or A = 265 Or A = 266 Or A = 270 Or A = 259 Or A = 260 Or A = 271 Or A = 272 Or A = 276 Or A = 288 Or A = 292
            A = A + D
            If A >= frmNPCs.NPC.Count Then D = -1
        Loop
        optBlockS.max = frmNPCs.NPC.Count
        optBlockS.Value = A
        If NPCIsYoshi(A) Then
            gW = 52
            gH = 56
        ElseIf NPCWidthGFX(A) = 0 Then
            gW = NPCWidth(A)
            gH = NPCHeight(A)
        Else
            gW = NPCWidthGFX(A)
            gH = NPCHeightGFX(A)
        End If
        If gW >= gH Then
            H = gH * PicNPC.ScaleWidth / gW
            W = PicNPC.ScaleWidth
        Else
            W = gW * PicNPC.ScaleHeight / gH
            H = PicNPC.ScaleHeight
        End If
        B = 14
        If B > 0 Then
            BitBlt PicNPC.hdc, 0, 0, PicNPC.ScaleWidth, PicNPC.ScaleHeight, GFXBackground2(B), GFXBackground2Width(B) / 2, GFXBackground2Height(B) / 2, vbSrcCopy
        Else
            PicNPC.Cls
        End If
        StretchBlt PicNPC.hdc, PicNPC.ScaleWidth / 2 - W / 2, PicNPC.ScaleHeight / 2 - H / 2, W, H, GFXNPCMask(A), 0, 0, gW, gH, vbSrcAnd
        StretchBlt PicNPC.hdc, PicNPC.ScaleWidth / 2 - W / 2, PicNPC.ScaleHeight / 2 - H / 2, W, H, GFXNPC(A), 0, 0, gW, gH, vbSrcPaint
    ElseIf A = 0 Then
        B = 14
        If B > 0 Then
            BitBlt PicNPC.hdc, 0, 0, PicNPC.ScaleWidth, PicNPC.ScaleHeight, GFXBackground2(B), GFXBackground2Width(B) / 2, GFXBackground2Height(B) / 2, vbSrcCopy
        Else
            PicNPC.Cls
        End If
        lblCoins.Caption = "Empty"
        lblCoins.Visible = True
    Else
        lblCoins.Caption = -A & " coins"
        lblCoins.Visible = True
        A = 10
        If NPCWidthGFX(A) = 0 Then
            gW = NPCWidth(A)
            gH = NPCHeight(A)
        Else
            gW = NPCWidthGFX(A)
            gH = NPCHeightGFX(A)
        End If
        If gW >= gH Then
            H = gH * PicNPC.ScaleWidth / gW
            W = PicNPC.ScaleWidth
        Else
            W = gW * PicNPC.ScaleHeight / gH
            H = PicNPC.ScaleHeight
        End If
        B = 14
        If B > 0 Then
            BitBlt PicNPC.hdc, 0, 0, PicNPC.ScaleWidth, PicNPC.ScaleHeight, GFXBackground2(B), GFXBackground2Width(B) / 2, GFXBackground2Height(B) / 2, vbSrcCopy
        Else
            PicNPC.Cls
        End If
        StretchBlt PicNPC.hdc, PicNPC.ScaleWidth / 2 - W / 2, PicNPC.ScaleHeight / 2 - H / 2, W, H, GFXNPCMask(A), 0, 0, gW, gH, vbSrcAnd
        StretchBlt PicNPC.hdc, PicNPC.ScaleWidth / 2 - W / 2, PicNPC.ScaleHeight / 2 - H / 2, W, H, GFXNPC(A), 0, 0, gW, gH, vbSrcPaint
    End If
    BlockSD = optBlockS.Value
    PicNPC.Refresh
    StopIt = False
End Sub

