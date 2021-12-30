Attribute VB_Name = "MD5"
Option Explicit

'/******************************************************************************
' *  Copyright (C) 2000 by Robert Hubley.                                      *
' *  All rights reserved.                                                      *
' *                                                                            *
' *  This software is provided ``AS IS'' and any express or implied            *
' *  warranties, including, but not limited to, the implied warranties of      *
' *  merchantability and fitness for a particular purpose, are disclaimed.     *
' *  In no event shall the authors be liable for any direct, indirect,         *
' *  incidental, special, exemplary, or consequential damages (including, but  *
' *  not limited to, procurement of substitute goods or services; loss of use, *
' *  data, or profits; or business interruption) however caused and on any     *
' *  theory of liability, whether in contract, strict liability, or tort       *
' *  (including negligence or otherwise) arising in any way out of the use of  *
' *  this software, even if advised of the possibility of such damage.         *
' *                                                                            *
' ******************************************************************************
'
'  CLASS: MD5
'
'  DESCRIPTION:
'     This is a class which encapsulates a set of MD5 Message Digest functions.
'     MD5 algorithm produces a 128 bit digital fingerprint (signature) from an
'     dataset of arbitrary length.  For details see RFC 1321 (summarized below).
'     This implementation is derived from the RSA Data Security, Inc. MD5 Message-Digest
'     algorithm reference implementation (originally written in C)
'
'  AUTHOR:
'     Robert M. Hubley 12/1999
'
'
'  NOTES:
'      Network Working Group                                    R. Rivest
'      Request for Comments: 1321     MIT Laboratory for Computer Science
'                                             and RSA Data Security, Inc.
'                                                              April 1992
'
'
'                           The MD5 Message-Digest Algorithm
'
'      Summary
'
'         This document describes the MD5 message-digest algorithm. The
'         algorithm takes as input a message of arbitrary length and produces
'         as output a 128-bit "fingerprint" or "message digest" of the input.
'         It is conjectured that it is computationally infeasible to produce
'         two messages having the same message digest, or to produce any
'         message having a given prespecified target message digest. The MD5
'         algorithm is intended for digital signature applications, where a
'         large file must be "compressed" in a secure manner before being
'         encrypted with a private (secret) key under a public-key cryptosystem
'         such as RSA.
'
'         The MD5 algorithm is designed to be quite fast on 32-bit machines. In
'         addition, the MD5 algorithm does not require any large substitution
'         tables; the algorithm can be coded quite compactly.
'
'         The MD5 algorithm is an extension of the MD4 message-digest algorithm
'         1,2]. MD5 is slightly slower than MD4, but is more "conservative" in
'         design. MD5 was designed because it was felt that MD4 was perhaps
'         being adopted for use more quickly than justified by the existing
'         critical review; because MD4 was designed to be exceptionally fast,
'         it is "at the edge" in terms of risking successful cryptanalytic
'         attack. MD5 backs off a bit, giving up a little in speed for a much
'         greater likelihood of ultimate security. It incorporates some
'         suggestions made by various reviewers, and contains additional
'         optimizations. The MD5 algorithm is being placed in the public domain
'         for review and possible adoption as a standard.
'
'         RFC Author:
'         Ronald L.Rivest
'         Massachusetts Institute of Technology
'         Laboratory for Computer Science
'         NE43 -324545    Technology Square
'         Cambridge, MA  02139-1986
'         Phone: (617) 253-5880
'         EMail:    Rivest@ theory.lcs.mit.edu
'
'
'
'  CHANGE HISTORY:
'
'     0.1.0  RMH    1999/12/29      Original version
'
'


'=
'= Class Constants
'=
Private Const OFFSET_4 = 4294967296#
Private Const MAXINT_4 = 2147483647

Private Const S11 = 7
Private Const S12 = 12
Private Const S13 = 17
Private Const S14 = 22
Private Const S21 = 5
Private Const S22 = 9
Private Const S23 = 14
Private Const S24 = 20
Private Const S31 = 4
Private Const S32 = 11
Private Const S33 = 16
Private Const S34 = 23
Private Const S41 = 6
Private Const S42 = 10
Private Const S43 = 15
Private Const S44 = 21


'=
'= Class Variables
'=
Private State(4) As Long
Private ByteCounter As Long
Private ByteBuffer(63) As Byte


'=
'= Class Properties
'=
Property Get RegisterA() As String
    RegisterA = State(1)
End Property

Property Get RegisterB() As String
    RegisterB = State(2)
End Property

Property Get RegisterC() As String
    RegisterC = State(3)
End Property

Property Get RegisterD() As String
    RegisterD = State(4)
End Property


'=
'= Class Functions
'=

'
' Function to quickly digest a file into a hex string
'
Public Function DigestFileToHexStr(FileName As String) As String
    Open FileName For Binary Access Read As #1
    MD5Init
    Do While Not EOF(1)
        Get #1, , ByteBuffer
        If Loc(1) < LOF(1) Then
            ByteCounter = ByteCounter + 64
            MD5Transform ByteBuffer
        End If
    Loop
    ByteCounter = ByteCounter + (LOF(1) Mod 64)
    Close #1
    MD5Final
    DigestFileToHexStr = GetValues
End Function

'
' Function to digest a text string and output the result as a string
' of hexadecimal characters.
'
Public Function DigestStrToHexStr(SourceString As String) As String
    MD5Init
    MD5Update Len(SourceString), StringToArray(SourceString)
    MD5Final
    DigestStrToHexStr = GetValues
End Function

'
' A utility function which converts a string into an array of
' bytes.
'
Private Function StringToArray(InString As String) As Byte()
    Dim i As Integer
    Dim bytBuffer() As Byte
    ReDim bytBuffer(Len(InString))
    For i = 0 To Len(InString) - 1
        bytBuffer(i) = Asc(Mid(InString, i + 1, 1))
    Next i
    StringToArray = bytBuffer
End Function

'
' Concatenate the four state vaules into one string
'
Public Function GetValues() As String
    GetValues = LongToString(State(1)) & LongToString(State(2)) & LongToString(State(3)) & LongToString(State(4))
End Function

'
' Convert a Long to a Hex string
'
Private Function LongToString(Num As Long) As String
        Dim A As Byte
        Dim B As Byte
        Dim C As Byte
        Dim d As Byte
        
        A = Num And &HFF&
        If A < 16 Then
            LongToString = "0" & Hex(A)
        Else
            LongToString = Hex(A)
        End If
               
        B = (Num And &HFF00&) \ 256
        If B < 16 Then
            LongToString = LongToString & "0" & Hex(B)
        Else
            LongToString = LongToString & Hex(B)
        End If
        
        C = (Num And &HFF0000) \ 65536
        If C < 16 Then
            LongToString = LongToString & "0" & Hex(C)
        Else
            LongToString = LongToString & Hex(C)
        End If
       
        If Num < 0 Then
            d = ((Num And &H7F000000) \ 16777216) Or &H80&
        Else
            d = (Num And &HFF000000) \ 16777216
        End If
        
        If d < 16 Then
            LongToString = LongToString & "0" & Hex(d)
        Else
            LongToString = LongToString & Hex(d)
        End If
    
End Function

'
' Initialize the class
'   This must be called before a digest calculation is started
'
Public Sub MD5Init()
    ByteCounter = 0
    State(1) = UnsignedToLong(1732584193#)
    State(2) = UnsignedToLong(4023233417#)
    State(3) = UnsignedToLong(2562383102#)
    State(4) = UnsignedToLong(271733878#)
End Sub

'
' MD5 Final
'
Public Sub MD5Final()
    Dim dblBits As Double
    
    Dim padding(72) As Byte
    Dim lngBytesBuffered As Long
    
    padding(0) = &H80
    
    dblBits = ByteCounter * 8
    
    ' Pad out
    lngBytesBuffered = ByteCounter Mod 64
    If lngBytesBuffered <= 56 Then
        MD5Update 56 - lngBytesBuffered, padding
    Else
        MD5Update 120 - ByteCounter, padding
    End If
    
    
    padding(0) = UnsignedToLong(dblBits) And &HFF&
    padding(1) = UnsignedToLong(dblBits) \ 256 And &HFF&
    padding(2) = UnsignedToLong(dblBits) \ 65536 And &HFF&
    padding(3) = UnsignedToLong(dblBits) \ 16777216 And &HFF&
    padding(4) = 0
    padding(5) = 0
    padding(6) = 0
    padding(7) = 0
    
    MD5Update 8, padding
End Sub

'
' Break up input stream into 64 byte chunks
'
Public Sub MD5Update(InputLen As Long, InputBuffer() As Byte)
    Dim II As Integer
    Dim i As Integer
    Dim J As Integer
    Dim K As Integer
    Dim lngBufferedBytes As Long
    Dim lngBufferRemaining As Long
    Dim lngRem As Long
    
    lngBufferedBytes = ByteCounter Mod 64
    lngBufferRemaining = 64 - lngBufferedBytes
    ByteCounter = ByteCounter + InputLen
    ' Use up old buffer results first
    If InputLen >= lngBufferRemaining Then
        For II = 0 To lngBufferRemaining - 1
            ByteBuffer(lngBufferedBytes + II) = InputBuffer(II)
        Next II
        MD5Transform ByteBuffer
        
        lngRem = (InputLen) Mod 64
        ' The transfer is a multiple of 64 lets do some transformations
        For i = lngBufferRemaining To InputLen - II - lngRem Step 64
            For J = 0 To 63
                ByteBuffer(J) = InputBuffer(i + J)
            Next J
            MD5Transform ByteBuffer
        Next i
        lngBufferedBytes = 0
    Else
      i = 0
    End If
    
    ' Buffer any remaining input
    For K = 0 To InputLen - i - 1
        ByteBuffer(lngBufferedBytes + K) = InputBuffer(i + K)
    Next K
    
End Sub

'
' MD5 Transform
'
Private Sub MD5Transform(Buffer() As Byte)
    Dim X(16) As Long
    Dim A As Long
    Dim B As Long
    Dim C As Long
    Dim d As Long
    
    A = State(1)
    B = State(2)
    C = State(3)
    d = State(4)
    
    Decode 64, X, Buffer

    ' Round 1
    FF A, B, C, d, X(0), S11, -680876936
    FF d, A, B, C, X(1), S12, -389564586
    FF C, d, A, B, X(2), S13, 606105819
    FF B, C, d, A, X(3), S14, -1044525330
    FF A, B, C, d, X(4), S11, -176418897
    FF d, A, B, C, X(5), S12, 1200080426
    FF C, d, A, B, X(6), S13, -1473231341
    FF B, C, d, A, X(7), S14, -45705983
    FF A, B, C, d, X(8), S11, 1770035416
    FF d, A, B, C, X(9), S12, -1958414417
    FF C, d, A, B, X(10), S13, -42063
    FF B, C, d, A, X(11), S14, -1990404162
    FF A, B, C, d, X(12), S11, 1804603682
    FF d, A, B, C, X(13), S12, -40341101
    FF C, d, A, B, X(14), S13, -1502002290
    FF B, C, d, A, X(15), S14, 1236535329
    
    ' Round 2
    GG A, B, C, d, X(1), S21, -165796510
    GG d, A, B, C, X(6), S22, -1069501632
    GG C, d, A, B, X(11), S23, 643717713
    GG B, C, d, A, X(0), S24, -373897302
    GG A, B, C, d, X(5), S21, -701558691
    GG d, A, B, C, X(10), S22, 38016083
    GG C, d, A, B, X(15), S23, -660478335
    GG B, C, d, A, X(4), S24, -405537848
    GG A, B, C, d, X(9), S21, 568446438
    GG d, A, B, C, X(14), S22, -1019803690
    GG C, d, A, B, X(3), S23, -187363961
    GG B, C, d, A, X(8), S24, 1163531501
    GG A, B, C, d, X(13), S21, -1444681467
    GG d, A, B, C, X(2), S22, -51403784
    GG C, d, A, B, X(7), S23, 1735328473
    GG B, C, d, A, X(12), S24, -1926607734
    
    ' Round 3
    HH A, B, C, d, X(5), S31, -378558
    HH d, A, B, C, X(8), S32, -2022574463
    HH C, d, A, B, X(11), S33, 1839030562
    HH B, C, d, A, X(14), S34, -35309556
    HH A, B, C, d, X(1), S31, -1530992060
    HH d, A, B, C, X(4), S32, 1272893353
    HH C, d, A, B, X(7), S33, -155497632
    HH B, C, d, A, X(10), S34, -1094730640
    HH A, B, C, d, X(13), S31, 681279174
    HH d, A, B, C, X(0), S32, -358537222
    HH C, d, A, B, X(3), S33, -722521979
    HH B, C, d, A, X(6), S34, 76029189
    HH A, B, C, d, X(9), S31, -640364487
    HH d, A, B, C, X(12), S32, -421815835
    HH C, d, A, B, X(15), S33, 530742520
    HH B, C, d, A, X(2), S34, -995338651
    
    ' Round 4
    II A, B, C, d, X(0), S41, -198630844
    II d, A, B, C, X(7), S42, 1126891415
    II C, d, A, B, X(14), S43, -1416354905
    II B, C, d, A, X(5), S44, -57434055
    II A, B, C, d, X(12), S41, 1700485571
    II d, A, B, C, X(3), S42, -1894986606
    II C, d, A, B, X(10), S43, -1051523
    II B, C, d, A, X(1), S44, -2054922799
    II A, B, C, d, X(8), S41, 1873313359
    II d, A, B, C, X(15), S42, -30611744
    II C, d, A, B, X(6), S43, -1560198380
    II B, C, d, A, X(13), S44, 1309151649
    II A, B, C, d, X(4), S41, -145523070
    II d, A, B, C, X(11), S42, -1120210379
    II C, d, A, B, X(2), S43, 718787259
    II B, C, d, A, X(9), S44, -343485551
    
    
    State(1) = LongOverflowAdd(State(1), A)
    State(2) = LongOverflowAdd(State(2), B)
    State(3) = LongOverflowAdd(State(3), C)
    State(4) = LongOverflowAdd(State(4), d)

'  /* Zeroize sensitive information.
'*/
'  MD5_memset ((POINTER)x, 0, sizeof (x));
    
End Sub

Private Sub Decode(Length As Integer, OutputBuffer() As Long, InputBuffer() As Byte)
    Dim intDblIndex As Integer
    Dim intByteIndex As Integer
    Dim dblSum As Double
    
    intDblIndex = 0
    For intByteIndex = 0 To Length - 1 Step 4
        dblSum = InputBuffer(intByteIndex) + _
                                    InputBuffer(intByteIndex + 1) * 256# + _
                                    InputBuffer(intByteIndex + 2) * 65536# + _
                                    InputBuffer(intByteIndex + 3) * 16777216#
        OutputBuffer(intDblIndex) = UnsignedToLong(dblSum)
        intDblIndex = intDblIndex + 1
    Next intByteIndex
End Sub

'
' FF, GG, HH, and II transformations for rounds 1, 2, 3, and 4.
' Rotation is separate from addition to prevent recomputation.
'
Private Function FF(A As Long, _
                    B As Long, _
                    C As Long, _
                    d As Long, _
                    X As Long, _
                    s As Long, _
                    ac As Long) As Long
    A = LongOverflowAdd4(A, (B And C) Or (Not (B) And d), X, ac)
    A = LongLeftRotate(A, s)
    A = LongOverflowAdd(A, B)
End Function

Private Function GG(A As Long, _
                    B As Long, _
                    C As Long, _
                    d As Long, _
                    X As Long, _
                    s As Long, _
                    ac As Long) As Long
    A = LongOverflowAdd4(A, (B And d) Or (C And Not (d)), X, ac)
    A = LongLeftRotate(A, s)
    A = LongOverflowAdd(A, B)
End Function

Private Function HH(A As Long, _
                    B As Long, _
                    C As Long, _
                    d As Long, _
                    X As Long, _
                    s As Long, _
                    ac As Long) As Long
    A = LongOverflowAdd4(A, B Xor C Xor d, X, ac)
    A = LongLeftRotate(A, s)
    A = LongOverflowAdd(A, B)
End Function

Private Function II(A As Long, _
                    B As Long, _
                    C As Long, _
                    d As Long, _
                    X As Long, _
                    s As Long, _
                    ac As Long) As Long
    A = LongOverflowAdd4(A, C Xor (B Or Not (d)), X, ac)
    A = LongLeftRotate(A, s)
    A = LongOverflowAdd(A, B)
End Function

'
' Rotate a long to the right
'
Function LongLeftRotate(Value As Long, bits As Long) As Long
    Dim lngSign As Long
    Dim lngI As Long
    bits = bits Mod 32
    If bits = 0 Then LongLeftRotate = Value: Exit Function
    For lngI = 1 To bits
        lngSign = Value And &HC0000000
        Value = (Value And &H3FFFFFFF) * 2
        Value = Value Or ((lngSign < 0) And 1) Or (CBool(lngSign And _
                &H40000000) And &H80000000)
    Next
    LongLeftRotate = Value
End Function

'
' Function to add two unsigned numbers together as in C.
' Overflows are ignored!
'
Private Function LongOverflowAdd(Val1 As Long, Val2 As Long) As Long
    Dim lngHighWord As Long
    Dim lngLowWord As Long
    Dim lngOverflow As Long

    lngLowWord = (Val1 And &HFFFF&) + (Val2 And &HFFFF&)
    lngOverflow = lngLowWord \ 65536
    lngHighWord = (((Val1 And &HFFFF0000) \ 65536) + ((Val2 And &HFFFF0000) \ 65536) + lngOverflow) And &HFFFF&
    LongOverflowAdd = UnsignedToLong((lngHighWord * 65536#) + (lngLowWord And &HFFFF&))
End Function

'
' Function to add two unsigned numbers together as in C.
' Overflows are ignored!
'
Private Function LongOverflowAdd4(Val1 As Long, Val2 As Long, val3 As Long, val4 As Long) As Long
    Dim lngHighWord As Long
    Dim lngLowWord As Long
    Dim lngOverflow As Long

    lngLowWord = (Val1 And &HFFFF&) + (Val2 And &HFFFF&) + (val3 And &HFFFF&) + (val4 And &HFFFF&)
    lngOverflow = lngLowWord \ 65536
    lngHighWord = (((Val1 And &HFFFF0000) \ 65536) + _
                   ((Val2 And &HFFFF0000) \ 65536) + _
                   ((val3 And &HFFFF0000) \ 65536) + _
                   ((val4 And &HFFFF0000) \ 65536) + _
                   lngOverflow) And &HFFFF&
    LongOverflowAdd4 = UnsignedToLong((lngHighWord * 65536#) + (lngLowWord And &HFFFF&))
End Function

'
' Convert an unsigned double into a long
'
Private Function UnsignedToLong(Value As Double) As Long
        If Value < 0 Or Value >= OFFSET_4 Then Error 6 ' Overflow
        If Value <= MAXINT_4 Then
          UnsignedToLong = Value
        Else
          UnsignedToLong = Value - OFFSET_4
        End If
      End Function

'
' Convert a long to an unsigned Double
'
Private Function LongToUnsigned(Value As Long) As Double
        If Value < 0 Then
          LongToUnsigned = Value + OFFSET_4
        Else
          LongToUnsigned = Value
        End If
End Function
