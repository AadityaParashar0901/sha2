$Console:Only
Print "Check: "; MD5$32("")
Sleep
For I = 1 To _CommandCount
    If _FileExists(Command$(I)) Then
        Open Command$(I) For Binary As #1
        F$ = String$(LOF(1), 0)
        Get #1, , F$
        Close #1
        ST! = Timer(0.01)
        Print Command$(I); "->"; LCase$(MD5$32(F$)); ", "; PrintTime(Timer(0.01) - ST!)
    End If
Next I
System
Function MD5$32 (I$)
    Dim As _Unsigned Long I, Part, Message_Parts, Parts, Message_Length
    Dim As _Unsigned Long a0, b0, c0, d0, a, b, c, d, t
    Dim As _Unsigned Long k(0 To 63), M(0 To 15)
    Dim As _Unsigned _Byte s(0 To 63), g(0 To 63)
    Restore SHIFT_DATA: For I = 0 To 63
        If (I And 15) > 3 Then
            s(I) = s(I - 4)
        Else
            Read s(I)
        End If
    Next I
    Restore MD5_CONSTANTS: For I = 0 To 63: Read k(I): Next I
    Restore MD5_ORDER: For I = 0 To 63: Read g(I): Next I
    a0 = &H67452301
    b0 = &HEFCDAB89
    c0 = &H98BADCFE
    d0 = &H10325476
    Message_Length = Len(I$): If (Message_Length And 63) > 55 Then Padding$ = String$(128 - (Message_Length And 63), 0) Else Padding$ = String$(64 - (Message_Length And 63), 0)
    Message_Length = _SHL(Message_Length, 3)
    Asc(Padding$, 1) = 128
    Asc(Padding$, Len(Padding$)) = Message_Length And 255
    Asc(Padding$, Len(Padding$) - 1) = _SHR(Message_Length, 8) And 255
    Asc(Padding$, Len(Padding$) - 2) = _SHR(Message_Length, 16) And 255
    Asc(Padding$, Len(Padding$) - 3) = _SHR(Message_Length, 24) And 255
    Asc(Padding$, Len(Padding$) - 4) = _SHR(Message_Length, 32) And 255
    Asc(Padding$, Len(Padding$) - 5) = _SHR(Message_Length, 40) And 255
    Asc(Padding$, Len(Padding$) - 6) = _SHR(Message_Length, 48) And 255
    Asc(Padding$, Len(Padding$) - 7) = _SHR(Message_Length, 56) And 255
    Message_Parts = _SHR(Message_Length, 9)
    Padding$ = Mid$(I$, _SHL(Message_Parts, 6) + 1) + Padding$
    Parts = Message_Parts + _SHR(Len(Padding$), 6)
    For Part = 1 To Parts
        If Part > Message_Parts Then
            CurrentBlock$ = Mid$(Padding$, _SHL(Part - Message_Parts, 6) - 63, 64)
        Else
            CurrentBlock$ = Mid$(I$, _SHL(Part, 6) - 63, 64)
        End If
        For I = 0 To 15: M(I) = ReverseCVL(Mid$(CurrentBlock$, I * 4 + 1, 4)): Next I
        a = a0
        b = b0
        c = c0
        d = d0
        For I = 0 To 63
            Select Case I
                Case 0 To 15
                    a = a + (d Xor (b And (c Xor d))) + M(g(I)) + k(I)
                Case 16 To 31
                    a = a + (c Xor (d And (c Xor b))) + M(g(I)) + k(I)
                Case 32 To 47
                    a = a + (b Xor c Xor d) + M(g(I)) + k(I)
                Case 48 To 63
                    a = a + (c Xor (b Or (Not d))) + M(g(I)) + k(I)
            End Select
            a = leftRotateLong(a, s(I)) + b
            t = d
            d = c
            c = b
            b = a
            a = t
        Next I
        a0 = a0 + a
        b0 = b0 + b
        c0 = c0 + c
        d0 = d0 + d
    Next Part
    MD5$32 = LongToHex$(a0) + LongToHex$(b0) + LongToHex$(c0) + LongToHex$(d0)
    Exit Function
    SHIFT_DATA:
    Data 7,12,17,22
    Data 5,9,14,20
    Data 4,11,16,23
    Data 6,10,15,21
    MD5_CONSTANTS:
    Data &Hd76aa478,&He8c7b756,&H242070db,&Hc1bdceee,&Hf57c0faf,&H4787c62a,&Ha8304613,&Hfd469501,&H698098d8,&H8b44f7af,&Hffff5bb1,&H895cd7be,&H6b901122,&Hfd987193,&Ha679438e,&H49b40821,&Hf61e2562,&Hc040b340,&H265e5a51,&He9b6c7aa,&Hd62f105d,&H02441453,&Hd8a1e681,&He7d3fbc8,&H21e1cde6,&Hc33707d6,&Hf4d50d87,&H455a14ed,&Ha9e3e905,&Hfcefa3f8,&H676f02d9,&H8d2a4c8a,&Hfffa3942,&H8771f681,&H6d9d6122,&Hfde5380c,&Ha4beea44,&H4bdecfa9,&Hf6bb4b60,&Hbebfbc70,&H289b7ec6,&Heaa127fa,&Hd4ef3085,&H04881d05,&Hd9d4d039,&He6db99e5,&H1fa27cf8,&Hc4ac5665,&Hf4292244,&H432aff97,&Hab9423a7,&Hfc93a039,&H655b59c3,&H8f0ccc92,&Hffeff47d,&H85845dd1,&H6fa87e4f,&Hfe2ce6e0,&Ha3014314,&H4e0811a1,&Hf7537e82,&Hbd3af235,&H2ad7d2bb,&Heb86d391
    MD5_ORDER:
    Data 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,1,6,11,0,5,10,15,4,9,14,3,8,13,2,7,12,5,8,11,14,1,4,7,10,13,0,3,6,9,12,15,2,0,7,14,5,12,3,10,1,8,15,6,13,4,11,2,9
End Function
'$Include:'include\leftRotate.bm'
'$Include:'include\rightRotate.bm'
'$Include:'include\hex.bm'
'$Include:'include\bits.bm'
'$Include:'include\reversecvl.bm'
'$Include:'include\printtime.bm'
