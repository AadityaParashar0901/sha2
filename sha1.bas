$Console:Only
For I = 1 To _CommandCount
    INFILE$ = Command$(I)
    If _FileExists(INFILE$) = 0 Then INFILE$ = _StartDir$ + "\" + INFILE$
    If _FileExists(INFILE$) Then
        Open INFILE$ For Binary As #1
        F$ = String$(LOF(1), 0)
        Get #1, , F$
        Close #1
        ST! = Timer(0.01)
        Print INFILE$; "->"; LCase$(SHA1$40(F$)); ", "; PrintTime(Timer(0.01) - ST!)
    End If
Next I
System
Function SHA1$40 (I$)
    Dim As _Unsigned Long I, Part, Message_Parts, Parts, Message_Length
    Dim As _Unsigned Long k, h(0 To 4), a, b, c, d, e, f, w(0 To 79), temp
    h(0) = &H67452301
    h(1) = &HEFCDAB89
    h(2) = &H98BADCFE
    h(3) = &H10325476
    h(4) = &HC3D2E1F0
    Message_Length = Len(I$)
    If (Message_Length And 63) > 55 Then Padding$ = String$(128 - (Message_Length And 63), 0) Else Padding$ = String$(64 - (Message_Length And 63), 0)
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
        For I = 0 To 15
            w(I) = ReverseCVL(Mid$(CurrentBlock$, I * 4 + 1, 4))
        Next I
        For I = 16 To 79
            w(I) = leftRotateLong(w(I - 3) Xor w(I - 8) Xor w(I - 14) Xor w(I - 16), 1)
        Next I
        a = h(0): b = h(1): c = h(2): d = h(3): e = h(4)
        For I = 0 To 79
            Select Case I
                Case 0 To 19
                    f = (b And c) Or ((Not b) And d)
                    k = &H5A827999
                Case 20 To 39
                    f = b Xor c Xor d
                    k = &H6ED9EBA1
                Case 40 To 59
                    f = (b And c) Xor (b And d) Xor (c And d)
                    k = &H8F1BBCDC
                Case 60 To 79
                    f = b Xor c Xor d
                    k = &HCA62C1D6
            End Select
            temp = leftRotateLong(a, 5) + f + e + k + w(I)
            e = d
            d = c
            c = leftRotateLong(b, 30)
            b = a
            a = temp
        Next I

        h(0) = h(0) + a
        h(1) = h(1) + b
        h(2) = h(2) + c
        h(3) = h(3) + d
        h(4) = h(4) + e
    Next Part
    digest$ = String$(40, 0)
    For I = 0 To 4
        Mid$(digest$, I * 8 + 1, 8) = LongToHex$(h(I))
    Next I
    SHA1$40 = digest$
End Function
'$Include:'include\leftRotate.bm'
'$Include:'include\rightRotate.bm'
'$Include:'include\hex.bm'
'$Include:'include\bits.bm'
'$Include:'include\reversecvl.bm'
'$Include:'include\printtime.bm'
