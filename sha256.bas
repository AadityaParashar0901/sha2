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
        Print INFILE$; "->"; LCase$(SHA256$64(F$)); ", "; PrintTime(Timer(0.01) - ST!)
    End If
Next I
System
Function SHA224$56 (I$)
    Dim As _Unsigned Long I, Part, Message_Parts, Parts, Message_Length
    Dim As _Unsigned Long h(0 To 7), a, b, c, d, e, f, g, h, w(0 To 63), s0, s1, ch, temp1, temp2, maj
    Static As _Unsigned Long k(0 To 63)
    If k(0) = 0 Then
        Restore SHA256_RoundConstants
        For I = 0 To 63
            Read k(I)
        Next I
    End If
    h(0) = &HC1059ED8: h(1) = &H367CD507: h(2) = &H3070DD17: h(3) = &HF70E5939: h(4) = &HFFC00B31: h(5) = &H68581511: h(6) = &H64F98FA7: h(7) = &HBEFA4FA4
    Message_Length = Len(I$)
    If (Message_Length And 63) > 55 Then Padding$ = String$(128 - (Message_Length And 63), 0) Else Padding$ = String$(64 - (Message_Length And 63), 0)
    Message_Length = _SHL(Message_Length, 3)
    Asc(Padding$, 1) = 128
    Asc(Padding$, Len(Padding$)) = Message_Length And 255
    Asc(Padding$, Len(Padding$) - 1) = _SHR(Message_Length, 8) And 255
    Asc(Padding$, Len(Padding$) - 2) = _SHR(Message_Length, 16) And 255
    Message_Parts = _SHR(Message_Length, 9)
    Padding$ = Mid$(I$, _SHL(Message_Parts, 6) + 1) + Padding$
    Parts = Message_Parts + _SHR(Len(Padding$), 6)
    For Part = 1 To Parts
        If Part > Message_Parts Then
            CurrentBlock$ = Mid$(Padding$, (Part - Message_Parts) * 64 - 63, 64)
        Else
            CurrentBlock$ = Mid$(I$, Part * 64 - 63, 64)
        End If
        For I = 0 To 15
            w(I) = ReverseCVL(Mid$(CurrentBlock$, I * 4 + 1, 4))
        Next I
        For I = 16 To 63
            s0 = rightRotateLong(w(I - 15), 7) Xor rightRotateLong(w(I - 15), 18) Xor _SHR(w(I - 15), 3)
            s1 = rightRotateLong(w(I - 2), 17) Xor rightRotateLong(w(I - 2), 19) Xor _SHR(w(I - 2), 10)
            w(I) = w(I - 16) + s0 + w(I - 7) + s1
        Next I
        a = h(0): b = h(1): c = h(2): d = h(3): e = h(4): f = h(5): g = h(6): h = h(7)
        For I = 0 To 63
            s1 = rightRotateLong(e, 6) Xor rightRotateLong(e, 11) Xor rightRotateLong(e, 25)
            ch = (e And f) Xor ((Not e) And g)
            temp1 = h + s1 + ch + k(I) + w(I)
            s0 = rightRotateLong(a, 2) Xor rightRotateLong(a, 13) Xor rightRotateLong(a, 22)
            maj = (a And b) Xor (a And c) Xor (b And c)
            temp2 = s0 + maj
            h = g
            g = f
            f = e
            e = d + temp1
            d = c
            c = b
            b = a
            a = temp1 + temp2
        Next I

        h(0) = h(0) + a
        h(1) = h(1) + b
        h(2) = h(2) + c
        h(3) = h(3) + d
        h(4) = h(4) + e
        h(5) = h(5) + f
        h(6) = h(6) + g
        h(7) = h(7) + h
    Next Part
    digest$ = String$(56, 0)
    For I = 0 To 6
        Mid$(digest$, I * 8 + 1, 8) = LongToHex$(h(I))
    Next I
    SHA224$56 = digest$
    Exit Function
End Function
Function SHA256$64 (I$)
    Dim As _Unsigned Long I, Part, Message_Parts, Parts, Message_Length
    Dim As _Unsigned Long h(0 To 7), a, b, c, d, e, f, g, h, w(0 To 63), s0, s1, ch, temp1, temp2, maj
    Static As _Unsigned Long k(0 To 63)
    If k(0) = 0 Then
        Restore SHA256_RoundConstants
        For I = 0 To 63
            Read k(I)
        Next I
    End If
    h(0) = &H6A09E667: h(1) = &HBB67AE85: h(2) = &H3C6EF372: h(3) = &HA54FF53A: h(4) = &H510E527F: h(5) = &H9B05688C: h(6) = &H1F83D9AB: h(7) = &H5BE0CD19
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
        For I = 16 To 63
            s0 = rightRotateLong(w(I - 15), 7) Xor rightRotateLong(w(I - 15), 18) Xor _SHR(w(I - 15), 3)
            s1 = rightRotateLong(w(I - 2), 17) Xor rightRotateLong(w(I - 2), 19) Xor _SHR(w(I - 2), 10)
            w(I) = w(I - 16) + s0 + w(I - 7) + s1
        Next I
        a = h(0): b = h(1): c = h(2): d = h(3): e = h(4): f = h(5): g = h(6): h = h(7)
        For I = 0 To 63
            s1 = rightRotateLong(e, 6) Xor rightRotateLong(e, 11) Xor rightRotateLong(e, 25)
            ch = (e And f) Xor ((Not e) And g)
            temp1 = h + s1 + ch + k(I) + w(I)
            s0 = rightRotateLong(a, 2) Xor rightRotateLong(a, 13) Xor rightRotateLong(a, 22)
            maj = (a And b) Xor (a And c) Xor (b And c)
            temp2 = s0 + maj
            h = g
            g = f
            f = e
            e = d + temp1
            d = c
            c = b
            b = a
            a = temp1 + temp2
        Next I

        h(0) = h(0) + a
        h(1) = h(1) + b
        h(2) = h(2) + c
        h(3) = h(3) + d
        h(4) = h(4) + e
        h(5) = h(5) + f
        h(6) = h(6) + g
        h(7) = h(7) + h
    Next Part
    digest$ = String$(64, 0)
    For I = 0 To 7
        Mid$(digest$, I * 8 + 1, 8) = LongToHex$(h(I))
    Next I
    SHA256$64 = digest$
    Exit Function
    SHA256_RoundConstants:
    Data &H428a2f98,&H71374491,&Hb5c0fbcf,&He9b5dba5,&H3956c25b,&H59f111f1,&H923f82a4,&Hab1c5ed5,&Hd807aa98,&H12835b01,&H243185be,&H550c7dc3,&H72be5d74,&H80deb1fe,&H9bdc06a7,&Hc19bf174,&He49b69c1,&Hefbe4786,&H0fc19dc6,&H240ca1cc,&H2de92c6f,&H4a7484aa,&H5cb0a9dc,&H76f988da,&H983e5152,&Ha831c66d,&Hb00327c8,&Hbf597fc7,&Hc6e00bf3,&Hd5a79147,&H06ca6351,&H14292967,&H27b70a85,&H2e1b2138,&H4d2c6dfc,&H53380d13,&H650a7354,&H766a0abb,&H81c2c92e,&H92722c85,&Ha2bfe8a1,&Ha81a664b,&Hc24b8b70,&Hc76c51a3,&Hd192e819,&Hd6990624,&Hf40e3585,&H106aa070,&H19a4c116,&H1e376c08,&H2748774c,&H34b0bcb5,&H391c0cb3,&H4ed8aa4a,&H5b9cca4f,&H682e6ff3,&H748f82ee,&H78a5636f,&H84c87814,&H8cc70208,&H90befffa,&Ha4506ceb,&Hbef9a3f7,&Hc67178f2
End Function
'$Include:'include\rightRotate.bm'
'$Include:'include\hex.bm'
'$Include:'include\bits.bm'
'$Include:'include\reversecvl.bm'
'$Include:'include\printtime.bm'
