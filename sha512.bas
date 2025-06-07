$Console:Only
For I = 1 To _CommandCount
    If _FileExists(Command$(I)) Then
        Open Command$(I) For Binary As #1
        F$ = String$(LOF(1), 0)
        Get #1, , F$
        Close #1
        ST! = Timer(0.01)
        Print Command$(I); "->"; LCase$(SHA512$128(F$)); ", "; PrintTime(Timer(0.01) - ST!)
    End If
Next I
System
Function SHA512$128 (I$)
    Dim As _Unsigned _Integer64 I, Part, Message_Parts, Parts, Message_Length
    Dim As _Unsigned _Integer64 h(0 To 7), a, b, c, d, e, f, g, h, w(0 To 79), s0, s1, ch, temp1, temp2, maj
    Static As _Unsigned _Integer64 k(0 To 79)
    If k(0) = 0 Then
        Restore SHA512_RoundConstants
        For I = 0 To 79
            Read k(I)
        Next I
    End If
    h(0) = &H6A09E667F3BCC908
    h(1) = &HBB67AE8584CAA73B
    h(2) = &H3C6EF372FE94F82B
    h(3) = &HA54FF53A5F1D36F1
    h(4) = &H510E527FADE682D1
    h(5) = &H9B05688C2B3E6C1F
    h(6) = &H1F83D9ABFB41BD6B
    h(7) = &H5BE0CD19137E2179
    Message_Length = Len(I$)
    If (Message_Length And 127) > 119 Then Padding$ = String$(256 - (Message_Length And 127), 0) Else Padding$ = String$(128 - (Message_Length And 127), 0)
    Message_Length = _SHL(Message_Length, 3)
    Asc(Padding$, 1) = 128
    Asc(Padding$, Len(Padding$)) = Message_Length And 255
    Asc(Padding$, Len(Padding$) - 1) = _SHR(Message_Length, 8) And 255
    Asc(Padding$, Len(Padding$) - 2) = _SHR(Message_Length, 16) And 255
    Asc(Padding$, Len(Padding$) - 3) = _SHR(Message_Length, 24) And 255
    Asc(Padding$, Len(Padding$) - 4) = _SHR(Message_Length, 32) And 255
    Asc(Padding$, Len(Padding$) - 5) = _SHR(Message_Length, 40) And 255
    Asc(Padding$, Len(Padding$) - 6) = _SHR(Message_Length, 48) And 255
    Asc(Padding$, Len(Padding$) - 7) = _SHR(Message_Length, 56) And 255 'it should be 128-bits
    Message_Parts = _SHR(Message_Length, 10)
    Padding$ = Mid$(I$, _SHL(Message_Parts, 7) + 1) + Padding$
    Parts = Message_Parts + _SHR(Len(Padding$), 7)
    For Part = 1 To Parts
        If Part > Message_Parts Then
            CurrentBlock$ = Mid$(Padding$, (Part - Message_Parts) * 128 - 127, 128)
        Else
            CurrentBlock$ = Mid$(I$, Part * 128 - 127, 128)
        End If
        For I = 0 To 15
            w(I) = ReverseCVI64(Mid$(CurrentBlock$, I * 8 + 1, 8))
        Next I
        For I = 16 To 79
            s0 = rightRotateInteger64(w(I - 15), 1) Xor rightRotateInteger64(w(I - 15), 8) Xor _SHR(w(I - 15), 7)
            s1 = rightRotateInteger64(w(I - 2), 19) Xor rightRotateInteger64(w(I - 2), 61) Xor _SHR(w(I - 2), 6)
            w(I) = w(I - 16) + s0 + w(I - 7) + s1
        Next I
        a = h(0): b = h(1): c = h(2): d = h(3): e = h(4): f = h(5): g = h(6): h = h(7)
        For I = 0 To 79
            s1 = rightRotateInteger64(e, 14) Xor rightRotateInteger64(e, 18) Xor rightRotateInteger64(e, 41)
            ch = g Xor (e And (f Xor g)) '(e And f) Xor ((Not e) And g)
            temp1 = h + s1 + ch + k(I) + w(I)
            s0 = rightRotateInteger64(a, 28) Xor rightRotateInteger64(a, 34) Xor rightRotateInteger64(a, 39)
            maj = ((a Or b) And c) Or (a And b) '(a And b) Xor (a And c) Xor (b And c)
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
    digest$ = String$(128, 0)
    For I = 0 To 7
        Mid$(digest$, I * 16 + 1, 16) = Integer64ToHex$(h(I))
    Next I
    SHA512$128 = digest$
    Exit Function
    SHA512_RoundConstants:
    Data &H428a2f98d728ae22,&H7137449123ef65cd,&Hb5c0fbcfec4d3b2f,&He9b5dba58189dbbc,&H3956c25bf348b538,&H59f111f1b605d019,&H923f82a4af194f9b,&Hab1c5ed5da6d8118
    Data &Hd807aa98a3030242,&H12835b0145706fbe,&H243185be4ee4b28c,&H550c7dc3d5ffb4e2,&H72be5d74f27b896f,&H80deb1fe3b1696b1,&H9bdc06a725c71235,&Hc19bf174cf692694
    Data &He49b69c19ef14ad2,&Hefbe4786384f25e3,&H0fc19dc68b8cd5b5,&H240ca1cc77ac9c65,&H2de92c6f592b0275,&H4a7484aa6ea6e483,&H5cb0a9dcbd41fbd4,&H76f988da831153b5
    Data &H983e5152ee66dfab,&Ha831c66d2db43210,&Hb00327c898fb213f,&Hbf597fc7beef0ee4,&Hc6e00bf33da88fc2,&Hd5a79147930aa725,&H06ca6351e003826f,&H142929670a0e6e70
    Data &H27b70a8546d22ffc,&H2e1b21385c26c926,&H4d2c6dfc5ac42aed,&H53380d139d95b3df,&H650a73548baf63de,&H766a0abb3c77b2a8,&H81c2c92e47edaee6,&H92722c851482353b
    Data &Ha2bfe8a14cf10364,&Ha81a664bbc423001,&Hc24b8b70d0f89791,&Hc76c51a30654be30,&Hd192e819d6ef5218,&Hd69906245565a910,&Hf40e35855771202a,&H106aa07032bbd1b8
    Data &H19a4c116b8d2d0c8,&H1e376c085141ab53,&H2748774cdf8eeb99,&H34b0bcb5e19b48a8,&H391c0cb3c5c95a63,&H4ed8aa4ae3418acb,&H5b9cca4f7763e373,&H682e6ff3d6b2b8a3
    Data &H748f82ee5defb2fc,&H78a5636f43172f60,&H84c87814a1f0ab72,&H8cc702081a6439ec,&H90befffa23631e28,&Ha4506cebde82bde9,&Hbef9a3f7b2c67915,&Hc67178f2e372532b
    Data &Hca273eceea26619c,&Hd186b8c721c0c207,&Heada7dd6cde0eb1e,&Hf57d4f7fee6ed178,&H06f067aa72176fba,&H0a637dc5a2c898a6,&H113f9804bef90dae,&H1b710b35131c471b
    Data &H28db77f523047d84,&H32caab7b40c72493,&H3c9ebe0a15c9bebc,&H431d67c49c100d4c,&H4cc5d4becb3e42b6,&H597f299cfc657e2a,&H5fcb6fab3ad6faec,&H6c44198c4a475817
End Function
Function SHA384$96 (I$)
    Dim As _Unsigned _Integer64 I, Part, Message_Parts, Parts, Message_Length
    Dim As _Unsigned _Integer64 h(0 To 7), a, b, c, d, e, f, g, h, w(0 To 79), s0, s1, ch, temp1, temp2, maj
    Static As _Unsigned _Integer64 k(0 To 79)
    If k(0) = 0 Then
        Restore SHA512_RoundConstants
        For I = 0 To 79
            Read k(I)
        Next I
    End If
    h(0) = &HCBBB9D5DC1059ED8
    h(1) = &H629A292A367CD507
    h(2) = &H9159015A3070DD17
    h(3) = &H152FECD8F70E5939
    h(4) = &H67332667FFC00B31
    h(5) = &H8EB44A8768581511
    h(6) = &HDB0C2E0D64F98FA7
    h(7) = &H47B5481DBEFA4FA4
    Message_Length = Len(I$)
    If (Message_Length And 127) > 119 Then Padding$ = String$(256 - (Message_Length And 127), 0) Else Padding$ = String$(128 - (Message_Length And 127), 0)
    Message_Length = _SHL(Message_Length, 3)
    Asc(Padding$, 1) = 128
    Asc(Padding$, Len(Padding$)) = Message_Length And 255
    Asc(Padding$, Len(Padding$) - 1) = _SHR(Message_Length, 8) And 255
    Asc(Padding$, Len(Padding$) - 2) = _SHR(Message_Length, 16) And 255
    Asc(Padding$, Len(Padding$) - 3) = _SHR(Message_Length, 24) And 255
    Asc(Padding$, Len(Padding$) - 4) = _SHR(Message_Length, 32) And 255
    Asc(Padding$, Len(Padding$) - 5) = _SHR(Message_Length, 40) And 255
    Asc(Padding$, Len(Padding$) - 6) = _SHR(Message_Length, 48) And 255
    Asc(Padding$, Len(Padding$) - 7) = _SHR(Message_Length, 56) And 255 'it should be 128-bits
    Message_Parts = _SHR(Message_Length, 10)
    Padding$ = Mid$(I$, _SHL(Message_Parts, 7) + 1) + Padding$
    Parts = Message_Parts + _SHR(Len(Padding$), 7)
    For Part = 1 To Parts
        If Part > Message_Parts Then
            CurrentBlock$ = Mid$(Padding$, (Part - Message_Parts) * 128 - 127, 128)
        Else
            CurrentBlock$ = Mid$(I$, Part * 128 - 127, 128)
        End If
        For I = 0 To 15
            w(I) = ReverseCVI64(Mid$(CurrentBlock$, I * 8 + 1, 8))
        Next I
        For I = 16 To 79
            s0 = rightRotateInteger64(w(I - 15), 1) Xor rightRotateInteger64(w(I - 15), 8) Xor _SHR(w(I - 15), 7)
            s1 = rightRotateInteger64(w(I - 2), 19) Xor rightRotateInteger64(w(I - 2), 61) Xor _SHR(w(I - 2), 6)
            w(I) = w(I - 16) + s0 + w(I - 7) + s1
        Next I
        a = h(0): b = h(1): c = h(2): d = h(3): e = h(4): f = h(5): g = h(6): h = h(7)
        For I = 0 To 79
            s1 = rightRotateInteger64(e, 14) Xor rightRotateInteger64(e, 18) Xor rightRotateInteger64(e, 41)
            ch = g Xor (e And (f Xor g)) '(e And f) Xor ((Not e) And g)
            temp1 = h + s1 + ch + k(I) + w(I)
            s0 = rightRotateInteger64(a, 28) Xor rightRotateInteger64(a, 34) Xor rightRotateInteger64(a, 39)
            maj = ((a Or b) And c) Or (a And b) '(a And b) Xor (a And c) Xor (b And c)
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
    digest$ = String$(96, 0)
    For I = 0 To 5
        Mid$(digest$, I * 16 + 1, 16) = Integer64ToHex$(h(I))
    Next I
    SHA384$96 = digest$
End Function
Function SHA512_224$56 (I$)
    Dim As _Unsigned _Integer64 I, Part, Message_Parts, Parts, Message_Length
    Dim As _Unsigned _Integer64 h(0 To 7), a, b, c, d, e, f, g, h, w(0 To 79), s0, s1, ch, temp1, temp2, maj
    Static As _Unsigned _Integer64 k(0 To 79)
    If k(0) = 0 Then
        Restore SHA512_RoundConstants
        For I = 0 To 79
            Read k(I)
        Next I
    End If
    h(0) = &H8C3D37C819544DA2
    h(1) = &H73E1996689DCD4D6
    h(2) = &H1DFAB7AE32FF9C82
    h(3) = &H679DD514582F9FCF
    h(4) = &H0F6D2B697BD44DA8
    h(5) = &H77E36F7304C48942
    h(6) = &H3F9D85A86A1D36C8
    h(7) = &H1112E6AD91D692A1
    Message_Length = Len(I$)
    If (Message_Length And 127) > 119 Then Padding$ = String$(256 - (Message_Length And 127), 0) Else Padding$ = String$(128 - (Message_Length And 127), 0)
    Message_Length = _SHL(Message_Length, 3)
    Asc(Padding$, 1) = 128
    Asc(Padding$, Len(Padding$)) = Message_Length And 255
    Asc(Padding$, Len(Padding$) - 1) = _SHR(Message_Length, 8) And 255
    Asc(Padding$, Len(Padding$) - 2) = _SHR(Message_Length, 16) And 255
    Asc(Padding$, Len(Padding$) - 3) = _SHR(Message_Length, 24) And 255
    Asc(Padding$, Len(Padding$) - 4) = _SHR(Message_Length, 32) And 255
    Asc(Padding$, Len(Padding$) - 5) = _SHR(Message_Length, 40) And 255
    Asc(Padding$, Len(Padding$) - 6) = _SHR(Message_Length, 48) And 255
    Asc(Padding$, Len(Padding$) - 7) = _SHR(Message_Length, 56) And 255 'it should be 128-bits
    Message_Parts = _SHR(Message_Length, 10)
    Padding$ = Mid$(I$, _SHL(Message_Parts, 7) + 1) + Padding$
    Parts = Message_Parts + _SHR(Len(Padding$), 7)
    For Part = 1 To Parts
        If Part > Message_Parts Then
            CurrentBlock$ = Mid$(Padding$, (Part - Message_Parts) * 128 - 127, 128)
        Else
            CurrentBlock$ = Mid$(I$, Part * 128 - 127, 128)
        End If
        For I = 0 To 15
            w(I) = ReverseCVI64(Mid$(CurrentBlock$, I * 8 + 1, 8))
        Next I
        For I = 16 To 79
            s0 = rightRotateInteger64(w(I - 15), 1) Xor rightRotateInteger64(w(I - 15), 8) Xor _SHR(w(I - 15), 7)
            s1 = rightRotateInteger64(w(I - 2), 19) Xor rightRotateInteger64(w(I - 2), 61) Xor _SHR(w(I - 2), 6)
            w(I) = w(I - 16) + s0 + w(I - 7) + s1
        Next I
        a = h(0): b = h(1): c = h(2): d = h(3): e = h(4): f = h(5): g = h(6): h = h(7)
        For I = 0 To 79
            s1 = rightRotateInteger64(e, 14) Xor rightRotateInteger64(e, 18) Xor rightRotateInteger64(e, 41)
            ch = g Xor (e And (f Xor g)) '(e And f) Xor ((Not e) And g)
            temp1 = h + s1 + ch + k(I) + w(I)
            s0 = rightRotateInteger64(a, 28) Xor rightRotateInteger64(a, 34) Xor rightRotateInteger64(a, 39)
            maj = ((a Or b) And c) Or (a And b) '(a And b) Xor (a And c) Xor (b And c)
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
    For I = 0 To 3
        Mid$(digest$, I * 16 + 1, 16) = Integer64ToHex$(h(I))
    Next I
    SHA512_224$56 = digest$
End Function
Function SHA512_256$64 (I$)
    Dim As _Unsigned _Integer64 I, Part, Message_Parts, Parts, Message_Length
    Dim As _Unsigned _Integer64 h(0 To 7), a, b, c, d, e, f, g, h, w(0 To 79), s0, s1, ch, temp1, temp2, maj
    Static As _Unsigned _Integer64 k(0 To 79)
    If k(0) = 0 Then
        Restore SHA512_RoundConstants
        For I = 0 To 79
            Read k(I)
        Next I
    End If
    h(0) = &H22312194FC2BF72C
    h(1) = &H9F555FA3C84C64C2
    h(2) = &H2393B86B6F53B151
    h(3) = &H963877195940EABD
    h(4) = &H96283EE2A88EFFE3
    h(5) = &HBE5E1E2553863992
    h(6) = &H2B0199FC2C85B8AA
    h(7) = &H0EB72DDC81C52CA2
    Message_Length = Len(I$)
    If (Message_Length And 127) > 119 Then Padding$ = String$(256 - (Message_Length And 127), 0) Else Padding$ = String$(128 - (Message_Length And 127), 0)
    Message_Length = _SHL(Message_Length, 3)
    Asc(Padding$, 1) = 128
    Asc(Padding$, Len(Padding$)) = Message_Length And 255
    Asc(Padding$, Len(Padding$) - 1) = _SHR(Message_Length, 8) And 255
    Asc(Padding$, Len(Padding$) - 2) = _SHR(Message_Length, 16) And 255
    Asc(Padding$, Len(Padding$) - 3) = _SHR(Message_Length, 24) And 255
    Asc(Padding$, Len(Padding$) - 4) = _SHR(Message_Length, 32) And 255
    Asc(Padding$, Len(Padding$) - 5) = _SHR(Message_Length, 40) And 255
    Asc(Padding$, Len(Padding$) - 6) = _SHR(Message_Length, 48) And 255
    Asc(Padding$, Len(Padding$) - 7) = _SHR(Message_Length, 56) And 255 'it should be 128-bits
    Message_Parts = _SHR(Message_Length, 10)
    Padding$ = Mid$(I$, _SHL(Message_Parts, 7) + 1) + Padding$
    Parts = Message_Parts + _SHR(Len(Padding$), 7)
    For Part = 1 To Parts
        If Part > Message_Parts Then
            CurrentBlock$ = Mid$(Padding$, (Part - Message_Parts) * 128 - 127, 128)
        Else
            CurrentBlock$ = Mid$(I$, Part * 128 - 127, 128)
        End If
        For I = 0 To 15
            w(I) = ReverseCVI64(Mid$(CurrentBlock$, I * 8 + 1, 8))
        Next I
        For I = 16 To 79
            s0 = rightRotateInteger64(w(I - 15), 1) Xor rightRotateInteger64(w(I - 15), 8) Xor _SHR(w(I - 15), 7)
            s1 = rightRotateInteger64(w(I - 2), 19) Xor rightRotateInteger64(w(I - 2), 61) Xor _SHR(w(I - 2), 6)
            w(I) = w(I - 16) + s0 + w(I - 7) + s1
        Next I
        a = h(0): b = h(1): c = h(2): d = h(3): e = h(4): f = h(5): g = h(6): h = h(7)
        For I = 0 To 79
            s1 = rightRotateInteger64(e, 14) Xor rightRotateInteger64(e, 18) Xor rightRotateInteger64(e, 41)
            ch = g Xor (e And (f Xor g)) '(e And f) Xor ((Not e) And g)
            temp1 = h + s1 + ch + k(I) + w(I)
            s0 = rightRotateInteger64(a, 28) Xor rightRotateInteger64(a, 34) Xor rightRotateInteger64(a, 39)
            maj = ((a Or b) And c) Or (a And b) '(a And b) Xor (a And c) Xor (b And c)
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
    For I = 0 To 3
        Mid$(digest$, I * 16 + 1, 16) = Integer64ToHex$(h(I))
    Next I
    SHA512_256$64 = digest$
End Function
'$Include:'include\rightRotate.bm'
'$Include:'include\hex.bm'
'$Include:'include\bits.bm'
'$Include:'include\reversecvi64.bm'
'$Include:'include\printtime.bm'
