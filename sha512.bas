$Console:Only
Print "Check: "; LCase$(SHA512$128(""))
Sleep
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
    h(0) = &H6A09E667F3BCC908: h(1) = &HBB67AE8584CAA73B: h(2) = &H3C6EF372FE94F82B: h(3) = &HA54FF53A5F1D36F1: h(4) = &H510E527FADE682D1: h(5) = &H9B05688C2B3E6C1F: h(6) = &H1F83D9ABFB41BD6B: h(7) = &H5BE0CD19137E2179
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
    Data &H428a2f98d728ae22,&H7137449123ef65cd
    Data &Hb5c0fbcfec4d3b2f,&He9b5dba58189dbbc
    Data &H3956c25bf348b538,&H59f111f1b605d019
    Data &H923f82a4af194f9b,&Hab1c5ed5da6d8118
    Data &Hd807aa98a3030242,&H12835b0145706fbe
    Data &H243185be4ee4b28c,&H550c7dc3d5ffb4e2
    Data &H72be5d74f27b896f,&H80deb1fe3b1696b1
    Data &H9bdc06a725c71235,&Hc19bf174cf692694
    Data &He49b69c19ef14ad2,&Hefbe4786384f25e3
    Data &H0fc19dc68b8cd5b5,&H240ca1cc77ac9c65
    Data &H2de92c6f592b0275,&H4a7484aa6ea6e483
    Data &H5cb0a9dcbd41fbd4,&H76f988da831153b5
    Data &H983e5152ee66dfab,&Ha831c66d2db43210
    Data &Hb00327c898fb213f,&Hbf597fc7beef0ee4
    Data &Hc6e00bf33da88fc2,&Hd5a79147930aa725
    Data &H06ca6351e003826f,&H142929670a0e6e70
    Data &H27b70a8546d22ffc,&H2e1b21385c26c926
    Data &H4d2c6dfc5ac42aed,&H53380d139d95b3df
    Data &H650a73548baf63de,&H766a0abb3c77b2a8
    Data &H81c2c92e47edaee6,&H92722c851482353b
    Data &Ha2bfe8a14cf10364,&Ha81a664bbc423001
    Data &Hc24b8b70d0f89791,&Hc76c51a30654be30
    Data &Hd192e819d6ef5218,&Hd69906245565a910
    Data &Hf40e35855771202a,&H106aa07032bbd1b8
    Data &H19a4c116b8d2d0c8,&H1e376c085141ab53
    Data &H2748774cdf8eeb99,&H34b0bcb5e19b48a8
    Data &H391c0cb3c5c95a63,&H4ed8aa4ae3418acb
    Data &H5b9cca4f7763e373,&H682e6ff3d6b2b8a3
    Data &H748f82ee5defb2fc,&H78a5636f43172f60
    Data &H84c87814a1f0ab72,&H8cc702081a6439ec
    Data &H90befffa23631e28,&Ha4506cebde82bde9
    Data &Hbef9a3f7b2c67915,&Hc67178f2e372532b
    Data &Hca273eceea26619c,&Hd186b8c721c0c207
    Data &Heada7dd6cde0eb1e,&Hf57d4f7fee6ed178
    Data &H06f067aa72176fba,&H0a637dc5a2c898a6
    Data &H113f9804bef90dae,&H1b710b35131c471b
    Data &H28db77f523047d84,&H32caab7b40c72493
    Data &H3c9ebe0a15c9bebc,&H431d67c49c100d4c
    Data &H4cc5d4becb3e42b6,&H597f299cfc657e2a
    Data &H5fcb6fab3ad6faec,&H6c44198c4a475817
End Function
'$Include:'include\rightRotate.bm'
'$Include:'include\hex.bm'
'$Include:'include\bits.bm'
'$Include:'include\reversecvi64.bm'
'$Include:'include\printtime.bm'
