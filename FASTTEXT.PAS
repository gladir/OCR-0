{ @author: Sylvain Maltais (support@gladir.com)
  @created: 2026
  @website(https://www.gladir.com/OCR-0)
  @abstract(Target: Free Pascal 3.2)
  @description: Clone de la commande fasttext (classification de texte supervisee) :
                fasttext predict-prob lid.176.ftz texte.txt 3

  Commandes prises en charge :
    fasttext predict      modele donnees [k] [seuil]   etiquettes les plus probables
    fasttext predict-prob modele donnees [k] [seuil]   etiquettes et probabilites

  Le modele est lu au format binaire de fastText (.bin dense ou .ftz quantifie,
  version 12) : arguments, dictionnaire (mots, etiquettes, index d'elagage),
  matrice d'entree (dense ou quantifiee par produit de quantificateurs), matrice
  de sortie et, pour la perte hs, l'arbre de Huffman reconstruit a partir des
  effectifs des etiquettes. Pour chaque ligne, les mots et leurs n-grammes de
  caracteres (hachage FNV-1a de fastText) forment la moyenne des vecteurs d'entree ;
  le classement (softmax, softmax hierarchique ou ova) donne les k meilleures
  etiquettes. Comme fastText, la ligne est lue mot par mot, sans mise en minuscules.

  Le modele lid.176.ftz (identification de 176 langues) se telecharge sur
  https://fasttext.cc/docs/en/language-identification.html

  Ce programme demande Free Pascal : les modeles depassent 64 Ko de memoire.
}

{$mode objfpc}
{$H-}
{$R-}
{$Q-}

Program FASTTEXT;

Const
  MagicNumber = 793712314;
  LabelPrefix = '__label__';
  EosToken = '</s>';
  LossHs = 1;
  LossNs = 2;
  LossSoftmax = 3;
  LossOva = 4;
  MaxLineWords = 1000000;

Type
  TByteArr = Array[0..MaxLongint - 1] Of Byte;
  PByteArr = ^TByteArr;
  TSingleArr = Array[0..(MaxLongint Div 4) - 1] Of Single;
  PSingleArr = ^TSingleArr;
  TLongArr = Array[0..(MaxLongint Div 4) - 1] Of Longint;
  PLongArr = ^TLongArr;
  TInt64Arr = Array[0..(MaxLongint Div 8) - 1] Of Int64;
  PInt64Arr = ^TInt64Arr;
  TWordText = String[255];
  TWordArr = Array[0..(MaxLongint Div 256) - 1] Of TWordText;
  PWordArr = ^TWordArr;

  TProductQuantizer = Record
    Dim, NSubQ, DSub, LastDSub: Longint;
    Cent: PSingleArr;
  End;

  TQuantMatrix = Record
    QNorm: Boolean;
    M, N: Int64;
    CodeSize: Longint;
    Codes: PByteArr;
    NormCodes: PByteArr;
    PQ, NPQ: TProductQuantizer;
  End;

  TDenseMatrix = Record
    M, N: Int64;
    Data: PSingleArr;
  End;

Var
  Buf: PByteArr;
  BufSize, BufPos: Longint;

  { Arguments du modele }
  Dim, WordNgrams, LossName, ModelName, Bucket, MinN, MaxN, Version: Longint;

  { Dictionnaire }
  DictSize, NWords, NLabels: Longint;
  NTokens, PruneSize: Int64;
  Words: PWordArr;
  WordTypes: PByteArr;
  WordCounts: PInt64Arr;
  WordHash: PLongArr;
  WordHashSize: Longint;
  PruneKeys, PruneValues: PLongArr;
  PruneHashSize: Longint;

  { Matrices }
  QuantInput, QuantOutput: Boolean;
  InQuant, OutQuant: TQuantMatrix;
  InDense, OutDense: TDenseMatrix;

  { Arbre de Huffman (perte hs) }
  TreeLeft, TreeRight, TreeParent: PLongArr;
  TreeCount: PInt64Arr;
  OutputSize: Longint;

  { Entree }
  InputFile: File;
  UseStdin: Boolean;
  InBuffer: Array[0..65535] Of Byte;
  InPos, InLen: Longint;
  Pushed: Longint;

  { Prediction }
  Ids: PLongArr;
  IdCount, IdCapacity: Longint;
  Hidden: PSingleArr;
  HeapScore: Array[0..1023] Of Single;
  HeapNode: Array[0..1023] Of Longint;
  HeapCount, TopK: Longint;
  Threshold: Single;
  PrintProb: Boolean;

{ ------------------------------------------------------------------ }
{ Lecture du modele                                                   }
{ ------------------------------------------------------------------ }

Procedure Fail(Const Text1: String);
Begin
  Writeln(Text1);
  Halt(1);
End;

Function ReadLong: Longint;
Begin
  If BufPos + 4 > BufSize Then Fail('Erreur : fichier modele tronque.');
  Move(Buf^[BufPos], Result, 4);
  Inc(BufPos, 4);
End;

Function ReadInt64: Int64;
Begin
  If BufPos + 8 > BufSize Then Fail('Erreur : fichier modele tronque.');
  Move(Buf^[BufPos], Result, 8);
  Inc(BufPos, 8);
End;

Function ReadDouble: Double;
Begin
  If BufPos + 8 > BufSize Then Fail('Erreur : fichier modele tronque.');
  Move(Buf^[BufPos], Result, 8);
  Inc(BufPos, 8);
End;

Function ReadByteValue: Byte;
Begin
  If BufPos + 1 > BufSize Then Fail('Erreur : fichier modele tronque.');
  ReadByteValue := Buf^[BufPos];
  Inc(BufPos);
End;

Function FnvHash(Const S: String): Longword;
Var
  I: Integer;
  H: Longword;
Begin
  H := 2166136261;
  For I := 1 To Length(S) Do
  Begin
    H := H Xor Longword(Longint(ShortInt(Byte(S[I]))));
    H := H * 16777619;
  End;
  FnvHash := H;
End;

Function PowerOfTwoAbove(Value: Longint): Longint;
Var
  P: Longint;
Begin
  P := 1024;
  While P < Value Do
    P := P * 2;
  PowerOfTwoAbove := P;
End;

Function FindWord(Const S: String): Longint;
Var
  H: Longint;
  Slot: Longint;
Begin
  H := Longint(FnvHash(S) And Longword(WordHashSize - 1));
  Slot := WordHash^[H];
  While Slot <> 0 Do
  Begin
    If Words^[Slot - 1] = S Then
    Begin
      FindWord := Slot - 1;
      Exit;
    End;
    H := (H + 1) And (WordHashSize - 1);
    Slot := WordHash^[H];
  End;
  FindWord := -1;
End;

Procedure LoadProductQuantizer(Var P: TProductQuantizer);
Var
  Count: Longint;
Begin
  P.Dim := ReadLong;
  P.NSubQ := ReadLong;
  P.DSub := ReadLong;
  P.LastDSub := ReadLong;
  Count := P.Dim * 256;
  GetMem(P.Cent, Count * 4);
  If BufPos + Count * 4 > BufSize Then Fail('Erreur : fichier modele tronque.');
  Move(Buf^[BufPos], P.Cent^, Count * 4);
  Inc(BufPos, Count * 4);
End;

Procedure LoadQuantMatrix(Var Q: TQuantMatrix);
Begin
  Q.QNorm := ReadByteValue <> 0;
  Q.M := ReadInt64;
  Q.N := ReadInt64;
  Q.CodeSize := ReadLong;
  If BufPos + Q.CodeSize > BufSize Then Fail('Erreur : fichier modele tronque.');
  Q.Codes := @Buf^[BufPos];
  Inc(BufPos, Q.CodeSize);
  LoadProductQuantizer(Q.PQ);
  Q.NormCodes := Nil;
  If Q.QNorm Then
  Begin
    If BufPos + Q.M > BufSize Then Fail('Erreur : fichier modele tronque.');
    Q.NormCodes := @Buf^[BufPos];
    Inc(BufPos, Q.M);
    LoadProductQuantizer(Q.NPQ);
  End;
End;

Procedure LoadDenseMatrix(Var D: TDenseMatrix);
Var
  Bytes: Int64;
Begin
  D.M := ReadInt64;
  D.N := ReadInt64;
  Bytes := D.M * D.N * 4;
  If BufPos + Bytes > BufSize Then Fail('Erreur : fichier modele tronque.');
  D.Data := PSingleArr(@Buf^[BufPos]);
  Inc(BufPos, Bytes);
End;

Procedure LoadModel(Const Name: String);
Var
  F: File;
  Got, Chunk: Longint;
  I, J, Slot, H: Longint;
  C, Ch: Byte;
  Text1: TWordText;
  Key, Value: Longint;
  Index: Longint;
Begin
  Assign(F, Name);
  {$I-}
  Reset(F, 1);
  {$I+}
  If IOResult <> 0 Then Fail('Erreur : impossible d''ouvrir le fichier modele ' + Name);
  BufSize := FileSize(F);
  GetMem(Buf, BufSize + 16);
  BufPos := 0;
  While BufPos < BufSize Do
  Begin
    Chunk := BufSize - BufPos;
    If Chunk > 1048576 Then Chunk := 1048576;
    BlockRead(F, Buf^[BufPos], Chunk, Got);
    If Got <= 0 Then Break;
    Inc(BufPos, Got);
  End;
  Close(F);
  BufPos := 0;

  If ReadLong <> MagicNumber Then Fail('Erreur : ce fichier n''est pas un modele fastText.');
  Version := ReadLong;
  If Version <> 12 Then
    Fail('Erreur : version de modele non prise en charge (12 attendue).');
  Dim := ReadLong;
  ReadLong; { ws }
  ReadLong; { epoch }
  ReadLong; { minCount }
  ReadLong; { neg }
  WordNgrams := ReadLong;
  LossName := ReadLong;
  ModelName := ReadLong;
  Bucket := ReadLong;
  MinN := ReadLong;
  MaxN := ReadLong;
  ReadLong; { lrUpdateRate }
  ReadDouble; { t }
  If ModelName <> 3 Then
    Fail('Erreur : seuls les modeles supervises (classification) sont pris en charge.');

  { Dictionnaire }
  DictSize := ReadLong;
  NWords := ReadLong;
  NLabels := ReadLong;
  NTokens := ReadInt64;
  PruneSize := ReadInt64;
  GetMem(Words, Int64(DictSize + 1) * SizeOf(TWordText));
  GetMem(WordTypes, DictSize + 1);
  GetMem(WordCounts, Int64(DictSize + 1) * 8);
  WordHashSize := PowerOfTwoAbove(DictSize * 2);
  GetMem(WordHash, Int64(WordHashSize) * 4);
  FillChar(WordHash^, Int64(WordHashSize) * 4, 0);
  For I := 0 To DictSize - 1 Do
  Begin
    Text1 := '';
    Ch := ReadByteValue;
    While Ch <> 0 Do
    Begin
      If Length(Text1) < 255 Then Text1 := Text1 + Chr(Ch);
      Ch := ReadByteValue;
    End;
    Words^[I] := Text1;
    WordCounts^[I] := ReadInt64;
    WordTypes^[I] := ReadByteValue;
    H := Longint(FnvHash(Text1) And Longword(WordHashSize - 1));
    While WordHash^[H] <> 0 Do
      H := (H + 1) And (WordHashSize - 1);
    WordHash^[H] := I + 1;
  End;
  If PruneSize > 0 Then
  Begin
    PruneHashSize := PowerOfTwoAbove(PruneSize * 2);
    GetMem(PruneKeys, Int64(PruneHashSize) * 4);
    GetMem(PruneValues, Int64(PruneHashSize) * 4);
    For I := 0 To PruneHashSize - 1 Do
      PruneKeys^[I] := -1;
    For I := 0 To PruneSize - 1 Do
    Begin
      Key := ReadLong;
      Value := ReadLong;
      H := Key And (PruneHashSize - 1);
      While PruneKeys^[H] >= 0 Do
        H := (H + 1) And (PruneHashSize - 1);
      PruneKeys^[H] := Key;
      PruneValues^[H] := Value;
    End;
  End;

  { Matrice d'entree }
  QuantInput := ReadByteValue <> 0;
  If QuantInput Then
    LoadQuantMatrix(InQuant)
  Else
    LoadDenseMatrix(InDense);
  QuantOutput := ReadByteValue <> 0;
  If QuantInput And QuantOutput Then
    LoadQuantMatrix(OutQuant)
  Else
  Begin
    QuantOutput := False;
    LoadDenseMatrix(OutDense);
  End;
  If QuantOutput Then OutputSize := OutQuant.M Else OutputSize := OutDense.M;

  { Arbre de Huffman pour la perte hs : les etiquettes sont triees par effectif. }
  If LossName = LossHs Then
  Begin
    GetMem(TreeLeft, (2 * OutputSize) * 4);
    GetMem(TreeRight, (2 * OutputSize) * 4);
    GetMem(TreeParent, (2 * OutputSize) * 4);
    GetMem(TreeCount, (2 * OutputSize) * 8);
    For I := 0 To 2 * OutputSize - 2 Do
    Begin
      TreeParent^[I] := -1;
      TreeLeft^[I] := -1;
      TreeRight^[I] := -1;
      TreeCount^[I] := 1000000000000000;
    End;
    For I := 0 To OutputSize - 1 Do
      TreeCount^[I] := WordCounts^[NWords + I];
    Slot := OutputSize - 1;  { feuille courante }
    J := OutputSize;         { noeud courant }
    For I := OutputSize To 2 * OutputSize - 2 Do
    Begin
      Key := 0;
      Value := 0;
      For H := 0 To 1 Do
      Begin
        If (Slot >= 0) And (TreeCount^[Slot] < TreeCount^[J]) Then
        Begin
          Index := Slot;
          Dec(Slot);
        End
        Else
        Begin
          Index := J;
          Inc(J);
        End;
        If H = 0 Then Key := Index Else Value := Index;
      End;
      TreeLeft^[I] := Key;
      TreeRight^[I] := Value;
      TreeCount^[I] := TreeCount^[Key] + TreeCount^[Value];
      TreeParent^[Key] := I;
      TreeParent^[Value] := I;
    End;
  End;
End;

{ ------------------------------------------------------------------ }
{ Calcul du vecteur cache et classement                               }
{ ------------------------------------------------------------------ }

Procedure AddId(Id: Longint);
Begin
  If IdCount >= IdCapacity Then
  Begin
    IdCapacity := IdCapacity * 2;
    ReAllocMem(Ids, IdCapacity * 4);
  End;
  Ids^[IdCount] := Id;
  Inc(IdCount);
End;

Procedure PushHash(Id: Longint);
Var
  H: Longint;
Begin
  If PruneSize = 0 Then Exit;
  If PruneSize > 0 Then
  Begin
    H := Id And (PruneHashSize - 1);
    While (PruneKeys^[H] >= 0) And (PruneKeys^[H] <> Id) Do
      H := (H + 1) And (PruneHashSize - 1);
    If PruneKeys^[H] < 0 Then Exit;
    Id := PruneValues^[H];
  End;
  AddId(Id + NWords);
End;

{ N-grammes de caracteres (UTF-8) de <mot>, de minn a maxn caracteres. }
Procedure ComputeSubwords(Const Token: String);
Var
  Word1, Gram: String;
  I, J, N, Count: Integer;
Begin
  Word1 := '<' + Token + '>';
  Count := Length(Word1);
  For I := 1 To Count Do
  Begin
    If (Byte(Word1[I]) And $C0) = $80 Then Continue;
    Gram := '';
    J := I;
    N := 1;
    While (J <= Count) And (N <= MaxN) Do
    Begin
      Gram := Gram + Word1[J];
      Inc(J);
      While (J <= Count) And ((Byte(Word1[J]) And $C0) = $80) Do
      Begin
        Gram := Gram + Word1[J];
        Inc(J);
      End;
      If (N >= MinN) And Not ((N = 1) And ((I = 1) Or (J > Count))) Then
        PushHash(Longint(FnvHash(Gram) Mod Longword(Bucket)));
      Inc(N);
    End;
  End;
End;

Procedure AddToken(Const Token: String);
Var
  Wid: Longint;
Begin
  If Copy(Token, 1, Length(LabelPrefix)) = LabelPrefix Then Exit;
  Wid := FindWord(Token);
  If (Wid >= 0) And (WordTypes^[Wid] <> 0) Then Exit;
  If Wid < 0 Then
  Begin
    If Token <> EosToken Then ComputeSubwords(Token);
  End
  Else
  Begin
    AddId(Wid);
    If (MaxN > 0) And (Token <> EosToken) Then ComputeSubwords(Token);
  End;
End;

Procedure AddQuantRow(Var Q: TQuantMatrix; Row: Longint; X: PSingleArr);
Var
  Norm: Single;
  M, D, K, Base: Longint;
  C: PSingleArr;
Begin
  Norm := 1;
  If Q.QNorm Then Norm := Q.NPQ.Cent^[Q.NormCodes^[Row]];
  For M := 0 To Q.PQ.NSubQ - 1 Do
  Begin
    D := Q.PQ.DSub;
    If M = Q.PQ.NSubQ - 1 Then
    Begin
      D := Q.PQ.LastDSub;
      Base := M * 256 * Q.PQ.DSub + Q.Codes^[Row * Q.PQ.NSubQ + M] * Q.PQ.LastDSub;
    End
    Else
      Base := (M * 256 + Q.Codes^[Row * Q.PQ.NSubQ + M]) * Q.PQ.DSub;
    C := PSingleArr(@Q.PQ.Cent^[Base]);
    For K := 0 To D - 1 Do
      X^[M * Q.PQ.DSub + K] := X^[M * Q.PQ.DSub + K] + Norm * C^[K];
  End;
End;

Function QuantDotRow(Var Q: TQuantMatrix; Row: Longint; X: PSingleArr): Single;
Var
  Norm, Sum: Single;
  M, D, K, Base: Longint;
  C: PSingleArr;
Begin
  Norm := 1;
  If Q.QNorm Then Norm := Q.NPQ.Cent^[Q.NormCodes^[Row]];
  Sum := 0;
  For M := 0 To Q.PQ.NSubQ - 1 Do
  Begin
    D := Q.PQ.DSub;
    If M = Q.PQ.NSubQ - 1 Then
    Begin
      D := Q.PQ.LastDSub;
      Base := M * 256 * Q.PQ.DSub + Q.Codes^[Row * Q.PQ.NSubQ + M] * Q.PQ.LastDSub;
    End
    Else
      Base := (M * 256 + Q.Codes^[Row * Q.PQ.NSubQ + M]) * Q.PQ.DSub;
    C := PSingleArr(@Q.PQ.Cent^[Base]);
    For K := 0 To D - 1 Do
      Sum := Sum + C^[K] * X^[M * Q.PQ.DSub + K];
  End;
  QuantDotRow := Sum * Norm;
End;

Function OutputDot(Row: Longint): Single;
Var
  K: Longint;
  Sum: Single;
  P: PSingleArr;
Begin
  If QuantOutput Then
    OutputDot := QuantDotRow(OutQuant, Row, Hidden)
  Else
  Begin
    P := PSingleArr(@OutDense.Data^[Int64(Row) * OutDense.N]);
    Sum := 0;
    For K := 0 To OutDense.N - 1 Do
      Sum := Sum + P^[K] * Hidden^[K];
    OutputDot := Sum;
  End;
End;

Procedure ComputeHidden;
Var
  I, K: Longint;
  P: PSingleArr;
Begin
  For K := 0 To Dim - 1 Do
    Hidden^[K] := 0;
  For I := 0 To IdCount - 1 Do
    If QuantInput Then
      AddQuantRow(InQuant, Ids^[I], Hidden)
    Else
    Begin
      P := PSingleArr(@InDense.Data^[Int64(Ids^[I]) * InDense.N]);
      For K := 0 To Dim - 1 Do
        Hidden^[K] := Hidden^[K] + P^[K];
    End;
  For K := 0 To Dim - 1 Do
    Hidden^[K] := Hidden^[K] * (1.0 / IdCount);
End;

Function StdLog(X: Single): Single;
Begin
  StdLog := Ln(X + 0.00001);
End;

Procedure HeapPush(Score: Single; Node: Longint);
Var
  I, Lowest: Longint;
Begin
  If HeapCount < TopK Then
  Begin
    HeapScore[HeapCount] := Score;
    HeapNode[HeapCount] := Node;
    Inc(HeapCount);
  End
  Else
  Begin
    Lowest := 0;
    For I := 1 To HeapCount - 1 Do
      If HeapScore[I] < HeapScore[Lowest] Then Lowest := I;
    If Score > HeapScore[Lowest] Then
    Begin
      HeapScore[Lowest] := Score;
      HeapNode[Lowest] := Node;
    End;
  End;
End;

Function HeapMinimum: Single;
Var
  I: Longint;
  Low: Single;
Begin
  Low := HeapScore[0];
  For I := 1 To HeapCount - 1 Do
    If HeapScore[I] < Low Then Low := HeapScore[I];
  HeapMinimum := Low;
End;

Procedure Dfs(Node: Longint; Score: Single);
Var
  F: Single;
Begin
  If Score < StdLog(Threshold) Then Exit;
  If (HeapCount = TopK) And (Score < HeapMinimum) Then Exit;
  If (TreeLeft^[Node] = -1) And (TreeRight^[Node] = -1) Then
  Begin
    HeapPush(Score, Node);
    Exit;
  End;
  F := OutputDot(Node - OutputSize);
  F := 1.0 / (1.0 + Exp(-F));
  Dfs(TreeLeft^[Node], Score + StdLog(1.0 - F));
  Dfs(TreeRight^[Node], Score + StdLog(F));
End;

Procedure FindBest;
Var
  I: Longint;
  Output: PSingleArr;
  Max, Sum, F: Single;
Begin
  HeapCount := 0;
  If LossName = LossHs Then
    Dfs(2 * OutputSize - 2, 0)
  Else
  Begin
    GetMem(Output, OutputSize * 4);
    For I := 0 To OutputSize - 1 Do
      Output^[I] := OutputDot(I);
    If LossName = LossOva Then
      For I := 0 To OutputSize - 1 Do
        Output^[I] := 1.0 / (1.0 + Exp(-Output^[I]))
    Else
    Begin
      Max := Output^[0];
      For I := 1 To OutputSize - 1 Do
        If Output^[I] > Max Then Max := Output^[I];
      Sum := 0;
      For I := 0 To OutputSize - 1 Do
      Begin
        Output^[I] := Exp(Output^[I] - Max);
        Sum := Sum + Output^[I];
      End;
      For I := 0 To OutputSize - 1 Do
        Output^[I] := Output^[I] / Sum;
    End;
    For I := 0 To OutputSize - 1 Do
    Begin
      F := Output^[I];
      If (F > Threshold) And ((HeapCount < TopK) Or (StdLog(F) > HeapMinimum)) Then
        HeapPush(StdLog(F), I);
    End;
    FreeMem(Output);
  End;
End;

{ ------------------------------------------------------------------ }
{ Affichage                                                           }
{ ------------------------------------------------------------------ }

Function Pow10(E: Integer): Double;
Var
  R: Double;
  I: Integer;
Begin
  R := 1;
  If E >= 0 Then
    For I := 1 To E Do
      R := R * 10
  Else
    For I := 1 To -E Do
      R := R / 10;
  Pow10 := R;
End;

{ Format %g de C++ (6 chiffres significatifs). }
Function FormatG(Value: Double): String;
Var
  E, I: Integer;
  Scaled: Int64;
  Digits, Text1: String;
  Negative: Boolean;
  P: Double;
Begin
  If Value = 0 Then
  Begin
    FormatG := '0';
    Exit;
  End;
  Negative := Value < 0;
  If Negative Then Value := -Value;
  E := Trunc(Ln(Value) / Ln(10));
  If Pow10(E) > Value Then Dec(E);
  Scaled := Round(Value / Pow10(E - 5));
  If Scaled >= 1000000 Then
  Begin
    Inc(E);
    Scaled := Round(Value / Pow10(E - 5));
  End;
  Str(Scaled, Digits);
  If (E < -4) Or (E >= 6) Then
  Begin
    Text1 := Copy(Digits, 1, 1) + '.' + Copy(Digits, 2, 5);
    While (Length(Text1) > 0) And (Text1[Length(Text1)] = '0') Do
      Delete(Text1, Length(Text1), 1);
    If Text1[Length(Text1)] = '.' Then Delete(Text1, Length(Text1), 1);
    Str(Abs(E), Digits);
    If Length(Digits) < 2 Then Digits := '0' + Digits;
    If E < 0 Then Text1 := Text1 + 'e-' + Digits Else Text1 := Text1 + 'e+' + Digits;
  End
  Else
  Begin
    { Valeur en notation fixe : 6 chiffres, point decimal place selon E. }
    If E >= 0 Then
    Begin
      Text1 := Copy(Digits, 1, E + 1);
      If E < 5 Then Text1 := Text1 + '.' + Copy(Digits, E + 2, 5 - E);
    End
    Else
    Begin
      Text1 := '0.';
      For I := 1 To -E - 1 Do
        Text1 := Text1 + '0';
      Text1 := Text1 + Digits;
    End;
    If Pos('.', Text1) > 0 Then
    Begin
      While Text1[Length(Text1)] = '0' Do
        Delete(Text1, Length(Text1), 1);
      If Text1[Length(Text1)] = '.' Then Delete(Text1, Length(Text1), 1);
    End;
  End;
  If Negative Then Text1 := '-' + Text1;
  FormatG := Text1;
End;

Procedure PrintPredictions;
Var
  I, J, Best: Longint;
  Done: Array[0..1023] Of Boolean;
  Line: String;
  Prob: Single;
Begin
  FillChar(Done, SizeOf(Done), 0);
  Line := '';
  For I := 1 To HeapCount Do
  Begin
    Best := -1;
    For J := 0 To HeapCount - 1 Do
      If Not Done[J] Then
        If (Best < 0) Or (HeapScore[J] > HeapScore[Best]) Then Best := J;
    Done[Best] := True;
    If Line <> '' Then Line := Line + ' ';
    Line := Line + Words^[NWords + HeapNode[Best]];
    If PrintProb Then
    Begin
      Prob := Exp(HeapScore[Best]);
      Line := Line + ' ' + FormatG(Prob);
    End;
  End;
  Writeln(Line);
End;

{ ------------------------------------------------------------------ }
{ Lecture des lignes de texte                                         }
{ ------------------------------------------------------------------ }

Function NextByte: Longint;
Var
  Ch: Char;
Begin
  If Pushed >= 0 Then
  Begin
    NextByte := Pushed;
    Pushed := -1;
    Exit;
  End;
  If UseStdin Then
  Begin
    If Eof(Input) Then
      NextByte := -1
    Else
    Begin
      Read(Input, Ch);
      NextByte := Ord(Ch);
    End;
    Exit;
  End;
  If InPos >= InLen Then
  Begin
    BlockRead(InputFile, InBuffer, SizeOf(InBuffer), InLen);
    InPos := 0;
    If InLen <= 0 Then
    Begin
      InLen := 0;
      NextByte := -1;
      Exit;
    End;
  End;
  NextByte := InBuffer[InPos];
  Inc(InPos);
End;

Procedure OpenData(Const Name: String);
Begin
  Pushed := -1;
  InPos := 0;
  InLen := 0;
  UseStdin := Name = '-';
  If UseStdin Then Exit;
  Assign(InputFile, Name);
  {$I-}
  Reset(InputFile, 1);
  {$I+}
  If IOResult <> 0 Then Fail('Erreur : impossible d''ouvrir le fichier de donnees ' + Name);
End;

Procedure PredictFile;
Var
  C, First: Longint;
  Token: TWordText;
  AtEnd: Boolean;
Begin
  First := NextByte;
  While First >= 0 Do
  Begin
    Pushed := First;
    IdCount := 0;
    Token := '';
    AtEnd := False;
    While Not AtEnd Do
    Begin
      C := NextByte;
      If C < 0 Then
      Begin
        If Token <> '' Then AddToken(Token);
        AtEnd := True;
      End
      Else If C = 10 Then
      Begin
        If Token <> '' Then AddToken(Token);
        Token := '';
        AddToken(EosToken);
        AtEnd := True;
      End
      Else If (C = 32) Or (C = 13) Or (C = 9) Or (C = 11) Or (C = 12) Or (C = 0) Then
      Begin
        If Token <> '' Then AddToken(Token);
        Token := '';
      End
      Else If Length(Token) < 255 Then
        Token := Token + Chr(C);
    End;
    HeapCount := 0;
    If IdCount > 0 Then
    Begin
      ComputeHidden;
      FindBest;
    End;
    PrintPredictions;
    First := NextByte;
  End;
End;

{ ------------------------------------------------------------------ }
{ Ligne de commande                                                   }
{ ------------------------------------------------------------------ }

Procedure ShowUsage;
Begin
  WriteLn('FASTTEXT - Cette commande permet de lancer l''outil de classification de texte.');
  WriteLn;
  Writeln('Syntaxe: fasttext <commande> <arguments>');
  Writeln;
  Writeln('Les commandes prises en charge sont :');
  Writeln;
  Writeln('  predict       affiche les etiquettes les plus probables de chaque ligne');
  Writeln('  predict-prob  affiche les etiquettes les plus probables avec leur probabilite');
  Writeln;
  Writeln('Syntaxe :');
  Writeln('  fasttext predict <modele> <donnees> [<k>] [<seuil>]');
  Writeln('  fasttext predict-prob <modele> <donnees> [<k>] [<seuil>]');
  Writeln;
  Writeln('  modele   fichier .bin ou .ftz de fastText (par ex. lid.176.ftz)');
  Writeln('  donnees  fichier texte, une ligne par exemple ; - pour l''entree standard');
  Writeln('  k        nombre d''etiquettes affichees par ligne (1 par defaut)');
  Writeln('  seuil    probabilite minimale (0 par defaut)');
  Writeln;
  Writeln('Exemple :');
  Writeln('  fasttext predict-prob lid.176.ftz texte.txt 3');
End;

Var
  Command: String;
  Code: Integer;
  Value: Double;

BEGIN
  If (ParamCount = 0) Or (ParamStr(1) = '/?') Or (ParamStr(1) = '-h') Or
     (ParamStr(1) = '--help') Or (ParamStr(1) = 'help') Then
  Begin
    ShowUsage;
    Halt(0);
  End;
  Command := ParamStr(1);
  If (Command <> 'predict') And (Command <> 'predict-prob') Then
  Begin
    Writeln('Erreur : commande non prise en charge : ', Command);
    ShowUsage;
    Halt(1);
  End;
  If ParamCount < 3 Then
  Begin
    Writeln('Erreur : arguments manquants.');
    ShowUsage;
    Halt(1);
  End;
  PrintProb := Command = 'predict-prob';
  TopK := 1;
  Threshold := 0;
  If ParamCount >= 4 Then
  Begin
    Val(ParamStr(4), TopK, Code);
    If (Code <> 0) Or (TopK < 1) Or (TopK > 1000) Then Fail('Erreur : k doit etre entre 1 et 1000.');
  End;
  If ParamCount >= 5 Then
  Begin
    Val(ParamStr(5), Value, Code);
    If Code <> 0 Then Fail('Erreur : seuil invalide.');
    Threshold := Value;
  End;

  LoadModel(ParamStr(2));
  If TopK > OutputSize Then TopK := OutputSize;
  IdCapacity := 1024;
  GetMem(Ids, IdCapacity * 4);
  GetMem(Hidden, Dim * 4);
  OpenData(ParamStr(3));
  PredictFile;
END.
