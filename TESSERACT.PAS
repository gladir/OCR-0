{ @author: Sylvain Maltais (support@gladir.com)
  @created: 2026
  @website(https://www.gladir.com/OCR-0)
  @abstract(Target: Turbo Pascal 7, Free Pascal 3.2)
  @description: Clone de la commande TESSERACT
}

Program TESSERACT;

Const
  MaxImageWidth = 32000;
  MaxImageHeight = 16000;
  FontCharacters: String[36] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  Font: Array[1..36] Of String[35] = (
    '01110100011000111111100011000110001',
    '11110100011000111110100011000111110',
    '01111100001000010000100001000001111',
    '11110100011000110001100011000111110',
    '11111100001000011110100001000011111',
    '11111100001000011110100001000010000',
    '01111100001000010111100011000101111',
    '10001100011000111111100011000110001',
    '11111001000010000100001000010011111',
    '00111000100001000010000101001001100',
    '10001100101010011000101001001010001',
    '10000100001000010000100001000011111',
    '10001110111010110101100011000110001',
    '10001110011010110011100011000110001',
    '01110100011000110001100011000101110',
    '11110100011000111110100001000010000',
    '01110100011000110001101011001001101',
    '11110100011000111110101001001010001',
    '01111100001000001110000010000111110',
    '11111001000010000100001000010000100',
    '10001100011000110001100011000101110',
    '10001100011000110001100010101000100',
    '10001100011000110101101011010101010',
    '10001100010101000100010101000110001',
    '10001100010101000100001000010000100',
    '11111000010001000100010001000011111',
    '01110100011001110101110011000101110',
    '00100011000010000100001000010001110',
    '01110100010000100010001000100011111',
    '11110000010000101110000010000111110',
    '00010001100101010010111110001000010',
    '11111100001000011110000010000111110',
    '01110100001000011110100011000101110',
    '11111000010001000100010001000010000',
    '01110100011000101110100011000101110',
    '01110100011000101111000010000101110'
  );

Type
  { L'image est stockee sur le tas, un bit par pixel, une rangee a la fois. }
  TImageRow = Array[0..(MaxImageWidth Div 8)] Of Byte;
  PImageRow = ^TImageRow;
  TImageRows = Array[1..MaxImageHeight] Of PImageRow;
  PImageRows = ^TImageRows;

Var
  OutputFile: Text;
  Image: PImageRows;
  ImageName, OutputName: String;
  Token: String;
  ImageWidth, ImageHeight: Integer;
  X, Y, TopY, BottomY, LeftX, RightX: Integer;
  CurrentX, PreviousRight: Integer;
  RowIndex, ColumnIndex, FontIndex: Integer;
  SourceX, SourceY, Errors, BestErrors, BestIndex: Integer;
  ArgumentIndex: Integer;
  RecognizedLine: String;

Procedure ShowHelp;
Begin
  Writeln('TESSERACT - Cette commande permet de lancer la reconnaissance de caracteres d''une image.');
  Writeln;
  Writeln('Description :');
  Writeln('  Reconnait le texte (caracteres A-Z et 0-9) d''une image PBM, PNG, JPEG');
  Writeln('  ou GIF et l''ecrit dans un fichier texte, une ligne de texte par ligne');
  Writeln('  d''image. Les decodeurs sont integres; le format est detecte par le');
  Writeln('  contenu du fichier et non par son extension.');
  Writeln;
  Writeln('Syntaxe :');
  Writeln('  TESSERACT image base_de_sortie [-l fra]');
  Writeln('  TESSERACT /?');
  Writeln;
  Writeln('Parametres :');
  Writeln('  image            Fichier image d''entree (maximum 32000 x 16000 pixels) :');
  Writeln('                   PBM ASCII P1, PNG (toutes profondeurs, entrelace ou non),');
  Writeln('                   JPEG de base (non progressif) et GIF (premiere image).');
  Writeln('  base_de_sortie   Nom de base du fichier de sortie; l''extension .txt');
  Writeln('                   est ajoutee automatiquement.');
  Writeln('  -l fra           Langue de reconnaissance (seule la valeur fra est acceptee).');
  Writeln('  /?               Affiche cette aide (aussi -?, -h et --help).');
  Writeln;
  Writeln('Exemple :');
  Writeln('  TESSERACT image.png sortie -l fra');
End;

Function GetInk(PixelX, PixelY: Integer): Boolean;
Begin
  GetInk := (Image^[PixelY]^[(PixelX - 1) Shr 3] And
             ($80 Shr ((PixelX - 1) And 7))) <> 0;
End;

Procedure SetInk(PixelX, PixelY: Integer; Ink: Boolean);
Var
  Mask: Byte;
Begin
  Mask := $80 Shr ((PixelX - 1) And 7);
  If Ink Then
    Image^[PixelY]^[(PixelX - 1) Shr 3] := Image^[PixelY]^[(PixelX - 1) Shr 3] Or Mask
  Else
    Image^[PixelY]^[(PixelX - 1) Shr 3] := Image^[PixelY]^[(PixelX - 1) Shr 3] And (Not Mask);
End;

Function RowHasInk(RowNumber: Integer): Boolean;
Var
  Column: Integer;
Begin
  RowHasInk := False;
  For Column := 1 To ImageWidth Do
    If GetInk(Column, RowNumber) Then
    Begin
      RowHasInk := True;
      Exit;
    End;
End;

Function ColumnHasInk(ColumnNumber, FirstRow, LastRow: Integer): Boolean;
Var
  Row: Integer;
Begin
  ColumnHasInk := False;
  For Row := FirstRow To LastRow Do
    If GetInk(ColumnNumber, Row) Then
    Begin
      ColumnHasInk := True;
      Exit;
    End;
End;

Function RecognizeGlyph(FirstColumn, LastColumn, FirstRow, LastRow: Integer): Char;
Begin
  BestErrors := 36;
  BestIndex := 0;
  For FontIndex := 1 To 36 Do
  Begin
    Errors := 0;
    For RowIndex := 1 To 7 Do
    Begin
      SourceY := FirstRow + (Longint(2 * RowIndex - 1) * (LastRow - FirstRow + 1)) Div 14;
      For ColumnIndex := 1 To 5 Do
      Begin
        SourceX := FirstColumn + (Longint(2 * ColumnIndex - 1) * (LastColumn - FirstColumn + 1)) Div 10;
        If GetInk(SourceX, SourceY) <>
           (Font[FontIndex][(RowIndex - 1) * 5 + ColumnIndex] = '1') Then
          Inc(Errors);
      End;
    End;
    If Errors < BestErrors Then
    Begin
      BestErrors := Errors;
      BestIndex := FontIndex;
    End;
  End;

  If (BestIndex = 0) Or (BestErrors > 17) Then
    RecognizeGlyph := '?'
  Else
    RecognizeGlyph := FontCharacters[BestIndex];
End;

Type
  TRowBuffer = Array[0..65519] Of Byte;
  PRowBuffer = ^TRowBuffer;
  TByteBuffer = Array[0..4097] Of Byte;
  PByteBuffer = ^TByteBuffer;
  TWordBuffer = Array[0..4097] Of Word;
  PWordBuffer = ^TWordBuffer;
  TWindow = Array[0..32767] Of Byte;
  PWindow = ^TWindow;
  THuffman = Record
    Count: Array[0..15] Of Word;
    Symbol: Array[0..287] Of Word;
  End;
  TJpegHuffman = Record
    MinCode, MaxCode: Array[1..16] Of Longint;
    ValPtr: Array[1..16] Of Integer;
    Values: Array[0..255] Of Byte;
    Defined: Boolean;
  End;
  TJpegComponent = Record
    Id, H, V, Tq, Td, Ta: Integer;
    Pred: Longint;
  End;

Const
  FormatPbm = 0;
  FormatPng = 1;
  FormatJpeg = 2;
  FormatGif = 3;

  { Tables DEFLATE }
  LengthBase: Array[0..28] Of Word = (
    3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
    35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258);
  LengthExtra: Array[0..28] Of Byte = (
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
    3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0);
  DistanceBase: Array[0..29] Of Word = (
    1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
    257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
    8193, 12289, 16385, 24577);
  DistanceExtra: Array[0..29] Of Byte = (
    0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
    7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13);
  CodeLengthOrder: Array[0..18] Of Byte = (
    16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);

  { Passes Adam7 de PNG }
  AdamStartX: Array[0..6] Of Byte = (0, 4, 0, 2, 0, 1, 0);
  AdamStartY: Array[0..6] Of Byte = (0, 0, 4, 0, 2, 0, 1);
  AdamStepX: Array[0..6] Of Byte = (8, 8, 4, 4, 2, 2, 1);
  AdamStepY: Array[0..6] Of Byte = (8, 8, 8, 4, 4, 2, 2);

  { Ordre zigzag de JPEG }
  ZigZag: Array[0..63] Of Byte = (
    0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18, 11, 4, 5,
    12, 19, 26, 33, 40, 48, 41, 34, 27, 20, 13, 6, 7, 14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63);

Var
  BinFile: File;
  InBuf: Array[0..4095] Of Byte;
  InPos, InLen: Word;
  Palette: Array[0..767] Of Byte;
  PaletteAlpha: Array[0..255] Of Byte;

  { PNG / DEFLATE }
  InflateWindow: PWindow;
  WindowPos: Word;
  BitBuffer: Longint;
  BitCount: Integer;
  IdatLeft: Longint;
  LiteralTable, DistanceTable: THuffman;
  CodeLengths: Array[0..319] Of Byte;
  PngWidth, PngHeight: Longint;
  PngDepth, PngColor, PngInterlace, PngChannels, PngBpp: Integer;
  PngPass, PngRow, PngPassWidth, PngPassHeight: Integer;
  PngRowBytes, PngRowFill: Word;
  PngStartX, PngStartY, PngStepX, PngStepY: Integer;
  PngDone: Boolean;
  PngCurrent, PngPrevious: PRowBuffer;

  { JPEG }
  JpegHuffman: Array[0..3] Of TJpegHuffman;
  JpegQuant: Array[0..3, 0..63] Of Word;
  JpegComponent: Array[1..3] Of TJpegComponent;
  JpegComponentCount, JpegHMax, JpegVMax, JpegRestart: Integer;
  JpegBitBuffer, JpegBitCount, JpegMarker: Integer;
  JpegBlock: Array[0..63] Of Longint;
  JpegCosine: Array[0..7, 0..7] Of Real;
  JpegFrameSeen, JpegLumaDone: Boolean;

  { GIF }
  GifPrefix: PWordBuffer;
  GifSuffix, GifStack: PByteBuffer;
  GifBlockLeft, GifTransparent: Integer;
  GifEnded, GifInterlaced: Boolean;
  GifFrameX, GifFrameY, GifFrameWidth, GifFrameHeight: Integer;
  GifColumn, GifRowNumber: Integer;

Procedure DecodeError(Reason: String);
Begin
  Writeln('Erreur : ', Reason);
  Halt(2);
End;

Function ReadBin: Integer;
Begin
  If InPos >= InLen Then
  Begin
    {$I-}
    BlockRead(BinFile, InBuf, SizeOf(InBuf), InLen);
    {$I+}
    If IOResult <> 0 Then InLen := 0;
    InPos := 0;
    If InLen = 0 Then
    Begin
      ReadBin := -1;
      Exit;
    End;
  End;
  ReadBin := InBuf[InPos];
  Inc(InPos);
End;

Function NeedByte: Byte;
Var
  Value: Integer;
Begin
  Value := ReadBin;
  If Value < 0 Then DecodeError('fin de fichier image inattendue.');
  NeedByte := Value;
End;

Procedure SkipBytes(Count: Longint);
Var
  Dummy: Byte;
Begin
  While Count > 0 Do
  Begin
    Dummy := NeedByte;
    Dec(Count);
  End;
End;

Function ReadBE16: Integer;
Var
  High: Integer;
Begin
  High := NeedByte;
  ReadBE16 := High * 256 + NeedByte;
End;

Function ReadBE32: Longint;
Var
  Value: Longint;
  I: Integer;
Begin
  Value := 0;
  For I := 1 To 4 Do
    Value := (Value Shl 8) Or NeedByte;
  ReadBE32 := Value;
End;

Function ReadLE16: Integer;
Var
  Low: Integer;
Begin
  Low := NeedByte;
  ReadLE16 := Low + NeedByte * 256;
End;

Function GrayOf(Red, Green, Blue: Integer): Integer;
Begin
  GrayOf := (77 * Longint(Red) + 150 * Longint(Green) + 29 * Longint(Blue)) Div 256;
End;

{ Memoire de tas disponible (Turbo Pascal) ; Free Pascal alloue a la demande. }
Function AvailableMemory: Longint;
Begin
{$IFDEF FPC}
  AvailableMemory := MaxLongint;
{$ELSE}
  AvailableMemory := MaxAvail;
{$ENDIF}
End;

Procedure SetImageSize(NewWidth, NewHeight: Longint);
Var
  Row: Integer;
Begin
  If (NewWidth < 1) Or (NewWidth > MaxImageWidth) Then
    DecodeError('largeur invalide (maximum 32000 pixels).');
  If (NewHeight < 1) Or (NewHeight > MaxImageHeight) Then
    DecodeError('hauteur invalide (maximum 16000 pixels).');
  If Image <> Nil Then DecodeError('en-tete d''image en double.');
  ImageWidth := NewWidth;
  ImageHeight := NewHeight;
  If AvailableMemory < NewHeight * SizeOf(PImageRow) Then
    DecodeError('memoire insuffisante pour l''image.');
  GetMem(Image, NewHeight * SizeOf(PImageRow));
  For Row := 1 To ImageHeight Do
  Begin
    If AvailableMemory < (NewWidth + 7) Div 8 Then
      DecodeError('memoire insuffisante pour l''image.');
    GetMem(Image^[Row], (NewWidth + 7) Div 8);
    FillChar(Image^[Row]^, (NewWidth + 7) Div 8, 0);
  End;
End;

{ Les pixels sombres (niveau de gris sous 128) deviennent de l'encre. }
Procedure PutPixel(PixelX, PixelY, Gray, Alpha: Integer);
Begin
  If (PixelX < 1) Or (PixelX > ImageWidth) Or
     (PixelY < 1) Or (PixelY > ImageHeight) Then Exit;
  If Alpha < 255 Then
    Gray := (Longint(Gray) * Alpha + 255 * Longint(255 - Alpha)) Div 255;
  SetInk(PixelX, PixelY, Gray < 128);
End;

Function DetectFormat: Integer;
Var
  Signature: Array[0..7] Of Integer;
  I: Integer;
Begin
  Assign(BinFile, ImageName);
  {$I-}
  Reset(BinFile, 1);
  {$I+}
  If IOResult <> 0 Then DecodeError('impossible de lire le fichier image.');
  InPos := 0;
  InLen := 0;
  For I := 0 To 7 Do
    Signature[I] := ReadBin;

  DetectFormat := FormatPbm;
  If (Signature[0] = $89) And (Signature[1] = $50) And
     (Signature[2] = $4E) And (Signature[3] = $47) Then
    DetectFormat := FormatPng
  Else If (Signature[0] = $FF) And (Signature[1] = $D8) Then
    DetectFormat := FormatJpeg
  Else If (Signature[0] = Ord('G')) And (Signature[1] = Ord('I')) And
          (Signature[2] = Ord('F')) Then
    DetectFormat := FormatGif;

  Seek(BinFile, 0);
  InPos := 0;
  InLen := 0;
End;

{ ------------------------------------------------------------------ }
{ PNG : decompression DEFLATE (zlib) puis defiltrage des lignes       }
{ ------------------------------------------------------------------ }

Procedure PngBeginPass;
Begin
  While True Do
  Begin
    If PngInterlace = 0 Then
    Begin
      If PngPass > 0 Then
      Begin
        PngDone := True;
        Exit;
      End;
      PngStartX := 0;
      PngStartY := 0;
      PngStepX := 1;
      PngStepY := 1;
    End
    Else
    Begin
      If PngPass > 6 Then
      Begin
        PngDone := True;
        Exit;
      End;
      PngStartX := AdamStartX[PngPass];
      PngStartY := AdamStartY[PngPass];
      PngStepX := AdamStepX[PngPass];
      PngStepY := AdamStepY[PngPass];
    End;
    PngPassWidth := (PngWidth - PngStartX + PngStepX - 1) Div PngStepX;
    PngPassHeight := (PngHeight - PngStartY + PngStepY - 1) Div PngStepY;
    If (PngPassWidth > 0) And (PngPassHeight > 0) Then
    Begin
      PngRowBytes := (Longint(PngPassWidth) * PngDepth * PngChannels + 7) Div 8;
      FillChar(PngPrevious^, PngRowBytes + 1, 0);
      PngRow := 0;
      PngRowFill := 0;
      Exit;
    End;
    Inc(PngPass);
  End;
End;

Function PngSample(PixelIndex, Channel: Integer): Integer;
Var
  Position: Longint;
  Shift: Integer;
Begin
  If PngDepth = 8 Then
    PngSample := PngCurrent^[Word(1 + Longint(PixelIndex) * PngChannels + Channel)]
  Else If PngDepth = 16 Then
    PngSample := PngCurrent^[Word(1 + (Longint(PixelIndex) * PngChannels + Channel) * 2)]
  Else
  Begin
    Position := Longint(PixelIndex) * PngDepth;
    Shift := 8 - PngDepth - Integer(Position And 7);
    PngSample := (PngCurrent^[Word(1 + Position Shr 3)] Shr Shift) And
                 ((1 Shl PngDepth) - 1);
  End;
End;

Procedure PngProcessRow;
Var
  I: Word;
  A, B, C, P, PA, PB, PC, V: Integer;
  Px, Gray, Alpha, Index: Integer;
Begin
  If PngCurrent^[0] > 4 Then DecodeError('filtre PNG invalide.');
  For I := 1 To PngRowBytes Do
  Begin
    If I > PngBpp Then
    Begin
      A := PngCurrent^[I - PngBpp];
      C := PngPrevious^[I - PngBpp];
    End
    Else
    Begin
      A := 0;
      C := 0;
    End;
    B := PngPrevious^[I];
    Case PngCurrent^[0] Of
      1: V := A;
      2: V := B;
      3: V := (A + B) Div 2;
      4:
      Begin
        P := A + B - C;
        PA := Abs(P - A);
        PB := Abs(P - B);
        PC := Abs(P - C);
        If (PA <= PB) And (PA <= PC) Then
          V := A
        Else If PB <= PC Then
          V := B
        Else
          V := C;
      End;
    Else
      V := 0;
    End;
    PngCurrent^[I] := (PngCurrent^[I] + V) And $FF;
  End;

  For Px := 0 To PngPassWidth - 1 Do
  Begin
    Alpha := 255;
    Case PngColor Of
      0:
      Begin
        Gray := PngSample(Px, 0);
        If PngDepth < 8 Then
          Gray := Gray * 255 Div ((1 Shl PngDepth) - 1);
      End;
      2: Gray := GrayOf(PngSample(Px, 0), PngSample(Px, 1), PngSample(Px, 2));
      3:
      Begin
        Index := PngSample(Px, 0);
        Gray := GrayOf(Palette[Index * 3], Palette[Index * 3 + 1],
                       Palette[Index * 3 + 2]);
        Alpha := PaletteAlpha[Index];
      End;
      4:
      Begin
        Gray := PngSample(Px, 0);
        Alpha := PngSample(Px, 1);
      End;
    Else
      Gray := GrayOf(PngSample(Px, 0), PngSample(Px, 1), PngSample(Px, 2));
      Alpha := PngSample(Px, 3);
    End;
    PutPixel(PngStartX + Px * PngStepX + 1, PngStartY + PngRow * PngStepY + 1,
             Gray, Alpha);
  End;

  Move(PngCurrent^, PngPrevious^, PngRowBytes + 1);
  Inc(PngRow);
  PngRowFill := 0;
  If PngRow >= PngPassHeight Then
  Begin
    Inc(PngPass);
    PngBeginPass;
  End;
End;

Procedure PngOutByte(Value: Byte);
Begin
  If PngDone Then Exit;
  PngCurrent^[PngRowFill] := Value;
  Inc(PngRowFill);
  If PngRowFill > PngRowBytes Then PngProcessRow;
End;

Procedure EmitByte(Value: Byte);
Begin
  InflateWindow^[WindowPos] := Value;
  WindowPos := (WindowPos + 1) And $7FFF;
  PngOutByte(Value);
End;

{ Octet suivant du flux compresse, repartis dans les blocs IDAT. }
Function InflateByte: Integer;
Begin
  While IdatLeft = 0 Do
  Begin
    SkipBytes(4);
    IdatLeft := ReadBE32;
    If ReadBE32 <> $49444154 Then
      DecodeError('donnees de pixels PNG incompletes.');
  End;
  Dec(IdatLeft);
  InflateByte := NeedByte;
End;

Function GetBits(Count: Integer): Longint;
Begin
  While BitCount < Count Do
  Begin
    BitBuffer := BitBuffer Or (Longint(InflateByte) Shl BitCount);
    Inc(BitCount, 8);
  End;
  GetBits := BitBuffer And ((Longint(1) Shl Count) - 1);
  BitBuffer := BitBuffer Shr Count;
  Dec(BitCount, Count);
End;

Function DecodeSymbol(Var Table: THuffman): Integer;
Var
  Len: Integer;
  Code, First, Index, Count: Longint;
Begin
  DecodeSymbol := 0;
  Code := 0;
  First := 0;
  Index := 0;
  For Len := 1 To 15 Do
  Begin
    Code := Code Or GetBits(1);
    Count := Table.Count[Len];
    If Code - Count < First Then
    Begin
      Index := Index + (Code - First);
      If (Index < 0) Or (Index > 287) Then
        DecodeError('code de Huffman PNG invalide.');
      DecodeSymbol := Table.Symbol[Index];
      Exit;
    End;
    Index := Index + Count;
    First := (First + Count) Shl 1;
    Code := Code Shl 1;
  End;
  DecodeError('code de Huffman PNG invalide.');
End;

Procedure BuildHuffman(Var Table: THuffman; Start, Number: Integer);
Var
  Offsets: Array[0..15] Of Integer;
  I, Len: Integer;
Begin
  FillChar(Table.Count, SizeOf(Table.Count), 0);
  For I := 0 To Number - 1 Do
    Inc(Table.Count[CodeLengths[Start + I]]);
  Offsets[0] := 0;
  Offsets[1] := 0;
  For I := 1 To 14 Do
    Offsets[I + 1] := Offsets[I] + Table.Count[I];
  For I := 0 To Number - 1 Do
  Begin
    Len := CodeLengths[Start + I];
    If Len <> 0 Then
    Begin
      If Offsets[Len] > 287 Then DecodeError('table de Huffman PNG invalide.');
      Table.Symbol[Offsets[Len]] := I;
      Inc(Offsets[Len]);
    End;
  End;
End;

Procedure InflateCodes;
Var
  Symbol, Len, I: Integer;
  Distance: Longint;
Begin
  While True Do
  Begin
    Symbol := DecodeSymbol(LiteralTable);
    If Symbol < 256 Then
      EmitByte(Symbol)
    Else If Symbol = 256 Then
      Exit
    Else
    Begin
      Dec(Symbol, 257);
      If Symbol >= 29 Then DecodeError('symbole DEFLATE invalide.');
      Len := LengthBase[Symbol] + Integer(GetBits(LengthExtra[Symbol]));
      Symbol := DecodeSymbol(DistanceTable);
      If Symbol >= 30 Then DecodeError('distance DEFLATE invalide.');
      Distance := DistanceBase[Symbol] + GetBits(DistanceExtra[Symbol]);
      For I := 1 To Len Do
        EmitByte(InflateWindow^[Word((Longint(WindowPos) - Distance) And $7FFF)]);
    End;
  End;
End;

Procedure BuildFixedTables;
Var
  I: Integer;
Begin
  For I := 0 To 143 Do CodeLengths[I] := 8;
  For I := 144 To 255 Do CodeLengths[I] := 9;
  For I := 256 To 279 Do CodeLengths[I] := 7;
  For I := 280 To 287 Do CodeLengths[I] := 8;
  BuildHuffman(LiteralTable, 0, 288);
  For I := 0 To 29 Do CodeLengths[I] := 5;
  BuildHuffman(DistanceTable, 0, 30);
End;

Procedure BuildDynamicTables;
Var
  LiteralCount, DistanceCount, CodeCount, I, Index, Symbol, Repeats: Integer;
  Previous: Byte;
Begin
  LiteralCount := GetBits(5) + 257;
  DistanceCount := GetBits(5) + 1;
  CodeCount := GetBits(4) + 4;
  If (LiteralCount > 286) Or (DistanceCount > 30) Then
    DecodeError('bloc DEFLATE invalide.');
  FillChar(CodeLengths, SizeOf(CodeLengths), 0);
  For I := 0 To CodeCount - 1 Do
    CodeLengths[CodeLengthOrder[I]] := GetBits(3);
  BuildHuffman(LiteralTable, 0, 19);

  Index := 0;
  While Index < LiteralCount + DistanceCount Do
  Begin
    Symbol := DecodeSymbol(LiteralTable);
    If Symbol < 16 Then
    Begin
      CodeLengths[Index] := Symbol;
      Inc(Index);
    End
    Else
    Begin
      Previous := 0;
      If Symbol = 16 Then
      Begin
        If Index = 0 Then DecodeError('bloc DEFLATE invalide.');
        Previous := CodeLengths[Index - 1];
        Repeats := 3 + GetBits(2);
      End
      Else If Symbol = 17 Then
        Repeats := 3 + GetBits(3)
      Else
        Repeats := 11 + GetBits(7);
      If Index + Repeats > LiteralCount + DistanceCount Then
        DecodeError('bloc DEFLATE invalide.');
      While Repeats > 0 Do
      Begin
        CodeLengths[Index] := Previous;
        Inc(Index);
        Dec(Repeats);
      End;
    End;
  End;
  BuildHuffman(LiteralTable, 0, LiteralCount);
  BuildHuffman(DistanceTable, LiteralCount, DistanceCount);
End;

Procedure InflateStream;
Var
  Last, BlockType, Header: Integer;
  Len, Complement: Longint;
Begin
  BitBuffer := 0;
  BitCount := 0;
  Header := InflateByte;
  If (Header And 15) <> 8 Then DecodeError('compression PNG non prise en charge.');
  If (InflateByte And $20) <> 0 Then
    DecodeError('dictionnaire zlib PNG non pris en charge.');
  Repeat
    Last := GetBits(1);
    BlockType := GetBits(2);
    If BlockType = 0 Then
    Begin
      BitBuffer := 0;
      BitCount := 0;
      Len := InflateByte;
      Len := Len + InflateByte * 256;
      Complement := InflateByte;
      Complement := Complement + InflateByte * 256;
      If Len <> (Complement Xor $FFFF) Then
        DecodeError('bloc DEFLATE invalide.');
      While Len > 0 Do
      Begin
        EmitByte(InflateByte);
        Dec(Len);
      End;
    End
    Else If BlockType = 1 Then
    Begin
      BuildFixedTables;
      InflateCodes;
    End
    Else If BlockType = 2 Then
    Begin
      BuildDynamicTables;
      InflateCodes;
    End
    Else
      DecodeError('bloc DEFLATE invalide.');
  Until (Last = 1) Or PngDone;
End;

Procedure LoadPng;
Var
  I, Value: Integer;
  ChunkLength, ChunkType: Longint;
  GotHeader: Boolean;
Begin
  SkipBytes(8);
  GotHeader := False;
  FillChar(Palette, SizeOf(Palette), 0);
  FillChar(PaletteAlpha, SizeOf(PaletteAlpha), 255);
  While True Do
  Begin
    ChunkLength := ReadBE32;
    ChunkType := ReadBE32;
    If ChunkLength < 0 Then DecodeError('bloc PNG invalide.');
    If ChunkType = $49484452 Then { IHDR }
    Begin
      If ChunkLength < 13 Then DecodeError('en-tete PNG invalide.');
      PngWidth := ReadBE32;
      PngHeight := ReadBE32;
      SetImageSize(PngWidth, PngHeight);
      PngDepth := NeedByte;
      PngColor := NeedByte;
      If (NeedByte <> 0) Or (NeedByte <> 0) Then
        DecodeError('methode de compression PNG non prise en charge.');
      PngInterlace := NeedByte;
      If PngInterlace > 1 Then DecodeError('entrelacement PNG invalide.');
      Case PngColor Of
        0: PngChannels := 1;
        2: PngChannels := 3;
        3: PngChannels := 1;
        4: PngChannels := 2;
        6: PngChannels := 4;
      Else
        DecodeError('type de couleur PNG invalide.');
      End;
      If Not (PngDepth In [1, 2, 4, 8, 16]) Then
        DecodeError('profondeur PNG invalide.');
      If (PngColor In [2, 4, 6]) And (PngDepth < 8) Then
        DecodeError('profondeur PNG invalide.');
      If (PngColor = 3) And (PngDepth = 16) Then
        DecodeError('profondeur PNG invalide.');
      PngBpp := (PngDepth * PngChannels + 7) Div 8;
      If (Longint(PngWidth) * PngDepth * PngChannels + 7) Div 8 + 1 > 65520 Then
        DecodeError('image PNG trop large pour la memoire disponible.');
      If AvailableMemory < 2 * Longint(65520) Then
        DecodeError('memoire insuffisante pour l''image.');
      GetMem(PngCurrent, (Longint(PngWidth) * PngDepth * PngChannels + 7) Div 8 + 1);
      GetMem(PngPrevious, (Longint(PngWidth) * PngDepth * PngChannels + 7) Div 8 + 1);
      GotHeader := True;
      SkipBytes(ChunkLength - 13 + 4);
    End
    Else If ChunkType = $504C5445 Then { PLTE }
    Begin
      For I := 0 To ChunkLength - 1 Do
      Begin
        Value := NeedByte;
        If I < 768 Then Palette[I] := Value;
      End;
      SkipBytes(4);
    End
    Else If ChunkType = $74524E53 Then { tRNS }
    Begin
      If GotHeader And (PngColor = 3) Then
      Begin
        For I := 0 To ChunkLength - 1 Do
        Begin
          Value := NeedByte;
          If I < 256 Then PaletteAlpha[I] := Value;
        End;
        SkipBytes(4);
      End
      Else
        SkipBytes(ChunkLength + 4);
    End
    Else If ChunkType = $49444154 Then { IDAT }
    Begin
      If Not GotHeader Then DecodeError('en-tete PNG manquant.');
      IdatLeft := ChunkLength;
      GetMem(InflateWindow, SizeOf(TWindow));
      FillChar(InflateWindow^, SizeOf(TWindow), 0);
      WindowPos := 0;
      PngPass := 0;
      PngDone := False;
      PngBeginPass;
      InflateStream;
      If Not PngDone Then DecodeError('donnees de pixels PNG incompletes.');
      FreeMem(InflateWindow, SizeOf(TWindow));
      Exit;
    End
    Else If ChunkType = $49454E44 Then { IEND }
      DecodeError('donnees d image PNG absentes.')
    Else
      SkipBytes(ChunkLength + 4);
  End;
End;

{ ------------------------------------------------------------------ }
{ JPEG : decodage Huffman + DCT inverse de la luminance (Y) seulement }
{ ------------------------------------------------------------------ }

Function JpegNextMarker: Integer;
Var
  Value: Integer;
Begin
  If JpegMarker <> 0 Then
  Begin
    JpegNextMarker := JpegMarker;
    JpegMarker := 0;
    Exit;
  End;
  While True Do
  Begin
    Repeat
      Value := NeedByte;
    Until Value = $FF;
    Repeat
      Value := NeedByte;
    Until Value <> $FF;
    If Value <> 0 Then
    Begin
      JpegNextMarker := Value;
      Exit;
    End;
  End;
End;

Function JpegGetBit: Integer;
Var
  Value, Second: Integer;
Begin
  If JpegBitCount = 0 Then
  Begin
    If JpegMarker <> 0 Then
      Value := 0
    Else
    Begin
      Value := NeedByte;
      If Value = $FF Then
      Begin
        Second := NeedByte;
        While Second = $FF Do
          Second := NeedByte;
        If Second <> 0 Then
        Begin
          JpegMarker := Second;
          Value := 0;
        End;
      End;
    End;
    JpegBitBuffer := Value;
    JpegBitCount := 8;
  End;
  Dec(JpegBitCount);
  JpegGetBit := (JpegBitBuffer Shr JpegBitCount) And 1;
End;

Function JpegReceive(Count: Integer): Longint;
Var
  Value: Longint;
  I: Integer;
Begin
  Value := 0;
  For I := 1 To Count Do
    Value := (Value Shl 1) Or JpegGetBit;
  JpegReceive := Value;
End;

Function JpegExtend(Value: Longint; Count: Integer): Longint;
Begin
  If Count = 0 Then
    JpegExtend := 0
  Else If Value < (Longint(1) Shl (Count - 1)) Then
    JpegExtend := Value - (Longint(1) Shl Count) + 1
  Else
    JpegExtend := Value;
End;

Function JpegDecode(Var Table: TJpegHuffman): Integer;
Var
  Len: Integer;
  Code: Longint;
Begin
  Code := 0;
  For Len := 1 To 16 Do
  Begin
    Code := (Code Shl 1) Or JpegGetBit;
    If Code <= Table.MaxCode[Len] Then
    Begin
      JpegDecode := Table.Values[Integer((Table.ValPtr[Len] + Code - Table.MinCode[Len]) And 255)];
      Exit;
    End;
  End;
  DecodeError('code de Huffman JPEG invalide.');
  JpegDecode := 0;
End;

Procedure JpegReadTables(Marker: Integer);
Var
  Remaining, Info, Precision, Index, Total, K, Len: Integer;
  Code: Longint;
  Counts: Array[1..16] Of Byte;
  SegmentLength: Integer;
Begin
  SegmentLength := ReadBE16;
  Remaining := SegmentLength - 2;
  If Marker = $DD Then
  Begin
    JpegRestart := ReadBE16;
    Exit;
  End;
  While Remaining > 0 Do
  Begin
    Info := NeedByte;
    If Marker = $DB Then
    Begin
      Precision := Info Shr 4;
      Index := Info And 15;
      If (Index > 3) Or (Precision > 1) Then DecodeError('table de quantification JPEG invalide.');
      For K := 0 To 63 Do
        If Precision = 0 Then
          JpegQuant[Index, K] := NeedByte
        Else
          JpegQuant[Index, K] := ReadBE16;
      Dec(Remaining, 1 + 64 * (Precision + 1));
    End
    Else
    Begin
      Index := (Info Shr 4) * 2 + (Info And 15);
      If ((Info Shr 4) > 1) Or ((Info And 15) > 1) Then
        DecodeError('table de Huffman JPEG invalide.');
      Total := 0;
      For K := 1 To 16 Do
      Begin
        Counts[K] := NeedByte;
        Inc(Total, Counts[K]);
      End;
      If Total > 256 Then DecodeError('table de Huffman JPEG invalide.');
      FillChar(JpegHuffman[Index].Values, 256, 0);
      For K := 0 To Total - 1 Do
        JpegHuffman[Index].Values[K] := NeedByte;
      Code := 0;
      K := 0;
      For Len := 1 To 16 Do
      Begin
        JpegHuffman[Index].ValPtr[Len] := K;
        JpegHuffman[Index].MinCode[Len] := Code;
        Inc(Code, Counts[Len]);
        Inc(K, Counts[Len]);
        If Counts[Len] > 0 Then
          JpegHuffman[Index].MaxCode[Len] := Code - 1
        Else
          JpegHuffman[Index].MaxCode[Len] := -1;
        Code := Code Shl 1;
      End;
      JpegHuffman[Index].Defined := True;
      Dec(Remaining, 17 + Total);
    End;
  End;
End;

Procedure JpegReadFrame;
Var
  SegmentLength, I, Info, Count: Integer;
  NewWidth, NewHeight: Integer;
Begin
  SegmentLength := ReadBE16;
  If NeedByte <> 8 Then DecodeError('precision JPEG non prise en charge.');
  NewHeight := ReadBE16;
  NewWidth := ReadBE16;
  SetImageSize(NewWidth, NewHeight);
  Count := NeedByte;
  If (Count <> 1) And (Count <> 3) Then
    DecodeError('nombre de composantes JPEG non pris en charge.');
  JpegComponentCount := Count;
  JpegHMax := 1;
  JpegVMax := 1;
  For I := 1 To Count Do
  Begin
    JpegComponent[I].Id := NeedByte;
    Info := NeedByte;
    JpegComponent[I].H := Info Shr 4;
    JpegComponent[I].V := Info And 15;
    JpegComponent[I].Tq := NeedByte;
    If (JpegComponent[I].H < 1) Or (JpegComponent[I].H > 4) Or
       (JpegComponent[I].V < 1) Or (JpegComponent[I].V > 4) Or
       (JpegComponent[I].Tq > 3) Then
      DecodeError('composante JPEG invalide.');
    If JpegComponent[I].H > JpegHMax Then JpegHMax := JpegComponent[I].H;
    If JpegComponent[I].V > JpegVMax Then JpegVMax := JpegComponent[I].V;
  End;
  SkipBytes(SegmentLength - 8 - 3 * Count);
  If (JpegComponent[1].H <> JpegHMax) Or (JpegComponent[1].V <> JpegVMax) Then
    DecodeError('echantillonnage JPEG non pris en charge.');
  JpegFrameSeen := True;
End;

{ DCT inverse separable ; ecrit les 8 x 8 pixels du bloc dans l'image. }
Procedure JpegStoreBlock(OriginX, OriginY: Integer);
Var
  Temp: Array[0..7, 0..7] Of Real;
  U, V, X, Y: Integer;
  Sum: Real;
  Value: Longint;
Begin
  For V := 0 To 7 Do
    For X := 0 To 7 Do
    Begin
      Sum := 0;
      For U := 0 To 7 Do
        Sum := Sum + JpegCosine[X, U] * JpegBlock[V * 8 + U];
      Temp[V, X] := Sum;
    End;
  For Y := 0 To 7 Do
    For X := 0 To 7 Do
    Begin
      Sum := 0;
      For V := 0 To 7 Do
        Sum := Sum + JpegCosine[Y, V] * Temp[V, X];
      Value := Round(Sum) + 128;
      If Value < 0 Then Value := 0;
      If Value > 255 Then Value := 255;
      PutPixel(OriginX + X + 1, OriginY + Y + 1, Value, 255);
    End;
End;

Procedure JpegDecodeBlock(Index: Integer);
Var
  Size, K, Symbol, Run: Integer;
  Table: Integer;
Begin
  FillChar(JpegBlock, SizeOf(JpegBlock), 0);
  Table := JpegComponent[Index].Tq;
  Size := JpegDecode(JpegHuffman[JpegComponent[Index].Td]);
  If Size > 11 Then DecodeError('donnees JPEG invalides.');
  Inc(JpegComponent[Index].Pred, JpegExtend(JpegReceive(Size), Size));
  JpegBlock[0] := JpegComponent[Index].Pred * JpegQuant[Table, 0];
  K := 1;
  While K < 64 Do
  Begin
    Symbol := JpegDecode(JpegHuffman[2 + JpegComponent[Index].Ta]);
    Run := Symbol Shr 4;
    Size := Symbol And 15;
    If Size = 0 Then
    Begin
      If Run = 15 Then
        Inc(K, 16)
      Else
        Break;
    End
    Else
    Begin
      Inc(K, Run);
      If K > 63 Then Break;
      JpegBlock[ZigZag[K]] := JpegExtend(JpegReceive(Size), Size) *
                              JpegQuant[Table, K];
      Inc(K);
    End;
  End;
End;

Procedure JpegReadScan;
Var
  SegmentLength, ScanCount, I, J, Selector, Info: Integer;
  ScanList: Array[1..3] Of Integer;
  Interleaved: Boolean;
  McuAcross, McuDown, McuIndex, McuTotal: Longint;
  McuX, McuY, BlockH, BlockV, Comp: Integer;
  Marker: Integer;
Begin
  If Not JpegFrameSeen Then DecodeError('en-tete JPEG manquant.');
  SegmentLength := ReadBE16;
  ScanCount := NeedByte;
  If (ScanCount < 1) Or (ScanCount > JpegComponentCount) Then
    DecodeError('balayage JPEG invalide.');
  For I := 1 To ScanCount Do
  Begin
    Selector := NeedByte;
    Info := NeedByte;
    ScanList[I] := 0;
    For J := 1 To JpegComponentCount Do
      If JpegComponent[J].Id = Selector Then ScanList[I] := J;
    If ScanList[I] = 0 Then DecodeError('composante JPEG inconnue.');
    JpegComponent[ScanList[I]].Td := Info Shr 4;
    JpegComponent[ScanList[I]].Ta := Info And 15;
    If (JpegComponent[ScanList[I]].Td > 1) Or (JpegComponent[ScanList[I]].Ta > 1) Then
      DecodeError('table de Huffman JPEG invalide.');
    If (Not JpegHuffman[JpegComponent[ScanList[I]].Td].Defined) Or
       (Not JpegHuffman[2 + JpegComponent[ScanList[I]].Ta].Defined) Then
      DecodeError('table de Huffman JPEG manquante.');
  End;
  SkipBytes(SegmentLength - 2 - 1 - 2 * ScanCount);

  For I := 1 To JpegComponentCount Do
    JpegComponent[I].Pred := 0;
  JpegBitCount := 0;
  JpegMarker := 0;
  Interleaved := ScanCount > 1;

  If Interleaved Then
  Begin
    McuAcross := (ImageWidth + 8 * JpegHMax - 1) Div (8 * JpegHMax);
    McuDown := (ImageHeight + 8 * JpegVMax - 1) Div (8 * JpegVMax);
  End
  Else
  Begin
    Comp := ScanList[1];
    McuAcross := (((Longint(ImageWidth) * JpegComponent[Comp].H + JpegHMax - 1) Div
                   JpegHMax) + 7) Div 8;
    McuDown := (((Longint(ImageHeight) * JpegComponent[Comp].V + JpegVMax - 1) Div
                 JpegVMax) + 7) Div 8;
  End;
  McuTotal := McuAcross * McuDown;

  McuIndex := 0;
  While McuIndex < McuTotal Do
  Begin
    If (JpegRestart > 0) And (McuIndex > 0) And (McuIndex Mod JpegRestart = 0) Then
    Begin
      Marker := JpegNextMarker;
      If (Marker < $D0) Or (Marker > $D7) Then
        DecodeError('marqueur de redemarrage JPEG invalide.');
      JpegBitCount := 0;
      For I := 1 To JpegComponentCount Do
        JpegComponent[I].Pred := 0;
    End;
    McuX := McuIndex Mod McuAcross;
    McuY := McuIndex Div McuAcross;
    For I := 1 To ScanCount Do
    Begin
      Comp := ScanList[I];
      If Interleaved Then
      Begin
        For BlockV := 0 To JpegComponent[Comp].V - 1 Do
          For BlockH := 0 To JpegComponent[Comp].H - 1 Do
          Begin
            JpegDecodeBlock(Comp);
            If Comp = 1 Then
              JpegStoreBlock((McuX * JpegComponent[1].H + BlockH) * 8,
                             (McuY * JpegComponent[1].V + BlockV) * 8);
          End;
      End
      Else
      Begin
        JpegDecodeBlock(Comp);
        If Comp = 1 Then JpegStoreBlock(McuX * 8, McuY * 8);
      End;
    End;
    Inc(McuIndex);
  End;

  For I := 1 To ScanCount Do
    If ScanList[I] = 1 Then JpegLumaDone := True;
End;

Procedure LoadJpeg;
Var
  Marker, Len, U, X: Integer;
Begin
  SkipBytes(2);
  For U := 0 To 7 Do
    For X := 0 To 7 Do
    Begin
      JpegCosine[X, U] := Cos((2 * X + 1) * U * Pi / 16) / 2;
      If U = 0 Then JpegCosine[X, U] := JpegCosine[X, U] / Sqrt(2);
    End;
  For U := 0 To 3 Do
    JpegHuffman[U].Defined := False;
  FillChar(JpegQuant, SizeOf(JpegQuant), 0);
  JpegRestart := 0;
  JpegMarker := 0;
  JpegFrameSeen := False;
  JpegLumaDone := False;

  While Not JpegLumaDone Do
  Begin
    Marker := JpegNextMarker;
    Case Marker Of
      $01, $D0..$D8: ;
      $D9: DecodeError('donnees d image JPEG absentes.');
      $C0, $C1: JpegReadFrame;
      $C2: DecodeError('JPEG progressif non pris en charge (JPEG de base seulement).');
      $C3, $C5..$C7, $C9..$CB, $CD..$CF:
        DecodeError('type de JPEG non pris en charge (JPEG de base seulement).');
      $C4, $DB, $DD: JpegReadTables(Marker);
      $DA: JpegReadScan;
    Else
      Len := ReadBE16;
      SkipBytes(Len - 2);
    End;
  End;
End;

{ ------------------------------------------------------------------ }
{ GIF : decompression LZW de la premiere image                        }
{ ------------------------------------------------------------------ }

Function GifNextByte: Integer;
Begin
  If GifEnded Then
  Begin
    GifNextByte := -1;
    Exit;
  End;
  If GifBlockLeft = 0 Then
  Begin
    GifBlockLeft := NeedByte;
    If GifBlockLeft = 0 Then
    Begin
      GifEnded := True;
      GifNextByte := -1;
      Exit;
    End;
  End;
  Dec(GifBlockLeft);
  GifNextByte := NeedByte;
End;

Function GifRowOf(Number: Integer): Integer;
Var
  Count: Integer;
Begin
  If Not GifInterlaced Then
  Begin
    GifRowOf := Number;
    Exit;
  End;
  Count := (GifFrameHeight + 7) Div 8;
  If Number < Count Then
  Begin
    GifRowOf := Number * 8;
    Exit;
  End;
  Dec(Number, Count);
  Count := (GifFrameHeight + 3) Div 8;
  If Number < Count Then
  Begin
    GifRowOf := 4 + Number * 8;
    Exit;
  End;
  Dec(Number, Count);
  Count := (GifFrameHeight + 1) Div 4;
  If Number < Count Then
  Begin
    GifRowOf := 2 + Number * 4;
    Exit;
  End;
  Dec(Number, Count);
  GifRowOf := 1 + Number * 2;
End;

Procedure GifPutIndex(Index: Integer);
Begin
  If GifRowNumber >= GifFrameHeight Then Exit;
  If Index <> GifTransparent Then
    PutPixel(GifFrameX + GifColumn + 1, GifFrameY + GifRowOf(GifRowNumber) + 1,
             GrayOf(Palette[Index * 3], Palette[Index * 3 + 1],
                    Palette[Index * 3 + 2]), 255);
  Inc(GifColumn);
  If GifColumn >= GifFrameWidth Then
  Begin
    GifColumn := 0;
    Inc(GifRowNumber);
  End;
End;

Procedure GifDecodeImage;
Var
  MinSize, ClearCode, EndCode, NextCode, CodeSize: Integer;
  Code, Old, InCode, First, StackTop, Value: Integer;
  Buffer: Longint;
  BufferBits: Integer;
Begin
  MinSize := NeedByte;
  If (MinSize < 2) Or (MinSize > 8) Then DecodeError('taille de code GIF invalide.');
  GetMem(GifPrefix, SizeOf(TWordBuffer));
  GetMem(GifSuffix, SizeOf(TByteBuffer));
  GetMem(GifStack, SizeOf(TByteBuffer));
  FillChar(GifPrefix^, SizeOf(TWordBuffer), 0);
  FillChar(GifSuffix^, SizeOf(TByteBuffer), 0);
  ClearCode := 1 Shl MinSize;
  EndCode := ClearCode + 1;
  NextCode := ClearCode + 2;
  CodeSize := MinSize + 1;
  Old := -1;
  First := 0;
  Buffer := 0;
  BufferBits := 0;
  GifBlockLeft := 0;
  GifEnded := False;
  GifColumn := 0;
  GifRowNumber := 0;

  While True Do
  Begin
    While BufferBits < CodeSize Do
    Begin
      Value := GifNextByte;
      If Value < 0 Then Exit;
      Buffer := Buffer Or (Longint(Value) Shl BufferBits);
      Inc(BufferBits, 8);
    End;
    Code := Buffer And ((Longint(1) Shl CodeSize) - 1);
    Buffer := Buffer Shr CodeSize;
    Dec(BufferBits, CodeSize);

    If Code = ClearCode Then
    Begin
      CodeSize := MinSize + 1;
      NextCode := ClearCode + 2;
      Old := -1;
    End
    Else If Code = EndCode Then
      Break
    Else If Old < 0 Then
    Begin
      If Code >= ClearCode Then DecodeError('donnees GIF invalides.');
      GifPutIndex(Code);
      First := Code;
      Old := Code;
    End
    Else
    Begin
      If Code > NextCode Then DecodeError('donnees GIF invalides.');
      InCode := Code;
      StackTop := 0;
      If Code = NextCode Then
      Begin
        GifStack^[StackTop] := First;
        Inc(StackTop);
        Code := Old;
      End;
      While Code >= ClearCode Do
      Begin
        GifStack^[StackTop] := GifSuffix^[Code];
        Inc(StackTop);
        Code := GifPrefix^[Code];
      End;
      First := Code;
      GifStack^[StackTop] := First;
      Inc(StackTop);
      If NextCode < 4096 Then
      Begin
        GifPrefix^[NextCode] := Old;
        GifSuffix^[NextCode] := First;
        Inc(NextCode);
        If (NextCode = (1 Shl CodeSize)) And (CodeSize < 12) Then Inc(CodeSize);
      End;
      Old := InCode;
      While StackTop > 0 Do
      Begin
        Dec(StackTop);
        GifPutIndex(GifStack^[StackTop]);
      End;
    End;
  End;
End;

Procedure GifSkipBlocks;
Var
  Size: Byte;
Begin
  Repeat
    Size := NeedByte;
    SkipBytes(Size);
  Until Size = 0;
End;

Procedure GifReadPalette(Colors: Integer);
Var
  I: Integer;
Begin
  For I := 0 To Colors * 3 - 1 Do
    Palette[I] := NeedByte;
End;

Procedure LoadGif;
Var
  Flags, BlockType, ExtType, Size, I, FrameWidth, FrameHeight: Integer;
  Extension: Array[1..255] Of Byte;
Begin
  SkipBytes(6);
  FrameWidth := ReadLE16;
  FrameHeight := ReadLE16;
  SetImageSize(FrameWidth, FrameHeight);
  Flags := NeedByte;
  SkipBytes(2);
  FillChar(Palette, SizeOf(Palette), 0);
  If (Flags And $80) <> 0 Then
    GifReadPalette(1 Shl ((Flags And 7) + 1));
  GifTransparent := -1;

  While True Do
  Begin
    BlockType := NeedByte;
    If BlockType = $21 Then
    Begin
      ExtType := NeedByte;
      If ExtType = $F9 Then
      Begin
        Size := NeedByte;
        For I := 1 To Size Do
          Extension[I] := NeedByte;
        If (Size >= 4) And ((Extension[1] And 1) <> 0) Then
          GifTransparent := Extension[4];
      End;
      GifSkipBlocks;
    End
    Else If BlockType = $2C Then
    Begin
      GifFrameX := ReadLE16;
      GifFrameY := ReadLE16;
      GifFrameWidth := ReadLE16;
      GifFrameHeight := ReadLE16;
      Flags := NeedByte;
      If (GifFrameWidth < 1) Or (GifFrameHeight < 1) Then
        DecodeError('image GIF vide.');
      If (Flags And $80) <> 0 Then
        GifReadPalette(1 Shl ((Flags And 7) + 1));
      GifInterlaced := (Flags And $40) <> 0;
      GifDecodeImage;
      Exit;
    End
    Else If BlockType = $3B Then
      DecodeError('aucune image dans le fichier GIF.')
    Else
      DecodeError('bloc GIF invalide.');
  End;
End;

{ Lecture du PBM ASCII P1 : jetons d'en-tete, puis pixels 0/1 avec ou sans espaces. }
Function PbmNextToken(Var Text1: String): Boolean;
Var
  Value: Integer;
Begin
  Text1 := '';
  Repeat
    Value := ReadBin;
    If Value = Ord('#') Then
      Repeat
        Value := ReadBin;
      Until (Value < 0) Or (Value = 10) Or (Value = 13);
  Until (Value < 0) Or (Value > 32);
  While (Value > 32) And (Value <> Ord('#')) Do
  Begin
    If Length(Text1) < 40 Then Text1 := Text1 + Chr(Value);
    Value := ReadBin;
  End;
  If Value = Ord('#') Then Dec(InPos);
  PbmNextToken := Text1 <> '';
End;

Function PbmNextPixel: Integer;
Var
  Value: Integer;
Begin
  Repeat
    Value := ReadBin;
    If Value = Ord('#') Then
      Repeat
        Value := ReadBin;
      Until (Value < 0) Or (Value = 10) Or (Value = 13);
  Until (Value < 0) Or (Value > 32);
  If Value < 0 Then
    PbmNextPixel := -1
  Else If (Value = Ord('0')) Or (Value = Ord('1')) Then
    PbmNextPixel := Value - Ord('0')
  Else
  Begin
    DecodeError('valeur de pixel PBM invalide.');
    PbmNextPixel := -1;
  End;
End;

Procedure LoadPbm;
Var
  Pixel, ParseError: Integer;
  NewWidth, NewHeight: Longint;
Begin
  If (Not PbmNextToken(Token)) Or (Token <> 'P1') Then
    DecodeError('image non prise en charge. Utilisez PBM ASCII P1, PNG, JPEG ou GIF.');
  If Not PbmNextToken(Token) Then DecodeError('largeur de l''image manquante.');
  Val(Token, NewWidth, ParseError);
  If ParseError <> 0 Then DecodeError('largeur invalide.');
  If Not PbmNextToken(Token) Then DecodeError('hauteur de l''image manquante.');
  Val(Token, NewHeight, ParseError);
  If ParseError <> 0 Then DecodeError('hauteur invalide.');
  SetImageSize(NewWidth, NewHeight);
  For Y := 1 To ImageHeight Do
    For X := 1 To ImageWidth Do
    Begin
      Pixel := PbmNextPixel;
      If Pixel < 0 Then DecodeError('donnees de pixels incompletes.');
      SetInk(X, Y, Pixel = 1);
    End;
  Close(BinFile);
End;

BEGIN
  If (ParamCount >= 1) And
     ((ParamStr(1) = '/?') Or (ParamStr(1) = '-?') Or
      (ParamStr(1) = '-h') Or (ParamStr(1) = '--help')) Then
  Begin
    ShowHelp;
    Halt(0);
  End;

  If ParamCount < 2 Then
  Begin
    ShowHelp;
    Halt(2);
  End;

  ArgumentIndex := 3;
  While ArgumentIndex <= ParamCount Do
  Begin
    If ParamStr(ArgumentIndex) = '-l' Then
    Begin
      If (ArgumentIndex = ParamCount) Or
         (ParamStr(ArgumentIndex + 1) <> 'fra') Then
      Begin
        Writeln('Erreur : seule la langue fra est acceptee.');
        Halt(2);
      End;
      Inc(ArgumentIndex);
    End
    Else
    Begin
      Writeln('Erreur : option non reconnue : ', ParamStr(ArgumentIndex));
      Halt(2);
    End;
    Inc(ArgumentIndex);
  End;

  ImageName := ParamStr(1);
  OutputName := ParamStr(2);
  If Length(OutputName) > 251 Then
  Begin
    Writeln('Erreur : le nom du fichier de sortie est trop long.');
    Halt(2);
  End;
  OutputName := OutputName + '.txt';

  Case DetectFormat Of
    FormatPng: LoadPng;
    FormatJpeg: LoadJpeg;
    FormatGif: LoadGif;
  Else
    LoadPbm;
  End;

  Assign(OutputFile, OutputName);
  {$I-}
  Rewrite(OutputFile);
  {$I+}
  If IOResult <> 0 Then
  Begin
    Writeln('Erreur : impossible de creer le fichier texte de sortie.');
    Halt(2);
  End;

  Y := 1;
  While Y <= ImageHeight Do
  Begin
    While (Y <= ImageHeight) And (Not RowHasInk(Y)) Do
      Inc(Y);
    If Y <= ImageHeight Then
    Begin
      TopY := Y;
      While (Y <= ImageHeight) And RowHasInk(Y) Do
        Inc(Y);
      BottomY := Y - 1;
      RecognizedLine := '';
      PreviousRight := 0;
      CurrentX := 1;
      While CurrentX <= ImageWidth Do
      Begin
        While (CurrentX <= ImageWidth) And
              (Not ColumnHasInk(CurrentX, TopY, BottomY)) Do
          Inc(CurrentX);
        If CurrentX <= ImageWidth Then
        Begin
          LeftX := CurrentX;
          While (CurrentX <= ImageWidth) And
                ColumnHasInk(CurrentX, TopY, BottomY) Do
            Inc(CurrentX);
          RightX := CurrentX - 1;
          If (PreviousRight > 0) And (LeftX - PreviousRight > 2) Then
            RecognizedLine := RecognizedLine + ' ';
          RecognizedLine := RecognizedLine +
            RecognizeGlyph(LeftX, RightX, TopY, BottomY);
          PreviousRight := RightX;
        End;
      End;
      Writeln(OutputFile, RecognizedLine);
    End;
  End;
  Close(OutputFile);
  Writeln('Reconnaissance terminee : ', OutputName);
END.
