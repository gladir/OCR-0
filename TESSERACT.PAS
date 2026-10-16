{ @author: Sylvain Maltais (support@gladir.com)
  @created: 2026
  @website(https://www.gladir.com/OCR-0)
  @abstract(Target: Turbo Pascal 7, Free Pascal 3.2)
  @description: Clone de la commande TESSERACT
}

Program TESSERACT;

Const
  MaxImageWidth = 200;
  MaxImageHeight = 200;
  FontCharacters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
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
  TImage = Array[1..MaxImageHeight, 1..MaxImageWidth] Of Byte;

Var
  InputFile, OutputFile: Text;
  Image: TImage;
  ImageName, OutputName: String;
  CurrentLine, Token: String;
  LinePosition: Integer;
  ImageWidth, ImageHeight: Integer;
  X, Y, TopY, BottomY, LeftX, RightX: Integer;
  CurrentX, PreviousRight: Integer;
  RowIndex, ColumnIndex, FontIndex: Integer;
  SourceX, SourceY, Errors, BestErrors, BestIndex: Integer;
  ParseError, ArgumentIndex: Integer;
  RecognizedLine: String;

Procedure ShowHelp;
Begin
  Writeln('TESSERACT - Cette commande permet de lancer la reconnaissance de caracteres d''une image.');
  Writeln;
  Writeln('Description :');
  Writeln('  Reconnait le texte (caracteres A-Z et 0-9) d''une image PBM ASCII P1');
  Writeln('  et l''ecrit dans un fichier texte, une ligne de texte par ligne d''image.');
  Writeln;
  Writeln('Syntaxe :');
  Writeln('  TESSERACT image.pbm base_de_sortie [-l fra]');
  Writeln('  TESSERACT /?');
  Writeln;
  Writeln('Parametres :');
  Writeln('  image.pbm        Fichier image d''entree au format PBM ASCII P1');
  Writeln('                   (maximum 200 x 200 pixels).');
  Writeln('  base_de_sortie   Nom de base du fichier de sortie; l''extension .txt');
  Writeln('                   est ajoutee automatiquement.');
  Writeln('  -l fra           Langue de reconnaissance (seule la valeur fra est acceptee).');
  Writeln('  /?               Affiche cette aide (aussi -?, -h et --help).');
  Writeln;
  Writeln('Exemple :');
  Writeln('  TESSERACT image.pbm sortie -l fra');
End;

Function NextToken(Var InputFile: Text; Var Token: String): Boolean;
Begin
  Token := '';
  While True Do
  Begin
    If LinePosition > Length(CurrentLine) Then
    Begin
      If Eof(InputFile) Then
      Begin
        NextToken := False;
        Exit;
      End;
      ReadLn(InputFile, CurrentLine);
      LinePosition := 1;
    End;

    If CurrentLine[LinePosition] = '#' Then
      LinePosition := Length(CurrentLine) + 1
    Else If CurrentLine[LinePosition] <= ' ' Then
      Inc(LinePosition)
    Else
      Break;
  End;

  While (LinePosition <= Length(CurrentLine)) And
        (CurrentLine[LinePosition] > ' ') And
        (CurrentLine[LinePosition] <> '#') Do
  Begin
    Token := Token + CurrentLine[LinePosition];
    Inc(LinePosition);
  End;
  NextToken := Token <> '';
End;

Function RowHasInk(RowNumber: Integer): Boolean;
Var
  Column: Integer;
Begin
  RowHasInk := False;
  For Column := 1 To ImageWidth Do
    If Image[RowNumber, Column] <> 0 Then
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
    If Image[Row, ColumnNumber] <> 0 Then
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
      SourceY := FirstRow + ((2 * RowIndex - 1) * (LastRow - FirstRow + 1)) Div 14;
      For ColumnIndex := 1 To 5 Do
      Begin
        SourceX := FirstColumn + ((2 * ColumnIndex - 1) * (LastColumn - FirstColumn + 1)) Div 10;
        If (Image[SourceY, SourceX] <> 0) <>
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

  Assign(InputFile, ImageName);
  {$I-}
  Reset(InputFile);
  {$I+}
  If IOResult <> 0 Then
  Begin
    Writeln('Erreur : impossible de lire le fichier image.');
    Halt(2);
  End;

  CurrentLine := '';
  LinePosition := 1;
  If (Not NextToken(InputFile, Token)) Or (Token <> 'P1') Then
  Begin
    Writeln('Erreur : image non prise en charge. Utilisez le format PBM ASCII P1.');
    Close(InputFile);
    Halt(2);
  End;

  If Not NextToken(InputFile, Token) Then
  Begin
    Writeln('Erreur : largeur de l image manquante.');
    Close(InputFile);
    Halt(2);
  End;
  Val(Token, ImageWidth, ParseError);
  If (ParseError <> 0) Or (ImageWidth < 1) Or (ImageWidth > MaxImageWidth) Then
  Begin
    Writeln('Erreur : largeur invalide (maximum 200 pixels).');
    Close(InputFile);
    Halt(2);
  End;

  If Not NextToken(InputFile, Token) Then
  Begin
    Writeln('Erreur : hauteur de l image manquante.');
    Close(InputFile);
    Halt(2);
  End;
  Val(Token, ImageHeight, ParseError);
  If (ParseError <> 0) Or (ImageHeight < 1) Or (ImageHeight > MaxImageHeight) Then
  Begin
    Writeln('Erreur : hauteur invalide (maximum 200 pixels).');
    Close(InputFile);
    Halt(2);
  End;

  For Y := 1 To ImageHeight Do
    For X := 1 To ImageWidth Do
    Begin
      If Not NextToken(InputFile, Token) Then
      Begin
        Writeln('Erreur : donnees de pixels incompletes.');
        Close(InputFile);
        Halt(2);
      End;
      If (Token <> '0') And (Token <> '1') Then
      Begin
        Writeln('Erreur : valeur de pixel PBM invalide.');
        Close(InputFile);
        Halt(2);
      End;
      If Token = '1' Then
        Image[Y, X] := 1
      Else
        Image[Y, X] := 0;
    End;
  Close(InputFile);

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