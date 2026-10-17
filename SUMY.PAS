{ @author: Sylvain Maltais (support@gladir.com)
  @created: 2026
  @website(https://www.gladir.com/OCR-0)
  @abstract(Target: Free Pascal 3.2)
  @description: Clone de la commande sumy (resume automatique de texte :
                sumy lex-rank --language=english --length=5 --url=https://...)

  Principe : le texte (--text, --file, --url ou l'entree standard) est decoupe
  en paragraphes, puis en phrases (abreviations connues, initiales, sigles).
  Chaque phrase est decoupee en mots, ramenee en minuscules, les mots vides de
    la
  langue sont ecartes et les autres sont reduits a leur radical (Porter pour
  l'anglais, radical simplifie pour les autres langues). Les phrases sont alors
  notees par l'algorithme choisi, les meilleures sont gardees et reaffichees
  dans l'ordre du document, comme le fait sumy.

  Algorithmes : luhn, edmundson, lsa, text-rank, lex-rank, sum-basic, kl,
  reduction et random.

  Une adresse (--url) est telechargee avec la commande externe curl (appelee
  par l'unite Dos), puis la page HTML est reduite aux paragraphes <p>.

  Ce programme utilise les chaines longues et les tableaux dynamiques de
  Free Pascal : il ne se compile pas avec Turbo Pascal 7.
}

{$mode objfpc}
{$H+}

Program SUMY;

Uses Dos;

Const
  Version = '1.0';
  HashSize = 262147;
  StopHashSize = 4099;

Type
  TIntArray = Array Of LongInt;
  TDblArray = Array Of Double;
  TStrArray = Array Of AnsiString;

  TSentence = Record
    Text: AnsiString;
    Par: LongInt;
    Ids: TIntArray;      { terme de chaque mot ; -1 pour un mot vide }
    Terms: TIntArray;    { termes des mots pleins seulement }
  End;

Var
  { Options }
  Algorithm: AnsiString;
  LanguageName: AnsiString;
  LengthText: AnsiString;
  SourceText, SourceFile, SourceUrl, SourceFormat: AnsiString;
  StopFile, BonusFile, StigmaFile, NullFile: AnsiString;

  { Document }
  Sents: Array Of TSentence;
  SentCount: LongInt;
  ParCount: LongInt;

  { Dictionnaire des radicaux }
  Dict: TStrArray;
  DictCount: LongInt;
  HashTab: Array Of LongInt;

  { Mots vides }
  StopWords: TStrArray;
  StopCount: LongInt;
  StopHash: Array Of LongInt;

  { Listes pour edmundson }
  BonusWords, StigmaWords, NullWords: TStrArray;

{ ------------------------------------------------------------------ }
{ Chaines et UTF-8                                                    }
{ ------------------------------------------------------------------ }

Function CodeToUtf8(Code: LongInt): AnsiString;
Begin
  If Code < $80 Then
    Result := Chr(Code)
  Else If Code < $800 Then
    Result := Chr($C0 Or (Code Shr 6)) + Chr($80 Or (Code And $3F))
  Else If Code < $10000 Then
    Result := Chr($E0 Or (Code Shr 12)) + Chr($80 Or ((Code Shr 6) And $3F)) +
        Chr($80 Or (Code And $3F))
  Else
    Result := Chr($F0 Or (Code Shr 18)) + Chr($80 Or ((Code Shr 12) And $3F)) +
              Chr($80 Or ((Code Shr 6) And $3F)) + Chr($80 Or (Code And $3F));
End;

Function IsValidUtf8(Const S: AnsiString): Boolean;
Var
  I, N: LongInt;
  C: Byte;
Begin
  Result := True;
  I := 1;
  While I <= Length(S) Do
  Begin
    C := Ord(S[I]);
    If C < $80 Then N := 0
    Else If (C >= $C2) And (C <= $DF) Then N := 1
    Else If (C >= $E0) And (C <= $EF) Then N := 2
    Else If (C >= $F0) And (C <= $F4) Then N := 3
    Else
    Begin
      Result := False;
      Exit;
    End;
    If I + N > Length(S) Then
    Begin
      Result := False;
      Exit;
    End;
    While N > 0 Do
    Begin
      Inc(I);
      If (Ord(S[I]) And $C0) <> $80 Then
      Begin
        Result := False;
        Exit;
      End;
      Dec(N);
    End;
    Inc(I);
  End;
End;

Function Latin1ToUtf8(Const S: AnsiString): AnsiString;
Var
  I: LongInt;
Begin
  Result := '';
  For I := 1 To Length(S) Do
    Result := Result + CodeToUtf8(Ord(S[I]));
End;

{ Minuscules : ASCII, Latin-1 et cyrillique de base. }
Function LowerText(Const S: AnsiString): AnsiString;
Var
  I: LongInt;
  C, D: Byte;
Begin
  Result := S;
  I := 1;
  While I <= Length(Result) Do
  Begin
    C := Ord(Result[I]);
    If (C >= 65) And (C <= 90) Then
      Result[I] := Chr(C + 32)
    Else If (C = $C3) And (I < Length(Result)) Then
    Begin
      D := Ord(Result[I + 1]);
      If (D >= $80) And (D <= $9E) And (D <> $97) Then Result[I + 1] := Chr(D +
          32);
      Inc(I);
    End
    Else If (C = $D0) And (I < Length(Result)) Then
    Begin
      D := Ord(Result[I + 1]);
      If (D >= $90) And (D <= $9F) Then Result[I + 1] := Chr(D + 32)
      Else If (D >= $A0) And (D <= $AF) Then
      Begin
        Result[I] := Chr($D1);
        Result[I + 1] := Chr(D - 32);
      End;
      Inc(I);
    End
    Else If C >= $C0 Then
      Inc(I);
    Inc(I);
  End;
End;

Function IsUpperAt(Const S: AnsiString; I: LongInt): Boolean;
Var
  C, D: Byte;
Begin
  Result := False;
  If (I < 1) Or (I > Length(S)) Then Exit;
  C := Ord(S[I]);
  If (C >= 65) And (C <= 90) Then Result := True
  Else If (C = $C3) And (I < Length(S)) Then
  Begin
    D := Ord(S[I + 1]);
    Result := (D >= $80) And (D <= $9E) And (D <> $97);
  End
  Else If (C = $D0) And (I < Length(S)) Then
  Begin
    D := Ord(S[I + 1]);
    Result := (D >= $90) And (D <= $AF);
  End
  Else If (C = $C4) Or (C = $C5) Then
  Begin
    If I < Length(S) Then Result := (Ord(S[I + 1]) And 1) = 0;
  End;
End;

Function Trim2(Const S: AnsiString): AnsiString;
Var
  A, B: LongInt;
Begin
  A := 1;
  B := Length(S);
  While (A <= B) And (S[A] In [' ', #9, #10, #13]) Do Inc(A);
  While (B >= A) And (S[B] In [' ', #9, #10, #13]) Do Dec(B);
  Result := Copy(S, A, B - A + 1);
End;

{ Espaces multiples et fins de ligne ramenes a une seule espace. }
Function Collapse(Const S: AnsiString): AnsiString;
Var
  I: LongInt;
  Last: Boolean;
Begin
  Result := '';
  Last := True;
  For I := 1 To Length(S) Do
    If S[I] In [' ', #9, #10, #13] Then
    Begin
      If Not Last Then Result := Result + ' ';
      Last := True;
    End
    Else
    Begin
      Result := Result + S[I];
      Last := False;
    End;
  If (Result <> '') And (Result[Length(Result)] = ' ') Then Delete(Result,
      Length(Result), 1);
End;

{ ------------------------------------------------------------------ }
{ Lecture des sources                                                 }
{ ------------------------------------------------------------------ }

Function ReadTextFile(Const Name: AnsiString; Var Ok: Boolean): AnsiString;
Var
  F: Text;
  Line: AnsiString;
Begin
  Result := '';
  Ok := False;
  If Name = '-' Then
  Begin
    While Not Eof(Input) Do
    Begin
      ReadLn(Input, Line);
      Result := Result + Line + #10;
    End;
    Ok := True;
    Exit;
  End;
  {$I-}
  Assign(F, Name);
  SetTextCodePage(F, CP_UTF8);
  Reset(F);
  {$I+}
  If IOResult <> 0 Then Exit;
  While Not Eof(F) Do
  Begin
    ReadLn(F, Line);
    Result := Result + Line + #10;
  End;
  Close(F);
  Ok := True;
  If (Length(Result) >= 3) And (Result[1] = #$EF) And (Result[2] = #$BB) And
      (Result[3] = #$BF) Then
    Delete(Result, 1, 3);
End;

Function ReadWordFile(Const Name: AnsiString): TStrArray;
Var
  Ok: Boolean;
  Text1, Word1: AnsiString;
  I, N: LongInt;
Begin
  SetLength(Result, 0);
  Text1 := ReadTextFile(Name, Ok);
  If Not Ok Then
  Begin
    Writeln('Erreur : impossible de lire le fichier ', Name);
    Halt(2);
  End;
  If Not IsValidUtf8(Text1) Then Text1 := Latin1ToUtf8(Text1);
  Text1 := LowerText(Text1);
  N := 0;
  Word1 := '';
  For I := 1 To Length(Text1) + 1 Do
    If (I > Length(Text1)) Or (Text1[I] In [' ', #9, #10, #13, ',', ';']) Then
    Begin
      If Word1 <> '' Then
      Begin
        SetLength(Result, N + 1);
        Result[N] := Word1;
        Inc(N);
        Word1 := '';
      End;
    End
    Else
      Word1 := Word1 + Text1[I];
End;

Function ParseNumber(Const S: AnsiString; Base: LongInt): LongInt;
Var
  I, D: LongInt;
  C: Char;
Begin
  Result := -1;
  If S = '' Then Exit;
  Result := 0;
  For I := 1 To Length(S) Do
  Begin
    C := UpCase(S[I]);
    If C In ['0'..'9'] Then D := Ord(C) - 48
    Else If (C In ['A'..'F']) And (Base = 16) Then D := Ord(C) - 55
    Else
    Begin
      Result := -1;
      Exit;
    End;
    Result := Result * Base + D;
    If Result > $10FFFF Then
    Begin
      Result := -1;
      Exit;
    End;
  End;
End;

Function NamedEntity(Const Name: AnsiString): LongInt;
Begin
  Result := -1;
  If Name = 'amp' Then Result := 38
  Else If Name = 'lt' Then Result := 60
  Else If Name = 'gt' Then Result := 62
  Else If Name = 'quot' Then Result := 34
  Else If Name = 'apos' Then Result := 39
  Else If Name = 'nbsp' Then Result := 32
  Else If Name = 'ndash' Then Result := $2013
  Else If Name = 'mdash' Then Result := $2014
  Else If Name = 'lsquo' Then Result := $2018
  Else If Name = 'rsquo' Then Result := $2019
  Else If Name = 'ldquo' Then Result := $201C
  Else If Name = 'rdquo' Then Result := $201D
  Else If Name = 'hellip' Then Result := $2026
  Else If Name = 'laquo' Then Result := $AB
  Else If Name = 'raquo' Then Result := $BB
  Else If Name = 'copy' Then Result := $A9
  Else If Name = 'reg' Then Result := $AE
  Else If Name = 'deg' Then Result := $B0
  Else If Name = 'euro' Then Result := $20AC
  Else If Name = 'agrave' Then Result := $E0
  Else If Name = 'aacute' Then Result := $E1
  Else If Name = 'acirc' Then Result := $E2
  Else If Name = 'atilde' Then Result := $E3
  Else If Name = 'auml' Then Result := $E4
  Else If Name = 'aring' Then Result := $E5
  Else If Name = 'aelig' Then Result := $E6
  Else If Name = 'ccedil' Then Result := $E7
  Else If Name = 'egrave' Then Result := $E8
  Else If Name = 'eacute' Then Result := $E9
  Else If Name = 'ecirc' Then Result := $EA
  Else If Name = 'euml' Then Result := $EB
  Else If Name = 'igrave' Then Result := $EC
  Else If Name = 'iacute' Then Result := $ED
  Else If Name = 'icirc' Then Result := $EE
  Else If Name = 'iuml' Then Result := $EF
  Else If Name = 'ntilde' Then Result := $F1
  Else If Name = 'ograve' Then Result := $F2
  Else If Name = 'oacute' Then Result := $F3
  Else If Name = 'ocirc' Then Result := $F4
  Else If Name = 'otilde' Then Result := $F5
  Else If Name = 'ouml' Then Result := $F6
  Else If Name = 'ugrave' Then Result := $F9
  Else If Name = 'uacute' Then Result := $FA
  Else If Name = 'ucirc' Then Result := $FB
  Else If Name = 'uuml' Then Result := $FC
  Else If Name = 'szlig' Then Result := $DF
  Else If Name = 'Agrave' Then Result := $C0
  Else If Name = 'Eacute' Then Result := $C9
  Else If Name = 'Egrave' Then Result := $C8
  Else If Name = 'Ccedil' Then Result := $C7
  Else If Name = 'Ouml' Then Result := $D6
  Else If Name = 'Uuml' Then Result := $DC
  Else If Name = 'Auml' Then Result := $C4;
End;

{ Retire les references entre crochets : [12], [citation needed], [note 3]. }
Function StripBrackets(Const S: AnsiString): AnsiString;
Var
  I, J: LongInt;
  Skip: Boolean;
Begin
  Result := '';
  I := 1;
  While I <= Length(S) Do
  Begin
    Skip := False;
    If S[I] = '[' Then
    Begin
      J := I + 1;
      While (J <= Length(S)) And (J - I <= 24) And (S[J] <> ']') And (S[J] <>
          '[') Do Inc(J);
      If (J <= Length(S)) And (S[J] = ']') Then
      Begin
        Result := Result + '';
        I := J + 1;
        Skip := True;
      End;
    End;
    If Not Skip Then
    Begin
      Result := Result + S[I];
      Inc(I);
    End;
  End;
End;

{ Page HTML -> texte : paragraphes <p> si possible, sinon tout le corps. }
Function HtmlToText(Const Html: AnsiString): AnsiString;
Var
  Low: AnsiString;
  I, J, K: LongInt;
  InP, HasP, InSkip, Pass: LongInt;
  Tag, Name: AnsiString;
  Par, Ent: AnsiString;
  Code: LongInt;
  C: Char;
  Acc: AnsiString;

  Procedure FlushPar;
  Var
    T: AnsiString;
  Begin
    T := Collapse(StripBrackets(Par));
    If T <> '' Then Acc := Acc + T + #10#10;
    Par := '';
  End;

Begin
  Low := LowerText(Html);
  HasP := Pos('<p', Low);
  Result := '';
  For Pass := 1 To 2 Do
  Begin
    { passe 1 : seulement les <p> ; passe 2 : tout le texte }
    If (Pass = 1) And (HasP = 0) Then Continue;
    Acc := '';
    Par := '';
    InP := 0;
    InSkip := 0;
    I := 1;
    While I <= Length(Html) Do
    Begin
      C := Html[I];
      If C = '<' Then
      Begin
        K := Pos('>', Copy(Html, I, 4000));
        If K = 0 Then K := Length(Html) - I + 1;
        Tag := Copy(Low, I + 1, K - 2);
        Name := '';
        J := 1;
        If (J <= Length(Tag)) And (Tag[J] = '/') Then Inc(J);
        While (J <= Length(Tag)) And (Tag[J] In ['a'..'z', '0'..'9']) Do
        Begin
          Name := Name + Tag[J];
          Inc(J);
        End;
        If Copy(Low, I, 4) = '<!--' Then
        Begin
          K := Pos('-->', Copy(Low, I, Length(Low)));
          If K = 0 Then K := Length(Low) - I + 1 Else K := K + 2;
        End;
        If (Name = 'script') Or (Name = 'style') Or (Name = 'noscript') Or (Name
            = 'head') Or
           (Name = 'nav') Or (Name = 'footer') Or (Name = 'aside') Or (Name =
               'table') Or
           (Name = 'sup') Then
        Begin
          If (Tag <> '') And (Tag[1] = '/') Then
          Begin
            If InSkip > 0 Then Dec(InSkip);
          End
          Else If (Tag = '') Or (Tag[Length(Tag)] <> '/') Then
            Inc(InSkip);
        End
        Else If (Name = 'p') Or (Name = 'li') Or (Name = 'h1') Or (Name = 'h2')
            Or (Name = 'h3') Or
                (Name = 'div') Or (Name = 'br') Or (Name = 'tr') Or (Name =
                    'blockquote') Then
        Begin
          If Pass = 2 Then FlushPar
          Else If (Tag <> '') And (Tag[1] = '/') And (Name = 'p') Then
          Begin
            FlushPar;
            InP := 0;
          End
          Else If (Name = 'p') And (Tag[1] <> '/') Then
          Begin
            FlushPar;
            InP := 1;
          End;
        End;
        I := I + K;
      End
      Else If (InSkip > 0) Or ((Pass = 1) And (InP = 0)) Then
        Inc(I)
      Else If C = '&' Then
      Begin
        K := Pos(';', Copy(Html, I, 12));
        Ent := '';
        If K > 2 Then Ent := Copy(Html, I + 1, K - 2);
        Code := -1;
        If Ent <> '' Then
        Begin
          If Ent[1] = '#' Then
          Begin
            If (Length(Ent) > 1) And (Ent[2] In ['x', 'X']) Then
              Code := ParseNumber(Copy(Ent, 3, 8), 16)
            Else
              Code := ParseNumber(Copy(Ent, 2, 8), 10);
          End
          Else
            Code := NamedEntity(Ent);
        End;
        If Code >= 0 Then
        Begin
          If Code = 160 Then Code := 32;
          Par := Par + CodeToUtf8(Code);
          I := I + K;
        End
        Else
        Begin
          Par := Par + C;
          Inc(I);
        End;
      End
      Else
      Begin
        If C In [#10, #13, #9] Then C := ' ';
        Par := Par + C;
        Inc(I);
      End;
    End;
    FlushPar;
    Result := Acc;
    If Length(Collapse(Acc)) > 40 Then Break;
  End;
End;

{ Telecharge une adresse avec curl (commande externe) vers un fichier
  temporaire. }
Function FetchUrl(Const Url: AnsiString; Var Ok: Boolean): AnsiString;
Var
  Tmp, Cmd, Shell: AnsiString;
  F: File;
  Buf: AnsiString;
  Got: LongInt;
  Dir: AnsiString;
Begin
  Result := '';
  Ok := False;
  If (Pos('"', Url) > 0) Or (Pos('`', Url) > 0) Or (Pos('$', Url) > 0) Or
      (Pos('%', Url) > 0) Then
  Begin
    Writeln('Erreur : adresse non valide : ', Url);
    Halt(2);
  End;
  Dir := GetEnv('TEMP');
  If Dir = '' Then Dir := GetEnv('TMP');
  If Dir = '' Then Dir := GetEnv('TMPDIR');
  If Dir = '' Then Dir := '.';
  If Dir[Length(Dir)] In ['\', '/'] Then Delete(Dir, Length(Dir), 1);
  Tmp := Dir + '/sumy_page.tmp';
  Cmd :=
      'curl -s -L --max-time 60 -A "Mozilla/5.0 (compatible; SUMY-Pascal)" -o "'
      + Tmp + '" "' + Url + '"';
  Shell := GetEnv('COMSPEC');
  SwapVectors;
  If Shell <> '' Then
    Exec(Shell, '/C ' + Cmd)
  Else
    Exec('/bin/sh', '-c ''' + Cmd + '''');
  SwapVectors;
  Assign(F, Tmp);
  {$I-}
  Reset(F, 1);
  {$I+}
  If IOResult <> 0 Then Exit;
  SetLength(Buf, FileSize(F));
  If Length(Buf) > 0 Then BlockRead(F, Buf[1], Length(Buf), Got);
  Close(F);
  Erase(F);
  Result := Buf;
  Ok := Length(Result) > 0;
End;

{ ------------------------------------------------------------------ }
{ Mots vides                                                          }
{ ------------------------------------------------------------------ }

Function StopWordsOf(Const Lang: AnsiString): AnsiString;
Begin
  Result := '';
  If Lang = 'english' Then
    Result :=
        'i me my myself we our ours ourselves you your yours yourself yourselv'+
        'es he him his himself she her hers herself it its itself they them th'+
        'eir theirs themselves what which who whom this that these those am is'+
        ' are was were be been being have has had having do does did doing a a'+
        'n the and but if or because as until while of at by for with about ag'+
        'ainst between into through during before after above below to from up'+
        ' down in out on off over under again further then once here there whe'+
        'n where why how all any both each few more most other some such no no'+
        'r not only own same so than too very s t can will just don should now'+
        ' d ll m o re ve y ain aren couldn didn doesn hadn hasn haven isn ma m'+
        'ightn mustn needn shan shouldn wasn weren won wouldn would could also'+
        ' may might must shall whose however yet among upon within without als'+
        'o via per'
  Else If Lang = 'french' Then
    Result :=
        'au aux avec ce ces dans de des du elle en et eux il je la le les leur'+
        ' lui ma mais me même mes moi mon ne nos notre nous on ou par pas pou'+
        'r qu que qui sa se ses son sur ta te tes toi ton tu un une vos votre '+
        'vous c d j l à m n s t y été étée étées étés étant suis es '+
        'est sommes êtes sont serai seras sera serons serez seront serais ser'+
        'ait serions seriez seraient étais était étions étiez étaient fus'+
        ' fut fûmes fûtes furent sois soit soyons soyez soient ayant eu eue '+
        'eues eus ai as avons avez ont aurai auras aura aurons aurez auront au'+
        'rais aurait aurions auriez auraient avais avait avions aviez avaient '+
        'eut eurent aie aies ait ayons ayez aient cet cette ceci cela ça celu'+
        'i celle ceux celles dont où ainsi alors aussi donc car si comme plus'+
        ' moins très tout tous toute toutes bien peu sans sous entre vers che'+
        'z après avant depuis pendant selon lors afin autres autre quelques q'+
        'uel quelle quels quelles ni ci là'
  Else If Lang = 'german' Then
    Result :=
        'aber alle allem allen aller alles als also am an ander andere anderem'+
        ' anderen anderer anderes auch auf aus bei bin bis bist da damit dann '+
        'der den des dem die das dass daß derselbe dich dir doch dort du durc'+
        'h ein eine einem einen einer eines einig einige er es etwas euch euer'+
        ' eure für gegen gewesen hab habe haben hat hatte hatten hier hin hin'+
        'ter ich ihm ihn ihnen ihr ihre im in indem ins ist jede jedem jeden j'+
        'eder jedes jene jenem jenen jener jenes jetzt kann kein keine können'+
        ' machen man manche manchem manchen mancher manches mein meine mit mus'+
        's musste nach nicht nichts noch nun nur ob oder ohne sehr sein seine '+
        'seinem seinen seiner seines selbst sich sie sind so solche solchen so'+
        'lcher soll sollte sondern sonst über um und uns unser unsere unter v'+
        'iel vom von vor während war waren warst was weg weil weiter welche w'+
        'elchem welchen welcher wenn werde werden wie wieder will wir wird wir'+
        'st wo wollen wollte würde würden zu zum zur zwar zwischen'
  Else If Lang = 'spanish' Then
    Result :=
        'de la que el en y a los del se las por un para con no una su al lo co'+
        'mo más pero sus le ya o este sí porque esta entre cuando muy sin so'+
        'bre también me hasta hay donde quien desde todo nos durante todos un'+
        'o les ni contra otros ese eso ante ellos e esto mí antes algunos qu'+
        'é unos yo otro otras otra él tanto esa estos mucho quienes nada muc'+
        'hos cual poco ella estar estas algunas algo nosotros mi mis tú te ti'+
        ' tu tus ellas nosotras vosotros vosotras os mío mía míos mías tuy'+
        'o tuya tuyos tuyas suyo suya suyos suyas nuestro nuestra nuestros nue'+
        'stras vuestro vuestra vuestros vuestras esos esas estoy estás está '+
        'estamos estáis están esté estés estemos estéis estén es son era'+
        ' eran fue fueron ser sido he has ha hemos han había habían tiene ti'+
        'enen tener tengo'
  Else If Lang = 'italian' Then
    Result :=
        'ad al allo ai agli all agl alla alle con col coi da dal dallo dai dag'+
        'li dall dagl dalla dalle di del dello dei degli dell degl della delle'+
        ' in nel nello nei negli nell negl nella nelle su sul sullo sui sugli '+
        'sull sugl sulla sulle per tra contro io tu lui lei noi voi loro mio m'+
        'ia miei mie tuo tua tuoi tue suo sua suoi sue nostro nostra nostri no'+
        'stre vostro vostra vostri vostre mi ti ci vi lo la li le gli ne il un'+
        ' uno una ma ed se perché anche come dov dove che chi cui non più qu'+
        'ale quanto quanti quanta quante quello quelli quella quelle questo qu'+
        'esti questa queste si tutto tutti a c e i l o ho hai ha abbiamo avete'+
        ' hanno abbia sono sei è siamo siete era erano fu essere stato stata '+
        'stati state avere fatto fare molto'
  Else If Lang = 'portuguese' Then
    Result :=
        'de a o que e do da em um para é com não uma os no se na por mais as'+
        ' dos como mas foi ao ele das tem à seu sua ou ser quando muito há n'+
        'os já está eu também só pelo pela até isso ela entre era depois '+
        'sem mesmo aos ter seus quem nas me esse eles estão você tinha foram'+
        ' essa num nem suas meu às minha têm numa pelos elas havia seja qual'+
        ' será nós tenho lhe deles essas esses pelas este fosse dele tu te v'+
        'ocês vos lhes meus minhas teu tua teus tuas nosso nossa nossos nossa'+
        's dela delas esta estes estas aquele aquela aqueles aquelas isto aqui'+
        'lo estou estamos estão sou somos são'
  Else If Lang = 'dutch' Then
    Result :=
        'de en van ik te dat die in een hij het niet zijn is was op aan met al'+
        's voor had er maar om hem dan zou of wat mijn men dit zo door over ze'+
        ' zich bij ook tot je mij uit der daar haar naar heb hoe heeft hebben '+
        'deze u want nog zal me zij nu ge geen omdat iets worden toch al waren'+
        ' veel meer doen toen moet ben zonder kan hun dus alles onder ja eens '+
        'hier wie werd altijd doch wordt wezen kunnen ons zelf tegen na reeds '+
        'wil kon niets uw iemand geweest andere'
  Else If Lang = 'czech' Then
    Result :=
        'a aby ale ani až že bude by byl byla byli bylo být co či do ho i '+
        'jak je jeho její jejich jen ještě jsem jsme jsou k když která kt'+
        'eré který kteří má mají může na nad nebo není ní o od po po'+
        'd pro před při s se si tak také to ty u už v ve z za'
  Else If Lang = 'slovak' Then
    Result :=
        'a aby ale ani áno až bol bola boli bolo byť čo do ho i ja je jeho'+
        ' jej ich jeden k keď ktorá ktoré ktorý ktorí má na nad alebo ni'+
        'e o od po pod pre pri s sa si som sme sú to ty už v vo z za že';
End;

Function KnownLanguage(Const Lang: AnsiString): Boolean;
Begin
  Result := (StopWordsOf(Lang) <> '');
End;

{ ------------------------------------------------------------------ }
{ Tables de hachage : dictionnaire des radicaux et mots vides         }
{ ------------------------------------------------------------------ }

Function HashOf(Const S: AnsiString; Size: LongInt): LongInt;
Var
  I: LongInt;
  H: LongInt;
Begin
  H := 5381;
  For I := 1 To Length(S) Do
    H := ((H * 33) Xor Ord(S[I])) And $FFFFFF;
  Result := H Mod Size;
End;

Function DictIndex(Const W: AnsiString): LongInt;
Var
  H: LongInt;
Begin
  H := HashOf(W, HashSize);
  While HashTab[H] >= 0 Do
  Begin
    If Dict[HashTab[H]] = W Then
    Begin
      Result := HashTab[H];
      Exit;
    End;
    Inc(H);
    If H >= HashSize Then H := 0;
  End;
  If DictCount >= Length(Dict) Then SetLength(Dict, Length(Dict) * 2 + 256);
  Dict[DictCount] := W;
  HashTab[H] := DictCount;
  Result := DictCount;
  Inc(DictCount);
End;

Procedure AddStop(Const W: AnsiString);
Var
  H: LongInt;
Begin
  H := HashOf(W, StopHashSize);
  While StopHash[H] >= 0 Do
  Begin
    If StopWords[StopHash[H]] = W Then Exit;
    Inc(H);
    If H >= StopHashSize Then H := 0;
  End;
  If StopCount >= Length(StopWords) Then SetLength(StopWords, Length(StopWords)
      * 2 + 64);
  StopWords[StopCount] := W;
  StopHash[H] := StopCount;
  Inc(StopCount);
End;

Function IsStop(Const W: AnsiString): Boolean;
Var
  H: LongInt;
Begin
  Result := False;
  H := HashOf(W, StopHashSize);
  While StopHash[H] >= 0 Do
  Begin
    If StopWords[StopHash[H]] = W Then
    Begin
      Result := True;
      Exit;
    End;
    Inc(H);
    If H >= StopHashSize Then H := 0;
  End;
End;

Procedure LoadStopWords;
Var
  Raw, W: AnsiString;
  I: LongInt;
  Extra: TStrArray;
Begin
  StopCount := 0;
  SetLength(StopWords, 64);
  SetLength(StopHash, StopHashSize);
  For I := 0 To StopHashSize - 1 Do StopHash[I] := -1;
  If StopFile <> '' Then
  Begin
    Extra := ReadWordFile(StopFile);
    For I := 0 To High(Extra) Do AddStop(Extra[I]);
    Exit;
  End;
  Raw := StopWordsOf(LanguageName) + ' ';
  W := '';
  For I := 1 To Length(Raw) Do
    If Raw[I] = ' ' Then
    Begin
      If W <> '' Then AddStop(W);
      W := '';
    End
    Else
      W := W + Raw[I];
End;

{ ------------------------------------------------------------------ }
{ Radicaux : Porter pour l'anglais, radical simplifie pour le reste   }
{ ------------------------------------------------------------------ }

Var
  PB: AnsiString;
  PK, PJ: LongInt;

Function PCons(I: LongInt): Boolean;
Begin
  Case PB[I] Of
    'a', 'e', 'i', 'o', 'u': Result := False;
    'y': If I = 1 Then Result := True Else Result := Not PCons(I - 1);
  Else
    Result := True;
  End;
End;

Function PM: LongInt;
Var
  N, I: LongInt;
Begin
  N := 0;
  I := 1;
  Result := 0;
  While True Do
  Begin
    If I > PJ Then Begin Result := N; Exit; End;
    If Not PCons(I) Then Break;
    Inc(I);
  End;
  Inc(I);
  While True Do
  Begin
    While True Do
    Begin
      If I > PJ Then Begin Result := N; Exit; End;
      If PCons(I) Then Break;
      Inc(I);
    End;
    Inc(I);
    Inc(N);
    While True Do
    Begin
      If I > PJ Then Begin Result := N; Exit; End;
      If Not PCons(I) Then Break;
      Inc(I);
    End;
    Inc(I);
  End;
End;

Function PVowelInStem: Boolean;
Var
  I: LongInt;
Begin
  Result := False;
  For I := 1 To PJ Do
    If Not PCons(I) Then
    Begin
      Result := True;
      Exit;
    End;
End;

Function PDoubleC(I: LongInt): Boolean;
Begin
  Result := False;
  If I < 2 Then Exit;
  If PB[I] <> PB[I - 1] Then Exit;
  Result := PCons(I);
End;

Function PCvc(I: LongInt): Boolean;
Begin
  Result := False;
  If (I < 3) Or (Not PCons(I)) Or PCons(I - 1) Or (Not PCons(I - 2)) Then Exit;
  If PB[I] In ['w', 'x', 'y'] Then Exit;
  Result := True;
End;

Function PEnds(Const S: AnsiString): Boolean;
Var
  L: LongInt;
Begin
  Result := False;
  L := Length(S);
  If L > PK Then Exit;
  If Copy(PB, PK - L + 1, L) <> S Then Exit;
  PJ := PK - L;
  Result := True;
End;

Procedure PSetTo(Const S: AnsiString);
Begin
  PB := Copy(PB, 1, PJ) + S;
  PK := PJ + Length(S);
End;

Procedure PR(Const S: AnsiString);
Begin
  If PM > 0 Then PSetTo(S);
End;

Procedure PStep1ab;
Begin
  If PB[PK] = 's' Then
  Begin
    If PEnds('sses') Then Dec(PK, 2)
    Else If PEnds('ies') Then PSetTo('i')
    Else If (PK > 1) And (PB[PK - 1] <> 's') Then Dec(PK);
  End;
  If PEnds('eed') Then
  Begin
    If PM > 0 Then Dec(PK);
  End
  Else If (PEnds('ed') Or PEnds('ing')) And PVowelInStem Then
  Begin
    PK := PJ;
    If PEnds('at') Then PSetTo('ate')
    Else If PEnds('bl') Then PSetTo('ble')
    Else If PEnds('iz') Then PSetTo('ize')
    Else If PDoubleC(PK) Then
    Begin
      Dec(PK);
      If PB[PK] In ['l', 's', 'z'] Then Inc(PK);
    End
    Else
    Begin
      PJ := PK;
      If (PM = 1) And PCvc(PK) Then PSetTo('e');
    End;
  End;
End;

Procedure PStep1c;
Begin
  If PEnds('y') And PVowelInStem Then PB[PK] := 'i';
End;

Procedure PStep2;
Begin
  If PK < 2 Then Exit;
  Case PB[PK - 1] Of
    'a':
      If PEnds('ational') Then PR('ate')
      Else If PEnds('tional') Then PR('tion');
    'c':
      If PEnds('enci') Then PR('ence')
      Else If PEnds('anci') Then PR('ance');
    'e':
      If PEnds('izer') Then PR('ize');
    'l':
      If PEnds('bli') Then PR('ble')
      Else If PEnds('alli') Then PR('al')
      Else If PEnds('entli') Then PR('ent')
      Else If PEnds('eli') Then PR('e')
      Else If PEnds('ousli') Then PR('ous');
    'o':
      If PEnds('ization') Then PR('ize')
      Else If PEnds('ation') Then PR('ate')
      Else If PEnds('ator') Then PR('ate');
    's':
      If PEnds('alism') Then PR('al')
      Else If PEnds('iveness') Then PR('ive')
      Else If PEnds('fulness') Then PR('ful')
      Else If PEnds('ousness') Then PR('ous');
    't':
      If PEnds('aliti') Then PR('al')
      Else If PEnds('iviti') Then PR('ive')
      Else If PEnds('biliti') Then PR('ble');
    'g':
      If PEnds('logi') Then PR('log');
  End;
End;

Procedure PStep3;
Begin
  Case PB[PK] Of
    'e':
      If PEnds('icate') Then PR('ic')
      Else If PEnds('ative') Then PR('')
      Else If PEnds('alize') Then PR('al');
    'i':
      If PEnds('iciti') Then PR('ic');
    'l':
      If PEnds('ical') Then PR('ic')
      Else If PEnds('ful') Then PR('');
    's':
      If PEnds('ness') Then PR('');
  End;
End;

Procedure PStep4;
Var
  Hit: Boolean;
Begin
  If PK < 2 Then Exit;
  Hit := False;
  Case PB[PK - 1] Of
    'a': Hit := PEnds('al');
    'c': Hit := PEnds('ance') Or PEnds('ence');
    'e': Hit := PEnds('er');
    'i': Hit := PEnds('ic');
    'l': Hit := PEnds('able') Or PEnds('ible');
    'n': Hit := PEnds('ant') Or PEnds('ement') Or PEnds('ment') Or PEnds('ent');
    'o':
      Begin
        If PEnds('ion') And (PJ >= 1) And (PB[PJ] In ['s', 't']) Then Hit :=
            True
        Else Hit := PEnds('ou');
      End;
    's': Hit := PEnds('ism');
    't': Hit := PEnds('ate') Or PEnds('iti');
    'u': Hit := PEnds('ous');
    'v': Hit := PEnds('ive');
    'z': Hit := PEnds('ize');
  End;
  If Hit And (PM > 1) Then PK := PJ;
End;

Procedure PStep5;
Var
  A: LongInt;
Begin
  PJ := PK;
  If PB[PK] = 'e' Then
  Begin
    A := PM;
    If (A > 1) Or ((A = 1) And (Not PCvc(PK - 1))) Then Dec(PK);
  End;
  If (PB[PK] = 'l') And PDoubleC(PK) Then
  Begin
    PJ := PK;
    If PM > 1 Then Dec(PK);
  End;
End;

Function PorterStem(Const W: AnsiString): AnsiString;
Var
  I: LongInt;
Begin
  Result := W;
  If Length(W) <= 2 Then Exit;
  For I := 1 To Length(W) Do
    If Not (W[I] In ['a'..'z']) Then Exit;
  PB := W;
  PK := Length(W);
  PJ := PK;
  PStep1ab;
  If PK > 0 Then
  Begin
    PStep1c;
    PStep2;
    PStep3;
    PStep4;
    PStep5;
  End;
  Result := Copy(PB, 1, PK);
End;

{ Radical simplifie : retire le plus long suffixe connu de la langue. }
Function SuffixesOf(Const Lang: AnsiString): AnsiString;
Begin
  If Lang = 'french' Then
    Result :=
        'issements issement atrices atrice ateurs ateur ations ation ements em'+
        'ent euses euse ments ment ités ité ives ive ées ée és er ez ent '+
        's x e'
  Else If Lang = 'spanish' Then
    Result :=
        'amientos amiento aciones ación adores ador adoras adora mente idades'+
        ' idad ables able ibles ible ando iendo ados adas ado ada idos idas id'+
        'o ida ar er ir es os as o a e s'
  Else If Lang = 'portuguese' Then
    Result :=
        'amentos amento ações ação mente idades idade adores ador ando end'+
        'o indo ados adas ado ada idos idas ido ida ar er ir es os as o a e s'
  Else If Lang = 'italian' Then
    Result :=
        'azioni azione amente mente atori atore ità ando endo ati ate ato ata'+
        ' are ere ire i e o a'
  Else If Lang = 'german' Then
    Result := 'ungen ung heiten heit keiten keit lich isch ern en er es e s n'
  Else If Lang = 'dutch' Then
    Result := 'heden heid ingen ing lijk en e s'
  Else
    Result := '';
End;

Function LightStem(Const W: AnsiString): AnsiString;
Var
  List, Suf: AnsiString;
  I, P, Best, L: LongInt;

  Function CharCount(Const S: AnsiString): LongInt;
  Var
    K: LongInt;
  Begin
    Result := 0;
    For K := 1 To Length(S) Do
      If (Ord(S[K]) And $C0) <> $80 Then Inc(Result);
  End;

Begin
  Result := W;
  List := SuffixesOf(LanguageName) + ' ';
  Best := 0;
  Suf := '';
  P := 1;
  For I := 1 To Length(List) Do
    If List[I] = ' ' Then
    Begin
      Suf := Copy(List, P, I - P);
      P := I + 1;
      L := Length(Suf);
      If (L > Best) And (L < Length(W)) And (Copy(W, Length(W) - L + 1, L) =
          Suf) Then
        If CharCount(Copy(W, 1, Length(W) - L)) >= 3 Then Best := L;
    End;
  If Best > 0 Then Result := Copy(W, 1, Length(W) - Best);
End;

Function StemOf(Const W: AnsiString): AnsiString;
Begin
  If LanguageName = 'english' Then
    Result := PorterStem(W)
  Else
    Result := LightStem(W);
End;

{ ------------------------------------------------------------------ }
{ Decoupage en paragraphes, phrases et mots                           }
{ ------------------------------------------------------------------ }

Function IsAbbreviation(Const Tok: AnsiString): Boolean;
Begin
  Result := False;
  If Tok = '' Then Exit;
  If Pos(' ' + Tok + ' ',
     ' mr mrs ms dr prof sr jr st mt vs etc cf fig figs no nos vol vols inc lt'+
         'd co corp dept est approx ca ed eds pp ' +
     'jan feb mar apr jun jul aug sep sept oct nov dec mon tue wed thu fri sat'+
         ' sun gen col lt sgt capt gov sen rep ' +
     'm mme mlle mmes mm dr pr env art av apr-j-c cie ' +
     'z.b u.a bzw ca d.h usw nr str dipl ing ' +
     'sra srta dra ud uds ' +
     'sig sigg dott ing avv ' +
     'dhr mevr ir bv nv ') > 0 Then
    Result := True;
End;

{ La position I (un point, ? ou !) termine-t-elle une phrase ? }
Function EndsSentenceAt(Const S: AnsiString; I: LongInt; Var Next: LongInt):
    Boolean;
Var
  J, A, B: LongInt;
  Tok: AnsiString;
  C: Char;
  Dotted: Boolean;
Begin
  Result := False;
  J := I;
  { terminateurs et fermetures successifs }
  While (J < Length(S)) And (S[J + 1] In ['.', '!', '?', '"', '''', ')', ']'])
      Do Inc(J);
  While (J + 2 < Length(S)) And (S[J + 1] = #$E2) And (S[J + 2] = #$80) And
        (S[J + 3] In [#$99, #$9D, #$A6]) Do Inc(J, 3);
  While (J + 1 < Length(S)) And (S[J + 1] = #$C2) And (S[J + 2] = #$BB) Do
      Inc(J, 2);
  Next := J + 1;
  If J >= Length(S) Then
  Begin
    Result := True;
    Exit;
  End;
  If Not (S[J + 1] = ' ') Then Exit;
  { mot suivant }
  A := J + 1;
  While (A <= Length(S)) And (S[A] = ' ') Do Inc(A);
  If A > Length(S) Then
  Begin
    Result := True;
    Exit;
  End;
  C := S[A];
  If Not (IsUpperAt(S, A) Or (C In ['0'..'9', '"', '(', '[', '''']) Or
          ((C = #$C2) And (A < Length(S)) And (S[A + 1] = #$AB)) Or
          ((C = #$E2) And (A + 2 <= Length(S)) And (S[A + 1] = #$80) And (S[A +
              2] In [#$9C, #$98]))) Then Exit;
  { abbreviation avant le point ? }
  If S[I] = '.' Then
  Begin
    B := I - 1;
    A := B;
    While (A >= 1) And (S[A] <> ' ') And (S[A] <> '(') And (S[A] <> '"') Do
        Dec(A);
    Tok := LowerText(Copy(S, A + 1, B - A));
    Dotted := Pos('.', Tok) > 0;
    If IsAbbreviation(Tok) Then Exit;
    If Dotted And (Length(Tok) <= 6) Then Exit;
    If (Length(Tok) = 1) And IsUpperAt(S, A + 1) Then Exit;
  End;
  Result := True;
End;

Procedure AddSentence(Const T: AnsiString; Par: LongInt);
Var
  X: AnsiString;
Begin
  X := Collapse(T);
  If Length(X) < 2 Then Exit;
  If SentCount >= Length(Sents) Then SetLength(Sents, Length(Sents) * 2 + 64);
  Sents[SentCount].Text := X;
  Sents[SentCount].Par := Par;
  Inc(SentCount);
End;

Procedure SplitParagraph(Const P: AnsiString; ParIndex: LongInt);
Var
  I, Start, Next: LongInt;
Begin
  Start := 1;
  I := 1;
  While I <= Length(P) Do
  Begin
    If P[I] In ['.', '!', '?'] Then
    Begin
      If EndsSentenceAt(P, I, Next) Then
      Begin
        AddSentence(Copy(P, Start, Next - Start), ParIndex);
        Start := Next;
        I := Next;
        Continue;
      End;
    End
    Else If (P[I] = #$E2) And (I + 2 <= Length(P)) And (P[I + 1] = #$80) And
        (P[I + 2] = #$A6) Then
    Begin
      If EndsSentenceAt(P, I + 2, Next) Then
      Begin
        AddSentence(Copy(P, Start, Next - Start), ParIndex);
        Start := Next;
        I := Next;
        Continue;
      End;
    End;
    Inc(I);
  End;
  If Start <= Length(P) Then AddSentence(Copy(P, Start, Length(P) - Start + 1),
      ParIndex);
End;

Procedure SplitDocument(Const Doc: AnsiString);
Var
  I, Start: LongInt;
  Par, Line: AnsiString;
  Blank: Boolean;

  Procedure FlushParagraph;
  Begin
    If Collapse(Par) <> '' Then
    Begin
      Inc(ParCount);
      SplitParagraph(Collapse(Par), ParCount);
    End;
    Par := '';
  End;

Begin
  SentCount := 0;
  ParCount := 0;
  SetLength(Sents, 128);
  Par := '';
  Start := 1;
  For I := 1 To Length(Doc) + 1 Do
    If (I > Length(Doc)) Or (Doc[I] = #10) Then
    Begin
      Line := Copy(Doc, Start, I - Start);
      Start := I + 1;
      Blank := Trim2(Line) = '';
      If Blank Then FlushParagraph
      Else Par := Par + ' ' + Line;
    End;
  FlushParagraph;
End;

{ Mots de la phrase : ids de radicaux (-1 pour les mots vides). }
Procedure Analyze(Var S: TSentence);
Var
  T, W, Stem: AnsiString;
  I, N, M, Id: LongInt;
  Ws: TStrArray;
  C: Char;

  Procedure PushWord;
  Begin
    If W = '' Then Exit;
    If N >= Length(Ws) Then SetLength(Ws, Length(Ws) * 2 + 16);
    Ws[N] := W;
    Inc(N);
    W := '';
  End;

Begin
  T := LowerText(S.Text);
  SetLength(Ws, 32);
  N := 0;
  W := '';
  I := 1;
  While I <= Length(T) Do
  Begin
    C := T[I];
    If (C In ['a'..'z', '0'..'9']) Then
    Begin
      W := W + C;
      Inc(I);
    End
    Else If (C = #$C2) And (I < Length(T)) Then
    Begin
      PushWord;
      Inc(I, 2);
    End
    Else If (C = #$E2) And (I + 2 <= Length(T)) Then
    Begin
      PushWord;
      Inc(I, 3);
    End
    Else If C >= #$80 Then
    Begin
      W := W + C;
      Inc(I);
    End
    Else
    Begin
      PushWord;
      Inc(I);
    End;
  End;
  PushWord;
  SetLength(S.Ids, N);
  SetLength(S.Terms, N);
  M := 0;
  For I := 0 To N - 1 Do
  Begin
    If IsStop(Ws[I]) Then
      S.Ids[I] := -1
    Else
    Begin
      Stem := StemOf(Ws[I]);
      Id := DictIndex(Stem);
      S.Ids[I] := Id;
      S.Terms[M] := Id;
      Inc(M);
    End;
  End;
  SetLength(S.Terms, M);
End;

{ ------------------------------------------------------------------ }
{ Outils : tri, PageRank, similarites                                 }
{ ------------------------------------------------------------------ }

Function CountOfLength(Const Spec: AnsiString; Total: LongInt): LongInt;
Var
  V: LongInt;
  Pct: Double;
  Code: LongInt;
Begin
  If Spec = '' Then
  Begin
    Result := (Total * 20 + 99) Div 100;
  End
  Else If Spec[Length(Spec)] = '%' Then
  Begin
    Val(Copy(Spec, 1, Length(Spec) - 1), Pct, Code);
    If (Code <> 0) Or (Pct < 0) Then
    Begin
      Writeln('Erreur : longueur non valide : ', Spec);
      Halt(2);
    End;
    Result := Round(Total * Pct / 100);
  End
  Else
  Begin
    Val(Spec, V, Code);
    If (Code <> 0) Or (V < 0) Then
    Begin
      Writeln('Erreur : longueur non valide : ', Spec);
      Halt(2);
    End;
    Result := V;
  End;
  If Result > Total Then Result := Total;
  If (Result < 1) And (Total > 0) Then Result := 1;
End;

{ Distribution stationnaire (PageRank) d'une matrice de poids W (n x n). }
Function PageRank(Const W: Array Of TDblArray; N: LongInt): TDblArray;
Var
  P, Q: TDblArray;
  Deg: TDblArray;
  I, J, Iter: LongInt;
  Sum, Diff, D: Double;
Begin
  SetLength(P, N);
  SetLength(Q, N);
  SetLength(Deg, N);
  For I := 0 To N - 1 Do
  Begin
    Sum := 0;
    For J := 0 To N - 1 Do Sum := Sum + W[I][J];
    Deg[I] := Sum;
    P[I] := 1.0 / N;
  End;
  D := 0.85;
  For Iter := 1 To 200 Do
  Begin
    For I := 0 To N - 1 Do Q[I] := (1.0 - D) / N;
    For I := 0 To N - 1 Do
      If Deg[I] > 0 Then
      Begin
        For J := 0 To N - 1 Do
          If W[I][J] > 0 Then Q[J] := Q[J] + D * P[I] * W[I][J] / Deg[I];
      End
      Else
        For J := 0 To N - 1 Do Q[J] := Q[J] + D * P[I] / N;
    Diff := 0;
    For I := 0 To N - 1 Do Diff := Diff + Abs(Q[I] - P[I]);
    For I := 0 To N - 1 Do P[I] := Q[I];
    If Diff < 0.00001 Then Break;
  End;
  Result := P;
End;

{ ------------------------------------------------------------------ }
{ Algorithmes : chacun renvoie une note par phrase (grand = meilleur) }
{ ------------------------------------------------------------------ }

Procedure TermFrequencies(Var Tf: TIntArray);
Var
  I, J: LongInt;
Begin
  SetLength(Tf, DictCount);
  For I := 0 To DictCount - 1 Do Tf[I] := 0;
  For I := 0 To SentCount - 1 Do
    For J := 0 To High(Sents[I].Terms) Do
      Inc(Tf[Sents[I].Terms[J]]);
End;

Function RankLuhn: TDblArray;
Var
  Tf: TIntArray;
  I, J, Last, First, Cnt, Gap: LongInt;
  Best, Score: Double;
  Id: LongInt;
Begin
  SetLength(Result, SentCount);
  TermFrequencies(Tf);
  Gap := 4;
  For I := 0 To SentCount - 1 Do
  Begin
    Best := 0;
    First := -1;
    Last := -1;
    Cnt := 0;
    For J := 0 To High(Sents[I].Ids) Do
    Begin
      Id := Sents[I].Ids[J];
      If (Id >= 0) And (Tf[Id] > 1) Then
      Begin
        If (First >= 0) And (J - Last - 1 > Gap) Then
        Begin
          Score := Cnt * Cnt / (Last - First + 1);
          If Score > Best Then Best := Score;
          First := -1;
          Cnt := 0;
        End;
        If First < 0 Then First := J;
        Last := J;
        Inc(Cnt);
      End;
    End;
    If First >= 0 Then
    Begin
      Score := Cnt * Cnt / (Last - First + 1);
      If Score > Best Then Best := Score;
    End;
    Result[I] := Best;
  End;
End;

{ Vecteurs creux : termes tries avec leurs frequences. }
Type
  TSparse = Record
    Ids: TIntArray;
    Counts: TDblArray;
  End;

Procedure BuildSparse(Var Sp: TSparse; Const Terms: TIntArray);
Var
  A: TIntArray;
  I, J, N, T: LongInt;
Begin
  SetLength(A, Length(Terms));
  For I := 0 To High(Terms) Do A[I] := Terms[I];
  { tri par insertion (les phrases sont courtes) }
  For I := 1 To High(A) Do
  Begin
    T := A[I];
    J := I - 1;
    While (J >= 0) And (A[J] > T) Do
    Begin
      A[J + 1] := A[J];
      Dec(J);
    End;
    A[J + 1] := T;
  End;
  SetLength(Sp.Ids, Length(A));
  SetLength(Sp.Counts, Length(A));
  N := 0;
  I := 0;
  While I <= High(A) Do
  Begin
    J := I;
    While (J <= High(A)) And (A[J] = A[I]) Do Inc(J);
    Sp.Ids[N] := A[I];
    Sp.Counts[N] := J - I;
    Inc(N);
    I := J;
  End;
  SetLength(Sp.Ids, N);
  SetLength(Sp.Counts, N);
End;

Function RankLexRank: TDblArray;
Var
  Sp: Array Of TSparse;
  Df: TIntArray;
  Idf: TDblArray;
  M: Array Of TDblArray;
  Norm: TDblArray;
  I, J, A, B, K: LongInt;
  Dot: Double;
Begin
  SetLength(Result, SentCount);
  If SentCount = 0 Then Exit;
  SetLength(Sp, SentCount);
  SetLength(Df, DictCount);
  SetLength(Idf, DictCount);
  For I := 0 To DictCount - 1 Do Df[I] := 0;
  For I := 0 To SentCount - 1 Do
  Begin
    BuildSparse(Sp[I], Sents[I].Terms);
    For K := 0 To High(Sp[I].Ids) Do Inc(Df[Sp[I].Ids[K]]);
  End;
  For I := 0 To DictCount - 1 Do
    Idf[I] := Ln(SentCount / (1 + Df[I]));
  SetLength(Norm, SentCount);
  For I := 0 To SentCount - 1 Do
  Begin
    Dot := 0;
    For K := 0 To High(Sp[I].Ids) Do
      Dot := Dot + Sqr(Sp[I].Counts[K] * Idf[Sp[I].Ids[K]]);
    Norm[I] := Sqrt(Dot);
  End;
  SetLength(M, SentCount);
  For I := 0 To SentCount - 1 Do
  Begin
    SetLength(M[I], SentCount);
    For J := 0 To SentCount - 1 Do M[I][J] := 0;
  End;
  For I := 0 To SentCount - 1 Do
  Begin
    M[I][I] := 1;
    For J := I + 1 To SentCount - 1 Do
    Begin
      If (Norm[I] = 0) Or (Norm[J] = 0) Then Continue;
      A := 0;
      B := 0;
      Dot := 0;
      While (A <= High(Sp[I].Ids)) And (B <= High(Sp[J].Ids)) Do
      Begin
        If Sp[I].Ids[A] = Sp[J].Ids[B] Then
        Begin
          Dot := Dot + Sp[I].Counts[A] * Sp[J].Counts[B] *
              Sqr(Idf[Sp[I].Ids[A]]);
          Inc(A);
          Inc(B);
        End
        Else If Sp[I].Ids[A] < Sp[J].Ids[B] Then Inc(A)
        Else Inc(B);
      End;
      If Dot / (Norm[I] * Norm[J]) > 0.1 Then
      Begin
        M[I][J] := 1;
        M[J][I] := 1;
      End;
    End;
  End;
  Result := PageRank(M, SentCount);
End;

Function RankTextRank: TDblArray;
Var
  Sp: Array Of TSparse;
  M: Array Of TDblArray;
  I, J, A, B, Common: LongInt;
  Norm: Double;
Begin
  SetLength(Result, SentCount);
  If SentCount = 0 Then Exit;
  SetLength(Sp, SentCount);
  For I := 0 To SentCount - 1 Do BuildSparse(Sp[I], Sents[I].Terms);
  SetLength(M, SentCount);
  For I := 0 To SentCount - 1 Do
  Begin
    SetLength(M[I], SentCount);
    For J := 0 To SentCount - 1 Do M[I][J] := 0;
  End;
  For I := 0 To SentCount - 1 Do
    For J := I + 1 To SentCount - 1 Do
    Begin
      If (Length(Sp[I].Ids) < 1) Or (Length(Sp[J].Ids) < 1) Then Continue;
      A := 0;
      B := 0;
      Common := 0;
      While (A <= High(Sp[I].Ids)) And (B <= High(Sp[J].Ids)) Do
      Begin
        If Sp[I].Ids[A] = Sp[J].Ids[B] Then
        Begin
          Inc(Common);
          Inc(A);
          Inc(B);
        End
        Else If Sp[I].Ids[A] < Sp[J].Ids[B] Then Inc(A)
        Else Inc(B);
      End;
      If Common = 0 Then Continue;
      Norm := Ln(Length(Sp[I].Ids)) + Ln(Length(Sp[J].Ids));
      If Norm <= 0 Then Continue;
      M[I][J] := Common / Norm;
      M[J][I] := M[I][J];
    End;
  Result := PageRank(M, SentCount);
End;

Function RankReduction: TDblArray;
Var
  Sp: Array Of TSparse;
  I, J, A, B, Common: LongInt;
  Norm, W: Double;
Begin
  SetLength(Result, SentCount);
  If SentCount = 0 Then Exit;
  SetLength(Sp, SentCount);
  For I := 0 To SentCount - 1 Do
  Begin
    BuildSparse(Sp[I], Sents[I].Terms);
    Result[I] := 0;
  End;
  For I := 0 To SentCount - 1 Do
    For J := I + 1 To SentCount - 1 Do
    Begin
      If (Length(Sp[I].Ids) < 1) Or (Length(Sp[J].Ids) < 1) Then Continue;
      A := 0;
      B := 0;
      Common := 0;
      While (A <= High(Sp[I].Ids)) And (B <= High(Sp[J].Ids)) Do
      Begin
        If Sp[I].Ids[A] = Sp[J].Ids[B] Then
        Begin
          Inc(Common);
          Inc(A);
          Inc(B);
        End
        Else If Sp[I].Ids[A] < Sp[J].Ids[B] Then Inc(A)
        Else Inc(B);
      End;
      If Common = 0 Then Continue;
      Norm := Ln(Length(Sp[I].Ids)) + Ln(Length(Sp[J].Ids));
      If Norm <= 0 Then Continue;
      W := Common / Norm;
      Result[I] := Result[I] + W;
      Result[J] := Result[J] + W;
    End;
End;

{ LSA (Steinberger-Jezek) : avec toutes les dimensions conservees, la note de la
  phrase j est sqrt(somme_k v_kj^2 * s_k^2), c'est-a-dire la norme de la colonne
    j
  de la matrice termes x phrases ; l'ordre est celui de sumy. }
Function RankLsa: TDblArray;
Var
  Sp: TSparse;
  I, K: LongInt;
  MaxC, Sum, Ntf: Double;
Begin
  SetLength(Result, SentCount);
  For I := 0 To SentCount - 1 Do
  Begin
    BuildSparse(Sp, Sents[I].Terms);
    MaxC := 0;
    For K := 0 To High(Sp.Ids) Do
      If Sp.Counts[K] > MaxC Then MaxC := Sp.Counts[K];
    Sum := 0;
    For K := 0 To High(Sp.Ids) Do
    Begin
      Ntf := 0.4 + 0.6 * (Sp.Counts[K] / MaxC);
      Sum := Sum + Ntf * Ntf;
    End;
    Result[I] := Sqrt(Sum);
  End;
End;

{ Selection gloutonne : renvoie les notes (ordre de choix inverse). }
Function RankSumBasic(Count: LongInt): TDblArray;
Var
  Freq: TDblArray;
  Total: Double;
  Used: Array Of Boolean;
  I, J, Pick, Round1: LongInt;
  Best, Avg: Double;
Begin
  SetLength(Result, SentCount);
  SetLength(Freq, DictCount);
  SetLength(Used, SentCount);
  Total := 0;
  For I := 0 To DictCount - 1 Do Freq[I] := 0;
  For I := 0 To SentCount - 1 Do
  Begin
    Used[I] := False;
    Result[I] := 0;
    For J := 0 To High(Sents[I].Terms) Do
    Begin
      Freq[Sents[I].Terms[J]] := Freq[Sents[I].Terms[J]] + 1;
      Total := Total + 1;
    End;
  End;
  If Total = 0 Then Exit;
  For I := 0 To DictCount - 1 Do Freq[I] := Freq[I] / Total;
  For Round1 := 1 To Count Do
  Begin
    Pick := -1;
    Best := -1;
    For I := 0 To SentCount - 1 Do
      If Not Used[I] Then
      Begin
        Avg := 0;
        If Length(Sents[I].Terms) > 0 Then
        Begin
          For J := 0 To High(Sents[I].Terms) Do Avg := Avg +
              Freq[Sents[I].Terms[J]];
          Avg := Avg / Length(Sents[I].Terms);
        End;
        If Avg > Best Then
        Begin
          Best := Avg;
          Pick := I;
        End;
      End;
    If Pick < 0 Then Break;
    Used[Pick] := True;
    Result[Pick] := Count - Round1 + 1;
    For J := 0 To High(Sents[Pick].Terms) Do
      Freq[Sents[Pick].Terms[J]] := Sqr(Freq[Sents[Pick].Terms[J]]);
  End;
End;

Function RankKL(Count: LongInt): TDblArray;
Var
  Doc, Summ: TDblArray;
  Total, SumTotal, Kl, P, Q, BestKl: Double;
  Used: Array Of Boolean;
  I, J, Pick, Round1, V: LongInt;
Begin
  SetLength(Result, SentCount);
  SetLength(Doc, DictCount);
  SetLength(Summ, DictCount);
  SetLength(Used, SentCount);
  Total := 0;
  For I := 0 To DictCount - 1 Do
  Begin
    Doc[I] := 0;
    Summ[I] := 0;
  End;
  For I := 0 To SentCount - 1 Do
  Begin
    Used[I] := False;
    Result[I] := 0;
    For J := 0 To High(Sents[I].Terms) Do
    Begin
      Doc[Sents[I].Terms[J]] := Doc[Sents[I].Terms[J]] + 1;
      Total := Total + 1;
    End;
  End;
  If Total = 0 Then Exit;
  For I := 0 To DictCount - 1 Do Doc[I] := Doc[I] / Total;
  SumTotal := 0;
  V := DictCount;
  For Round1 := 1 To Count Do
  Begin
    Pick := -1;
    BestKl := 1E300;
    For I := 0 To SentCount - 1 Do
      If (Not Used[I]) And (Length(Sents[I].Terms) > 0) Then
      Begin
        { ajoute temporairement la phrase au resume }
        For J := 0 To High(Sents[I].Terms) Do
          Summ[Sents[I].Terms[J]] := Summ[Sents[I].Terms[J]] + 1;
        Kl := 0;
        For J := 0 To DictCount - 1 Do
          If Doc[J] > 0 Then
          Begin
            P := Doc[J];
            Q := (Summ[J] + 0.01) / (SumTotal + Length(Sents[I].Terms) + 0.01 *
                V);
            Kl := Kl + P * Ln(P / Q);
          End;
        For J := 0 To High(Sents[I].Terms) Do
          Summ[Sents[I].Terms[J]] := Summ[Sents[I].Terms[J]] - 1;
        If Kl < BestKl Then
        Begin
          BestKl := Kl;
          Pick := I;
        End;
      End;
    If Pick < 0 Then Break;
    Used[Pick] := True;
    Result[Pick] := Count - Round1 + 1;
    For J := 0 To High(Sents[Pick].Terms) Do
      Summ[Sents[Pick].Terms[J]] := Summ[Sents[Pick].Terms[J]] + 1;
    SumTotal := SumTotal + Length(Sents[Pick].Terms);
  End;
End;

Function RankRandom: TDblArray;
Var
  I: LongInt;
Begin
  SetLength(Result, SentCount);
  Randomize;
  For I := 0 To SentCount - 1 Do Result[I] := Random;
End;

{ Edmundson : indices de mots (bonus / malus), mots cles et emplacement. }
Function InWordList(Const List: TStrArray; Const W: AnsiString): Boolean;
Var
  I: LongInt;
Begin
  Result := False;
  For I := 0 To High(List) Do
    If List[I] = W Then
    Begin
      Result := True;
      Exit;
    End;
End;

Function RankEdmundson: TDblArray;
Var
  Tf: TIntArray;
  Sig: Array Of Boolean;
  Cue, Key, Loc: Double;
  I, J, N, Limit, Id: LongInt;
  T, W: AnsiString;
  C: Char;
  Best: LongInt;
  Cnt: TIntArray;
  Words: TStrArray;

  Procedure SplitWords(Const S: AnsiString);
  Var
    K, M: LongInt;
  Begin
    SetLength(Words, 0);
    M := 0;
    W := '';
    T := LowerText(S) + ' ';
    For K := 1 To Length(T) Do
    Begin
      C := T[K];
      If (C In ['a'..'z', '0'..'9']) Or (C >= #$80) Then W := W + C
      Else
      Begin
        If W <> '' Then
        Begin
          SetLength(Words, M + 1);
          Words[M] := W;
          Inc(M);
        End;
        W := '';
      End;
    End;
  End;

Begin
  SetLength(Result, SentCount);
  If (BonusFile = '') Or (StigmaFile = '') Or (NullFile = '') Then
  Begin
    Writeln('Erreur : edmundson demande --bonus-words, --stigma-words et --nul'+
        'l-words.');
    Halt(2);
  End;
  BonusWords := ReadWordFile(BonusFile);
  StigmaWords := ReadWordFile(StigmaFile);
  NullWords := ReadWordFile(NullFile);
  TermFrequencies(Tf);
  { mots cles : les plus frequents hors mots nuls }
  SetLength(Sig, DictCount);
  For I := 0 To DictCount - 1 Do Sig[I] := False;
  Limit := DictCount Div 10;
  If Limit < 5 Then Limit := 5;
  SetLength(Cnt, DictCount);
  For I := 0 To DictCount - 1 Do Cnt[I] := Tf[I];
  For N := 1 To Limit Do
  Begin
    Best := -1;
    For I := 0 To DictCount - 1 Do
      If (Cnt[I] > 1) And (Not Sig[I]) And (Not InWordList(NullWords, Dict[I]))
          Then
        If (Best < 0) Or (Cnt[I] > Cnt[Best]) Then Best := I;
    If Best < 0 Then Break;
    Sig[Best] := True;
  End;
  For I := 0 To SentCount - 1 Do
  Begin
    SplitWords(Sents[I].Text);
    Cue := 0;
    For J := 0 To High(Words) Do
    Begin
      If InWordList(BonusWords, Words[J]) Then Cue := Cue + 1;
      If InWordList(StigmaWords, Words[J]) Then Cue := Cue - 1;
    End;
    Key := 0;
    For J := 0 To High(Sents[I].Terms) Do
    Begin
      Id := Sents[I].Terms[J];
      If Sig[Id] Then Key := Key + 1;
    End;
    Loc := 0;
    If (I = 0) Or (I = SentCount - 1) Then Loc := 1;
    If (I > 0) And (Sents[I].Par <> Sents[I - 1].Par) Then Loc := Loc + 0.5;
    Result[I] := Cue + Key + Loc;
  End;
End;

{ ------------------------------------------------------------------ }
{ Aide et ligne de commande                                           }
{ ------------------------------------------------------------------ }

Procedure ShowHelp;
Begin
  Writeln('SUMY ', Version,
      ' - Cette commande permet de resumer automatiquement du texte.');
  Writeln;
  Writeln('Syntaxe : SUMY algorithme [options]');
  Writeln;
  Writeln('Algorithmes :');
  Writeln('  luhn        mots significatifs groupes dans la phrase (Luhn)');
  Writeln(
      '  edmundson   mots bonus / malus, mots cles et emplacement (Edmundson)');
  Writeln('  lsa         analyse semantique latente (Steinberger-Jezek)');
  Writeln('  text-rank   TextRank : PageRank sur le recouvrement des mots');
  Writeln('  lex-rank    LexRank : PageRank sur la similarite tf-idf');
  Writeln('  sum-basic   SumBasic : probabilite moyenne des mots');
  Writeln('  kl          divergence de Kullback-Leibler');
  Writeln('  reduction   somme des liens entre phrases');
  Writeln('  random      phrases choisies au hasard');
  Writeln;
  Writeln('Options :');
  Writeln(
      '  --length=N         nombre de phrases (ou N% des phrases, defaut 20%)');
  Writeln('  --language=LANGUE  english, french, german, spanish, italian, por'+
      'tuguese,');
  Writeln('                     dutch, czech, slovak (defaut english)');
  Writeln('  --url=ADRESSE      page a resumer (telechargee avec curl)');
  Writeln('  --file=FICHIER     fichier a resumer (- pour l''entree standard)');
  Writeln('  --text=TEXTE       texte a resumer');
  Writeln('  --format=FORMAT    plaintext ou html (defaut : selon la source)');
  Writeln('  --stopwords=FICHIER  liste de mots vides (un mot par ligne)');
  Writeln(
      '  --bonus-words=FICHIER, --stigma-words=FICHIER, --null-words=FICHIER');
  Writeln('                     listes de mots pour edmundson');
  Writeln('  -h, --help, /?     cette aide');
  Writeln('  -V, --version      version du programme');
  Writeln;
  Writeln('Sans source, le texte est lu sur l''entree standard. Les phrases re'+
      'tenues sont');
  Writeln('affichees dans l''ordre du document, en UTF-8 (sous Windows, taper '+
      'd''abord');
  Writeln('CHCP 65001 pour afficher les accents).');
  Writeln;
  Writeln('Exemple : SUMY lex-rank --language=english --length=5 --url=https:/'+
      '/en.wikipedia.org/wiki/Artificial_intelligence');
End;

Function OptionValue(Const Arg, Name: AnsiString; Var Value: AnsiString):
    Boolean;
Begin
  Result := False;
  If Copy(Arg, 1, Length(Name) + 1) = Name + '=' Then
  Begin
    Value := Copy(Arg, Length(Name) + 2, Length(Arg));
    Result := True;
  End;
End;

Function IsAlgorithm(Const S: AnsiString): Boolean;
Begin
  Result := (S = 'luhn') Or (S = 'edmundson') Or (S = 'lsa') Or (S =
      'text-rank') Or
            (S = 'lex-rank') Or (S = 'sum-basic') Or (S = 'kl') Or (S =
                'reduction') Or
            (S = 'random');
End;

Procedure ParseArguments;
Var
  I: LongInt;
  A, V: AnsiString;

  Function TakeValue(Const Name: AnsiString): Boolean;
  Begin
    Result := False;
    If OptionValue(A, Name, V) Then
      Result := True
    Else If A = Name Then
    Begin
      If I < ParamCount Then
      Begin
        Inc(I);
        V := ParamStr(I);
        Result := True;
      End
      Else
      Begin
        Writeln('Erreur : l''option ', Name, ' demande une valeur.');
        Halt(2);
      End;
    End;
  End;

Begin
  Algorithm := '';
  LanguageName := 'english';
  LengthText := '';
  SourceText := '';
  SourceFile := '';
  SourceUrl := '';
  SourceFormat := '';
  If ParamCount = 0 Then
  Begin
    ShowHelp;
    Halt(1);
  End;
  I := 1;
  While I <= ParamCount Do
  Begin
    A := ParamStr(I);
    If (A = '/?') Or (A = '-?') Or (A = '-h') Or (A = '--help') Then
    Begin
      ShowHelp;
      Halt(0);
    End
    Else If (A = '-V') Or (A = '--version') Then
    Begin
      Writeln('SUMY (clone en Pascal) ', Version);
      Halt(0);
    End
    Else If TakeValue('--length') Then LengthText := V
    Else If TakeValue('--language') Then LanguageName := LowerText(V)
    Else If TakeValue('--url') Then SourceUrl := V
    Else If TakeValue('--file') Then SourceFile := V
    Else If TakeValue('--text') Then SourceText := V
    Else If TakeValue('--format') Then SourceFormat := LowerText(V)
    Else If TakeValue('--stopwords') Then StopFile := V
    Else If TakeValue('--bonus-words') Then BonusFile := V
    Else If TakeValue('--stigma-words') Then StigmaFile := V
    Else If TakeValue('--null-words') Then NullFile := V
    Else If Copy(A, 1, 1) = '-' Then
    Begin
      Writeln('Erreur : option non prise en charge : ', A);
      Halt(2);
    End
    Else If (Algorithm = '') And IsAlgorithm(LowerText(A)) Then
      Algorithm := LowerText(A)
    Else
    Begin
      Writeln('Erreur : argument inattendu : ', A);
      Halt(2);
    End;
    Inc(I);
  End;
  If Algorithm = '' Then
  Begin
    Writeln('Erreur : aucun algorithme indique (luhn, lsa, lex-rank, text-rank'+
        ', ...).');
    Halt(2);
  End;
  If (StopFile = '') And (Not KnownLanguage(LanguageName)) Then
  Begin
    Writeln('Erreur : langue non prise en charge : ', LanguageName);
    Halt(2);
  End;
End;

{ ------------------------------------------------------------------ }
{ Programme principal                                                 }
{ ------------------------------------------------------------------ }

Var
  Raw, Doc: AnsiString;
  Ok, IsHtml: Boolean;
  Ratings: TDblArray;
  Chosen: Array Of Boolean;
  Count, I, J, Best: LongInt;
  Ext: AnsiString;

BEGIN
  { les octets sont traites tels quels (UTF-8), sans conversion de page de codes
    }
  DefaultSystemCodePage := CP_UTF8;
  SetTextCodePage(Output, CP_UTF8);
  SetTextCodePage(Input, CP_UTF8);
  ParseArguments;

  IsHtml := (SourceFormat = 'html');
  If SourceUrl <> '' Then
  Begin
    Raw := FetchUrl(SourceUrl, Ok);
    If Not Ok Then
    Begin
      Writeln('Erreur : impossible de telecharger ', SourceUrl,
          ' (la commande curl est requise).');
      Halt(2);
    End;
    If SourceFormat = '' Then IsHtml := True;
  End
  Else If SourceFile <> '' Then
  Begin
    Raw := ReadTextFile(SourceFile, Ok);
    If Not Ok Then
    Begin
      Writeln('Erreur : impossible de lire le fichier ', SourceFile);
      Halt(2);
    End;
    Ext := LowerText(Copy(SourceFile, Length(SourceFile) - 4, 5));
    If (SourceFormat = '') And ((Copy(Ext, 2, 4) = '.htm') Or (Ext = '.html'))
        Then IsHtml := True;
  End
  Else If SourceText <> '' Then
    Raw := SourceText
  Else
  Begin
    Raw := ReadTextFile('-', Ok);
  End;
  If Not IsValidUtf8(Raw) Then Raw := Latin1ToUtf8(Raw);
  If IsHtml Then Doc := HtmlToText(Raw) Else Doc := Raw;

  SplitDocument(Doc);
  If SentCount = 0 Then Halt(0);

  { analyse des phrases }
  SetLength(Dict, 1024);
  DictCount := 0;
  SetLength(HashTab, HashSize);
  For I := 0 To HashSize - 1 Do HashTab[I] := -1;
  LoadStopWords;
  For I := 0 To SentCount - 1 Do Analyze(Sents[I]);

  Count := CountOfLength(LengthText, SentCount);

  If Algorithm = 'luhn' Then Ratings := RankLuhn
  Else If Algorithm = 'edmundson' Then Ratings := RankEdmundson
  Else If Algorithm = 'lsa' Then Ratings := RankLsa
  Else If Algorithm = 'text-rank' Then Ratings := RankTextRank
  Else If Algorithm = 'lex-rank' Then Ratings := RankLexRank
  Else If Algorithm = 'sum-basic' Then Ratings := RankSumBasic(Count)
  Else If Algorithm = 'kl' Then Ratings := RankKL(Count)
  Else If Algorithm = 'reduction' Then Ratings := RankReduction
  Else Ratings := RankRandom;

  { meilleures phrases : note decroissante, a note egale la plus ancienne
    d'abord }
  SetLength(Chosen, SentCount);
  For I := 0 To SentCount - 1 Do Chosen[I] := False;
  For J := 1 To Count Do
  Begin
    Best := -1;
    For I := 0 To SentCount - 1 Do
      If Not Chosen[I] Then
        If (Best < 0) Or (Ratings[I] > Ratings[Best]) Then Best := I;
    If Best < 0 Then Break;
    Chosen[Best] := True;
  End;
  For I := 0 To SentCount - 1 Do
    If Chosen[I] Then Writeln(Sents[I].Text);
END.
