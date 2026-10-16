{ @author: Sylvain Maltais (support@gladir.com)
  @created: 2026
  @website(https://www.gladir.com/OCR-0)
  @abstract(Target: Turbo Pascal 7, Free Pascal 3.2)
  @description: Clone de la commande lingua-cli (detection de la langue du texte
                passe en parametre : lingua-cli "Bonjour, comment allez-vous ?")

  Principe : le texte est decode (UTF-8, UTF-16 ou Latin-1), les lettres sont
  comptees par ecriture (latin, grec, cyrillique, arabe, hebreu, thai, devanagari,
  kana, han, hangul). Les ecritures non latines donnent directement la langue
  (sauf le cyrillique, departage par les mots les plus frequents). Pour les
  langues a ecriture latine ou cyrillique, chaque mot est cherche dans une table
  des mots les plus frequents de chaque langue, avec un bonus pour les lettres
  caracteristiques ; la confiance de chaque langue est son score normalise.
}

Program LINGUACLI;


Const
  Version = '1.0';
  MaxText = 32000;
  LanguageCount = 21;      { langues departagees par les mots }
  HashSize = 4099;
  MaxEntries = 2100;

  { Rang de chaque langue : bit du masque associe a un mot. }
  LangCodes: Array[0..20] Of String[2] = (
    'en', 'fr', 'de', 'es', 'pt', 'it', 'nl', 'sv', 'da', 'no', 'fi',
    'pl', 'cs', 'ro', 'hu', 'tr', 'et', 'is', 'ru', 'uk', 'bg');

  { Ecritures. }
  ScriptLatin = 1;
  ScriptGreek = 2;
  ScriptCyrillic = 3;
  ScriptArabic = 4;
  ScriptHebrew = 5;
  ScriptThai = 6;
  ScriptDevanagari = 7;
  ScriptKana = 8;
  ScriptHan = 9;
  ScriptHangul = 10;

  { Mots les plus frequents : 'code mot mot mot ...' (UTF-8, minuscules). }
  WordLists: Array[0..65] Of String = (
    'en the of and to a in is that it was for on are with as his they at be this from have or by one had not but what all were when we there can an your which their said if do will each about how up out them then she many',
    'en some so these would other into has more her two like him see could no make than been its who now my did only may very after where most also much before any our me just such here because over between both those while',
    'en without during don isn didn doesn s t',
    'fr le la les de des du un une et est en que qui dans pour pas sur au aux avec ce cette ces il elle ils elles nous vous je tu on ne se son sa ses leur leurs mais ou donc car ni plus par comme sont ont été être avoir',
    'fr fait faire aussi tout tous bien très sans sous entre encore alors même autre où quand dont y a à c qu l d j n m t s lui eux moi toi cet cela ça ainsi après avant depuis chez vers peut peuvent mon ma mes ton ta',
    'fr tes notre votre nos vos était étaient sera seront celui celle ceux',
    'de der die das und ist in den von zu mit sich des auf für nicht ein eine einer einem einen als auch es an werden aus er hat daß dass sie nach wird bei um am sind noch wie über so zum kann nur sein ihre oder aber vor',
    'de zur bis mehr durch man dann soll sondern diese wenn dem im ich du wir ihr uns euch war waren wurde wurden hatte haben habe ihn ihm ihnen kein keine schon sehr hier da wo was wer warum weil immer nun muss müssen',
    'de können konnte',
    'es el la los las de del un una unos unas y o en que es por con para se su sus al lo como más pero sin sobre este esta estos estas entre cuando muy ya todo también fue han ha son ser está están hay yo tú él ella',
    'es nosotros ellos ellas me te le nos les mi mis tu tus ese esa eso así porque donde quien qué cual todos todas otro otra otros desde hasta durante tiene tienen había hacer puede pueden a e',
    'pt o a os as de do da dos das um uma uns umas e em no na nos nas por para com que se não mais como mas foi ao aos ele ela eles elas seu sua seus suas ou quando muito já também só pelo pela pelos pelas até isso esse',
    'pt essa este esta estes estas entre depois sem mesmo ter tem têm são está estão há eu tu nós vocês me te lhe lhes meu minha onde quem qual todos tudo outro outra ser era fazer pode',
    'it il lo la i gli le di del dello della dei degli delle un uno una e è in che con per su da non si come più ma al allo alla ai agli alle nel nello nella nei negli nelle sono ha ho hanno anche se o ci questo questa',
    'it questi queste quello quella io tu lui lei noi voi loro mi ti ne molto tutto tutti essere stato era fatto fare dove quando perché cosa chi l dell po',
    'nl de het een en van in is dat op te zijn voor met niet aan er maar om ook als dan bij of uit door over ze zich nog naar tot wordt worden werd heeft hebben had hij zij wij jij ik je mijn haar hun deze die dit wat wie',
    'nl waar hoe dus meer veel al wel geen kan kunnen moet zou nu toch onder tegen na tussen',
    'sv och i att det som en på är av för med till den har de inte om ett men var jag sig från vi så kan man när år hans där blir ska hon eller mycket sin nu alla efter vid över under ut bara utan mot också dem ha',
    'sv hade då här vara denna detta dessa vad vem hur vilken sedan skulle kunde blev',
    'da jeg jer os hvem hvorfor dejligt godt',
    'da og i at det en den til er som på de med han af for ikke der var mig sig men et har om vi min havde ham hun nu over da fra du ud sin dem os op man hans hvor eller hvad skal selv her alle vil blev kunne ind når være',
    'da dog noget ville jo deres efter ned skulle denne end dette mit også under have dig anden hende mine alt meget sit sine vor mod disse hvis din nogle hos blive mange ad bliver',
    'no og i det er som en på til av for med at ikke den har de om et var jeg han men fra vi kan så å skal ble eller hva også nå ut seg meg deg henne ham dem noe mye hun vært sin sitt sine hvor når bare etter over',
    'no under mot ved ingen disse denne dette alle ville kunne måtte uten',
    'fi ja on ei että se oli hän mutta kun ovat tai olen sen joka kuin jos niin mitä vain myös tämä nyt kanssa sitä ole minä sinä me te he mikä ne ollut ennen jo vielä sekä mukaan koska kuten yli tässä siitä',
    'fi hänen heidän',
    'pl i w nie na się z to że do jest o jak ale po co tak za od przez dla czy ten ta te tego jego jej ich są był była było być będzie lub oraz tylko już jeszcze bardzo może ma mają tym tej tych który która',
    'pl które gdy kiedy gdzie więc też',
    'cs a v je se na že to s z do o k i ale jako by si které který která co tak za od pro po u jsou byl byla bylo být bude nebo také už jen ještě velmi může má mají tím této tento tyto jeho její jejich když',
    'cs kde proto',
    'ro și în de la a cu pe că este un o nu se mai din pentru care sau sunt dar ca ce al lui fi fost au ale va i am să cel cea acest această acestei acești mult foarte dacă când unde cine',
    'hu a az és hogy nem is egy de van volt meg már csak még el ki be fel le mint vagy ez ezt azt amely mert után között ahogy lehet kell minden nagy két mi te ő ők én ön',
    'tr ve bir bu da de için ile ne o mi çok daha gibi ama ben sen biz siz onlar var yok olarak kadar sonra ancak ise her şey değil olan olduğu',
    'et ja on ei et see oli kui ta aga ka või mis siis nii veel ning kes mida kuid oma ma sa me te nad üks selle seda sellel',
    'is og í á að er það sem hann ekki var en til með um frá af við hún þá sig þeir fyrir eða eftir hafa hefur verið mjög þetta þessi',
    'ru и в не на я что он с как а то все она так его но да ты к у же вы за бы по только ее мне было вот от меня еще нет о из ему',
    'ru теперь когда даже ну вдруг ли если уже или ни быть был него до вас нибудь опять уж вам сказал ведь там потом себя ничего',
    'ru ей может они тут где есть надо ней для мы тебя их чем была сам чтоб без будто человек чего раз тоже себе под жизнь будет',
    'ru ж тогда кто этот говорил того потому этого какой совсем ним здесь этом один почти мой тем чтобы нее кажется сейчас были',
    'ru куда зачем сказать всех никогда сегодня можно при наконец два об другой хоть после над больше тот через эти нас про',
    'ru всего них какая много разве сказала три эту моя впрочем хорошо свою этой перед иногда лучше чуть том нельзя такой им',
    'ru более всегда конечно всю между',
    'uk і в не на я що він з як а то все вона так його але та ти до у же ви за би по тільки її мені було ось від мене ще ні про із',
    'uk йому тепер коли навіть ну раптом чи якщо вже або бути був нього вас знову уже вам сказав адже там потім себе нічого їй',
    'uk може вони тут де є треба нею для ми тебе їх чим була сам щоб без ніби людина чого раз теж собі під життя буде ж тоді хто',
    'uk цей говорив того тому цього який зовсім ним цьому один майже мій тим неї здається зараз були куди навіщо сказати всіх',
    'uk ніколи сьогодні можна при нарешті два інший хоч після над більше той через ці нас всього них яка багато невже сказала',
    'uk три цю моя проте добре свою цій перед іноді краще трохи такий їм більш завжди звичайно всю між',
    'bg и в не на че се да е от за с по като но той тя то те ние вие аз ти му й ги ще бе са съм беше бил била били този тази това тези',
    'bg още вече само кога къде как защо какво кой коя което ако или',
    'en hello hi world thanks thank please yes you your are am i my me we us he she good morning day night welcome sorry',
    'fr bonjour salut merci monde oui non plaisir madame monsieur bonsoir bonne nuit jour',
    'de hallo danke bitte welt ja nein guten tag morgen abend nacht herr frau gut',
    'es hola gracias favor mundo sí no buenos días tardes noches señor señora bueno buena',
    'pt obrigado obrigada olá oi mundo sim não bom bons dia noite boa senhor senhora favor',
    'it ciao grazie prego mondo sì no buongiorno buonasera buonanotte signore signora bene',
    'nl hallo dank dankjewel alsjeblieft wereld ja nee goedemorgen goedenavond mijnheer mevrouw goed',
    'sv hej tack världen ja nej god morgon kväll natt herr fru bra',
    'da hej tak verden ja nej godmorgen aften nat hr fru godt',
    'no hei takk verden ja nei god morgen kveld natt herr fru bra',
    'fi hei kiitos maailma kyllä hyvää huomenta päivää iltaa yötä',
    'pl cześć dziękuję proszę świat tak nie dzień dobry wieczór noc pan pani',
    'cs ahoj děkuji prosím svět ano ne dobrý den večer noc pan paní',
    'ro bună mulțumesc vă rog lume da nu ziua seara noapte domnul doamna bine',
    'hu szia köszönöm kérem világ igen nem jó napot estét éjszakát úr hölgy',
    'tr merhaba selam teşekkürler lütfen dünya evet hayır günaydın iyi akşamlar geceler'
  );

  { Lettres caracteristiques de chaque langue (points de code, minuscules). }
  SpecialCount: Array[0..20] Of Integer = (0, 13, 4, 8, 10, 6, 2, 3, 3, 3, 2, 9, 13, 7, 9, 7, 3, 10, 4, 4, 1);
  SpecialLetters: Array[0..20, 0..12] Of Word = (
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (224, 226, 231, 232, 233, 234, 235, 238, 239, 244, 249, 251, 339),
    (223, 228, 246, 252, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (241, 225, 233, 237, 243, 250, 191, 161, 0, 0, 0, 0, 0),
    (227, 245, 231, 225, 226, 234, 243, 244, 250, 224, 0, 0, 0),
    (224, 232, 233, 236, 242, 249, 0, 0, 0, 0, 0, 0, 0),
    (235, 239, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (229, 228, 246, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (230, 248, 229, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (230, 248, 229, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (228, 246, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (261, 263, 281, 322, 324, 243, 347, 378, 380, 0, 0, 0, 0),
    (345, 283, 353, 269, 382, 253, 367, 271, 357, 328, 225, 237, 250),
    (259, 537, 539, 238, 226, 351, 355, 0, 0, 0, 0, 0, 0),
    (337, 369, 225, 233, 237, 243, 246, 250, 252, 0, 0, 0, 0),
    (287, 305, 351, 231, 246, 252, 0, 0, 0, 0, 0, 0, 0),
    (245, 228, 246, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (240, 254, 225, 233, 237, 243, 250, 253, 230, 246, 0, 0, 0),
    (1099, 1101, 1098, 1105, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (1110, 1111, 1108, 1169, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (1098, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0));

  AllCodes: Array[0..28] Of String[2] = (
    'en', 'fr', 'de', 'es', 'pt', 'it', 'nl', 'sv', 'da', 'no', 'fi', 'pl', 'cs', 'ro',
    'hu', 'tr', 'et', 'is', 'ru', 'uk', 'bg', 'el', 'ar', 'he', 'th', 'hi', 'ja', 'zh', 'ko');
  AllNames: Array[0..28] Of String[11] = (
    'English', 'French', 'German', 'Spanish', 'Portuguese', 'Italian', 'Dutch',
    'Swedish', 'Danish', 'Norwegian', 'Finnish', 'Polish', 'Czech', 'Romanian',
    'Hungarian', 'Turkish', 'Estonian', 'Icelandic', 'Russian', 'Ukrainian',
    'Bulgarian', 'Greek', 'Arabic', 'Hebrew', 'Thai', 'Hindi', 'Japanese',
    'Chinese', 'Korean');

Type
  TEntry = Record
    Text: String[23];
    Mask: Longint;
  End;
  TEntries = Array[0..MaxEntries - 1] Of TEntry;
  PEntries = ^TEntries;
  TTextBuffer = Array[0..32767] Of Byte;
  PTextBuffer = ^TTextBuffer;

Var
  Entries: PEntries;
  EntryCount: Integer;
  HashTable: Array[0..HashSize - 1] Of Integer;

  InputFile: File;
  TextBuf: PTextBuffer;
  MemoryLength, MemoryPos: Word;
  UseMemory: Boolean;
  SourceName: String;
  Confidence: Array[0..28] Of Real;
  Buffer: Array[0..4095] Of Byte;
  BufferPos, BufferLen: Word;

  Encoding: Integer;           { 0 = Latin-1, 1 = UTF-8, 2 = UTF-16 LE, 3 = UTF-16 BE }
  ScriptCount: Array[1..10] Of Longint;
  Hits: Array[0..20] Of Longint;
  SpecialHits: Array[0..20] Of Longint;
  TokenCount: Longint;
  CurrentWord: String;

  ArgumentIndex: Integer;

{ ------------------------------------------------------------------ }
{ Table des mots                                                      }
{ ------------------------------------------------------------------ }

Function HashOf(Const S: String): Integer;
Var
  I: Integer;
  H: Longint;
Begin
  H := 0;
  For I := 1 To Length(S) Do
    H := (H * 31 + Ord(S[I])) Mod HashSize;
  HashOf := H;
End;

Function FindEntry(Const S: String): Integer;
Var
  H, Count: Integer;
Begin
  H := HashOf(S);
  Count := 0;
  While (HashTable[H] >= 0) And (Count < HashSize) Do
  Begin
    If Entries^[HashTable[H]].Text = S Then
    Begin
      FindEntry := HashTable[H];
      Exit;
    End;
    Inc(H);
    If H >= HashSize Then H := 0;
    Inc(Count);
  End;
  FindEntry := -1;
End;

Procedure AddWord(Const S: String; Language: Integer);
Var
  Index, H: Integer;
Begin
  If (Length(S) = 0) Or (Length(S) > 23) Then Exit;
  Index := FindEntry(S);
  If Index >= 0 Then
  Begin
    Entries^[Index].Mask := Entries^[Index].Mask Or (Longint(1) Shl Language);
    Exit;
  End;
  If EntryCount >= MaxEntries Then Exit;
  H := HashOf(S);
  While HashTable[H] >= 0 Do
  Begin
    Inc(H);
    If H >= HashSize Then H := 0;
  End;
  Entries^[EntryCount].Text := S;
  Entries^[EntryCount].Mask := Longint(1) Shl Language;
  HashTable[H] := EntryCount;
  Inc(EntryCount);
End;

Function LanguageIndex(Const Code: String): Integer;
Var
  I: Integer;
Begin
  LanguageIndex := -1;
  For I := 0 To LanguageCount - 1 Do
    If LangCodes[I] = Code Then LanguageIndex := I;
End;

Procedure BuildTable;
Var
  I, Position, Language: Integer;
  Line, Item: String;
Begin
  GetMem(Entries, SizeOf(TEntries));
  FillChar(HashTable, SizeOf(HashTable), $FF);
  EntryCount := 0;
  For I := 0 To High(WordLists) Do
  Begin
    Line := WordLists[I];
    If Length(Line) > 2 Then
    Begin
      Language := LanguageIndex(Copy(Line, 1, 2));
      Delete(Line, 1, 3);
      If Language >= 0 Then
      Begin
        Line := Line + ' ';
        Position := Pos(' ', Line);
        While Position > 0 Do
        Begin
          Item := Copy(Line, 1, Position - 1);
          Delete(Line, 1, Position);
          If Item <> '' Then AddWord(Item, Language);
          Position := Pos(' ', Line);
        End;
      End;
    End;
  End;
End;

Function Lookup(Const S: String): Longint;
Var
  Index: Integer;
Begin
  Index := FindEntry(S);
  If Index >= 0 Then
    Lookup := Entries^[Index].Mask
  Else
    Lookup := 0;
End;

{ ------------------------------------------------------------------ }
{ Caracteres : minuscules et ecritures                                }
{ ------------------------------------------------------------------ }

Function ToLowerCode(Code: Longint): Longint;
Begin
  If (Code >= 65) And (Code <= 90) Then
    Code := Code + 32
  Else If (Code >= 192) And (Code <= 222) And (Code <> 215) Then
    Code := Code + 32
  Else If (Code >= 256) And (Code <= 311) Then
  Begin
    If (Code And 1) = 0 Then Inc(Code);
  End
  Else If (Code >= 313) And (Code <= 328) Then
  Begin
    If (Code And 1) = 1 Then Inc(Code);
  End
  Else If (Code >= 330) And (Code <= 375) Then
  Begin
    If (Code And 1) = 0 Then Inc(Code);
  End
  Else If Code = 376 Then
    Code := 255
  Else If (Code >= 377) And (Code <= 382) Then
  Begin
    If (Code And 1) = 1 Then Inc(Code);
  End
  Else If (Code = 536) Or (Code = 538) Then
    Inc(Code)
  Else If (Code >= 913) And (Code <= 939) And (Code <> 930) Then
    Code := Code + 32
  Else If (Code >= 1024) And (Code <= 1039) Then
    Code := Code + 80
  Else If (Code >= 1040) And (Code <= 1071) Then
    Code := Code + 32
  Else If (Code >= 1120) And (Code <= 1215) And ((Code And 1) = 0) Then
    Inc(Code);
  ToLowerCode := Code;
End;

{ 0 = pas une lettre. }
Function ScriptOf(Code: Longint): Integer;
Begin
  ScriptOf := 0;
  If ((Code >= 97) And (Code <= 122)) Or ((Code >= 192) And (Code <= 591) And
     (Code <> 215) And (Code <> 247)) Or (Code = 170) Or (Code = 186) Or
     ((Code >= 7680) And (Code <= 7935)) Then
    ScriptOf := ScriptLatin
  Else If (Code >= 880) And (Code <= 1023) Then
    ScriptOf := ScriptGreek
  Else If (Code >= 1024) And (Code <= 1327) Then
    ScriptOf := ScriptCyrillic
  Else If ((Code >= 1536) And (Code <= 1791)) Or ((Code >= 1872) And (Code <= 1919)) Or
          ((Code >= 64336) And (Code <= 65279)) Then
    ScriptOf := ScriptArabic
  Else If (Code >= 1425) And (Code <= 1535) Then
    ScriptOf := ScriptHebrew
  Else If (Code >= 3585) And (Code <= 3675) Then
    ScriptOf := ScriptThai
  Else If (Code >= 2304) And (Code <= 2431) Then
    ScriptOf := ScriptDevanagari
  Else If ((Code >= 12353) And (Code <= 12447)) Or ((Code >= 12449) And (Code <= 12543)) Then
    ScriptOf := ScriptKana
  Else If ((Code >= 19968) And (Code <= 40959)) Or ((Code >= 13312) And (Code <= 19903)) Then
    ScriptOf := ScriptHan
  Else If ((Code >= 44032) And (Code <= 55215)) Or ((Code >= 4352) And (Code <= 4607)) Or
          ((Code >= 12593) And (Code <= 12686)) Then
    ScriptOf := ScriptHangul;
End;

Procedure AppendUtf8(Var S: String; Code: Longint);
Begin
  If Length(S) > 24 Then Exit;
  If Code < 128 Then
    S := S + Chr(Code)
  Else If Code < 2048 Then
    S := S + Chr($C0 Or (Code Shr 6)) + Chr($80 Or (Code And $3F))
  Else
    S := S + Chr($E0 Or (Code Shr 12)) + Chr($80 Or ((Code Shr 6) And $3F)) +
         Chr($80 Or (Code And $3F));
End;

{ ------------------------------------------------------------------ }
{ Analyse du texte                                                    }
{ ------------------------------------------------------------------ }

Procedure EndWord;
Var
  Mask: Longint;
  I: Integer;
Begin
  If CurrentWord <> '' Then
  Begin
    Inc(TokenCount);
    Mask := Lookup(CurrentWord);
    If Mask <> 0 Then
      For I := 0 To LanguageCount - 1 Do
        If (Mask And (Longint(1) Shl I)) <> 0 Then Inc(Hits[I]);
    CurrentWord := '';
  End;
End;

Procedure FeedCode(Code: Longint);
Var
  Script, I, J: Integer;
Begin
  Code := ToLowerCode(Code);
  For I := 0 To LanguageCount - 1 Do
    For J := 0 To SpecialCount[I] - 1 Do
      If SpecialLetters[I, J] = Code Then Inc(SpecialHits[I]);
  Script := ScriptOf(Code);
  If Script = 0 Then
    EndWord
  Else
  Begin
    Inc(ScriptCount[Script]);
    If Script <= ScriptCyrillic Then
      AppendUtf8(CurrentWord, Code)
    Else
      EndWord;
  End;
End;

Function ReadByte: Integer;
Begin
  If UseMemory Then
  Begin
    If MemoryPos < MemoryLength Then
    Begin
      ReadByte := TextBuf^[MemoryPos];
      Inc(MemoryPos);
    End
    Else
      ReadByte := -1;
    Exit;
  End;
  If BufferPos >= BufferLen Then
  Begin
    {$I-}
    BlockRead(InputFile, Buffer, SizeOf(Buffer), BufferLen);
    {$I+}
    If IOResult <> 0 Then BufferLen := 0;
    BufferPos := 0;
    If BufferLen = 0 Then
    Begin
      ReadByte := -1;
      Exit;
    End;
  End;
  ReadByte := Buffer[BufferPos];
  Inc(BufferPos);
End;

Procedure CloseInput;
Begin
  If Not UseMemory Then Close(InputFile);
End;

Procedure ResetInput(Const Name: String);
Begin
  BufferPos := 0;
  BufferLen := 0;
  MemoryPos := 0;
  If UseMemory Then Exit;
  Assign(InputFile, Name);
  {$I-}
  Reset(InputFile, 1);
  {$I+}
  If IOResult <> 0 Then
  Begin
    Writeln('Erreur : impossible de lire le fichier ', Name);
    Halt(1);
  End;
End;

{ Premiere lecture : determine l'encodage (BOM, sinon UTF-8 valide ou Latin-1). }
Function DetectEncoding(Const Name: String): Integer;
Var
  B, First, Second, Expected: Integer;
  Valid, Seen: Boolean;
Begin
  ResetInput(Name);
  First := ReadByte;
  Second := ReadByte;
  If (First = $FF) And (Second = $FE) Then
  Begin
    DetectEncoding := 2;
    CloseInput;
    Exit;
  End;
  If (First = $FE) And (Second = $FF) Then
  Begin
    DetectEncoding := 3;
    CloseInput;
    Exit;
  End;
  CloseInput;
  ResetInput(Name);
  Valid := True;
  Seen := False;
  Expected := 0;
  B := ReadByte;
  While (B >= 0) And Valid Do
  Begin
    If Expected > 0 Then
    Begin
      If (B And $C0) = $80 Then
        Dec(Expected)
      Else
        Valid := False;
    End
    Else If B >= $F8 Then
      Valid := False
    Else If B >= $F0 Then
    Begin
      Expected := 3;
      Seen := True;
    End
    Else If B >= $E0 Then
    Begin
      Expected := 2;
      Seen := True;
    End
    Else If B >= $C2 Then
    Begin
      Expected := 1;
      Seen := True;
    End
    Else If B >= $80 Then
      Valid := False;
    B := ReadByte;
  End;
  CloseInput;
  If Valid And (Expected = 0) Then
    DetectEncoding := 1
  Else
    DetectEncoding := 0;
  If Not Seen Then DetectEncoding := 1;
End;

Procedure AnalyzeFile(Const Name: String);
Var
  B, B2, B3, B4: Integer;
  Code, Low, High: Longint;
Begin
  Encoding := DetectEncoding(Name);
  ResetInput(Name);
  CurrentWord := '';
  Case Encoding Of
    2, 3:
    Begin
      B := ReadByte;
      B := ReadByte;
      While True Do
      Begin
        B := ReadByte;
        B2 := ReadByte;
        If (B < 0) Or (B2 < 0) Then Break;
        If Encoding = 2 Then
          Code := B + B2 * 256
        Else
          Code := B * 256 + B2;
        If (Code >= $D800) And (Code <= $DBFF) Then
        Begin
          High := Code;
          B := ReadByte;
          B2 := ReadByte;
          If (B < 0) Or (B2 < 0) Then Break;
          If Encoding = 2 Then
            Low := B + B2 * 256
          Else
            Low := B * 256 + B2;
          Code := $10000 + ((High - $D800) Shl 10) + (Low - $DC00);
        End;
        FeedCode(Code);
      End;
    End;
    1:
    Begin
      B := ReadByte;
      If B = $EF Then
      Begin
        B2 := ReadByte;
        B3 := ReadByte;
        If Not ((B2 = $BB) And (B3 = $BF)) Then
        Begin
          { pas un BOM : on traite les trois octets lus }
          FeedCode(((B And $0F) Shl 12) Or ((B2 And $3F) Shl 6) Or (B3 And $3F));
        End;
        B := ReadByte;
      End;
      While B >= 0 Do
      Begin
        If B < $80 Then
          Code := B
        Else If B >= $F0 Then
        Begin
          B2 := ReadByte;
          B3 := ReadByte;
          B4 := ReadByte;
          Code := ((Longint(B) And $07) Shl 18) Or ((Longint(B2) And $3F) Shl 12) Or
                  ((Longint(B3) And $3F) Shl 6) Or (Longint(B4) And $3F);
        End
        Else If B >= $E0 Then
        Begin
          B2 := ReadByte;
          B3 := ReadByte;
          Code := ((Longint(B) And $0F) Shl 12) Or ((Longint(B2) And $3F) Shl 6) Or
                  (Longint(B3) And $3F);
        End
        Else
        Begin
          B2 := ReadByte;
          Code := ((Longint(B) And $1F) Shl 6) Or (Longint(B2) And $3F);
        End;
        FeedCode(Code);
        B := ReadByte;
      End;
    End;
  Else
    B := ReadByte;
    While B >= 0 Do
    Begin
      FeedCode(B);
      B := ReadByte;
    End;
  End;
  EndWord;
  CloseInput;
End;

{ ------------------------------------------------------------------ }
{ Decision : confiance de chaque langue                               }
{ ------------------------------------------------------------------ }

{ Calcule Confidence[0..28] (somme = 1) et renvoie l'indice de la meilleure
  langue, ou -1 si le texte n'a pas assez de lettres. }
Function Decide: Integer;
Var
  Total, Cjk: Longint;
  Script, Dominant, I, First, Last, Single: Integer;
  Score, Sum: Real;
Begin
  For I := 0 To 28 Do
    Confidence[I] := 0;
  Decide := -1;
  Total := 0;
  Dominant := 1;
  For Script := 1 To 10 Do
  Begin
    Inc(Total, ScriptCount[Script]);
    If ScriptCount[Script] > ScriptCount[Dominant] Then Dominant := Script;
  End;
  If Total = 0 Then Exit;
  Single := -1;
  Cjk := ScriptCount[ScriptKana] + ScriptCount[ScriptHan] + ScriptCount[ScriptHangul];
  If Cjk * 100 >= Total * 30 Then
  Begin
    If ScriptCount[ScriptHangul] > ScriptCount[ScriptKana] + ScriptCount[ScriptHan] Then
      Single := 28
    Else If ScriptCount[ScriptKana] > 0 Then
      Single := 26
    Else
      Single := 27;
  End
  Else
    Case Dominant Of
      ScriptGreek: Single := 21;
      ScriptArabic: Single := 22;
      ScriptHebrew: Single := 23;
      ScriptThai: Single := 24;
      ScriptDevanagari: Single := 25;
      ScriptKana: Single := 26;
      ScriptHan: Single := 27;
      ScriptHangul: Single := 28;
    End;
  If Single >= 0 Then
  Begin
    Confidence[Single] := 1;
    Decide := Single;
    Exit;
  End;

  If Dominant = ScriptCyrillic Then
  Begin
    First := 18;
    Last := 20;
  End
  Else
  Begin
    First := 0;
    Last := 17;
  End;
  If TokenCount < 1 Then TokenCount := 1;
  Sum := 0;
  For I := First To Last Do
  Begin
    Score := (Hits[I] + 0.25 * SpecialHits[I]) / TokenCount;
    Confidence[I] := Score * Score;
    Sum := Sum + Confidence[I];
  End;
  If Sum <= 0.0009 Then
  Begin
    For I := 0 To 28 Do
      Confidence[I] := 0;
    Exit;
  End;
  Single := First;
  For I := First To Last Do
  Begin
    Confidence[I] := Confidence[I] / Sum;
    If Confidence[I] > Confidence[Single] Then Single := I;
  End;
  Decide := Single;
End;

Function DetectLanguage: Integer;
Begin
  FillChar(ScriptCount, SizeOf(ScriptCount), 0);
  FillChar(Hits, SizeOf(Hits), 0);
  FillChar(SpecialHits, SizeOf(SpecialHits), 0);
  TokenCount := 0;
  AnalyzeFile(SourceName);
  DetectLanguage := Decide;
End;

{ ------------------------------------------------------------------ }
{ Ligne de commande                                                   }
{ ------------------------------------------------------------------ }

{ ------------------------------------------------------------------ }
{ Arguments : lus par ParamStr (page de code de la console). Pour du texte hors  }
{ de cette page de code (cyrillique, japonais, ...), utiliser -f fichier ou      }
{ l'entree standard, qui conservent l'UTF-8.                                     }
{ ------------------------------------------------------------------ }

Var
  Args: Array[0..63] Of String;
  ArgTotal: Integer;

Function Utf8Of(Code: Longint): String;
Begin
  If Code < 128 Then
    Utf8Of := Chr(Code)
  Else If Code < 2048 Then
    Utf8Of := Chr($C0 Or (Code Shr 6)) + Chr($80 Or (Code And $3F))
  Else If Code < 65536 Then
    Utf8Of := Chr($E0 Or (Code Shr 12)) + Chr($80 Or ((Code Shr 6) And $3F)) +
              Chr($80 Or (Code And $3F))
  Else
    Utf8Of := Chr($F0 Or (Code Shr 18)) + Chr($80 Or ((Code Shr 12) And $3F)) +
              Chr($80 Or ((Code Shr 6) And $3F)) + Chr($80 Or (Code And $3F));
End;

Procedure CollectArguments;
Var
  I: Integer;
Begin
  ArgTotal := ParamCount;
  For I := 1 To ParamCount Do
    If I < 64 Then Args[I] := ParamStr(I);
End;

Procedure ShowHelp;
Begin
  Writeln('LINGUA-CLI - Cette commande permet de detecter la langue d''un texte.');
  Writeln;
  Writeln('Syntaxe :');
  Writeln('  LINGUA-CLI [options] "texte"');
  Writeln('  LINGUA-CLI [options] -f fichier');
  Writeln('  LINGUA-CLI /?');
  Writeln;
  Writeln('Parametres :');
  Writeln('  texte            Texte dont on veut connaitre la langue. Sans texte ni');
  Writeln('                   fichier, le texte est lu sur l''entree standard.');
  Writeln('  -f, --file       Lit le texte dans un fichier (UTF-8, UTF-16 ou Latin-1).');
  Writeln('  -c, --code       Affiche le code ISO 639-1 (fr) au lieu du nom (French).');
  Writeln('  -a, --all        Affiche la confiance de chaque langue candidate (0 a 1),');
  Writeln('                   de la plus probable a la moins probable.');
  Writeln('  -n, --top N      Avec --all, limite l''affichage aux N premieres langues.');
  Writeln('  -V, --version    Affiche la version.');
  Writeln('  -h, --help, /?   Affiche cette aide.');
  Writeln;
  Writeln('Langues : anglais, francais, allemand, espagnol, portugais, italien,');
  Writeln('neerlandais, suedois, danois, norvegien, finnois, polonais, tcheque, roumain,');
  Writeln('hongrois, turc, estonien, islandais, russe, ukrainien, bulgare, grec, arabe,');
  Writeln('hebreu, thai, hindi, japonais, chinois et coreen. Le resultat est Unknown si');
  Writeln('le texte ne permet pas de decider.');
  Writeln;
  Writeln('Exemple :');
  Writeln('  LINGUA-CLI "Bonjour, comment allez-vous ?"');
End;

Procedure ReadStandardInput;
Var
  Ch: Char;
Begin
  While (MemoryLength < MaxText) And (Not Eof(Input)) Do
  Begin
    Read(Input, Ch);
    TextBuf^[MemoryLength] := Ord(Ch);
    Inc(MemoryLength);
  End;
End;

Procedure AddArgumentText(Const S: String);
Var
  I: Integer;
Begin
  If MemoryLength > 0 Then
  Begin
    TextBuf^[MemoryLength] := 32;
    Inc(MemoryLength);
  End;
  For I := 1 To Length(S) Do
    If MemoryLength < MaxText Then
    Begin
      TextBuf^[MemoryLength] := Ord(S[I]);
      Inc(MemoryLength);
    End;
End;

Function Rank(I: Integer): Integer;
Var
  J, Count: Integer;
Begin
  Count := 0;
  For J := 0 To 28 Do
    If (Confidence[J] > Confidence[I]) Or ((Confidence[J] = Confidence[I]) And (J < I)) Then
      Inc(Count);
  Rank := Count;
End;

Var
  Best, Shown, Position, ErrorCode: Integer;
  PrintCode, PrintAll, ReadFromFile: Boolean;
  TopCount: Integer;
  Argument: String;
  Text1: String;

BEGIN
  PrintCode := False;
  PrintAll := False;
  ReadFromFile := False;
  TopCount := 29;
  CollectArguments;
  SourceName := '';
  MemoryLength := 0;
  GetMem(TextBuf, MaxText + 1);
  ArgumentIndex := 1;
  While ArgumentIndex <= ArgTotal Do
  Begin
    Argument := Args[ArgumentIndex];
    If (Argument = '/?') Or (Argument = '-?') Or (Argument = '-h') Or (Argument = '--help') Then
    Begin
      ShowHelp;
      Halt(0);
    End
    Else If (Argument = '-V') Or (Argument = '--version') Then
    Begin
      Writeln('LINGUA-CLI (clone en Pascal) ', Version);
      Halt(0);
    End
    Else If (Argument = '-c') Or (Argument = '--code') Then
      PrintCode := True
    Else If (Argument = '-a') Or (Argument = '--all') Then
      PrintAll := True
    Else If (Argument = '-n') Or (Argument = '--top') Then
    Begin
      If ArgumentIndex = ArgTotal Then
      Begin
        Writeln('Erreur : nombre manquant apres ', Argument);
        Halt(2);
      End;
      Inc(ArgumentIndex);
      Val(Args[ArgumentIndex], TopCount, ErrorCode);
      If (ErrorCode <> 0) Or (TopCount < 1) Then
      Begin
        Writeln('Erreur : nombre invalide : ', Args[ArgumentIndex]);
        Halt(2);
      End;
    End
    Else If (Argument = '-f') Or (Argument = '--file') Then
    Begin
      If ArgumentIndex = ArgTotal Then
      Begin
        Writeln('Erreur : nom de fichier manquant apres ', Argument);
        Halt(2);
      End;
      Inc(ArgumentIndex);
      SourceName := Args[ArgumentIndex];
      ReadFromFile := True;
    End
    Else If (Copy(Argument, 1, 1) = '-') And (Length(Argument) > 1) Then
    Begin
      Writeln('Erreur : option non prise en charge : ', Argument);
      Halt(2);
    End
    Else
      AddArgumentText(Argument);
    Inc(ArgumentIndex);
  End;

  If ReadFromFile Then
    UseMemory := False
  Else
  Begin
    UseMemory := True;
    If MemoryLength = 0 Then ReadStandardInput;
  End;

  BuildTable;
  Best := DetectLanguage;

  If PrintAll Then
  Begin
    Shown := 0;
    For Position := 0 To 28 Do
      For ArgumentIndex := 0 To 28 Do
        If (Rank(ArgumentIndex) = Position) And (Confidence[ArgumentIndex] > 0) And
           (Shown < TopCount) Then
        Begin
          If PrintCode Then
            Text1 := AllCodes[ArgumentIndex]
          Else
            Text1 := AllNames[ArgumentIndex];
          Str(Confidence[ArgumentIndex]:0:4, Argument);
          Writeln(Text1, ': ', Argument);
          Inc(Shown);
        End;
    If Shown = 0 Then Writeln('Unknown');
  End
  Else If Best < 0 Then
    Writeln('Unknown')
  Else If PrintCode Then
    Writeln(AllCodes[Best])
  Else
    Writeln(AllNames[Best]);
END.
