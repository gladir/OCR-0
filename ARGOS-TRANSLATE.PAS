{ @author: Sylvain Maltais (support@gladir.com)
  @created: 2026
  @website(https://www.gladir.com/OCR-0)
  @abstract(Target: Free Pascal 3.2)
  @description: Clone de la commande argos-translate (traduction de phrases) :
                argos-translate --from-lang en --to-lang fr "Hello, how are
                  you?"

  Argos Translate utilise des reseaux de neurones (OpenNMT / CTranslate2) dont
    les
  modeles pesent plusieurs dizaines de Mo : ils ne sont pas realisables en
    Pascal.
  Ce clone est un traducteur A REGLES, sans reseau : anglais -> francais,
    anglais ->
  allemand, francais -> anglais, allemand -> anglais, allemand -> francais,
  japonais (romaji) -> anglais, japonais (romaji) -> francais, espagnol ->
    anglais,
  espagnol -> francais, albanais -> anglais, albanais -> francais, portugais ->
    anglais,
  portugais -> francais, suedois -> anglais, suedois -> francais, danois ->
    anglais, danois -> francais, italien -> anglais, italien -> francais, latin
    -> anglais, latin -> francais
  et innu-aimun
  (montagnais) -> francais (mots et phrases connus seulement).
    - lexique d'environ 600 mots (noms avec genre, adjectifs avec accord et
      place,
      verbes avec groupe et auxiliaire, adverbes, prepositions) ;
    - verbes irreguliers francais (present, futur, conditionnel, imparfait,
      participe) ;
    - formes irregulieres anglaises, contractions (don't, I'm, ...), locutions ;
    - analyse d'un groupe nominal (determinant, adjectifs, noms) avec accord ;
    - phrases declaratives, negatives, interrogatives (est-ce que, inversion,
      mots
      interrogatifs) et imperatives, presents, passes (passe compose), futurs ;
    - post-traitement : elisions (j', l', n'), contractions (du, au, des, aux),
      ponctuation francaise et majuscule initiale.
    - allemand : cas (nominatif, accusatif, datif), declinaison des articles et
      des
      adjectifs, genre et pluriel des noms, verbes forts/faibles/separables,
        parfait,
      ordre des mots (verbe en 2e position, verbe final des subordonnees),
        negation
      (nicht/kein), contractions (im, zum, zur...) et "Sie" de politesse.
    - francais -> anglais : lecture des formes conjuguees (present, futur,
      conditionnel,
      imparfait, passe compose), pronoms, negation (ne ... pas), inversions et
        "est-ce
      que", groupes nominaux, puis generation anglaise (auxiliaires
        do/will/have, -s,
      temps du passe, ordre des adjectifs).
    - allemand -> anglais : formes conjuguees, participes, prefixes separables,
      cas des
      groupes nominaux (article, genre), ordre des mots (verbe 2e / verbe
        final), puis
      generation anglaise ; allemand -> francais passe par l'anglais (traduction
        pivot).
    - japonais (romaji) : particules (wa, ga, o, ni, de, to, no...), formes
      polies et
      neutres des verbes et des adjectifs (-masu, -te, -ta, -nai, -tai), copule
        desu,
      lexique d'environ 350 mots ; japonais -> francais passe par l'anglais.
    - espagnol : formes conjuguees (presents, preterit, imparfait, futur,
      conditionnel,
      participes, gerondifs, verbes irreguliers et a diphtongue), pronoms sujets
        omis
      et enclitiques, accords noms/adjectifs, ser/estar/haber/tener, gustar,
        hay,
      lexique d'environ 450 mots ; espagnol -> francais passe par l'anglais.
    - albanais : articles definis en suffixe, verbes reguliers et irreguliers
      (present,
      aoriste, imparfait, participe, futur "do te"), pronoms clitiques,
        possessifs
      places apres le nom, genitif, lexique d'environ 330 mots ; l'albanais est
      analyse avec la meme proposition que l'espagnol puis passe par l'anglais.
    - portugais : contractions (do, na, ao, pelo...), conjugaisons regulieres et
      irregulieres, pronoms sujets omis et enclitiques (ajuda-lo, chamo-me), ir
        +
      infinitif, ter que, estar a + infinitif, gostar de, ha ; lexique d'environ
        420
      mots ; comme l'albanais, le portugais reutilise l'analyse de l'espagnol.
    - suedois : noms a article defini en suffixe (huset, pojken), pluriels,
      adjectifs
      accordes, verbes (presents, preterits, supins, imperatifs), ordre des mots
        (verbe
      en 2e position retabli en ordre sujet-verbe), inte, ha + supin, att, ska,
      genitif en -s, det finns ; lexique d'environ 471 mots ; comme l'albanais
        et le
      portugais, le suedois reutilise l'analyse de l'espagnol puis passe par
        l'anglais.
    - danois : meme principe que le suedois (article defini en suffixe, ikke
      apres le
      verbe, verbe en 2e position, genitif en -s, der er, at) ; lexique
        d'environ 399
      mots, analyse de l'espagnol puis anglais.
    - italien : contractions (del, nel, sul...), articles elides (l', dell'),
      conjugaisons
      regulieres (-are, -ere, -ire, -isco) et irregulieres, passe compose rendu
        par un
      preterit anglais, ci + essere (il y a), partitif (del pane), fa freddo, ho
        fame,
      enclitiques (vederlo) ; lexique d'environ 441 mots ; analyse de l'espagnol
        puis
      anglais.
    - latin : cinq declinaisons (cas nominatif a ablatif, singulier et pluriel)
      et quatre
      conjugaisons engendrees depuis les formes de base, verbes irreguliers
        (sum, possum,
      eo, fero, volo, nolo), present, imparfait, futur, parfait, infinitif et
        imperatif ;
      l'ordre des mots est libre : on repere les groupes (nom + adjectifs
        accordes),
      on donne un role d'apres le cas (sujet, objet, "a", "de", ablatif), puis
        on range
      sujet - verbe - complements ; articles definis ajoutes, vocatif, -que et
        -ne,
      propositions coordonnees ; lexique d'environ 435 mots ; analyse de
        l'espagnol puis
      anglais.
  Il traduit correctement des phrases simples ; une phrase complexe peut donner
    une
  traduction approximative, et un mot inconnu est conserve tel quel.
}

{$mode objfpc}
{$H-}

Program ARGOSTRANSLATE;

Const
  Version = '1.0';
  MaxLex = 1100;
  MaxTokens = 80;
  MaxIrregular = 96;
  MaxEnglishIrregular = 120;

Type
  TLex = Record
    En: String[24];
    Fr: String[26];
    Pos: Char;          { n a v r p c j u }
    Sub: Char;          { genre m/f, place B/A, groupe e/i/r/x }
    Aux: Char;          { a = avoir, e = etre }
    Link: String[3];    { preposition apres l'infinitif : a, de }
    Fem: String[16];    { feminin irregulier de l'adjectif }
    Plu: String[18];    { pluriel irregulier }
    Idiom: String[10];  { avoir + idiom : faim, soif, froid ... }
    De: String[26];     { allemand }
    DeG: Char;          { genre : m f n, P = pluriel seulement }
    DePl: String[20];   { pluriel du nom ; radical irregulier de l'adjectif }
    DeFlags: String[6]; { w faible, s etre, d datif, S subordonnant, i
        invariable, P pluriel }
    DeSep: String[8];   { prefixe separable }
    DeCase: Char;       { cas de la preposition : a d w }
  End;

  TIrregular = Record
    Inf: String[24];
    Pres: Array[1..6] Of String[14];
    Part: String[14];
    Fut: String[14];
    Imp: String[14];
  End;

  TEnglishForm = Record
    Form: String[14];
    Base: String[14];
    Kind: Char;         { p passe, q participe, b les deux }
  End;

Const
  { ------------------------------------------------------------------ }
  { Lexique : en=fr:specification (n m/f, a B/A, v groupe aux, r, p, c, j) }
  { ------------------------------------------------------------------ }
  LexData: Array[0..100] Of String = (
    'time=temps:nm/temps~Zeit|f/Zeiten;day=jour:nm~Tag|m/Tage;night=nuit:nf~Na'+
        'cht|f/Nächte;morning=matin:nm~Morgen|m/-;evening=soir:nm~Abend|m/Abe'+
        'nde;afternoon=après-midi:nm/après-midi~Nachmittag|m/Nachmittage;',
    'week=semaine:nf~Woche|f/Wochen;month=mois:nm/mois~Monat|m/Monate;year=ann'+
        'ée:nf~Jahr|n/Jahre;hour=heure:nf~Stunde|f/Stunden;minute=minute:nf~M'+
        'inute|f/Minuten;second=seconde:nf~Sekunde|f/Sekunden;man=homme:nm~Man'+
        'n|m/Männer;',
    'woman=femme:nf/femmes~Frau|f/Frauen;child=enfant:nm~Kind|n/Kinder;boy=gar'+
        'çon:nm~Junge|mw/Jungen;girl=fille:nf~Mädchen|n/-;friend=ami:nm~Freu'+
        'nd|m/Freunde;family=famille:nf~Familie|f/Familien;mother=mère:nf~Mut'+
        'ter|f/Mütter;',
    'father=père:nm~Vater|m/Väter;brother=frère:nm~Bruder|m/Brüder;sister='+
        'sœur:nf~Schwester|f/Schwestern;son=fils:nm/fils~Sohn|m/Söhne;daught'+
        'er=fille:nf~Tochter|f/Töchter;husband=mari:nm~Ehemann|m/Ehemänner;',
    'wife=femme:nf~Frau|f/Frauen;parent=parent:nm~Elternteil|m/Eltern;people=g'+
        'ens:nm/gens~Leute|P;person=personne:nf~Person|f/Personen;house=maison'+
        ':nf~Haus|n/Häuser;home=maison:nf~Zuhause|n/-;room=chambre:nf~Zimmer|'+
        'n/-;',
    'kitchen=cuisine:nf~Küche|f/Küchen;bathroom=salle de bain:nf~Badezimmer|'+
        'n/-;bedroom=chambre:nf~Schlafzimmer|n/-;door=porte:nf~Tür|f/Türen;w'+
        'indow=fenêtre:nf~Fenster|n/-;table=table:nf~Tisch|m/Tische;',
    'chair=chaise:nf~Stuhl|m/Stühle;bed=lit:nm~Bett|n/Betten;toilet=toilettes'+
        ':nf/toilettes~Toilette|f/Toiletten;floor=sol:nm~Boden|m/Böden;wall=m'+
        'ur:nm~Wand|f/Wände;roof=toit:nm~Dach|n/Dächer;car=voiture:nf~Auto|n'+
        '/Autos;',
    'bus=bus:nm/bus~Bus|m/Busse;train=train:nm~Zug|m/Züge;plane=avion:nm~Flug'+
        'zeug|n/Flugzeuge;bike=vélo:nm~Fahrrad|n/Fahrräder;boat=bateau:nm/ba'+
        'teaux~Boot|n/Boote;street=rue:nf~Straße|f/Straßen;road=route:nf~Str'+
        'aße|f/Straßen;',
    'city=ville:nf~Stadt|f/Städte;town=ville:nf~Stadt|f/Städte;village=villa'+
        'ge:nm~Dorf|n/Dörfer;country=pays:nm/pays~Land|n/Länder;world=monde:'+
        'nm~Welt|f/Welten;school=école:nf~Schule|f/Schulen;',
    'university=université:nf~Universität|f/Universitäten;class=classe:nf~K'+
        'lasse|f/Klassen;lesson=leçon:nf~Lektion|f/Lektionen;work=travail:nm/'+
        'travaux~Arbeit|f/Arbeiten;job=emploi:nm~Job|m/Jobs;',
    'office=bureau:nm/bureaux~Büro|n/Büros;company=entreprise:nf~Firma|f/Fir'+
        'men;shop=magasin:nm~Geschäft|n/Geschäfte;store=magasin:nm~Laden|m/L'+
        'äden;market=marché:nm~Markt|m/Märkte;',
    'restaurant=restaurant:nm~Restaurant|n/Restaurants;hotel=hôtel:nm~Hotel|n'+
        '/Hotels;hospital=hôpital:nm/hôpitaux~Krankenhaus|n/Krankenhäuser;l'+
        'ibrary=bibliothèque:nf~Bibliothek|f/Bibliotheken;bank=banque:nf~Bank'+
        '|f/Banken;',
    'airport=aéroport:nm~Flughafen|m/Flughäfen;station=gare:nf~Bahnhof|m/Bah'+
        'nhöfe;museum=musée:nm~Museum|n/Museen;church=église:nf~Kirche|f/Ki'+
        'rchen;theater=théâtre:nm~Theater|n/-;cinema=cinéma:nm~Kino|n/Kinos'+
        ';',
    'beach=plage:nf~Strand|m/Strände;sea=mer:nf~Meer|n/Meere;ocean=océan:nm~'+
        'Ozean|m/Ozeane;river=rivière:nf~Fluss|m/Flüsse;lake=lac:nm~See|m/Se'+
        'en;mountain=montagne:nf~Berg|m/Berge;forest=forêt:nf~Wald|m/Wälder;',
    'garden=jardin:nm~Garten|m/Gärten;park=parc:nm~Park|m/Parks;tree=arbre:nm'+
        '~Baum|m/Bäume;flower=fleur:nf~Blume|f/Blumen;grass=herbe:nf~Gras|n/G'+
        'räser;island=île:nf~Insel|f/Inseln;field=champ:nm~Feld|n/Felder;',
    'bridge=pont:nm~Brücke|f/Brücken;sky=ciel:nm/cieux~Himmel|m/-;star=étoi'+
        'le:nf~Stern|m/Sterne;moon=lune:nf~Mond|m/Monde;sun=soleil:nm~Sonne|f/'+
        'Sonnen;water=eau:nf/eaux~Wasser|n/-;food=nourriture:nf~Essen|n/-;',
    'bread=pain:nm~Brot|n/Brote;cheese=fromage:nm~Käse|m/-;meat=viande:nf~Fle'+
        'isch|n/-;fish=poisson:nm~Fisch|m/Fische;apple=pomme:nf~Apfel|m/Äpfel'+
        ';orange=orange:nf~Orange|f/Orangen;banana=banane:nf~Banane|f/Bananen;',
    'egg=œuf:nm~Ei|n/Eier;rice=riz:nm/riz~Reis|m/-;salad=salade:nf~Salat|m/Sa'+
        'late;soup=soupe:nf~Suppe|f/Suppen;cake=gâteau:nm/gâteaux~Kuchen|m/-'+
        ';chocolate=chocolat:nm~Schokolade|f/Schokoladen;sugar=sucre:nm~Zucker'+
        '|m/-;',
    'salt=sel:nm~Salz|n/Salze;vegetable=légume:nm~Gemüse|n/-;fruit=fruit:nm~'+
        'Obst|n/-;coffee=café:nm~Kaffee|m/Kaffees;tea=thé:nm~Tee|m/Tees;milk'+
        '=lait:nm~Milch|f/-;wine=vin:nm~Wein|m/Weine;beer=bière:nf~Bier|n/Bie'+
        're;',
    'juice=jus:nm/jus~Saft|m/Säfte;drink=boisson:nf~Getränk|n/Getränke;brea'+
        'kfast=petit-déjeuner:nm~Frühstück|n/Frühstücke;lunch=déjeuner:n'+
        'm~Mittagessen|n/-;dinner=dîner:nm~Abendessen|n/-;',
    'meal=repas:nm/repas~Mahlzeit|f/Mahlzeiten;menu=menu:nm~Speisekarte|f/Spei'+
        'sekarten;bill=addition:nf~Rechnung|f/Rechnungen;plate=assiette:nf~Tel'+
        'ler|m/-;glass=verre:nm~Glas|n/Gläser;cup=tasse:nf~Tasse|f/Tassen;',
    'bottle=bouteille:nf~Flasche|f/Flaschen;knife=couteau:nm/couteaux~Messer|n'+
        '/-;fork=fourchette:nf~Gabel|f/Gabeln;spoon=cuillère:nf~Löffel|m/-;m'+
        'oney=argent:nm~Geld|n/-;price=prix:nm/prix~Preis|m/Preise;card=carte:'+
        'nf~Karte|f/Karten;',
    'book=livre:nm~Buch|n/Bücher;letter=lettre:nf~Brief|m/Briefe;newspaper=jo'+
        'urnal:nm/journaux~Zeitung|f/Zeitungen;magazine=magazine:nm~Zeitschrif'+
        't|f/Zeitschriften;page=page:nf~Seite|f/Seiten;phone=téléphone:nm~Te'+
        'lefon|n/Telefone;',
    'telephone=téléphone:nm~Telefon|n/Telefone;computer=ordinateur:nm~Comput'+
        'er|m/-;television=télévision:nf~Fernseher|m/-;radio=radio:nf~Radio|'+
        'n/Radios;camera=appareil photo:nm~Kamera|f/Kameras;internet=internet:'+
        'nm~Internet|n/-;',
    'email=courriel:nm~E-Mail|f/E-Mails;message=message:nm~Nachricht|f/Nachric'+
        'hten;website=site web:nm/sites web~Website|f/Websites;name=nom:nm~Nam'+
        'e|mw/Namen;language=langue:nf~Sprache|f/Sprachen;word=mot:nm~Wort|n/W'+
        'örter;',
    'sentence=phrase:nf~Satz|m/Sätze;question=question:nf~Frage|f/Fragen;answ'+
        'er=réponse:nf~Antwort|f/Antworten;problem=problème:nm~Problem|n/Pro'+
        'bleme;idea=idée:nf~Idee|f/Ideen;story=histoire:nf~Geschichte|f/Gesch'+
        'ichten;',
    'history=histoire:nf~Geschichte|f/-;news=nouvelles:nf/nouvelles~Nachrichte'+
        'n|P;information=information:nf~Information|f/Informationen;thing=chos'+
        'e:nf~Sache|f/Sachen;place=endroit:nm~Ort|m/Orte;way=chemin:nm~Weg|m/W'+
        'ege;',
    'reason=raison:nf~Grund|m/Gründe;example=exemple:nm~Beispiel|n/Beispiele;'+
        'number=numéro:nm~Nummer|f/Nummern;part=partie:nf~Teil|m/Teile;end=fi'+
        'n:nf~Ende|n/Enden;beginning=début:nm~Anfang|m/Anfänge;side=côté:n'+
        'm~Seite|f/Seiten;',
    'top=haut:nm~Spitze|f/Spitzen;bottom=bas:nm~Boden|m/Böden;music=musique:n'+
        'f~Musik|f/-;song=chanson:nf~Lied|n/Lieder;movie=film:nm~Film|m/Filme;'+
        'film=film:nm~Film|m/Filme;game=jeu:nm/jeux~Spiel|n/Spiele;',
    'picture=image:nf~Bild|n/Bilder;photo=photo:nf~Foto|n/Fotos;painting=table'+
        'au:nm/tableaux~Gemälde|n/-;art=art:nm~Kunst|f/Künste;dance=danse:nf'+
        '~Tanz|m/Tänze;party=fête:nf~Party|f/Partys;',
    'gift=cadeau:nm/cadeaux~Geschenk|n/Geschenke;birthday=anniversaire:nm~Gebu'+
        'rtstag|m/Geburtstage;holiday=vacances:nf/vacances~Urlaub|m/-;vacation'+
        '=vacances:nf/vacances~Urlaub|m/-;trip=voyage:nm~Reise|f/Reisen;',
    'journey=voyage:nm~Reise|f/Reisen;ticket=billet:nm~Ticket|n/Tickets;passpo'+
        'rt=passeport:nm~Reisepass|m/Reisepässe;luggage=bagages:nm/bagages~Ge'+
        'päck|n/-;bag=sac:nm~Tasche|f/Taschen;suitcase=valise:nf~Koffer|m/-;',
    'key=clé:nf~Schlüssel|m/-;dog=chien:nm~Hund|m/Hunde;cat=chat:nm~Katze|f/'+
        'Katzen;bird=oiseau:nm/oiseaux~Vogel|m/Vögel;horse=cheval:nm/chevaux~'+
        'Pferd|n/Pferde;animal=animal:nm/animaux~Tier|n/Tiere;cow=vache:nf~Kuh'+
        '|f/Kühe;',
    'pig=cochon:nm~Schwein|n/Schweine;mouse=souris:nf/souris~Maus|f/Mäuse;rab'+
        'bit=lapin:nm~Kaninchen|n/-;weather=temps:nm~Wetter|n/-;rain=pluie:nf~'+
        'Regen|m/-;snow=neige:nf~Schnee|m/-;wind=vent:nm~Wind|m/Winde;',
    'cloud=nuage:nm~Wolke|f/Wolken;storm=orage:nm~Sturm|m/Stürme;life=vie:nf~'+
        'Leben|n/-;death=mort:nf~Tod|m/-;love=amour:nm~Liebe|f/-;heart=cœur:n'+
        'm~Herz|n/Herzen;peace=paix:nf~Frieden|m/-;war=guerre:nf~Krieg|m/Krieg'+
        'e;',
    'freedom=liberté:nf~Freiheit|f/-;head=tête:nf~Kopf|m/Köpfe;hand=main:nf'+
        '~Hand|f/Hände;foot=pied:nm~Fuß|m/Füße;arm=bras:nm/bras~Arm|m/Arme'+
        ';leg=jambe:nf~Bein|n/Beine;eye=œil:nm/yeux~Auge|n/Augen;ear=oreille:'+
        'nf~Ohr|n/Ohren;',
    'nose=nez:nm/nez~Nase|f/Nasen;mouth=bouche:nf~Mund|m/Münder;face=visage:n'+
        'm~Gesicht|n/Gesichter;hair=cheveux:nm/cheveux~Haare|P;body=corps:nm/c'+
        'orps~Körper|m/-;health=santé:nf~Gesundheit|f/-;doctor=médecin:nm~A'+
        'rzt|m/Ärzte;',
    'nurse=infirmière:nf~Krankenschwester|f/Krankenschwestern;teacher=profess'+
        'eur:nm~Lehrer|m/-;student=étudiant:nm~Student|mw/Studenten;driver=ch'+
        'auffeur:nm~Fahrer|m/-;worker=ouvrier:nm~Arbeiter|m/-;king=roi:nm~Kön'+
        'ig|m/Könige;',
    'queen=reine:nf~Königin|f/Königinnen;president=président:nm~Präsident|'+
        'mw/Präsidenten;police=police:nf~Polizei|f/-;soldier=soldat:nm~Soldat'+
        '|mw/Soldaten;shirt=chemise:nf~Hemd|n/Hemden;dress=robe:nf~Kleid|n/Kle'+
        'ider;',
    'shoe=chaussure:nf~Schuh|m/Schuhe;hat=chapeau:nm/chapeaux~Hut|m/Hüte;coat'+
        '=manteau:nm/manteaux~Mantel|m/Mäntel;pants=pantalon:nm~Hose|f/Hosen;'+
        'clothes=vêtements:nm/vêtements~Kleidung|f/-;color=couleur:nf~Farbe|'+
        'f/Farben;',
    'size=taille:nf~Größe|f/Größen;shape=forme:nf~Form|f/Formen;line=ligne'+
        ':nf~Linie|f/Linien;point=point:nm~Punkt|m/Punkte;level=niveau:nm/nive'+
        'aux~Niveau|n/Niveaus;group=groupe:nm~Gruppe|f/Gruppen;',
    'team=équipe:nf~Mannschaft|f/Mannschaften;sport=sport:nm~Sport|m/-;ball=b'+
        'allon:nm~Ball|m/Bälle;machine=machine:nf~Maschine|f/Maschinen;tool=o'+
        'util:nm~Werkzeug|n/Werkzeuge;light=lumière:nf~Licht|n/Lichter;',
    'fire=feu:nm/feux~Feuer|n/-;air=air:nm~Luft|f/-;earth=terre:nf~Erde|f/-;st'+
        'one=pierre:nf~Stein|m/Steine;gold=or:nm~Gold|n/-;paper=papier:nm~Papi'+
        'er|n/Papiere;pen=stylo:nm~Stift|m/Stifte;pencil=crayon:nm~Bleistift|m'+
        '/Bleistifte;',
    'monday=lundi:nm~Montag|m/Montage;tuesday=mardi:nm~Dienstag|m/Dienstage;we'+
        'dnesday=mercredi:nm~Mittwoch|m/Mittwoche;thursday=jeudi:nm~Donnerstag'+
        '|m/Donnerstage;friday=vendredi:nm~Freitag|m/Freitage;',
    'saturday=samedi:nm~Samstag|m/Samstage;sunday=dimanche:nm~Sonntag|m/Sonnta'+
        'ge;january=janvier:nm~Januar|m/-;february=février:nm~Februar|m/-;mar'+
        'ch=mars:nm~März|m/-;april=avril:nm~April|m/-;may=mai:nm~Mai|m/-;june'+
        '=juin:nm~Juni|m/-;',
    'july=juillet:nm~Juli|m/-;august=août:nm~August|m/-;september=septembre:n'+
        'm~September|m/-;october=octobre:nm~Oktober|m/-;november=novembre:nm~N'+
        'ovember|m/-;december=décembre:nm~Dezember|m/-;spring=printemps:nm~Fr'+
        'ühling|m/-;',
    'summer=été:nm~Sommer|m/-;autumn=automne:nm~Herbst|m/-;winter=hiver:nm~W'+
        'inter|m/-;good=bon:aB/bonne~gut;bad=mauvais:aB~schlecht;big=grand:aB~'+
        'groß;large=grand:aB~groß;small=petit:aB~klein;little=petit:aB~klein'+
        ';',
    'long=long:aB/longue~lang;short=court:aA/courte~kurz;tall=grand:aB~groß;h'+
        'igh=haut:aB~hoch/hoh;low=bas:aA/basse~niedrig;new=nouveau:aB/nouvelle'+
        '/nouveaux~neu;old=vieux:aB/vieille/vieux~alt;young=jeune:aB~jung;',
    'beautiful=beau:aB/belle/beaux~schön;pretty=joli:aB~hübsch;nice=agréabl'+
        'e:aA~schön;great=grand:aB~toll;wonderful=merveilleux:aA/merveilleuse'+
        '~wunderbar;terrible=terrible:aA~schrecklich;delicious=délicieux:aA/d'+
        'élicieuse~lecker;',
    'happy=heureux:aA/heureuse~glücklich;sad=triste:aA~traurig;tired=fatigué'+
        ':aA~müde;hungry=affamé:aA!faim~hungrig;thirsty=assoiffé:aA!soif~du'+
        'rstig;afraid=effrayé:aA!peur~ängstlich;sleepy=endormi:aA!sommeil~m'+
        'üde;',
    'cold=froid:aA!froid~kalt;hot=chaud:aA!chaud~heiß;warm=chaud:aA~warm;cool'+
        '=frais:aA/fraîche~kühl;easy=facile:aA~einfach;difficult=difficile:a'+
        'A~schwierig;hard=difficile:aA~schwer;important=important:aA~wichtig;',
    'interesting=intéressant:aA~interessant;boring=ennuyeux:aA/ennuyeuse~lang'+
        'weilig;funny=drôle:aA~lustig;strange=étrange:aA~seltsam;famous=cél'+
        'èbre:aA~berühmt;popular=populaire:aA~beliebt;expensive=cher:aA/chè'+
        're~teuer/teur;',
    'cheap=bon marché:aA/bon marché/bon marché~billig;free=gratuit:aA/gratu'+
        'ite~kostenlos;open=ouvert:aA/ouverte~offen;closed=fermé:aA~geschloss'+
        'en;ready=prêt:aA/prête~fertig;busy=occupé:aA~beschäftigt;sick=mal'+
        'ade:aA~krank;',
    'ill=malade:aA~krank;rich=riche:aA~reich;poor=pauvre:aA~arm;clean=propre:a'+
        'A~sauber;dirty=sale:aA~schmutzig;fast=rapide:aA~schnell;quick=rapide:'+
        'aA~schnell;slow=lent:aA/lente~langsam;strong=fort:aA/forte~stark;weak'+
        '=faible:aA~schwach;',
    'heavy=lourd:aA/lourde~schwer;light=léger:aA/légère~leicht;full=plein:a'+
        'A/pleine~voll;empty=vide:aA~leer;dark=sombre:aA~dunkel/dunkl;bright=b'+
        'rillant:aA~hell;red=rouge:aA~rot;blue=bleu:aA/bleue~blau;green=vert:a'+
        'A/verte~grün;',
    'yellow=jaune:aA~gelb;black=noir:aA/noire~schwarz;white=blanc:aA/blanche/b'+
        'lancs~weiß;brown=marron:aA/marron/marron~braun;gray=gris:aA/grise~gr'+
        'au;grey=gris:aA/grise~grau;pink=rose:aA~rosa/*;purple=violet:aA/viole'+
        'tte~lila/*;',
    'first=premier:aB/première~erst;last=dernier:aB/dernière~letzt;next=proc'+
        'hain:aB/prochaine~nächst;other=autre:aB~ander;same=même:aB~gleich;d'+
        'ifferent=différent:aA/différente~anders;sure=sûr:aA/sûre~sicher;',
    'true=vrai:aA/vraie~wahr;false=faux:aA/fausse~falsch;possible=possible:aA~'+
        'möglich;special=spécial:aA/spéciale/spéciaux~besonders;proud=fier'+
        ':aA/fière~stolz;kind=gentil:aA/gentille~freundlich;nervous=nerveux:a'+
        'A/nerveuse~nervös;',
    'lost=perdu:aA/perdue~verloren;late=en retard:aA/en retard/en retard~spät'+
        ';lonely=seul:aA/seule~einsam;alone=seul:aA/seule~allein;married=mari'+
        'é:aA/mariée~verheiratet;single=célibataire:aA~ledig;very=très:r~s'+
        'ehr;too=trop:r~zu;',
    'also=aussi:r~auch;always=toujours:r~immer;never=jamais:r~nie;often=souven'+
        't:r~oft;sometimes=parfois:r~manchmal;usually=d''habitude:r~normalerwe'+
        'ise;now=maintenant:r~jetzt;today=aujourd''hui:r~heute;tomorrow=demain'+
        ':r~morgen;',
    'yesterday=hier:r~gestern;tonight=ce soir:r~heute Abend;here=ici:r~hier;th'+
        'ere=là:r~dort;well=bien:r~gut;quickly=rapidement:r~schnell;slowly=le'+
        'ntement:r~langsam;again=encore:r~wieder;soon=bientôt:r~bald;already='+
        'déjà:r~schon;',
    'still=encore:r~noch;just=juste:r~gerade;only=seulement:r~nur;really=vraim'+
        'ent:r~wirklich;much=beaucoup:r~viel;maybe=peut-être:r~vielleicht;per'+
        'haps=peut-être:r~vielleicht;together=ensemble:r~zusammen;almost=pres'+
        'que:r~fast;',
    'then=puis:r~dann;early=tôt:r~früh;away=loin:r~weg;everywhere=partout:r~'+
        'überall;somewhere=quelque part:r~irgendwo;outside=dehors:r~draußen;'+
        'inside=dedans:r~drinnen;however=cependant:r~jedoch;very much=beaucoup'+
        ':r~sehr;',
    'a lot=beaucoup:r~viel;a lot of=beaucoup de:r~viele;right now=en ce moment'+
        ':r~gerade jetzt;of course=bien sûr:r~natürlich;at home=à la maison'+
        ':r~zu Hause;every day=tous les jours:r~jeden Tag;',
    'all day=toute la journée:r~den ganzen Tag;next week=la semaine prochaine'+
        ':r~nächste Woche;last night=hier soir:r~gestern Abend;this morning=c'+
        'e matin:r~heute Morgen;this evening=ce soir:r~heute Abend;',
    'this afternoon=cet après-midi:r~heute Nachmittag;in=dans:p~in|w;on=sur:p'+
        '~auf|w;at=à:p~an|w;from=de:p~von|d;with=avec:p~mit|d;without=sans:p~'+
        'ohne|a;for=pour:p~für|a;of=de:p~von|d;about=à propos de:p~über|a;b'+
        'y=par:p~von|d;',
    'under=sous:p~unter|w;over=sur:p~über|w;between=entre:p~zwischen|w;before'+
        '=avant:p~vor|w;after=après:p~nach|d;during=pendant:p~während|d;sinc'+
        'e=depuis:p~seit|d;until=jusqu''à:p~bis|a;near=près de:p~in der Näh'+
        'e von|d;',
    'behind=derrière:p~hinter|w;around=autour de:p~um|a;through=à travers:p~'+
        'durch|a;into=dans:p~in|a;against=contre:p~gegen|a;across=à travers:p'+
        '~über|a;toward=vers:p~zu|d;towards=vers:p~zu|d;above=au-dessus de:p~'+
        'über|w;',
    'below=en dessous de:p~unter|w;next to=à côté de:p~neben|w;in front of='+
        'devant:p~vor|w;because of=à cause de:p~wegen|d;instead of=au lieu de'+
        ':p~statt|d;out of=hors de:p~aus|d;up=en haut:r~hoch;down=en bas:r~run'+
        'ter;off=loin:r~weg;',
    'and=et:c~und;or=ou:c~oder;but=mais:c~aber;because=parce que:c~weil|S;if=s'+
        'i:c~wenn|S;when=quand:c~wenn|S;while=pendant que:c~während|S;so=donc'+
        ':c~also;that=que:c~dass|S;than=que:c~als;as=comme:c~wie;although=bien'+
        ' que:c~obwohl|S;',
    'hello=bonjour:j~Hallo;hi=salut:j~Hallo;goodbye=au revoir:j~Auf Wiedersehe'+
        'n;bye=au revoir:j~Tschüss;thanks=merci:j~Danke;thank you=merci:j~Dan'+
        'ke;please=s''il vous plaît:j~Bitte;sorry=désolé:j~Entschuldigung;',
    'excuse me=excusez-moi:j~Entschuldigung;welcome=bienvenue:j~Willkommen;yes'+
        '=oui:j~Ja;no=non:j~Nein;okay=d''accord:j~Okay;ok=d''accord:j~Okay;goo'+
        'd morning=bonjour:j~Guten Morgen;good evening=bonsoir:j~Guten Abend;',
    'good night=bonne nuit:j~Gute Nacht;good afternoon=bonjour:j~Guten Tag;con'+
        'gratulations=félicitations:j~Herzlichen Glückwunsch;cheers=santé:j'+
        '~Prost;wow=ouah:j~Wow;oh=oh:j~Oh;love=aimer:ve~lieben|;like=aimer:ve~'+
        'mögen|;',
    'hate=détester:ve~hassen|;want=vouloir:vxa@~wollen|;need=avoir besoin de:'+
        'vxa@~brauchen|;go=aller:vxe~gehen|s;come=venir:vxe~kommen|s;see=voir:'+
        'vxa~sehen|;look=regarder:ve~schauen|;watch=regarder:ve~schauen|;',
    'hear=entendre:vr~hören|;listen=écouter:ve~hören|d+zu;speak=parler:ve~s'+
        'prechen|;talk=parler:ve~sprechen|;say=dire:vxa~sagen|;tell=dire:vxa~s'+
        'agen|;ask=demander:ve~fragen|;answer=répondre:vr~antworten|d;',
    'understand=comprendre:vxa~verstehen|;know=savoir:vxa~wissen|;think=penser'+
        ':ve~denken|;believe=croire:vxa~glauben|;feel=sentir:vxa~fühlen|;find'+
        '=trouver:ve~finden|;give=donner:ve~geben|;take=prendre:vxa~nehmen|;',
    'make=faire:vxa~machen|;put=mettre:vxa~legen|;get=obtenir:vxa~bekommen|;bu'+
        'y=acheter:vxa~kaufen|;sell=vendre:vr~verkaufen|;pay=payer:vxa~bezahle'+
        'n|;eat=manger:ve~essen|;drink=boire:vxa~trinken|;cook=cuisiner:ve~koc'+
        'hen|;',
    'sleep=dormir:vxa~schlafen|;live=habiter:ve~wohnen|;work=travailler:ve~arb'+
        'eiten|;study=étudier:ve~studieren|;learn=apprendre:vxa@à~lernen|;te'+
        'ach=enseigner:ve~unterrichten|;read=lire:vxa~lesen|;write=écrire:vxa'+
        '~schreiben|;',
    'play=jouer:ve~spielen|;sing=chanter:ve~singen|;dance=danser:ve~tanzen|;ru'+
        'n=courir:vxa~laufen|s;walk=marcher:ve~gehen|s;drive=conduire:vxa~fahr'+
        'en|s;travel=voyager:ve~reisen|s;arrive=arriver:vee~kommen|s+an;',
    'leave=partir:vxe~fahren|s+ab;stay=rester:vee~bleiben|s;return=revenir:vxe'+
        '~kommen|s+zurück;begin=commencer:ve@à~beginnen|;start=commencer:ve@'+
        'à~fangen|+an;finish=finir:via~beenden|;stop=arrêter:ve@de~hören|+a'+
        'uf;',
    'open=ouvrir:vxa~öffnen|;close=fermer:ve~schließen|;help=aider:ve~helfen'+
        '|d;wait=attendre:vr~warten|;try=essayer:vxa@de~versuchen|;call=appele'+
        'r:vxa~rufen|+an;meet=rencontrer:ve~treffen|;visit=visiter:ve~besuchen'+
        '|;',
    'use=utiliser:ve~benutzen|;forget=oublier:ve~vergessen|;choose=choisir:via'+
        '~wählen|;decide=décider:ve@de~entscheiden|;hope=espérer:ve~hoffen|'+
        ';enjoy=apprécier:ve~genießen|;miss=manquer:ve~vermissen|;send=envoy'+
        'er:vxa~schicken|;',
    'bring=apporter:ve~bringen|;carry=porter:ve~tragen|;wear=porter:ve~tragen|'+
        ';cost=coûter:ve~kosten|;change=changer:ve~ändern|;seem=sembler:ve~s'+
        'cheinen|;become=devenir:vxe~werden|s;show=montrer:ve~zeigen|;swim=nag'+
        'er:ve~schwimmen|s;',
    'smile=sourire:vxa~lächeln|;laugh=rire:vxa~lachen|;cry=pleurer:ve~weinen|'+
        ';die=mourir:vxe~sterben|s;win=gagner:ve~gewinnen|;lose=perdre:vr~verl'+
        'ieren|;break=casser:ve~brechen|;build=construire:vxa~bauen|;enter=ent'+
        'rer:vee~betreten|;',
    'wash=laver:ve~waschen|;clean=nettoyer:vxa~putzen|;cut=couper:ve~schneiden'+
        '|;prefer=préférer:vxa~bevorzugen|;remember=se rappeler:ve~behalten|'+
        ';receive=recevoir:vxa~erhalten|;fall=tomber:vee~fallen|s;order=comman'+
        'der:ve~bestellen|;',
    'rain=pleuvoir:ve~regnen|;english=anglais:nm~Englisch|n/-;french=français'+
        ':nm~Französisch|n/-;spanish=espagnol:nm~Spanisch|n/-;german=allemand'+
        ':nm~Deutsch|n/-;italian=italien:nm~Italienisch|n/-;chinese=chinois:nm'+
        '~Chinesisch|n/-;',
    'japanese=japonais:nm~Japanisch|n/-;russian=russe:nm~Russisch|n/-;portugue'+
        'se=portugais:nm~Portugiesisch|n/-;arabic=arabe:nm~Arabisch|n/-;let=la'+
        'isser:ve~lassen|;near here=près d''ici:r~in der Nähe;happen=arriver'+
        ':vee~passieren|s;',
    'thank=remercier:ve~danken|d;a bit=un peu:r~ein bisschen;right=juste:aA!ra'+
        'ison~richtig;wrong=faux:aA!tort~falsch;capital=capitale:nf~Hauptstadt'+
        '|f/Hauptstädte;homework=devoir:nm~Hausaufgabe|f/Hausaufgaben;',
    'present=cadeau:nm/cadeaux~Geschenk|n/Geschenke;born=né:aA/née~geboren;h'+
        'elp=aide:nf~Hilfe|f/-;last week=la semaine dernière:r~letzte Woche;l'+
        'ast year=l''année dernière:r~letztes Jahr;last month=le mois dernie'+
        'r:r~letzten Monat;',
    'next month=le mois prochain:r~nächsten Monat;next year=l''année prochai'+
        'ne:r~nächstes Jahr;this week=cette semaine:r~diese Woche;this year=c'+
        'ette année:r~dieses Jahr;this month=ce mois-ci:r~diesen Monat;',
    'pizza=pizza:nf~Pizza|f/Pizzen;sandwich=sandwich:nm~Sandwich|n/Sandwiches;'+
        'hamburger=hamburger:nm~Hamburger|m/-;to=à:p~zu|d;swedish=suédois:nm'+
        ';danish=danois:nm;anger=colère:nf;army=armée:nf;chariot=char:nm;cit'+
        'izen=citoyen:nm;',
    'danger=danger:nm;duty=devoir:nm;emperor=empereur:nm;empire=empire:nm;enem'+
        'y=ennemi:nm;faith=foi:nf;farmer=fermier:nm;fear=peur:nf;forum=forum:n'+
        'm;gate=porte:nf;glory=gloire:nf;god=dieu:nm/dieux;goddess=déesse:nf;'+
        'homeland=patrie:nf;',
    'honor=honneur:nm;horn=corne:nf;inhabitant=habitant:nm;kingdom=royaume:nm;'+
        'labor=travail:nm/travaux;lady=dame:nf;land=terre:nf;law=loi:nf;leader'+
        '=chef:nm;leaf=feuille:nf;lion=lion:nm;maid=servante:nf;master=maître'+
        ':nm;',
    'messenger=messager:nm;mind=esprit:nm;nation=nation:nf;pain=douleur:nf;pla'+
        'in=plaine:nf;plan=plan:nm;poet=poète:nm;port=port:nm;pupil=élève:n'+
        'm;reward=récompense:nf;sailor=marin:nm;senate=sénat:nm;senator=sén'+
        'ateur:nm;',
    'shepherd=berger:nm;ship=navire:nm;sign=signe:nm;slave=esclave:nm;state=ci'+
        'té:nf;supply=provision:nf;sword=épée:nf;tablet=tablette:nf;temple='+
        'temple:nm;truth=vérité:nf;victory=victoire:nf;virtue=vertu:nf;voice'+
        '=voix:nf/voix;',
    'wolf=loup:nm;roman=Romain:nm;blessed=béni:aA;bold=audacieux:aA/audacieus'+
        'e;brave=courageux:aA/courageuse;cruel=cruel:aA/cruelle;dear=cher:aA/c'+
        'hère;huge=énorme:aB;learned=savant:aA;lucky=chanceux:aA/chanceuse;n'+
        'oble=noble:aA;',
    'powerful=puissant:aA;serious=sérieux:aA/sérieuse;sweet=doux:aA/douce;us'+
        'eful=utile:aA;wise=sage:aA;wretched=misérable:aA;advise=conseiller:v'+
        'e;announce=annoncer:ve;approve=approuver:ve;attack=attaquer:ve;avoid='+
        'éviter:ve;',
    'tend=soigner:ve;defend=défendre:vr;desire=désirer:ve;divide=diviser:ve;'+
        'drag=traîner:ve;fortify=fortifier:ve;frighten=terrifier:ve;greet=sal'+
        'uer:ve;guard=garder:ve;kill=tuer:ve;lead=conduire:vxa;pray=prier:ve;',
    'prepare=préparer:ve;punish=punir:via;refuse=refuser:ve;remain=rester:vee'+
        ';rule=gouverner:ve;save=sauver:ve;seek=chercher:ve;shout=crier:ve;tur'+
        'n=tourner:ve;wander=errer:ve;weep=pleurer:ve;move=remuer:ve;throw=lan'+
        'cer:ve;',
    'conquer=conquérir:vxa;fear=craindre:vxa;fight=combattre:vxa;flee=fuir:vx'+
        'a;destroy=détruire:vxa;hold=tenir:vxa;serve=servir:vxa;seize=saisir:'+
        'via;dismiss=congédier:ve;badly=mal:r;enough=assez:r;far=loin:r;final'+
        'ly=enfin:r;',
    'hardly=à peine:r;immediately=immédiatement:r;once=autrefois:r;thus=ains'+
        'i:r;praise=louer:ve;wage=mener:vxa;'
  );

  { Verbes francais irreguliers :
    inf|je|tu|il|nous|vous|ils|participe|futur|imparfait }
  IrregularData: Array[0..51] Of String = (
    'être|suis|es|est|sommes|êtes|sont|été|ser|ét',
    'avoir|ai|as|a|avons|avez|ont|eu|aur|av',
    'aller|vais|vas|va|allons|allez|vont|allé|ir|all',
    'faire|fais|fais|fait|faisons|faites|font|fait|fer|fais',
    'pouvoir|peux|peux|peut|pouvons|pouvez|peuvent|pu|pourr|pouv',
    'vouloir|veux|veux|veut|voulons|voulez|veulent|voulu|voudr|voul',
    'savoir|sais|sais|sait|savons|savez|savent|su|saur|sav',
    'devoir|dois|dois|doit|devons|devez|doivent|dû|devr|dev',
    'venir|viens|viens|vient|venons|venez|viennent|venu|viendr|ven',
    'voir|vois|vois|voit|voyons|voyez|voient|vu|verr|voy',
    'dire|dis|dis|dit|disons|dites|disent|dit|dir|dis',
    'prendre|prends|prends|prend|prenons|prenez|prennent|pris|prendr|pren',
    'mettre|mets|mets|met|mettons|mettez|mettent|mis|mettr|mett',
    'boire|bois|bois|boit|buvons|buvez|boivent|bu|boir|buv',
    'lire|lis|lis|lit|lisons|lisez|lisent|lu|lir|lis',
    'écrire|écris|écris|écrit|écrivons|écrivez|écrivent|écrit|écrir|'+
        'écriv',
    'dormir|dors|dors|dort|dormons|dormez|dorment|dormi|dormir|dorm',
    'courir|cours|cours|court|courons|courez|courent|couru|courr|cour',
    'conduire|conduis|conduis|conduit|conduisons|conduisez|conduisent|conduit|'+
        'conduir|conduis',
    'croire|crois|crois|croit|croyons|croyez|croient|cru|croir|croy',
    'comprendre|comprends|comprends|comprend|comprenons|comprenez|comprennent|'+
        'compris|comprendr|compren',
    'apprendre|apprends|apprends|apprend|apprenons|apprenez|apprennent|appris|'+
        'apprendr|appren',
    'ouvrir|ouvre|ouvres|ouvre|ouvrons|ouvrez|ouvrent|ouvert|ouvrir|ouvr',
    'sentir|sens|sens|sent|sentons|sentez|sentent|senti|sentir|sent',
    'partir|pars|pars|part|partons|partez|partent|parti|partir|part',
    'devenir|deviens|deviens|devient|devenons|devenez|deviennent|devenu|devien'+
        'dr|deven',
    'revenir|reviens|reviens|revient|revenons|revenez|reviennent|revenu|revien'+
        'dr|reven',
    'sourire|souris|souris|sourit|sourions|souriez|sourient|souri|sourir|souri',
    'rire|ris|ris|rit|rions|riez|rient|ri|rir|ri',
    'mourir|meurs|meurs|meurt|mourons|mourez|meurent|mort|mourr|mour',
    'construire|construis|construis|construit|construisons|construisez|constru'+
        'isent|construit|construir|construis',
    'recevoir|reçois|reçois|reçoit|recevons|recevez|reçoivent|reçu|recevr'+
        '|recev',
    'obtenir|obtiens|obtiens|obtient|obtenons|obtenez|obtiennent|obtenu|obtien'+
        'dr|obten',
    'connaître|connais|connais|connaît|connaissons|connaissez|connaissent|co'+
        'nnu|connaîtr|connaiss',
    'acheter|achète|achètes|achète|achetons|achetez|achètent|acheté|achè'+
        'ter|achet',
    'appeler|appelle|appelles|appelle|appelons|appelez|appellent|appelé|appel'+
        'ler|appel',
    'préférer|préfère|préfères|préfère|préférons|préférez|préfèr'+
        'ent|préféré|préférer|préfér',
    'envoyer|envoie|envoies|envoie|envoyons|envoyez|envoient|envoyé|enverr|en'+
        'voy',
    'payer|paie|paies|paie|payons|payez|paient|payé|paier|pay',
    'nettoyer|nettoie|nettoies|nettoie|nettoyons|nettoyez|nettoient|nettoyé|n'+
        'ettoier|nettoy',
    'essayer|essaie|essaies|essaie|essayons|essayez|essaient|essayé|essaier|e'+
        'ssay',
    'pleuvoir|pleus|pleus|pleut|pleuvons|pleuvez|pleuvent|plu|pleuvr|pleuv',
    'se rappeler|me rappelle|te rappelles|se rappelle|nous rappelons|vous rapp'+
        'elez|se rappellent|rappelé|rappeller|rappel',
    'avoir besoin de|ai besoin de|as besoin de|a besoin de|avons besoin de|ave'+
        'z besoin de|ont besoin de|eu besoin de|aur besoin de|av besoin de',
    'conquérir|conquiers|conquiers|conquiert|conquérons|conquérez|conquièr'+
        'ent|conquis|conquerr|conquér',
    'craindre|crains|crains|craint|craignons|craignez|craignent|craint|craindr'+
        '|craign',
    'combattre|combats|combats|combat|combattons|combattez|combattent|combattu'+
        '|combattr|combatt',
    'fuir|fuis|fuis|fuit|fuyons|fuyez|fuient|fui|fuir|fuy',
    'détruire|détruis|détruis|détruit|détruisons|détruisez|détruisent|d'+
        'étruit|détruir|détruis',
    'tenir|tiens|tiens|tient|tenons|tenez|tiennent|tenu|tiendr|ten',
    'servir|sers|sers|sert|servons|servez|servent|servi|servir|serv',
    'mener|mène|mènes|mène|menons|menez|mènent|mené|mèner|men'
  );

  { Formes anglaises irregulieres : forme=base:type }
  EnglishData: Array[0..4] Of String = (
    'went=go:p;gone=go:q;ate=eat:p;eaten=eat:q;saw=see:p;seen=see:q;took=take:'+
        'p;taken=take:q;made=make:b;said=say:b;came=come:p;got=get:b;gave=give'+
        ':p;given=give:q;knew=know:p;known=know:q;thought=think:b;told=tell:b;'+
        'found=find:b;',
    'wrote=write:p;written=write:q;bought=buy:b;spoke=speak:p;spoken=speak:q;r'+
        'an=run:p;drank=drink:p;drunk=drink:q;slept=sleep:b;met=meet:b;left=le'+
        'ave:b;felt=feel:b;sat=sit:b;stood=stand:b;lost=lose:b;paid=pay:b;hear'+
        'd=hear:b;',
    'brought=bring:b;built=build:b;sold=sell:b;sent=send:b;began=begin:p;begun'+
        '=begin:q;drove=drive:p;driven=drive:q;read=read:b;put=put:b;cut=cut:b'+
        ';chose=choose:p;chosen=choose:q;forgot=forget:p;forgotten=forget:q;',
    'understood=understand:b;became=become:p;won=win:b;broke=break:p;broken=br'+
        'eak:q;fell=fall:p;fallen=fall:q;swam=swim:p;swum=swim:q;wore=wear:p;w'+
        'orn=wear:q;taught=teach:b;sang=sing:p;sung=sing:q;died=die:b;children'+
        '=child:n;men=man:n;',
    'women=woman:n;feet=foot:n;teeth=tooth:n;mice=mouse:n;people=person:n;wive'+
        's=wife:n;knives=knife:n;lives=life:n;leaves=leaf:n;wolves=wolf:n;'
  );

  { Japonais (romaji) : romaji=anglais:categorie }
  JaData: Array[0..24] Of String = (
    'hito=person:n;otoko_no_hito=man:n;onna_no_hito=woman:n;otoko_no_ko=boy:n;'+
        'onna_no_ko=girl:n;kodomo=child:n;tomodachi=friend:n;kazoku=family:n;h'+
        'aha=mother:n;okaasan=mother:n;chichi=father:n;otousan=father:n;ani=br'+
        'other:n;',
    'oniisan=brother:n;otouto=brother:n;ane=sister:n;oneesan=sister:n;imouto=s'+
        'ister:n;kyoudai=brother:n;musuko=son:n;musume=daughter:n;otto=husband'+
        ':n;tsuma=wife:n;sensei=teacher:n;gakusei=student:n;isha=doctor:n;kang'+
        'oshi=nurse:n;',
    'kaishain=employee:n;ie=house:n;uchi=house:n;heya=room:n;daidokoro=kitchen'+
        ':n;mado=window:n;doa=door:n;tsukue=table:n;isu=chair:n;teeburu=table:'+
        'n;beddo=bed:n;gakkou=school:n;daigaku=university:n;kaisha=company:n;m'+
        'ise=shop:n;',
    'eki=station:n;kuukou=airport:n;byouin=hospital:n;ginkou=bank:n;toshokan=l'+
        'ibrary:n;resutoran=restaurant:n;hoteru=hotel:n;machi=city:n;kuni=coun'+
        'try:n;koen=park:n;kouen=park:n;umi=sea:n;yama=mountain:n;kawa=river:n'+
        ';ki=tree:n;',
    'hana=flower:n;niwa=garden:n;mizu=water:n;ocha=tea:n;koohii=coffee:n;gyuun'+
        'yuu=milk:n;biiru=beer:n;wain=wine:n;pan=bread:n;gohan=rice:n;niku=mea'+
        't:n;sakana=fish:n;yasai=vegetable:n;kudamono=fruit:n;ringo=apple:n;ta'+
        'mago=egg:n;',
    'sushi=sushi:n;ryouri=meal:n;asagohan=breakfast:n;hirugohan=lunch:n;bangoh'+
        'an=dinner:n;hon=book:n;shinbun=newspaper:n;zasshi=magazine:n;tegami=l'+
        'etter:n;denwa=phone:s;konpyuutaa=computer:n;kuruma=car:n;densha=train'+
        ':n;basu=bus:n;',
    'hikouki=plane:n;jitensha=bike:n;fune=boat:n;michi=road:n;ame=rain:n;yuki='+
        'snow:n;kaze=wind:n;sora=sky:n;inu=dog:n;neko=cat:n;tori=bird:n;uma=ho'+
        'rse:n;namae=name:n;kotoba=word:n;nihongo=Japanese:n;eigo=English:n;fu'+
        'ransugo=French:n;',
    'doitsugo=German:n;jikan=time:n;shigoto=work:n;okane=money:n;kippu=ticket:'+
        'n;kaban=bag:n;kagi=key:n;tokei=watch:n;kasa=umbrella:n;kutsu=shoe:n;f'+
        'uku=clothes:n;shashin=photo:n;eiga=movie:n;ongaku=music:n;uta=song:n;',
    'tanjoubi=birthday:n;purezento=present:n;mondai=problem:n;shitsumon=questi'+
        'on:n;kotae=answer:n;tenki=weather:n;toire=toilet:n;ue=top:l;shita=bot'+
        'tom:l;naka=inside:l;mae=front:l;ushiro=back:l;tonari=next:l;soba=near'+
        ':l;kyou=today:r;',
    'ashita=tomorrow:r;kinou=yesterday:r;ima=now:r;asa=morning:r;mainichi=ever'+
        'y_day:r;maiasa=every_morning:r;konban=tonight:r;kesa=this_morning:r;t'+
        'otemo=very:r;yoku=often:r;tokidoki=sometimes:r;itsumo=always:r;mou=al'+
        'ready:r;',
    'mada=still:r;sugu=soon:r;sukoshi=a_little:r;takusan=a_lot:r;zenzen=never:'+
        'r;issho_ni=together:r;koko=here:r;soko=there:r;asoko=there:r;taberu=e'+
        'at:v1;nomu=drink:v5;iku=go:v5;kuru=come:vk;suru=do:vs;miru=see:v1;kik'+
        'u=listen:v5;',
    'yomu=read:v5;kaku=write:v5;hanasu=speak:v5;au=meet:v5;kau=buy:v5;uru=sell'+
        ':v5;aruku=walk:v5;hashiru=run:v5;oyogu=swim:v5;neru=sleep:v1;okiru=ge'+
        't_up:v1;hataraku=work:v5;wakaru=understand:v5;shiru=know:v5;omou=thin'+
        'k:v5;iu=say:v5;',
    'kangaeru=think:v1;oshieru=teach:v1;narau=learn:v5;tsukuru=make:v5;tsukau='+
        'use:v5;motsu=have:v5;miseru=show:v1;ageru=give:v1;kureru=give:v1;mora'+
        'u=receive:v5;toru=take:v5;okuru=send:v5;yobu=call:v5;matsu=wait:v5;su'+
        'waru=sit:v5;',
    'hairu=enter:v5;deru=leave:v1;kaeru=return:v5;tsuku=arrive:v5;noru=ride:v5'+
        ';oriru=get_off:v1;akeru=open:v1;shimeru=close:v1;hajimeru=begin:v1;ow'+
        'aru=end:v5;wasureru=forget:v1;oboeru=remember:v1;sagasu=look_for:v5;m'+
        'itsukeru=find:v1;',
    'tetsudau=help:v5;utau=sing:v5;odoru=dance:v5;asobu=play:v5;warau=laugh:v5'+
        ';naku=cry:v5;shinu=die:v5;sumu=live:v5;naru=become:v5;dekiru=can:v1;a'+
        'rau=wash:v5;kiru=wear:v1;aru=exist:va;iru=exist:vi;kaeru=return:v5;be'+
        'nkyou=study:s;',
    'ryokou=travel:s;sanpo=walk:s;kekkon=marry:s;shokuji=eat:s;sentaku=wash:s;'+
        'souji=clean:s;kaimono=shop:s;unten=drive:s;ookii=big:ia;chiisai=small'+
        ':ia;atarashii=new:ia;furui=old:ia;takai=expensive:ia;yasui=cheap:ia;i'+
        'i=good:ii;',
    'yoi=good:ii;warui=bad:ia;atsui=hot:ia;samui=cold:ia;tsumetai=cold:ia;oish'+
        'ii=delicious:ia;tanoshii=pleasant:ia;omoshiroi=interesting:ia;muzukas'+
        'hii=difficult:ia;yasashii=easy:ia;isogashii=busy:ia;ureshii=happy:ia;'+
        'kanashii=sad:ia;',
    'nagai=long:ia;mijikai=short:ia;hayai=fast:ia;osoi=slow:ia;hiroi=large:ia;'+
        'semai=small:ia;tooi=far:ia;chikai=near:ia;akai=red:ia;aoi=blue:ia;shi'+
        'roi=white:ia;kuroi=black:ia;wakai=young:ia;kawaii=pretty:ia;utsukushi'+
        'i=beautiful:ia;',
    'itai=painful:ia;hoshii=want:ia;kirei=beautiful:na;shizuka=quiet:na;nigiya'+
        'ka=lively:na;yuumei=famous:na;genki=happy:na;suki=like:na;kirai=disli'+
        'ke:na;daisuki=love:na;jouzu=good:na;heta=bad:na;benri=convenient:na;',
    'taisetsu=important:na;daijoubu=fine:na;hima=free:na;kantan=easy:na;shinse'+
        'tsu=kind:na;nan=what:w;nani=what:w;dare=who:w;donata=who:w;doko=where'+
        ':w;itsu=when:w;naze=why:w;doushite=why:w;dou=how:w;ikura=how_much:w;i'+
        'kutsu=how_many:w;',
    'dono=which:w;konnichiwa=Hello:j;ohayou=Good_morning:j;ohayou_gozaimasu=Go'+
        'od_morning:j;konbanwa=Good_evening:j;oyasumi_nasai=Good_night:j;sayou'+
        'nara=Goodbye:j;arigatou=Thank_you:j;arigatou_gozaimasu=Thank_you:j;',
    'sumimasen=Excuse_me:j;gomen_nasai=Sorry:j;hai=Yes:j;iie=No:j;kudasai=plea'+
        'se:j;onegai_shimasu=please:j;furu=rain:v5;nihon=Japan:n;amerika=Ameri'+
        'ca:n;furansu=France:n;doitsu=Germany:n;igirisu=England:n;chuugoku=Chi'+
        'na:n;',
    'kankoku=Korea:n;toukyou=Tokyo:n;kyouto=Kyoto:n;oosaka=Osaka:n;pari=Paris:'+
        'n;rondon=London:n;nyuuyooku=New_York:n;shukudai=homework:n;kyoushitsu'+
        '=classroom:n;tsukue=desk:n;pen=pen:n;enpitsu=pencil:n;kamera=camera:n'+
        ';',
    'terebi=television:n;rajio=radio:n;heya=room:n;biru=building:n;yuubinkyoku'+
        '=post_office:n;kouban=police_station:n;yakkyoku=pharmacy:n;supa=super'+
        'market:n;kissaten=cafe:n;mise=shop:n;kirai=dislike:na;yasumu=rest:v5;'+
        'tomaru=stop:v5;',
    'tsutomeru=work:v1;kariru=borrow:v1;kasu=lend:v5;kaesu=return:v5;tazuneru='+
        'ask:v1;kotaeru=answer:v1;warau=laugh:v5;nemuru=sleep:v5;dochira=where'+
        ':w;dore=which:w;'
  );

  { Espagnol : espagnol=anglais:categorie }
  SpData: Array[0..31] Of String = (
    'hombre=man:n;mujer=woman:n;chico=boy:n;chica=girl:n;niño=child:n;niña=g'+
        'irl:n;amigo=friend:n;amiga=friend:n;familia=family:n;madre=mother:n;p'+
        'adre=father:n;hermano=brother:n;hermana=sister:n;hijo=son:n;hija=daug'+
        'hter:n;',
    'marido=husband:n;esposa=wife:n;esposo=husband:n;profesor=teacher:n;profes'+
        'ora=teacher:n;estudiante=student:n;medico=doctor:n;enfermera=nurse:n;'+
        'persona=person:n;gente=people:n;señor=gentleman:n;señora=lady:n;cas'+
        'a=house:n;',
    'habitacion=room:n;cocina=kitchen:n;baño=bathroom:n;dormitorio=bedroom:n;'+
        'puerta=door:n;ventana=window:n;mesa=table:n;silla=chair:n;cama=bed:n;'+
        'escuela=school:n;colegio=school:n;universidad=university:n;clase=clas'+
        's:n;',
    'oficina=office:n;tienda=shop:n;mercado=market:n;restaurante=restaurant:n;'+
        'hotel=hotel:n;hospital=hospital:n;biblioteca=library:n;banco=bank:n;a'+
        'eropuerto=airport:n;estacion=station:n;museo=museum:n;iglesia=church:'+
        'n;cine=cinema:n;',
    'teatro=theater:n;playa=beach:n;mar=sea:n;rio=river:n;montaña=mountain:n;'+
        'bosque=forest:n;jardin=garden:n;parque=park:n;arbol=tree:n;flor=flowe'+
        'r:n;ciudad=city:n;pueblo=village:n;pais=country:n;calle=street:n;carr'+
        'etera=road:n;',
    'mundo=world:n;agua=water:n;comida=food:n;pan=bread:n;queso=cheese:n;carne'+
        '=meat:n;pescado=fish:n;manzana=apple:n;naranja=orange:n;platano=banan'+
        'a:n;huevo=egg:n;arroz=rice:n;ensalada=salad:n;sopa=soup:n;pastel=cake'+
        ':n;cafe=coffee:n;',
    'te=tea:n;leche=milk:n;vino=wine:n;cerveza=beer:n;zumo=juice:n;jugo=juice:'+
        'n;desayuno=breakfast:n;almuerzo=lunch:n;cena=dinner:n;dinero=money:n;'+
        'precio=price:n;libro=book:n;carta=letter:n;periodico=newspaper:n;revi'+
        'sta=magazine:n;',
    'telefono=phone:n;ordenador=computer:n;computadora=computer:n;coche=car:n;'+
        'carro=car:n;autobus=bus:n;tren=train:n;avion=plane:n;bicicleta=bike:n'+
        ';barco=boat:n;nombre=name:n;idioma=language:n;palabra=word:n;pregunta'+
        '=question:n;',
    'respuesta=answer:n;problema=problem:n;idea=idea:n;historia=story:n;cosa=t'+
        'hing:n;lugar=place:n;trabajo=work:n;tiempo=time:n;dia=day:n;noche=nig'+
        'ht:n;semana=week:n;mes=month:n;año=year:n;hora=hour:n;minuto=minute:'+
        'n;musica=music:n;',
    'cancion=song:n;pelicula=movie:n;juego=game:n;regalo=gift:n;fiesta=party:n'+
        ';viaje=trip:n;billete=ticket:n;boleto=ticket:n;maleta=suitcase:n;llav'+
        'e=key:n;bolsa=bag:n;perro=dog:n;gato=cat:n;pajaro=bird:n;caballo=hors'+
        'e:n;',
    'animal=animal:n;lluvia=rain:n;nieve=snow:n;viento=wind:n;sol=sun:n;luna=m'+
        'oon:n;cielo=sky:n;cabeza=head:n;mano=hand:n;pie=foot:n;ojo=eye:n;boca'+
        '=mouth:n;cara=face:n;corazon=heart:n;camisa=shirt:n;vestido=dress:n;z'+
        'apato=shoe:n;',
    'sombrero=hat:n;lunes=Monday:n;martes=Tuesday:n;miercoles=Wednesday:n;juev'+
        'es=Thursday:n;viernes=Friday:n;sabado=Saturday:n;domingo=Sunday:n;ene'+
        'ro=January:n;febrero=February:n;marzo=March:n;abril=April:n;mayo=May:'+
        'n;junio=June:n;',
    'julio=July:n;agosto=August:n;septiembre=September:n;octubre=October:n;nov'+
        'iembre=November:n;diciembre=December:n;español=Spanish:n;ingles=Engl'+
        'ish:n;frances=French:n;aleman=German:n;italiano=Italian:n;chino=Chine'+
        'se:n;',
    'japones=Japanese:n;portugues=Portuguese:n;ruso=Russian:n;arabe=Arabic:n;b'+
        'ueno=good:a;malo=bad:a;grande=big:a;pequeño=small:a;largo=long:a;cor'+
        'to=short:a;alto=tall:a;bajo=short:a;nuevo=new:a;viejo=old:a;joven=you'+
        'ng:a;',
    'bonito=pretty:a;bello=beautiful:a;hermoso=beautiful:a;feliz=happy:a;trist'+
        'e=sad:a;cansado=tired:a;enfermo=sick:a;caliente=hot:a;frio=cold:a;car'+
        'o=expensive:a;barato=cheap:a;facil=easy:a;dificil=difficult:a;importa'+
        'nte=important:a;',
    'interesante=interesting:a;aburrido=boring:a;divertido=funny:a;rapido=fast'+
        ':a;lento=slow:a;fuerte=strong:a;debil=weak:a;rico=rich:a;pobre=poor:a'+
        ';limpio=clean:a;sucio=dirty:a;lleno=full:a;vacio=empty:a;abierto=open'+
        ':a;',
    'cerrado=closed:a;listo=ready:a;ocupado=busy:a;libre=free:a;rojo=red:a;azu'+
        'l=blue:a;verde=green:a;amarillo=yellow:a;negro=black:a;blanco=white:a'+
        ';marron=brown:a;gris=gray:a;rosa=pink:a;mismo=same:a;diferente=differ'+
        'ent:a;otro=other:a;',
    'primero=first:a;ultimo=last:a;proximo=next:a;delicioso=delicious:a;tranqu'+
        'ilo=quiet:a;amable=kind:a;simpatico=nice:a;contento=glad:a;preocupado'+
        '=worried:a;seguro=sure:a;verdadero=true:a;famoso=famous:a;popular=pop'+
        'ular:a;',
    'especial=special:a;cansada=tired:a;calor=hot:a;orgulloso=proud:a;nervioso'+
        '=nervous:a;perdido=lost:a;ser=be:v;estar=be:v;tener=have:v;haber=have'+
        ':v;hacer=do:v;ir=go:v;venir=come:v;ver=see:v;mirar=look_at:v;oir=hear'+
        ':v;',
    'escuchar=listen_to:v;hablar=speak:v;decir=say:v;preguntar=ask:v;responder'+
        '=answer:v;entender=understand:v;saber=know:v;conocer=know:v;pensar=th'+
        'ink:v;creer=believe:v;sentir=feel:v;encontrar=find:v;dar=give:v;tomar'+
        '=take:v;',
    'poner=put:v;comprar=buy:v;vender=sell:v;pagar=pay:v;comer=eat:v;beber=dri'+
        'nk:v;cocinar=cook:v;dormir=sleep:v;vivir=live:v;trabajar=work:v;estud'+
        'iar=study:v;aprender=learn:v;enseñar=teach:v;leer=read:v;escribir=wr'+
        'ite:v;jugar=play:v;',
    'cantar=sing:v;bailar=dance:v;correr=run:v;caminar=walk:v;conducir=drive:v'+
        ';viajar=travel:v;llegar=arrive:v;salir=leave:v;quedar=stay:v;volver=r'+
        'eturn:v;empezar=begin:v;terminar=finish:v;abrir=open:v;cerrar=close:v'+
        ';ayudar=help:v;',
    'esperar=wait:v;llamar=call:v;visitar=visit:v;usar=use:v;olvidar=forget:v;'+
        'elegir=choose:v;decidir=decide:v;disfrutar=enjoy:v;enviar=send:v;trae'+
        'r=bring:v;llevar=carry:v;costar=cost:v;cambiar=change:v;parecer=seem:'+
        'v;mostrar=show:v;',
    'nadar=swim:v;reir=laugh:v;llorar=cry:v;morir=die:v;ganar=win:v;perder=los'+
        'e:v;romper=break:v;construir=build:v;entrar=enter:v;lavar=wash:v;limp'+
        'iar=clean:v;cortar=cut:v;preferir=prefer:v;recordar=remember:v;recibi'+
        'r=receive:v;',
    'pedir=ask_for:v;querer=want:v;poder=can:v;deber=must:v;necesitar=need:v;g'+
        'ustar=like:v;llover=rain:v;nacer=be_born:v;amar=love:v;odiar=hate:v;m'+
        'uy=very:r;tambien=also:r;siempre=always:r;nunca=never:r;a_menudo=ofte'+
        'n:r;',
    'a_veces=sometimes:r;ahora=now:r;hoy=today:r;ayer=yesterday:r;anoche=last_'+
        'night:r;aqui=here:r;alli=there:r;ahi=there:r;bien=well:r;mal=badly:r;'+
        'despacio=slowly:r;otra_vez=again:r;pronto=soon:r;ya=already:r;todavia'+
        '=still:r;',
    'solo=only:r;realmente=really:r;mucho=a_lot:r;poco=a_little:r;juntos=toget'+
        'her:r;casi=almost:r;entonces=then:r;temprano=early:r;tarde=late:r;lej'+
        'os=far:r;cerca=near:r;fuera=outside:r;dentro=inside:r;quizas=maybe:r;'+
        'claro=of_course:r;',
    'por_supuesto=of_course:r;mañana=tomorrow:r;a=to:p;de=of:p;en=in:p;con=wi'+
        'th:p;sin=without:p;por=for:p;para=for:p;sobre=on:p;entre=between:p;ha'+
        'sta=until:p;desde=from:p;hacia=toward:p;durante=during:p;antes_de=bef'+
        'ore:p;',
    'despues_de=after:p;cerca_de=near:p;lejos_de=far_from:p;delante_de=in_fron'+
        't_of:p;detras_de=behind:p;debajo_de=under:p;encima_de=above:p;al_lado'+
        '_de=next_to:p;dentro_de=inside:p;fuera_de=out_of:p;y=and:c;e=and:c;o='+
        'or:c;u=or:c;',
    'pero=but:c;porque=because:c;si=if:c;cuando=when:c;mientras=while:c;que=th'+
        'at:c;aunque=although:c;como=as:c;hola=Hello:j;adios=Goodbye:j;gracias'+
        '=Thank_you:j;por_favor=please:j;perdon=Excuse_me:j;lo_siento=I_am_sor'+
        'ry:j;',
    'buenos_dias=Good_morning:j;buenas_tardes=Good_afternoon:j;buenas_noches=G'+
        'ood_night:j;bienvenido=Welcome:j;felicidades=Congratulations:j;salud='+
        'Cheers:j;que=what:w;quien=who:w;donde=where:w;cuando=when:w;como=how:'+
        'w;por_que=why:w;',
    'cuanto=how_much:w;cuantos=how_many:w;cual=which:w;pizza=pizza:n;paris=Par'+
        'is:n;esta_noche=tonight:r;esta_mañana=this morning:r;esta_tarde=this'+
        ' afternoon:r;hoy_en_dia=nowadays:r;'
  );

  { Albanais : albanais=anglais:categorie }
  AlData: Array[0..32] Of String = (
    'burrë/burra=man:nm;grua/gra=woman:nf;djalë/djem=boy:nm;vajzë/vajza=gir'+
        'l:nf;fëmijë/fëmijë=child:nm;shok/shokë=friend:nm;shoqe/shoqe=fri'+
        'end:nf;mik/miq=friend:nm;familje/familje=family:nf;nënë/nëna=mothe'+
        'r:nf;',
    'baba/baballarë=father:nm;vëlla/vëllezër=brother:nm;motër/motra=siste'+
        'r:nf;bir/bij=son:nm;bijë/bija=daughter:nf;mësues/mësues=teacher:nm'+
        ';mësuese/mësuese=teacher:nf;nxënës/nxënës=student:nm;student/st'+
        'udentë=student:nm;',
    'mjek/mjekë=doctor:nm;infermiere/infermiere=nurse:nf;person/persona=perso'+
        'n:nm;njeri/njerëz=person:nm;shtëpi/shtëpi=house:nf;dhomë/dhoma=ro'+
        'om:nf;kuzhinë/kuzhina=kitchen:nf;banjë/banja=bathroom:nf;derë/dyer'+
        '=door:nf;',
    'dritare/dritare=window:nf;tavolinë/tavolina=table:nf;karrige/karrige=cha'+
        'ir:nf;krevat/krevate=bed:nm;shkollë/shkolla=school:nf;universitet/un'+
        'iversitete=university:nm;klasë/klasa=class:nf;zyrë/zyra=office:nf;',
    'dyqan/dyqane=shop:nm;treg/tregje=market:nm;restorant/restorante=restauran'+
        't:nm;hotel/hotele=hotel:nm;spital/spitale=hospital:nm;bibliotekë/bib'+
        'lioteka=library:nf;bankë/banka=bank:nf;aeroport/aeroporte=airport:nm'+
        ';',
    'stacion/stacione=station:nm;muze/muze=museum:nm;kishë/kisha=church:nf;ki'+
        'nema/kinema=cinema:nf;plazh/plazhe=beach:nm;det/dete=sea:nm;lumë/lum'+
        'enj=river:nm;mal/male=mountain:nm;pyll/pyje=forest:nm;kopsht/kopshte='+
        'garden:nm;',
    'park/parqe=park:nm;pemë/pemë=tree:nf;lule/lule=flower:nf;qytet/qytete=c'+
        'ity:nm;fshat/fshatra=village:nm;vend/vende=country:nm;rrugë/rrugë=s'+
        'treet:nf;botë/botë=world:nf;ujë/ujëra=water:nm;ushqim/ushqime=foo'+
        'd:nm;',
    'bukë/bukë=bread:nf;djathë/djathëra=cheese:nm;mish/mishra=meat:nm;pesh'+
        'k/peshq=fish:nm;mollë/mollë=apple:nf;portokall/portokaj=orange:nm;b'+
        'anane/banane=banana:nf;vezë/vezë=egg:nf;oriz/orizra=rice:nm;sallat'+
        'ë/sallata=salad:nf;',
    'supë/supa=soup:nf;kafe/kafe=coffee:nf;çaj/çajra=tea:nm;qumësht/qumës'+
        'htra=milk:nm;verë/vera=wine:nf;birrë/birra=beer:nf;lëng/lëngje=ju'+
        'ice:nm;mëngjes/mëngjese=breakfast:nm;drekë/dreka=lunch:nf;darkë/d'+
        'arka=dinner:nf;',
    'para/para=money:nf;çmim/çmime=price:nm;libër/libra=book:nm;letër/letr'+
        'a=letter:nf;gazetë/gazeta=newspaper:nf;revistë/revista=magazine:nf;'+
        'telefon/telefona=phone:nm;kompjuter/kompjutera=computer:nm;makinë/ma'+
        'kina=car:nf;',
    'autobus/autobusë=bus:nm;tren/trena=train:nm;avion/avionë=plane:nm;biçi'+
        'kletë/biçikleta=bike:nf;anije/anije=boat:nf;emër/emra=name:nm;gjuh'+
        'ë/gjuhë=language:nf;fjalë/fjalë=word:nf;pyetje/pyetje=question:nf'+
        ';',
    'përgjigje/përgjigje=answer:nf;problem/probleme=problem:nm;ide/ide=idea:'+
        'nf;histori/histori=story:nf;gjë/gjëra=thing:nf;punë/punë=work:nf;'+
        'kohë/kohë=time:nf;ditë/ditë=day:nf;natë/net=night:nf;javë/javë'+
        '=week:nf;',
    'muaj/muaj=month:nm;vit/vite=year:nm;orë/orë=hour:nf;minutë/minuta=minu'+
        'te:nf;muzikë/muzika=music:nf;këngë/këngë=song:nf;film/filma=movi'+
        'e:nm;lojë/lojëra=game:nf;dhuratë/dhurata=gift:nf;festë/festa=part'+
        'y:nf;',
    'udhëtim/udhëtime=trip:nm;biletë/bileta=ticket:nf;valixhe/valixhe=suitc'+
        'ase:nf;çelës/çelësa=key:nm;çantë/çanta=bag:nf;qen/qen=dog:nm;m'+
        'ace/mace=cat:nf;zog/zogj=bird:nm;kalë/kuaj=horse:nm;kafshë/kafshë='+
        'animal:nf;',
    'shi/shira=rain:nm;borë/borëra=snow:nf;erë/erëra=wind:nf;diell/diej=su'+
        'n:nm;hënë/hëna=moon:nf;qiell/qiej=sky:nm;kokë/koka=head:nf;dorë/'+
        'duar=hand:nf;këmbë/këmbë=foot:nf;sy/sy=eye:nm;gojë/gojë=mouth:n'+
        'f;',
    'fytyrë/fytyra=face:nf;zemër/zemra=heart:nf;këmishë/këmisha=shirt:nf;'+
        'fustan/fustane=dress:nm;këpucë/këpucë=shoe:nf;kapelë/kapela=hat:'+
        'nf;janar=January:nm;shkurt=February:nm;mars=March:nm;prill=April:nm;m'+
        'aj=May:nm;',
    'qershor=June:nm;korrik=July:nm;gusht=August:nm;shtator=September:nm;tetor'+
        '=October:nm;nëntor=November:nm;dhjetor=December:nm;e_hënë=Monday:n'+
        'f;e_martë=Tuesday:nf;e_mërkurë=Wednesday:nf;e_enjte=Thursday:nf;e_'+
        'premte=Friday:nf;',
    'e_shtunë=Saturday:nf;e_diel=Sunday:nf;shqip=Albanian:nm;anglisht=English'+
        ':nm;frëngjisht=French:nm;gjermanisht=German:nm;italisht=Italian:nm;s'+
        'panjisht=Spanish:nm;rusisht=Russian:nm;greqisht=Greek:nm;turqisht=Tur'+
        'kish:nm;',
    'madh/mëdhenj=big:a;vogël/vegjël=small:a;bukur=beautiful:a;mirë=good:a'+
        ';keq/këqij=bad:a;gjatë=long:a;shkurtër=short:a;ri/rinj=new:a;vjet'+
        'ër=old:a;lumtur=happy:a;trishtuar=sad:a;lodhur=tired:a;sëmurë=sick'+
        ':a;nxehtë=hot:a;',
    'ftohtë=cold:a;shtrenjtë=expensive:a;lirë=cheap:a;lehtë=easy:a;vështi'+
        'rë=difficult:a;rëndësishëm=important:a;interesant=interesting:a;s'+
        'hpejtë=fast:a;ngadaltë=slow:a;fortë=strong:a;dobët=weak:a;pasur=r'+
        'ich:a;',
    'varfër=poor:a;pastër=clean:a;pistë=dirty:a;plotë=full:a;bosh=empty:a;'+
        'hapur=open:a;mbyllur=closed:a;gati=ready:a;zënë=busy:a;kuq=red:a;bl'+
        'u=blue:a;jeshil=green:a;verdhë=yellow:a;zi=black:a;bardhë=white:a;g'+
        'ri=gray:a;',
    'rozë=pink:a;njëjtë=same:a;ndryshëm=different:a;tjetër=other:a;parë='+
        'first:a;fundit=last:a;ardhshëm=next:a;shijshëm=delicious:a;qetë=qu'+
        'iet:a;sjellshëm=kind:a;këndshëm=nice:a;kënaqur=glad:a;shqetësuar'+
        '=worried:a;',
    'sigurt=sure:a;vërtetë=true:a;famshëm=famous:a;veçantë=special:a;urit'+
        'ur=hungry:a;etur=thirsty:a;jam=be:vi:ser;kam=have:vi:tener;vij=come:v'+
        'i:venir;shkoj=go:vo:ir;bëj=do:vi:hacer;them=say:vi:decir;shoh=see:vi'+
        ':ver;',
    'di=know:vi:saber;dua=want:vi:querer;mund=can:vi:poder;duhet=must:vi:deber'+
        ';jap=give:vi:dar;marr=take:vi:tomar;dal=leave:vi:salir;hyj=enter:vi:e'+
        'ntrar;ha=eat:vi:comer;pi=drink:vi:beber;fle=sleep:vi:dormir;flas=spea'+
        'k:vi:hablar;',
    'shkruaj=write:vj:escribir;punoj=work:vo;lexoj=read:vo;jetoj=live:vo;banoj'+
        '=live:vo;studioj=study:vo;mësoj=learn:vo;ndihmoj=help:vo;telefonoj=c'+
        'all:vo;vizitoj=visit:vo;harroj=forget:vo;kujtoj=remember:vo;mendoj=th'+
        'ink:vo;',
    'besoj=believe:vo;vrapoj=run:vo;notoj=swim:vo;këndoj=sing:vo;udhëtoj=tra'+
        'vel:vo;pëlqej=like:vo:gustar;dashuroj=love:vo;urrej=hate:vj;pres=wai'+
        't:vc;hap=open:vc;mbyll=close:vc;përdor=use:vc;shes=sell:vc;ec=walk:v'+
        'c;',
    'ndërroj=change:vo;fitoj=win:vo;ndërtoj=build:vo;pastroj=clean:vo;lanë='+
        'wash:vc;pyes=ask:vc;përgjigjem=answer:vc;blej=buy:vj;gjej=find:vj;k'+
        'ërcej=dance:vj;mbërrij=arrive:vj;qesh=laugh:vc;qaj=cry:vj;në=in:p;'+
        'me=with:p;',
    'nga=from:p;për=for:p;te=at:p;tek=at:p;pa=without:p;mbi=on:p;nën=under:p'+
        ';pas=after:p;para=before:p;deri=until:p;gjatë=during:p;midis=between'+
        ':p;afër=near:p;larg=far_from:p;përpara=in_front_of:p;prapa=behind:p'+
        ';brenda=inside:p;',
    'jashtë=outside:p;rreth=around:p;sipas=according_to:p;drejt=toward:p;dhe='+
        'and:c;por=but:c;ose=or:c;sepse=because:c;që=that:c;nëse=if:c;kur=wh'+
        'en:c;ndërsa=while:c;megjithëse=although:c;shumë=very:r;gjithmonë='+
        'always:r;',
    'kurrë=never:r;shpesh=often:r;ndonjëherë=sometimes:r;tani=now:r;sot=tod'+
        'ay:r;nesër=tomorrow:r;dje=yesterday:r;sonte=tonight:r;këtu=here:r;a'+
        'tje=there:r;ngadalë=slowly:r;përsëri=again:r;shpejt=soon:r;tashmë'+
        '=already:r;',
    'ende=still:r;vetëm=only:r;vërtet=really:r;bashkë=together:r;pothuajse='+
        'almost:r;atëherë=then:r;herët=early:r;vonë=late:r;larg=far:r;ndos'+
        'hta=maybe:r;sigurisht=of_course:r;pak=a_little:r;edhe=also:r;përshë'+
        'ndetje=Hello:j;',
    'mirëdita=Hello:j;mirëmëngjes=Good_morning:j;mirëmbrëma=Good_evening:'+
        'j;mirupafshim=Goodbye:j;faleminderit=Thank_you:j;ju_lutem=please:j;m'+
        'ë_falni=Excuse_me:j;po=Yes:j;jo=No:j;urime=Congratulations:j;gëzuar'+
        '=Cheers:j;',
    'mirë_se_vini=Welcome:j;ku=where:w;kur=when:w;pse=why:w;si=how:w;çfarë='+
        'what:w;kush=who:w;sa=how_many:w;cili=which:w;cila=which:w;quhem=be_ca'+
        'lled:vi:llamar;luaj=play:vj;'
  );

  { Portugais : portugais=anglais:categorie }
  PtData: Array[0..34] Of String = (
    'homem/homens=man:n;mulher/mulheres=woman:n;rapaz/rapazes=boy:n;rapariga=g'+
        'irl:n;menina=girl:n;menino=boy:n;criança=child:n;amigo=friend:n;amig'+
        'a=friend:n;família=family:n;mãe/mães=mother:n;pai=father:n;irmão/'+
        'irmãos=brother:n;',
    'irmã=sister:n;filho=son:n;filha=daughter:n;marido=husband:n;esposa=wife:'+
        'n;professor/professores=teacher:n;professora=teacher:n;estudante=stud'+
        'ent:n;aluno=student:n;médico=doctor:n;enfermeira=nurse:n;pessoa=pers'+
        'on:n;',
    'gente=people:n;senhor/senhores=gentleman:n;senhora=lady:n;casa=house:n;qu'+
        'arto=room:n;cozinha=kitchen:n;casa_de_banho=bathroom:n;banheiro=bathr'+
        'oom:n;porta=door:n;janela=window:n;mesa=table:n;cadeira=chair:n;cama='+
        'bed:n;',
    'escola=school:n;universidade=university:n;aula=class:n;escritório=office'+
        ':n;loja=shop:n;mercado=market:n;restaurante=restaurant:n;hotel/hotéi'+
        's=hotel:n;hospital/hospitais=hospital:n;biblioteca=library:n;banco=ba'+
        'nk:n;',
    'aeroporto=airport:n;estação/estações=station:n;museu=museum:n;igreja='+
        'church:n;cinema=cinema:n;teatro=theater:n;praia=beach:n;mar/mares=sea'+
        ':n;rio=river:n;montanha=mountain:n;floresta=forest:n;jardim/jardins=g'+
        'arden:n;',
    'parque=park:n;árvore=tree:n;flor/flores=flower:n;cidade=city:n;aldeia=vi'+
        'llage:n;país/países=country:n;rua=street:n;estrada=road:n;mundo=wor'+
        'ld:n;água=water:n;comida=food:n;pão/pães=bread:n;queijo=cheese:n;c'+
        'arne=meat:n;',
    'peixe=fish:n;maçã=apple:n;laranja=orange:n;banana=banana:n;ovo=egg:n;ar'+
        'roz=rice:n;salada=salad:n;sopa=soup:n;bolo=cake:n;café=coffee:n;chá'+
        '=tea:n;leite=milk:n;vinho=wine:n;cerveja=beer:n;sumo=juice:n;suco=jui'+
        'ce:n;',
    'pequeno-almoço=breakfast:n;almoço=lunch:n;jantar=dinner:n;dinheiro=mone'+
        'y:n;preço=price:n;livro=book:n;carta=letter:n;jornal/jornais=newspap'+
        'er:n;revista=magazine:n;telefone=phone:n;computador=computer:n;carro='+
        'car:n;',
    'autocarro=bus:n;ônibus=bus:n;comboio=train:n;trem/trens=train:n;avião/a'+
        'viões=plane:n;bicicleta=bike:n;barco=boat:n;nome=name:n;língua=lang'+
        'uage:n;idioma=language:n;palavra=word:n;pergunta=question:n;resposta='+
        'answer:n;',
    'problema=problem:n;ideia=idea:n;história=story:n;coisa=thing:n;lugar/lug'+
        'ares=place:n;trabalho=work:n;tempo=time:n;dia=day:n;noite=night:n;sem'+
        'ana=week:n;mês/meses=month:n;ano=year:n;hora=hour:n;minuto=minute:n;'+
        'música=music:n;',
    'canção/canções=song:n;filme=movie:n;jogo=game:n;presente=gift:n;festa'+
        '=party:n;viagem/viagens=trip:n;bilhete=ticket:n;mala=suitcase:n;chave'+
        '=key:n;saco=bag:n;cão/cães=dog:n;cachorro=dog:n;gato=cat:n;pássaro'+
        '=bird:n;',
    'cavalo=horse:n;animal/animais=animal:n;chuva=rain:n;neve=snow:n;vento=win'+
        'd:n;sol/sóis=sun:n;lua=moon:n;céu=sky:n;cabeça=head:n;mão/mãos=h'+
        'and:n;pé=foot:n;olho=eye:n;boca=mouth:n;cara=face:n;coração/coraç'+
        'ões=heart:n;',
    'camisa=shirt:n;vestido=dress:n;sapato=shoe:n;chapéu=hat:n;segunda-feira='+
        'Monday:n;terça-feira=Tuesday:n;quarta-feira=Wednesday:n;quinta-feira'+
        '=Thursday:n;sexta-feira=Friday:n;sábado=Saturday:n;domingo=Sunday:n;'+
        'janeiro=January:n;',
    'fevereiro=February:n;março=March:n;abril=April:n;maio=May:n;junho=June:n'+
        ';julho=July:n;agosto=August:n;setembro=September:n;outubro=October:n;'+
        'novembro=November:n;dezembro=December:n;português=Portuguese:n;ingl'+
        'ês=English:n;',
    'francês=French:n;alemão=German:n;espanhol=Spanish:n;italiano=Italian:n;'+
        'chinês=Chinese:n;japonês=Japanese:n;russo=Russian:n;árabe=Arabic:n'+
        ';bom/boa/bons/boas=good:a;mau/má/maus/más=bad:a;grande=big:a;pequen'+
        'o=small:a;',
    'longo=long:a;curto=short:a;alto=tall:a;baixo=short:a;novo=new:a;velho=old'+
        ':a;jovem=young:a;bonito=pretty:a;belo=beautiful:a;lindo=beautiful:a;f'+
        'eliz=happy:a;triste=sad:a;cansado=tired:a;doente=sick:a;quente=hot:a;'+
        'frio=cold:a;',
    'caro=expensive:a;barato=cheap:a;fácil=easy:a;difícil=difficult:a;import'+
        'ante=important:a;interessante=interesting:a;aborrecido=boring:a;diver'+
        'tido=funny:a;rápido=fast:a;lento=slow:a;forte=strong:a;fraco=weak:a;'+
        'rico=rich:a;',
    'pobre=poor:a;limpo=clean:a;sujo=dirty:a;cheio=full:a;vazio=empty:a;aberto'+
        '=open:a;fechado=closed:a;pronto=ready:a;ocupado=busy:a;livre=free:a;v'+
        'ermelho=red:a;azul=blue:a;verde=green:a;amarelo=yellow:a;preto=black:'+
        'a;branco=white:a;',
    'castanho=brown:a;cinzento=gray:a;rosa=pink:a;mesmo=same:a;diferente=diffe'+
        'rent:a;outro=other:a;primeiro=first:a;último=last:a;próximo=next:a;'+
        'delicioso=delicious:a;tranquilo=quiet:a;simpático=nice:a;contente=gl'+
        'ad:a;',
    'preocupado=worried:a;seguro=sure:a;verdadeiro=true:a;famoso=famous:a;espe'+
        'cial=special:a;calor=hot:a;orgulhoso=proud:a;nervoso=nervous:a;perdid'+
        'o=lost:a;ser=be:v:ser;estar=be:v:estar;ter=have:v:tener;haver=have:v:'+
        'haber;',
    'fazer=do:v:hacer;ir=go:v:ir;vir=come:v:venir;ver=see:v:ver;olhar=look_at:'+
        'v;ouvir=hear:v;escutar=listen_to:v;falar=speak:v;dizer=say:v:decir;pe'+
        'rguntar=ask:v;responder=answer:v;entender=understand:v;compreender=un'+
        'derstand:v;',
    'saber=know:v:saber;conhecer=know:v::conheço;pensar=think:v;acreditar=bel'+
        'ieve:v;sentir=feel:v::sinto;encontrar=find:v;dar=give:v:dar;tomar=tak'+
        'e:v;colocar=put:v;comprar=buy:v;vender=sell:v;pagar=pay:v;comer=eat:v'+
        ';beber=drink:v;',
    'cozinhar=cook:v;dormir=sleep:v::durmo;viver=live:v;morar=live:v;trabalhar'+
        '=work:v;estudar=study:v;aprender=learn:v;ensinar=teach:v;ler=read:v;e'+
        'screver=write:v;jogar=play:v;cantar=sing:v;dançar=dance:v;correr=run'+
        ':v;caminhar=walk:v;',
    'andar=walk:v;conduzir=drive:v::conduzo;dirigir=drive:v::dirijo;viajar=tra'+
        'vel:v;chegar=arrive:v;sair=leave:v;ficar=stay:v;voltar=return:v;come'+
        'çar=begin:v:empezar;terminar=finish:v;abrir=open:v;fechar=close:v;aj'+
        'udar=help:v;',
    'esperar=wait:v;chamar=call:v:llamar;visitar=visit:v;usar=use:v;esquecer=f'+
        'orget:v;escolher=choose:v;decidir=decide:v;aproveitar=enjoy:v;enviar='+
        'send:v;trazer=bring:v;levar=carry:v;custar=cost:v;mudar=change:v;',
    'parecer=seem:v::pareço;mostrar=show:v;nadar=swim:v;rir=laugh:v;chorar=cr'+
        'y:v;morrer=die:v::morro;ganhar=win:v;perder=lose:v::perco;quebrar=bre'+
        'ak:v;construir=build:v;entrar=enter:v;lavar=wash:v;limpar=clean:v;cor'+
        'tar=cut:v;',
    'preferir=prefer:v::prefiro;lembrar=remember:v;receber=receive:v;pedir=ask'+
        '_for:v::peço;querer=want:v:querer;poder=can:v:poder;dever=must:v:deb'+
        'er;precisar=need:v:necesitar;gostar=like:v;chover=rain:v:llover;nasce'+
        'r=be_born:v;',
    'amar=love:v;odiar=hate:v::odeio;acabar=finish:v:acabar;muito=very:r;tamb'+
        'ém=also:r;sempre=always:r;nunca=never:r;frequentemente=often:r;às_v'+
        'ezes=sometimes:r;agora=now:r;hoje=today:r;ontem=yesterday:r;aqui=here'+
        ':r;ali=there:r;',
    'lá=there:r;bem=well:r;mal=badly:r;devagar=slowly:r;de_novo=again:r;em_br'+
        'eve=soon:r;já=already:r;ainda=still:r;só=only:r;somente=only:r;real'+
        'mente=really:r;pouco=a_little:r;juntos=together:r;quase=almost:r;ent'+
        'ão=then:r;',
    'cedo=early:r;tarde=late:r;longe=far:r;perto=near:r;fora=outside:r;dentro='+
        'inside:r;talvez=maybe:r;claro=of_course:r;amanhã=tomorrow:r;esta_noi'+
        'te=tonight:r;hoje_à_noite=tonight:r;esta_manhã=this_morning:r;',
    'esta_tarde=this_afternoon:r;a=to:p;de=of:p;em=in:p;com=with:p;sem=without'+
        ':p;por=for:p;para=for:p;sobre=on:p;entre=between:p;até=until:p;desde'+
        '=from:p;durante=during:p;antes_de=before:p;depois_de=after:p;perto_de'+
        '=near:p;',
    'longe_de=far_from:p;em_frente_de=in_front_of:p;atrás_de=behind:p;debaixo'+
        '_de=under:p;em_cima_de=on_top_of:p;ao_lado_de=next_to:p;dentro_de=ins'+
        'ide:p;fora_de=out_of:p;e=and:c;ou=or:c;mas=but:c;porque=because:c;se='+
        'if:c;quando=when:c;',
    'enquanto=while:c;que=that:c;embora=although:c;olá=Hello:j;bom_dia=Good_m'+
        'orning:j;boa_tarde=Good_afternoon:j;boa_noite=Good_night:j;obrigado=T'+
        'hank_you:j;obrigada=Thank_you:j;por_favor=please:j;desculpe=Excuse_me'+
        ':j;',
    'desculpa=Excuse_me:j;adeus=Goodbye:j;parabéns=Congratulations:j;saúde=C'+
        'heers:j;bem-vindo=Welcome:j;que=what:w;quem=who:w;onde=where:w;quando'+
        '=when:w;como=how:w;por_que=why:w;quanto=how_much:w;quantos=how_many:w'+
        ';',
    'quantas=how_many:w;qual=which:w;pizza=pizza:n;brincar=play:v;o_que=what:w'+
        ';todos_os_dias=every_day:r;todo_o_dia=all_day:r;'
  );

  { Suedois : suedois=anglais:categorie }
  SvData: Array[0..46] Of String = (
    'pojke/pojkar=boy:nu;flicka/flickor=girl:nu;man/män=man:nu;kvinna/kvinnor'+
        '=woman:nu;barn/barn=child:nn;vän/vänner=friend:nu;familj/familjer=f'+
        'amily:nu;mamma/mammor=mother:nu;mor/mödrar=mother:nu;pappa/pappor=fa'+
        'ther:nu;',
    'far/fäder=father:nu;bror/bröder=brother:nu;syster/systrar=sister:nu;son'+
        '/söner=son:nu;dotter/döttrar=daughter:nu;make/makar=husband:nu;fru/'+
        'fruar=wife:nu;lärare/lärare=teacher:nu;student/studenter=student:nu'+
        ';',
    'elev/elever=student:nu;läkare/läkare=doctor:nu;sjuksköterska/sjuksköt'+
        'erskor=nurse:nu;person/personer=person:nu;människa/människor=person'+
        ':nu;hus/hus=house:nn;rum/rum=room:nn;kök/kök=kitchen:nn;badrum/badr'+
        'um=bathroom:nn;',
    'sovrum/sovrum=bedroom:nn;dörr/dörrar=door:nu;fönster/fönster=window:n'+
        'n;bord/bord=table:nn;stol/stolar=chair:nu;säng/sängar=bed:nu;skola/'+
        'skolor=school:nu;universitet/universitet=university:nn;klass/klasser='+
        'class:nu;',
    'kontor/kontor=office:nn;butik/butiker=shop:nu;marknad/marknader=market:nu'+
        ';restaurang/restauranger=restaurant:nu;hotell/hotell=hotel:nn;sjukhus'+
        '/sjukhus=hospital:nn;bibliotek/bibliotek=library:nn;bank/banker=bank:'+
        'nu;',
    'flygplats/flygplatser=airport:nu;station/stationer=station:nu;museum/muse'+
        'er=museum:nn;kyrka/kyrkor=church:nu;bio/biografer=cinema:nu;teater/te'+
        'atrar=theater:nu;strand/stränder=beach:nu;hav/hav=sea:nn;flod/floder'+
        '=river:nu;',
    'berg/berg=mountain:nn;skog/skogar=forest:nu;trädgård/trädgårdar=garde'+
        'n:nu;park/parker=park:nu;träd/träd=tree:nn;blomma/blommor=flower:nu'+
        ';stad/städer=city:nu;by/byar=village:nu;land/länder=country:nn;gata'+
        '/gator=street:nu;',
    'väg/vägar=road:nu;värld/världar=world:nu;vatten/vatten=water:nn;mat/m'+
        'at=food:nu;bröd/bröd=bread:nn;ost/ostar=cheese:nu;kött/kött=meat:'+
        'nn;fisk/fiskar=fish:nu;äpple/äpplen=apple:nn;apelsin/apelsiner=oran'+
        'ge:nu;',
    'banan/bananer=banana:nu;ägg/ägg=egg:nn;ris/ris=rice:nn;sallad/sallader='+
        'salad:nu;soppa/soppor=soup:nu;tårta/tårtor=cake:nu;kaffe/kaffe=coff'+
        'ee:nn;te/te=tea:nn;mjölk/mjölk=milk:nu;vin/viner=wine:nn;öl/öl=be'+
        'er:nn;',
    'juice/juicer=juice:nu;frukost/frukostar=breakfast:nu;lunch/luncher=lunch:'+
        'nu;middag/middagar=dinner:nu;pengar/pengar=money:nu;pris/priser=price'+
        ':nn;bok/böcker=book:nu;brev/brev=letter:nn;tidning/tidningar=newspap'+
        'er:nu;',
    'tidskrift/tidskrifter=magazine:nn;telefon/telefoner=phone:nu;dator/datore'+
        'r=computer:nu;bil/bilar=car:nu;buss/bussar=bus:nu;tåg/tåg=train:nn;'+
        'flygplan/flygplan=plane:nn;cykel/cyklar=bike:nu;båt/båtar=boat:nu;n'+
        'amn/namn=name:nn;',
    'språk/språk=language:nn;ord/ord=word:nn;fråga/frågor=question:nu;svar'+
        '/svar=answer:nn;problem/problem=problem:nn;idé/idéer=idea:nu;histor'+
        'ia/historier=story:nu;sak/saker=thing:nu;plats/platser=place:nu;arbet'+
        'e/arbeten=work:nn;',
    'jobb/jobb=job:nn;tid/tider=time:nu;dag/dagar=day:nu;natt/nätter=night:nu'+
        ';vecka/veckor=week:nu;månad/månader=month:nu;år/år=year:nn;timme/'+
        'timmar=hour:nu;minut/minuter=minute:nu;morgon/morgnar=morning:nu;',
    'kväll/kvällar=evening:nu;musik/musik=music:nu;sång/sånger=song:nu;fil'+
        'm/filmer=movie:nu;spel/spel=game:nn;present/presenter=gift:nu;fest/fe'+
        'ster=party:nu;resa/resor=trip:nu;biljett/biljetter=ticket:nu;',
    'resväska/resväskor=suitcase:nu;nyckel/nycklar=key:nu;väska/väskor=bag'+
        ':nu;hund/hundar=dog:nu;katt/katter=cat:nu;fågel/fåglar=bird:nu;häs'+
        't/hästar=horse:nu;djur/djur=animal:nn;regn/regn=rain:nn;snö/snö=sn'+
        'ow:nu;',
    'vind/vindar=wind:nu;sol/solar=sun:nu;måne/månar=moon:nu;himmel/himlar=s'+
        'ky:nu;huvud/huvuden=head:nn;hand/händer=hand:nu;fot/fötter=foot:nu;'+
        'öga/ögon=eye:nn;mun/munnar=mouth:nu;ansikte/ansikten=face:nn;',
    'hjärta/hjärtan=heart:nn;skjorta/skjortor=shirt:nu;klänning/klänningar'+
        '=dress:nu;sko/skor=shoe:nu;hatt/hattar=hat:nu;måndag=Monday:nu;tisda'+
        'g=Tuesday:nu;onsdag=Wednesday:nu;torsdag=Thursday:nu;fredag=Friday:nu'+
        ';',
    'lördag=Saturday:nu;söndag=Sunday:nu;januari=January:nu;februari=Februar'+
        'y:nu;mars=March:nu;april=April:nu;maj=May:nu;juni=June:nu;juli=July:n'+
        'u;augusti=August:nu;september=September:nu;oktober=October:nu;novembe'+
        'r=November:nu;',
    'december=December:nu;svenska=Swedish:nu;engelska=English:nu;franska=Frenc'+
        'h:nu;tyska=German:nu;spanska=Spanish:nu;italienska=Italian:nu;kinesis'+
        'ka=Chinese:nu;japanska=Japanese:nu;ryska=Russian:nu;arabiska=Arabic:n'+
        'u;',
    'stor/stort/stora=big:a;liten/litet/små=small:a;lång/långt/långa=long:'+
        'a;kort/kort/korta=short:a;ny/nytt/nya=new:a;gammal/gammalt/gamla=old:'+
        'a;ung/ungt/unga=young:a;vacker/vackert/vackra=beautiful:a;snygg/snygg'+
        't/snygga=pretty:a;',
    'glad/glatt/glada=happy:a;ledsen/ledset/ledsna=sad:a;trött/trött/trötta'+
        '=tired:a;sjuk/sjukt/sjuka=sick:a;varm/varmt/varma=hot:a;kall/kallt/ka'+
        'lla=cold:a;dyr/dyrt/dyra=expensive:a;billig/billigt/billiga=cheap:a;',
    'lätt/lätt/lätta=easy:a;svår/svårt/svåra=difficult:a;viktig/viktigt/'+
        'viktiga=important:a;intressant/intressant/intressanta=interesting:a;t'+
        'råkig/tråkigt/tråkiga=boring:a;rolig/roligt/roliga=funny:a;',
    'snabb/snabbt/snabba=fast:a;långsam/långsamt/långsamma=slow:a;stark/sta'+
        'rkt/starka=strong:a;svag/svagt/svaga=weak:a;rik/rikt/rika=rich:a;fatt'+
        'ig/fattigt/fattiga=poor:a;ren/rent/rena=clean:a;smutsig/smutsigt/smut'+
        'siga=dirty:a;',
    'full/fullt/fulla=full:a;tom/tomt/tomma=empty:a;öppen/öppet/öppna=open:'+
        'a;stängd/stängt/stängda=closed:a;färdig/färdigt/färdiga=ready:a'+
        ';upptagen/upptaget/upptagna=busy:a;fri/fritt/fria=free:a;röd/rött/r'+
        'öda=red:a;',
    'blå/blått/blå=blue:a;grön/grönt/gröna=green:a;gul/gult/gula=yellow:'+
        'a;svart/svart/svarta=black:a;vit/vitt/vita=white:a;brun/brunt/bruna=b'+
        'rown:a;grå/grått/grå=gray:a;rosa/rosa/rosa=pink:a;samma/samma/samm'+
        'a=same:a;',
    'annorlunda/annorlunda/annorlunda=different:a;annan/annat/andra=other:a;f'+
        'örsta/första/första=first:a;sista/sista/sista=last:a;nästa/nästa'+
        '/nästa=next:a;god/gott/goda=good:a;bra/bra/bra=good:a;dålig/dåligt'+
        '/dåliga=bad:a;',
    'hungrig/hungrigt/hungriga=hungry:a;törstig/törstigt/törstiga=thirsty:a'+
        ';rädd/rätt/rädda=afraid:a;nöjd/nöjt/nöjda=satisfied:a;snäll/sn'+
        'ällt/snälla=kind:a;trevlig/trevligt/trevliga=nice:a;säker/säkert/'+
        'säkra=sure:a;',
    'sann/sant/sanna=true:a;känd/känt/kända=famous:a;speciell/speciellt/spe'+
        'ciella=special:a;lugn/lugnt/lugna=quiet:a;vara/är/var/varit=be:v4:se'+
        'r;ha/har/hade/haft=have:v4:tener;gå/går/gick/gått=go:v4:ir;',
    'se/ser/såg/sett=see:v4:ver;ta/tar/tog/tagit=take:v4;ge/ger/gav/gett=give'+
        ':v4:dar;komma/kommer/kom/kommit=come:v4:venir;bli/blir/blev/blivit=be'+
        'come:v4;göra/gör/gjorde/gjort=do:v4:hacer;säga/säger/sa/sagt=say:'+
        'v4:decir;',
    'ligga/ligger/låg/legat=lie:v4;sitta/sitter/satt/suttit=sit:v4;stå/står'+
        '/stod/stått=stand:v4;äta/äter/åt/ätit=eat:v4;dricka/dricker/drac'+
        'k/druckit=drink:v4;sova/sover/sov/sovit=sleep:v4;vinna/vinner/vann/vu'+
        'nnit=win:v4;',
    'springa/springer/sprang/sprungit=run:v4;skriva/skriver/skrev/skrivit=writ'+
        'e:v4;få/får/fick/fått=get:v4;veta/vet/visste/vetat=know:v4:saber;k'+
        'unna/kan/kunde/kunnat=can:v4:poder;vilja/vill/ville/velat=want:v4:que'+
        'rer;',
    'måste/måste/måste/måst=must:v4:deber;skola/ska/skulle/skolat=shall:v4'+
        ':ir;finnas/finns/fanns/funnits=exist:v4:haber;heta/heter/hette/hetat='+
        'be_called:v4:llamar;sälja/säljer/sålde/sålt=sell:v4;',
    'sjunga/sjunger/sjöng/sjungit=sing:v4;välja/väljer/valde/valt=choose:v4'+
        ';bära/bär/bar/burit=carry:v4;gråta/gråter/grät/gråtit=cry:v4;d'+
        'ö/dör/dog/dött=die:v4;bryta/bryter/bröt/brutit=break:v4;',
    'bygga/bygger/byggde/byggt=build:v4;förstå/förstår/förstod/förstått'+
        '=understand:v4;stänga/stänger/stängde/stängt=close:v4;tala=speak:'+
        'v1;arbeta=work:v1;bo=live:v3;kosta=cost:v1;köpa=buy:v2;betala=pay:v1'+
        ';laga=cook:v1;',
    'studera=study:v1;lära=learn:v2;undervisa=teach:v1;spela=play:v1;dansa=da'+
        'nce:v1;promenera=walk:v1;köra=drive:v2;resa=travel:v2;lämna=leave:v'+
        '1;stanna=stay:v1;börja=begin:v1;sluta=finish:v1;öppna=open:v1;hjäl'+
        'pa=help:v2;',
    'vänta=wait:v1;ringa=call:v2;besöka=visit:v2;använda=use:v1;glömma=for'+
        'get:v2;bestämma=decide:v2;njuta=enjoy:v2;skicka=send:v1;hämta=bring'+
        ':v1;ändra=change:v1;visa=show:v1;simma=swim:v1;le=smile:v3;skratta=l'+
        'augh:v1;',
    'förlora=lose:v1;tvätta=wash:v1;städa=clean:v1;skära=cut:v2;föredra=p'+
        'refer:v1;beställa=order:v1;älska=love:v1;hata=hate:v1;behöva=need:'+
        'v1:necesitar;gilla=like:v1;regna=rain:v1:llover;tro=believe:v3;tänka'+
        '=think:v2;',
    'känna=feel:v2;hitta=find:v1;titta=look_at:v1;höra=hear:v2;lyssna=listen'+
        '_to:v1;fråga=ask:v1;svara=answer:v1;leva=live:v1;tycka=think:v2;myck'+
        'et=very:r;också=also:r;alltid=always:r;aldrig=never:r;ofta=often:r;i'+
        'bland=sometimes:r;',
    'nu=now:r;idag=today:r;imorgon=tomorrow:r;igår=yesterday:r;i_kväll=tonig'+
        'ht:r;här=here:r;där=there:r;bra=well:r;illa=badly:r;långsamt=slowl'+
        'y:r;igen=again:r;snart=soon:r;redan=already:r;fortfarande=still:r;bar'+
        'a=only:r;',
    'verkligen=really:r;tillsammans=together:r;nästan=almost:r;sedan=then:r;t'+
        'idigt=early:r;sent=late:r;långt=far:r;nära=near:r;ute=outside:r;inn'+
        'e=inside:r;kanske=maybe:r;förstås=of_course:r;lite=a_little:r;i=in:'+
        'p;på=on:p;',
    'till=to:p;från=from:p;med=with:p;av=of:p;för=for:p;om=about:p;vid=at:p;'+
        'under=under:p;över=over:p;efter=after:p;före=before:p;mellan=betwee'+
        'n:p;utan=without:p;hos=at:p;mot=toward:p;genom=through:p;bakom=behind'+
        ':p;',
    'framför=in_front_of:p;bredvid=next_to:p;och=and:c;men=but:c;eller=or:c;a'+
        'tt=that:c;eftersom=because:c;när=when:c;medan=while:c;fast=although:'+
        'c;hej=Hello:j;hej_då=Goodbye:j;tack=Thank_you:j;tack_så_mycket=Than'+
        'k_you_very_much:j;',
    'snälla=please:j;förlåt=Sorry:j;ursäkta=Excuse_me:j;god_morgon=Good_mo'+
        'rning:j;god_kväll=Good_evening:j;godnatt=Good_night:j;ja=Yes:j;nej=N'+
        'o:j;välkommen=Welcome:j;grattis=Congratulations:j;skål=Cheers:j;vad'+
        '=what:w;vem=who:w;',
    'var=where:w;när=when:w;varför=why:w;hur=how:w;vilken=which:w;vilket=whi'+
        'ch:w;vilka=which:w;hur_många=how_many:w;hur_mycket=how_much:w;pizza/'+
        'pizzor=pizza:nu;leka=play:v2;åka/åker/åkte/åkt=go:v4;må/mår/må'+
        'dde/mått=feel:v4;',
    'i_morgon=tomorrow:r;i_dag=today:r;i_går=yesterday:r;i_natt=tonight:r;i_m'+
        'orse=this_morning:r;nästa_vecka=next_week:r;förra_veckan=last_week:'+
        'r;läsa=read:v2;lägenhet/lägenheter=apartment:nu;tavla/tavlor=board'+
        ':nu;',
    'taxi/taxibilar=taxi:nu;frukt/frukter=fruit:nu;hemma=at_home:r;borta=away:'+
        'r;hit=here:r;dit=there:r;uppe=upstairs:r;nere=downstairs:r;träffa=me'+
        'et:v2;berätta=tell:v2;flyga/flyger/flög/flugit=fly:v4;cykla=cycle:v'+
        '1;',
    'minnas=remember:v2;hoppas=hope:v1;fungera=work:v1;hända=happen:v2;förs'+
        'öka=try:v2;handla=shop:v1;hålla/håller/höll/hållit=hold:v4;gift='+
        'married:a;'
  );

  { Danois : danois=anglais:categorie }
  DaData: Array[0..40] Of String = (
    'dreng/drenge=boy:nu;pige/piger=girl:nu;mand/mænd=man:nu;kvinde/kvinder=w'+
        'oman:nu;barn/børn=child:nn;ven/venner=friend:nu;veninde/veninder=fri'+
        'end:nu;familie/familier=family:nu;mor/mødre=mother:nu;far/fædre=fat'+
        'her:nu;',
    'bror/brødre=brother:nu;søster/søstre=sister:nu;søn/sønner=son:nu;dat'+
        'ter/døtre=daughter:nu;kone/koner=wife:nu;lærer/lærere=teacher:nu;s'+
        'tuderende/studerende=student:nu;elev/elever=student:nu;læge/læger=d'+
        'octor:nu;',
    'sygeplejerske/sygeplejersker=nurse:nu;person/personer=person:nu;menneske/'+
        'mennesker=person:nu;hus/huse=house:nn;værelse/værelser=room:nn;køk'+
        'ken/køkkener=kitchen:nn;badeværelse/badeværelser=bathroom:nn;',
    'soveværelse/soveværelser=bedroom:nn;dør/døre=door:nu;vindue/vinduer=w'+
        'indow:nn;bord/borde=table:nn;stol/stole=chair:nu;seng/senge=bed:nu;sk'+
        'ole/skoler=school:nu;universitet/universiteter=university:nn;klasse/k'+
        'lasser=class:nu;',
    'kontor/kontorer=office:nn;butik/butikker=shop:nu;marked/markeder=market:n'+
        'n;restaurant/restauranter=restaurant:nu;hotel/hoteller=hotel:nn;hospi'+
        'tal/hospitaler=hospital:nn;bibliotek/biblioteker=library:nn;bank/bank'+
        'er=bank:nu;',
    'lufthavn/lufthavne=airport:nu;station/stationer=station:nu;museum/museer='+
        'museum:nn;kirke/kirker=church:nu;biograf/biografer=cinema:nu;teater/t'+
        'eatre=theater:nn;strand/strande=beach:nu;hav/have=sea:nn;flod/floder='+
        'river:nu;',
    'bjerg/bjerge=mountain:nn;skov/skove=forest:nu;have/haver=garden:nu;park/p'+
        'arker=park:nu;træ/træer=tree:nn;blomst/blomster=flower:nu;by/byer=c'+
        'ity:nu;landsby/landsbyer=village:nu;land/lande=country:nn;gade/gader='+
        'street:nu;',
    'vej/veje=road:nu;verden/verdener=world:nu;lejlighed/lejligheder=apartment'+
        ':nu;tavle/tavler=board:nu;tog/tog=train:nn;bus/busser=bus:nu;bil/bile'+
        'r=car:nu;cykel/cykler=bicycle:nu;fly/fly=plane:nn;båd/både=boat:nu;',
    'taxa/taxaer=taxi:nu;rejse/rejser=trip:nu;mad/mad=food:nu;vand/vand=water:'+
        'nn;mælk/mælk=milk:nu;brød/brød=bread:nn;kød/kød=meat:nn;fisk/fi'+
        'sk=fish:nu;ost/oste=cheese:nu;æg/æg=egg:nn;frugt/frugter=fruit:nu;t'+
        'e/te=tea:nu;',
    'øl/øl=beer:nn;vin/vine=wine:nu;kaffe/kaffe=coffee:nu;æble/æbler=apple'+
        ':nn;pizza/pizzaer=pizza:nu;middag/middage=dinner:nu;morgenmad/morgenm'+
        'ad=breakfast:nu;frokost/frokoster=lunch:nu;penge/penge=money:nu;tid/t'+
        'ider=time:nu;',
    'dag/dage=day:nu;nat/nætter=night:nu;uge/uger=week:nu;måned/måneder=mon'+
        'th:nu;år/år=year:nn;time/timer=hour:nu;minut/minutter=minute:nn;mor'+
        'gen/morgener=morning:nu;aften/aftener=evening:nu;eftermiddag/eftermid'+
        'dage=afternoon:nu;',
    'mandag=Monday:nu;tirsdag=Tuesday:nu;onsdag=Wednesday:nu;torsdag=Thursday:'+
        'nu;fredag=Friday:nu;lørdag=Saturday:nu;søndag=Sunday:nu;bog/bøger='+
        'book:nu;avis/aviser=newspaper:nu;brev/breve=letter:nn;telefon/telefon'+
        'er=telephone:nu;',
    'computer/computere=computer:nu;film/film=movie:nu;musik/musik=music:nu;sp'+
        'il/spil=game:nn;arbejde/arbejder=work:nn;job/job=job:nn;gave/gaver=gi'+
        'ft:nu;hund/hunde=dog:nu;kat/katte=cat:nu;fugl/fugle=bird:nu;hest/hest'+
        'e=horse:nu;',
    'ko/køer=cow:nu;hånd/hænder=hand:nu;hoved/hoveder=head:nn;øje/øjne=ey'+
        'e:nn;fod/fødder=foot:nu;hjerte/hjerter=heart:nn;sprog/sprog=language'+
        ':nn;navn/navne=name:nn;problem/problemer=problem:nn;spørgsmål/spør'+
        'gsmål=question:nn;',
    'svar/svar=answer:nn;idé/idéer=idea:nu;ting/ting=thing:nu;sted/steder=pl'+
        'ace:nn;dansk=Danish:nu;svensk=Swedish:nu;engelsk=English:nu;tysk=Germ'+
        'an:nu;fransk=French:nu;spansk=Spanish:nu;italiensk=Italian:nu;',
    'være/er/var/været=be:v4:ser;have/har/havde/haft=have:v4:tener;gå/går/'+
        'gik/gået=go:v4:ir;se/ser/så/set=see:v4:ver;tage/tager/tog/taget=tak'+
        'e:v4;give/giver/gav/givet=give:v4:dar;komme/kommer/kom/kommet=come:v4'+
        ':venir;',
    'blive/bliver/blev/blevet=become:v4;gøre/gør/gjorde/gjort=do:v4:hacer;si'+
        'ge/siger/sagde/sagt=say:v4:decir;ligge/ligger/lå/ligget=lie:v4;sidde'+
        '/sidder/sad/siddet=sit:v4;stå/står/stod/stået=stand:v4;',
    'spise/spiser/spiste/spist=eat:v4;drikke/drikker/drak/drukket=drink:v4;sov'+
        'e/sover/sov/sovet=sleep:v4;vinde/vinder/vandt/vundet=win:v4;løbe/lø'+
        'ber/løb/løbet=run:v4;skrive/skriver/skrev/skrevet=write:v4;få/får'+
        '/fik/fået=get:v4;',
    'vide/ved/vidste/vidst=know:v4:saber;kunne/kan/kunne/kunnet=can:v4:poder;v'+
        'ille/vil/ville/villet=want:v4:querer;måtte/må/måtte/måttet=must:v'+
        '4:deber;skulle/skal/skulle/skullet=shall:v4:ir;',
    'findes/findes/fandtes/fundtes=exist:v4:haber;hedde/hedder/hed/heddet=be_c'+
        'alled:v4:llamar;sælge/sælger/solgte/solgt=sell:v4;synge/synger/sang'+
        '/sunget=sing:v4;vælge/vælger/valgte/valgt=choose:v4;bære/bærer/ba'+
        'r/båret=carry:v4;',
    'græde/græder/græd/grædt=cry:v4;dø/dør/døde/død=die:v4;bygge/bygge'+
        'r/byggede/bygget=build:v4;forstå/forstår/forstod/forstået=understa'+
        'nd:v4;finde/finder/fandt/fundet=find:v4;hjælpe/hjælper/hjalp/hjulpe'+
        't=help:v4;',
    'holde/holder/holdt/holdt=hold:v4;flyve/flyver/fløj/fløjet=fly:v4;tale=s'+
        'peak:v2;købe=buy:v2;læse=read:v2;høre=hear:v2;køre=drive:v2;rejse'+
        '=travel:v2;glemme=forget:v2;begynde=begin:v2;kende=know:v2;lære=lear'+
        'n:v2;mene=think:v2;',
    'tænke=think:v2;føle=feel:v2;arbejde=work:v1;elske=love:v1;lave=make:v1;'+
        'kalde=call:v1;svare=answer:v1;vente=wait:v1;betale=pay:v1;bruge=use:v'+
        '1;regne=rain:v1:llover;besøge=visit:v1;huske=remember:v1;åbne=open:'+
        'v1;lukke=close:v1;',
    'leve=live:v1;danse=dance:v1;lytte=listen:v1;kigge=look:v1;spille=play:v1;'+
        'studere=study:v1;vaske=wash:v1;lege=play:v1;cykle=cycle:v1;svømme=sw'+
        'im:v1;ønske=wish:v1;håbe=hope:v1;stoppe=stop:v1;prøve=try:v1;',
    'behøve=need:v1:necesitar;ringe=call:v1;spørge=ask:v1;vise=show:v1;rydde'+
        '=clean:v1;handle=shop:v1;synes/synes/syntes/syntes=think:v4;bo=live:v'+
        '3;tro=believe:v3;stor/stort/store=big:a;lille/lille/små=small:a;',
    'lang/langt/lange=long:a;kort/kort/korte=short:a;ny/nyt/nye=new:a;gammel/g'+
        'ammelt/gamle=old:a;ung/ungt/unge=young:a;smuk/smukt/smukke=beautiful:'+
        'a;pæn/pænt/pæne=pretty:a;glad/glad/glade=happy:a;ked/ked/kede=sad:'+
        'a;',
    'træt/træt/trætte=tired:a;syg/sygt/syge=sick:a;varm/varmt/varme=hot:a;k'+
        'old/koldt/kolde=cold:a;dyr/dyrt/dyre=expensive:a;billig/billigt/billi'+
        'ge=cheap:a;let/let/lette=easy:a;svær/svært/svære=difficult:a;',
    'vigtig/vigtigt/vigtige=important:a;interessant/interessant/interessante=i'+
        'nteresting:a;kedelig/kedeligt/kedelige=boring:a;sjov/sjovt/sjove=funn'+
        'y:a;hurtig/hurtigt/hurtige=fast:a;langsom/langsomt/langsomme=slow:a;',
    'stærk/stærkt/stærke=strong:a;svag/svagt/svage=weak:a;rig/rigt/rige=ric'+
        'h:a;fattig/fattigt/fattige=poor:a;ren/rent/rene=clean:a;beskidt/beski'+
        'dt/beskidte=dirty:a;fuld/fuldt/fulde=full:a;tom/tomt/tomme=empty:a;',
    'åben/åbent/åbne=open:a;lukket/lukket/lukkede=closed:a;færdig/færdigt'+
        '/færdige=ready:a;optaget/optaget/optagede=busy:a;fri/frit/frie=free:'+
        'a;rød/rødt/røde=red:a;blå/blåt/blå=blue:a;grøn/grønt/grønne='+
        'green:a;',
    'gul/gult/gule=yellow:a;sort/sort/sorte=black:a;hvid/hvidt/hvide=white:a;b'+
        'run/brunt/brune=brown:a;grå/gråt/grå=gray:a;lyserød/lyserødt/lys'+
        'erøde=pink:a;samme/samme/samme=same:a;anderledes/anderledes/anderled'+
        'es=different:a;',
    'anden/andet/andre=other:a;første/første/første=first:a;sidste/sidste/s'+
        'idste=last:a;næste/næste/næste=next:a;god/godt/gode=good:a;dårlig'+
        '/dårligt/dårlige=bad:a;sulten/sultent/sultne=hungry:a;',
    'tørstig/tørstigt/tørstige=thirsty:a;tilfreds/tilfreds/tilfredse=satisf'+
        'ied:a;flink/flinkt/flinke=kind:a;rar/rart/rare=nice:a;sikker/sikkert/'+
        'sikre=sure:a;sand/sandt/sande=true:a;kendt/kendt/kendte=famous:a;',
    'særlig/særligt/særlige=special:a;rolig/roligt/rolige=quiet:a;gift/gift'+
        '/gifte=married:a;meget=very:r;også=also:r;altid=always:r;ofte=often:'+
        'r;nu=now:r;her=here:r;der=there:r;dårligt=badly:r;langsomt=slowly:r;'+
        'hurtigt=quickly:r;',
    'igen=again:r;snart=soon:r;allerede=already:r;stadig=still:r;kun=only:r;vi'+
        'rkelig=really:r;sammen=together:r;næsten=almost:r;derefter=then:r;ti'+
        'dligt=early:r;sent=late:r;langt=far:r;tæt=near:r;ude=outside:r;inde='+
        'inside:r;',
    'måske=maybe:r;selvfølgelig=of_course:r;lidt=a_little:r;hjemme=at_home:r'+
        ';væk=away:r;oppe=upstairs:r;nede=downstairs:r;i_dag=today:r;i_morgen'+
        '=tomorrow:r;i_går=yesterday:r;i_aften=tonight:r;i_nat=tonight:r;',
    'i_morges=this_morning:r;næste_uge=next_week:r;sidste_uge=last_week:r;nog'+
        'le_gange=sometimes:r;i=in:p;på=on:p;til=to:p;fra=from:p;med=with:p;a'+
        'f=of:p;for=for:p;om=about:p;ved=at:p;under=under:p;over=over:p;efter='+
        'after:p;',
    'før=before:p;mellem=between:p;uden=without:p;hos=at:p;mod=toward:p;genne'+
        'm=through:p;bag=behind:p;foran=in_front_of:p;ved_siden_af=next_to:p;o'+
        'g=and:c;men=but:c;eller=or:c;at=that:c;fordi=because:c;når=when:c;me'+
        'ns=while:c;',
    'selvom=although:c;hvis=if:c;hej=Hello:j;farvel=Goodbye:j;tak=Thank_you:j;'+
        'mange_tak=Thank_you_very_much:j;venligst=please:j;undskyld=Sorry:j;go'+
        'd_morgen=Good_morning:j;god_aften=Good_evening:j;godnat=Good_night:j;'+
        'ja=Yes:j;nej=No:j;',
    'velkommen=Welcome:j;tillykke=Congratulations:j;skål=Cheers:j;goddag=Good'+
        '_day:j;hvad=what:w;hvem=who:w;hvor=where:w;hvornår=when:w;hvorfor=wh'+
        'y:w;hvordan=how:w;hvilken=which:w;hvilket=which:w;hvilke=which:w;hvor'+
        '_mange=how_many:w;',
    'hvor_meget=how_much:w;høj/højt/høje=tall:a;'
  );

  { Italien : italien=anglais:categorie }
  ItData: Array[0..34] Of String = (
    'ragazzo=boy:n;ragazza=girl:n;bambino=child:n;bambina=child:n;uomo/uomini='+
        'man:n;donna=woman:n;amico/amici=friend:n;amica=friend:n;famiglia=fami'+
        'ly:n;madre=mother:n;mamma=mother:n;padre=father:n;papà/papà=father:'+
        'n;',
    'fratello=brother:n;sorella=sister:n;figlio=son:n;figlia=daughter:n;marito'+
        '=husband:n;moglie/mogli=wife:n;insegnante=teacher:n;professore=teache'+
        'r:n;professoressa=teacher:n;studente=student:n;studentessa=student:n;',
    'medico/medici=doctor:n;dottore=doctor:n;infermiera=nurse:n;persona=person'+
        ':n;gente/gente=people:n;casa=house:n;camera=room:n;stanza=room:n;cuci'+
        'na=kitchen:n;bagno=bathroom:n;porta=door:n;finestra=window:n;tavolo=t'+
        'able:n;',
    'sedia=chair:n;letto=bed:n;scuola=school:n;università/università=univers'+
        'ity:n;classe=class:n;ufficio=office:n;negozio/negozi=shop:n;mercato=m'+
        'arket:n;ristorante=restaurant:n;albergo/alberghi=hotel:n;hotel/hotel='+
        'hotel:n;',
    'ospedale=hospital:n;biblioteca=library:n;banca/banche=bank:n;aeroporto=ai'+
        'rport:n;stazione=station:n;museo=museum:n;chiesa=church:n;cinema/cine'+
        'ma=cinema:n;teatro=theater:n;spiaggia/spiagge=beach:n;mare=sea:n;fium'+
        'e=river:n;',
    'montagna=mountain:n;bosco/boschi=forest:n;giardino=garden:n;parco/parchi='+
        'park:n;albero=tree:n;fiore=flower:n;città/città=city:n;paese=countr'+
        'y:n;strada=street:n;via/vie=street:n;mondo=world:n;appartamento=apart'+
        'ment:n;',
    'lavagna=board:n;treno=train:n;autobus/autobus=bus:n;macchina=car:n;automo'+
        'bile=car:n;bicicletta=bicycle:n;aereo=plane:n;barca/barche=boat:n;tax'+
        'i/taxi=taxi:n;viaggio/viaggi=trip:n;cibo=food:n;acqua/acque=water:n;l'+
        'atte/latte=milk:n;',
    'pane=bread:n;carne=meat:n;pesce=fish:n;formaggio=cheese:n;uovo/uova=egg:n'+
        ';frutta=fruit:n;tè/tè=tea:n;birra=beer:n;vino=wine:n;caffè/caffè='+
        'coffee:n;mela=apple:n;pizza=pizza:n;cena=dinner:n;colazione=breakfast'+
        ':n;pranzo=lunch:n;',
    'soldi/soldi=money:n;denaro=money:n;tempo=time:n;giorno=day:n;notte=night:'+
        'n;settimana=week:n;mese=month:n;anno=year:n;ora=hour:n;minuto=minute:'+
        'n;mattina=morning:n;sera=evening:n;pomeriggio=afternoon:n;lunedì/lun'+
        'edì=Monday:n;',
    'martedì/martedì=Tuesday:n;mercoledì/mercoledì=Wednesday:n;giovedì/gi'+
        'ovedì=Thursday:n;venerdì/venerdì=Friday:n;sabato=Saturday:n;domeni'+
        'ca=Sunday:n;libro=book:n;giornale=newspaper:n;lettera=letter:n;telefo'+
        'no=telephone:n;',
    'computer/computer=computer:n;film/film=movie:n;musica=music:n;gioco/gioch'+
        'i=game:n;lavoro=work:n;regalo=gift:n;cane=dog:n;gatto=cat:n;uccello=b'+
        'ird:n;cavallo=horse:n;mucca/mucche=cow:n;mano/mani=hand:n;testa=head:'+
        'n;',
    'occhio/occhi=eye:n;piede=foot:n;cuore=heart:n;lingua/lingue=language:n;no'+
        'me=name:n;problema/problemi=problem:n;domanda=question:n;risposta=ans'+
        'wer:n;idea=idea:n;cosa=thing:n;posto=place:n;fame/fame=hungry:n;sete/'+
        'sete=thirsty:n;',
    'freddo=cold:n;caldo=hot:n;sonno=sleepy:n;paura=afraid:n;ragione=right:n;f'+
        'retta=in_a_hurry:n;vergogna=ashamed:n;italiano=Italian:n;inglese=Engl'+
        'ish:n;francese=French:n;tedesco=German:n;spagnolo=Spanish:n;portoghes'+
        'e=Portuguese:n;',
    'svedese=Swedish:n;danese=Danish:n;russo=Russian:n;cinese=Chinese:n;giappo'+
        'nese=Japanese:n;essere=be:vx:ser;avere=have:vx:tener;andare=go:vx:ir;'+
        'fare=do:vx:hacer;dire=say:vx:decir;venire=come:vx:venir;potere=can:vx'+
        ':poder;',
    'volere=want:vx:querer;dovere=must:vx:deber;sapere=know:vx:saber;dare=give'+
        ':vx:dar;stare=be:vx:estar;uscire=go_out:vx;bere=drink:vx;rimanere=sta'+
        'y:vx;tenere=keep:vx;scegliere=choose:vx;tradurre=translate:vx;morire='+
        'die:vx;',
    'piacere=like:vx:gustar;vedere/visto/vedr=see:v:ver;leggere/letto=read:v;s'+
        'crivere/scritto=write:v;prendere/preso=take:v;mettere/messo=put:v;apr'+
        'ire/aperto=open:v;chiudere/chiuso=close:v;vivere/vissuto/vivr=live:v;',
    'vincere/vinto=win:v;perdere/perso=lose:v;correre/corso/corr=run:v;decider'+
        'e/deciso=decide:v;chiedere/chiesto=ask:v;rispondere/risposto=answer:v'+
        ';offrire/offerto=offer:v;conoscere/conosciuto=know:v;nascere/nato=be_'+
        'born:v;',
    'cadere/caduto/cadr=fall:v;spendere/speso=spend:v;piovere/piovuto=rain:v:l'+
        'lover;parlare=speak:v;mangiare=eat:v;lavorare=work:v;abitare=live:v;s'+
        'tudiare=study:v;comprare=buy:v;amare=love:v;cantare=sing:v;ballare=da'+
        'nce:v;',
    'cucinare=cook:v;aspettare=wait:v;chiamare=call:v:llamar;arrivare=arrive:v'+
        ';tornare=return:v;entrare=enter:v;trovare=find:v;usare=use:v;pagare=p'+
        'ay:v;costare=cost:v;cominciare=begin:v;aiutare=help:v;ascoltare=liste'+
        'n_to:v;',
    'guardare=look:v;giocare=play:v;pensare=think:v;cercare=look_for:v;cammina'+
        're=walk:v;viaggiare=travel:v;nuotare=swim:v;ricordare=remember:v;dime'+
        'nticare=forget:v;telefonare=call:v;visitare=visit:v;invitare=invite:v'+
        ';lavare=wash:v;',
    'sperare=hope:v;desiderare=wish:v;preparare=prepare:v;spiegare=explain:v;d'+
        'omandare=ask:v;ricevere=receive:v;credere=believe:v;vendere=sell:v;ri'+
        'petere=repeat:v;insegnare=teach:v;imparare=learn:v;lasciare=leave:v;p'+
        'ortare=bring:v;',
    'mandare=send:v;ringraziare=thank:v;dormire=sleep:v;partire=leave:v;sentir'+
        'e=hear:v;seguire=follow:v;finire=finish:vi;preferire=prefer:vi;capire'+
        '=understand:vi;pulire=clean:vi;costruire=build:vi;grande=big:a;piccol'+
        'o=small:a;',
    'lungo/lunga/lunghi/lunghe=long:a;corto=short:a;nuovo=new:a;vecchio/vecchi'+
        'a/vecchi/vecchie=old:a;giovane=young:a;bello=beautiful:a;bravo=good:a'+
        ';felice=happy:a;triste=sad:a;stanco/stanca/stanchi/stanche=tired:a;ma'+
        'lato=sick:a;',
    'caldo=hot:a;freddo=cold:a;caro=expensive:a;economico/economica/economici/'+
        'economiche=cheap:a;facile=easy:a;difficile=difficult:a;importante=imp'+
        'ortant:a;interessante=interesting:a;noioso=boring:a;divertente=funny:'+
        'a;veloce=fast:a;',
    'lento=slow:a;forte=strong:a;debole=weak:a;ricco/ricca/ricchi/ricche=rich:'+
        'a;povero=poor:a;pulito=clean:a;sporco/sporca/sporchi/sporche=dirty:a;'+
        'pieno=full:a;vuoto=empty:a;aperto=open:a;chiuso=closed:a;pronto=ready'+
        ':a;occupato=busy:a;',
    'libero=free:a;rosso=red:a;blu=blue:a;verde=green:a;giallo=yellow:a;nero=b'+
        'lack:a;bianco/bianca/bianchi/bianche=white:a;marrone=brown:a;grigio/g'+
        'rigia/grigi/grigie=gray:a;rosa=pink:a;stesso=same:a;diverso=different'+
        ':a;altro=other:a;',
    'primo=first:a;ultimo=last:a;prossimo=next:a;buono=good:a;cattivo=bad:a;al'+
        'to=tall:a;basso=short:a;gentile=kind:a;simpatico/simpatica/simpatici/'+
        'simpatiche=nice:a;sicuro=sure:a;vero=true:a;famoso=famous:a;speciale='+
        'special:a;',
    'tranquillo=quiet:a;sposato=married:a;contento=glad:a;bene=well:r;male=bad'+
        'ly:r;molto=very:r;troppo=too_much:r;poco=little:r;sempre=always:r;spe'+
        'sso=often:r;qualche_volta=sometimes:r;ora=now:r;adesso=now:r;oggi=tod'+
        'ay:r;',
    'domani=tomorrow:r;ieri=yesterday:r;stasera=tonight:r;stanotte=tonight:r;q'+
        'ui=here:r;qua=here:r;là=there:r;lì=there:r;ancora=again:r;presto=so'+
        'on:r;già=already:r;solo=only:r;soltanto=only:r;davvero=really:r;insi'+
        'eme=together:r;',
    'quasi=almost:r;poi=then:r;dopo=afterwards:r;prima=before:r;tardi=late:r;l'+
        'ontano=far:r;vicino=near:r;fuori=outside:r;dentro=inside:r;forse=mayb'+
        'e:r;naturalmente=of_course:r;certo=of_course:r;anche=also:r;di=of:p;a'+
        '=to:p;da=from:p;',
    'in=in:p;con=with:p;su=on:p;per=for:p;tra=between:p;fra=between:p;senza=wi'+
        'thout:p;sopra=above:p;sotto=under:p;dopo=after:p;prima_di=before:p;ve'+
        'rso=toward:p;contro=against:p;presso=at:p;accanto_a=next_to:p;davanti'+
        '_a=in_front_of:p;',
    'dietro=behind:p;vicino_a=near:p;fino_a=until:p;durante=during:p;e=and:c;m'+
        'a=but:c;o=or:c;oppure=or:c;che=that:c;perché=because:c;quando=when:c'+
        ';mentre=while:c;se=if:c;però=but:c;anche_se=although:c;ciao=Hello:j;',
    'arrivederci=Goodbye:j;grazie=Thank_you:j;grazie_mille=Thank_you_very_much'+
        ':j;prego=You''re_welcome:j;per_favore=please:j;scusa=Sorry:j;scusi=Ex'+
        'cuse_me:j;buongiorno=Good_morning:j;buonasera=Good_evening:j;buonanot'+
        'te=Good_night:j;',
    'sì=Yes:j;benvenuto=Welcome:j;auguri=Congratulations:j;salute=Cheers:j;ch'+
        'e=what:w;cosa=what:w;che_cosa=what:w;chi=who:w;dove=where:w;quando=wh'+
        'en:w;perché=why:w;come=how:w;quale=which:w;quali=which:w;quanto=how_'+
        'much:w;',
    'quanti=how_many:w;quante=how_many:w;quanta=how_much:w;no=No:j;come=like:p'+
        ';mi_dispiace=I''m_sorry:j;affamato=hungry:a;assetato=thirsty:a;piange'+
        're/pianto=cry:v;'
  );

  { Latin : latin=anglais:categorie }
  LaData: Array[0..37] Of String = (
    'puella=girl:n1;femina=woman:n1;mensa=table:n1;aqua=water:n1;terra=land:n1'+
        ';patria=homeland:n1;villa=house:n1;porta=gate:n1;via=road:n1;silva=fo'+
        'rest:n1;rosa=rose:n1;luna=moon:n1;vita=life:n1;insula=island:n1;ancil'+
        'la=maid:n1;',
    'regina=queen:n1;filia=daughter:n1;dea=goddess:n1;domina=lady:n1;schola=sc'+
        'hool:n1;epistula=letter:n1;pecunia=money:n1;causa=reason:n1;hora=hour'+
        ':n1;fabula=story:n1;ira=anger:n1;gloria=glory:n1;victoria=victory:n1;'+
        'copia=supply:n1;',
    'agricola=farmer:n1:m;poeta=poet:n1:m;nauta=sailor:n1:m;incola=inhabitant:'+
        'n1:m;dominus=master:n2m;servus=slave:n2m;amicus=friend:n2m;filius=son'+
        ':n2m;equus=horse:n2m;lupus=wolf:n2m;annus=year:n2m;deus=god:n2m;popul'+
        'us=nation:n2m;',
    'ager/agri=field:n2m;puer/pueri=boy:n2m;liber/libri=book:n2m;magister/magi'+
        'stri=teacher:n2m;vir/viri=man:n2m;discipulus=pupil:n2m;hortus=garden:'+
        'n2m;gladius=sword:n2m;nuntius=messenger:n2m;murus=wall:n2m;campus=pla'+
        'in:n2m;',
    'locus=place:n2m;animus=mind:n2m;ventus=wind:n2m;fluvius=river:n2m;mundus='+
        'world:n2m;cibus=food:n2m;somnus=sleep:n2m;bellum=war:n2n;donum=gift:n'+
        '2n;verbum=word:n2n;templum=temple:n2n;oppidum=town:n2n;forum=forum:n2'+
        'n;caelum=sky:n2n;',
    'saxum=stone:n2n;auxilium=help:n2n;periculum=danger:n2n;consilium=plan:n2n'+
        ';regnum=kingdom:n2n;vinum=wine:n2n;aurum=gold:n2n;signum=sign:n2n;pra'+
        'emium=reward:n2n;cubiculum=bedroom:n2n;pomum=apple:n2n;folium=leaf:n2'+
        'n;',
    'imperium=empire:n2n;officium=duty:n2n;exemplum=example:n2n;rex/regis=king'+
        ':n3m;miles/militis=soldier:n3m;dux/ducis=leader:n3m;pater/patris=fath'+
        'er:n3m;mater/matris=mother:n3f;frater/fratris=brother:n3m;soror/soror'+
        'is=sister:n3f;',
    'homo/hominis=man:n3m;mulier/mulieris=woman:n3f;civis/civis=citizen:n3m;ur'+
        'bs/urbis=city:n3f;pax/pacis=peace:n3f;lux/lucis=light:n3f;nox/noctis='+
        'night:n3f;lex/legis=law:n3f;canis/canis=dog:n3m;leo/leonis=lion:n3m;p'+
        'es/pedis=foot:n3m;',
    'mons/montis=mountain:n3m;pons/pontis=bridge:n3m;fons/fontis=spring:n3m;ig'+
        'nis/ignis=fire:n3m;navis/navis=ship:n3f;mare/maris=sea:n3n;nomen/nomi'+
        'nis=name:n3n;corpus/corporis=body:n3n;tempus/temporis=time:n3n;',
    'flumen/fluminis=river:n3n;caput/capitis=head:n3n;opus/operis=work:n3n;ite'+
        'r/itineris=journey:n3n;carmen/carminis=song:n3n;genus/generis=kind:n3'+
        'n;mors/mortis=death:n3f;amor/amoris=love:n3m;labor/laboris=labor:n3m;',
    'timor/timoris=fear:n3m;dolor/doloris=pain:n3m;honor/honoris=honor:n3m;pas'+
        'tor/pastoris=shepherd:n3m;imperator/imperatoris=emperor:n3m;senator/s'+
        'enatoris=senator:n3m;uxor/uxoris=wife:n3f;virtus/virtutis=virtue:n3f;',
    'civitas/civitatis=state:n3f;libertas/libertatis=freedom:n3f;veritas/verit'+
        'atis=truth:n3f;ars/artis=art:n3f;pars/partis=part:n3f;gens/gentis=nat'+
        'ion:n3f;mens/mentis=mind:n3f;sol/solis=sun:n3m;vox/vocis=voice:n3f;',
    'lapis/lapidis=stone:n3m;avis/avis=bird:n3f;hostis/hostis=enemy:n3m;exerci'+
        'tus=army:n4m;senatus=senate:n4m;manus=hand:n4m:f;fructus=fruit:n4m;cu'+
        'rrus=chariot:n4m;portus=port:n4m;lacus=lake:n4m;metus=fear:n4m;cornu='+
        'horn:n4n;',
    'res=thing:n5;dies=day:n5:m;spes=hope:n5;fides=faith:n5;facies=face:n5;Mar'+
        'cus/Marci=Marcus:n2m:P;Iulius/Iulii=Julius:n2m:P;Brutus/Bruti=Brutus:'+
        'n2m:P;Cicero/Ciceronis=Cicero:n3m:P;Caesar/Caesaris=Caesar:n3m:P;Roma'+
        '=Rome:n1:P;',
    'Italia=Italy:n1:P;Gallia=Gaul:n1:P;Graecia=Greece:n1:P;Iulia=Julia:n1:P;C'+
        'ornelia=Cornelia:n1:P;Livia=Livia:n1:P;Hispania=Spain:n1:P;Britannia='+
        'Britain:n1:P;bonus=good:a12;malus=bad:a12;magnus=big:a12;parvus=small'+
        ':a12;',
    'longus=long:a12;altus=tall:a12;novus=new:a12;antiquus=old:a12;pulcher/pul'+
        'chra=beautiful:a12;niger/nigra=black:a12;ruber/rubra=red:a12;albus=wh'+
        'ite:a12;laetus=happy:a12;miser/misera=wretched:a12;liber/libera=free:'+
        'a12;',
    'primus=first:a12;secundus=second:a12;verus=true:a12;clarus=famous:a12;car'+
        'us=dear:a12;validus=strong:a12;fessus=tired:a12;aeger/aegra=sick:a12;'+
        'beatus=blessed:a12;doctus=learned:a12;multus=many:a12;meus=my:a12;tuu'+
        's=your:a12;',
    'noster/nostra=our:a12;vester/vestra=your:a12;suus=his:a12;fortis=brave:a3'+
        ';gravis=serious:a3;brevis=short:a3;levis=light:a3;facilis=easy:a3;dif'+
        'ficilis=difficult:a3;utilis=useful:a3;dulcis=sweet:a3;crudelis=cruel:'+
        'a3;',
    'nobilis=noble:a3;tristis=sad:a3;felix/felicis=lucky:a3x;ingens/ingentis=h'+
        'uge:a3x;audax/audacis=bold:a3x;potens/potentis=powerful:a3x;sapiens/s'+
        'apientis=wise:a3x;amare/amav=love:v1;laudare/laudav=praise:v1;vocare/'+
        'vocav=call:v1;',
    'portare/portav=carry:v1;narrare/narrav=tell:v1;necare/necav=kill:v1;pugna'+
        're/pugnav=fight:v1;ambulare/ambulav=walk:v1;habitare/habitav=live:v1;'+
        'laborare/laborav=work:v1;clamare/clamav=shout:v1;spectare/spectav=wat'+
        'ch:v1;',
    'parare/parav=prepare:v1;servare/servav=save:v1;superare/superav=conquer:v'+
        '1;vitare/vitav=avoid:v1;rogare/rogav=ask:v1;dare/ded=give:v1:dar;star'+
        'e/stet=stand:v1;iuvare/iuv=help:v1;errare/errav=wander:v1;cantare/can'+
        'tav=sing:v1;',
    'saltare/saltav=dance:v1;oppugnare/oppugnav=attack:v1;curare/curav=tend:v1'+
        ';donare/donav=give:v1;nuntiare/nuntiav=announce:v1;orare/orav=pray:v1'+
        ';plorare/plorav=weep:v1;salutare/salutav=greet:v1;appellare/appellav='+
        'call:v1;',
    'aedificare/aedificav=build:v1;cogitare/cogitav=think:v1;desiderare/deside'+
        'rav=desire:v1;liberare/liberav=free:v1;monstrare/monstrav=show:v1;occ'+
        'upare/occupav=seize:v1;probare/probav=approve:v1;monere/monu=advise:v'+
        '2;',
    'habere/habu=have:v2:tener;videre/vid=see:v2:ver;tenere/tenu=hold:v2;timer'+
        'e/timu=fear:v2;docere/docu=teach:v2;debere/debu=must:v2:deber;movere/'+
        'mov=move:v2;manere/mans=remain:v2;sedere/sed=sit:v2;respondere/respon'+
        'd=answer:v2;',
    'ridere/ris=laugh:v2;tacere/tacu=be_silent:v2;valere/valu=be_well:v2;iuber'+
        'e/iuss=order:v2;terrere/terru=frighten:v2;delere/delev=destroy:v2;reg'+
        'ere/rex=rule:v3;scribere/scrips=write:v3;legere/leg=read:v3;ducere/du'+
        'x=lead:v3;',
    'mittere/mis=send:v3;dicere/dix=say:v3:decir;vincere/vic=conquer:v3;vivere'+
        '/vix=live:v3;ponere/posu=put:v3;petere/petiv=seek:v3;quaerere/quaesiv'+
        '=seek:v3;currere/cucurr=run:v3;discere/didic=learn:v3;bibere/bib=drin'+
        'k:v3;',
    'claudere/claus=close:v3;cedere/cess=yield:v3;credere/credid=believe:v3;de'+
        'fendere/defend=defend:v3;dividere/divis=divide:v3;gerere/gess=wage:v3'+
        ';metuere/metu=fear:v3;pellere/pepul=drive:v3;relinquere/reliqu=leave:'+
        'v3;',
    'surgere/surrex=rise:v3;trahere/trax=drag:v3;vertere/vert=turn:v3;vendere/'+
        'vendid=sell:v3;agere/eg=do:v3;cognoscere/cognov=learn:v3;constituere/'+
        'constitu=decide:v3;dimittere/dimis=dismiss:v3;intellegere/intellex=un'+
        'derstand:v3;',
    'ostendere/ostend=show:v3;perdere/perdid=lose:v3;capere/cep=take:v3i;facer'+
        'e/fec=do:v3i:hacer;fugere/fug=flee:v3i;iacere/iec=throw:v3i;rapere/ra'+
        'pu=seize:v3i;accipere/accep=receive:v3i;cupere/cupiv=desire:v3i;',
    'incipere/incep=begin:v3i;interficere/interfec=kill:v3i;aspicere/aspex=loo'+
        'k:v3i;audire/audiv=hear:v4;venire/ven=come:v4:venir;scire/sciv=know:v'+
        '4:saber;dormire/dormiv=sleep:v4;munire/muniv=fortify:v4;sentire/sens='+
        'feel:v4;',
    'servire/serviv=serve:v4;punire/puniv=punish:v4;aperire/aperu=open:v4;inve'+
        'nire/inven=find:v4;custodire/custodiv=guard:v4;esse=be:vx:ser;posse=c'+
        'an:vx:poder;ire=go:vx:ir;ferre=carry:vx;velle=want:vx:querer;nolle=re'+
        'fuse:vx;',
    'bene=well:r;male=badly:r;semper=always:r;saepe=often:r;nunc=now:r;tum=the'+
        'n:r;tunc=then:r;iam=already:r;hodie=today:r;cras=tomorrow:r;heri=yest'+
        'erday:r;ibi=there:r;illic=there:r;huc=here:r;ubique=everywhere:r;etia'+
        'm=also:r;',
    'quoque=also:r;tamen=however:r;ita=so:r;sic=thus:r;valde=very:r;tam=so:r;n'+
        'imis=too:r;mox=soon:r;diu=long:r;olim=once:r;iterum=again:r;tandem=fi'+
        'nally:r;statim=immediately:r;satis=enough:r;vix=hardly:r;procul=far:r'+
        ';ad=to:p;in=in:p;',
    'ex=from:p;e=from:p;de=about:p;ab=from:p;a=from:p;cum=with:p;per=through:p'+
        ';sine=without:p;pro=for:p;ante=before:p;post=after:p;inter=between:p;'+
        'apud=at:p;sub=under:p;super=over:p;contra=against:p;propter=because_o'+
        'f:p;',
    'circum=around:p;trans=across:p;prope=near:p;ob=because_of:p;supra=above:p'+
        ';infra=below:p;iuxta=next_to:p;extra=outside:p;et=and:c;sed=but:c;aut'+
        '=or:c;vel=or:c;nam=for:c;enim=for:c;quia=because:c;quod=because:c;si='+
        'if:c;ut=that:c;',
    'ne=lest:c;dum=while:c;postquam=after:c;antequam=before:c;quamquam=althoug'+
        'h:c;nisi=unless:c;quoniam=since:c;atque=and:c;ac=and:c;nec=nor:c;nequ'+
        'e=nor:c;qui=who:c;quae=who:c;ave=Hello:j;salve=Hello:j;salvete=Hello:'+
        'j;vale=Goodbye:j;',
    'valete=Goodbye:j;quaeso=please:j;quis=who:w;quid=what:w;ubi=where:w;cur=w'+
        'hy:w;quando=when:w;quomodo=how:w;quot=how_many:w;unde=from_where:w;qu'+
        'o=where:w;ludere/lus=play:v3;pluma=pen:n1;mensa=table:n1;navis/navis='+
        'ship:n3f;',
    'tabula=tablet:n1;discedere/discess=leave:v3;Romanus=Roman:n2m;vadere/vas='+
        'go:v3:ir;agere/eg=do:v3;'
  );

  { Innu-aimun : mots et phrases (voir logic5zg) }
  MoeData: Array[0..13] Of String = (
    'mecatina=grande montagne (Mécatina);assi=terre, sol;maikan=loup;anapi=fi'+
        'let de pêche;ishkueu=femme;mashku=ours (ours noir);mashk=ours (ours '+
        'noir);atiku=caribou;atik=caribou;amishku=castor;amishk=castor;kaku=po'+
        'rc-épic;',
    'kak=porc-épic;matsheshu=renard;muaku=plongeon huard;muak=plongeon huard;'+
        'atush=pointe de flèche;atshakash=vison;atimu=chien;atim=chien;kakats'+
        'hiship=cormoran à aigrettes;minishtiku=île;minishtik=île;atshiku=p'+
        'hoque;',
    'atshik=phoque;assiku=marmite, seau;assik=marmite, seau;kakatshimin=baie d'+
        'e genévrier;assimin=camarine noire;kukamess=touladi;matameku=omble d'+
        'e fontaine;matamek=omble de fontaine;ishkuteu=feu;emikuan=cuillère;i'+
        'shkuatem=porte;',
    'kakumin=cassis;akakuai=sangsue;eniku=fourmi, araignée;enik=fourmi, araig'+
        'née;mishui=durbec des sapins;masseku=marais, tourbière;massek=marai'+
        's, tourbière;anushkan=framboise;innikuai=bouleau à papier;akushames'+
        'heu=balbuzard;',
    'akameku=écaille de poisson;akamek=écaille de poisson;akupitamaun=emplâ'+
        'tre, cataplasme;anikutshash=écureuil;inniminan=bleuet;atumin=amélan'+
        'che (fruit);atikuminan=busserole;neu=quatre;unan=ouananiche;shikuteu='+
        'chicoutai;',
    'uapush=lièvre (lapin);nishuasht=sept;pipun=hiver;nishk=outarde (bernache'+
        ' du Canada);pishku=engoulevent d''Amérique;pishk=engoulevent d''Amé'+
        'rique;utsheu=mouche;shiship=canard;utshek=pékan;uapishtan=martre d'''+
        'Amérique;',
    'mukuman=couteau;pineminanish=chiogène;nitshiku=loutre;nitshik=loutre;shi'+
        'pu=rivière;nameu=esturgeon;utshashumeku=saumon atlantique;utshashume'+
        'k=saumon atlantique;shakaikan=lac;uapatsheshu=renard arctique;mushumi'+
        'n=pimbina;',
    'tshinash=sterne;munaikutan=macareux moine;tshetsheku=busard Saint-Martin;'+
        'tshetshek=busard Saint-Martin;upueiashku=érable à sucre;upueiashk='+
        'érable à sucre;shashatshu=grand héron;pipitshish=faucon émerillon'+
        ';teueikan=tambour;',
    'teueshekaikan=cloche;tatuauatshipitshikan=charrue;napeiatshiku=phoque mâ'+
        'le;napeiatshik=phoque mâle;pipunatshiku=phoque du Groenland;pipunats'+
        'hik=phoque du Groenland;uteiminan=fraise;nissiminan=airelle des marai'+
        's;',
    'tshitshinakuss=fruit de la chiogène;shapumin=groseille à maquereau;uapu'+
        'shimin=baie;shashakuminan=quatre-temps (fruit);auass=enfant;auassat=e'+
        'nfants;nitauassim=mon enfant;nitauassimat=mes enfants;',
    'nitauassimissat=mes jeunes enfants;kuei=bonjour;eshe=oui;mauat=non;tshina'+
        'shkumitin=merci (je te remercie);iame=au revoir;niaut=au revoir;nimin'+
        'upanin=je vais bien;nitassi=mon pays;nuapamau=je le vois (animé);',
    'nuapaten=je le vois (inanimé);tshuapamau=tu le vois (animé);tshuapaten='+
        'tu le vois (inanimé);uapameu=il le voit (animé);uapatamu=il le voit'+
        ' (inanimé);uapatam=il le voit (inanimé);nuapamanan=nous le voyons ('+
        'exclusif, animé);',
    'nuapatenan=nous le voyons (exclusif, inanimé);tshuapamanan=nous le voyon'+
        's (inclusif, animé);tshuapatenan=nous le voyons (inclusif, inanimé)'+
        ';tshuapamanau=vous le voyez (animé);tshuapetenau=vous le voyez (inan'+
        'imé);',
    'uapameuat=ils le voient (animé);uapatamuat=ils le voient (inanimé);'
  );

  MoePhrData: Array[0..28] Of String = (
    'mashten-atushkan nene katshi unian ekue mitshishuian|Dimanche dernier, ap'+
        'rès m''être levé, j''ai mangé.',
    'atauitshuapit ekue ituteian|Puis, je suis allé au dépanneur.',
    'mina nitaiati tshetshi minaputsheian|J''ai acheté des baies pour faire d'+
        'e la confiture.',
    'nimishta-aiati|J''en ai acheté beaucoup.',
    'katshi takushinian nitshinat ekue minaputsheian|Rendu chez moi, j''ai fai'+
        't la confiture.',
    'nimishta-minaputsheti|J''en ai fait beaucoup.',
    'katshi tshishi-minaputsheian ekue tshishtapunitishuian|Après en avoir fa'+
        'it, je me suis lavé.',
    'nuitsheuakan peiku nitaimikuti tshetshi natshi-kutueiat|Une de mes amies '+
        'm''a appelé pour aller pique-niquer.',
    'innu-aimi ma|Parle donc innu-aimun !',
    'kuei|Bonjour !',
    'tan eshpanin|Comment ça va ?',
    'niminupanin|Je vais bien',
    'miam a|Ça va bien',
    'tan eshinikashuin|Comment t''appelles-tu ?',
    'tan eshinikatikauin|Comment t''appelles-tu ?',
    'auen tshin|Qui es-tu ?',
    'tanite uetshipanin|D''où viens-tu ?',
    'tan etatupipuneshin|Quel âge as-tu ?',
    'tshekuan etutamin|Que fais-tu ?',
    'apu tshekuan tutaman|Je ne fais rien',
    'eshe|Oui',
    'mauat|Non',
    'tshinashkumitin|Merci (je te remercie)',
    'iame|Au revoir',
    'niaut|Au revoir',
    'tanite etat|Où est-il ? (objet animé)',
    'tanite tekuak|Où est-il ? (objet inanimé)',
    'tshekuan ma|Pourquoi ?',
    'tanite nana etutamin nitassi|Qu''as-tu fait de mon pays ?'
  );

Var
  Lex: Array[1..MaxLex] Of TLex;
  LexCount: Integer;
  Irregular: Array[1..MaxIrregular] Of TIrregular;
  IrregularCount: Integer;
  English: Array[1..MaxEnglishIrregular] Of TEnglishForm;
  EnglishCount: Integer;


{ ------------------------------------------------------------------ }
{ Chargement des tables                                               }
{ ------------------------------------------------------------------ }

Function CopyUntil(Var S: String; Separator: Char): String;
Var
  P: Integer;
Begin
  P := Pos(Separator, S);
  If P = 0 Then
  Begin
    CopyUntil := S;
    S := '';
  End
  Else
  Begin
    CopyUntil := Copy(S, 1, P - 1);
    Delete(S, 1, P);
  End;
End;

Procedure ParseGerman(L: Integer; Const Part: String);
Var
  G, Spec, Stem: String;
  E: Integer;
Begin
  G := Part;
  Lex[L].De := CopyUntil(G, '|');
  Spec := G;
  Case Lex[L].Pos Of
    'n':
    Begin
      If Spec <> '' Then
      Begin
        Lex[L].DeG := Spec[1];
        If Lex[L].DeG = 'P' Then Lex[L].DeFlags := 'P';
        If (Length(Spec) > 1) And (Spec[2] = 'w') Then Lex[L].DeFlags :=
            Lex[L].DeFlags + 'w';
        E := Pos('/', Spec);
        If E > 0 Then Lex[L].DePl := Copy(Spec, E + 1, 20);
      End;
    End;
    'a':
    Begin
      E := Pos('/', Lex[L].De);
      If E > 0 Then
      Begin
        Stem := Copy(Lex[L].De, E + 1, 20);
        Lex[L].De := Copy(Lex[L].De, 1, E - 1);
        If Stem = '*' Then Lex[L].DeFlags := 'i' Else Lex[L].DePl := Stem;
      End;
    End;
    'v':
    Begin
      E := Pos('+', Spec);
      If E > 0 Then
      Begin
        Lex[L].DeSep := Copy(Spec, E + 1, 8);
        Spec := Copy(Spec, 1, E - 1);
      End;
      Lex[L].DeFlags := Spec;
    End;
    'p': If Spec <> '' Then Lex[L].DeCase := Spec[1];
    'c': If Spec = 'S' Then Lex[L].DeFlags := 'S';
  End;
End;

Procedure AddLexEntry(Const Item: String);
Var
  Rest, Spec, Part, German: String;
  E: Integer;
Begin
  If (Item = '') Or (LexCount >= MaxLex) Then Exit;
  Rest := Item;
  German := '';
  E := Pos('~', Rest);
  If E > 0 Then
  Begin
    German := Copy(Rest, E + 1, 80);
    Rest := Copy(Rest, 1, E - 1);
  End;
  Inc(LexCount);
  FillChar(Lex[LexCount], SizeOf(TLex), 0);
  Lex[LexCount].En := CopyUntil(Rest, '=');
  Lex[LexCount].Fr := CopyUntil(Rest, ':');
  Spec := Rest;
  If Spec = '' Then
  Begin
    Dec(LexCount);
    Exit;
  End;
  Lex[LexCount].Pos := Spec[1];
  Delete(Spec, 1, 1);
  Lex[LexCount].Aux := 'a';
  Lex[LexCount].Sub := ' ';
  E := Pos('!', Spec);
  If E > 0 Then
  Begin
    Lex[LexCount].Idiom := Copy(Spec, E + 1, 10);
    Spec := Copy(Spec, 1, E - 1);
  End;
  E := Pos('@', Spec);
  If E > 0 Then
  Begin
    Lex[LexCount].Link := Copy(Spec, E + 1, 3);
    Spec := Copy(Spec, 1, E - 1);
  End;
  Part := CopyUntil(Spec, '/');
  If Part <> '' Then
  Begin
    Lex[LexCount].Sub := Part[1];
    If (Lex[LexCount].Pos = 'v') And (Length(Part) > 1) Then Lex[LexCount].Aux
        := Part[2];
  End;
  If Spec <> '' Then
  Begin
    If Lex[LexCount].Pos = 'a' Then
    Begin
      Lex[LexCount].Fem := CopyUntil(Spec, '/');
      Lex[LexCount].Plu := Spec;
    End
    Else
      Lex[LexCount].Plu := Spec;
  End;
  If German <> '' Then ParseGerman(LexCount, German);
End;

Procedure LoadLexicon;
Var
  I: Integer;
  Line, Item: String;
Begin
  LexCount := 0;
  For I := 0 To High(LexData) Do
  Begin
    Line := LexData[I];
    While Line <> '' Do
    Begin
      Item := CopyUntil(Line, ';');
      AddLexEntry(Item);
    End;
  End;
End;

Procedure LoadIrregular;
Var
  I, K: Integer;
  Line: String;
Begin
  IrregularCount := 0;
  For I := 0 To High(IrregularData) Do
    If IrregularCount < MaxIrregular Then
    Begin
      Line := IrregularData[I];
      Inc(IrregularCount);
      Irregular[IrregularCount].Inf := CopyUntil(Line, '|');
      For K := 1 To 6 Do
        Irregular[IrregularCount].Pres[K] := CopyUntil(Line, '|');
      Irregular[IrregularCount].Part := CopyUntil(Line, '|');
      Irregular[IrregularCount].Fut := CopyUntil(Line, '|');
      Irregular[IrregularCount].Imp := Line;
    End;
End;

Procedure LoadEnglish;
Var
  I: Integer;
  Line, Item: String;
Begin
  EnglishCount := 0;
  For I := 0 To High(EnglishData) Do
  Begin
    Line := EnglishData[I];
    While Line <> '' Do
    Begin
      Item := CopyUntil(Line, ';');
      If (Item <> '') And (EnglishCount < MaxEnglishIrregular) Then
      Begin
        Inc(EnglishCount);
        English[EnglishCount].Form := CopyUntil(Item, '=');
        English[EnglishCount].Base := CopyUntil(Item, ':');
        English[EnglishCount].Kind := Item[1];
      End;
    End;
  End;
End;

{ ------------------------------------------------------------------ }
{ Utilitaires de chaines                                              }
{ ------------------------------------------------------------------ }

Function LowerAscii(Const S: String): String;
Var
  I: Integer;
  R: String;
Begin
  R := S;
  For I := 1 To Length(R) Do
    If (R[I] >= 'A') And (R[I] <= 'Z') Then R[I] := Chr(Ord(R[I]) + 32);
  LowerAscii := R;
End;

Function EndsWith(Const S, Suffix: String): Boolean;
Begin
  EndsWith := (Length(S) >= Length(Suffix)) And
              (Copy(S, Length(S) - Length(Suffix) + 1, Length(Suffix)) =
                  Suffix);
End;

Function StartsWith(Const S, Prefix: String): Boolean;
Begin
  StartsWith := Copy(S, 1, Length(Prefix)) = Prefix;
End;

Function IsAspirated(Const W: String): Boolean;
Begin
  IsAspirated := (W = 'haricot') Or (W = 'hasard') Or (W = 'héros') Or (W =
      'hibou') Or
                 (W = 'honte') Or (W = 'hôtel hôtel') Or (W = 'hockey') Or (W
                     = 'huit') Or
                 (W = 'hurler') Or (W = 'haut') Or (W = 'haute') Or (W =
                     'hauteur') Or
                 (W = 'hamburger') Or (W = 'handicap') Or (W = 'hanche') Or (W =
                     'hanter') Or
                 (W = 'hache') Or (W = 'hamster') Or (W = 'héron') Or (W =
                     'hiérarchie');
End;

{ Voyelle ou h muet au debut du mot (UTF-8) ? }
Function StartsVowel(Const S: String): Boolean;
Var
  C, D: Byte;
  Word1: String;
  P: Integer;
Begin
  StartsVowel := False;
  If S = '' Then Exit;
  P := Pos(' ', S);
  If P > 0 Then Word1 := Copy(S, 1, P - 1) Else Word1 := S;
  C := Ord(Word1[1]);
  If C < 128 Then
  Begin
    C := Ord(LowerAscii(Chr(C))[1]);
    If Chr(C) In ['a', 'e', 'i', 'o', 'u'] Then
      StartsVowel := True
    Else If Chr(C) = 'h' Then
      StartsVowel := Not IsAspirated(LowerAscii(Word1));
    Exit;
  End;
  If (C = $C3) And (Length(Word1) >= 2) Then
  Begin
    D := Ord(Word1[2]);
    { a grave/accent, e, i, o, u accentues, minuscules ou majuscules }
    If ((D >= $A0) And (D <= $A5)) Or ((D >= $A8) And (D <= $AF)) Or
       ((D >= $B2) And (D <= $B6)) Or ((D >= $B9) And (D <= $BC)) Or
       ((D >= $80) And (D <= $85)) Or ((D >= $88) And (D <= $8F)) Or
       ((D >= $92) And (D <= $96)) Or ((D >= $99) And (D <= $9C)) Then
      StartsVowel := True;
    Exit;
  End;
  If (C = $C5) And (Length(Word1) >= 2) And ((Ord(Word1[2]) = $93) Or
      (Ord(Word1[2]) = $92)) Then
    StartsVowel := True;
End;

Function UpperFirst(Const S: String): String;
Var
  R: String;
Begin
  R := S;
  If R = '' Then
  Begin
    UpperFirst := R;
    Exit;
  End;
  If (R[1] >= 'a') And (R[1] <= 'z') Then
    R[1] := Chr(Ord(R[1]) - 32)
  Else If (Ord(R[1]) = $C3) And (Length(R) >= 2) And (Ord(R[2]) >= $A0) And
          (Ord(R[2]) <= $BE) And (Ord(R[2]) <> $B7) Then
    R[2] := Chr(Ord(R[2]) - 32);
  UpperFirst := R;
End;

Function Join(Const A, B: String): String;
Begin
  If A = '' Then
    Join := B
  Else If B = '' Then
    Join := A
  Else
    Join := A + ' ' + B;
End;

{ ------------------------------------------------------------------ }
{ Lexique                                                             }
{ ------------------------------------------------------------------ }

Function FindLex(Const En: String; Const Kinds: String): Integer;
Var
  I: Integer;
Begin
  For I := 1 To LexCount Do
    If (Lex[I].En = En) And (Pos(Lex[I].Pos, Kinds) > 0) Then
    Begin
      FindLex := I;
      Exit;
    End;
  FindLex := 0;
End;

Function FindIrregular(Const Inf: String): Integer;
Var
  I: Integer;
Begin
  For I := 1 To IrregularCount Do
    If Irregular[I].Inf = Inf Then
    Begin
      FindIrregular := I;
      Exit;
    End;
  FindIrregular := 0;
End;

{ ------------------------------------------------------------------ }
{ Conjugaison francaise                                               }
{ ------------------------------------------------------------------ }

{ Radical du verbe (infinitif sans terminaison) et groupe. }
Function VerbGroup(Const Inf: String): Char;
Begin
  If EndsWith(Inf, 'er') Then
    VerbGroup := 'e'
  Else If EndsWith(Inf, 'ir') Then
    VerbGroup := 'i'
  Else
    VerbGroup := 'r';
End;

Function FirstWord(Const S: String): String;
Var
  P: Integer;
Begin
  P := Pos(' ', S);
  If P = 0 Then FirstWord := S Else FirstWord := Copy(S, 1, P - 1);
End;

Function RestWords(Const S: String): String;
Var
  P: Integer;
Begin
  P := Pos(' ', S);
  If P = 0 Then RestWords := '' Else RestWords := Copy(S, P, 255);
End;

{ Present de l'indicatif : slot 1..6 = je, tu, il, nous, vous, ils }
Function ConjugatePresent(Const Inf: String; Slot: Integer): String;
Var
  Index: Integer;
  Stem, Word1: String;
Begin
  Index := FindIrregular(Inf);
  If Index > 0 Then
  Begin
    ConjugatePresent := Irregular[Index].Pres[Slot];
    Exit;
  End;
  Word1 := Inf;
  Case VerbGroup(Word1) Of
    'e':
    Begin
      Stem := Copy(Word1, 1, Length(Word1) - 2);
      Case Slot Of
        1, 3: ConjugatePresent := Stem + 'e';
        2: ConjugatePresent := Stem + 'es';
        4:
          If EndsWith(Stem, 'g') Then
            ConjugatePresent := Stem + 'eons'
          Else If EndsWith(Stem, 'c') Then
            ConjugatePresent := Copy(Stem, 1, Length(Stem) - 1) + 'çons'
          Else
            ConjugatePresent := Stem + 'ons';
        5: ConjugatePresent := Stem + 'ez';
      Else
        ConjugatePresent := Stem + 'ent';
      End;
    End;
    'i':
    Begin
      Stem := Copy(Word1, 1, Length(Word1) - 2);
      Case Slot Of
        1, 2: ConjugatePresent := Stem + 'is';
        3: ConjugatePresent := Stem + 'it';
        4: ConjugatePresent := Stem + 'issons';
        5: ConjugatePresent := Stem + 'issez';
      Else
        ConjugatePresent := Stem + 'issent';
      End;
    End;
  Else
    Begin
      Stem := Copy(Word1, 1, Length(Word1) - 2);
      Case Slot Of
        1, 2: ConjugatePresent := Stem + 's';
        3: ConjugatePresent := Stem;
        4: ConjugatePresent := Stem + 'ons';
        5: ConjugatePresent := Stem + 'ez';
      Else
        ConjugatePresent := Stem + 'ent';
      End;
    End;
  End;
End;

Function PastParticiple(Const Inf: String): String;
Var
  Index: Integer;
  Word1: String;
Begin
  Index := FindIrregular(Inf);
  If Index > 0 Then
  Begin
    PastParticiple := Irregular[Index].Part;
    Exit;
  End;
  Word1 := Inf;
  Case VerbGroup(Word1) Of
    'e': PastParticiple := Copy(Word1, 1, Length(Word1) - 2) + 'é';
    'i': PastParticiple := Copy(Word1, 1, Length(Word1) - 2) + 'i';
  Else
    PastParticiple := Copy(Word1, 1, Length(Word1) - 2) + 'u';
  End;
End;

Function FutureStem(Const Inf: String): String;
Var
  Index: Integer;
Begin
  Index := FindIrregular(Inf);
  If Index > 0 Then
    FutureStem := Irregular[Index].Fut
  Else If EndsWith(Inf, 're') Then
    FutureStem := Copy(Inf, 1, Length(Inf) - 1)
  Else
    FutureStem := Inf;
End;

Function ConjugateFuture(Const Inf: String; Slot: Integer): String;
Const
  Endings: Array[1..6] Of String[4] = ('ai', 'as', 'a', 'ons', 'ez', 'ont');
Begin
  ConjugateFuture := FutureStem(Inf) + Endings[Slot];
End;

Function ConjugateConditional(Const Inf: String; Slot: Integer): String;
Const
  Endings: Array[1..6] Of String[6] = ('ais', 'ais', 'ait', 'ions', 'iez',
      'aient');
Begin
  ConjugateConditional := FutureStem(Inf) + Endings[Slot];
End;

Function ConjugateImperfect(Const Inf: String; Slot: Integer): String;
Const
  Endings: Array[1..6] Of String[6] = ('ais', 'ais', 'ait', 'ions', 'iez',
      'aient');
Var
  Index: Integer;
  Stem, Nous: String;
Begin
  Index := FindIrregular(Inf);
  If Index > 0 Then
  Begin
    If Inf = 'avoir besoin de' Then
      Stem := 'av'
    Else
      Stem := Irregular[Index].Imp;
    ConjugateImperfect := Stem + Endings[Slot];
    Exit;
  End;
  Nous := ConjugatePresent(Inf, 4);
  Stem := Copy(Nous, 1, Length(Nous) - 3);
  If (Slot = 4) Or (Slot = 5) Then
    If EndsWith(Stem, 'ge') Then Stem := Copy(Stem, 1, Length(Stem) - 1);
  ConjugateImperfect := Stem + Endings[Slot];
End;

{ Conjugue le premier mot d'un infinitif compose (avoir besoin de). }
Function Conjugate(Const Inf: String; Slot: Integer; Tense: Char): String;
Var
  Head, Tail: String;
Begin
  Head := Inf;
  Tail := '';
  If (Pos(' ', Inf) > 0) And ((Tense <> 'p') Or (FindIrregular(Inf) = 0)) And
     (Copy(Inf, 1, 3) <> 'se ') Then
  Begin
    Head := FirstWord(Inf);
    Tail := RestWords(Inf);
  End;
  Case Tense Of
    'f': Conjugate := ConjugateFuture(Head, Slot) + Tail;
    'c': Conjugate := ConjugateConditional(Head, Slot) + Tail;
    'i': Conjugate := ConjugateImperfect(Head, Slot) + Tail;
  Else
    Conjugate := ConjugatePresent(Head, Slot) + Tail;
  End;
End;

{ ------------------------------------------------------------------ }
{ Accord des noms et des adjectifs                                    }
{ ------------------------------------------------------------------ }

Function PluralOf(Const S: String): String;
Begin
  If (S = '') Or EndsWith(S, 's') Or EndsWith(S, 'x') Or EndsWith(S, 'z') Then
    PluralOf := S
  Else If EndsWith(S, 'eau') Or EndsWith(S, 'au') Then
    PluralOf := S + 'x'
  Else If EndsWith(S, 'al') Then
    PluralOf := Copy(S, 1, Length(S) - 2) + 'aux'
  Else
    PluralOf := S + 's';
End;

Function FeminineOf(Const S: String): String;
Begin
  If EndsWith(S, 'e') Then
    FeminineOf := S
  Else If EndsWith(S, 'eux') Then
    FeminineOf := Copy(S, 1, Length(S) - 3) + 'euse'
  Else If EndsWith(S, 'if') Then
    FeminineOf := Copy(S, 1, Length(S) - 2) + 'ive'
  Else If EndsWith(S, 'er') Then
    FeminineOf := Copy(S, 1, Length(S) - 2) + 'ère'
  Else If EndsWith(S, 'on') Or EndsWith(S, 'en') Or EndsWith(S, 'el') Or
      EndsWith(S, 'et') Then
    FeminineOf := S + Copy(S, Length(S), 1) + 'e'
  Else
    FeminineOf := S + 'e';
End;

Function NounForm(L: Integer; Plural: Boolean): String;
Begin
  If Not Plural Then
    NounForm := Lex[L].Fr
  Else If Lex[L].Plu <> '' Then
    NounForm := Lex[L].Plu
  Else
    NounForm := PluralOf(Lex[L].Fr);
End;

{ Adjectif accorde ; BeforeVowel : pour beau/nouveau/vieux masculin devant
  voyelle. }
Function AdjForm(L: Integer; Gen: Char; Plural: Boolean; BeforeVowel: Boolean):
    String;
Var
  Base, Fem, Res: String;
  Invariable: Boolean;
Begin
  Base := Lex[L].Fr;
  Invariable := (Lex[L].Plu <> '') And (Lex[L].Plu = Base) And (Lex[L].Fem =
      Base);
  If Invariable Then
  Begin
    AdjForm := Base;
    Exit;
  End;
  If Gen = 'f' Then
  Begin
    If Lex[L].Fem <> '' Then Fem := Lex[L].Fem Else Fem := FeminineOf(Base);
    Res := Fem;
    If Plural Then Res := PluralOf(Res);
  End
  Else
  Begin
    Res := Base;
    If Plural Then
    Begin
      If Lex[L].Plu <> '' Then Res := Lex[L].Plu Else Res := PluralOf(Base);
    End
    Else If BeforeVowel Then
    Begin
      If Base = 'beau' Then Res := 'bel';
      If Base = 'nouveau' Then Res := 'nouvel';
      If Base = 'vieux' Then Res := 'vieil';
    End;
  End;
  AdjForm := Res;
End;

{ ------------------------------------------------------------------ }
{ Analyse morphologique de l'anglais                                  }
{ ------------------------------------------------------------------ }

Type
  TCand = Record
    N, V, A, R, P, C, J: Integer;
    NPlural: Boolean;
    VForm: Char; { b base, s 3e personne, g -ing, p passe, q participe, y passe
        ou participe }
    AdvFr: String[30];
    AdvA: Integer;
  End;

Function IsConsonant(C: Char): Boolean;
Begin
  IsConsonant := (C >= 'a') And (C <= 'z') And Not (C In ['a', 'e', 'i', 'o',
      'u']);
End;

Procedure Analyze(Const W: String; Var C: TCand);
Var
  I, L: Integer;
  B: String;

  Procedure TryVerb(Const Base: String; Form: Char);
  Var
    Idx: Integer;
  Begin
    If C.V = 0 Then
    Begin
      Idx := FindLex(Base, 'v');
      If Idx > 0 Then
      Begin
        C.V := Idx;
        C.VForm := Form;
      End;
    End;
  End;

  Procedure TryNoun(Const Base: String);
  Var
    Idx: Integer;
  Begin
    If C.N = 0 Then
    Begin
      Idx := FindLex(Base, 'n');
      If Idx > 0 Then
      Begin
        C.N := Idx;
        C.NPlural := True;
      End;
    End;
  End;

Begin
  FillChar(C, SizeOf(C), 0);
  C.VForm := 'b';
  C.N := FindLex(W, 'n');
  C.A := FindLex(W, 'a');
  C.V := FindLex(W, 'v');
  C.R := FindLex(W, 'r');
  C.P := FindLex(W, 'p');
  C.C := FindLex(W, 'c');
  C.J := FindLex(W, 'j');
  L := Length(W);

  For I := 1 To EnglishCount Do
    If English[I].Form = W Then
    Begin
      If English[I].Kind = 'n' Then
        TryNoun(English[I].Base)
      Else
      Begin
        C.V := 0;
        If English[I].Kind = 'p' Then
          TryVerb(English[I].Base, 'p')
        Else If English[I].Kind = 'q' Then
          TryVerb(English[I].Base, 'q')
        Else
          TryVerb(English[I].Base, 'y');
      End;
    End;

  If C.V = 0 Then
  Begin
    If EndsWith(W, 'ing') And (L > 4) Then
    Begin
      B := Copy(W, 1, L - 3);
      TryVerb(B, 'g');
      TryVerb(B + 'e', 'g');
      If (Length(B) > 2) And (B[Length(B)] = B[Length(B) - 1]) Then
        TryVerb(Copy(B, 1, Length(B) - 1), 'g');
    End;
    If EndsWith(W, 'ied') And (L > 4) Then
      TryVerb(Copy(W, 1, L - 3) + 'y', 'y');
    If EndsWith(W, 'ed') And (L > 3) Then
    Begin
      B := Copy(W, 1, L - 2);
      TryVerb(B, 'y');
      TryVerb(B + 'e', 'y');
      If (Length(B) > 2) And (B[Length(B)] = B[Length(B) - 1]) Then
        TryVerb(Copy(B, 1, Length(B) - 1), 'y');
    End;
    If EndsWith(W, 'ies') And (L > 4) Then
      TryVerb(Copy(W, 1, L - 3) + 'y', 's')
    Else If EndsWith(W, 'es') And (L > 3) Then
    Begin
      TryVerb(Copy(W, 1, L - 2), 's');
      TryVerb(Copy(W, 1, L - 1), 's');
    End
    Else If EndsWith(W, 's') And Not EndsWith(W, 'ss') And (L > 2) Then
      TryVerb(Copy(W, 1, L - 1), 's');
  End;

  If C.N = 0 Then
  Begin
    If EndsWith(W, 'ies') And (L > 4) Then
      TryNoun(Copy(W, 1, L - 3) + 'y')
    Else If EndsWith(W, 'es') And (L > 3) Then
    Begin
      TryNoun(Copy(W, 1, L - 2));
      TryNoun(Copy(W, 1, L - 1));
    End
    Else If EndsWith(W, 's') And Not EndsWith(W, 'ss') And (L > 2) Then
      TryNoun(Copy(W, 1, L - 1));
  End;

  { Adverbe forme sur un adjectif : quickly -> rapidement }
  If (C.R = 0) And EndsWith(W, 'ly') And (L > 4) Then
  Begin
    I := FindLex(Copy(W, 1, L - 2), 'a');
    If I = 0 Then I := FindLex(Copy(W, 1, L - 3) + 'e', 'a');
    If I = 0 Then I := FindLex(Copy(W, 1, L - 3) + 'y', 'a');
    If I > 0 Then
    Begin
      C.AdvA := I;
      If Lex[I].Fem <> '' Then B := Lex[I].Fem Else B := FeminineOf(Lex[I].Fr);
      If EndsWith(Lex[I].Fr, 'ent') Then
        C.AdvFr := Copy(Lex[I].Fr, 1, Length(Lex[I].Fr) - 3) + 'emment'
      Else
        C.AdvFr := B + 'ment';
    End;
  End;
End;

{ ------------------------------------------------------------------ }
{ Mots-outils                                                         }
{ ------------------------------------------------------------------ }

Function NumberWord(Const W: String): String;
Begin
  NumberWord := '';
  If W = 'two' Then NumberWord := 'deux'
  Else If W = 'three' Then NumberWord := 'trois'
  Else If W = 'four' Then NumberWord := 'quatre'
  Else If W = 'five' Then NumberWord := 'cinq'
  Else If W = 'six' Then NumberWord := 'six'
  Else If W = 'seven' Then NumberWord := 'sept'
  Else If W = 'eight' Then NumberWord := 'huit'
  Else If W = 'nine' Then NumberWord := 'neuf'
  Else If W = 'ten' Then NumberWord := 'dix'
  Else If W = 'eleven' Then NumberWord := 'onze'
  Else If W = 'twelve' Then NumberWord := 'douze'
  Else If W = 'thirteen' Then NumberWord := 'treize'
  Else If W = 'fourteen' Then NumberWord := 'quatorze'
  Else If W = 'fifteen' Then NumberWord := 'quinze'
  Else If W = 'sixteen' Then NumberWord := 'seize'
  Else If W = 'seventeen' Then NumberWord := 'dix-sept'
  Else If W = 'eighteen' Then NumberWord := 'dix-huit'
  Else If W = 'nineteen' Then NumberWord := 'dix-neuf'
  Else If W = 'twenty' Then NumberWord := 'vingt'
  Else If W = 'thirty' Then NumberWord := 'trente'
  Else If W = 'forty' Then NumberWord := 'quarante'
  Else If W = 'fifty' Then NumberWord := 'cinquante'
  Else If W = 'sixty' Then NumberWord := 'soixante'
  Else If W = 'seventy' Then NumberWord := 'soixante-dix'
  Else If W = 'eighty' Then NumberWord := 'quatre-vingts'
  Else If W = 'ninety' Then NumberWord := 'quatre-vingt-dix'
  Else If W = 'hundred' Then NumberWord := 'cent'
  Else If W = 'thousand' Then NumberWord := 'mille'
  Else If W = 'million' Then NumberWord := 'million';
End;

Function ProperFr(Const W: String): String;
Begin
  ProperFr := '';
  If W = 'london' Then ProperFr := 'Londres'
  Else If W = 'brussels' Then ProperFr := 'Bruxelles'
  Else If W = 'geneva' Then ProperFr := 'Genève'
  Else If W = 'moscow' Then ProperFr := 'Moscou'
  Else If W = 'vienna' Then ProperFr := 'Vienne'
  Else If W = 'lisbon' Then ProperFr := 'Lisbonne'
  Else If W = 'athens' Then ProperFr := 'Athènes'
  Else If W = 'cairo' Then ProperFr := 'Le Caire'
  Else If W = 'rome' Then ProperFr := 'Rome'
  Else If W = 'gaul' Then ProperFr := 'Gaule'
  Else If W = 'britain' Then ProperFr := 'Bretagne'
  Else If W = 'caesar' Then ProperFr := 'César'
  Else If W = 'cicero' Then ProperFr := 'Cicéron'
  Else If W = 'julius' Then ProperFr := 'Jules'
  Else If W = 'julia' Then ProperFr := 'Julie'
  Else If W = 'livia' Then ProperFr := 'Livie'
  Else If W = 'spain' Then ProperFr := 'Espagne'
  Else If W = 'germany' Then ProperFr := 'Allemagne'
  Else If W = 'italy' Then ProperFr := 'Italie'
  Else If W = 'england' Then ProperFr := 'Angleterre'
  Else If W = 'switzerland' Then ProperFr := 'Suisse'
  Else If W = 'belgium' Then ProperFr := 'Belgique'
  Else If W = 'portugal' Then ProperFr := 'Portugal'
  Else If W = 'china' Then ProperFr := 'Chine'
  Else If W = 'japan' Then ProperFr := 'Japon'
  Else If W = 'russia' Then ProperFr := 'Russie'
  Else If W = 'brazil' Then ProperFr := 'Brésil'
  Else If W = 'mexico' Then ProperFr := 'Mexique'
  Else If W = 'india' Then ProperFr := 'Inde'
  Else If W = 'france' Then ProperFr := 'France'
  Else If W = 'canada' Then ProperFr := 'Canada'
  Else If W = 'america' Then ProperFr := 'Amérique'
  Else If W = 'africa' Then ProperFr := 'Afrique'
  Else If W = 'europe' Then ProperFr := 'Europe'
  Else If W = 'asia' Then ProperFr := 'Asie'
  Else If W = 'australia' Then ProperFr := 'Australie'
  Else If W = 'ireland' Then ProperFr := 'Irlande'
  Else If W = 'scotland' Then ProperFr := 'Écosse'
  Else If W = 'sweden' Then ProperFr := 'Suède'
  Else If W = 'norway' Then ProperFr := 'Norvège'
  Else If W = 'denmark' Then ProperFr := 'Danemark'
  Else If W = 'poland' Then ProperFr := 'Pologne'
  Else If W = 'greece' Then ProperFr := 'Grèce'
  Else If W = 'turkey' Then ProperFr := 'Turquie'
  Else If W = 'egypt' Then ProperFr := 'Égypte'
  Else If W = 'morocco' Then ProperFr := 'Maroc'
  Else If W = 'algeria' Then ProperFr := 'Algérie'
  Else If W = 'tunisia' Then ProperFr := 'Tunisie'
  Else If W = 'quebec' Then ProperFr := 'Québec'
  Else If W = 'montreal' Then ProperFr := 'Montréal';
End;

{ Genre et preposition du lieu : 'm' pour au/du, 'f' pour en/de, 'p' pour
  aux/des ; ' ' si ce n'est pas un pays. }
Function CountryKind(Const W: String): Char;
Begin
  CountryKind := ' ';
  If (W = 'france') Or (W = 'spain') Or (W = 'germany') Or (W = 'italy') Or (W =
      'england') Or
     (W = 'switzerland') Or (W = 'belgium') Or (W = 'china') Or (W = 'russia')
         Or (W = 'india') Or
     (W = 'america') Or (W = 'africa') Or (W = 'europe') Or (W = 'asia') Or (W =
         'australia') Or
     (W = 'ireland') Or (W = 'scotland') Or (W = 'sweden') Or (W = 'norway') Or
         (W = 'poland') Or
     (W = 'greece') Or (W = 'gaul') Or (W = 'britain') Or (W = 'turkey') Or (W =
         'egypt') Or (W = 'algeria') Or (W = 'tunisia') Then
    CountryKind := 'f'
  Else If (W = 'canada') Or (W = 'portugal') Or (W = 'japan') Or (W = 'brazil')
      Or (W = 'mexico') Or
          (W = 'denmark') Or (W = 'morocco') Or (W = 'quebec') Then
    CountryKind := 'm'
  Else If (W = 'usa') Or (W = 'states') Then
    CountryKind := 'p';
End;

Function IsMealNoun(Const W: String): Boolean;
Begin
  IsMealNoun := (W = 'breakfast') Or (W = 'lunch') Or (W = 'dinner') Or (W =
      'coffee') Or
                (W = 'tea') Or (W = 'meal') Or (W = 'drink') Or (W = 'juice') Or
                    (W = 'beer') Or
                (W = 'wine') Or (W = 'milk') Or (W = 'cake') Or (W = 'soup');
End;

Function IsWeatherAdj(Const W: String): Boolean;
Begin
  IsWeatherAdj := (W = 'cold') Or (W = 'hot') Or (W = 'warm') Or (W = 'cool') Or
      (W = 'nice') Or
                  (W = 'bad') Or (W = 'good') Or (W = 'beautiful') Or (W =
                      'terrible');
End;

Function IsCalendar(Const W: String): Boolean;
Begin
  IsCalendar := (W = 'monday') Or (W = 'tuesday') Or (W = 'wednesday') Or (W =
      'thursday') Or
                (W = 'friday') Or (W = 'saturday') Or (W = 'sunday') Or (W =
                    'january') Or
                (W = 'february') Or (W = 'march') Or (W = 'april') Or (W =
                    'may') Or (W = 'june') Or
                (W = 'july') Or (W = 'august') Or (W = 'september') Or (W =
                    'october') Or
                (W = 'november') Or (W = 'december') Or (W = 'english') Or (W =
                    'french') Or
                (W = 'german') Or (W = 'spanish') Or (W = 'italian') Or (W =
                    'chinese') Or
                (W = 'japanese') Or (W = 'russian') Or (W = 'portuguese') Or (W
                    = 'arabic') Or (W = 'swedish') Or (W = 'danish');
End;

{ ------------------------------------------------------------------ }
{ Jetons                                                              }
{ ------------------------------------------------------------------ }

Type
  TTok = Record
    W, Orig: String[30];
    Cap, Punct: Boolean;
    Tag: Char;
    N, V, A, R, O, AdvA: Integer;
    NPlural: Boolean;
    VForm: Char;
    Fixed: String[30];
  End;

Var
  Tk: Array[0..MaxTokens + 1] Of TTok;
  TN: Integer;
  EndMark: Char;

Procedure AddTok(Const W, Orig: String; Cap, Punct: Boolean);
Begin
  If TN >= MaxTokens Then Exit;
  Inc(TN);
  FillChar(Tk[TN], SizeOf(TTok), 0);
  Tk[TN].W := W;
  Tk[TN].Orig := Orig;
  Tk[TN].Cap := Cap;
  Tk[TN].Punct := Punct;
  Tk[TN].VForm := 'b';
End;

Function IsWordChar(C: Char): Boolean;
Begin
  IsWordChar := ((C >= 'a') And (C <= 'z')) Or ((C >= 'A') And (C <= 'Z')) Or
                ((C >= '0') And (C <= '9')) Or (Ord(C) >= 128);
End;

Function IsPronounStem(Const W: String): Boolean;
Begin
  IsPronounStem := (W = 'he') Or (W = 'she') Or (W = 'it') Or (W = 'that') Or (W
      = 'there') Or
                   (W = 'what') Or (W = 'where') Or (W = 'who') Or (W = 'how')
                       Or (W = 'here') Or
                   (W = 'this') Or (W = 'when') Or (W = 'why');
End;

{ Ajoute un mot en developpant les contractions (don't, I'm, ...). }
Procedure AddWord(Const Original: String; Cap: Boolean);
Var
  W, Stem: String;
  P: Integer;
Begin
  W := LowerAscii(Original);
  If W = 'let''s' Then
  Begin
    AddTok('let', 'let', False, False);
    AddTok('us', 'us', False, False);
    Exit;
  End;
  If W = 'can''t' Then
  Begin
    AddTok('can', 'can', Cap, False);
    AddTok('not', 'not', False, False);
    Exit;
  End;
  If W = 'cannot' Then
  Begin
    AddTok('can', 'can', Cap, False);
    AddTok('not', 'not', False, False);
    Exit;
  End;
  If W = 'won''t' Then
  Begin
    AddTok('will', 'will', Cap, False);
    AddTok('not', 'not', False, False);
    Exit;
  End;
  If W = 'ain''t' Then
  Begin
    AddTok('is', 'is', Cap, False);
    AddTok('not', 'not', False, False);
    Exit;
  End;
  If EndsWith(W, 'n''t') And (Length(W) > 3) Then
  Begin
    AddTok(Copy(W, 1, Length(W) - 3), Copy(Original, 1, Length(W) - 3), Cap,
        False);
    AddTok('not', 'not', False, False);
    Exit;
  End;
  P := Pos('''', W);
  If (P > 1) And (P < Length(W)) Then
  Begin
    Stem := Copy(W, 1, P - 1);
    If EndsWith(W, '''m') Then
    Begin
      AddTok(Stem, Copy(Original, 1, P - 1), Cap, False);
      AddTok('am', 'am', False, False);
      Exit;
    End;
    If EndsWith(W, '''re') Then
    Begin
      AddTok(Stem, Copy(Original, 1, P - 1), Cap, False);
      AddTok('are', 'are', False, False);
      Exit;
    End;
    If EndsWith(W, '''ve') Then
    Begin
      AddTok(Stem, Copy(Original, 1, P - 1), Cap, False);
      AddTok('have', 'have', False, False);
      Exit;
    End;
    If EndsWith(W, '''ll') Then
    Begin
      AddTok(Stem, Copy(Original, 1, P - 1), Cap, False);
      AddTok('will', 'will', False, False);
      Exit;
    End;
    If EndsWith(W, '''d') Then
    Begin
      AddTok(Stem, Copy(Original, 1, P - 1), Cap, False);
      AddTok('would', 'would', False, False);
      Exit;
    End;
    If EndsWith(W, '''s') Then
    Begin
      AddTok(Stem, Copy(Original, 1, P - 1), Cap, False);
      If IsPronounStem(Stem) Then
        AddTok('is', 'is', False, False)
      Else
        AddTok('''s', '''s', False, False);
      Exit;
    End;
  End;
  AddTok(W, Original, Cap, False);
End;

Procedure Tokenize(Const Sentence: String);
Var
  I, L, Start: Integer;
  S, Original: String;
  C: Char;
Begin
  TN := 0;
  EndMark := #0;
  S := Sentence;
  L := Length(S);
  While (L > 0) And (S[L] = ' ') Do
    Dec(L);
  While (L > 0) And (S[L] In ['.', '!', '?']) Do
  Begin
    If EndMark = #0 Then EndMark := S[L];
    If S[L] = '?' Then EndMark := '?';
    Dec(L);
  End;
  If L = 0 Then Exit;
  I := 1;
  While I <= L Do
  Begin
    C := S[I];
    If C = ' ' Then
      Inc(I)
    Else If IsWordChar(C) Then
    Begin
      Start := I;
      While (I <= L) And (IsWordChar(S[I]) Or ((S[I] = '''') And (I < L) And
          IsWordChar(S[I + 1]) And
            (I > Start)) Or ((S[I] = '-') And (I < L) And IsWordChar(S[I + 1])))
                Do
        Inc(I);
      Original := Copy(S, Start, I - Start);
      AddWord(Original, (Original[1] >= 'A') And (Original[1] <= 'Z'));
    End
    Else If (C = ',') Or (C = ';') Or (C = ':') Then
    Begin
      AddTok(C, C, False, True);
      Inc(I);
    End
    Else
      Inc(I);
  End;
End;

{ ------------------------------------------------------------------ }
{ Classes de mots                                                     }
{ ------------------------------------------------------------------ }

Function InList(Const W, List: String): Boolean;
Begin
  InList := Pos(' ' + W + ' ', ' ' + List + ' ') > 0;
End;

{ Etiquettes : P pronom sujet, O pronom complement, H her, D article, S
  possessif,
  T demonstratif, Q quantificateur, X auxiliaire, G negation, K to, W
    interrogatif,
  U nombre, N nom, A adjectif, V verbe, R adverbe, E preposition, C conjonction,
  J interjection, Z nom propre, ',' ponctuation }
Function ClosedTag(Const W: String): Char;
Begin
  ClosedTag := #0;
  If InList(W, 'i you he she it we they') Then ClosedTag := 'P'
  Else If InList(W, 'me him us them') Then ClosedTag := 'O'
  Else If W = 'her' Then ClosedTag := 'H'
  Else If InList(W, 'the a an') Then ClosedTag := 'D'
  Else If InList(W, 'my your his our their its') Then ClosedTag := 'S'
  Else If InList(W, 'this that these those') Then ClosedTag := 'T'
  Else If InList(W,
      'some any many much few several every each all both no another more enou'+
      'gh') Then
    ClosedTag := 'Q'
  Else If InList(W,
      'am is are was were be been being have has had having do does did will w'+
      'ould shall should can could may might must') Then
    ClosedTag := 'X'
  Else If W = 'not' Then ClosedTag := 'G'
  Else If W = 'to' Then ClosedTag := 'K'
  Else If InList(W, 'what who whom whose which where when why how') Then
      ClosedTag := 'W'
  Else If W = '''s' Then ClosedTag := 'F';
End;

Function PronounSlot(Const W: String): Integer;
Begin
  If W = 'i' Then PronounSlot := 1
  Else If W = 'you' Then PronounSlot := 5
  Else If (W = 'he') Or (W = 'she') Or (W = 'it') Then PronounSlot := 3
  Else If W = 'we' Then PronounSlot := 4
  Else If W = 'they' Then PronounSlot := 6
  Else PronounSlot := 0;
End;

Function SubjectFr(Const W: String): String;
Begin
  If W = 'i' Then SubjectFr := 'je'
  Else If W = 'you' Then SubjectFr := 'vous'
  Else If W = 'he' Then SubjectFr := 'il'
  Else If W = 'she' Then SubjectFr := 'elle'
  Else If W = 'it' Then SubjectFr := 'il'
  Else If W = 'we' Then SubjectFr := 'nous'
  Else If W = 'they' Then SubjectFr := 'ils'
  Else SubjectFr := W;
End;

{ Pronom complement place devant le verbe. }
Function ClitFr(Const W: String): String;
Begin
  If (W = 'me') Or (W = 'i') Then ClitFr := 'me'
  Else If W = 'you' Then ClitFr := 'vous'
  Else If W = 'him' Then ClitFr := 'le'
  Else If W = 'her' Then ClitFr := 'la'
  Else If W = 'it' Then ClitFr := 'le'
  Else If W = 'us' Then ClitFr := 'nous'
  Else If W = 'them' Then ClitFr := 'les'
  Else ClitFr := W;
End;

{ Pronom apres une preposition. }
Function DisjunctiveFr(Const W: String): String;
Begin
  If (W = 'me') Or (W = 'i') Then DisjunctiveFr := 'moi'
  Else If W = 'you' Then DisjunctiveFr := 'vous'
  Else If (W = 'him') Or (W = 'he') Then DisjunctiveFr := 'lui'
  Else If (W = 'her') Or (W = 'she') Then DisjunctiveFr := 'elle'
  Else If W = 'it' Then DisjunctiveFr := 'cela'
  Else If (W = 'us') Or (W = 'we') Then DisjunctiveFr := 'nous'
  Else If (W = 'them') Or (W = 'they') Then DisjunctiveFr := 'eux'
  Else DisjunctiveFr := W;
End;

Function PronounGender(Const W: String): Char;
Begin
  If (W = 'she') Or (W = 'her') Then PronounGender := 'f' Else PronounGender :=
      'm';
End;

Function IsMassNoun(Const W: String): Boolean;
Begin
  IsMassNoun := InList(W,
      'water bread cheese meat rice sugar salt money coffee tea milk wine beer'+
      ' juice food fruit chocolate soup salad fish time weather rain snow wind'+
      ' music information news help');
End;

Function IsLanguage(Const W: String): Boolean;
Begin
  IsLanguage := InList(W,
      'english french german spanish italian chinese japanese russian portugue'+
      'se arabic swedish danish');
End;

Function IsProfession(Const W: String): Boolean;
Begin
  IsProfession := InList(W,
      'teacher doctor student nurse driver worker president king queen soldier'+
      ' friend');
End;

{ ------------------------------------------------------------------ }
{ Etiquetage                                                          }
{ ------------------------------------------------------------------ }

Function IsBeForm(Const W: String): Boolean;
Begin
  IsBeForm := InList(W, 'am is are was were be been being');
End;

Procedure TagTokens;
Var
  I: Integer;
  C: TCand;
  W: String;
  T, Prev: Char;
  HasN, HasV, HasA, HasR: Boolean;
  Following: Char;
Begin
  For I := 1 To TN Do
  Begin
    Tk[I].Tag := '?';
    If Tk[I].Punct Then
      Tk[I].Tag := ','
    Else
    Begin
      W := Tk[I].W;
      T := ClosedTag(W);
      If T <> #0 Then
        Tk[I].Tag := T
      Else If (W[1] >= '0') And (W[1] <= '9') Then
        Tk[I].Tag := 'U'
      Else If NumberWord(W) <> '' Then
        Tk[I].Tag := 'U'
      Else If (W = 'one') Then
        Tk[I].Tag := 'U'
      Else
      Begin
        Analyze(W, C);
        Tk[I].N := C.N;
        Tk[I].V := C.V;
        Tk[I].A := C.A;
        Tk[I].R := C.R;
        Tk[I].O := 0;
        Tk[I].NPlural := C.NPlural;
        Tk[I].VForm := C.VForm;
        Tk[I].Fixed := C.AdvFr;
        Tk[I].AdvA := C.AdvA;
        If C.P > 0 Then Tk[I].O := C.P
        Else If C.C > 0 Then Tk[I].O := C.C
        Else If C.J > 0 Then Tk[I].O := C.J;
        HasN := C.N > 0;
        HasV := C.V > 0;
        HasA := C.A > 0;
        HasR := (C.R > 0) Or (C.AdvFr <> '');

        If Tk[I].Cap And (I > 1) And Not IsCalendar(W) Then
          Tk[I].Tag := 'Z'
        Else If Tk[I].Cap And (I = 1) And Not (HasN Or HasV Or HasA Or HasR Or
            (C.P > 0) Or
                (C.C > 0) Or (C.J > 0)) Then
          Tk[I].Tag := 'Z'
        Else If (C.J > 0) And Not (HasN Or HasV Or HasA) Then
          Tk[I].Tag := 'J'
        Else If (C.P > 0) And Not (HasN Or HasV Or HasA) Then
          Tk[I].Tag := 'E'
        Else If (C.C > 0) And Not (HasN Or HasV Or HasA) Then
          Tk[I].Tag := 'C'
        Else If HasR And Not (HasN Or HasV Or HasA) Then
          Tk[I].Tag := 'R'
        Else If Not (HasN Or HasV Or HasA Or HasR) Then
          Tk[I].Tag := 'Z'
        Else
        Begin
          If I > 1 Then Prev := Tk[I - 1].Tag Else Prev := '.';
          If HasN And HasV Then
          Begin
            If Prev In ['D', 'S', 'T', 'Q', 'U', 'A', 'H', 'F'] Then
              Tk[I].Tag := 'N'
            Else If Prev In ['P', 'X', 'G', 'K', 'N', 'Z'] Then
              Tk[I].Tag := 'V'
            Else If (Prev = 'W') And (I = 2) And (C.VForm = 's') Then
              Tk[I].Tag := 'V'
            Else If (Prev = 'R') And (C.VForm In ['s', 'b', 'y']) And (I > 2)
                And
                    (Tk[I - 2].Tag In ['P', 'N', 'Z', 'X']) Then
              Tk[I].Tag := 'V'
            Else If (Prev = 'E') And (C.VForm = 'g') Then
              Tk[I].Tag := 'V'
            Else If (Prev = '.') And (C.VForm = 'b') And Not C.NPlural Then
              Tk[I].Tag := 'V'
            Else If (C.VForm In ['s', 'g', 'p', 'q', 'y']) And Not C.NPlural
                Then
              Tk[I].Tag := 'V'
            Else
              Tk[I].Tag := 'N';
          End
          Else If HasA And HasN Then
          Begin
            If I < TN Then Following := Tk[I + 1].Tag Else Following := '.';
            If (Following = 'N') Or (Following = '?') Then
              Tk[I].Tag := 'A'
            Else
              Tk[I].Tag := 'N';
          End
          Else If HasV And HasA Then
          Begin
            If Prev In ['X', 'D', 'S', 'R'] Then Tk[I].Tag := 'A' Else Tk[I].Tag
                := 'V';
          End
          Else If HasN Then
            Tk[I].Tag := 'N'
          Else If HasV Then
            Tk[I].Tag := 'V'
          Else If HasA Then
            Tk[I].Tag := 'A'
          Else
            Tk[I].Tag := 'R';
        End;
      End;
    End;
  End;

  { 'to' : marque de l'infinitif ou preposition }
  For I := 1 To TN Do
    If Tk[I].Tag = 'K' Then
    Begin
      If I < TN Then
      Begin
        If (I > 1) And (Tk[I + 1].W = 'work') And (Tk[I + 1].N > 0) And
           InList(Tk[I - 1].W,
               'go goes went going come comes came walk walks walked drive dri'+
               'ves drove') Then
        Begin
          Tk[I].Tag := 'E';
          Tk[I + 1].Tag := 'N';
          Tk[I + 1].NPlural := False;
        End
        Else If (Tk[I + 1].Tag = 'V') And (Tk[I + 1].VForm In ['b', 's', 'y'])
            Then
          Tk[I].Tag := 'K'
        Else If (Tk[I + 1].Tag = 'N') And (Tk[I + 1].V > 0) And (I > 1) And
                InList(Tk[I - 1].W,
                    'want need like love hate try start begin learn decide hop'+
                    'e prefer going wish forget remember stop have has had abl'+
                    'e') Then
        Begin
          Tk[I].Tag := 'K';
          Tk[I + 1].Tag := 'V';
          Tk[I + 1].VForm := 'b';
        End
        Else
          Tk[I].Tag := 'E';
      End
      Else
        Tk[I].Tag := 'E';
      If Tk[I].Tag = 'E' Then
      Begin
        Tk[I].O := FindLex('to', 'p');
        If Tk[I].O = 0 Then Tk[I].O := FindLex('at', 'p');
      End;
    End;

  { that : conjonction (que) apres un verbe de pensee ou de parole }
  For I := 2 To TN - 1 Do
    If (Tk[I].W = 'that') And (Tk[I - 1].Tag = 'V') And (Tk[I + 1].Tag In ['P',
        'D', 'S', 'T', 'N', 'Z']) And
       Not ((Tk[I + 1].Tag = 'N') And (Not Tk[I + 1].NPlural)) Then
    Begin
      Tk[I].Tag := 'C';
      Tk[I].O := FindLex('that', 'c');
    End;

  { her : possessif (suivi d'un nom/adjectif) ou pronom complement }
  For I := 1 To TN Do
    If Tk[I].Tag = 'H' Then
    Begin
      If (I < TN) And (Tk[I + 1].Tag In ['N', 'A', 'U', 'Z']) Then
        Tk[I].Tag := 'S'
      Else
        Tk[I].Tag := 'O';
    End;
  { you et it servent de complement apres un verbe ou une preposition : le sens
    est conserve par ClitFr / DisjunctiveFr }
End;

{ ------------------------------------------------------------------ }
{ Groupe nominal                                                      }
{ ------------------------------------------------------------------ }

Type
  TNP = Record
    Text: String;
    Gen: Char;
    Plu: Boolean;
    Head: String[24];
    Proper: Boolean;
    Used: Integer;
    Found: Boolean;
  End;

Function GenderOfNoun(L: Integer): Char;
Begin
  If Lex[L].Sub = 'f' Then GenderOfNoun := 'f' Else GenderOfNoun := 'm';
End;

Function PossessiveFr(Const W: String; Gen: Char; Plural: Boolean; VowelNext:
    Boolean): String;
Begin
  If W = 'my' Then
  Begin
    If Plural Then PossessiveFr := 'mes'
    Else If (Gen = 'm') Or VowelNext Then PossessiveFr := 'mon'
    Else PossessiveFr := 'ma';
  End
  Else If (W = 'his') Or (W = 'her') Or (W = 'its') Then
  Begin
    If Plural Then PossessiveFr := 'ses'
    Else If (Gen = 'm') Or VowelNext Then PossessiveFr := 'son'
    Else PossessiveFr := 'sa';
  End
  Else If W = 'your' Then
  Begin
    If Plural Then PossessiveFr := 'vos' Else PossessiveFr := 'votre';
  End
  Else If W = 'our' Then
  Begin
    If Plural Then PossessiveFr := 'nos' Else PossessiveFr := 'notre';
  End
  Else
  Begin
    If Plural Then PossessiveFr := 'leurs' Else PossessiveFr := 'leur';
  End;
End;

{ Mode : ' ' normal, 'g' generique (aimer, detester...), 'n' complement d'une
  phrase negative. Renvoie True si un groupe nominal a ete lu. }
Function ParseNP(Start: Integer; Mode: Char; Var NP: TNP): Boolean;
Var
  I, K: Integer;
  DetTag: Char;
  DetWord, NumberFr, QuantFr, AdvText, Pre, Post, NounText, Noun1, PossText:
      String;
  Adjs: Array[1..6] Of Integer;
  AdjAdv: Array[1..6] Of String[10];
  AdjCount, NounCount, HeadLex, AdjPos: Integer;
  Nouns: Array[1..4] Of Integer;
  NounPlural: Boolean;
  ProperName: String;
  IsProper: Boolean;
  Gen: Char;
  Plu, VowelNoun, AllFlag, BothFlag: Boolean;
  Rest: TNP;
  Det: String;
  FirstWord: String;
Begin
  FillChar(NP, SizeOf(NP), 0);
  NP.Gen := 'm';
  I := Start;
  DetTag := ' ';
  DetWord := '';
  NumberFr := '';
  QuantFr := '';
  AllFlag := False;
  BothFlag := False;
  AdjCount := 0;
  NounCount := 0;
  IsProper := False;
  ProperName := '';
  NounPlural := False;
  HeadLex := 0;

  If (I <= TN) And (Tk[I].Tag = 'Q') And (Tk[I].W = 'all') Then
  Begin
    AllFlag := True;
    Inc(I);
  End
  Else If (I <= TN) And (Tk[I].Tag = 'Q') And (Tk[I].W = 'both') Then
  Begin
    BothFlag := True;
    Inc(I);
  End;

  If I <= TN Then
    Case Tk[I].Tag Of
      'D':
      Begin
        DetTag := 'D';
        DetWord := Tk[I].W;
        Inc(I);
      End;
      'S':
      Begin
        DetTag := 'S';
        DetWord := Tk[I].W;
        Inc(I);
      End;
      'T':
        If (I < TN) And (Tk[I + 1].Tag In ['N', 'A', 'U']) Then
        Begin
          DetTag := 'T';
          DetWord := Tk[I].W;
          Inc(I);
        End;
      'Q':
      Begin
        DetTag := 'Q';
        DetWord := Tk[I].W;
        Inc(I);
      End;
      'U':
      Begin
        DetTag := 'U';
        If NumberWord(Tk[I].W) <> '' Then
          NumberFr := NumberWord(Tk[I].W)
        Else If Tk[I].W = 'one' Then
          NumberFr := 'un'
        Else
          NumberFr := Tk[I].W;
        DetWord := Tk[I].W;
        Inc(I);
      End;
    End;
  If AllFlag And (I <= TN) And (Tk[I].Tag = 'D') Then
  Begin
    DetWord := Tk[I].W;
    DetTag := 'D';
    Inc(I);
  End;

  { adjectifs, precedes eventuellement d'un adverbe (tres, trop...) }
  While I <= TN Do
  Begin
    If (Tk[I].Tag = 'A') Then
    Begin
      If AdjCount < 6 Then
      Begin
        Inc(AdjCount);
        Adjs[AdjCount] := Tk[I].A;
        AdjAdv[AdjCount] := '';
      End;
      Inc(I);
    End
    Else If (Tk[I].Tag = 'R') And (I < TN) And (Tk[I + 1].Tag = 'A') Then
    Begin
      If AdjCount < 6 Then
      Begin
        Inc(AdjCount);
        Adjs[AdjCount] := Tk[I + 1].A;
        If Tk[I].R > 0 Then AdjAdv[AdjCount] := Lex[Tk[I].R].Fr Else
            AdjAdv[AdjCount] := '';
      End;
      Inc(I, 2);
    End
    Else If (Tk[I].Tag = 'C') And (Tk[I].W = 'and') And (AdjCount > 0) And (I <
        TN) And
            (Tk[I + 1].Tag = 'A') Then
      Inc(I)
    Else
      Break;
  End;

  { noms }
  While (I <= TN) And (Tk[I].Tag = 'N') And (NounCount < 4) Do
  Begin
    Inc(NounCount);
    Nouns[NounCount] := Tk[I].N;
    If Tk[I].NPlural Then NounPlural := True Else NounPlural := False;
    Inc(I);
  End;
  If (NounCount = 0) And (I <= TN) And (Tk[I].Tag = 'Z') Then
  Begin
    IsProper := True;
    While (I <= TN) And (Tk[I].Tag = 'Z') Do
    Begin
      If ProperFr(Tk[I].W) <> '' Then
        ProperName := Join(ProperName, ProperFr(Tk[I].W))
      Else
        ProperName := Join(ProperName, Tk[I].Orig);
      If ProperName <> '' Then NP.Head := LowerAscii(Tk[I].W);
      Inc(I);
    End;
  End;

  If (NounCount = 0) And (Not IsProper) Then
  Begin
    If (AdjCount > 0) Or (DetTag <> ' ') Then
      NP.Used := I - Start
    Else
      NP.Used := 0;
    If (DetTag = 'U') And (AdjCount = 0) Then
    Begin
      NP.Text := NumberFr;
      NP.Found := True;
      NP.Used := I - Start;
      NP.Plu := NumberFr <> 'un';
      ParseNP := True;
      Exit;
    End;
    ParseNP := False;
    Exit;
  End;

  { possessif 's : "John's book" -> "le livre de John" }
  If (I <= TN) And (Tk[I].Tag = 'F') Then
  Begin
    If IsProper Then PossText := ProperName
    Else
    Begin
      PossText := NounForm(Nouns[NounCount], NounPlural);
      If GenderOfNoun(Nouns[NounCount]) = 'f' Then PossText := 'la ' + PossText
      Else PossText := 'le ' + PossText;
    End;
    If ParseNP(I + 1, Mode, Rest) Then
    Begin
      NP := Rest;
      { le possessif 's tient lieu de determinant : article defini }
      If (NP.Plu) And (Copy(NP.Text, 1, 4) = 'des ') Then NP.Text := 'les ' +
          Copy(NP.Text, 5, 200)
      Else If NP.Plu Then NP.Text := 'les ' + NP.Text
      Else If StartsVowel(NP.Text) Then NP.Text := 'l''' + NP.Text
      Else If NP.Gen = 'f' Then NP.Text := 'la ' + NP.Text
      Else NP.Text := 'le ' + NP.Text;
      NP.Text := NP.Text + ' de ' + PossText;
      NP.Used := (I + 1 + Rest.Used) - Start;
      NP.Found := True;
      ParseNP := True;
      Exit;
    End;
  End;

  If IsProper Then
  Begin
    NP.Proper := True;
    NP.Found := True;
    NP.Used := I - Start;
    NP.Gen := 'm';
    Case CountryKind(NP.Head) Of
      'f': NP.Text := 'la ' + ProperName;
      'm': NP.Text := 'le ' + ProperName;
      'p': NP.Text := 'les ' + ProperName;
    Else
      NP.Text := ProperName;
    End;
    If CountryKind(NP.Head) = 'f' Then NP.Gen := 'f';
    NP.Plu := CountryKind(NP.Head) = 'p';
    ParseNP := True;
    Exit;
  End;

  HeadLex := Nouns[NounCount];
  Gen := GenderOfNoun(HeadLex);
  Plu := NounPlural;
  If (DetTag = 'U') And (NumberFr <> 'un') Then Plu := True;
  If (DetTag = 'Q') And InList(DetWord, 'many few several both these those')
      Then Plu := True;
  If BothFlag Then Plu := True;

  { forme du nom (noms composes : dernier nom de premier nom) }
  NounText := NounForm(HeadLex, Plu);
  If NounCount > 1 Then
  Begin
    Noun1 := Lex[Nouns[NounCount - 1]].Fr;
    If NounCount > 2 Then Noun1 := Lex[Nouns[NounCount - 2]].Fr + ' ' + Noun1;
    NounText := NounText + ' de ' + Noun1;
  End;

  { adjectifs accordes }
  Pre := '';
  Post := '';
  For K := 1 To AdjCount Do
  Begin
    If Lex[Adjs[K]].Sub = 'B' Then
    Begin
      If AdjAdv[K] <> '' Then Pre := Join(Pre, AdjAdv[K]);
      Pre := Join(Pre, AdjForm(Adjs[K], Gen, Plu, StartsVowel(NounText)));
    End
    Else
    Begin
      If AdjAdv[K] <> '' Then Post := Join(Post, AdjAdv[K]);
      If Post <> '' Then
        If (K > 1) And (Lex[Adjs[K - 1]].Sub = 'A') And (AdjAdv[K] = '') Then
            Post := Post + ' et';
      Post := Join(Post, AdjForm(Adjs[K], Gen, Plu, False));
    End;
  End;

  FirstWord := Join(Pre, NounText);
  VowelNoun := StartsVowel(FirstWord);

  { determinant }
  Det := '';
  If AllFlag Then
  Begin
    If Plu Then
    Begin
      If Gen = 'f' Then Det := 'toutes les' Else Det := 'tous les';
    End
    Else If Gen = 'f' Then Det := 'toute la'
    Else Det := 'tout le';
  End
  Else If BothFlag Then
    Det := 'les deux'
  Else
    Case DetTag Of
      'D':
        If (DetWord = 'the') Then
        Begin
          If Plu Then Det := 'les' Else If Gen = 'f' Then Det := 'la' Else Det
              := 'le';
        End
        Else If Mode = 'n' Then
          Det := 'de'
        Else If Plu Then
          Det := 'des'
        Else If Gen = 'f' Then
          Det := 'une'
        Else
          Det := 'un';
      'S':
        If DetWord = 'her' Then
          Det := PossessiveFr('her', Gen, Plu, VowelNoun)
        Else
          Det := PossessiveFr(DetWord, Gen, Plu, VowelNoun And (Not Plu));
      'T':
        If Plu Then
          Det := 'ces'
        Else If Gen = 'f' Then
          Det := 'cette'
        Else If VowelNoun Then
          Det := 'cet'
        Else
          Det := 'ce';
      'U': Det := NumberFr;
      'Q':
        If InList(DetWord, 'some any') Then
        Begin
          If Mode = 'n' Then
            Det := 'de'
          Else If Plu Then
            Det := 'des'
          Else If VowelNoun Then
            Det := 'de l'''
          Else If Gen = 'f' Then
            Det := 'de la'
          Else
            Det := 'du';
        End
        Else If DetWord = 'many' Then Det := 'beaucoup de'
        Else If DetWord = 'much' Then Det := 'beaucoup de'
        Else If DetWord = 'few' Then Det := 'peu de'
        Else If DetWord = 'several' Then Det := 'plusieurs'
        Else If InList(DetWord, 'every each') Then Det := 'chaque'
        Else If DetWord = 'no' Then
        Begin
          If Gen = 'f' Then Det := 'aucune' Else Det := 'aucun';
        End
        Else If DetWord = 'another' Then
        Begin
          If Gen = 'f' Then Det := 'une autre' Else Det := 'un autre';
        End
        Else If DetWord = 'more' Then Det := 'plus de'
        Else If DetWord = 'enough' Then Det := 'assez de';
    Else
      Begin
        If (Mode = 'o') Or (Mode = 's') Then
          Det := ''
        Else If IsLanguage(Lex[HeadLex].En) And (NounCount = 1) Then
        Begin
          Det := 'le';
        End
        Else If Mode = 'g' Then
        Begin
          If Plu Then Det := 'les' Else If Gen = 'f' Then Det := 'la' Else Det
              := 'le';
        End
        Else If Plu Then
        Begin
          If Mode = 'n' Then Det := 'de' Else Det := 'des';
        End
        Else If IsMassNoun(Lex[HeadLex].En) And (NounCount = 1) Then
        Begin
          If Mode = 'n' Then
            Det := 'de'
          Else If VowelNoun Then
            Det := 'de l'''
          Else If Gen = 'f' Then
            Det := 'de la'
          Else
            Det := 'du';
        End;
      End;
    End;

  NP.Text := FirstWord;
  If Post <> '' Then NP.Text := Join(NP.Text, Post);
  If Det <> '' Then
  Begin
    If Copy(Det, Length(Det), 1) = '''' Then
      NP.Text := Det + NP.Text
    Else If (Det = 'de') And VowelNoun Then
      NP.Text := 'd''' + NP.Text
    Else
      NP.Text := Det + ' ' + NP.Text;
  End;
  NP.Gen := Gen;
  NP.Plu := Plu;
  NP.Head := Lex[HeadLex].En;
  NP.Found := True;
  NP.Used := I - Start;
  ParseNP := True;
End;

{ ------------------------------------------------------------------ }
{ Sujet et groupe verbal                                              }
{ ------------------------------------------------------------------ }

Type
  TSubj = Record
    Text: String;
    Slot: Integer;
    Gen: Char;
    Plu: Boolean;
    IsPron, IsI, IsIt, IsThere, IsDemo, IsWh, Found: Boolean;
    Pron: String[8];
    Used: Integer;
  End;

  TVG = Record
    Neg: Boolean;
    NegWord: String[8];
    Adv: String;
    AdvTok: Array[1..3] Of Integer;
    AdvCount: Integer;
    MainTok: Integer;
    BeMain, HaveMain, DoDummy, HaveTo: Boolean;
    Past, Future, Cond, Perfect, Prog, Going, Passive, ModalCond: Boolean;
    Modal: Char;
    Next: Integer;
  End;

Var
  CurSubj: TSubj;
  CurVG: TVG;

Function ParseSubject(Start: Integer; Var S: TSubj): Boolean;
Var
  NP: TNP;
  W: String;
Begin
  FillChar(S, SizeOf(S), 0);
  S.Gen := 'm';
  ParseSubject := False;
  If Start > TN Then Exit;
  W := Tk[Start].W;
  If Tk[Start].Tag = 'P' Then
  Begin
    S.Found := True;
    S.IsPron := True;
    S.Slot := PronounSlot(W);
    S.Text := SubjectFr(W);
    S.Pron := S.Text;
    S.Gen := PronounGender(W);
    S.Plu := (W = 'we') Or (W = 'they');
    S.IsI := W = 'i';
    S.IsIt := W = 'it';
    S.Used := 1;
    ParseSubject := True;
    Exit;
  End;
  If (W = 'there') And (Start < TN) And IsBeForm(Tk[Start + 1].W) Then
  Begin
    S.Found := True;
    S.IsThere := True;
    S.Slot := 3;
    S.Text := 'il';
    S.Used := 1;
    ParseSubject := True;
    Exit;
  End;
  If (Tk[Start].Tag = 'T') And (Start < TN) And IsBeForm(Tk[Start + 1].W) Then
  Begin
    S.Found := True;
    S.IsDemo := True;
    S.IsIt := True;
    S.Plu := (W = 'these') Or (W = 'those');
    If S.Plu Then S.Slot := 6 Else S.Slot := 3;
    S.Text := 'ce';
    S.Used := 1;
    ParseSubject := True;
    Exit;
  End;
  If Tk[Start].Tag = 'W' Then
  Begin
    S.Found := True;
    S.IsWh := True;
    S.Slot := 3;
    If W = 'who' Then S.Text := 'qui' Else S.Text := 'qu''est-ce qui';
    S.Used := 1;
    ParseSubject := True;
    Exit;
  End;
  If ParseNP(Start, ' ', NP) Then
  Begin
    S.Found := True;
    S.Text := NP.Text;
    S.Gen := NP.Gen;
    S.Plu := NP.Plu;
    If NP.Plu Then S.Slot := 6 Else S.Slot := 3;
    S.Used := NP.Used;
    ParseSubject := True;
  End;
End;

Procedure ParseVG(Start: Integer; Var VG: TVG);
Var
  I, J: Integer;
  W: String;
Begin
  FillChar(VG, SizeOf(VG), 0);
  VG.Modal := ' ';
  VG.NegWord := 'pas';
  I := Start;
  While I <= TN Do
  Begin
    W := Tk[I].W;
    If Tk[I].Tag = 'X' Then
    Begin
      If InList(W, 'will shall') Then
        VG.Future := True
      Else If W = 'would' Then
        VG.Cond := True
      Else If InList(W, 'can may') Then
        VG.Modal := 'p'
      Else If InList(W, 'could might') Then
      Begin
        VG.Modal := 'p';
        VG.ModalCond := True;
      End
      Else If W = 'must' Then
        VG.Modal := 'd'
      Else If W = 'should' Then
      Begin
        VG.Modal := 'd';
        VG.ModalCond := True;
      End
      Else If InList(W, 'do does') Then
        VG.DoDummy := True
      Else If W = 'did' Then
      Begin
        VG.DoDummy := True;
        VG.Past := True;
      End
      Else If IsBeForm(W) Then
      Begin
        If InList(W, 'was were') Then VG.Past := True;
        { be + mot a la fois verbe et adjectif (open, close...) : adjectif }
        If (I < TN) And (Tk[I + 1].Tag = 'V') And (Tk[I + 1].A > 0) And
           (Tk[I + 1].VForm = 'b') Then
          Tk[I + 1].Tag := 'A';
        If (I + 3 <= TN) And (Tk[I + 1].W = 'going') And (Tk[I + 2].Tag = 'K')
            And
           (Tk[I + 3].Tag = 'V') Then
        Begin
          VG.Going := True;
          Inc(I, 2);
        End
        Else If (I < TN) And (Tk[I + 1].Tag = 'V') And (Tk[I + 1].VForm = 'g')
            Then
          VG.Prog := True
        Else If (I < TN) And (Tk[I + 1].Tag = 'V') And (Tk[I + 1].VForm In ['q',
            'y']) And
                Not VG.Perfect Then
          VG.Passive := True
        Else
          VG.BeMain := True;
      End
      Else If InList(W, 'have has had') Then
      Begin
        J := I + 1;
        While (J < TN) And ((Tk[J].Tag = 'G') Or (Tk[J].W = 'never') Or
              ((Tk[J].Tag = 'R') And InList(Tk[J].W,
                  'ever already just always often'))) Do
          Inc(J);
        If (J <= TN) And (J > I) And (((Tk[J].Tag = 'V') And (Tk[J].VForm In
            ['q', 'y', 'p'])) Or
           (Tk[J].W = 'been')) Then
          VG.Perfect := True
        Else If (I < TN) And (Tk[I + 1].Tag = 'K') Then
        Begin
          VG.Modal := 'd';
          VG.HaveTo := True;
          Inc(I);
        End
        Else
        Begin
          VG.HaveMain := True;
          If W = 'had' Then VG.Past := True;
        End;
      End;
    End
    Else If Tk[I].Tag = 'G' Then
      VG.Neg := True
    Else If (Tk[I].W = 'never') Then
    Begin
      VG.Neg := True;
      VG.NegWord := 'jamais';
    End
    Else If (Tk[I].Tag = 'R') And (I < TN) And (Tk[I + 1].Tag In ['X', 'V'])
        Then
    Begin
      If VG.AdvCount < 3 Then
      Begin
        Inc(VG.AdvCount);
        VG.AdvTok[VG.AdvCount] := I;
      End;
      If Tk[I].R > 0 Then VG.Adv := Join(VG.Adv, Lex[Tk[I].R].Fr)
      Else VG.Adv := Join(VG.Adv, Tk[I].Fixed);
    End
    Else If Tk[I].Tag = 'V' Then
    Begin
      VG.MainTok := I;
      VG.Next := I + 1;
      Exit;
    End
    Else
      Break;
    Inc(I);
  End;
  VG.Next := I;
End;

{ ------------------------------------------------------------------ }
{ Complements                                                         }
{ ------------------------------------------------------------------ }

Function WeatherFr(Const En: String): String;
Begin
  If En = 'cold' Then WeatherFr := 'froid'
  Else If (En = 'hot') Or (En = 'warm') Then WeatherFr := 'chaud'
  Else If En = 'cool' Then WeatherFr := 'frais'
  Else If (En = 'nice') Or (En = 'beautiful') Or (En = 'good') Then WeatherFr :=
      'beau'
  Else If (En = 'bad') Or (En = 'terrible') Then WeatherFr := 'mauvais'
  Else WeatherFr := '';
End;

Procedure AddClitic(Var Clit: String; Const New: String);
Begin
  If Clit = '' Then
    Clit := New
  Else If (New = 'me') Or (New = 'nous') Or (New = 'vous') Then
    Clit := New + ' ' + Clit
  Else
    Clit := Clit + ' ' + New;
End;

Function PrepFr(I: Integer): String;
Begin
  If Tk[I].O > 0 Then PrepFr := Lex[Tk[I].O].Fr Else PrepFr := Tk[I].Orig;
End;

{ Complement introduit par une preposition. }
Function TranslatePP(Var I: Integer): String;
Var
  W, Obj, PrepW: String;
  J, K: Integer;
  NP: TNP;
  Kind: Char;
  Name: String;
Begin
  W := Tk[I].W;
  PrepW := PrepFr(I);
  J := I + 1;
  TranslatePP := PrepW;
  If J > TN Then
  Begin
    I := J;
    Exit;
  End;
  { expressions de temps }
  If (W = 'in') Or (W = 'at') Or (W = 'on') Then
  Begin
    K := J;
    If (K <= TN) And (Tk[K].W = 'the') Then Inc(K);
    If (K <= TN) And (Tk[K].N > 0) Then
    Begin
      If (W = 'in') And (Tk[K].W = 'morning') Then
      Begin
        TranslatePP := 'le matin';
        I := K + 1;
        Exit;
      End;
      If (W = 'in') And (Tk[K].W = 'afternoon') Then
      Begin
        TranslatePP := 'l''après-midi';
        I := K + 1;
        Exit;
      End;
      If (W = 'in') And (Tk[K].W = 'evening') Then
      Begin
        TranslatePP := 'le soir';
        I := K + 1;
        Exit;
      End;
      If (W = 'at') And (Tk[K].W = 'night') Then
      Begin
        TranslatePP := 'la nuit';
        I := K + 1;
        Exit;
      End;
      If (W = 'on') And InList(Tk[K].W,
          'monday tuesday wednesday thursday friday saturday sunday') Then
      Begin
        TranslatePP := Lex[Tk[K].N].Fr;
        I := K + 1;
        Exit;
      End;
      If (W = 'in') And IsCalendar(Tk[K].W) And Not InList(Tk[K].W,
          'monday tuesday wednesday thursday friday saturday sunday english fr'+
          'ench german spanish italian chinese japanese russian portuguese ara'+
          'bic swedish danish') Then
      Begin
        TranslatePP := 'en ' + Lex[Tk[K].N].Fr;
        I := K + 1;
        Exit;
      End;
    End;
    If (Tk[J].Tag = 'U') And (Tk[J].W[1] >= '0') And (Tk[J].W[1] <= '9') Then
    Begin
      If W = 'in' Then
        TranslatePP := 'en ' + Tk[J].W
      Else If W = 'at' Then
        TranslatePP := 'à ' + Tk[J].W + ' heures'
      Else
        TranslatePP := PrepW + ' ' + Tk[J].W;
      I := J + 1;
      If (I <= TN) And ((Tk[I].W = 'o''clock') Or (Tk[I].W = 'oclock')) Then
          Inc(I);
      Exit;
    End;
  End;
  { noms de lieux }
  If Tk[J].Tag = 'Z' Then
  Begin
    Name := Tk[J].Orig;
    If ProperFr(Tk[J].W) <> '' Then Name := ProperFr(Tk[J].W);
    Kind := CountryKind(Tk[J].W);
    Obj := Name;
    If InList(W, 'in to at') Then
    Begin
      Case Kind Of
        'f': TranslatePP := 'en ' + Name;
        'm': TranslatePP := 'au ' + Name;
        'p': TranslatePP := 'aux ' + Name;
      Else
        TranslatePP := 'à ' + Name;
      End;
    End
    Else If W = 'of' Then
    Begin
      Case Kind Of
        'f': TranslatePP := 'de la ' + Name;
        'm': TranslatePP := 'du ' + Name;
        'p': TranslatePP := 'des ' + Name;
      Else
        TranslatePP := 'de ' + Name;
      End;
    End
    Else If W = 'from' Then
    Begin
      Case Kind Of
        'f': TranslatePP := 'de ' + Name;
        'm': TranslatePP := 'du ' + Name;
        'p': TranslatePP := 'des ' + Name;
      Else
        TranslatePP := 'de ' + Name;
      End;
    End
    Else
      TranslatePP := PrepW + ' ' + Name;
    I := J + 1;
    While (I <= TN) And (Tk[I].Tag = 'Z') Do
    Begin
      TranslatePP := TranslatePP + ' ' + Tk[I].Orig;
      Inc(I);
    End;
    Exit;
  End;
  { pronom }
  If Tk[J].Tag In ['O', 'P'] Then
  Begin
    TranslatePP := PrepW + ' ' + DisjunctiveFr(Tk[J].W);
    I := J + 1;
    Exit;
  End;
  { gerondif : for helping -> pour aider }
  If (Tk[J].Tag = 'V') And (Tk[J].VForm = 'g') Then
  Begin
    TranslatePP := PrepW + ' ' + Lex[Tk[J].V].Fr;
    I := J + 1;
    Exit;
  End;
  If W = 'of' Then Kind := 'o' Else If W = 'in' Then Kind := 's' Else Kind :=
      ' ';
  If ParseNP(J, Kind, NP) Then
  Begin
    TranslatePP := PrepW + ' ' + NP.Text;
    I := J + NP.Used;
    Exit;
  End;
  I := J;
End;

Function WhTailFr(Const W: String): String;
Begin
  If W = 'where' Then WhTailFr := 'où'
  Else If W = 'when' Then WhTailFr := 'quand'
  Else If W = 'why' Then WhTailFr := 'pourquoi'
  Else If W = 'how' Then WhTailFr := 'comment'
  Else If (W = 'who') Or (W = 'whom') Then WhTailFr := 'qui'
  Else If W = 'which' Then WhTailFr := 'quel'
  Else WhTailFr := 'ce que';
End;

Function ObjectMode(Const VerbEn: String): Char;
Begin
  If InList(VerbEn, 'speak talk') Then
    ObjectMode := 's'
  Else If InList(VerbEn, 'like love hate prefer enjoy') Then
    ObjectMode := 'g'
  Else If CurVG.Neg Then
    ObjectMode := 'n'
  Else
    ObjectMode := ' ';
End;

{ Complements apres le verbe : renvoie le texte, les pronoms complements a
  placer
  devant le verbe (Clit) ou devant l'infinitif (ClitInf). }
Function TranslateTail(First, Last: Integer; Const VerbEn: String; Var Clit:
    String;
                       Var ClitInf: String): String;
Var
  I, J: Integer;
  Res, T, LinkWord: String;
  NP: TNP;
  Mode: Char;
  Gen: Char;
  Plu: Boolean;
  AdvText: String;
  ObjClit: String;
  BeLike: Boolean;
Begin
  Res := '';
  I := First;
  Mode := ObjectMode(VerbEn);
  BeLike := CurVG.BeMain Or InList(VerbEn,
      'become seem look feel taste smell sound');
  Gen := CurSubj.Gen;
  Plu := CurSubj.Plu;
  While I <= Last Do
  Begin
    Case Tk[I].Tag Of
      'O', 'P':
      Begin
        If (Tk[I].Tag = 'P') And (I = First) And BeLike Then
        Begin
          Res := Join(Res, DisjunctiveFr(Tk[I].W));
          Inc(I);
        End
        Else
        Begin
          { verbe a deux complements : "give him the book" -> lui }
          J := I + 1;
          While (J <= Last) And Not (Tk[J].Tag In ['N', 'D', 'S', 'T', 'Q', 'U',
              'Z']) Do
            Inc(J);
          If InList(VerbEn,
              'give send show tell bring teach write offer sell lend') And (J <=
              Last) And
             (J = I + 1) And InList(Tk[I].W, 'him her them it') Then
          Begin
            If Tk[I].W = 'them' Then AddClitic(Clit, 'leur') Else
                AddClitic(Clit, 'lui');
          End
          Else
            AddClitic(Clit, ClitFr(Tk[I].W));
          Inc(I);
        End;
      End;
      'K':
      Begin
        If (I < Last) And (Tk[I + 1].Tag = 'V') Then
        Begin
          LinkWord := '';
          If (CurVG.MainTok > 0) And (Tk[CurVG.MainTok].V > 0) Then
            LinkWord := Lex[Tk[CurVG.MainTok].V].Link;
          ClitInf := '';
          J := I + 2;
          While (J <= Last) And (Tk[J].Tag In ['O', 'P']) Do
          Begin
            AddClitic(ClitInf, ClitFr(Tk[J].W));
            Inc(J);
          End;
          Res := Join(Res, Join(LinkWord, Join(ClitInf, Lex[Tk[I + 1].V].Fr)));
          ClitInf := '';
          I := J;
          If Mode = 'g' Then Mode := ' ';
        End
        Else
        Begin
          Res := Join(Res, 'à');
          Inc(I);
        End;
      End;
      'E': Res := Join(Res, TranslatePP(I));
      'N', 'D', 'S', 'T', 'Q', 'U', 'Z':
      Begin
        If InList(VerbEn,
            'give send show tell bring teach write offer sell lend') And
            ParseNP(I, Mode, NP) And
           (I + NP.Used <= Last) And (Tk[I + NP.Used].Tag In ['N', 'D', 'S',
               'T', 'Q', 'U']) Then
        Begin
          { give the woman a flower -> donner une fleur a la femme }
          T := NP.Text;
          J := I + NP.Used;
          If ParseNP(J, Mode, NP) Then
          Begin
            Res := Join(Res, Join(NP.Text, 'à ' + T));
            I := J + NP.Used;
          End
          Else
          Begin
            Res := Join(Res, T);
            I := J;
          End;
        End
        Else If ParseNP(I, Mode, NP) Then
        Begin
          T := NP.Text;
          { metier apres etre : pas d'article }
          If BeLike And ((Tk[I].W = 'a') Or (Tk[I].W = 'an')) Then
            If IsProfession(NP.Head) And Not NP.Plu Then
            Begin
              If Copy(T, 1, 3) = 'un ' Then Delete(T, 1, 3)
              Else If Copy(T, 1, 4) = 'une ' Then Delete(T, 1, 4);
            End;
          Res := Join(Res, T);
          Inc(I, NP.Used);
        End
        Else
        Begin
          Res := Join(Res, Tk[I].Orig);
          Inc(I);
        End;
      End;
      'A', 'R':
      Begin
        If (Tk[I].Tag = 'R') And (I < Last) And (Tk[I + 1].Tag = 'A') And BeLike
            Then
        Begin
          If Tk[I].R > 0 Then AdvText := Lex[Tk[I].R].Fr Else AdvText :=
              Tk[I].Fixed;
          Res := Join(Res, AdvText);
          Inc(I);
        End
        Else If (Tk[I].Tag = 'A') Then
        Begin
          If BeLike Then
            Res := Join(Res, AdjForm(Tk[I].A, Gen, Plu, False))
          Else If ParseNP(I, Mode, NP) Then
          Begin
            Res := Join(Res, NP.Text);
            Inc(I, NP.Used - 1);
          End
          Else
            Res := Join(Res, AdjForm(Tk[I].A, 'm', False, False));
          Inc(I);
        End
        Else
        Begin
          If Tk[I].R > 0 Then
            Res := Join(Res, Lex[Tk[I].R].Fr)
          Else If Tk[I].Fixed <> '' Then
            Res := Join(Res, Tk[I].Fixed)
          Else
            Res := Join(Res, Tk[I].Orig);
          Inc(I);
        End;
      End;
      'C':
      Begin
        If Tk[I].O > 0 Then Res := Join(Res, Lex[Tk[I].O].Fr) Else Res :=
            Join(Res, Tk[I].Orig);
        Inc(I);
      End;
      'J':
      Begin
        If Tk[I].O > 0 Then Res := Join(Res, Lex[Tk[I].O].Fr) Else Res :=
            Join(Res, Tk[I].Orig);
        Inc(I);
      End;
      ',':
      Begin
        Res := Res + ',';
        Inc(I);
      End;
      'V':
      Begin
        If Tk[I].V > 0 Then
          Res := Join(Res, Lex[Tk[I].V].Fr)
        Else
          Res := Join(Res, Tk[I].Orig);
        Inc(I);
      End;
      'G':
      Begin
        Inc(I);
      End;
      'W':
      Begin
        Res := Join(Res, WhTailFr(Tk[I].W));
        Inc(I);
      End;
      'X':
      Begin
        If Tk[I].W = 'is' Then Res := Join(Res, 'est')
        Else If Tk[I].W = 'are' Then Res := Join(Res, 'sont')
        Else If Tk[I].W = 'was' Then Res := Join(Res, 'était')
        Else If Tk[I].W = 'were' Then Res := Join(Res, 'étaient')
        Else If Tk[I].W = 'am' Then Res := Join(Res, 'suis');
        Inc(I);
      End;
    Else
      Res := Join(Res, Tk[I].Orig);
      Inc(I);
    End;
  End;
  TranslateTail := Res;
End;

{ ------------------------------------------------------------------ }
{ Formes verbales                                                     }
{ ------------------------------------------------------------------ }

Function AgreeParticiple(Const Part: String; Aux: Char; Const S: TSubj): String;
Var
  R: String;
Begin
  R := Part;
  If Aux = 'e' Then
  Begin
    If (S.Gen = 'f') And Not EndsWith(R, 'e') Then R := R + 'e';
    If S.Plu And Not S.IsPron Then R := PluralOf(R)
    Else If S.Plu And (S.Slot <> 5) Then R := PluralOf(R);
  End;
  AgreeParticiple := R;
End;

{ Verbe dont le passe s'ecrit comme l'infinitif (read, put, cut) : passe
  seulement avec un indice de temps. }
Function MainIsPast(Tok: Integer): Boolean;
Var
  I: Integer;
  Cue: Boolean;
Begin
  MainIsPast := False;
  If Tok < 1 Then Exit;
  If Not (Tk[Tok].VForm In ['p', 'y']) Then Exit;
  If (Tk[Tok].V > 0) And (Tk[Tok].W = Lex[Tk[Tok].V].En) Then
  Begin
    Cue := False;
    For I := 1 To TN Do
      If InList(Tk[I].W, 'yesterday ago last') Then Cue := True;
    MainIsPast := Cue;
  End
  Else
    MainIsPast := True;
End;

Procedure BuildVerbForms(Const VG: TVG; Const S: TSubj; Const Inf0: String;
    AuxType: Char;
                         Var Fin, Nonfin: String; Var OnInf: Boolean);
Var
  Slot: Integer;
  Tense: Char;
  ModalInf, AuxInf: String;
  MainPast, PastFlag: Boolean;
Begin
  Slot := S.Slot;
  If Slot < 1 Then Slot := 3;
  OnInf := False;
  Nonfin := '';
  MainPast := False;
  If VG.MainTok > 0 Then
    MainPast := MainIsPast(VG.MainTok) And Not VG.Perfect And Not VG.Prog And
                Not VG.Passive;
  PastFlag := VG.Past;
  If VG.Passive Then
  Begin
    If PastFlag Then Tense := 'i' Else If VG.Future Then Tense := 'f' Else Tense
        := 'p';
    Fin := Conjugate('être', Slot, Tense);
    Nonfin := AgreeParticiple(PastParticiple(Inf0), 'e', S);
  End
  Else If VG.Modal <> ' ' Then
  Begin
    If VG.Modal = 'd' Then ModalInf := 'devoir' Else ModalInf := 'pouvoir';
    If VG.ModalCond Then Tense := 'c'
    Else If VG.Future Then Tense := 'f'
    Else If PastFlag Or MainPast Then Tense := 'i'
    Else Tense := 'p';
    Fin := Conjugate(ModalInf, Slot, Tense);
    Nonfin := Inf0;
    OnInf := True;
  End
  Else If VG.Going Then
  Begin
    If PastFlag Then Tense := 'i' Else Tense := 'p';
    Fin := Conjugate('aller', Slot, Tense);
    Nonfin := Inf0;
    OnInf := True;
  End
  Else If VG.Future Then
    Fin := Conjugate(Inf0, Slot, 'f')
  Else If VG.Cond Then
    Fin := Conjugate(Inf0, Slot, 'c')
  Else If (VG.Perfect Or ((PastFlag Or MainPast) And Not VG.BeMain And Not
      VG.HaveMain And
          Not VG.Prog)) And (Inf0 <> '') Then
  Begin
    If AuxType = 'e' Then AuxInf := 'être' Else AuxInf := 'avoir';
    Fin := Conjugate(AuxInf, Slot, 'p');
    Nonfin := AgreeParticiple(PastParticiple(Inf0), AuxType, S);
  End
  Else If PastFlag Or MainPast Then
    Fin := Conjugate(Inf0, Slot, 'i')
  Else
    Fin := Conjugate(Inf0, Slot, 'p');
End;

Function WhFr(Const W: String): String;
Begin
  If W = 'where' Then WhFr := 'où'
  Else If W = 'when' Then WhFr := 'quand'
  Else If W = 'why' Then WhFr := 'pourquoi'
  Else If W = 'how' Then WhFr := 'comment'
  Else If (W = 'who') Or (W = 'whom') Then WhFr := 'qui'
  Else If W = 'whose' Then WhFr := 'à qui'
  Else If W = 'which' Then WhFr := 'quel'
  Else WhFr := 'que';
End;

Function QuelFr(Gen: Char; Plural: Boolean): String;
Begin
  If Gen = 'f' Then
  Begin
    If Plural Then QuelFr := 'quelles' Else QuelFr := 'quelle';
  End
  Else If Plural Then QuelFr := 'quels'
  Else QuelFr := 'quel';
End;

Function ImpClit(Const C: String): String;
Begin
  If C = 'me' Then ImpClit := 'moi' Else ImpClit := C;
End;

Function HyphenJoin(Const S: String): String;
Var
  I: Integer;
  R: String;
Begin
  R := S;
  For I := 1 To Length(R) Do
    If R[I] = ' ' Then R[I] := '-';
  HyphenJoin := R;
End;

Function ClauseHasVerb(A, B: Integer): Boolean;
Var
  I: Integer;
Begin
  ClauseHasVerb := False;
  For I := A To B Do
    If Tk[I].Tag In ['X', 'V'] Then ClauseHasVerb := True;
End;

Function ConjOf(I: Integer): String;
Begin
  If Tk[I].O > 0 Then ConjOf := Lex[Tk[I].O].Fr Else ConjOf := Tk[I].Orig;
End;

{ ------------------------------------------------------------------ }
{ Proposition                                                         }
{ ------------------------------------------------------------------ }

Function TranslateClause(A, B: Integer; IsQ: Boolean): String;
Var
  I, Aux0, EndSubj, K: Integer;
  WhW, Wh, Inf0, Fin, Nonfin, Tail, Clit, ClitInf, Res, VerbEn, Idiom, Pre,
      Pos2: String;
  Subj: TSubj;
  VG: TVG;
  OnInf, Inverted, Imperative, Please, BeLike, Inv, EstCe, WhSubject, NoPas:
      Boolean;
  Tmp: TTok;
  AuxType: Char;
  NP: TNP;
  FinInv, PronInv: String;
  Tense: String;
  L: Integer;
  ClitBeforeFin, ClitBeforeInf: String;
  Next: Integer;
Begin
  TranslateClause := '';
  If A > B Then Exit;
  Wh := '';
  WhW := '';
  FillChar(Subj, SizeOf(Subj), 0);
  Inverted := False;
  Imperative := False;
  Please := False;
  WhSubject := False;
  Idiom := '';

  { interjection ou mot de liaison en tete }
  If (Tk[A].Tag = 'J') And (A < B) And (Tk[A].W = 'please') Then
  Begin
    Please := True;
    Inc(A);
  End
  Else If (Tk[A].Tag = 'J') And (A < B) Then
  Begin
    TranslateClause := ConjOf(A) + ' ' + TranslateClause(A + 1, B, IsQ);
    Exit;
  End
  Else If (Tk[A].Tag = 'J') And (A = B) Then
  Begin
    TranslateClause := ConjOf(A);
    Exit;
  End;
  If (Tk[A].Tag = 'C') And (A < B) Then
  Begin
    TranslateClause := ConjOf(A) + ' ' + TranslateClause(A + 1, B, IsQ);
    Exit;
  End;

  { adverbe de temps en tete : today, tomorrow... }
  If (Tk[A].Tag = 'R') And (A < B) And (Tk[A + 1].Tag In ['P', 'D', 'S', 'N',
      'Z', 'T', 'Q', 'U']) And
     (Tk[A].R > 0) Then
  Begin
    TranslateClause := Lex[Tk[A].R].Fr + ' ' + TranslateClause(A + 1, B, IsQ);
    Exit;
  End;

  { mot interrogatif }
  If Tk[A].Tag = 'W' Then
  Begin
    WhW := Tk[A].W;
    If (WhW = 'how') And (A < B) And ((Tk[A + 1].W = 'much') Or (Tk[A + 1].W =
        'many')) Then
    Begin
      Wh := 'combien';
      If Tk[A + 1].W = 'many' Then Wh := 'combien de';
      Inc(A, 2);
      If (Wh = 'combien de') And (A <= B) And (Tk[A].Tag = 'N') And
         Not ((B - A >= 2) And IsBeForm(Tk[B - 1].W) And (Tk[B].W = 'there'))
             Then
      Begin
        Wh := 'combien de ' + NounForm(Tk[A].N, True);
        Inc(A);
      End;
    End
    Else If ((WhW = 'what') Or (WhW = 'which')) And (A < B) And (Tk[A + 1].Tag =
        'N') Then
    Begin
      If ParseNP(A + 1, ' ', NP) Then
      Begin
        Wh := QuelFr(NP.Gen, NP.Plu) + ' ' + NounForm(Tk[A + 1].N, Tk[A +
            1].NPlural);
        Inc(A, 1 + NP.Used);
      End
      Else
        Inc(A);
    End
    Else
    Begin
      Wh := WhFr(WhW);
      Inc(A);
    End;
    If (A > B) Then
    Begin
      TranslateClause := UpperFirst(Wh);
      Exit;
    End;
  End;

  { combien de N y a-t-il }
  If (Copy(Wh, 1, 7) = 'combien') And (B - A >= 2) And IsBeForm(Tk[B - 1].W) And
     (Tk[B].W = 'there') Then
  Begin
    If ParseNP(A, ' ', NP) Then
    Begin
      Res := Wh;
      If Wh = 'combien de' Then
      Begin
        If Tk[A].Tag = 'N' Then Res := Res + ' ' + NounForm(Tk[A].N, True)
        Else Res := Res + ' ' + NP.Text;
      End;
      If InList(Tk[B - 1].W, 'was were') Then Res := Res + ' y avait-il' Else
          Res := Res + ' y a-t-il';
      TranslateClause := Res;
      Exit;
    End;
  End;

  { inversion sujet-auxiliaire : on remet la phrase dans l'ordre
    sujet-auxiliaire }
  If (IsQ Or (Wh <> '')) And (Tk[A].Tag = 'X') And (A < B) Then
  Begin
    Aux0 := A;
    If (Tk[A + 1].W = 'there') And IsBeForm(Tk[A].W) Then
    Begin
      FillChar(Subj, SizeOf(Subj), 0);
      Subj.Found := True;
      Subj.IsThere := True;
      Subj.Slot := 3;
      Subj.Text := 'il';
      Subj.Used := 1;
    End
    Else If Not ParseSubject(A + 1, Subj) Then
      Subj.Found := False;
    If Subj.Found Then
    Begin
      EndSubj := A + Subj.Used;
      Tmp := Tk[Aux0];
      For K := Aux0 To EndSubj - 1 Do
        Tk[K] := Tk[K + 1];
      Tk[EndSubj] := Tmp;
      Inverted := True;
    End;
  End;

  { impératif }
  If Not Inverted Then
  Begin
    If (Tk[A].Tag = 'V') And (Tk[A].VForm In ['b', 'y']) And (Wh = '') And Not
        IsQ And
       Not ((Tk[A].W = 'let') And (A < B) And (Tk[A + 1].W = 'us')) Then
      Imperative := True
    Else If (Tk[A].W = 'do') And (A + 2 <= B) And (Tk[A + 1].Tag = 'G') And
        (Tk[A + 2].Tag = 'V') And
            (Wh = '') Then
    Begin
      Imperative := True;
      Tk[A + 1].Tag := 'G';
    End
    Else If (Tk[A].W = 'let') And (A + 2 <= B) And (Tk[A + 1].W = 'us') And
        (Tk[A + 2].Tag = 'V') Then
    Begin
      L := Tk[A + 2].V;
      Fin := Conjugate(Lex[L].Fr, 4, 'p');
      Clit := '';
      ClitInf := '';
      CurVG.BeMain := False;
      Tail := TranslateTail(A + 3, B, Lex[L].En, Clit, ClitInf);
      Res := Join(Fin, Join(ImpClit(Clit), Tail));
      If Please Then Res := 'S''il vous plaît, ' + Res;
      TranslateClause := Res;
      Exit;
    End;
  End;

  FillChar(CurVG, SizeOf(CurVG), 0);
  CurVG.Modal := ' ';
  If Imperative Then
  Begin
    FillChar(Subj, SizeOf(Subj), 0);
    Subj.Slot := 5;
    Subj.Gen := 'm';
    Subj.IsPron := True;
    Subj.Text := '';
    CurSubj := Subj;
    I := A;
    If Tk[I].W = 'do' Then
    Begin
      CurVG.Neg := True;
      CurVG.NegWord := 'pas';
      Inc(I, 2);
    End;
    L := Tk[I].V;
    Clit := '';
    ClitInf := '';
    Tail := TranslateTail(I + 1, B, Lex[L].En, Clit, ClitInf);
    Fin := Conjugate(Lex[L].Fr, 5, 'p');
    If CurVG.Neg Then
    Begin
      Res := Join('ne', Join(Clit, Join(Fin, 'pas')));
      Res := Join(Res, Tail);
    End
    Else
    Begin
      Res := Fin;
      If Clit <> '' Then Res := Res + '-' + HyphenJoin(ImpClit(Clit));
      Res := Join(Res, Tail);
    End;
    If Please Then Res := 'S''il vous plaît, ' + Res;
    TranslateClause := Res;
    Exit;
  End;

  { who / what sujet : "Who lives here ?" }
  If (Not Inverted) And (Wh <> '') And ((WhW = 'who') Or (WhW = 'what')) And (A
      <= B) And
     (Tk[A].Tag = 'V') And (Tk[A].VForm In ['s', 'b']) Then
  Begin
    FillChar(Subj, SizeOf(Subj), 0);
    Subj.Found := True;
    Subj.IsWh := True;
    Subj.Slot := 3;
    Subj.Gen := 'm';
    Subj.Used := 0;
    If WhW = 'who' Then Subj.Text := 'qui' Else Subj.Text := 'qu''est-ce qui';
    Wh := '';
  End;
  { sujet }
  If (Not Inverted) And (Not Subj.Found) Then
  Begin
    If Not ParseSubject(A, Subj) Then
    Begin
      { pas de sujet : traduction des mots a la suite }
      Clit := '';
      ClitInf := '';
      CurSubj := Subj;
      Tail := TranslateTail(A, B, '', Clit, ClitInf);
      TranslateClause := Join(Wh, Join(Clit, Tail));
      Exit;
    End;
  End
  Else
  Begin
    { sujet deja lu : il commence en A }
    If Subj.IsThere And (Tk[A].W <> 'there') Then Subj.IsThere := True;
  End;
  If Subj.IsWh Then WhSubject := True;
  CurSubj := Subj;
  I := A + Subj.Used;

  ParseVG(I, VG);
  CurVG := VG;
  Next := VG.Next;
  AuxType := 'a';
  VerbEn := '';
  Inf0 := '';
  L := 0;
  If VG.MainTok > 0 Then
  Begin
    L := Tk[VG.MainTok].V;
    VerbEn := Lex[L].En;
    Inf0 := Lex[L].Fr;
    AuxType := Lex[L].Aux;
  End
  Else If VG.BeMain Then
  Begin
    Inf0 := 'être';
    VerbEn := 'be';
  End
  Else If VG.HaveMain Then
  Begin
    Inf0 := 'avoir';
    VerbEn := 'have';
  End
  Else If VG.DoDummy Then
  Begin
    Inf0 := 'faire';
    VerbEn := 'do';
  End;

  Tail := '';
  Clit := '';
  ClitInf := '';
  If (Next <= B) And (Tk[Next].W = 'born') Then VG.Past := False;

  { idiomes avec etre / avoir }
  If VG.BeMain And (Next <= B) Then
  Begin
    If (Tk[Next].Tag = 'A') And (Lex[Tk[Next].A].Idiom <> '') And
       Not (Subj.IsIt Or Subj.IsThere Or Subj.IsDemo) And (Not VG.Perfect) Then
    Begin
      Inf0 := 'avoir';
      Idiom := Lex[Tk[Next].A].Idiom;
      VG.BeMain := False;
      VG.HaveMain := True;
      Inc(Next);
    End
    Else If (Tk[Next].Tag = 'A') And (WeatherFr(Lex[Tk[Next].A].En) <> '') And
            ((Subj.IsIt And Not Subj.IsDemo) Or Subj.IsThere) And (Subj.Text <>
                'ce') Then
    Begin
      If Tk[A].W = 'it' Then
      Begin
        Inf0 := 'faire';
        Idiom := WeatherFr(Lex[Tk[Next].A].En);
        VG.BeMain := False;
        Inc(Next);
      End;
    End
    Else If Tk[Next].W = 'from' Then
    Begin
      Inf0 := 'venir';
      VG.BeMain := False;
      AuxType := 'e';
    End;
  End;
  If VG.HaveMain And (Idiom = '') And (Next <= B) And (Tk[Next].Tag In ['N',
      'D', 'Q']) Then
  Begin
    K := Next;
    While (K <= B) And Not (Tk[K].Tag = 'N') Do
      Inc(K);
    If (K <= B) And (Tk[K].N > 0) And IsMealNoun(Lex[Tk[K].N].En) Then Inf0 :=
        'prendre';
  End;
  If VG.BeMain And (Next + 2 <= B) And (Tk[Next].Tag = 'U') And InList(Tk[Next +
      1].W, 'year years') And
     (Tk[Next + 2].W = 'old') And Not (Subj.IsIt Or Subj.IsThere) Then
  Begin
    Inf0 := 'avoir';
    If Tk[Next].W[1] In ['0'..'9'] Then Idiom := Tk[Next].W + ' ans' Else Idiom
        := NumberWord(Tk[Next].W) + ' ans';
    VG.BeMain := False;
    VG.HaveMain := True;
    Inc(Next, 3);
  End;
  If (VerbEn = 'know') And (Next <= B) And (Tk[Next].Tag In ['Z', 'O']) Then
      Inf0 := 'connaître';
  If (VerbEn = 'leave') And (Next <= B) And (Tk[Next].Tag In ['D', 'S', 'N',
      'Z', 'O']) Then
  Begin
    Inf0 := 'quitter';
    AuxType := 'a';
  End;
  If (VerbEn = 'like') Or (VerbEn = 'love') Then
  Begin
    If (Next <= B) And (Tk[Next].Tag = 'O') Then Inf0 := 'aimer';
    If VG.Cond Then Inf0 := 'vouloir';
  End;
  If (WhW = 'how') And VG.BeMain And Subj.IsPron And (Next > B) Then
  Begin
    Inf0 := 'aller';
    VG.BeMain := False;
  End;

  { verbes a particule : look for, look at, wait for, listen to, ask for, talk
    about }
  If (VG.MainTok > 0) And (Next <= B) And (Tk[Next].Tag = 'E') Then
  Begin
    If (VerbEn = 'look') And (Tk[Next].W = 'for') Then
      Inf0 := 'chercher'
    Else If (VerbEn = 'look') And (Tk[Next].W = 'at') Then
      Inf0 := 'regarder'
    Else If (VerbEn = 'wait') And (Tk[Next].W = 'for') Then
      Inf0 := 'attendre'
    Else If (VerbEn = 'listen') And (Tk[Next].W = 'to') Then
      Inf0 := 'écouter'
    Else If (VerbEn = 'ask') And (Tk[Next].W = 'for') Then
      Inf0 := 'demander'
    Else If (InList(VerbEn, 'talk speak')) And (Tk[Next].W = 'about') Then
    Begin
      Tk[Next].O := 0;
      Tk[Next].Orig := 'de';
    End;
    If (Inf0 = 'chercher') Or (Inf0 = 'regarder') Or (Inf0 = 'attendre') Or
        (Inf0 = 'écouter') Or
       (Inf0 = 'demander') Then
      Inc(Next);
  End;

  { c'est : il / ceci / cela + etre }
  If VG.BeMain And Subj.IsIt And (Idiom = '') And Not Subj.IsThere Then
  Begin
    Subj.Text := 'ce';
    Subj.Pron := 'ce';
    CurSubj.Gen := 'm';
    CurSubj.Plu := False;
  End
  Else If VG.BeMain And Subj.IsPron And (Subj.Pron = 'ils') And (Next <= B) And
          (Tk[Next].Tag In ['D', 'N', 'Q', 'U']) And (Tk[A].W = 'they') Then
  Begin
    Subj.Text := 'ce';
    Subj.Pron := 'ce';
    CurSubj := Subj;
  End;

  { conjugaison }
  If VG.Modal <> ' ' Then
    If (Not VG.HaveTo) And (VG.MainTok = 0) And Not VG.HaveMain And Not
        VG.BeMain Then Inf0 := '';
  BuildVerbForms(VG, Subj, Inf0, AuxType, Fin, Nonfin, OnInf);
  If Subj.IsThere Then
  Begin
    If VG.Past Then Tense := 'i' Else If VG.Future Then Tense := 'f' Else If
        VG.Cond Then Tense := 'c' Else Tense := 'p';
    Fin := Conjugate('avoir', 3, Tense[1]);
    Nonfin := '';
    OnInf := False;
  End
  ;

  { complements }
  If Idiom <> '' Then
    Tail := Join(Idiom, TranslateTail(Next, B, VerbEn, Clit, ClitInf))
  Else
    Tail := TranslateTail(Next, B, VerbEn, Clit, ClitInf);

  { negation }
  Pre := '';
  Pos2 := '';
  If VG.Neg Then
  Begin
    Pre := 'ne';
    Pos2 := VG.NegWord;
  End;

  { clitiques : devant l'infinitif apres un modal / aller, sinon devant le verbe
    }
  ClitBeforeFin := '';
  ClitBeforeInf := '';
  If OnInf Then ClitBeforeInf := Clit Else ClitBeforeFin := Clit;
  If Subj.IsThere Then ClitBeforeFin := 'y';

  { forme : inversion, est-ce que ou ordre direct }
  Inv := False;
  EstCe := False;
  If (IsQ Or Inverted) And Not WhSubject Then
  Begin
    If Subj.IsThere Then
      Inv := True
    Else If Subj.IsPron And Not Subj.IsI And Not VG.Neg Then
      Inv := True
    Else If Subj.IsPron And Subj.IsI And (VG.Modal = 'p') And (Not VG.ModalCond)
        Then
      Inv := True
    Else If (Not Subj.IsPron) And (Wh <> '') And (Inf0 = 'être') And Not VG.Neg
        And
            (Nonfin = '') Then
      Inv := False
    Else
      EstCe := True;
  End;

  If WhSubject And (Wh <> '') Then Wh := '';

  If Inv Then
  Begin
    PronInv := Subj.Pron;
    If Subj.IsThere Then PronInv := 'il';
    If Subj.IsI Then
    Begin
      FinInv := Fin;
      If (VG.Modal = 'p') And (Fin = 'peux') Then FinInv := 'puis';
      FinInv := FinInv + '-je';
    End
    Else
    Begin
      FinInv := Fin;
      If (PronInv = 'il') Or (PronInv = 'elle') Or (PronInv = 'on') Then
        If EndsWith(Fin, 'a') Or EndsWith(Fin, 'e') Then FinInv := FinInv +
            '-t';
      FinInv := FinInv + '-' + PronInv;
    End;
    If Subj.IsThere Then
    Begin
      If VG.Neg Then Res := Join('n''y', Join(FinInv, Pos2))
      Else Res := Join('y', FinInv);
      FinInv := '';
    End
    Else
    Begin
      Res := '';
      If Wh <> '' Then Res := Wh;
      If WhW = 'what' Then
        If Wh = 'que' Then Res := 'que';
      If Pre <> '' Then Res := Join(Res, Pre);
      Res := Join(Res, Join(ClitBeforeFin, FinInv));
      Res := Join(Res, Pos2);
    End;
    Res := Join(Res, VG.Adv);
    Res := Join(Res, Join(ClitBeforeInf, Nonfin));
    Res := Join(Res, Tail);
  End
  Else
  Begin
    Res := '';
    If EstCe Then
    Begin
      If (Wh = 'que') Then Res := 'qu''est-ce que'
      Else If Wh <> '' Then Res := Wh + ' est-ce que'
      Else Res := 'est-ce que';
    End
    Else If Wh <> '' Then
    Begin
      If (Wh = 'que') And (Inf0 = 'être') Then
      Begin
        If Subj.IsIt Or Subj.IsDemo Then
        Begin
          Res := 'qu''est-ce que';
        End
        Else
          Res := QuelFr(Subj.Gen, Subj.Plu);
      End
      Else
        Res := Wh;
    End;
    If (Wh <> '') And (Not EstCe) And (Inf0 = 'être') And (Not Subj.IsPron) And
       (Subj.Text <> 'ce') Then
    Begin
      { Ou est la bibliotheque ? }
      Res := Join(Res, Join(Fin, Subj.Text));
      Res := Join(Res, Tail);
    End
    Else If (Subj.Text = 'ce') And (Wh = 'que') And Not EstCe Then
    Begin
      Res := 'qu''est-ce que c''est';
      Res := Join(Res, Tail);
    End
    Else
    Begin
      Res := Join(Res, Subj.Text);
      If Pre <> '' Then Res := Join(Res, Pre);
      If Subj.IsThere Then Res := Join(Res, 'y')
      Else If ClitBeforeFin <> '' Then Res := Join(Res, ClitBeforeFin);
      Res := Join(Res, Fin);
      Res := Join(Res, Pos2);
      Res := Join(Res, VG.Adv);
      Res := Join(Res, Join(ClitBeforeInf, Nonfin));
      Res := Join(Res, Tail);
    End;
  End;
  If Please Then Res := 'S''il vous plaît, ' + Res;
  TranslateClause := Res;
End;

{ ------------------------------------------------------------------ }
{ ALLEMAND : verbes                                                   }
{ ------------------------------------------------------------------ }

Const
  { inf|du|er|participe|preterit (3e personne) }
  DeIrregularData: Array[0..48] Of String = (
    'essen|isst|isst|gegessen|aß',
    'sehen|siehst|sieht|gesehen|sah',
    'lesen|liest|liest|gelesen|las',
    'geben|gibst|gibt|gegeben|gab',
    'nehmen|nimmst|nimmt|genommen|nahm',
    'sprechen|sprichst|spricht|gesprochen|sprach',
    'helfen|hilfst|hilft|geholfen|half',
    'fahren|fährst|fährt|gefahren|fuhr',
    'schlafen|schläfst|schläft|geschlafen|schlief',
    'laufen|läufst|läuft|gelaufen|lief',
    'treffen|triffst|trifft|getroffen|traf',
    'vergessen|vergisst|vergisst|vergessen|vergaß',
    'gehen|gehst|geht|gegangen|ging',
    'kommen|kommst|kommt|gekommen|kam',
    'finden|findest|findet|gefunden|fand',
    'trinken|trinkst|trinkt|getrunken|trank',
    'schreiben|schreibst|schreibt|geschrieben|schrieb',
    'singen|singst|singt|gesungen|sang',
    'schwimmen|schwimmst|schwimmt|geschwommen|schwamm',
    'bleiben|bleibst|bleibt|geblieben|blieb',
    'bringen|bringst|bringt|gebracht|brachte',
    'denken|denkst|denkt|gedacht|dachte',
    'verstehen|verstehst|versteht|verstanden|verstand',
    'beginnen|beginnst|beginnt|begonnen|begann',
    'gewinnen|gewinnst|gewinnt|gewonnen|gewann',
    'verlieren|verlierst|verliert|verloren|verlor',
    'schließen|schließt|schließt|geschlossen|schloss',
    'tragen|trägst|trägt|getragen|trug',
    'waschen|wäschst|wäscht|gewaschen|wusch',
    'schneiden|schneidest|schneidet|geschnitten|schnitt',
    'sterben|stirbst|stirbt|gestorben|starb',
    'bekommen|bekommst|bekommt|bekommen|bekam',
    'erhalten|erhältst|erhält|erhalten|erhielt',
    'verlassen|verlässt|verlässt|verlassen|verließ',
    'fallen|fällst|fällt|gefallen|fiel',
    'lassen|lässt|lässt|gelassen|ließ',
    'brechen|brichst|bricht|gebrochen|brach',
    'rufen|rufst|ruft|gerufen|rief',
    'fangen|fängst|fängt|gefangen|fing',
    'betreten|betrittst|betritt|betreten|betrat',
    'entscheiden|entscheidest|entscheidet|entschieden|entschied',
    'kennen|kennst|kennt|gekannt|kannte',
    'nennen|nennst|nennt|genannt|nannte',
    'sitzen|sitzt|sitzt|gesessen|saß',
    'stehen|stehst|steht|gestanden|stand',
    'liegen|liegst|liegt|gelegen|lag',
    'heißen|heißt|heißt|geheißen|hieß',
    'werfen|wirfst|wirft|geworfen|warf',
    'ziehen|ziehst|zieht|gezogen|zog'
  );

Type
  TDeIrr = Record
    Inf, Du, Er, Part, Pret: String[20];
  End;

Var
  DeIrr: Array[1..64] Of TDeIrr;
  DeIrrCount: Integer;

Procedure LoadGermanVerbs;
Var
  I: Integer;
  Line: String;
Begin
  DeIrrCount := 0;
  For I := 0 To High(DeIrregularData) Do
    If DeIrrCount < 64 Then
    Begin
      Line := DeIrregularData[I];
      Inc(DeIrrCount);
      DeIrr[DeIrrCount].Inf := CopyUntil(Line, '|');
      DeIrr[DeIrrCount].Du := CopyUntil(Line, '|');
      DeIrr[DeIrrCount].Er := CopyUntil(Line, '|');
      DeIrr[DeIrrCount].Part := CopyUntil(Line, '|');
      DeIrr[DeIrrCount].Pret := Line;
    End;
End;

Function FindDeIrr(Const Inf: String): Integer;
Var
  I: Integer;
Begin
  For I := 1 To DeIrrCount Do
    If DeIrr[I].Inf = Inf Then
    Begin
      FindDeIrr := I;
      Exit;
    End;
  FindDeIrr := 0;
End;

Function DeStem(Const Inf: String): String;
Begin
  If EndsWith(Inf, 'eln') Or EndsWith(Inf, 'ern') Then
    DeStem := Copy(Inf, 1, Length(Inf) - 1)
  Else If EndsWith(Inf, 'en') Then
    DeStem := Copy(Inf, 1, Length(Inf) - 2)
  Else
    DeStem := Copy(Inf, 1, Length(Inf) - 1);
End;

Function NeedsE(Const Stem: String): Boolean;
Var
  L: Integer;
  C, P: Char;
Begin
  NeedsE := False;
  L := Length(Stem);
  If L < 2 Then Exit;
  C := Stem[L];
  P := Stem[L - 1];
  If (C = 't') Or (C = 'd') Then
    NeedsE := True
  Else If ((C = 'm') Or (C = 'n')) And Not (P In ['a', 'e', 'i', 'o', 'u', 'l',
      'r', 'h', 'm', 'n']) Then
    NeedsE := True;
End;

Function DeFullInf(L: Integer): String;
Begin
  DeFullInf := Lex[L].DeSep + Lex[L].De;
End;

{ Verbes de modalite et auxiliaires : present (tense p), preterit (i),
  subjonctif II (c). }
Function DeSpecialForm(Const Inf: String; Slot: Integer; Tense: Char): String;
Const
  Sein: Array[1..6] Of String[6] = ('bin', 'bist', 'ist', 'sind', 'seid',
      'sind');
  SeinP: Array[1..6] Of String[6] = ('war', 'warst', 'war', 'waren', 'wart',
      'waren');
  Haben: Array[1..6] Of String[6] = ('habe', 'hast', 'hat', 'haben', 'habt',
      'haben');
  HabenP: Array[1..6] Of String[8] = ('hatte', 'hattest', 'hatte', 'hatten',
      'hattet', 'hatten');
  Werden: Array[1..6] Of String[8] = ('werde', 'wirst', 'wird', 'werden',
      'werdet', 'werden');
  WerdenK: Array[1..6] Of String[8] = ('würde', 'würdest', 'würde',
      'würden', 'würdet', 'würden');
  Endings: Array[1..6] Of String[3] = ('e', 'est', 'e', 'en', 'et', 'en');
  Endings2: Array[1..6] Of String[3] = ('', 'st', '', 'en', 't', 'en');
Var
  Stem: String;
Begin
  DeSpecialForm := '';
  If Inf = 'sein' Then
  Begin
    If Tense = 'p' Then DeSpecialForm := Sein[Slot] Else If Tense = 'c' Then
        DeSpecialForm := 'wäre' + Copy(Endings[Slot], 2, 3) Else DeSpecialForm
        := SeinP[Slot];
    If (Tense = 'c') And (Slot = 2) Then DeSpecialForm := 'wärst';
    If (Tense = 'c') And (Slot = 4) Then DeSpecialForm := 'wären';
    If (Tense = 'c') And (Slot = 5) Then DeSpecialForm := 'wärt';
    If (Tense = 'c') And (Slot = 6) Then DeSpecialForm := 'wären';
    If (Tense = 'c') And (Slot In [1, 3]) Then DeSpecialForm := 'wäre';
  End
  Else If Inf = 'haben' Then
  Begin
    If Tense = 'p' Then DeSpecialForm := Haben[Slot] Else DeSpecialForm :=
        HabenP[Slot];
    If Tense = 'c' Then DeSpecialForm := 'hätte' + Copy(Endings[Slot], 2, 3);
    If (Tense = 'c') And (Slot In [1, 3]) Then DeSpecialForm := 'hätte';
    If (Tense = 'c') And (Slot = 2) Then DeSpecialForm := 'hättest';
    If (Tense = 'c') And (Slot In [4, 6]) Then DeSpecialForm := 'hätten';
    If (Tense = 'c') And (Slot = 5) Then DeSpecialForm := 'hättet';
  End
  Else If Inf = 'werden' Then
  Begin
    If Tense = 'c' Then
      DeSpecialForm := WerdenK[Slot]
    Else If Tense = 'p' Then
      DeSpecialForm := Werden[Slot]
    Else If Slot In [1, 3] Then
      DeSpecialForm := 'wurde'
    Else
      DeSpecialForm := 'wurde' + Endings2[Slot];
  End
  Else If Inf = 'wissen' Then
  Begin
    Case Slot Of
      1, 3: DeSpecialForm := 'weiß';
      2: DeSpecialForm := 'weißt';
      4, 6: DeSpecialForm := 'wissen';
      5: DeSpecialForm := 'wisst';
    End;
    If Tense <> 'p' Then
    Begin
      Stem := 'wusste';
      If Slot = 2 Then DeSpecialForm := 'wusstest'
      Else If Slot In [4, 6] Then DeSpecialForm := 'wussten'
      Else If Slot = 5 Then DeSpecialForm := 'wusstet'
      Else DeSpecialForm := Stem;
    End;
  End
  Else If InList(Inf, 'können müssen wollen sollen dürfen mögen möchten')
      Then
  Begin
    If Inf = 'können' Then Stem := 'kann'
    Else If Inf = 'müssen' Then Stem := 'muss'
    Else If Inf = 'wollen' Then Stem := 'will'
    Else If Inf = 'sollen' Then Stem := 'soll'
    Else If Inf = 'dürfen' Then Stem := 'darf'
    Else If Inf = 'mögen' Then Stem := 'mag'
    Else Stem := 'möchte';
    If Tense = 'p' Then
    Begin
      If Inf = 'möchten' Then
      Begin
        Case Slot Of
          2: DeSpecialForm := 'möchtest';
          4, 6: DeSpecialForm := 'möchten';
          5: DeSpecialForm := 'möchtet';
        Else
          DeSpecialForm := 'möchte';
        End;
      End
      Else
      Begin
        Case Slot Of
          1, 3: DeSpecialForm := Stem;
          2:
            If Stem = 'darf' Then DeSpecialForm := 'darfst'
            Else If Stem = 'kann' Then DeSpecialForm := 'kannst'
            Else If Stem = 'will' Then DeSpecialForm := 'willst'
            Else If Stem = 'muss' Then DeSpecialForm := 'musst'
            Else If Stem = 'soll' Then DeSpecialForm := 'sollst'
            Else DeSpecialForm := 'magst';
          4, 6: DeSpecialForm := Inf;
          5:
            If Inf = 'dürfen' Then DeSpecialForm := 'dürft'
            Else If Inf = 'können' Then DeSpecialForm := 'könnt'
            Else If Inf = 'müssen' Then DeSpecialForm := 'müsst'
            Else If Inf = 'wollen' Then DeSpecialForm := 'wollt'
            Else If Inf = 'sollen' Then DeSpecialForm := 'sollt'
            Else DeSpecialForm := 'mögt';
        End;
      End;
    End
    Else
    Begin
      { preterit / subjonctif II }
      If Inf = 'können' Then Stem := 'konnte'
      Else If Inf = 'müssen' Then Stem := 'musste'
      Else If Inf = 'wollen' Then Stem := 'wollte'
      Else If Inf = 'sollen' Then Stem := 'sollte'
      Else If Inf = 'dürfen' Then Stem := 'durfte'
      Else If Inf = 'mögen' Then Stem := 'mochte'
      Else Stem := 'möchte';
      If Tense = 'c' Then
      Begin
        If Inf = 'können' Then Stem := 'könnte';
        If Inf = 'müssen' Then Stem := 'müsste';
        If Inf = 'dürfen' Then Stem := 'dürfte';
      End;
      Case Slot Of
        2: DeSpecialForm := Stem + 'st';
        4, 6: DeSpecialForm := Stem + 'n';
        5: DeSpecialForm := Stem + 't';
      Else
        DeSpecialForm := Stem;
      End;
    End;
  End;
End;

Function IsDeSpecial(Const Inf: String): Boolean;
Begin
  IsDeSpecial := InList(Inf,
      'sein haben werden wissen können müssen wollen sollen dürfen mögen m'+
      'öchten');
End;

{ Forme conjuguee (sans prefixe separable). Tense : p present, i preterit, c
  subjonctif II. }
Function DeFinite(L: Integer; Slot: Integer; Tense: Char): String;
Var
  Inf, Stem, Form: String;
  Irr: Integer;
Begin
  Inf := Lex[L].De;
  If IsDeSpecial(Inf) Then
  Begin
    DeFinite := DeSpecialForm(Inf, Slot, Tense);
    Exit;
  End;
  Irr := FindDeIrr(Inf);
  Stem := DeStem(Inf);
  If Tense <> 'p' Then
  Begin
    If Irr > 0 Then Form := DeIrr[Irr].Pret Else Form := Stem + 'te';
    Case Slot Of
      2: Form := Form + 'st';
      4, 6: Form := Form + 'n';
      5: Form := Form + 't';
    End;
    DeFinite := Form;
    Exit;
  End;
  Case Slot Of
    1:
      If EndsWith(Inf, 'eln') Then DeFinite := Copy(Stem, 1, Length(Stem) - 2) +
          'le'
      Else DeFinite := Stem + 'e';
    2:
      If Irr > 0 Then DeFinite := DeIrr[Irr].Du
      Else If NeedsE(Stem) Then DeFinite := Stem + 'est'
      Else If EndsWith(Stem, 's') Or EndsWith(Stem, 'ß') Or EndsWith(Stem, 'x')
          Or EndsWith(Stem, 'z') Then
        DeFinite := Stem + 't'
      Else DeFinite := Stem + 'st';
    3:
      If Irr > 0 Then DeFinite := DeIrr[Irr].Er
      Else If NeedsE(Stem) Then DeFinite := Stem + 'et'
      Else DeFinite := Stem + 't';
    4, 6: DeFinite := Inf;
    5:
      If NeedsE(Stem) Then DeFinite := Stem + 'et' Else DeFinite := Stem + 't';
  End;
End;

Function HasInseparablePrefix(Const Inf: String): Boolean;
Begin
  HasInseparablePrefix := (Length(Inf) > 6) And (StartsWith(Inf, 'be') Or
      StartsWith(Inf, 'ver') Or
                          StartsWith(Inf, 'er') Or StartsWith(Inf, 'ent') Or
                              StartsWith(Inf, 'ge') Or
                          StartsWith(Inf, 'zer') Or StartsWith(Inf, 'miss'));
End;

Function DeParticiple(L: Integer): String;
Var
  Inf, Stem, Base: String;
  Irr: Integer;
Begin
  Inf := Lex[L].De;
  If Inf = 'sein' Then Base := 'gewesen'
  Else If Inf = 'haben' Then Base := 'gehabt'
  Else If Inf = 'werden' Then Base := 'geworden'
  Else If Inf = 'wissen' Then Base := 'gewusst'
  Else If Inf = 'können' Then Base := 'gekonnt'
  Else If Inf = 'müssen' Then Base := 'gemusst'
  Else If Inf = 'wollen' Then Base := 'gewollt'
  Else If Inf = 'sollen' Then Base := 'gesollt'
  Else If Inf = 'dürfen' Then Base := 'gedurft'
  Else If Inf = 'mögen' Then Base := 'gemocht'
  Else
  Begin
    Irr := FindDeIrr(Inf);
    If Irr > 0 Then
      Base := DeIrr[Irr].Part
    Else
    Begin
      Stem := DeStem(Inf);
      If NeedsE(Stem) Then Stem := Stem + 'e';
      If EndsWith(Inf, 'ieren') Or HasInseparablePrefix(Inf) Then
        Base := Stem + 't'
      Else
        Base := 'ge' + Stem + 't';
    End;
  End;
  DeParticiple := Lex[L].DeSep + Base;
End;

Function DeUsesSein(L: Integer): Boolean;
Begin
  DeUsesSein := Pos('s', Lex[L].DeFlags) > 0;
End;

{ ------------------------------------------------------------------ }
{ ALLEMAND : articles, noms et adjectifs                              }
{ ------------------------------------------------------------------ }

Function CaseIndex(C: Char): Integer;
Begin
  If C = 'a' Then CaseIndex := 2 Else If C = 'd' Then CaseIndex := 3 Else
      CaseIndex := 1;
End;

Function ArtDef(G: Char; Plural: Boolean; C: Char): String;
Begin
  If Plural Then
  Begin
    If C = 'd' Then ArtDef := 'den' Else ArtDef := 'die';
  End
  Else
    Case G Of
      'm':
        Case C Of
          'a': ArtDef := 'den';
          'd': ArtDef := 'dem';
        Else
          ArtDef := 'der';
        End;
      'f':
        If C = 'd' Then ArtDef := 'der' Else ArtDef := 'die';
    Else
      If C = 'd' Then ArtDef := 'dem' Else ArtDef := 'das';
    End;
End;

{ Terminaison de ein-/kein-/mein- : m f n pluriel selon le cas. }
Function EinEnding(G: Char; Plural: Boolean; C: Char): String;
Begin
  If Plural Then
  Begin
    If C = 'd' Then EinEnding := 'en' Else EinEnding := 'e';
  End
  Else
    Case G Of
      'm':
        Case C Of
          'a': EinEnding := 'en';
          'd': EinEnding := 'em';
        Else
          EinEnding := '';
        End;
      'f':
        If C = 'd' Then EinEnding := 'er' Else EinEnding := 'e';
    Else
      If C = 'd' Then EinEnding := 'em' Else EinEnding := '';
    End;
End;

{ Terminaison de dies-/jed-/welch- (comme l'article defini). }
Function DerEnding(G: Char; Plural: Boolean; C: Char): String;
Begin
  If Plural Then
  Begin
    If C = 'd' Then DerEnding := 'en' Else DerEnding := 'e';
  End
  Else
    Case G Of
      'm':
        Case C Of
          'a': DerEnding := 'en';
          'd': DerEnding := 'em';
        Else
          DerEnding := 'er';
        End;
      'f':
        If C = 'd' Then DerEnding := 'er' Else DerEnding := 'e';
    Else
      If C = 'd' Then DerEnding := 'em' Else DerEnding := 'es';
    End;
End;

{ Terminaison de l'adjectif : K = d (apres article defini), m (apres ein-), s
  (sans article). }
Function AdjEnding(K: Char; G: Char; Plural: Boolean; C: Char): String;
Begin
  If K = 'd' Then
  Begin
    If Plural Or (C = 'd') Then
      AdjEnding := 'en'
    Else If (G = 'm') And (C = 'a') Then
      AdjEnding := 'en'
    Else
      AdjEnding := 'e';
  End
  Else If K = 'm' Then
  Begin
    If Plural Or (C = 'd') Then
      AdjEnding := 'en'
    Else If G = 'm' Then
    Begin
      If C = 'a' Then AdjEnding := 'en' Else AdjEnding := 'er';
    End
    Else If G = 'f' Then
      AdjEnding := 'e'
    Else
      AdjEnding := 'es';
  End
  Else
    AdjEnding := DerEnding(G, Plural, C);
End;

Function AdjectiveDe(L: Integer; K: Char; G: Char; Plural: Boolean; C: Char):
    String;
Var
  Stem: String;
Begin
  If Pos('i', Lex[L].DeFlags) > 0 Then
  Begin
    AdjectiveDe := Lex[L].De;
    Exit;
  End;
  If Lex[L].DePl <> '' Then
    Stem := Lex[L].DePl
  Else
  Begin
    Stem := Lex[L].De;
    If EndsWith(Stem, 'el') And (Length(Stem) > 3) Then Stem := Copy(Stem, 1,
        Length(Stem) - 2) + 'l';
  End;
  AdjectiveDe := Stem + AdjEnding(K, G, Plural, C);
End;

{ Nom allemand : pluriel, datif pluriel, declinaison faible des masculins en -n.
  }
Function NounDe(L: Integer; Plural: Boolean; C: Char): String;
Var
  S: String;
Begin
  S := Lex[L].De;
  If Plural Then
  Begin
    If (Lex[L].DePl <> '') And (Lex[L].DePl <> '-') Then S := Lex[L].DePl;
    If (C = 'd') And Not (EndsWith(S, 'n') Or EndsWith(S, 's')) Then S := S +
        'n';
  End
  Else If (Pos('w', Lex[L].DeFlags) > 0) And (C <> 'n') Then
  Begin
    If EndsWith(S, 'e') Then S := S + 'n' Else S := S + 'en';
  End;
  NounDe := S;
End;

Function DeNumber(Const W: String): String;
Begin
  DeNumber := '';
  If W = 'one' Then DeNumber := 'eins'
  Else If W = 'two' Then DeNumber := 'zwei'
  Else If W = 'three' Then DeNumber := 'drei'
  Else If W = 'four' Then DeNumber := 'vier'
  Else If W = 'five' Then DeNumber := 'fünf'
  Else If W = 'six' Then DeNumber := 'sechs'
  Else If W = 'seven' Then DeNumber := 'sieben'
  Else If W = 'eight' Then DeNumber := 'acht'
  Else If W = 'nine' Then DeNumber := 'neun'
  Else If W = 'ten' Then DeNumber := 'zehn'
  Else If W = 'eleven' Then DeNumber := 'elf'
  Else If W = 'twelve' Then DeNumber := 'zwölf'
  Else If W = 'thirteen' Then DeNumber := 'dreizehn'
  Else If W = 'fourteen' Then DeNumber := 'vierzehn'
  Else If W = 'fifteen' Then DeNumber := 'fünfzehn'
  Else If W = 'sixteen' Then DeNumber := 'sechzehn'
  Else If W = 'seventeen' Then DeNumber := 'siebzehn'
  Else If W = 'eighteen' Then DeNumber := 'achtzehn'
  Else If W = 'nineteen' Then DeNumber := 'neunzehn'
  Else If W = 'twenty' Then DeNumber := 'zwanzig'
  Else If W = 'thirty' Then DeNumber := 'dreißig'
  Else If W = 'forty' Then DeNumber := 'vierzig'
  Else If W = 'fifty' Then DeNumber := 'fünfzig'
  Else If W = 'sixty' Then DeNumber := 'sechzig'
  Else If W = 'seventy' Then DeNumber := 'siebzig'
  Else If W = 'eighty' Then DeNumber := 'achtzig'
  Else If W = 'ninety' Then DeNumber := 'neunzig'
  Else If W = 'hundred' Then DeNumber := 'hundert'
  Else If W = 'thousand' Then DeNumber := 'tausend'
  Else If W = 'million' Then DeNumber := 'Million';
End;

Function ProperDe(Const W: String): String;
Begin
  ProperDe := '';
  If W = 'germany' Then ProperDe := 'Deutschland'
  Else If W = 'france' Then ProperDe := 'Frankreich'
  Else If W = 'spain' Then ProperDe := 'Spanien'
  Else If W = 'italy' Then ProperDe := 'Italien'
  Else If W = 'england' Then ProperDe := 'England'
  Else If W = 'canada' Then ProperDe := 'Kanada'
  Else If W = 'switzerland' Then ProperDe := 'Schweiz'
  Else If W = 'austria' Then ProperDe := 'Österreich'
  Else If W = 'belgium' Then ProperDe := 'Belgien'
  Else If W = 'portugal' Then ProperDe := 'Portugal'
  Else If W = 'china' Then ProperDe := 'China'
  Else If W = 'japan' Then ProperDe := 'Japan'
  Else If W = 'russia' Then ProperDe := 'Russland'
  Else If W = 'brazil' Then ProperDe := 'Brasilien'
  Else If W = 'mexico' Then ProperDe := 'Mexiko'
  Else If W = 'india' Then ProperDe := 'Indien'
  Else If W = 'america' Then ProperDe := 'Amerika'
  Else If W = 'africa' Then ProperDe := 'Afrika'
  Else If W = 'europe' Then ProperDe := 'Europa'
  Else If W = 'asia' Then ProperDe := 'Asien'
  Else If W = 'australia' Then ProperDe := 'Australien'
  Else If W = 'ireland' Then ProperDe := 'Irland'
  Else If W = 'scotland' Then ProperDe := 'Schottland'
  Else If W = 'sweden' Then ProperDe := 'Schweden'
  Else If W = 'norway' Then ProperDe := 'Norwegen'
  Else If W = 'denmark' Then ProperDe := 'Dänemark'
  Else If W = 'poland' Then ProperDe := 'Polen'
  Else If W = 'greece' Then ProperDe := 'Griechenland'
  Else If W = 'turkey' Then ProperDe := 'Türkei'
  Else If W = 'egypt' Then ProperDe := 'Ägypten'
  Else If W = 'morocco' Then ProperDe := 'Marokko'
  Else If W = 'london' Then ProperDe := 'London'
  Else If W = 'brussels' Then ProperDe := 'Brüssel'
  Else If W = 'geneva' Then ProperDe := 'Genf'
  Else If W = 'moscow' Then ProperDe := 'Moskau'
  Else If W = 'vienna' Then ProperDe := 'Wien'
  Else If W = 'lisbon' Then ProperDe := 'Lissabon'
  Else If W = 'athens' Then ProperDe := 'Athen'
  Else If W = 'cairo' Then ProperDe := 'Kairo'
  Else If W = 'rome' Then ProperDe := 'Rom'
  Else If W = 'munich' Then ProperDe := 'München'
  Else If W = 'cologne' Then ProperDe := 'Köln'
  Else If W = 'prague' Then ProperDe := 'Prag';
End;

{ Pays avec article en allemand : die Schweiz, die Türkei, die USA. }
Function DeCountryArticle(Const W: String): Boolean;
Begin
  DeCountryArticle := (W = 'switzerland') Or (W = 'turkey') Or (W = 'usa') Or (W
      = 'states');
End;

Function IsDeCountry(Const W: String): Boolean;
Begin
  IsDeCountry := InList(W,
      'germany france spain italy england canada switzerland austria belgium p'+
      'ortugal china japan russia brazil mexico india america africa europe as'+
      'ia australia ireland scotland sweden norway denmark poland greece turke'+
      'y egypt morocco usa');
End;

{ ------------------------------------------------------------------ }
{ ALLEMAND : groupe nominal                                           }
{ ------------------------------------------------------------------ }

Type
  TDeNP = Record
    Found, Proper, Plural, Neg, Pron: Boolean;
    Used: Integer;
    Kind: Char; { d defini, i indefini, p possessif, k demonstratif, q
        quantificateur, n nombre, 0 aucun }
    DetWord: String[12];
    NumText: String[16];
    Adjs: Array[1..6] Of Integer;
    AdjAdv: Array[1..6] Of String[12];
    AdjCount: Integer;
    Head, Modif: Integer;
    PText: String[40];
    PCountry: Boolean;
    PronW: String[8];
    Gen: Char;
    AllFlag: Boolean;
  End;

Function LowerFirst(Const S: String): String;
Var
  R: String;
Begin
  R := S;
  If (R <> '') And (R[1] >= 'A') And (R[1] <= 'Z') Then R[1] := Chr(Ord(R[1]) +
      32);
  LowerFirst := R;
End;

Function IsMassDe(Const En: String): Boolean;
Begin
  IsMassDe := IsMassNoun(En);
End;

Function HeadEn(Const NP: TDeNP): String;
Begin
  If NP.Head > 0 Then HeadEn := Lex[NP.Head].En Else HeadEn := '';
End;

Function PossStemDe(Const W: String): String;
Begin
  If W = 'my' Then PossStemDe := 'mein'
  Else If W = 'your' Then PossStemDe := 'Ihr'
  Else If (W = 'his') Or (W = 'its') Then PossStemDe := 'sein'
  Else If (W = 'her') Or (W = 'their') Then PossStemDe := 'ihr'
  Else PossStemDe := 'unser';
End;

Function ParseNPDe(Start: Integer; Mode: Char; Var NP: TDeNP): Boolean;
Var
  I, K, NounCount: Integer;
  Nouns: Array[1..3] Of Integer;
  NounPlural: Boolean;
Begin
  FillChar(NP, SizeOf(NP), 0);
  NP.Kind := '0';
  NP.Gen := 'm';
  I := Start;
  NounCount := 0;
  NounPlural := False;
  ParseNPDe := False;
  If I > TN Then Exit;

  If (Tk[I].Tag = 'Q') And (Tk[I].W = 'all') Then
  Begin
    NP.AllFlag := True;
    Inc(I);
  End;
  If I <= TN Then
    Case Tk[I].Tag Of
      'D':
      Begin
        If Tk[I].W = 'the' Then NP.Kind := 'd' Else NP.Kind := 'i';
        Inc(I);
      End;
      'S':
      Begin
        NP.Kind := 'p';
        NP.DetWord := Tk[I].W;
        Inc(I);
      End;
      'T':
        If (I < TN) And (Tk[I + 1].Tag In ['N', 'A', 'U']) Then
        Begin
          NP.Kind := 'k';
          NP.DetWord := Tk[I].W;
          Inc(I);
        End;
      'Q':
      Begin
        NP.Kind := 'q';
        NP.DetWord := Tk[I].W;
        Inc(I);
      End;
      'U':
      Begin
        NP.Kind := 'n';
        If DeNumber(Tk[I].W) <> '' Then NP.NumText := DeNumber(Tk[I].W) Else
            NP.NumText := Tk[I].W;
        If Tk[I].W = 'one' Then NP.NumText := 'ein';
        NP.DetWord := Tk[I].W;
        Inc(I);
      End;
    End;
  If NP.AllFlag And (I <= TN) And (Tk[I].Tag = 'D') Then
  Begin
    NP.Kind := 'q';
    NP.DetWord := 'all';
    Inc(I);
  End;
  If NP.AllFlag And (NP.Kind = '0') Then
  Begin
    NP.Kind := 'q';
    NP.DetWord := 'all';
  End;

  While I <= TN Do
  Begin
    If Tk[I].Tag = 'A' Then
    Begin
      If NP.AdjCount < 6 Then
      Begin
        Inc(NP.AdjCount);
        NP.Adjs[NP.AdjCount] := Tk[I].A;
        NP.AdjAdv[NP.AdjCount] := '';
      End;
      Inc(I);
    End
    Else If (Tk[I].Tag = 'R') And (I < TN) And (Tk[I + 1].Tag = 'A') Then
    Begin
      If NP.AdjCount < 6 Then
      Begin
        Inc(NP.AdjCount);
        NP.Adjs[NP.AdjCount] := Tk[I + 1].A;
        If Tk[I].R > 0 Then NP.AdjAdv[NP.AdjCount] := Lex[Tk[I].R].De Else
            NP.AdjAdv[NP.AdjCount] := '';
      End;
      Inc(I, 2);
    End
    Else If (Tk[I].Tag = 'C') And (Tk[I].W = 'and') And (NP.AdjCount > 0) And (I
        < TN) And
            (Tk[I + 1].Tag = 'A') Then
      Inc(I)
    Else
      Break;
  End;

  While (I <= TN) And (Tk[I].Tag = 'N') And (NounCount < 3) Do
  Begin
    Inc(NounCount);
    Nouns[NounCount] := Tk[I].N;
    NounPlural := Tk[I].NPlural;
    Inc(I);
  End;
  If (NounCount = 0) And (I <= TN) And (Tk[I].Tag = 'Z') Then
  Begin
    NP.Proper := True;
    While (I <= TN) And (Tk[I].Tag = 'Z') Do
    Begin
      If ProperDe(Tk[I].W) <> '' Then
      Begin
        NP.PText := Join(NP.PText, ProperDe(Tk[I].W));
        NP.PCountry := IsDeCountry(Tk[I].W);
        If DeCountryArticle(Tk[I].W) Then NP.Gen := 'f';
      End
      Else
        NP.PText := Join(NP.PText, Tk[I].Orig);
      Inc(I);
    End;
    NP.Found := True;
    NP.Used := I - Start;
    ParseNPDe := True;
    Exit;
  End;
  If NounCount = 0 Then
  Begin
    If (NP.Kind = 'n') And (NP.AdjCount = 0) Then
    Begin
      NP.Found := True;
      NP.Used := I - Start;
      NP.Plural := True;
      ParseNPDe := True;
    End;
    Exit;
  End;

  { possessif 's : "John's book" -> "das Buch von John" }
  If (I <= TN) And (Tk[I].Tag = 'F') Then
  Begin
    NP.Found := True;
    NP.Head := Nouns[NounCount];
    NP.Plural := NounPlural;
    NP.Used := I - Start;
    ParseNPDe := True;
    Exit;
  End;

  NP.Head := Nouns[NounCount];
  If NounCount > 1 Then NP.Modif := Nouns[NounCount - 1];
  NP.Plural := NounPlural;
  If Lex[NP.Head].DeG = 'P' Then NP.Plural := True;
  If (NP.Kind = 'n') And (NP.DetWord <> 'one') Then NP.Plural := True;
  If (NP.Kind = 'q') And InList(NP.DetWord,
      'many few several both these those some any') Then
    If (NP.DetWord <> 'some') And (NP.DetWord <> 'any') Then NP.Plural := True;
  NP.Gen := Lex[NP.Head].DeG;
  If (NP.Gen <> 'm') And (NP.Gen <> 'f') And (NP.Gen <> 'n') Then NP.Gen := 'f';
  NP.Found := True;
  NP.Used := I - Start;
  K := 0;
  ParseNPDe := True;
End;

{ Genre et nombre du groupe nominal (le nom avec son composant). }
Function RenderNPDe(Const NP: TDeNP; C: Char; ForceDef: Boolean): String;
Var
  Gen: Char;
  Plu: Boolean;
  Art, NounText, Adjs, Kind, Det: String;
  K: Integer;
  Adj: String;
  Decl: Char;
  Kd: Char;
Begin
  If NP.Pron Then
  Begin
    RenderNPDe := NP.PronW;
    Exit;
  End;
  If NP.Proper Then
  Begin
    If NP.PCountry And (NP.Gen = 'f') Then
      RenderNPDe := ArtDef('f', False, C) + ' ' + NP.PText
    Else
      RenderNPDe := NP.PText;
    Exit;
  End;
  If NP.Head = 0 Then
  Begin
    RenderNPDe := NP.NumText;
    Exit;
  End;
  Gen := NP.Gen;
  Plu := NP.Plural;
  NounText := NounDe(NP.Head, Plu, C);
  If NP.Modif > 0 Then
    NounText := Lex[NP.Modif].De + LowerFirst(NounText);
  Kd := NP.Kind;
  If ForceDef And (Kd = '0') And Not Plu And Not IsMassDe(HeadEn(NP)) Then Kd :=
      'd';
  Art := '';
  Decl := 's';
  Case Kd Of
    'd':
    Begin
      Art := ArtDef(Gen, Plu, C);
      Decl := 'd';
    End;
    'i':
      If NP.Neg Then
      Begin
        Art := 'kein' + EinEnding(Gen, Plu, C);
        Decl := 'm';
      End
      Else If Not Plu Then
      Begin
        Art := 'ein' + EinEnding(Gen, False, C);
        Decl := 'm';
      End;
    'p':
    Begin
      Art := PossStemDe(NP.DetWord) + EinEnding(Gen, Plu, C);
      Decl := 'm';
    End;
    'k':
    Begin
      Art := 'dies' + DerEnding(Gen, Plu, C);
      Decl := 'd';
    End;
    'n':
    Begin
      Art := NP.NumText;
      Decl := 's';
    End;
    'q':
    Begin
      If InList(NP.DetWord, 'some any') Then
      Begin
        If NP.Neg Then
        Begin
          Art := 'kein' + EinEnding(Gen, Plu, C);
          Decl := 'm';
        End
        Else If Plu Then
          Art := 'einige';
      End
      Else If NP.DetWord = 'many' Then Art := 'viele'
      Else If NP.DetWord = 'much' Then Art := 'viel'
      Else If NP.DetWord = 'few' Then Art := 'wenige'
      Else If NP.DetWord = 'several' Then Art := 'mehrere'
      Else If InList(NP.DetWord, 'every each') Then
      Begin
        Art := 'jed' + DerEnding(Gen, False, C);
        Decl := 'd';
      End
      Else If NP.DetWord = 'all' Then
      Begin
        Art := 'alle';
        Decl := 'd';
        Plu := True;
        NounText := NounDe(NP.Head, True, C);
      End
      Else If NP.DetWord = 'no' Then
      Begin
        Art := 'kein' + EinEnding(Gen, Plu, C);
        Decl := 'm';
      End
      Else If NP.DetWord = 'another' Then
      Begin
        Art := 'noch ein' + EinEnding(Gen, False, C);
        Decl := 'm';
      End
      Else If NP.DetWord = 'more' Then Art := 'mehr'
      Else If NP.DetWord = 'enough' Then Art := 'genug';
    End;
  Else
    If NP.Neg Then
    Begin
      Art := 'kein' + EinEnding(Gen, Plu, C);
      Decl := 'm';
    End;
  End;
  Adjs := '';
  For K := 1 To NP.AdjCount Do
  Begin
    Adj := AdjectiveDe(NP.Adjs[K], Decl, Gen, Plu, C);
    If NP.AdjAdv[K] <> '' Then Adj := NP.AdjAdv[K] + ' ' + Adj;
    Adjs := Join(Adjs, Adj);
  End;
  RenderNPDe := Join(Art, Join(Adjs, NounText));
End;

{ ------------------------------------------------------------------ }
{ ALLEMAND : pronoms et sujet                                         }
{ ------------------------------------------------------------------ }

Function SubjDe(Const W: String): String;
Begin
  If W = 'i' Then SubjDe := 'ich'
  Else If W = 'you' Then SubjDe := 'Sie'
  Else If W = 'he' Then SubjDe := 'er'
  Else If (W = 'she') Or (W = 'they') Then SubjDe := 'sie'
  Else If W = 'it' Then SubjDe := 'es'
  Else If W = 'we' Then SubjDe := 'wir'
  Else SubjDe := W;
End;

Function PronounDe(Const W: String; C: Char): String;
Begin
  If C = 'd' Then
  Begin
    If (W = 'me') Or (W = 'i') Then PronounDe := 'mir'
    Else If W = 'you' Then PronounDe := 'Ihnen'
    Else If (W = 'him') Or (W = 'it') Or (W = 'he') Then PronounDe := 'ihm'
    Else If (W = 'her') Or (W = 'she') Then PronounDe := 'ihr'
    Else If (W = 'us') Or (W = 'we') Then PronounDe := 'uns'
    Else If (W = 'them') Or (W = 'they') Then PronounDe := 'ihnen'
    Else PronounDe := W;
  End
  Else
  Begin
    If (W = 'me') Or (W = 'i') Then PronounDe := 'mich'
    Else If W = 'you' Then PronounDe := 'Sie'
    Else If (W = 'him') Or (W = 'he') Then PronounDe := 'ihn'
    Else If (W = 'her') Or (W = 'she') Or (W = 'them') Or (W = 'they') Then
        PronounDe := 'sie'
    Else If W = 'it' Then PronounDe := 'es'
    Else If (W = 'us') Or (W = 'we') Then PronounDe := 'uns'
    Else PronounDe := W;
  End;
End;

Type
  TDeSubj = Record
    Found, IsPron, IsI, IsIt, IsThere, IsDemo, IsWh: Boolean;
    Slot: Integer;
    Text: String;
    Used: Integer;
    NP: TDeNP;
    PronW: String[8];
  End;

Var
  CurDeSubj: TDeSubj;

Function PronounSlotDe(Const W: String): Integer;
Begin
  If W = 'i' Then PronounSlotDe := 1
  Else If W = 'you' Then PronounSlotDe := 6
  Else If (W = 'he') Or (W = 'she') Or (W = 'it') Then PronounSlotDe := 3
  Else If W = 'we' Then PronounSlotDe := 4
  Else If W = 'they' Then PronounSlotDe := 6
  Else PronounSlotDe := 0;
End;

Function ParseSubjectDe(Start: Integer; Var S: TDeSubj): Boolean;
Var
  W: String;
Begin
  FillChar(S, SizeOf(S), 0);
  ParseSubjectDe := False;
  If Start > TN Then Exit;
  W := Tk[Start].W;
  If Tk[Start].Tag = 'P' Then
  Begin
    S.Found := True;
    S.IsPron := True;
    S.Slot := PronounSlotDe(W);
    S.Text := SubjDe(W);
    S.PronW := W;
    S.IsI := W = 'i';
    S.IsIt := W = 'it';
    S.Used := 1;
    ParseSubjectDe := True;
    Exit;
  End;
  If (W = 'there') And (Start < TN) And IsBeForm(Tk[Start + 1].W) Then
  Begin
    S.Found := True;
    S.IsThere := True;
    S.Slot := 3;
    S.Text := 'es';
    S.Used := 1;
    ParseSubjectDe := True;
    Exit;
  End;
  If (Tk[Start].Tag = 'T') And (Start < TN) And IsBeForm(Tk[Start + 1].W) Then
  Begin
    S.Found := True;
    S.IsDemo := True;
    S.IsIt := True;
    S.Slot := 3;
    If (W = 'these') Or (W = 'those') Then S.Slot := 6;
    S.Text := 'das';
    S.Used := 1;
    ParseSubjectDe := True;
    Exit;
  End;
  If Tk[Start].Tag = 'W' Then
  Begin
    S.Found := True;
    S.IsWh := True;
    S.Slot := 3;
    If W = 'who' Then S.Text := 'wer' Else S.Text := 'was';
    S.Used := 1;
    ParseSubjectDe := True;
    Exit;
  End;
  If ParseNPDe(Start, ' ', S.NP) Then
  Begin
    S.Found := True;
    S.Used := S.NP.Used;
    If S.NP.Plural Then S.Slot := 6 Else S.Slot := 3;
    S.Text := RenderNPDe(S.NP, 'n', False);
    ParseSubjectDe := True;
  End;
End;

Function AdvDe(I: Integer): String;
Begin
  If Tk[I].R > 0 Then
    AdvDe := Lex[Tk[I].R].De
  Else If Tk[I].AdvA > 0 Then
    AdvDe := Lex[Tk[I].AdvA].De
  Else
    AdvDe := Tk[I].Orig;
End;

Function IsMotionVerb(Const En: String): Boolean;
Begin
  IsMotionVerb := InList(En,
      'go come travel return put place bring carry take send throw fly');
End;

Function IsVehicle(Const En: String): Boolean;
Begin
  IsVehicle := InList(En, 'bus car train plane bike boat');
End;

Var
  DeCurVerb: String;
  DeFemSubj: Boolean;

Function FemProfession(Const S: String): String;
Begin
  If EndsWith(S, 'Arzt') Then FemProfession := Copy(S, 1, Length(S) - 4) +
      'Ärztin'
  Else If EndsWith(S, 'Koch') Then FemProfession := Copy(S, 1, Length(S) - 4) +
      'Köchin'
  Else If EndsWith(S, 'Freund') Then FemProfession := S + 'in'
  Else FemProfession := S + 'in';
End;

{ Groupe prepositionnel : renvoie le texte allemand. }
Function TranslatePPDe(Var I: Integer; Motion: Boolean): String;
Var
  W, PrepW, Name: String;
  J, K: Integer;
  C: Char;
  NP: TDeNP;
  PrepCase: Char;
  Res: String;
Begin
  W := Tk[I].W;
  If Tk[I].O > 0 Then
  Begin
    PrepW := Lex[Tk[I].O].De;
    PrepCase := Lex[Tk[I].O].DeCase;
  End
  Else
  Begin
    PrepW := Tk[I].Orig;
    PrepCase := 'd';
  End;
  If (W = 'to') And (DeCurVerb = 'be') And (I < TN) And (Tk[I + 1].Tag = 'Z')
      Then W := 'in';
  { verbes a preposition fixe }
  If (W = 'for') And (DeCurVerb = 'wait') Then Begin PrepW := 'auf'; PrepCase :=
      'a'; End
  Else If (W = 'for') And (DeCurVerb = 'ask') Then Begin PrepW := 'um'; PrepCase
      := 'a'; End
  Else If (W = 'at') And (DeCurVerb = 'look') Then Begin PrepW := 'auf';
      PrepCase := 'a'; End
  Else If (W = 'about') And InList(DeCurVerb, 'talk speak think') Then Begin
      PrepW := 'über'; PrepCase := 'a'; End;
  J := I + 1;
  TranslatePPDe := PrepW;
  If J > TN Then
  Begin
    I := J;
    Exit;
  End;
  { expressions de temps }
  K := J;
  If (K <= TN) And (Tk[K].W = 'the') Then Inc(K);
  If (K <= TN) And (Tk[K].N > 0) And ((W = 'in') Or (W = 'at') Or (W = 'on'))
      Then
  Begin
    If (W = 'in') And (Tk[K].W = 'morning') Then
    Begin
      TranslatePPDe := 'morgens';
      I := K + 1;
      Exit;
    End;
    If (W = 'in') And (Tk[K].W = 'afternoon') Then
    Begin
      TranslatePPDe := 'nachmittags';
      I := K + 1;
      Exit;
    End;
    If (W = 'in') And (Tk[K].W = 'evening') Then
    Begin
      TranslatePPDe := 'abends';
      I := K + 1;
      Exit;
    End;
    If (W = 'at') And (Tk[K].W = 'night') Then
    Begin
      TranslatePPDe := 'nachts';
      I := K + 1;
      Exit;
    End;
    If (W = 'on') And InList(Tk[K].W,
        'monday tuesday wednesday thursday friday saturday sunday') Then
    Begin
      TranslatePPDe := 'am ' + Lex[Tk[K].N].De;
      I := K + 1;
      Exit;
    End;
    If (W = 'in') And IsCalendar(Tk[K].W) And
       Not InList(Tk[K].W,
           'monday tuesday wednesday thursday friday saturday sunday english f'+
           'rench german spanish italian chinese japanese russian portuguese a'+
           'rabic swedish danish') Then
    Begin
      TranslatePPDe := 'im ' + Lex[Tk[K].N].De;
      I := K + 1;
      Exit;
    End;
  End;
  If (Tk[J].Tag = 'U') And (W = 'at') And (DeNumber(Tk[J].W) <> '') And
      (Tk[J].W[1] > '9') Then
  Begin
    TranslatePPDe := 'um ' + DeNumber(Tk[J].W) + ' Uhr';
    I := J + 1;
    If (I <= TN) And ((Tk[I].W = 'o''clock') Or (Tk[I].W = 'oclock')) Then
        Inc(I);
    Exit;
  End;
  If (Tk[J].Tag = 'U') And (Tk[J].W[1] >= '0') And (Tk[J].W[1] <= '9') Then
  Begin
    If W = 'in' Then
      TranslatePPDe := Tk[J].W
    Else If W = 'at' Then
      TranslatePPDe := 'um ' + Tk[J].W + ' Uhr'
    Else
      TranslatePPDe := PrepW + ' ' + Tk[J].W;
    I := J + 1;
    If (I <= TN) And ((Tk[I].W = 'o''clock') Or (Tk[I].W = 'oclock')) Then
        Inc(I);
    Exit;
  End;
  { noms de lieux }
  If Tk[J].Tag = 'Z' Then
  Begin
    Name := Tk[J].Orig;
    If ProperDe(Tk[J].W) <> '' Then Name := ProperDe(Tk[J].W);
    If IsDeCountry(Tk[J].W) Then
    Begin
      If DeCountryArticle(Tk[J].W) Then
      Begin
        If W = 'to' Then
          TranslatePPDe := 'in die ' + Name
        Else If W = 'from' Then
          TranslatePPDe := 'aus der ' + Name
        Else
          TranslatePPDe := 'in der ' + Name;
      End
      Else If W = 'to' Then
        TranslatePPDe := 'nach ' + Name
      Else If W = 'from' Then
        TranslatePPDe := 'aus ' + Name
      Else If (W = 'in') Or (W = 'at') Then
        TranslatePPDe := 'in ' + Name
      Else If W = 'of' Then
        TranslatePPDe := 'von ' + Name
      Else
        TranslatePPDe := PrepW + ' ' + Name;
    End
    Else
    Begin
      If W = 'to' Then
        TranslatePPDe := 'nach ' + Name
      Else If W = 'from' Then
        TranslatePPDe := 'aus ' + Name
      Else If (W = 'in') Or (W = 'at') Then
        TranslatePPDe := 'in ' + Name
      Else
        TranslatePPDe := PrepW + ' ' + Name;
    End;
    I := J + 1;
    While (I <= TN) And (Tk[I].Tag = 'Z') Do
    Begin
      TranslatePPDe := TranslatePPDe + ' ' + Tk[I].Orig;
      Inc(I);
    End;
    Exit;
  End;
  If PrepCase = 'w' Then
  Begin
    If Motion Then C := 'a' Else C := 'd';
  End
  Else If PrepCase = 'a' Then
    C := 'a'
  Else
    C := 'd';
  If Tk[J].Tag In ['O', 'P'] Then
  Begin
    TranslatePPDe := PrepW + ' ' + PronounDe(Tk[J].W, C);
    I := J + 1;
    Exit;
  End;
  If ParseNPDe(J, ' ', NP) Then
  Begin
    If (W = 'from') And InList(DeCurVerb, 'come be') Then PrepW := 'aus';
    If (W = 'by') And (NP.Head > 0) And IsVehicle(HeadEn(NP)) Then
    Begin
      PrepW := 'mit';
      C := 'd';
    End;
    If (W = 'of') And (I > 1) And InList(Tk[I - 1].W,
        'bottle bottles glass glasses cup cups piece pieces kilo kilos liter l'+
        'iters slice slices') Then
    Begin
      Res := RenderNPDe(NP, 'n', False);
      TranslatePPDe := Res;
      I := J + NP.Used;
      Exit;
    End;
    Res := RenderNPDe(NP, C, True);
    TranslatePPDe := PrepW + ' ' + Res;
    I := J + NP.Used;
    Exit;
  End;
  I := J;
End;

{ ------------------------------------------------------------------ }
{ ALLEMAND : complements                                              }
{ ------------------------------------------------------------------ }

Type
  TDeObj = Record
    IsPron, Dat, HasCo: Boolean;
    PronW: String[8];
    NP, Co: TDeNP;
    CoConj: String[8];
  End;

  TDeTail = Record
    Objs: Array[1..3] Of TDeObj;
    ObjCount: Integer;
    Advs: String;
    Manner: String;
    PPs: String;
    Pred: String;
    InfVerb: Integer;      { verbe a l'infinitif apres to }
    InfObjs: String;
    ZuLink: Boolean;
  End;

Function DeNegAdj(Const S: String): String;
Begin
  DeNegAdj := S;
End;

Procedure ParseTailDe(First, Last: Integer; Const VerbEn: String; BeLike, Neg:
    Boolean;
                      Var T: TDeTail; Motion: Boolean);
Var
  I: Integer;
  NP: TDeNP;
  PP, Adv: String;
  Plu: Boolean;
Begin
  FillChar(T, SizeOf(T), 0);
  DeCurVerb := VerbEn;
  I := First;
  While I <= Last Do
  Begin
    Case Tk[I].Tag Of
      'O', 'P':
      Begin
        If T.ObjCount < 3 Then
        Begin
          Inc(T.ObjCount);
          T.Objs[T.ObjCount].IsPron := True;
          T.Objs[T.ObjCount].PronW := Tk[I].W;
        End;
        Inc(I);
      End;
      'K':
      Begin
        If (I < Last) And (Tk[I + 1].Tag = 'V') Then
        Begin
          T.InfVerb := Tk[I + 1].V;
          Inc(I, 2);
          { objets de l'infinitif : on les traite comme des objets ordinaires }
        End
        Else
        Begin
          Inc(I);
        End;
      End;
      'E':
      Begin
        If (Tk[I].W = 'to') And (I < Last) And (T.ObjCount < 3) And
           InList(VerbEn,
               'give send show tell bring teach write say sell lend offer expl'+
               'ain listen') And
           (Tk[I + 1].Tag In ['O', 'P', 'N', 'D', 'S', 'Z']) Then
        Begin
          Inc(T.ObjCount);
          T.Objs[T.ObjCount].Dat := True;
          If Tk[I + 1].Tag In ['O', 'P'] Then
          Begin
            T.Objs[T.ObjCount].IsPron := True;
            T.Objs[T.ObjCount].PronW := Tk[I + 1].W;
            Inc(I, 2);
          End
          Else If ParseNPDe(I + 1, ' ', NP) Then
          Begin
            T.Objs[T.ObjCount].NP := NP;
            Inc(I, 1 + NP.Used);
          End
          Else
            Inc(I, 2);
          Continue;
        End;
        PP := TranslatePPDe(I, Motion);
        { expressions de temps sans preposition : adverbes }
        If InList(PP, 'morgens nachmittags abends nachts') Or (Copy(PP, 1, 3) =
            'am ') Or
           (Copy(PP, 1, 3) = 'im ') And (Pos(' ', Copy(PP, 4, 40)) = 0) Then
          T.Advs := Join(T.Advs, PP)
        Else If Copy(PP, 1, 4) = 'mit ' Then
          T.Manner := Join(T.Manner, PP)
        Else
          T.PPs := Join(T.PPs, PP);
      End;
      'N', 'D', 'S', 'T', 'Q', 'U', 'Z':
      Begin
        If (Tk[I].Tag = 'N') And (Tk[I].W = 'home') And ((I = First) Or Not
            (Tk[I - 1].Tag In ['D', 'S', 'A'])) Then
        Begin
          If Motion Then T.PPs := Join(T.PPs, 'nach Hause') Else T.PPs :=
              Join(T.PPs, 'zu Hause');
          Inc(I);
        End
        Else If ParseNPDe(I, ' ', NP) Then
        Begin
          If BeLike And (T.ObjCount = 0) Then
          Begin
            { complement d'un etre : nominatif }
            If Neg And (NP.Kind In ['i', '0']) And Not NP.Proper Then NP.Neg :=
                True;
            If (NP.Kind = 'i') And (Not NP.Plural) And (NP.AdjCount = 0) And
                IsProfession(HeadEn(NP)) Then
              NP.Kind := '0';
            If DeFemSubj And (NP.Kind = '0') And (Not NP.Plural) And (NP.Gen =
                'm') And
               IsProfession(HeadEn(NP)) Then
              T.Pred := Join(T.Pred, FemProfession(RenderNPDe(NP, 'n', False)))
            Else
              T.Pred := Join(T.Pred, RenderNPDe(NP, 'n', False));
          End
          Else If T.ObjCount < 3 Then
          Begin
            If Neg And (NP.Kind In ['i', '0', 'q']) And Not NP.Proper Then
              If (NP.Kind <> 'q') Or InList(NP.DetWord, 'some any') Then NP.Neg
                  := True;
            If (NP.Kind = '0') And (Not NP.Plural) And (Not NP.Proper) And
               (Not IsMassDe(HeadEn(NP))) And (Not Neg) And Not InList(VerbEn,
                   'like love hate prefer enjoy') Then
              NP.Kind := '0';
            If (NP.Kind = '0') And (Not NP.Plural) And (Not NP.Proper) And
                (NP.Head > 0) And
               IsMealNoun(HeadEn(NP)) And InList(VerbEn, 'cook make prepare')
                   Then
              NP.Kind := 'd';
            Inc(T.ObjCount);
            T.Objs[T.ObjCount].IsPron := False;
            T.Objs[T.ObjCount].NP := NP;
          End;
          Inc(I, NP.Used);
        End
        Else
        Begin
          If Tk[I].Tag = 'T' Then
            T.Pred := Join(T.Pred, 'das')
          Else
            T.PPs := Join(T.PPs, Tk[I].Orig);
          Inc(I);
        End;
      End;
      'A':
      Begin
        Adv := '';
        If BeLike Then
          T.Pred := Join(T.Pred, Join(Adv, Lex[Tk[I].A].De))
        Else If ParseNPDe(I, ' ', NP) Then
        Begin
          If T.ObjCount < 3 Then
          Begin
            Inc(T.ObjCount);
            T.Objs[T.ObjCount].IsPron := False;
            T.Objs[T.ObjCount].NP := NP;
          End;
          Inc(I, NP.Used - 1);
        End
        Else
          T.Pred := Join(T.Pred, Lex[Tk[I].A].De);
        Inc(I);
      End;
      'R':
      Begin
        If (Tk[I].W = 'very') And (I < Last) And (Tk[I + 1].Tag = 'A') And
            BeLike Then
        Begin
          T.Pred := Join(T.Pred, 'sehr');
          Inc(I);
        End
        Else If (Tk[I].W = 'too') And (I < Last) And (Tk[I + 1].Tag = 'A') And
            BeLike Then
        Begin
          T.Pred := Join(T.Pred, 'zu');
          Inc(I);
        End
        Else
        Begin
          Adv := AdvDe(I);
          If Motion And (Adv = 'zu Hause') Then Adv := 'nach Hause';
          If InList(Adv, 'hier dort da drüben draußen drinnen oben unten') Or
             (Adv = 'zu Hause') Or (Adv = 'nach Hause') Then
            T.PPs := Join(T.PPs, Adv)
          Else
            T.Advs := Join(T.Advs, Adv);
          Inc(I);
        End;
      End;
      'C', 'J':
      Begin
        If BeLike And (T.Pred <> '') And (I < Last) And InList(Tk[I].W,
            'and or but') Then
        Begin
          If Tk[I].W = 'but' Then T.Pred := T.Pred + ', aber'
          Else If Tk[I].W = 'and' Then T.Pred := T.Pred + ' und'
          Else T.Pred := T.Pred + ' oder';
          Inc(I);
          Continue;
        End;
        If InList(Tk[I].W, 'and or') And (I < Last) And (T.ObjCount > 0) And
            (T.Pred = '') And
           (Not T.Objs[T.ObjCount].IsPron) And (Not T.Objs[T.ObjCount].HasCo)
               And
           (Tk[I + 1].Tag In ['N', 'D', 'S', 'T', 'Q', 'U', 'A']) And
               ParseNPDe(I + 1, ' ', NP) Then
        Begin
          T.Objs[T.ObjCount].HasCo := True;
          T.Objs[T.ObjCount].Co := NP;
          If Tk[I].W = 'and' Then T.Objs[T.ObjCount].CoConj := 'und' Else
              T.Objs[T.ObjCount].CoConj := 'oder';
          Inc(I, 1 + NP.Used);
          Continue;
        End;
        If Tk[I].O > 0 Then T.PPs := Join(T.PPs, LowerFirst(Lex[Tk[I].O].De))
            Else T.PPs := Join(T.PPs, Tk[I].Orig);
        Inc(I);
      End;
      ',':
      Begin
        T.PPs := T.PPs + ',';
        Inc(I);
      End;
      'V':
      Begin
        If Tk[I].V > 0 Then T.PPs := Join(T.PPs, DeFullInf(Tk[I].V)) Else T.PPs
            := Join(T.PPs, Tk[I].Orig);
        Inc(I);
      End;
      'G': Inc(I);
    Else
      T.PPs := Join(T.PPs, Tk[I].Orig);
      Inc(I);
    End;
  End;
End;

{ ------------------------------------------------------------------ }
{ ALLEMAND : proposition                                              }
{ ------------------------------------------------------------------ }

Var
  TargetDe: Boolean;

{ Indice d'un verbe allemand du lexique (ou d'une entree creee a la demande). }
Function DeVerb(Const Inf: String): Integer;
Var
  I: Integer;
Begin
  DeVerb := 1;
  For I := 1 To LexCount Do
    If (Lex[I].Pos = 'v') And (Lex[I].De = Inf) And (Lex[I].DeSep = '') Then
    Begin
      DeVerb := I;
      Exit;
    End;
  If LexCount < MaxLex Then
  Begin
    Inc(LexCount);
    FillChar(Lex[LexCount], SizeOf(TLex), 0);
    Lex[LexCount].Pos := 'v';
    Lex[LexCount].De := Inf;
    Lex[LexCount].DeFlags := '';
    If Inf = 'kommen' Then Lex[LexCount].DeFlags := 's';
    DeVerb := LexCount;
  End;
End;

Function ConjDe(I: Integer): String;
Begin
  If Tk[I].O > 0 Then ConjDe := Lex[Tk[I].O].De Else ConjDe := Tk[I].Orig;
  If (ConjDe <> '') And (ConjDe[1] >= 'A') And (ConjDe[1] <= 'Z') And (Tk[I].Tag
      = 'C') Then
    ConjDe := LowerFirst(ConjDe);
End;

Function IsSubConj(I: Integer): Boolean;
Begin
  IsSubConj := (Tk[I].Tag = 'C') And (Tk[I].O > 0) And (Lex[Tk[I].O].DeFlags =
      'S');
End;

Function WhDe(Const W: String; Motion: Boolean; FromV: Boolean): String;
Begin
  If W = 'where' Then
  Begin
    If FromV Then WhDe := 'woher'
    Else If Motion Then WhDe := 'wohin'
    Else WhDe := 'wo';
  End
  Else If W = 'when' Then WhDe := 'wann'
  Else If W = 'why' Then WhDe := 'warum'
  Else If W = 'how' Then WhDe := 'wie'
  Else If W = 'who' Then WhDe := 'wer'
  Else If W = 'whom' Then WhDe := 'wen'
  Else If W = 'whose' Then WhDe := 'wessen'
  Else If W = 'which' Then WhDe := 'welch'
  Else WhDe := 'was';
End;

Function ModalWordOf(A, B: Integer): String;
Var
  I: Integer;
Begin
  ModalWordOf := '';
  For I := A To B Do
    If (Tk[I].Tag = 'X') And InList(Tk[I].W, 'can may could might must should')
        Then
      ModalWordOf := Tk[I].W;
End;

Function ModalInfDe(Const W: String): String;
Begin
  If (W = 'can') Or (W = 'could') Or (W = 'might') Then ModalInfDe := 'können'
  Else If W = 'may' Then ModalInfDe := 'dürfen'
  Else If W = 'should' Then ModalInfDe := 'sollen'
  Else ModalInfDe := 'müssen';
End;

Function IsModalDe(Const Inf: String): Boolean;
Begin
  IsModalDe := InList(Inf,
      'können müssen wollen sollen dürfen mögen möchten');
End;

{ Indique la forme du pronom datif pour "mir ist kalt". }
Function DeIdiomAdj(Const En: String): String;
Begin
  If En = 'hungry' Then DeIdiomAdj := 'Hunger'
  Else If En = 'thirsty' Then DeIdiomAdj := 'Durst'
  Else If En = 'afraid' Then DeIdiomAdj := 'Angst'
  Else If En = 'right' Then DeIdiomAdj := 'recht'
  Else If En = 'wrong' Then DeIdiomAdj := 'unrecht'
  Else DeIdiomAdj := '';
End;

Procedure BuildVerbDe(Const VG: TVG; Slot: Integer; L: Integer; ModalWord:
    String;
                      Var Fin, FinPrefix, Rest: String);
Var
  Tense: Char;
  PastFlag, MainPast, Sein: Boolean;
  Inf, ModalInf: String;
  Aux: Integer;
Begin
  Fin := '';
  FinPrefix := '';
  Rest := '';
  If Slot < 1 Then Slot := 3;
  Inf := Lex[L].De;
  MainPast := False;
  If VG.MainTok > 0 Then
    MainPast := MainIsPast(VG.MainTok) And Not VG.Perfect And Not VG.Prog And
                Not VG.Passive;
  PastFlag := VG.Past Or MainPast;
  If VG.Passive Then
  Begin
    If PastFlag Then Tense := 'i' Else Tense := 'p';
    If VG.Future Then
    Begin
      Fin := DeFinite(DeVerb('werden'), Slot, 'p');
      Rest := DeParticiple(L) + ' werden';
    End
    Else
    Begin
      Fin := DeFinite(DeVerb('werden'), Slot, Tense);
      Rest := DeParticiple(L);
    End;
  End
  Else If (VG.Modal <> ' ') Or IsModalDe(Inf) Then
  Begin
    If IsModalDe(Inf) Then
      ModalInf := Inf
    Else
      ModalInf := ModalInfDe(ModalWord);
    If VG.ModalCond Then Tense := 'c'
    Else If PastFlag Then Tense := 'i'
    Else Tense := 'p';
    Fin := DeFinite(DeVerb(ModalInf), Slot, Tense);
    If Not IsModalDe(Inf) Then
    Begin
      If VG.BeMain Then Rest := 'sein'
      Else If VG.HaveMain Then Rest := 'haben'
      Else If VG.MainTok > 0 Then Rest := DeFullInf(L);
    End;
  End
  Else If VG.Going Or VG.Future Then
  Begin
    Fin := DeFinite(DeVerb('werden'), Slot, 'p');
    Rest := DeFullInf(L);
  End
  Else If VG.Cond Then
  Begin
    Fin := DeFinite(DeVerb('werden'), Slot, 'c');
    Rest := DeFullInf(L);
  End
  Else If PastFlag Or VG.Perfect Then
  Begin
    If IsDeSpecial(Inf) And Not VG.Perfect Then
    Begin
      Fin := DeFinite(L, Slot, 'i');
    End
    Else If VG.Perfect And VG.BeMain Then
    Begin
      Fin := DeFinite(DeVerb('sein'), Slot, 'p');
      Rest := 'gewesen';
    End
    Else
    Begin
      Sein := DeUsesSein(L) Or (Inf = 'werden');
      If Sein Then Aux := DeVerb('sein') Else Aux := DeVerb('haben');
      Fin := DeFinite(Aux, Slot, 'p');
      Rest := DeParticiple(L);
    End;
  End
  Else
  Begin
    Fin := DeFinite(L, Slot, 'p');
    FinPrefix := Lex[L].DeSep;
  End;
End;

{ Assemble les champs d'une proposition allemande. }
Function AssembleDe(Const Front, SubjText, Fin, FinPrefix, Rest, Middle, Trail:
    String;
                    Mode: Char): String;
Var
  R, Verb: String;
Begin
  { Mode : 2 verbe en deuxieme position, 1 verbe en tete, F verbe final }
  If Mode = 'F' Then
  Begin
    R := Join(SubjText, Middle);
    If Rest <> '' Then R := Join(R, Join(Rest, Fin))
    Else R := Join(R, FinPrefix + Fin);
    AssembleDe := Join(R, Trail);
    Exit;
  End;
  If Mode = '1' Then
    R := Join(Fin, Join(SubjText, Middle))
  Else If Front <> '' Then
    R := Join(Front, Join(Fin, Join(SubjText, Middle)))
  Else
    R := Join(SubjText, Join(Fin, Middle));
  R := Join(R, Join(FinPrefix, Rest));
  AssembleDe := Join(R, Trail);
End;

Function ObjCaseDe(Const T: TDeTail; K: Integer; DatVerb: Boolean): Char;
Var
  J, DatCount: Integer;
Begin
  If T.Objs[K].Dat Then
  Begin
    ObjCaseDe := 'd';
    Exit;
  End;
  DatCount := 0;
  For J := 1 To T.ObjCount Do
    If T.Objs[J].Dat Then Inc(DatCount);
  If DatCount > 0 Then
    ObjCaseDe := 'a'
  Else If T.ObjCount >= 2 Then
  Begin
    If K = 1 Then ObjCaseDe := 'd' Else ObjCaseDe := 'a';
  End
  Else If DatVerb Then
    ObjCaseDe := 'd'
  Else
    ObjCaseDe := 'a';
End;

{ Ordre du Mittelfeld : pronoms, adverbes, groupes nominaux (datif avant
  accusatif). }
Function MiddleDe(Const T: TDeTail; DatVerb, Nicht: Boolean; Const Gern:
    String): String;
Var
  K: Integer;
  C: Char;
  Pron, Nouns, Dats, Accs, Txt: String;
Begin
  Pron := '';
  Dats := '';
  Accs := '';
  For K := 1 To T.ObjCount Do
  Begin
    C := ObjCaseDe(T, K, DatVerb);
    If T.Objs[K].IsPron Then
    Begin
      Txt := PronounDe(T.Objs[K].PronW, C);
      If C = 'a' Then Pron := Join(Txt, Pron) Else Pron := Join(Pron, Txt);
      { acc avant dat entre pronoms : on remet l'accusatif devant }
    End
    Else
    Begin
      Txt := RenderNPDe(T.Objs[K].NP, C, False);
      If T.Objs[K].HasCo Then
        Txt := Join(Txt, Join(T.Objs[K].CoConj, RenderNPDe(T.Objs[K].Co, C,
            False)));
      If C = 'd' Then Dats := Join(Dats, Txt) Else Accs := Join(Accs, Txt);
    End;
  End;
  Nouns := Join(Dats, Accs);
  MiddleDe := Join(Pron, Join(T.Advs, Nouns));
  If Nicht Then MiddleDe := Join(MiddleDe, 'nicht');
  If Gern <> '' Then MiddleDe := Join(MiddleDe, Gern);
  MiddleDe := Join(MiddleDe, Join(T.Manner, Join(T.Pred, T.PPs)));
End;

Function TailHasKeinDe(Const T: TDeTail): Boolean;
Var
  K: Integer;
Begin
  TailHasKeinDe := False;
  For K := 1 To T.ObjCount Do
    If (Not T.Objs[K].IsPron) And T.Objs[K].NP.Neg Then TailHasKeinDe := True;
  If Pos('kein', T.Pred) > 0 Then TailHasKeinDe := True;
End;

Function ZuInfDe(L: Integer): String;
Var
  Sep: String;
Begin
  Sep := Lex[L].DeSep;
  If Sep <> '' Then
    ZuInfDe := Sep + 'zu' + Lex[L].De
  Else
    ZuInfDe := 'zu ' + Lex[L].De;
End;

Function TranslateClauseDe(A, B: Integer; IsQ, Final: Boolean; Const FrontIn:
    String): String;
Var
  I, Aux0, EndSubj, K, Next, L, KPos, KV, InfL, Pass: Integer;
  Front, Wh, WhW, Fin, FinPrefix, Rest, Res, VerbEn, Mid, Trail, Gern, SubjText,
      ModalWord, Tmp2: String;
  Subj: TDeSubj;
  VG: TVG;
  T, T2: TDeTail;
  Tmp: TTok;
  Imperative, Please, WhSubject, Inverted, DatVerb, Nicht, Motion, ZuType, Opt,
      HaveIdiom: Boolean;
  NP: TDeNP;
  Mode: Char;
  Idiom, Pre, SubClause: String;
  Sub2: TDeSubj;
  DatPron: Boolean;
Begin
  TranslateClauseDe := '';
  If A > B Then Exit;
  Front := FrontIn;
  Wh := '';
  WhW := '';
  Please := False;
  Imperative := False;
  Inverted := False;
  WhSubject := False;
  Idiom := '';
  Gern := '';
  Trail := '';
  SubClause := '';

  { "please" en fin de phrase }
  If (B > A) And (Tk[B].W = 'please') Then
  Begin
    Please := True;
    Dec(B);
    If (B > A) And (Tk[B].Tag = ',') Then Dec(B);
  End;
  { interjection ou liaison en tete }
  If (Tk[A].Tag = 'J') And (A < B) And (Tk[A].W = 'please') Then
  Begin
    Please := True;
    Inc(A);
  End
  Else If (Tk[A].Tag = 'J') And (A < B) Then
  Begin
    TranslateClauseDe := ConjDe(A) + ' ' + TranslateClauseDe(A + 1, B, IsQ,
        Final, Front);
    Exit;
  End
  Else If (Tk[A].Tag = 'J') And (A = B) Then
  Begin
    TranslateClauseDe := UpperFirst(ConjDe(A));
    Exit;
  End;
  If (Tk[A].Tag = 'C') And (A < B) Then
  Begin
    TranslateClauseDe := ConjDe(A) + ' ' + TranslateClauseDe(A + 1, B, IsQ,
        Final, Front);
    Exit;
  End;

  { adverbe ou groupe prepositionnel de temps en tete : le sujet passe apres le
    verbe }
  If (Front = '') And (Not Final) And (Tk[A].Tag = 'R') And (A < B) And
     (Tk[A + 1].Tag In ['P', 'D', 'S', 'N', 'Z', 'T', 'Q', 'U']) And (Tk[A].R >
         0) Then
  Begin
    Front := Lex[Tk[A].R].De;
    Inc(A);
  End
  Else If (Front = '') And (Not Final) And (Tk[A].Tag = 'E') And (Not IsQ) Then
  Begin
    I := A;
    Tmp2 := TranslatePPDe(I, False);
    If (I <= B) And (Tk[I].Tag = ',') Then Inc(I);
    If (I <= B) And ClauseHasVerb(I, B) Then
    Begin
      Front := Tmp2;
      A := I;
    End;
  End;

  { mot interrogatif }
  If Tk[A].Tag = 'W' Then
  Begin
    WhW := Tk[A].W;
    If (WhW = 'how') And (A < B) And ((Tk[A + 1].W = 'much') Or (Tk[A + 1].W =
        'many')) Then
    Begin
      If Tk[A + 1].W = 'many' Then Wh := 'wie viele' Else Wh := 'wie viel';
      Inc(A, 2);
      If (Tk[A - 1].W = 'many') And (A <= B) And ParseNPDe(A, ' ', NP) Then
      Begin
        NP.Plural := True;
        Wh := Wh + ' ' + NounDe(NP.Head, True, 'a');
        Inc(A, NP.Used);
      End;
    End
    Else If ((WhW = 'what') Or (WhW = 'which')) And (A < B) And (Tk[A + 1].Tag =
        'N') Then
    Begin
      If ParseNPDe(A + 1, ' ', NP) Then
      Begin
        Wh := 'welch' + DerEnding(NP.Gen, NP.Plural, 'n') + ' ' +
            NounDe(NP.Head, NP.Plural, 'n');
        Inc(A, 1 + NP.Used);
      End
      Else
        Inc(A);
    End
    Else
    Begin
      Wh := WhDe(WhW, False, False);
      Inc(A);
    End;
    If A > B Then
    Begin
      TranslateClauseDe := UpperFirst(Wh);
      Exit;
    End;
  End;

  { who / what sujet : "Who lives here ?" }
  If (Wh = 'wer') Or (Wh = 'was') Then
    If (A <= B) And (Tk[A].Tag = 'V') Then
    Begin
      FillChar(Subj, SizeOf(Subj), 0);
      Subj.Found := True;
      Subj.IsWh := True;
      Subj.Slot := 3;
      Subj.Text := Wh;
      Subj.Used := 0;
      Wh := '';
      Inverted := True;
    End;

  { inversion sujet - auxiliaire : on remet la phrase dans l'ordre sujet -
    auxiliaire }
  If (IsQ Or (Wh <> '')) And (Tk[A].Tag = 'X') And (A < B) And Not Inverted Then
  Begin
    Aux0 := A;
    If (Tk[A + 1].W = 'there') And IsBeForm(Tk[A].W) Then
    Begin
      FillChar(Subj, SizeOf(Subj), 0);
      Subj.Found := True;
      Subj.IsThere := True;
      Subj.Slot := 3;
      Subj.Text := 'es';
      Subj.Used := 1;
    End
    Else If Not ParseSubjectDe(A + 1, Subj) Then
      Subj.Found := False;
    If Subj.Found Then
    Begin
      EndSubj := A + Subj.Used;
      Tmp := Tk[Aux0];
      For K := Aux0 To EndSubj - 1 Do
        Tk[K] := Tk[K + 1];
      Tk[EndSubj] := Tmp;
      Inverted := True;
    End;
  End;

  { imperatif }
  If Not Inverted Then
  Begin
    If (Tk[A].W = 'let') And (A + 2 <= B) And (Tk[A + 1].W = 'us') And (Tk[A +
        2].Tag = 'V') Then
    Begin
      L := Tk[A + 2].V;
      ParseTailDe(A + 3, B, Lex[L].En, False, False, T,
          IsMotionVerb(Lex[L].En));
      DatVerb := Pos('d', Lex[L].DeFlags) > 0;
      Mid := MiddleDe(T, DatVerb, False, '');
      Res := AssembleDe('', 'wir', DeFinite(L, 4, 'p'), Lex[L].DeSep, '', Mid,
          '', '1');
      TranslateClauseDe := Res;
      Exit;
    End
    Else If (Tk[A].Tag = 'V') And (Tk[A].VForm In ['b', 'y']) And (Wh = '') And
        Not IsQ Then
      Imperative := True
    Else If (Tk[A].W = 'be') And (A < B) And (Wh = '') And Not IsQ Then
      Imperative := True
    Else If (Tk[A].W = 'do') And (A + 2 <= B) And (Tk[A + 1].Tag = 'G') And
        (Tk[A + 2].Tag = 'V') And
            (Wh = '') Then
      Imperative := True
    Else If (Tk[A].W = 'let') And (A + 2 <= B) And (Tk[A + 1].W = 'us') And
        (Tk[A + 2].Tag = 'V') Then
    Begin
      L := Tk[A + 2].V;
      ParseTailDe(A + 3, B, Lex[L].En, False, False, T,
          IsMotionVerb(Lex[L].En));
      DatVerb := Pos('d', Lex[L].DeFlags) > 0;
      Mid := MiddleDe(T, DatVerb, False, '');
      Res := AssembleDe('', 'wir', DeFinite(L, 4, 'p'), Lex[L].DeSep, '', Mid,
          '', '1');
      TranslateClauseDe := Res;
      Exit;
    End;
  End;

  If Imperative Then
  Begin
    I := A;
    Nicht := False;
    If Tk[I].W = 'do' Then
    Begin
      Nicht := True;
      Inc(I, 2);
    End;
    If Tk[I].W = 'be' Then
    Begin
      ParseTailDe(I + 1, B, 'be', True, False, T, False);
      Mid := MiddleDe(T, False, False, '');
      If Nicht Then Mid := Join('nicht', Mid);
      Res := Join('Seien Sie', Mid);
      If Please Then Res := 'Seien Sie bitte ' + Mid;
      TranslateClauseDe := Res;
      Exit;
    End;
    L := Tk[I].V;
    VerbEn := Lex[L].En;
    DatVerb := Pos('d', Lex[L].DeFlags) > 0;
    ParseTailDe(I + 1, B, VerbEn, False, Nicht, T, IsMotionVerb(VerbEn));
    If (Tk[I].W = 'do') Or Nicht Then Nicht := True;
    Mid := MiddleDe(T, DatVerb, Nicht And Not TailHasKeinDe(T), '');
    If Please Then Mid := Join('bitte', Mid);
    Res := AssembleDe('', 'Sie', DeFinite(L, 6, 'p'), Lex[L].DeSep, '', Mid, '',
        '1');
    TranslateClauseDe := Res;
    Exit;
  End;

  { sujet }
  FillChar(CurDeSubj, SizeOf(CurDeSubj), 0);
  If Not Inverted Then
  Begin
    If Not ParseSubjectDe(A, Subj) Then
    Begin
      ParseTailDe(A, B, '', False, False, T, False);
      TranslateClauseDe := Join(Wh, MiddleDe(T, False, False, ''));
      Exit;
    End;
  End;
  If Subj.IsWh Then WhSubject := True;
  I := A + Subj.Used;
  SubjText := Subj.Text;

  ParseVG(I, VG);
  Next := VG.Next;
  ModalWord := ModalWordOf(I, Next - 1);
  VerbEn := '';
  L := 0;
  If VG.MainTok > 0 Then
  Begin
    L := Tk[VG.MainTok].V;
    VerbEn := Lex[L].En;
  End
  Else If VG.BeMain Then
  Begin
    L := DeVerb('sein');
    VerbEn := 'be';
  End
  Else If VG.HaveMain Then
  Begin
    L := DeVerb('haben');
    VerbEn := 'have';
  End
  Else If VG.DoDummy Then
  Begin
    L := DeVerb('machen');
    VerbEn := 'do';
  End
  Else If VG.Modal <> ' ' Then
  Begin
    L := DeVerb('sein');
  End
  Else
  Begin
    { pas de verbe : on traduit les mots restants }
    ParseTailDe(Next, B, '', False, False, T, False);
    TranslateClauseDe := Join(Wh, Join(SubjText, MiddleDe(T, False, False,
        '')));
    Exit;
  End;

  If (Next <= B) And (Tk[Next].W = 'born') Then VG.Past := False;

  { idiomes avec sein }
  HaveIdiom := False;
  DatPron := False;
  If VG.BeMain And (Next <= B) Then
  Begin
    If (Tk[Next].Tag = 'A') And (DeIdiomAdj(Lex[Tk[Next].A].En) <> '') And
       Not (Subj.IsIt Or Subj.IsThere Or Subj.IsDemo) And (Not VG.Perfect) Then
    Begin
      L := DeVerb('haben');
      Idiom := DeIdiomAdj(Lex[Tk[Next].A].En);
      If Idiom = 'unrecht' Then Idiom := 'unrecht' Else If Idiom = 'recht' Then
          Idiom := 'recht';
      VG.BeMain := False;
      VG.HaveMain := True;
      HaveIdiom := True;
      Inc(Next);
    End
    Else If (Tk[Next].Tag = 'A') And InList(Lex[Tk[Next].A].En, 'cold hot warm')
        And Subj.IsPron And
            Not Subj.IsIt And (Not Subj.IsI Or True) And Not VG.Perfect Then
    Begin
      DatPron := True;
      SubjText := PronounDe(Subj.PronW, 'd');
      Subj.Slot := 3;
    End
    Else If Tk[Next].W = 'from' Then
    Begin
      L := DeVerb('kommen');
      VG.BeMain := False;
      VerbEn := 'come';
    End
    Else If Tk[Next].W = 'born' Then
    Begin
      Inc(Next);
      VG.Past := True;
      VG.Passive := True;
      VG.BeMain := False;
      L := DeVerb('gebären');
      Idiom := '';
    End;
  End;
  If VG.HaveMain And (Idiom = '') And (Next <= B) And (Tk[Next].Tag In ['N',
      'D', 'Q']) Then
  Begin
    K := Next;
    While (K <= B) And Not (Tk[K].Tag = 'N') Do
      Inc(K);
    If (K <= B) And (Tk[K].N > 0) And IsMealNoun(Lex[Tk[K].N].En) Then
    Begin
      L := DeVerb('essen');
      If InList(Lex[Tk[K].N].En, 'coffee tea water juice milk wine beer') Then L
          := DeVerb('trinken');
      VerbEn := 'eat';
      VG.HaveMain := False;
    End;
  End;
  If (VerbEn = 'know') And (Next <= B) And (Tk[Next].Tag In ['Z', 'O', 'D', 'S',
      'N']) And
     Not InList(Tk[Next].W, 'it that this what') Then
    L := DeVerb('kennen');

  { look for : suchen ; go ... by bus : fahren }
  If (VerbEn = 'look') And (Next <= B) And (Tk[Next].W = 'for') Then
  Begin
    L := DeVerb('suchen');
    Inc(Next);
  End;
  If (VerbEn = 'go') Or (VerbEn = 'come') Then
    For K := Next To B - 1 Do
      If (Tk[K].W = 'by') And (Tk[K + 1].N > 0) And IsVehicle(Lex[Tk[K +
          1].N].En) Then
        If VerbEn = 'go' Then L := DeVerb('fahren');

  { verbe de pensee suivi d'une proposition sans "that" }
  If (VG.MainTok > 0) And InList(VerbEn, 'think believe hope say feel know') And
      (Next < B) Then
    If ParseSubjectDe(Next, Sub2) Then
      If (Next + Sub2.Used <= B) And (Tk[Next + Sub2.Used].Tag In ['X', 'V'])
          Then
      Begin
        SubClause := ', ' + TranslateClauseDe(Next, B, False, False, '');
        B := Next - 1;
      End;

  { il y a }
  If Subj.IsThere Then
  Begin
    VG.BeMain := False;
    L := DeVerb('geben');
    VerbEn := 'be';
  End;

  { construction avec un infinitif : want, need, like, try ... }
  ZuType := False;
  KPos := 0;
  KV := 0;
  If ((VG.MainTok > 0) Or VG.BeMain) And (Next <= B) Then
    For K := Next To B Do
      If (KPos = 0) And (Tk[K].Tag = 'K') And (K < B) And (Tk[K + 1].Tag = 'V')
          Then
      Begin
        KPos := K;
        KV := Tk[K + 1].V;
      End
      Else If (KPos = 0) And (K = Next) And (Tk[K].Tag = 'V') And (Tk[K].VForm =
          'g') And
              InList(VerbEn,
                  'like love hate prefer enjoy start begin stop finish') Then
      Begin
        KPos := K - 1;
        KV := Tk[K].V;
      End;
  Opt := False;
  If VG.Cond And (KV = 0) And InList(VerbEn, 'like love') Then
  Begin
    L := DeVerb('möchten');
    VG.Cond := False;
  End;
  If KV > 0 Then
  Begin
    If VG.Cond And InList(VerbEn, 'like love') Then
    Begin
      L := DeVerb('möchten');
      VG.Cond := False;
    End
    Else If InList(VerbEn, 'like love enjoy') Then
    Begin
      If VerbEn = 'love' Then Gern := 'sehr gern' Else Gern := 'gern';
      L := KV;
      VG.MainTok := VG.MainTok;
      Opt := True;
    End
    Else If VerbEn = 'prefer' Then
    Begin
      Gern := 'lieber';
      L := KV;
      Opt := True;
    End
    Else If VerbEn = 'hate' Then
    Begin
      Gern := 'ungern';
      L := KV;
      Opt := True;
    End
    Else If VerbEn = 'need' Then
      L := DeVerb('müssen')
    Else If VerbEn = 'want' Then
      L := DeVerb('wollen')
    Else If (Lex[L].DeFlags <> 'xx') And (Not IsModalDe(Lex[L].De)) Then
      ZuType := True;
  End;

  If Opt Then
  Begin
    { le verbe lexical devient celui de l'infinitif, avec "gern" }
    VerbEn := Lex[L].En;
  End;

  DatVerb := Pos('d', Lex[L].DeFlags) > 0;
  Motion := IsMotionVerb(VerbEn);
  If Opt And (KV > 0) Then DatVerb := Pos('d', Lex[KV].DeFlags) > 0;

  { complements }
  DeFemSubj := Subj.IsPron And (Subj.PronW = 'she');
  Nicht := VG.Neg And (VG.NegWord = 'pas');
  If KV > 0 Then
  Begin
    If ZuType Then
    Begin
      ParseTailDe(Next, KPos - 1, VerbEn, VG.BeMain, Nicht, T, Motion);
      ParseTailDe(KPos + 2, B, Lex[KV].En, False, False, T2,
          IsMotionVerb(Lex[KV].En));
      Trail := MiddleDe(T2, Pos('d', Lex[KV].DeFlags) > 0, False, '');
      If Trail <> '' Then Trail := ', ' + Join(Trail, ZuInfDe(KV))
      Else Trail := ZuInfDe(KV);
      If Trail[1] <> ',' Then Trail := ' ' + Trail;
    End
    Else
    Begin
      { modal / gern : un seul groupe, le verbe est celui de l'infinitif }
      ParseTailDe(Next, B, Lex[KV].En, False, Nicht, T,
          IsMotionVerb(Lex[KV].En));
      If Opt Then DatVerb := Pos('d', Lex[KV].DeFlags) > 0;
      If IsModalDe(Lex[L].De) Then
      Begin
        Motion := IsMotionVerb(Lex[KV].En);
        DatVerb := Pos('d', Lex[KV].DeFlags) > 0;
      End;
    End;
  End
  Else
  Begin
    ParseTailDe(Next, B, VerbEn, VG.BeMain, Nicht, T, Motion);
    If (VG.BeMain Or Subj.IsThere) And (VerbEn = 'be') And (Not Subj.IsThere)
        Then
      DatVerb := False;
  End;
  If HaveIdiom Then
  Begin
    T.Pred := Join(Idiom, T.Pred);
    Pre := '';
  End;

  { conjugaison }
  If (KV > 0) And Not ZuType And Not Opt And Not IsModalDe(Lex[L].De) Then
    KV := 0;
  If (KV > 0) And Not ZuType Then
  Begin
    If Opt Then
      BuildVerbDe(VG, Subj.Slot, L, ModalWord, Fin, FinPrefix, Rest)
    Else
    Begin
      BuildVerbDe(VG, Subj.Slot, L, ModalWord, Fin, FinPrefix, Rest);
      Rest := DeFullInf(KV);
    End;
  End
  Else
    BuildVerbDe(VG, Subj.Slot, L, ModalWord, Fin, FinPrefix, Rest);
  If VG.Passive And (VerbEn = 'be') And (L > 0) And (Lex[L].De = 'gebären')
      Then
  Begin
    Fin := DeFinite(DeVerb('werden'), Subj.Slot, 'i');
    FinPrefix := '';
    Rest := 'geboren';
  End;
  If Subj.IsThere Then
  Begin
    If VG.Past Then
      Fin := 'gab'
    Else If VG.Future Then
    Begin
      Fin := 'wird';
      Rest := 'geben';
    End
    Else If VG.Cond Then
    Begin
      Fin := 'würde';
      Rest := 'geben';
    End
    Else
      Fin := 'gibt';
    FinPrefix := '';
    If (Not VG.Future) And (Not VG.Cond) Then Rest := '';
  End;
  { "il n'y a pas de" : kein ; "never" : nie }
  If VG.Neg And (VG.NegWord = 'jamais') Then T.Advs := Join('nie', T.Advs);
  If VG.Adv <> '' Then
  Begin
    Tmp2 := '';
    For K := 1 To VG.AdvCount Do
      Tmp2 := Join(Tmp2, AdvDe(VG.AdvTok[K]));
    T.Advs := Join(Tmp2, T.Advs);
  End;

  Nicht := Nicht And Not TailHasKeinDe(T);
  If Subj.IsThere And Nicht Then
  Begin
    { "es gibt kein ..." }
    Nicht := False;
    For K := 1 To T.ObjCount Do
      If Not T.Objs[K].IsPron Then
      Begin
        T.Objs[K].NP.Neg := True;
        Nicht := False;
      End;
    If T.ObjCount = 0 Then Nicht := True;
  End;
  Mid := MiddleDe(T, DatVerb, Nicht, Gern);
  If Please Then Mid := Join('bitte', Mid);
  If SubClause <> '' Then Trail := Trail + SubClause;

  { "wie geht es Ihnen" }
  If (WhW = 'how') And VG.BeMain And Subj.IsPron And Not VG.Past Then
  Begin
    Tmp2 := '';
    For K := Next To B do
      If Tk[K].Tag = 'R' Then Tmp2 := Join(Tmp2, AdvDe(K)) Else Tmp2 := '#';
    If (Tmp2 <> '#') And (Copy(Tmp2, 1, 1) <> '#') Then
    Begin
      TranslateClauseDe := Join(Wh + ' geht es ' + PronounDe(Subj.PronW, 'd'),
          Tmp2);
      Exit;
    End;
  End;

  { mot interrogatif : orientation }
  If (Wh <> '') And (WhW <> '') Then
    If (WhW = 'where') Then
    Begin
      If Motion Then Wh := 'wohin';
      If (VerbEn = 'come') Then Wh := 'woher';
    End;
  If WhSubject Then
  Begin
    Mode := '2';
    Front := '';
  End
  Else If Wh <> '' Then
  Begin
    Front := Wh;
    Mode := '2';
  End
  Else If (IsQ Or Inverted) And (Not Final) Then
    Mode := '1'
  Else If Final Then
    Mode := 'F'
  Else
    Mode := '2';

  If DatPron Then
  Begin
    { mir ist kalt : le datif ouvre la phrase }
    Res := AssembleDe(Front, SubjText, Fin, FinPrefix, Rest, Mid, Trail, Mode);
  End
  Else
    Res := AssembleDe(Front, SubjText, Fin, FinPrefix, Rest, Mid, Trail, Mode);
  If Final And (Front <> '') Then Res := Join(Front, Res);
  TranslateClauseDe := Res;
End;

{ Proposition(s) : virgules, conjonctions, subordonnees. }
Function TranslateTokensDe(A, B: Integer; IsQ: Boolean): String;
Var
  K, M: Integer;
  SubText: String;
Begin
  If A > B Then
  Begin
    TranslateTokensDe := '';
    Exit;
  End;
  { subordonnee en tete : "If it rains, I stay." }
  If IsSubConj(A) Then
    For K := A + 2 To B - 1 Do
      If (Tk[K].Tag = ',') And (Tk[K].W = ',') And ClauseHasVerb(A + 1, K - 1)
          And ClauseHasVerb(K + 1, B) Then
      Begin
        SubText := ConjDe(A) + ' ' + TranslateClauseDe(A + 1, K - 1, False,
            True, '');
        TranslateTokensDe := TranslateClauseDe(K + 1, B, IsQ, False, SubText +
            ',');
        Exit;
      End;
  For K := A + 1 To B - 1 Do
    If (Tk[K].Tag = ',') And (Tk[K].W = ',') Then
      If ((K - A = 1) And (Tk[A].Tag = 'J')) Or
         (ClauseHasVerb(A, K - 1) And ClauseHasVerb(K + 1, B)) Then
      Begin
        TranslateTokensDe := TranslateTokensDe(A, K - 1, False) + ', ' +
            TranslateTokensDe(K + 1, B, IsQ);
        Exit;
      End;
  For K := A + 1 To B - 1 Do
    If (Tk[K].Tag = 'C') And ClauseHasVerb(A, K - 1) And ClauseHasVerb(K + 1, B)
        Then
    Begin
      If IsSubConj(K) Then
        TranslateTokensDe := TranslateTokensDe(A, K - 1, False) + ', ' +
            ConjDe(K) + ' ' +
                             TranslateClauseDe(K + 1, B, False, True, '')
      Else If (ConjDe(K) = 'aber') Or (ConjDe(K) = 'sondern') Then
        TranslateTokensDe := TranslateTokensDe(A, K - 1, False) + ', ' +
            ConjDe(K) + ' ' +
                             TranslateTokensDe(K + 1, B, IsQ)
      Else
        TranslateTokensDe := TranslateTokensDe(A, K - 1, False) + ' ' +
            ConjDe(K) + ' ' +
                             TranslateTokensDe(K + 1, B, IsQ);
      Exit;
    End;
  TranslateTokensDe := TranslateClauseDe(A, B, IsQ, False, '');
End;

{ Contractions et ponctuation allemandes. }
Function FixGerman(Const S: String): String;
Var
  Words: Array[1..120] Of String[40];
  Count, I, P: Integer;
  Cur, Res: String;
Begin
  Count := 0;
  Cur := S;
  While Cur <> '' Do
  Begin
    P := Pos(' ', Cur);
    If P = 0 Then
    Begin
      If Count < 120 Then
      Begin
        Inc(Count);
        Words[Count] := Cur;
      End;
      Cur := '';
    End
    Else
    Begin
      If P > 1 Then
        If Count < 120 Then
        Begin
          Inc(Count);
          Words[Count] := Copy(Cur, 1, P - 1);
        End;
      Delete(Cur, 1, P);
    End;
  End;
  I := 1;
  While I < Count Do
  Begin
    If (Words[I + 1] = 'dem') And (I > 0) Then
    Begin
      If Words[I] = 'in' Then Begin Words[I] := 'im'; Words[I + 1] := ''; End
      Else If Words[I] = 'an' Then Begin Words[I] := 'am'; Words[I + 1] := '';
          End
      Else If Words[I] = 'zu' Then Begin Words[I] := 'zum'; Words[I + 1] := '';
          End
      Else If Words[I] = 'von' Then Begin Words[I] := 'vom'; Words[I + 1] := '';
          End
      Else If Words[I] = 'bei' Then Begin Words[I] := 'beim'; Words[I + 1] :=
          ''; End;
    End
    Else If (Words[I + 1] = 'der') And (Words[I] = 'zu') Then
    Begin
      Words[I] := 'zur';
      Words[I + 1] := '';
    End
    Else If (Words[I + 1] = 'das') And (Words[I] = 'in') Then
    Begin
      Words[I] := 'ins';
      Words[I + 1] := '';
    End
    Else If (Words[I + 1] = 'das') And (Words[I] = 'an') Then
    Begin
      Words[I] := 'ans';
      Words[I + 1] := '';
    End;
    If Words[I + 1] = '' Then
    Begin
      { le mot suivant est absorbe : on le saute en gardant l'indice }
      Inc(I);
    End;
    Inc(I);
  End;
  Res := '';
  For I := 1 To Count Do
    If Words[I] <> '' Then
    Begin
      If (Words[I][1] = ',') Or (Res = '') Then
        Res := Res + Words[I]
      Else
        Res := Res + ' ' + Words[I];
    End;
  FixGerman := Res;
End;

{ ------------------------------------------------------------------ }
{ FRANCAIS -> ANGLAIS : jetons et classes de mots                     }
{ ------------------------------------------------------------------ }

Type
  TFTok = Record
    W, Orig: String[34];
    Cap, Punct, Elided: Boolean;
    Cat: Char;        { voir PickFrCat }
    L: Integer;       { entree du lexique }
    Gen: Char;
    Plu: Boolean;
    Tns: Char;        { p f c i : temps, P participe, I infinitif }
    Slot: Integer;
    Inf: String[26];
    En: String[34];
  End;

Var
  FT: Array[0..MaxTokens + 1] Of TFTok;
  FN: Integer;
  FEnd: Char;
  TargetEn: Boolean;

Function FrLower(Const S: String): String;
Var
  I: Integer;
  R: String;
Begin
  R := S;
  I := 1;
  While I <= Length(R) Do
  Begin
    If (R[I] >= 'A') And (R[I] <= 'Z') Then
      R[I] := Chr(Ord(R[I]) + 32)
    Else If (R[I] = #$C3) And (I < Length(R)) Then
    Begin
      If (R[I + 1] >= #$80) And (R[I + 1] <= #$9E) And (R[I + 1] <> #$97) Then
        R[I + 1] := Chr(Ord(R[I + 1]) + 32);
      Inc(I);
    End;
    Inc(I);
  End;
  FrLower := R;
End;

Function IsDigitsOnly(Const S: String): Boolean;
Var
  I: Integer;
Begin
  IsDigitsOnly := S <> '';
  For I := 1 To Length(S) Do
    If Not (S[I] In ['0'..'9']) Then IsDigitsOnly := False;
End;

Function FrNumberEn(Const W: String): String;
Begin
  FrNumberEn := '';
  If W = 'zéro' Then FrNumberEn := 'zero'
  Else If (W = 'deux') Then FrNumberEn := 'two'
  Else If W = 'trois' Then FrNumberEn := 'three'
  Else If W = 'quatre' Then FrNumberEn := 'four'
  Else If W = 'cinq' Then FrNumberEn := 'five'
  Else If W = 'six' Then FrNumberEn := 'six'
  Else If W = 'sept' Then FrNumberEn := 'seven'
  Else If W = 'huit' Then FrNumberEn := 'eight'
  Else If W = 'neuf' Then FrNumberEn := 'nine'
  Else If W = 'dix' Then FrNumberEn := 'ten'
  Else If W = 'onze' Then FrNumberEn := 'eleven'
  Else If W = 'douze' Then FrNumberEn := 'twelve'
  Else If W = 'treize' Then FrNumberEn := 'thirteen'
  Else If W = 'quatorze' Then FrNumberEn := 'fourteen'
  Else If W = 'quinze' Then FrNumberEn := 'fifteen'
  Else If W = 'seize' Then FrNumberEn := 'sixteen'
  Else If W = 'vingt' Then FrNumberEn := 'twenty'
  Else If W = 'trente' Then FrNumberEn := 'thirty'
  Else If W = 'quarante' Then FrNumberEn := 'forty'
  Else If W = 'cinquante' Then FrNumberEn := 'fifty'
  Else If W = 'soixante' Then FrNumberEn := 'sixty'
  Else If W = 'cent' Then FrNumberEn := 'one hundred'
  Else If W = 'mille' Then FrNumberEn := 'one thousand';
End;

{ Mots grammaticaux : renvoie la classe, ou #0. }
Function FrClosedCat(Const W: String): Char;
Begin
  FrClosedCat := #0;
  If W = 'ne' Then FrClosedCat := 'G'
  Else If InList(W, 'pas jamais rien personne aucun aucune guère') Then
      FrClosedCat := 'g'
  Else If InList(W, 'je tu il elle on ils elles ça ceci cela') Then FrClosedCat
      := 'P'
  Else If InList(W, 'nous vous') Then FrClosedCat := 'P'
  Else If InList(W, 'me te se lui y') Then FrClosedCat := 'O'
  Else If InList(W, 'moi toi eux soi') Then FrClosedCat := 'B'
  Else If InList(W, 'le la les un une des du') Then FrClosedCat := 'D'
  Else If InList(W, 'au aux') Then FrClosedCat := 'a'
  Else If InList(W,
      'mon ma mes ton ta tes son sa ses notre nos votre vos leur leurs') Then
      FrClosedCat := 'S'
  Else If InList(W, 'ce cet cette ces') Then FrClosedCat := 'T'
  Else If InList(W,
      'tout toute tous toutes chaque plusieurs quelques quelque autre autres m'+
      'ême certains') Then FrClosedCat := 'Q'
  Else If InList(W,
      'qui que quoi où quand comment pourquoi combien quel quelle quels quell'+
      'es') Then FrClosedCat := 'W'
  Else If InList(W, 'et ou mais donc car lorsque puisque si parce que') Then
      FrClosedCat := 'C'
  Else If InList(W, 'à de en') Then FrClosedCat := 'E';
End;

Function FrLexMatch(Const W: String; Const Kinds: String): Integer;
Var
  I: Integer;
Begin
  FrLexMatch := 0;
  For I := 1 To LexCount Do
    If (Lex[I].Fr = W) And (Pos(Lex[I].Pos, Kinds) > 0) Then
    Begin
      FrLexMatch := I;
      Exit;
    End;
End;

Function FrKnown(Const W: String): Boolean;
Begin
  FrKnown := (FrClosedCat(W) <> #0) Or (FrNumberEn(W) <> '') Or (FrLexMatch(W,
      'nvarpcj') > 0);
End;

{ ---- Formes des noms, adjectifs et verbes ---- }

Function FrFindNoun(Const W: String; Var Plu: Boolean): Integer;
Var
  I: Integer;
Begin
  FrFindNoun := 0;
  For I := 1 To LexCount Do
    If (Lex[I].Pos = 'n') And (Lex[I].Fr <> '') And (Pos(' ', Lex[I].Fr) = 0)
        Then
    Begin
      If NounForm(I, False) = W Then
      Begin
        Plu := False;
        FrFindNoun := I;
        Exit;
      End;
      If NounForm(I, True) = W Then
      Begin
        Plu := True;
        FrFindNoun := I;
        Exit;
      End;
    End;
End;

Function FrFindAdj(Const W: String; Var Gen: Char; Var Plu: Boolean): Integer;
Var
  I: Integer;
  G: Char;
  P: Boolean;
Begin
  FrFindAdj := 0;
  For I := 1 To LexCount Do
    If (Lex[I].Pos = 'a') And (Lex[I].Fr <> '') And (Pos(' ', Lex[I].Fr) = 0)
        Then
      For G := 'm' To 'm' Do
      Begin
        If AdjForm(I, 'm', False, True) = W Then
        Begin
          Gen := 'm'; Plu := False; FrFindAdj := I; Exit;
        End;
        For P := False To True Do
        Begin
          If AdjForm(I, 'm', P, False) = W Then
          Begin
            Gen := 'm'; Plu := P; FrFindAdj := I; Exit;
          End;
          If AdjForm(I, 'f', P, False) = W Then
          Begin
            Gen := 'f'; Plu := P; FrFindAdj := I; Exit;
          End;
        End;
      End;
End;

Function SpecialVerbCount: Integer;
Begin
  SpecialVerbCount := 4;
End;

Function SpecialVerbName(K: Integer): String;
Begin
  Case K Of
    1: SpecialVerbName := 'être';
    2: SpecialVerbName := 'avoir';
    3: SpecialVerbName := 'pouvoir';
  Else
    SpecialVerbName := 'devoir';
  End;
End;

{ Cherche la forme verbale W parmi les verbes du lexique et quelques verbes
  speciaux. }
Function FrMatchVerb(Const W: String; Var Inf: String; Var Tns: Char; Var Slot:
    Integer;
                     Var VL: Integer): Boolean;
Var
  K, S: Integer;
  I: String;
  Part: String;
  Reflex: Boolean;

  Function Test(Const Cand: String; T: Char; Sl: Integer): Boolean;
  Begin
    Test := False;
    If Cand = W Then
    Begin
      Inf := I;
      Tns := T;
      Slot := Sl;
      Test := True;
    End;
  End;

  Function TryVerb(Const Base: String; L: Integer): Boolean;
  Var
    Q: Integer;
  Begin
    TryVerb := True;
    I := Base;
    VL := L;
    For Q := 1 To 6 Do
      If Test(Conjugate(Base, Q, 'p'), 'p', Q) Then Exit;
    For Q := 1 To 6 Do
      If Test(Conjugate(Base, Q, 'i'), 'i', Q) Then Exit;
    For Q := 1 To 6 Do
      If Test(Conjugate(Base, Q, 'f'), 'f', Q) Then Exit;
    For Q := 1 To 6 Do
      If Test(Conjugate(Base, Q, 'c'), 'c', Q) Then Exit;
    Part := PastParticiple(Base);
    If Test(Part, 'P', 0) Or Test(Part + 's', 'P', 0) Or Test(Part + 'e', 'P',
        0) Or
       Test(Part + 'es', 'P', 0) Then Exit;
    If Test(Base, 'I', 0) Then Exit;
    TryVerb := False;
  End;

Begin
  FrMatchVerb := True;
  If InList(W, 'sois soyons soyez aie ayons ayez') Then
  Begin
    If Copy(W, 1, 1) = 's' Then Inf := 'être' Else Inf := 'avoir';
    Tns := 'M';
    VL := 0;
    If EndsWith(W, 'ons') Then Slot := 4 Else If EndsWith(W, 'ez') Then Slot :=
        5 Else Slot := 2;
    Exit;
  End;
  For K := 1 To SpecialVerbCount Do
    If TryVerb(SpecialVerbName(K), 0) Then Exit;
  For S := 1 To LexCount Do
    If (Lex[S].Pos = 'v') And (Lex[S].Fr <> '') Then
    Begin
      Reflex := Copy(Lex[S].Fr, 1, 3) = 'se ';
      If Reflex Then
      Begin
        If Pos(' ', Copy(Lex[S].Fr, 4, 30)) = 0 Then
          If TryVerb(Copy(Lex[S].Fr, 4, 30), S) Then Exit;
      End
      Else If Pos(' ', Lex[S].Fr) = 0 Then
        If TryVerb(Lex[S].Fr, S) Then Exit;
    End;
  FrMatchVerb := False;
End;

{ ---- Tokenisation ---- }

Procedure FrPush(Const W, Orig: String; Cap, Punct, Elided: Boolean);
Begin
  If FN >= MaxTokens Then Exit;
  Inc(FN);
  FillChar(FT[FN], SizeOf(TFTok), 0);
  FT[FN].W := W;
  FT[FN].Orig := Orig;
  FT[FN].Cap := Cap;
  FT[FN].Punct := Punct;
  FT[FN].Elided := Elided;
  FT[FN].Gen := 'm';
  FT[FN].Tns := ' ';
End;

Procedure FrSplit(Const Raw: String; Cap: Boolean);
Var
  LW, Pre, Rest, Word1: String;
  P: Integer;
Begin
  If Raw = '' Then Exit;
  LW := FrLower(Raw);
  If LW = 't' Then Exit;
  P := Pos('''', LW);
  If FrKnown(LW) Or IsDigitsOnly(LW) Then
    FrPush(LW, Raw, Cap, False, False)
  Else If (P > 1) And (P < Length(LW)) Then
  Begin
    Pre := Copy(LW, 1, P - 1);
    Rest := Copy(LW, P + 1, 255);
    If Pre = 'l' Then Word1 := 'le'
    Else If Pre = 'j' Then Word1 := 'je'
    Else If Pre = 'd' Then Word1 := 'de'
    Else If Pre = 'n' Then Word1 := 'ne'
    Else If Pre = 'm' Then Word1 := 'me'
    Else If Pre = 't' Then Word1 := 'te'
    Else If Pre = 'c' Then Word1 := 'ce'
    Else If Pre = 'qu' Then Word1 := 'que'
    Else If Pre = 's' Then
    Begin
      If (Rest = 'il') Or (Rest = 'ils') Then Word1 := 'si' Else Word1 := 'se';
    End
    Else If Pre = 'jusqu' Then Word1 := 'jusque'
    Else If Pre = 'lorsqu' Then Word1 := 'lorsque'
    Else If Pre = 'puisqu' Then Word1 := 'puisque'
    Else Word1 := Pre;
    FrPush(Word1, Copy(LW, 1, P), Cap, False, True);
    FrSplit(Rest, False);
  End
  Else If (Pos('-', LW) > 1) And (Pos('-', LW) < Length(LW)) Then
  Begin
    P := Pos('-', LW);
    FrSplit(Copy(Raw, 1, P - 1), Cap);
    FrSplit(Copy(Raw, P + 1, 255), False);
  End
  Else
    FrPush(LW, Raw, Cap, False, False);
  { une majuscule sur un mot inconnu devient un nom propre }
  If (FN > 0) And Cap And (Not FrKnown(FT[FN].W)) And (Not
      IsDigitsOnly(FT[FN].W)) Then
    FT[FN].Orig := Raw;
End;

Function FrWordByte(C: Char): Boolean;
Begin
  FrWordByte := (C In ['A'..'Z', 'a'..'z', '0'..'9']) Or (C >= #$80);
End;

Procedure FrTokenize(Const S0: String);
Var
  S, Word1, T: String;
  I, L: Integer;
  C: Char;
Begin
  FN := 0;
  FEnd := #0;
  { apostrophes typographiques }
  S := '';
  I := 1;
  While I <= Length(S0) Do
  Begin
    If (S0[I] = #$E2) And (I + 2 <= Length(S0)) And (S0[I + 1] = #$80) And
       ((S0[I + 2] = #$99) Or (S0[I + 2] = #$98)) Then
    Begin
      S := S + '''';
      Inc(I, 3);
    End
    Else If (S0[I] = #$C2) And (I < Length(S0)) And
            ((S0[I + 1] = #$AB) Or (S0[I + 1] = #$BB) Or (S0[I + 1] = #$A0))
                Then
    Begin
      S := S + ' ';
      Inc(I, 2);
    End
    Else
    Begin
      S := S + S0[I];
      Inc(I);
    End;
  End;
  L := Length(S);
  I := 1;
  While I <= L Do
  Begin
    C := S[I];
    If FrWordByte(C) Then
    Begin
      Word1 := '';
      While (I <= L) And (FrWordByte(S[I]) Or
            (((S[I] = '''') Or (S[I] = '-')) And (Word1 <> '') And (I < L) And
                FrWordByte(S[I + 1]))) Do
      Begin
        Word1 := Word1 + S[I];
        Inc(I);
      End;
      FrSplit(Word1, (Word1 <> '') And (((Word1[1] >= 'A') And (Word1[1] <=
          'Z')) Or
              ((Word1[1] = #$C3) And (Length(Word1) > 1) And (Word1[2] >= #$80)
                  And (Word1[2] <= #$9E))));
    End
    Else
    Begin
      If (C = '.') Or (C = '?') Or (C = '!') Then
      Begin
        If FEnd = #0 Then FEnd := C;
        If C = '?' Then FEnd := '?';
      End
      Else If (C = ',') Or (C = ';') Or (C = ':') Then
      Begin
        FrPush(',', ',', False, True, False);
        FT[FN].Cat := ',';
      End;
      Inc(I);
    End;
  End;
End;

{ Remplace par un seul jeton les locutions du lexique (a cote de, tous les
  jours...). }
Procedure FrMergeMultiword;
Var
  I, K, Len, L: Integer;
  Key: String;
  Found, Bad: Boolean;
Begin
  I := 1;
  While I < FN Do
  Begin
    Found := False;
    For Len := 4 Downto 2 Do
      If (Not Found) And (I + Len - 1 <= FN) Then
      Begin
        Key := '';
        Bad := False;
        For K := I To I + Len - 1 Do
        Begin
          If FT[K].Punct Then Bad := True;
          If Key = '' Then
            Key := FrLower(FT[K].Orig)
          Else If (FT[K - 1].Orig <> '') And (FT[K - 1].Orig[Length(FT[K -
              1].Orig)] = '''') And
                  FT[K - 1].Elided Then
            Key := Key + FrLower(FT[K].Orig)
          Else
            Key := Key + ' ' + FrLower(FT[K].Orig);
        End;
        If Not Bad Then
        Begin
          L := FrLexMatch(Key, 'narpcj');
          If (L > 0) And (Length(Key) <= 34) Then
          Begin
            Found := True;
            FT[I].W := Key;
            FT[I].Orig := Key;
            FT[I].Elided := False;
            For K := I + 1 To FN - Len + 1 Do
              FT[K] := FT[K + Len - 1];
            Dec(FN, Len - 1);
          End;
        End;
      End;
    Inc(I);
  End;
End;

{ ------------------------------------------------------------------ }
{ FRANCAIS -> ANGLAIS : classification                                }
{ ------------------------------------------------------------------ }

Type
  TFCand = Record
    N, A, V, R, P, C, J: Integer;
    NPlu, APlu, HasV: Boolean;
    AGen: Char;
    Inf: String[26];
    Tns: Char;
    Slot: Integer;
    VL: Integer;
  End;

Procedure FrCands(Const W: String; Var C: TFCand);
Var
  Inf: String;
  T: Char;
  Sl, VL: Integer;
Begin
  FillChar(C, SizeOf(C), 0);
  C.AGen := 'm';
  C.N := FrFindNoun(W, C.NPlu);
  C.A := FrFindAdj(W, C.AGen, C.APlu);
  C.R := FrLexMatch(W, 'r');
  C.P := FrLexMatch(W, 'p');
  C.C := FrLexMatch(W, 'c');
  C.J := FrLexMatch(W, 'j');
  If FrMatchVerb(W, Inf, T, Sl, VL) Then
  Begin
    C.HasV := True;
    C.Inf := Inf;
    C.Tns := T;
    C.Slot := Sl;
    C.VL := VL;
  End;
End;

Function IsPersonEn(Const En: String): Boolean;
Begin
  IsPersonEn := InList(En,
      'man woman boy girl brother sister friend child mother father son daught'+
      'er husband wife parent teacher student doctor nurse driver worker presi'+
      'dent king queen soldier person');
End;

{ Verbes qui introduisent un infinitif ou prennent "de" : on les repere par leur
  infinitif francais. }
Function VerbEnOf(Const Inf: String; L: Integer): String;
Begin
  If Inf = 'être' Then VerbEnOf := 'be'
  Else If Inf = 'avoir' Then VerbEnOf := 'have'
  Else If Inf = 'pouvoir' Then VerbEnOf := 'can'
  Else If Inf = 'devoir' Then VerbEnOf := 'must'
  Else If Inf = 'faire' Then VerbEnOf := 'do'
  Else If Inf = 'voir' Then VerbEnOf := 'see'
  Else If Inf = 'regarder' Then VerbEnOf := 'look at'
  Else If Inf = 'parler' Then VerbEnOf := 'speak'
  Else If Inf = 'arriver' Then VerbEnOf := 'arrive'
  Else If L > 0 Then VerbEnOf := Lex[L].En
  Else VerbEnOf := Inf;
End;

Function NextIsNounish(I: Integer): Boolean;
Var
  C: TFCand;
Begin
  NextIsNounish := False;
  If I > FN Then Exit;
  If FT[I].Punct Then Exit;
  If InList(FT[I].W, 'ne y en') Then Exit;
  If FrClosedCat(FT[I].W) In ['D', 'S', 'T', 'Q', 'a'] Then
  Begin
    NextIsNounish := True;
    Exit;
  End;
  FrCands(FT[I].W, C);
  NextIsNounish := (C.N > 0) Or (C.A > 0);
End;

Function NextIsVerbOnly(I: Integer): Boolean;
Var
  C: TFCand;
Begin
  NextIsVerbOnly := False;
  If I > FN Then Exit;
  If FT[I].Punct Then Exit;
  If FT[I].W = 'ne' Then
  Begin
    NextIsVerbOnly := True;
    Exit;
  End;
  If InList(FT[I].W, 'y en me te se lui la le les nous vous leur') Then
  Begin
    NextIsVerbOnly := NextIsVerbOnly(I + 1);
    Exit;
  End;
  FrCands(FT[I].W, C);
  NextIsVerbOnly := C.HasV And (C.N = 0) And (C.A = 0);
End;

{ Classes : N nom, A adjectif, V verbe, R adverbe, E preposition, C conjonction,
  J interjection,
  D article, S possessif, T demonstratif, Q quantificateur, U nombre, P pronom
    sujet,
  O pronom complement, B pronom disjoint, G ne, g negation, W interrogatif, Z
    nom propre,
  a contraction a+article, d contraction de+article }
Procedure PickFrCat(I: Integer);
Var
  C: TFCand;
  W: String;
  Prev, Cl: Char;
  K: Integer;
  PlaceN: Boolean;
Begin
  W := FT[I].W;
  If I > 1 Then Prev := FT[I - 1].Cat Else Prev := '.';
  If FT[I].Punct Then
  Begin
    FT[I].Cat := ',';
    Exit;
  End;
  If IsDigitsOnly(W) Then
  Begin
    FT[I].Cat := 'U';
    FT[I].En := W;
    Exit;
  End;
  Cl := FrClosedCat(W);
  If (FrNumberEn(W) <> '') And (Cl = #0) Then
  Begin
    FT[I].Cat := 'U';
    FT[I].En := FrNumberEn(W);
    Exit;
  End;
  { mots grammaticaux ambigus }
  Case Cl Of
    'P':
    Begin
      If (W = 'nous') Or (W = 'vous') Then
      Begin
        If Prev = 'E' Then
          FT[I].Cat := 'B'
        Else If Prev In ['P', 'G', 'O', 'N'] Then
          FT[I].Cat := 'O'
        Else
          FT[I].Cat := 'P';
      End
      Else If (W = 'elle') And (Prev = 'E') Then
        FT[I].Cat := 'B'
      Else
        FT[I].Cat := 'P';
      Exit;
    End;
    'O':
    Begin
      If W = 'lui' Then
      Begin
        If Prev = 'E' Then FT[I].Cat := 'B' Else FT[I].Cat := 'O';
      End
      Else FT[I].Cat := 'O';
      Exit;
    End;
    'D':
    Begin
      If InList(W, 'le la les') Then
      Begin
        If (Not NextIsNounish(I + 1)) And NextIsVerbOnly(I + 1) Then
          FT[I].Cat := 'O'
        Else
          FT[I].Cat := 'D';
      End
      Else If InList(W, 'du des') Then
      Begin
        FT[I].Cat := 'D';
        If (Prev = 'N') Or (Prev = 'E') Or ((Prev = 'V') And (FT[I - 1].Inf <>
            '') And
           InList(FT[I - 1].Inf,
               'venir revenir sortir parler rêver dépendre')) Then
          FT[I].Cat := 'd';
      End
      Else
        FT[I].Cat := 'D';
      Exit;
    End;
    'a', 'B', 'T', 'Q', 'G', 'g':
    Begin
      FT[I].Cat := Cl;
      If W = 'ce' Then
        If (I < FN) And ((FT[I + 1].W = 'est') Or (FT[I + 1].W = 'sont') Or
            (FT[I + 1].W = 'était') Or
           (FT[I + 1].W = 'sera') Or (FT[I + 1].W = 'étaient')) Then FT[I].Cat
               := 'P';
      If W = 'plus' Then FT[I].Cat := 'R';
      Exit;
    End;
    'S':
    Begin
      FT[I].Cat := 'S';
      If (W = 'leur') And NextIsVerbOnly(I + 1) Then FT[I].Cat := 'O';
      Exit;
    End;
    'E':
    Begin
      FT[I].Cat := 'E';
      If W = 'en' Then
      Begin
        If NextIsVerbOnly(I + 1) And Not NextIsNounish(I + 1) Then FT[I].Cat :=
            'O';
      End;
      Exit;
    End;
    'W':
    Begin
      FT[I].Cat := 'W';
      If W = 'que' Then
        If (Prev = 'V') Or (Prev = 'N') Or (Prev = 'A') Or (Prev = ',') Or (Prev
            = 'R') Then
        Begin
          If (Prev <> ',') Or (I > 2) Then
          begin
            FT[I].Cat := 'C';
            FT[I].En := 'that';
          end;
        End;
      If (W = 'qui') And (Prev = 'N') Then FT[I].Cat := 'W';
      Exit;
    End;
    'C':
    Begin
      FT[I].Cat := 'C';
      FT[I].L := FrLexMatch(W, 'c');
      If W = 'que' Then FT[I].En := 'that'
      Else If FT[I].L > 0 Then FT[I].En := Lex[FT[I].L].En
      Else If W = 'car' Then FT[I].En := 'because'
      Else If W = 'lorsque' Then FT[I].En := 'when'
      Else If W = 'puisque' Then FT[I].En := 'since'
      Else FT[I].En := W;
      Exit;
    End;
  End;
  FrCands(W, C);
  FT[I].L := 0;
  If (Not (C.N > 0)) And (Not (C.A > 0)) And (Not C.HasV) And (C.R = 0) And (C.P
      = 0) And
     (C.C = 0) And (C.J = 0) Then
  Begin
    FT[I].Cat := 'Z';
    Exit;
  End;
  If FT[I].Cap And (I > 1) And (Not C.HasV) And (C.N = 0) And (C.A = 0) Then
  Begin
    FT[I].Cat := 'Z';
    Exit;
  End;
  { choix selon le contexte }
  If (Prev In ['D', 'S', 'T', 'Q', 'U', 'a', 'd']) And (C.N > 0) Then
    FT[I].Cat := 'N'
  Else If (Prev In ['D', 'S', 'T', 'Q', 'U', 'a', 'd']) And (C.A > 0) Then
    FT[I].Cat := 'A'
  Else If (Prev In ['P', 'O', 'G']) And C.HasV And (C.Tns <> 'P') Then
    FT[I].Cat := 'V'
  Else If (Prev In ['P', 'O', 'G']) And C.HasV Then
    FT[I].Cat := 'V'
  Else If (Prev = 'E') And (C.HasV) And (C.Tns = 'I') And (C.N = 0) Then
    FT[I].Cat := 'V'
  Else If (Prev = 'E') And (C.N > 0) Then
    FT[I].Cat := 'N'
  Else If (Prev = 'E') And (C.A > 0) Then
    FT[I].Cat := 'A'
  Else If (Prev = 'N') And (C.A > 0) Then
    FT[I].Cat := 'A'
  Else If (Prev = 'A') And (C.N > 0) Then
    FT[I].Cat := 'N'
  Else If (Prev = 'V') And (FT[I - 1].Inf = 'être') And (C.A > 0) Then
    FT[I].Cat := 'A'
  Else If (Prev = 'V') And (C.A > 0) And Not (C.HasV And (C.Tns = 'P')) Then
    FT[I].Cat := 'A'
  Else If (Prev = 'V') And (C.HasV) And (C.Tns In ['P', 'I']) Then
    FT[I].Cat := 'V'
  Else If (Prev = 'V') And (C.N > 0) Then
    FT[I].Cat := 'N'
  Else If (Prev = 'R') And C.HasV And (I > 1) And (FT[I - 1].Cat = 'R') And (I >
      2) And
          (FT[I - 2].Cat In ['P', 'O', 'G', 'V']) Then
    FT[I].Cat := 'V'
  Else If (C.R > 0) And (C.N = 0) And (C.A = 0) And (Not C.HasV) Then
    FT[I].Cat := 'R'
  Else If (C.P > 0) And (C.N = 0) And (Not C.HasV) Then
  Begin
    FT[I].Cat := 'E';
    If (C.R > 0) And Not (NextIsNounish(I + 1) Or ((I < FN) And FT[I + 1].Cap))
        Then FT[I].Cat := 'R';
  End
  Else If (C.C > 0) And (C.N = 0) And (Not C.HasV) Then
    FT[I].Cat := 'C'
  Else If (C.J > 0) And (C.N = 0) And (Not C.HasV) Then
    FT[I].Cat := 'J'
  Else If C.HasV And ((I = 1) Or (Prev In [',', 'W', 'C', 'R'])) And ((C.N = 0)
      Or (C.Tns <> 'I')) Then
    FT[I].Cat := 'V'
  Else If C.N > 0 Then
    FT[I].Cat := 'N'
  Else If C.A > 0 Then
    FT[I].Cat := 'A'
  Else If C.HasV Then
    FT[I].Cat := 'V'
  Else If C.R > 0 Then
    FT[I].Cat := 'R'
  Else If C.P > 0 Then
    FT[I].Cat := 'E'
  Else If C.C > 0 Then
    FT[I].Cat := 'C'
  Else If C.J > 0 Then
    FT[I].Cat := 'J'
  Else
    FT[I].Cat := 'Z';
  { details }
  Case FT[I].Cat Of
    'N':
    Begin
      FT[I].L := C.N;
      FT[I].Plu := C.NPlu;
      FT[I].Gen := Lex[C.N].Sub;
      If FT[I].Gen <> 'f' Then FT[I].Gen := 'm';
      FT[I].En := Lex[C.N].En;
    End;
    'A':
    Begin
      FT[I].L := C.A;
      FT[I].Plu := C.APlu;
      FT[I].Gen := C.AGen;
      FT[I].En := Lex[C.A].En;
    End;
    'V':
    Begin
      FT[I].L := C.VL;
      FT[I].Inf := C.Inf;
      FT[I].Tns := C.Tns;
      FT[I].Slot := C.Slot;
      FT[I].En := VerbEnOf(C.Inf, C.VL);
    End;
    'R':
    Begin
      FT[I].L := C.R;
      FT[I].En := Lex[C.R].En;
    End;
    'E':
    Begin
      If C.P > 0 Then
      Begin
        FT[I].L := C.P;
        FT[I].En := Lex[C.P].En;
      End;
    End;
    'C':
    Begin
      FT[I].L := C.C;
      FT[I].En := Lex[C.C].En;
    End;
    'J':
    Begin
      FT[I].L := C.J;
      FT[I].En := Lex[C.J].En;
    End;
  End;
  K := 0;
  PlaceN := False;
End;

Procedure FrOverrides(I: Integer);
Var
  W: String;
Begin
  W := FT[I].W;
  If FT[I].Cat = 'R' Then
  Begin
    If W = 'beaucoup' Then FT[I].En := 'a lot'
    Else If W = 'encore' Then FT[I].En := 'again'
    Else If W = 'plus' Then FT[I].En := 'more'
    Else If W = 'très' Then FT[I].En := 'very'
    Else If W = 'trop' Then FT[I].En := 'too'
    Else If W = 'bien' Then FT[I].En := 'well'
    Else If W = 'peu' Then FT[I].En := 'little';
  End;
  If (W = 'temps') And (FT[I].Cat = 'N') And (I + 2 <= FN) And (FT[I + 1].Cat =
      'V') And
     (FT[I + 1].Inf = 'être') And (FT[I + 2].Cat = 'A') And
     InList(FT[I + 2].En, 'nice bad good beautiful terrible cold hot warm cool')
         Then
    FT[I].En := 'weather';
End;

Procedure FrClassify;
Var
  I: Integer;
Begin
  FrMergeMultiword;
  For I := 1 To FN Do
    PickFrCat(I);
  For I := 1 To FN Do
    FrOverrides(I);
  { "nous" et "vous" apres un verbe en tete de phrase : complement (imperatif)
    ou sujet (inversion) }
End;

{ ------------------------------------------------------------------ }
{ ANGLAIS : formes                                                    }
{ ------------------------------------------------------------------ }

Function EnVowelStart(Const W: String): Boolean;
Begin
  EnVowelStart := False;
  If W = '' Then Exit;
  If InList(Copy(W, 1, 3), 'uni use eur one') Or StartsWith(W, 'one') Or
      StartsWith(W, 'euro') Then Exit;
  If StartsWith(W, 'hour') Or StartsWith(W, 'honest') Or StartsWith(W, 'heir')
      Then
  Begin
    EnVowelStart := True;
    Exit;
  End;
  EnVowelStart := W[1] In ['a', 'e', 'i', 'o', 'u'];
End;

Function EnArticle(Const W: String): String;
Begin
  If EnVowelStart(W) Then EnArticle := 'an' Else EnArticle := 'a';
End;

Function EnIrregular(Const Base: String; Kind1, Kind2: Char): String;
Var
  I: Integer;
Begin
  EnIrregular := '';
  For I := 1 To EnglishCount Do
    If (English[I].Base = Base) And ((English[I].Kind = Kind1) Or
        (English[I].Kind = Kind2)) Then
    Begin
      EnIrregular := English[I].Form;
      Exit;
    End;
End;

Function IsVowelCh(C: Char): Boolean;
Begin
  IsVowelCh := C In ['a', 'e', 'i', 'o', 'u'];
End;

Function EnVowelGroups(Const W: String): Integer;
Var
  I, N: Integer;
  Prev: Boolean;
Begin
  N := 0;
  Prev := False;
  For I := 1 To Length(W) Do
  Begin
    If IsVowelCh(W[I]) Or ((W[I] = 'y') And (I > 1)) Then
    Begin
      If Not Prev Then Inc(N);
      Prev := True;
    End
    Else
      Prev := False;
  End;
  EnVowelGroups := N;
End;

{ Redouble la consonne finale (stop -> stopped, run -> running). }
Function EnDoubles(Const W: String): Boolean;
Var
  L: Integer;
Begin
  L := Length(W);
  EnDoubles := False;
  If L < 3 Then Exit;
  If (Not IsVowelCh(W[L])) And (Not (W[L] In ['w', 'x', 'y'])) And IsVowelCh(W[L
      - 1]) And
     (Not IsVowelCh(W[L - 2])) And (EnVowelGroups(W) = 1) Then
    EnDoubles := True;
End;

Function EnPast(Const Base: String): String;
Var
  R: String;
  L: Integer;
Begin
  If Base = 'be' Then begin EnPast := 'was'; Exit; End;
  If Base = 'have' Then begin EnPast := 'had'; Exit; End;
  If Base = 'do' Then begin EnPast := 'did'; Exit; End;
  If Base = 'can' Then begin EnPast := 'could'; Exit; End;
  If Base = 'must' Then begin EnPast := 'had to'; Exit; End;
  R := EnIrregular(Base, 'p', 'b');
  If R <> '' Then
  Begin
    EnPast := R;
    Exit;
  End;
  L := Length(Base);
  If EndsWith(Base, 'e') Then
    EnPast := Base + 'd'
  Else If EndsWith(Base, 'y') And (L > 1) And (Not IsVowelCh(Base[L - 1])) Then
    EnPast := Copy(Base, 1, L - 1) + 'ied'
  Else If EnDoubles(Base) Then
    EnPast := Base + Base[L] + 'ed'
  Else
    EnPast := Base + 'ed';
End;

Function EnParticiple(Const Base: String): String;
Var
  R: String;
Begin
  If Base = 'be' Then begin EnParticiple := 'been'; Exit; End;
  If Base = 'have' Then begin EnParticiple := 'had'; Exit; End;
  If Base = 'do' Then begin EnParticiple := 'done'; Exit; End;
  R := EnIrregular(Base, 'q', 'b');
  If R <> '' Then
    EnParticiple := R
  Else
    EnParticiple := EnPast(Base);
End;

Function EnThird(Const Base: String): String;
Var
  L: Integer;
Begin
  L := Length(Base);
  If Base = 'be' Then EnThird := 'is'
  Else If Base = 'have' Then EnThird := 'has'
  Else If Base = 'do' Then EnThird := 'does'
  Else If Base = 'go' Then EnThird := 'goes'
  Else If EndsWith(Base, 's') Or EndsWith(Base, 'x') Or EndsWith(Base, 'z') Or
      EndsWith(Base, 'ch') Or
          EndsWith(Base, 'sh') Then
    EnThird := Base + 'es'
  Else If EndsWith(Base, 'y') And (L > 1) And (Not IsVowelCh(Base[L - 1])) Then
    EnThird := Copy(Base, 1, L - 1) + 'ies'
  Else
    EnThird := Base + 's';
End;

Function EnIng(Const Base: String): String;
Var
  L: Integer;
Begin
  L := Length(Base);
  If Base = 'be' Then EnIng := 'being'
  Else If EndsWith(Base, 'ie') Then EnIng := Copy(Base, 1, L - 2) + 'ying'
  Else If EndsWith(Base, 'e') And (Not EndsWith(Base, 'ee')) Then EnIng :=
      Copy(Base, 1, L - 1) + 'ing'
  Else If EnDoubles(Base) Then EnIng := Base + Base[L] + 'ing'
  Else EnIng := Base + 'ing';
End;

Function EnPlural(Const N: String): String;
Var
  R: String;
  L: Integer;
Begin
  R := EnIrregular(N, 'n', 'n');
  If R <> '' Then
  Begin
    EnPlural := R;
    Exit;
  End;
  L := Length(N);
  If InList(N, 'fish sheep news money water bread information') Then
    EnPlural := N
  Else If EndsWith(N, 's') Or EndsWith(N, 'x') Or EndsWith(N, 'z') Or
      EndsWith(N, 'ch') Or
          EndsWith(N, 'sh') Then
    EnPlural := N + 'es'
  Else If EndsWith(N, 'y') And (L > 1) And (Not IsVowelCh(N[L - 1])) Then
    EnPlural := Copy(N, 1, L - 1) + 'ies'
  Else If InList(N, 'potato tomato hero') Then
    EnPlural := N + 'es'
  Else If InList(N, 'wolf leaf knife wife life half calf shelf thief loaf') Then
    EnPlural := Copy(N, 1, L - 1 - Ord(EndsWith(N, 'fe'))) + 'ves'
  Else
    EnPlural := N + 's';
End;

Function EnCap(Const W: String): String;
Begin
  If IsCalendar(W) And Not InList(W, 'may') Then
    EnCap := UpperFirst(W)
  Else If (W = 'may') Then
    EnCap := W
  Else
    EnCap := W;
End;

Function FrProperEn(Const W: String; Const Orig: String): String;
Var
  L: String;
Begin
  L := FrLower(W);
  If L = 'allemagne' Then FrProperEn := 'Germany'
  Else If L = 'angleterre' Then FrProperEn := 'England'
  Else If L = 'espagne' Then FrProperEn := 'Spain'
  Else If L = 'italie' Then FrProperEn := 'Italy'
  Else If L = 'suisse' Then FrProperEn := 'Switzerland'
  Else If L = 'belgique' Then FrProperEn := 'Belgium'
  Else If L = 'chine' Then FrProperEn := 'China'
  Else If L = 'japon' Then FrProperEn := 'Japan'
  Else If L = 'russie' Then FrProperEn := 'Russia'
  Else If L = 'brésil' Then FrProperEn := 'Brazil'
  Else If L = 'mexique' Then FrProperEn := 'Mexico'
  Else If L = 'inde' Then FrProperEn := 'India'
  Else If L = 'grèce' Then FrProperEn := 'Greece'
  Else If L = 'turquie' Then FrProperEn := 'Turkey'
  Else If L = 'égypte' Then FrProperEn := 'Egypt'
  Else If L = 'maroc' Then FrProperEn := 'Morocco'
  Else If L = 'algérie' Then FrProperEn := 'Algeria'
  Else If L = 'tunisie' Then FrProperEn := 'Tunisia'
  Else If L = 'pologne' Then FrProperEn := 'Poland'
  Else If L = 'suède' Then FrProperEn := 'Sweden'
  Else If L = 'norvège' Then FrProperEn := 'Norway'
  Else If L = 'danemark' Then FrProperEn := 'Denmark'
  Else If L = 'irlande' Then FrProperEn := 'Ireland'
  Else If L = 'écosse' Then FrProperEn := 'Scotland'
  Else If L = 'australie' Then FrProperEn := 'Australia'
  Else If L = 'états-unis' Then FrProperEn := 'the United States'
  Else If L = 'londres' Then FrProperEn := 'London'
  Else If L = 'bruxelles' Then FrProperEn := 'Brussels'
  Else If L = 'genève' Then FrProperEn := 'Geneva'
  Else If L = 'vienne' Then FrProperEn := 'Vienna'
  Else If L = 'lisbonne' Then FrProperEn := 'Lisbon'
  Else If L = 'moscou' Then FrProperEn := 'Moscow'
  Else If L = 'pékin' Then FrProperEn := 'Beijing'
  Else If L = 'montréal' Then FrProperEn := 'Montreal'
  Else If L = 'québec' Then FrProperEn := 'Quebec'
  Else If L = 'france' Then FrProperEn := 'France'
  Else FrProperEn := Orig;
End;

{ ------------------------------------------------------------------ }
{ FRANCAIS -> ANGLAIS : groupes nominaux et prepositionnels           }
{ ------------------------------------------------------------------ }

Type
  TFNP = Record
    Found, Proper, Plural, All: Boolean;
    Used: Integer;
    Kind: Char; { d defini, i indefini, p partitif, s possessif, t demonstratif,
        n nombre, q quantificateur, 0 aucun }
    DetTok, Head: Integer;
    Adjs: Array[1..4] Of Integer;
    AdjAdv: Array[1..4] Of String[10];
    AdjN: Integer;
    Text: String[60];     { nom propre }
    NumText: String[24];
    PartOf: String[70];
  End;

  TFCtx = Record
    VerbEn: String[30];
    BeLike, Neg, Motion, Generic, PersonSubj: Boolean;
    SubjPoss: String[6];
  End;

Function RenderPartFr(Const NP: TFNP): String; Forward;
Function TranslateRelative(A, B: Integer; Plu: Boolean): String; Forward;

Var
  EmbedFlag: Boolean;

Function InflectFirst(Const Phrase: String; Kind: Char): String;
Var
  P: Integer;
  Head, Tail: String;
Begin
  P := Pos(' ', Phrase);
  If P > 0 Then
  Begin
    Head := Copy(Phrase, 1, P - 1);
    Tail := Copy(Phrase, P, 100);
  End
  Else
  Begin
    Head := Phrase;
    Tail := '';
  End;
  Case Kind Of
    '3': Head := EnThird(Head);
    'p': Head := EnPast(Head);
    'q': Head := EnParticiple(Head);
    'g': Head := EnIng(Head);
  End;
  InflectFirst := Head + Tail;
End;

Function IsContainerNoun(Const En: String): Boolean;
Begin
  IsContainerNoun := InList(En,
      'bottle glass cup piece kilo liter slice plate bag box group lot');
End;

Function PossEn(Const W, SubjPoss: String): String;
Begin
  If InList(W, 'mon ma mes') Then PossEn := 'my'
  Else If InList(W, 'ton ta tes') Then PossEn := 'your'
  Else If InList(W, 'son sa ses') Then
  Begin
    If SubjPoss <> '' Then PossEn := SubjPoss Else PossEn := 'his';
  End
  Else If InList(W, 'notre nos') Then PossEn := 'our'
  Else If InList(W, 'votre vos') Then PossEn := 'your'
  Else PossEn := 'their';
End;

Function ObjPronEn(Const W: String): String;
Begin
  If W = 'me' Then ObjPronEn := 'me'
  Else If W = 'te' Then ObjPronEn := 'you'
  Else If W = 'nous' Then ObjPronEn := 'us'
  Else If W = 'vous' Then ObjPronEn := 'you'
  Else If W = 'le' Then ObjPronEn := 'him'
  Else If W = 'la' Then ObjPronEn := 'her'
  Else If W = 'les' Then ObjPronEn := 'them'
  Else If W = 'lui' Then ObjPronEn := 'him'
  Else If W = 'leur' Then ObjPronEn := 'them'
  Else If W = 'y' Then ObjPronEn := 'there'
  Else ObjPronEn := '';
End;

Function DisjEn(Const W: String): String;
Begin
  If W = 'moi' Then DisjEn := 'me'
  Else If W = 'toi' Then DisjEn := 'you'
  Else If W = 'lui' Then DisjEn := 'him'
  Else If W = 'elle' Then DisjEn := 'her'
  Else If W = 'nous' Then DisjEn := 'us'
  Else If W = 'vous' Then DisjEn := 'you'
  Else If (W = 'eux') Or (W = 'elles') Then DisjEn := 'them'
  Else If W = 'ils' Then DisjEn := 'them'
  Else DisjEn := 'oneself';
End;

Function SubjEn(Const W: String): String;
Begin
  If W = 'je' Then SubjEn := 'I'
  Else If (W = 'tu') Or (W = 'vous') Then SubjEn := 'you'
  Else If W = 'il' Then SubjEn := 'he'
  Else If W = 'elle' Then SubjEn := 'she'
  Else If (W = 'nous') Or (W = 'on') Then SubjEn := 'we'
  Else If (W = 'ils') Or (W = 'elles') Then SubjEn := 'they'
  Else If W = 'cela' Then SubjEn := 'that'
  Else If W = 'ceci' Then SubjEn := 'this'
  Else SubjEn := 'it';
End;

{ Personne grammaticale anglaise : 1 I, 2 you, 3 he/she/it, 4 we, 5 you (pl), 6
  they }
Function SubjPerson(Const W: String): Integer;
Begin
  If W = 'je' Then SubjPerson := 1
  Else If W = 'tu' Then SubjPerson := 2
  Else If (W = 'nous') Or (W = 'on') Then SubjPerson := 4
  Else If W = 'vous' Then SubjPerson := 5
  Else If (W = 'ils') Or (W = 'elles') Then SubjPerson := 6
  Else SubjPerson := 3;
End;

Function ParseNPFr(Start: Integer; Var NP: TFNP): Boolean;
Var
  I, K: Integer;
  W: String;
  NP2: TFNP;
  Pre: Boolean;
Begin
  FillChar(NP, SizeOf(NP), 0);
  NP.Kind := '0';
  ParseNPFr := False;
  I := Start;
  If I > FN Then Exit;
  If (FT[I].Cat = 'Q') And InList(FT[I].W, 'tout toute tous toutes') And (I <
      FN) And
     (FT[I + 1].Cat In ['D', 'S', 'T']) Then
  Begin
    NP.All := True;
    Inc(I);
  End;
  W := FT[I].W;
  Case FT[I].Cat Of
    'D':
    Begin
      NP.DetTok := I;
      If InList(W, 'un une') Then NP.Kind := 'i'
      Else If W = 'des' Then
      Begin
        NP.Kind := 'i';
        NP.Plural := True;
      End
      Else If W = 'du' Then NP.Kind := 'p'
      Else NP.Kind := 'd';
      Inc(I);
    End;
    'S':
    Begin
      NP.DetTok := I;
      NP.Kind := 's';
      Inc(I);
    End;
    'T':
    Begin
      NP.DetTok := I;
      NP.Kind := 't';
      Inc(I);
    End;
    'Q':
    Begin
      NP.DetTok := I;
      NP.Kind := 'q';
      Inc(I);
    End;
    'U':
    Begin
      NP.DetTok := I;
      NP.Kind := 'n';
      NP.NumText := FT[I].En;
      Inc(I);
    End;
  End;
  { adjectifs avant le nom }
  While I <= FN Do
  Begin
    If FT[I].Cat = 'A' Then
    Begin
      If NP.AdjN < 4 Then
      Begin
        Inc(NP.AdjN);
        NP.Adjs[NP.AdjN] := I;
      End;
      Inc(I);
    End
    Else If (FT[I].Cat = 'R') And (I < FN) And (FT[I + 1].Cat = 'A') And
        (FT[I].En <> '') Then
    Begin
      If NP.AdjN < 4 Then
      Begin
        Inc(NP.AdjN);
        NP.Adjs[NP.AdjN] := I + 1;
        NP.AdjAdv[NP.AdjN] := FT[I].En;
      End;
      Inc(I, 2);
    End
    Else
      Break;
  End;
  If (I <= FN) And (FT[I].Cat = 'N') Then
  Begin
    NP.Head := I;
    NP.Plural := FT[I].Plu;
    Inc(I);
    { adjectifs apres le nom }
    While I <= FN Do
    Begin
      If FT[I].Cat = 'A' Then
      Begin
        If NP.AdjN < 4 Then
        Begin
          Inc(NP.AdjN);
          NP.Adjs[NP.AdjN] := I;
        End;
        Inc(I);
      End
      Else If (FT[I].Cat = 'R') And (I < FN) And (FT[I + 1].Cat = 'A') And
          (FT[I].En <> '') And
              (FT[I].W <> 'plus') Then
      Begin
        If NP.AdjN < 4 Then
        Begin
          Inc(NP.AdjN);
          NP.Adjs[NP.AdjN] := I + 1;
          NP.AdjAdv[NP.AdjN] := FT[I].En;
        End;
        Inc(I, 2);
      End
      Else If (FT[I].Cat = 'C') And (FT[I].W = 'et') And (I < FN) And (FT[I +
          1].Cat = 'A') And
              (NP.AdjN > 0) Then
        Inc(I)
      Else
        Break;
    End;
    { bouteille de vin }
    If (I < FN) And (FT[I].W = 'de') And IsContainerNoun(FT[NP.Head].En) Then
    Begin
      If ParseNPFr(I + 1, NP2) Then
      Begin
        NP.PartOf := '';
        K := I + 1;
        NP.PartOf := 'of ' + RenderPartFr(NP2);
        I := I + 1 + NP2.Used;
      End;
    End;
  End
  Else If (I <= FN) And (FT[I].Cat = 'Z') Then
  Begin
    NP.Proper := True;
    While (I <= FN) And (FT[I].Cat = 'Z') Do
    Begin
      NP.Text := Join(NP.Text, FrProperEn(FT[I].W, FT[I].Orig));
      Inc(I);
    End;
  End
  Else If (NP.Kind = 'n') And (NP.AdjN = 0) Then
  Begin
    NP.Plural := NP.NumText <> 'one';
  End
  Else If (NP.Kind = 'q') And (NP.AdjN = 0) Then
  Begin
    { quantificateur seul : tous, plusieurs }
  End
  Else
    Exit;
  Pre := True;
  { accord du nombre selon le determinant }
  If NP.DetTok > 0 Then
  Begin
    If InList(FT[NP.DetTok].W, 'les des mes tes ses nos vos leurs ces') Then
        NP.Plural := True
    Else If InList(FT[NP.DetTok].W,
        'le la un une mon ma ton ta son sa notre votre leur ce cet cette du')
        Then
      NP.Plural := False;
    If (NP.Kind = 'n') And (NP.NumText <> 'one') Then NP.Plural := True;
    If (NP.Kind = 'q') And InList(FT[NP.DetTok].W,
        'tous toutes plusieurs quelques certains') Then NP.Plural := True;
  End;
  If Pre Then NP.Found := True;
  NP.Used := I - Start;
  ParseNPFr := True;
End;

Function AdjEnText(Tok: Integer; PersonHead: Boolean): String;
Var
  R: String;
Begin
  R := FT[Tok].En;
  If PersonHead And (R = 'big') Then R := 'tall';
  AdjEnText := R;
End;

Function NounEnText(Const NP: TFNP): String;
Var
  N: String;
Begin
  N := FT[NP.Head].En;
  If NP.Plural And ((Not IsMassNoun(N)) Or (NP.Kind = 'n')) Then N :=
      EnPlural(N);
  If IsCalendar(N) And (Not InList(N, 'may')) Then N := UpperFirst(N);
  NounEnText := N;
End;

Function AdjListText(Const NP: TFNP): String;
Var
  K: Integer;
  R, A: String;
  Person: Boolean;
Begin
  R := '';
  Person := (NP.Head > 0) And IsPersonEn(FT[NP.Head].En);
  For K := 1 To NP.AdjN Do
  Begin
    A := AdjEnText(NP.Adjs[K], Person);
    If NP.AdjAdv[K] <> '' Then A := NP.AdjAdv[K] + ' ' + A;
    R := Join(R, A);
  End;
  AdjListText := R;
End;

Function RenderPartFr(Const NP: TFNP): String;
Begin
  If NP.Proper Then
    RenderPartFr := NP.Text
  Else If NP.Head = 0 Then
    RenderPartFr := NP.NumText
  Else
    RenderPartFr := Join(AdjListText(NP), NounEnText(NP));
End;

Function RenderNPFr(Const NP: TFNP; Generic, Neg: Boolean; Const SubjPoss:
    String): String;
Var
  Det, Core, W: String;
Begin
  If NP.Proper Then
  Begin
    RenderNPFr := NP.Text;
    Exit;
  End;
  Core := RenderPartFr(NP);
  If NP.Head = 0 Then
  Begin
    If NP.Kind = 'q' Then
    Begin
      W := FT[NP.DetTok].W;
      If InList(W, 'tout tous toute toutes') Then Core := 'all'
      Else If W = 'plusieurs' Then Core := 'several'
      Else If W = 'quelques' Then Core := 'a few'
      Else If W = 'autre' Then Core := 'another'
      Else Core := FT[NP.DetTok].W;
    End;
    RenderNPFr := Core;
    Exit;
  End;
  Det := '';
  Case NP.Kind Of
    'd':
      If Generic Or IsLanguage(FT[NP.Head].En) Then Det := '' Else Det := 'the';
    'i':
    Begin
      If NP.Plural Then
      Begin
        If Neg Then Det := 'any';
      End
      Else
        Det := 'a';
    End;
    'p':
    Begin
      If Neg Then
      Begin
        If NP.Plural Or IsMassNoun(FT[NP.Head].En) Then Det := 'any' Else Det :=
            'a';
      End;
    End;
    's': Det := PossEn(FT[NP.DetTok].W, SubjPoss);
    't':
      If NP.Plural Then Det := 'these' Else Det := 'this';
    'n': Det := NP.NumText;
    'q':
    Begin
      W := FT[NP.DetTok].W;
      If InList(W, 'tout tous toute toutes') Then Det := 'all'
      Else If W = 'chaque' Then Det := 'each'
      Else If W = 'plusieurs' Then Det := 'several'
      Else If (W = 'quelques') Or (W = 'quelque') Then Det := 'some'
      Else If W = 'autre' Then
      Begin
        If NP.Plural Then Det := 'other' Else Det := 'another';
      End
      Else If W = 'autres' Then Det := 'other'
      Else If W = 'même' Then Det := 'the same'
      Else Det := 'some';
    End;
  End;
  If NP.All Then Det := Join('all', Det);
  If Det = 'a' Then
    Det := EnArticle(Join(AdjListText(NP), NounEnText(NP)));
  RenderNPFr := Join(Det, Join(Core, NP.PartOf));
End;

{ Un infinitif francais comme complement : "to eat" ou "eating". }
Function InfinitiveEn(Tok: Integer; Gerund: Boolean): String;
Begin
  If Gerund Then
    InfinitiveEn := InflectFirst(FT[Tok].En, 'g')
  Else
    InfinitiveEn := 'to ' + FT[Tok].En;
End;

Function IsOriginVerb(Const V: String): Boolean;
Begin
  IsOriginVerb := InList(V, 'come leave return arrive be');
End;

Function IsTransportNoun(Const En: String): Boolean;
Begin
  IsTransportNoun := InList(En, 'car bus train plane bike boat');
End;

{ Groupe prepositionnel. I pointe sur la preposition ; renvoie le texte anglais
  et avance I. }
Function TranslatePPFr(Var I: Integer; Last: Integer; Const Ctx: TFCtx): String;
Var
  W, Prep, Obj: String;
  NP: TFNP;
  Cat: Char;
  J: Integer;
  TheArt: Boolean;
Begin
  W := FT[I].W;
  Cat := FT[I].Cat;
  J := I + 1;
  Prep := FT[I].En;
  Obj := '';
  TheArt := False;
  TranslatePPFr := '';
  { preposition suivie d'un infinitif }
  If (J <= Last) And (FT[J].Cat = 'V') And (FT[J].Tns = 'I') Then
  Begin
    If (W = 'pour') Then
      TranslatePPFr := 'to ' + FT[J].En
    Else If (W = 'sans') Then
      TranslatePPFr := 'without ' + InflectFirst(FT[J].En, 'g')
    Else If Prep <> '' Then
      TranslatePPFr := Prep + ' ' + InflectFirst(FT[J].En, 'g')
    Else
      TranslatePPFr := 'to ' + FT[J].En;
    I := J + 1;
    Exit;
  End;
  { l'heure : a 7 heures }
  If (W = 'à') And (J + 1 <= Last) And (FT[J].Cat = 'U') And (FT[J + 1].L > 0)
      And
     (FT[J + 1].Cat = 'N') Then
    If Lex[FT[J + 1].L].En = 'hour' Then
    Begin
      TranslatePPFr := 'at ' + FT[J].En + ' o''clock';
      I := J + 2;
      Exit;
    End;
  { choix de la preposition }
  If (Cat = 'a') And (Ctx.VerbEn = 'prefer') Then
    Prep := 'to'
  Else If Cat = 'a' Then
  Begin
    If (J <= Last) And (FT[J].Cat = 'Z') Then
    Begin
      If Ctx.Motion Then Prep := 'to' Else Prep := 'in';
    End
    Else
    Begin
      If Ctx.Motion Then Prep := 'to' Else Prep := 'at';
      TheArt := True;
    End;
  End
  Else If Cat = 'd' Then
  Begin
    If IsOriginVerb(Ctx.VerbEn) Then Prep := 'from' Else Prep := 'of';
    If Not ((J <= Last) And (FT[J].Cat = 'Z')) Then TheArt := True;
  End
  Else If W = 'à' Then
  Begin
    If (J <= Last) And (FT[J].Cat = 'Z') Then
    Begin
      If Ctx.Motion Then Prep := 'to' Else Prep := 'in';
    End
    Else If (J <= Last) And (FT[J].Cat In ['B', 'O']) Then
      Prep := 'to'
    Else If InList(Ctx.VerbEn,
        'give send show tell say write bring teach ask sell lend offer speak t'+
        'alk prefer') Then
      Prep := 'to'
    Else If Ctx.Motion Then
      Prep := 'to'
    Else
      Prep := 'at';
  End
  Else If W = 'de' Then
  Begin
    If IsOriginVerb(Ctx.VerbEn) Then Prep := 'from' Else Prep := 'of';
  End
  Else If W = 'en' Then
  Begin
    If (J <= Last) And (FT[J].Cat = 'Z') Then
    Begin
      If Ctx.Motion Then Prep := 'to' Else Prep := 'in';
    End
    Else If (J <= Last) And (FT[J].Cat = 'N') And (FT[J].L > 0) And
        IsTransportNoun(Lex[FT[J].L].En) Then
      Prep := 'by'
    Else
      Prep := 'in';
  End;
  { complement }
  If J > Last Then
  Begin
    TranslatePPFr := Prep;
    I := J;
    Exit;
  End;
  If FT[J].Cat = 'B' Then
  Begin
    TranslatePPFr := Join(Prep, DisjEn(FT[J].W));
    I := J + 1;
    Exit;
  End;
  If ParseNPFr(J, NP) Then
  Begin
    If TheArt And (NP.Kind = '0') And (Not NP.Proper) Then NP.Kind := 'd';
    If (NP.Kind = 'd') And (NP.Head > 0) And (Not NP.Plural) And (NP.AdjN = 0)
        And
       InList(Prep, 'to at in') And InList(FT[NP.Head].En,
           'school church bed work prison university') Then
      NP.Kind := '0';
    Obj := RenderNPFr(NP, False, False, Ctx.SubjPoss);
    TranslatePPFr := Join(Prep, Obj);
    I := J + NP.Used;
    Exit;
  End;
  TranslatePPFr := Prep;
  I := J;
End;

{ ------------------------------------------------------------------ }
{ Complements du verbe                                                }
{ ------------------------------------------------------------------ }

Function IsFreqAdverb(Const En: String): Boolean;
Begin
  IsFreqAdverb := InList(En,
      'always never often sometimes usually already just');
End;

Function TranslateClauseEn(A, B: Integer; IsQ: Boolean; Const PreSubj: String;
    PrePlu: Boolean): String; Forward;

Function TailEn(First, Last: Integer; Const Ctx: TFCtx; Var Freq: String):
    String;
Var
  I: Integer;
  R, Obj: String;
  NP: TFNP;
  Gerund: Boolean;
  C2: TFCtx;
Begin
  R := '';
  I := First;
  While I <= Last Do
  Begin
    Case FT[I].Cat Of
      'D', 'S', 'T', 'Q', 'U', 'N', 'Z':
      Begin
        If ParseNPFr(I, NP) Then
        Begin
          If Ctx.BeLike And (NP.Kind = '0') And (Not NP.Plural) And (Not
              NP.Proper) And
             (NP.Head > 0) And (NP.AdjN = 0) Then
            If IsProfession(FT[NP.Head].En) Then NP.Kind := 'i';
          Obj := RenderNPFr(NP, Ctx.Generic, Ctx.Neg, Ctx.SubjPoss);
          R := Join(R, Obj);
          Inc(I, NP.Used);
        End
        Else
        Begin
          R := Join(R, FT[I].Orig);
          Inc(I);
        End;
      End;
      'A':
      Begin
        If (Not Ctx.BeLike) And ParseNPFr(I, NP) Then
        Begin
          Obj := RenderNPFr(NP, Ctx.Generic, Ctx.Neg, Ctx.SubjPoss);
          R := Join(R, Obj);
          Inc(I, NP.Used);
        End
        Else
        Begin
          R := Join(R, AdjEnText(I, Ctx.PersonSubj));
          Inc(I);
        End;
      End;
      'R':
      Begin
        If (I < Last) And (FT[I + 1].Cat = 'A') And (FT[I].En <> '') And
            Ctx.BeLike Then
        Begin
          R := Join(R, Join(FT[I].En, AdjEnText(I + 1, Ctx.PersonSubj)));
          Inc(I, 2);
        End
        Else If IsFreqAdverb(FT[I].En) Then
        Begin
          Freq := Join(Freq, FT[I].En);
          Inc(I);
        End
        Else
        Begin
          If FT[I].En <> '' Then R := Join(R, FT[I].En) Else R := Join(R,
              FT[I].Orig);
          Inc(I);
        End;
      End;
      'E', 'a', 'd':
      Begin
        If (FT[I].W = 'de') And (I < Last) And (FT[I + 1].Cat = 'D') And
           (I > 1) And (FT[I - 1].Cat In ['V', 'g', 'R']) Then
        Begin
          { partitif : de la, de l' }
          If ParseNPFr(I + 1, NP) Then
          Begin
            NP.Kind := 'p';
            R := Join(R, RenderNPFr(NP, Ctx.Generic, Ctx.Neg, Ctx.SubjPoss));
            Inc(I, 1 + NP.Used);
          End
          Else
            R := Join(R, TranslatePPFr(I, Last, Ctx));
        End
        Else If (FT[I].W = 'de') And (I < Last) And (FT[I + 1].Cat In ['N',
            'A']) And
                (I > 1) And (FT[I - 1].Cat In ['g', 'V']) Then
        Begin
          { pas de N ; de + nom apres un verbe }
          If ParseNPFr(I + 1, NP) Then
          Begin
            NP.Kind := 'p';
            R := Join(R, RenderNPFr(NP, Ctx.Generic, Ctx.Neg, Ctx.SubjPoss));
            Inc(I, 1 + NP.Used);
          End
          Else
            R := Join(R, TranslatePPFr(I, Last, Ctx));
        End
        Else If InList(FT[I].W, 'à de') And (I + 2 <= Last) And (FT[I + 1].Cat
            In ['O', 'B', 'P']) And
                (FT[I + 2].Cat = 'V') And (FT[I + 2].Tns = 'I') And
                    (ObjPronEn(FT[I + 1].W) <> '') Then
        Begin
          { de vous voir : pronom complement avant l'infinitif }
          Gerund := InList(Ctx.VerbEn, 'stop finish enjoy');
          C2 := Ctx;
          C2.VerbEn := FT[I + 2].En;
          C2.BeLike := False;
          Obj := Join(InfinitiveEn(I + 2, Gerund), ObjPronEn(FT[I + 1].W));
          R := Join(R, Join(Obj, TailEn(I + 3, Last, C2, Freq)));
          I := Last + 1;
        End
        Else If InList(FT[I].W, 'à de') And (I < Last) And (FT[I + 1].Cat =
            'V') And
                (FT[I + 1].Tns = 'I') Then
        Begin
          { a / de + infinitif }
          Gerund := InList(Ctx.VerbEn, 'stop finish enjoy');
          C2 := Ctx;
          C2.VerbEn := FT[I + 1].En;
          C2.BeLike := False;
          Obj := InfinitiveEn(I + 1, Gerund);
          R := Join(R, Join(Obj, TailEn(I + 2, Last, C2, Freq)));
          I := Last + 1;
        End
        Else
          R := Join(R, TranslatePPFr(I, Last, Ctx));
      End;
      'V':
      Begin
        If FT[I].Tns = 'I' Then
        Begin
          Gerund := InList(Ctx.VerbEn, 'stop finish enjoy');
          C2 := Ctx;
          C2.VerbEn := FT[I].En;
          C2.BeLike := False;
          R := Join(R, Join(InfinitiveEn(I, Gerund), TailEn(I + 1, Last, C2,
              Freq)));
          I := Last + 1;
        End
        Else
        Begin
          R := Join(R, FT[I].En);
          Inc(I);
        End;
      End;
      'B':
      Begin
        R := Join(R, DisjEn(FT[I].W));
        Inc(I);
      End;
      'O', 'P':
      Begin
        If ObjPronEn(FT[I].W) <> '' Then
          R := Join(R, ObjPronEn(FT[I].W))
        Else
          R := Join(R, SubjEn(FT[I].W));
        Inc(I);
      End;
      'g':
      Begin
        If FT[I].W = 'rien' Then R := Join(R, 'anything')
        Else If FT[I].W = 'personne' Then R := Join(R, 'anyone')
        Else If FT[I].W = 'jamais' Then Freq := Join(Freq, 'never');
        Inc(I);
      End;
      'C':
      Begin
        If FT[I].En <> '' Then R := Join(R, FT[I].En) Else R := Join(R,
            FT[I].Orig);
        Inc(I);
      End;
      'J':
      Begin
        R := Join(R, LowerFirst(FT[I].En));
        Inc(I);
      End;
      ',':
      Begin
        R := R + ',';
        Inc(I);
      End;
      'W':
      Begin
        { question indirecte : ou est la gare }
        If (FT[I].W <> 'qui') And (FT[I].W <> 'que') And (I < Last) And (I > 1)
            And
           (FT[I - 1].Cat In ['g', 'V']) Then
        Begin
          EmbedFlag := True;
          R := Join(R, TranslateClauseEn(I, Last, False, '', False));
          EmbedFlag := False;
          I := Last + 1;
        End
        { proposition relative : qui + verbe }
        Else If (FT[I].W = 'qui') And (I < Last) And (FT[I + 1].Cat In ['V',
            'O', 'G']) And (I > First) Then
        Begin
          If (FT[I - 1].Cat = 'N') And (FT[I - 1].L > 0) And IsPersonEn(Lex[FT[I
              - 1].L].En) Then
            R := Join(R, 'who')
          Else
            R := Join(R, 'that');
          R := Join(R, TranslateRelative(I + 1, Last, FT[I - 1].Plu));
          I := Last + 1;
        End
        Else
        Begin
          R := Join(R, FT[I].Orig);
          Inc(I);
        End;
      End;
    Else
      R := Join(R, FT[I].Orig);
      Inc(I);
    End;
  End;
  TailEn := R;
End;

{ ------------------------------------------------------------------ }
{ FRANCAIS -> ANGLAIS : phrases toutes faites                         }
{ ------------------------------------------------------------------ }

Const
  PhrasesFr: Array[0..43] Of String = (
    'comment allez vous|How are you',
    'comment vas tu|How are you',
    'comment ça va|How is it going',
    'quel âge avez vous|How old are you',
    'quel âge as tu|How old are you',
    'comment vous appelez vous|What is your name',
    'comment t''appelles tu|What is your name',
    'comment tu t''appelles|What is your name',
    'd''où venez vous|Where are you from',
    'd''où viens tu|Where are you from',
    'quelle heure est il|What time is it',
    'enchanté|Nice to meet you',
    'de rien|You are welcome',
    'pas de problème|No problem',
    'bonne chance|Good luck',
    'joyeux anniversaire|Happy birthday',
    'joyeux noël|Merry Christmas',
    'bonne année|Happy New Year',
    'bonne journée|Have a nice day',
    'à demain|See you tomorrow',
    'à plus tard|See you later',
    'à bientôt|See you soon',
    'tu me manques|I miss you',
    'je t''aime|I love you',
    'je suis désolé|I am sorry',
    'je ne sais pas|I do not know',
    'je ne comprends pas|I do not understand',
    'je vais bien|I am fine',
    'combien ça coûte|How much does it cost',
    'combien coûte ceci|How much is this',
    'où sont les toilettes|Where is the bathroom',
    'où est la salle de bain|Where is the bathroom',
    'qu''est ce que c''est|What is this',
    'que se passe t il|What is going on',
    'merci beaucoup|Thank you very much',
    'bonne nuit|Good night',
    'je ne parle pas français|I do not speak French',
    'parlez vous anglais|Do you speak English',
    'je suis perdu|I am lost',
    'je n''ai pas le temps|I do not have time',
    'excusez moi|Excuse me',
    'bon appétit|Enjoy your meal',
    'à votre santé|Cheers',
    'ça va bien|I am fine'
  );

{ Cle d'une suite de jetons : mots en minuscules separes par une espace (sans
  espace apres une elision). }
Function FrPhraseKey(A, B: Integer): String;
Var
  K: Integer;
  R: String;
Begin
  R := '';
  For K := A To B Do
  Begin
    If FT[K].Punct Then
    Begin
      FrPhraseKey := '#';
      Exit;
    End;
    If R = '' Then
      R := FrLower(FT[K].Orig)
    Else If (FT[K - 1].Elided) And (FT[K - 1].Orig <> '') And
            (FT[K - 1].Orig[Length(FT[K - 1].Orig)] = '''') Then
      R := R + FrLower(FT[K].Orig)
    Else
      R := R + ' ' + FrLower(FT[K].Orig);
  End;
  FrPhraseKey := R;
End;

Function FindPhraseFr(Const Key: String): String;
Var
  N: Integer;
  Pair, K: String;
Begin
  FindPhraseFr := '';
  If Key = '#' Then Exit;
  For N := 0 To High(PhrasesFr) Do
  Begin
    Pair := PhrasesFr[N];
    K := CopyUntil(Pair, '|');
    If K = Key Then
    Begin
      FindPhraseFr := Pair;
      Exit;
    End;
  End;
End;

{ ------------------------------------------------------------------ }
{ FRANCAIS -> ANGLAIS : sujet, groupe verbal et proposition           }
{ ------------------------------------------------------------------ }

Type
  TFSubj = Record
    Found, IsPron, IsWh, PersonSubj, Plural: Boolean;
    Used, Person: Integer;
    Text: String[70];
    Poss: String[6];
    Pron: String[6];
  End;

  TFVG = Record
    Mode: Char; { s simple, c compose, p passif, m modal, g aller + inf, j venir
        de, b etre, h avoir, t il y a }
    V1, Main, Next: Integer;
    Tns: Char;
    Perf, Neg, Refl, NeSeen: Boolean;
    Freq: String[40];
    Clit: String[40];
    MainEn: String[34];
    Modal: String[8];
    Pred: String[60];
    CliW: String[8];
    Rain: Boolean;
  End;

Function ClauseHasVerbFr(A, B: Integer): Boolean;
Var
  I: Integer;
Begin
  ClauseHasVerbFr := False;
  For I := A To B Do
    If (FT[I].Cat = 'V') And (FT[I].Tns In ['p', 'f', 'c', 'i', 'M']) Then
        ClauseHasVerbFr := True;
End;

Function ParseSubjFr(Start: Integer; Var S: TFSubj): Boolean;
Var
  NP: TFNP;
  W: String;
Begin
  FillChar(S, SizeOf(S), 0);
  ParseSubjFr := False;
  If Start > FN Then Exit;
  W := FT[Start].W;
  If FT[Start].Cat = 'P' Then
  Begin
    S.Found := True;
    S.IsPron := True;
    S.Used := 1;
    S.Pron := W;
    S.Text := SubjEn(W);
    S.Person := SubjPerson(W);
    S.PersonSubj := Not InList(W, 'ce ça ceci cela');
    S.Plural := S.Person >= 4;
    If W = 'je' Then S.Poss := 'my'
    Else If (W = 'tu') Or (W = 'vous') Then S.Poss := 'your'
    Else If W = 'il' Then S.Poss := 'his'
    Else If W = 'elle' Then S.Poss := 'her'
    Else If (W = 'nous') Or (W = 'on') Then S.Poss := 'our'
    Else If (W = 'ils') Or (W = 'elles') Then S.Poss := 'their'
    Else S.Poss := 'its';
    ParseSubjFr := True;
    Exit;
  End;
  If (FT[Start].Cat = 'W') And (W = 'qui') Then
  Begin
    S.Found := True;
    S.IsWh := True;
    S.Used := 1;
    S.Text := 'who';
    S.Person := 3;
    ParseSubjFr := True;
    Exit;
  End;
  If ParseNPFr(Start, NP) Then
  Begin
    S.Found := True;
    S.Used := NP.Used;
    S.Plural := NP.Plural;
    If NP.Plural Then S.Person := 6 Else S.Person := 3;
    S.Text := RenderNPFr(NP, False, False, '');
    S.PersonSubj := (NP.Head > 0) And IsPersonEn(FT[NP.Head].En);
    If NP.Plural Then S.Poss := 'their'
    Else If S.PersonSubj Then S.Poss := 'his'
    Else S.Poss := 'its';
    ParseSubjFr := True;
  End;
End;

Function BeFormEn(Sp: Integer; Past: Boolean): String;
Begin
  If Past Then
  Begin
    If (Sp = 1) Or (Sp = 3) Then BeFormEn := 'was' Else BeFormEn := 'were';
  End
  Else If Sp = 1 Then BeFormEn := 'am'
  Else If Sp = 3 Then BeFormEn := 'is'
  Else BeFormEn := 'are';
End;

Function HaveFormEn(Sp: Integer; Past: Boolean): String;
Begin
  If Past Then HaveFormEn := 'had'
  Else If Sp = 3 Then HaveFormEn := 'has'
  Else HaveFormEn := 'have';
End;

Function DoFormEn(Sp: Integer; T: Char): String;
Begin
  If T = 'i' Then DoFormEn := 'did'
  Else If Sp = 3 Then DoFormEn := 'does'
  Else DoFormEn := 'do';
End;

Function NegContract(Const Aux: String): String;
Begin
  If Aux = 'am' Then NegContract := 'am not'
  Else If Aux = 'will' Then NegContract := 'won''t'
  Else If Aux = 'can' Then NegContract := 'can''t'
  Else If Aux = 'must' Then NegContract := 'mustn''t'
  Else NegContract := Aux + 'n''t';
End;

Function IsMotionEn(Const V: String): Boolean;
Begin
  IsMotionEn := InList(V,
      'go come return travel arrive leave walk run fly drive');
End;

{ Analyse du groupe verbal a partir de Start (negation et pronoms compris). }
Procedure ScanVG(Start, Last: Integer; Var G: TFVG);
Var
  I, J, K: Integer;
  W: String;
  Inf1: String;
  Done: Boolean;
  There: Boolean;
Begin
  FillChar(G, SizeOf(G), 0);
  G.Mode := 's';
  G.Tns := 'p';
  There := False;
  I := Start;
  Done := False;
  While (I <= Last) And Not Done Do
  Begin
    Case FT[I].Cat Of
      'G':
      Begin
        G.NeSeen := True;
        Inc(I);
      End;
      'O':
      Begin
        W := FT[I].W;
        If W = 'se' Then G.Refl := True
        Else If W = 'y' Then There := True
        Else If W = 'en' Then
        Begin
        End
        Else
        Begin
          G.Clit := Join(G.Clit, ObjPronEn(W));
          G.CliW := W;
        End;
        Inc(I);
      End;
      'R':
      Begin
        If (I < Last) And (FT[I + 1].Cat In ['V', 'O', 'G']) Then
        Begin
          If IsFreqAdverb(FT[I].En) Then G.Freq := Join(G.Freq, FT[I].En) Else
              G.Freq := Join(G.Freq, FT[I].En);
          Inc(I);
        End
        Else
          Done := True;
      End;
    Else
      Done := True;
    End;
  End;
  If (I > Last) Or (FT[I].Cat <> 'V') Then
  Begin
    G.V1 := 0;
    G.Next := I;
    Exit;
  End;
  G.V1 := I;
  G.Tns := FT[I].Tns;
  If G.Tns = 'M' Then G.Tns := 'p';
  If Not (G.Tns In ['p', 'i', 'f', 'c']) Then G.Tns := 'p';
  Inf1 := FT[I].Inf;
  G.MainEn := FT[I].En;
  G.Main := I;
  { negation et adverbes apres le verbe fini }
  J := I + 1;
  While J <= Last Do
  Begin
    If FT[J].Cat = 'g' Then
    Begin
      If FT[J].W = 'pas' Then
      Begin
        G.Neg := True;
        Inc(J);
      End
      Else If FT[J].W = 'jamais' Then
      Begin
        G.Freq := Join(G.Freq, 'never');
        Inc(J);
      End
      Else If FT[J].W = 'guère' Then
      Begin
        G.Freq := Join(G.Freq, 'hardly');
        Inc(J);
      End
      Else
        Break;
    End
    Else If (FT[J].Cat = 'R') And (FT[J].W = 'plus') And G.NeSeen Then
    Begin
      G.Freq := Join(G.Freq, 'no longer');
      Inc(J);
    End
    Else If (FT[J].Cat = 'R') And (J < Last) And (FT[J + 1].Cat = 'V') And
            (FT[J + 1].Tns In ['P', 'I']) Then
    Begin
      G.Freq := Join(G.Freq, FT[J].En);
      Inc(J);
    End
    Else If (FT[J].Cat = 'R') And IsFreqAdverb(FT[J].En) Then
    Begin
      G.Freq := Join(G.Freq, FT[J].En);
      Inc(J);
    End
    Else If (FT[J].Cat = 'O') And (J < Last) And (FT[J + 1].Cat In ['V', 'O'])
        Then
    Begin
      If FT[J].W <> 'se' Then G.Clit := Join(G.Clit, ObjPronEn(FT[J].W));
      Inc(J);
    End
    Else
      Break;
  End;
  G.Next := J;
  If G.NeSeen And (J <= Last) Then
    If (FT[J].Cat = 'g') And ((FT[J].W = 'rien') Or (FT[J].W = 'personne')) Then
        G.Neg := True;
  { il y a }
  If There And (Inf1 = 'avoir') And (G.Tns In ['p', 'i', 'f', 'c']) Then
  Begin
    G.Mode := 't';
    G.MainEn := 'be';
    Exit;
  End;
  If Inf1 = 'être' Then
  Begin
    G.MainEn := 'be';
    If (J <= Last) And (FT[J].Cat = 'V') And (FT[J].Tns = 'P') Then
    Begin
      If (FT[J].L > 0) And (Lex[FT[J].L].Aux = 'e') Then
      Begin
        G.Mode := 'c';
        G.Main := J;
        G.MainEn := FT[J].En;
        G.Next := J + 1;
      End
      Else
      Begin
        G.Mode := 'p';
        G.Main := J;
        G.MainEn := FT[J].En;
        G.Next := J + 1;
      End;
    End
    Else
      G.Mode := 'b';
    Exit;
  End;
  If Inf1 = 'avoir' Then
  Begin
    G.MainEn := 'have';
    If (J <= Last) And (FT[J].Cat = 'V') And (FT[J].Tns = 'P') Then
    Begin
      If FT[J].Inf = 'être' Then
      Begin
        G.Mode := 'b';
        G.MainEn := 'be';
        G.Next := J + 1;
      End
      Else
      Begin
        G.Mode := 'c';
        G.Main := J;
        G.MainEn := FT[J].En;
        G.Next := J + 1;
        If (G.Tns = 'p') And (Pos('already', G.Freq) + Pos('never', G.Freq) +
            Pos('just', G.Freq) > 0) Then
          G.Perf := True;
      End;
    End
    Else
      G.Mode := 'h';
    Exit;
  End;
  { verbes + infinitif }
  If (J <= Last) And (FT[J].Cat = 'V') And (FT[J].Tns = 'I') Then
  Begin
    If (Inf1 = 'pouvoir') Or (Inf1 = 'devoir') Or (Inf1 = 'vouloir') Or (Inf1 =
        'savoir') Then
    Begin
      G.Mode := 'm';
      G.Modal := Inf1;
      G.Main := J;
      G.MainEn := FT[J].En;
      G.Next := J + 1;
    End
    Else If Inf1 = 'aller' Then
    Begin
      G.Mode := 'g';
      G.Main := J;
      G.MainEn := FT[J].En;
      G.Next := J + 1;
    End;
    Exit;
  End;
  If (Inf1 = 'venir') And (J < Last) And (FT[J].W = 'de') And (FT[J + 1].Cat =
      'V') And
     (FT[J + 1].Tns = 'I') And (G.Tns In ['p', 'i']) Then
  Begin
    G.Mode := 'j';
    G.Main := J + 1;
    G.MainEn := FT[J + 1].En;
    G.Next := J + 2;
    Exit;
  End;
  K := 0;
End;

{ Construit l'auxiliaire (ou le verbe fini) anglais. Renvoie Aux, ou
  LexBase/LexTns si le verbe est lexical. }
Procedure BuildEn(Const G: TFVG; Sp: Integer; Var Aux, Rest, LexBase, Post:
    String; Var LexTns: Char);
Var
  Past: Boolean;
  Base: String;
Begin
  Aux := '';
  Rest := '';
  LexBase := '';
  Post := '';
  LexTns := 'p';
  Past := G.Tns = 'i';
  Base := G.MainEn;
  Case G.Mode Of
    'b', 't':
    Begin
      Case G.Tns Of
        'f': begin Aux := 'will'; Rest := 'be'; end;
        'c': begin Aux := 'would'; Rest := 'be'; end;
      Else
        If G.Perf Then
        Begin
          Aux := HaveFormEn(Sp, False);
          Rest := 'been';
        End
        Else
          Aux := BeFormEn(Sp, Past);
      End;
    End;
    'h':
    Begin
      Case G.Tns Of
        'f': begin Aux := 'will'; Rest := 'have'; end;
        'c': begin Aux := 'would'; Rest := 'have'; end;
      Else
        LexBase := 'have';
        If Past Then LexTns := 'i';
      End;
    End;
    's':
    Begin
      Case G.Tns Of
        'f': begin Aux := 'will'; Rest := Base; end;
        'c': begin Aux := 'would'; Rest := Base; end;
      Else
        If G.Rain And (G.Tns = 'p') Then
        Begin
          Aux := BeFormEn(Sp, False);
          Rest := EnIng(Base);
        End
        Else
        Begin
          LexBase := Base;
          If Past Then LexTns := 'i';
        End;
      End;
    End;
    'c':
    Begin
      If G.Perf Then
      Begin
        Aux := HaveFormEn(Sp, False);
        Rest := InflectFirst(Base, 'q');
      End
      Else Case G.Tns Of
        'f': begin Aux := 'will'; Rest := 'have ' + InflectFirst(Base, 'q');
            end;
        'c': begin Aux := 'would'; Rest := 'have ' + InflectFirst(Base, 'q');
            end;
        'i': begin Aux := 'had'; Rest := InflectFirst(Base, 'q'); end;
      Else
        LexBase := Base;
        LexTns := 'i';
      End;
    End;
    'p':
    Begin
      Case G.Tns Of
        'f': begin Aux := 'will'; Rest := 'be ' + InflectFirst(Base, 'q'); end;
        'c': begin Aux := 'would'; Rest := 'be ' + InflectFirst(Base, 'q'); end;
      Else
        Aux := BeFormEn(Sp, Past);
        Rest := InflectFirst(Base, 'q');
      End;
    End;
    'g':
    Begin
      Case G.Tns Of
        'f': begin Aux := 'will'; Rest := 'be going to ' + Base; end;
        'c': begin Aux := 'would'; Rest := 'be going to ' + Base; end;
      Else
        Aux := BeFormEn(Sp, Past);
        Rest := 'going to ' + Base;
      End;
    End;
    'j':
    Begin
      Aux := HaveFormEn(Sp, Past);
      Rest := 'just ' + InflectFirst(Base, 'q');
    End;
    'm':
    Begin
      If (G.Modal = 'pouvoir') Or (G.Modal = 'savoir') Then
      Begin
        Case G.Tns Of
          'f': begin Aux := 'will'; Rest := 'be able to ' + Base; end;
          'i', 'c': begin Aux := 'could'; Rest := Base; end;
        Else
          Aux := 'can';
          Rest := Base;
        End;
      End
      Else If G.Modal = 'devoir' Then
      Begin
        Case G.Tns Of
          'f': begin Aux := 'will'; Rest := 'have to ' + Base; end;
          'c': begin Aux := 'should'; Rest := Base; end;
          'i':
          Begin
            LexBase := 'have';
            LexTns := 'i';
            Post := 'to ' + Base;
          End;
        Else
          Aux := 'must';
          Rest := Base;
        End;
      End
      Else
      Begin
        { vouloir }
        Case G.Tns Of
          'f': begin Aux := 'will'; Rest := 'want to ' + Base; end;
          'c': begin Aux := 'would'; Rest := 'like to ' + Base; end;
        Else
          LexBase := 'want';
          If Past Then LexTns := 'i';
          Post := 'to ' + Base;
        End;
      End;
    End;
  End;
End;

Function FiniteEn(Const Base: String; T: Char; Sp: Integer): String;
Begin
  If T = 'i' Then FiniteEn := InflectFirst(Base, 'p')
  Else If Sp = 3 Then FiniteEn := InflectFirst(Base, '3')
  Else FiniteEn := Base;
End;

Function WhEnOf(Const W: String): String;
Begin
  If W = 'où' Then WhEnOf := 'where'
  Else If W = 'quand' Then WhEnOf := 'when'
  Else If W = 'pourquoi' Then WhEnOf := 'why'
  Else If W = 'comment' Then WhEnOf := 'how'
  Else If W = 'qui' Then WhEnOf := 'who'
  Else If W = 'combien' Then WhEnOf := 'how much'
  Else If InList(W, 'quel quelle quels quelles') Then WhEnOf := 'what'
  Else WhEnOf := 'what';
End;

Function IsWeatherAdjEn(Const En: String): Boolean;
Begin
  IsWeatherAdjEn := InList(En,
      'cold hot warm cool nice bad good beautiful terrible');
End;

Function TranslateClauseEn(A, B: Integer; IsQ: Boolean; Const PreSubj: String;
    PrePlu: Boolean): String;
Var
  I, J, K, Next, EndSubj, V1: Integer;
  Wh, WhW, Res, Tail, SubjText, Aux, Rest, LexBase, Post, Freq, Front, AuxN,
      Pre2, Pred, Tmp: String;
  LexTns: Char;
  Subj: TFSubj;
  G: TFVG;
  Ctx: TFCtx;
  NP: TFNP;
  Please, Imperative, Inverted, EstCe, WhSubject, WhObject, DoQ, NegQ: Boolean;
  Sp: Integer;
  Imp: String;
  Cl, VStart, A0: Integer;
  Embed: Boolean;
  TmpTok: TFTok;
  NpInv: Boolean;
Begin
  NpInv := False;
  VStart := 0;
  A0 := A;
  Embed := EmbedFlag;
  EmbedFlag := False;
  TranslateClauseEn := '';
  If A > B Then Exit;
  Please := False;
  Imperative := False;
  Inverted := False;
  EstCe := False;
  WhSubject := False;
  WhObject := False;
  Wh := '';
  WhW := '';
  Front := '';
  Freq := '';

  { interjection ou liaison en tete }
  If (FT[A].Cat = 'J') And (A < B) And (FT[A].En = 'please') Then
  Begin
    Please := True;
    Inc(A);
    If (A <= B) And (FT[A].Cat = ',') Then Inc(A);
  End
  Else If (FT[A].Cat = 'J') And (A < B) Then
  Begin
    TranslateClauseEn := FT[A].En + ' ' + TranslateClauseEn(A + 1, B, IsQ,
        PreSubj, PrePlu);
    Exit;
  End
  Else If (FT[A].Cat = 'J') And (A = B) Then
  Begin
    TranslateClauseEn := FT[A].En;
    Exit;
  End;
  If (FT[A].Cat = 'C') And (A < B) Then
  Begin
    TranslateClauseEn := FT[A].En + ' ' + TranslateClauseEn(A + 1, B, IsQ,
        PreSubj, PrePlu);
    Exit;
  End;

  { complement de temps ou de lieu place avant la virgule }
  If (PreSubj = '') And (FT[A].Cat In ['R', 'E', 'a', 'd', 'U', 'D', 'S', 'T'])
      Then
    For K := A + 1 To B - 1 Do
      If (FT[K].Cat = ',') And ClauseHasVerbFr(K + 1, B) And Not
          ClauseHasVerbFr(A, K - 1) Then
      Begin
        Ctx.VerbEn := '';
        Ctx.BeLike := False;
        Ctx.Neg := False;
        Ctx.Motion := False;
        Ctx.Generic := False;
        Ctx.PersonSubj := False;
        Ctx.SubjPoss := '';
        Tmp := TailEn(A, K - 1, Ctx, Freq);
        TranslateClauseEn := Tmp + ', ' + TranslateClauseEn(K + 1, B, IsQ,
            PreSubj, PrePlu);
        Exit;
      End;

  { mot interrogatif }
  If (FT[A].Cat = 'W') And (PreSubj = '') Then
  Begin
    WhW := FT[A].W;
    If (WhW = 'que') And (A + 3 <= B) And (FT[A + 1].W = 'est') And (FT[A + 2].W
        = 'ce') And
       ((FT[A + 3].W = 'que') Or (FT[A + 3].W = 'qui')) Then
    Begin
      Wh := 'what';
      If FT[A + 3].W = 'qui' Then WhSubject := True Else WhObject := True;
      EstCe := FT[A + 3].W = 'que';
      Inc(A, 4);
    End
    Else If (WhW = 'qui') And (A + 3 <= B) And (FT[A + 1].W = 'est') And (FT[A +
        2].W = 'ce') And
            ((FT[A + 3].W = 'que') Or (FT[A + 3].W = 'qui')) Then
    Begin
      Wh := 'who';
      If FT[A + 3].W = 'qui' Then WhSubject := True Else WhObject := True;
      EstCe := FT[A + 3].W = 'que';
      Inc(A, 4);
    End
    Else If InList(WhW, 'quel quelle quels quelles') And (A < B) Then
    Begin
      If (FT[A + 1].Cat = 'V') And (FT[A + 1].Inf = 'être') Then
      Begin
        Wh := 'what';
        Inc(A);
      End
      Else If ParseNPFr(A + 1, NP) And (NP.Head > 0) Then
      Begin
        Wh := 'which ' + RenderPartFr(NP);
        Inc(A, 1 + NP.Used);
      End
      Else
      Begin
        Wh := 'what';
        Inc(A);
      End;
    End
    Else If (WhW = 'combien') And (A < B) Then
    Begin
      Wh := 'how many';
      Inc(A);
      If (A <= B) And (FT[A].W = 'de') Then
      Begin
        Inc(A);
        If ParseNPFr(A, NP) Then
        Begin
          NP.Plural := True;
          NP.Kind := '0';
          Wh := 'how many ' + RenderPartFr(NP);
          Inc(A, NP.Used);
        End;
      End
      Else
        Wh := 'how much';
    End
    Else
    Begin
      Wh := WhEnOf(WhW);
      If WhW = 'que' Then WhObject := True;
      If WhW = 'quoi' Then WhObject := True;
      If WhW = 'qui' Then
      Begin
        If (A < B) And (FT[A + 1].Cat = 'V') And Not ((A + 2 <= B) And (FT[A +
            2].Cat = 'P')) Then
          WhSubject := True
        Else
          WhObject := True;
      End;
      Inc(A);
    End;
    If A > B Then
    Begin
      TranslateClauseEn := UpperFirst(Wh);
      Exit;
    End;
    { Wh + est-ce que }
    If (A + 2 <= B) And (FT[A].W = 'est') And (FT[A + 1].W = 'ce') And
       ((FT[A + 2].W = 'que') Or (FT[A + 2].W = 'qui')) Then
    Begin
      If FT[A + 2].W = 'qui' Then WhSubject := True;
      EstCe := True;
      Inc(A, 3);
    End;
  End
  Else If (A + 2 <= B) And (FT[A].W = 'est') And (FT[A + 1].W = 'ce') And
          ((FT[A + 2].W = 'que') Or (FT[A + 2].W = 'qui')) And (PreSubj = '')
              Then
  Begin
    EstCe := True;
    IsQ := True;
    Inc(A, 3);
  End;
  If A > B Then Exit;

  { sujet }
  FillChar(Subj, SizeOf(Subj), 0);
  I := A;
  If PreSubj <> '' Then
  Begin
    Subj.Found := True;
    Subj.Text := '';
    If PrePlu Then Subj.Person := 6 Else Subj.Person := 3;
    Subj.Plural := PrePlu;
    Subj.Used := 0;
    I := A;
  End
  Else
  Begin
    { recherche d'une inversion : verbe suivi d'un pronom sujet }
    J := A;
    While (J <= B) And (FT[J].Cat In ['G', 'O']) Do
      Inc(J);
    If (J <= B) And (FT[J].Cat = 'V') And (FT[J].Tns In ['p', 'f', 'c', 'i'])
        Then
    Begin
      If (J < B) And (FT[J + 1].Cat = 'P') And (WhSubject = False) Then
      Begin
        { verbe - pronom }
        Subj.Found := True;
        Subj.IsPron := True;
        Subj.Pron := FT[J + 1].W;
        Subj.Text := SubjEn(FT[J + 1].W);
        Subj.Person := SubjPerson(FT[J + 1].W);
        Subj.PersonSubj := True;
        Subj.Plural := Subj.Person >= 4;
        If FT[J + 1].W = 'je' Then Subj.Poss := 'my' Else If InList(FT[J + 1].W,
            'tu vous') Then Subj.Poss := 'your'
        Else If FT[J + 1].W = 'il' Then Subj.Poss := 'his' Else If FT[J + 1].W =
            'elle' Then Subj.Poss := 'her'
        Else If InList(FT[J + 1].W, 'nous on') Then Subj.Poss := 'our' Else
            Subj.Poss := 'their';
        { on retire le pronom de la chaine }
        For K := J + 1 To B - 1 Do
          FT[K] := FT[K + 1];
        Dec(B);
        Inverted := True;
        I := A;
        VStart := A;
      End
      Else If (WhSubject) Then
      Begin
        Subj.Found := True;
        Subj.IsWh := True;
        Subj.Text := Wh;
        If Wh = '' Then Subj.Text := 'who';
        Subj.Person := 3;
        Wh := '';
        I := A;
      End
      Else If (Wh <> '') Or IsQ Then
      Begin
        { verbe - groupe nominal : "Ou est la bibliotheque ?" }
        If ParseSubjFr(J + 1, Subj) Then
        Begin
          { on deplace le verbe apres le sujet }
          TmpTok := FT[J];
          For K := J To J + Subj.Used - 1 Do
            FT[K] := FT[K + 1];
          FT[J + Subj.Used] := TmpTok;
          VStart := J + Subj.Used;
          Inverted := True;
          NpInv := True;
        End
        Else
        Begin
          Imperative := False;
          I := A;
        End;
      End
      Else If (Not IsQ) And (Wh = '') Then
      Begin
        Imperative := True;
        I := A;
      End;
    End
    Else If (J <= B) And (FT[J].Cat = 'V') And (FT[J].Tns = 'M') And (Wh = '')
        Then
    Begin
      Imperative := True;
      I := A;
    End;
  End;

  If Imperative Then
  Begin
    { imperatif }
    ScanVG(A, B, G);
    If G.V1 = 0 Then Exit;
    Ctx.VerbEn := G.MainEn;
    Ctx.BeLike := G.Mode = 'b';
    Ctx.Neg := G.Neg;
    Ctx.Motion := IsMotionEn(G.MainEn);
    Ctx.Generic := False;
    Ctx.PersonSubj := True;
    Ctx.SubjPoss := '';
    Tail := TailEn(G.Next, B, Ctx, Freq);
    If (G.Mode = 'b') Or (G.MainEn = 'be') Then Imp := 'be' Else Imp :=
        G.MainEn;
    Res := '';
    If FT[G.V1].Slot = 4 Then
      Res := 'let''s'
    Else If G.Neg Then
      Res := 'don''t'
    Else If Please Then
      Res := 'please';
    If (FT[G.V1].Slot = 4) And G.Neg Then Res := 'let''s not';
    Res := Join(Res, Join(Freq, Join(Imp, Join(G.Clit, Tail))));
    If (G.Neg) And (FT[G.V1].Slot <> 4) And Please Then Res := Join('please',
        Res);
    TranslateClauseEn := Res;
    Exit;
  End;

  If (Not Subj.Found) And (PreSubj = '') Then
  Begin
    If Not ParseSubjFr(A, Subj) Then
    Begin
      Ctx.VerbEn := '';
      Ctx.BeLike := False;
      Ctx.Neg := False;
      Ctx.Motion := False;
      Ctx.Generic := False;
      Ctx.PersonSubj := False;
      Ctx.SubjPoss := '';
      Tail := TailEn(A, B, Ctx, Freq);
      TranslateClauseEn := Join(Wh, Tail);
      Exit;
    End;
    I := A + Subj.Used;
  End;
  If VStart > 0 Then I := VStart;
  { proposition relative apres le sujet : le garcon qui habite ici est mon ami }
  If (PreSubj = '') And (Not Inverted) And (I < B) And (FT[I].Cat = 'W') And
      (FT[I].W = 'qui') And
     (FT[I + 1].Cat In ['V', 'G', 'O']) Then
  Begin
    J := I + 1;
    While (J <= B) And Not ((FT[J].Cat = 'V') And (FT[J].Tns In ['p', 'f', 'c',
        'i', 'M'])) Do
      Inc(J);
    K := J + 1;
    { fin de la relative : le deuxieme verbe fini }
    While (K <= B) And Not ((FT[K].Cat = 'V') And (FT[K].Tns In ['p', 'f', 'c',
        'i', 'M']) And
          (Not (FT[K - 1].Cat In ['G', 'O']) Or (FT[K - 1].Cat = 'G'))) Do
      Inc(K);
    { le verbe principal peut etre precede de ne ou de pronoms }
    EndSubj := K - 1;
    While (EndSubj > J) And (FT[EndSubj].Cat In ['G', 'O']) Do
      Dec(EndSubj);
    If K > B Then EndSubj := B;
    If (Subj.PersonSubj) Then Tmp := 'who' Else Tmp := 'that';
    SubjText := Subj.Text + ' ' + Tmp + ' ' + TranslateClauseEn(I + 1, EndSubj,
        False, '%', Subj.Plural);
    Subj.Text := SubjText;
    I := EndSubj + 1;
  End;

  { le verbe }
  ScanVG(I, B, G);
  If G.V1 = 0 Then
  Begin
    Ctx.VerbEn := '';
    Ctx.BeLike := False;
    Ctx.Neg := False;
    Ctx.Motion := False;
    Ctx.Generic := False;
    Ctx.PersonSubj := False;
    Ctx.SubjPoss := '';
    Tail := TailEn(I, B, Ctx, Freq);
    TranslateClauseEn := Join(Wh, Join(Subj.Text, Tail));
    Exit;
  End;
  V1 := G.V1;
  Next := G.Next;
  Sp := Subj.Person;
  If Sp < 1 Then Sp := 3;
  SubjText := Subj.Text;
  Pred := '';
  If (G.Mode = 'b') And (Next <= B) And (FT[Next].Cat = 'A') And (FT[Next].En =
      'born') And
     (G.Tns = 'p') Then G.Tns := 'i';
  If (G.Mode = 's') And (G.MainEn = 'go') And (WhW = 'où') And (G.Tns = 'p')
      Then G.Rain := True;

  { il impersonnel et expressions }
  If (Subj.Pron = 'il') And (Not Inverted Or True) Then
  Begin
    If (FT[V1].Inf = 'pleuvoir') Or (FT[V1].Inf = 'falloir') Then SubjText :=
        'it';
  End;
  If (G.Mode = 't') Then
  Begin
    SubjText := 'there';
    If (Next <= B) And ParseNPFr(Next, NP) Then
    Begin
      If NP.Plural Then Sp := 6 Else Sp := 3;
    End;
  End;
  { avoir + idiome : faim, soif... }
  If (G.Mode = 'h') And (Next <= B) Then
  Begin
    Tmp := FT[Next].W;
    For K := 1 To LexCount Do
      If (Lex[K].Pos = 'a') And (Lex[K].Idiom = Tmp) And (Tmp <> '') Then
      Begin
        G.Mode := 'b';
        G.MainEn := 'be';
        Pred := Lex[K].En;
        Inc(Next);
        Break;
      End;
    If (G.Mode = 'h') And (FT[Next].Cat = 'U') And (Next < B) And InList(FT[Next
        + 1].W, 'an ans année années') Then
    Begin
      G.Mode := 'b';
      G.MainEn := 'be';
      Pred := FT[Next].En + ' years old';
      Inc(Next, 2);
    End;
    If (G.Mode = 'h') And (FT[Next].W = 'besoin') And (Next < B) And (FT[Next +
        1].W = 'de') Then
    Begin
      G.Mode := 's';
      G.MainEn := 'need';
      Inc(Next, 2);
    End;
  End;
  { le temps qu'il fait }
  If (FT[V1].Inf = 'faire') And (Subj.Pron = 'il') And (Next <= B) And
      (FT[Next].Cat = 'A') And
     IsWeatherAdjEn(FT[Next].En) Then
  Begin
    G.Mode := 'b';
    G.MainEn := 'be';
    SubjText := 'it';
    Sp := 3;
    If (FT[Next].En = 'beautiful') Or (FT[Next].En = 'good') Then FT[Next].En :=
        'nice';
  End;
  { il est huit heures }
  If (Subj.Pron = 'il') And (G.Mode = 'b') And (Next < B) And (FT[Next].Cat =
      'U') And
     (FT[Next + 1].Cat = 'N') And (FT[Next + 1].L > 0) Then
    If Lex[FT[Next + 1].L].En = 'hour' Then
    Begin
      SubjText := 'it';
      Pred := FT[Next].En + ' o''clock';
      Inc(Next, 2);
    End;
  { c'est }
  If (Subj.Pron = 'ce') And (G.Mode = 'b') Then
  Begin
    If (G.Tns = 'p') And (Next <= B) And (FT[Next].Cat In ['D', 'S', 'T', 'N',
        'U']) Then
    Begin
      If Subj.Plural Or (FT[V1].Slot = 6) Then
      Begin
        SubjText := 'these';
        Sp := 6;
      End
      Else SubjText := 'this';
    End
    Else
      SubjText := 'it';
  End;
  If (Subj.Pron = 'il') And (G.Mode = 'b') And (Next <= B) And (FT[Next].Cat =
      'R') And
     (FT[Next].W = 'tard') Then
    SubjText := 'it';
  { choix du verbe anglais selon les complements }
  If FT[V1].Inf = 'faire' Then
  Begin
    G.MainEn := 'do';
    For K := Next To B Do
      If (FT[K].Cat = 'N') And InList(FT[K].En,
          'cake meal mistake noise coffee soup salad') Then G.MainEn := 'make';
  End;
  If (FT[V1].Inf = 'prendre') Then
    For K := Next To B Do
      If (FT[K].Cat = 'N') And IsMealNoun(FT[K].En) Then G.MainEn := 'have';
  If (FT[V1].Inf = 'vouloir') And (G.Tns = 'c') And (G.Mode = 's') Then G.MainEn
      := 'like';
  If (FT[V1].Inf = 'aimer') Then
  Begin
    If G.Clit <> '' Then G.MainEn := 'love' Else G.MainEn := 'like';
    If (G.Mode = 's') And (G.Tns = 'c') Then G.MainEn := 'like';
  End;
  If (G.Mode = 's') And (G.MainEn = 'rain') And (Subj.Pron = 'il') Then
  Begin
    If Not ((A0 > 1) And (FT[A0 - 1].Cat = 'C')) Then G.Rain := True;
    SubjText := 'it';
  End;
  { reflechis }
  If (FT[V1].Inf = 'appeler') And (G.Mode = 's') And (Not G.Refl) And (G.CliW <>
      '') Then
    If ((Subj.Pron = 'je') And (G.CliW = 'me')) Or ((Subj.Pron = 'tu') And
        (G.CliW = 'te')) Or
       ((Subj.Pron = 'nous') And (G.CliW = 'nous')) Or ((Subj.Pron = 'vous') And
           (G.CliW = 'vous')) Then
    Begin
      G.Refl := True;
      G.Clit := '';
    End;
  If G.Refl And (FT[V1].Inf = 'appeler') And (G.Mode = 's') Then
  Begin
    Ctx.VerbEn := 'be';
    Ctx.BeLike := True;
    Ctx.Neg := False;
    Ctx.Motion := False;
    Ctx.Generic := False;
    Ctx.PersonSubj := True;
    Ctx.SubjPoss := '';
    Tail := TailEn(Next, B, Ctx, Freq);
    If Subj.Pron = 'je' Then Res := 'My name is'
    Else If (Subj.Pron = 'tu') Or (Subj.Pron = 'vous') Then Res :=
        'Your name is'
    Else If Subj.Pron = 'il' Then Res := 'His name is'
    Else If Subj.Pron = 'elle' Then Res := 'Her name is'
    Else Res := 'The name is';
    TranslateClauseEn := Join(Res, Tail);
    Exit;
  End;
  If G.Refl And (G.Mode = 's') And (Lex[FT[V1].L].Fr <> '') And (FT[V1].L > 0)
      Then
    If Copy(Lex[FT[V1].L].Fr, 1, 3) <> 'se ' Then
    Begin
      If Sp = 1 Then G.Clit := Join(G.Clit, 'myself')
      Else If Sp = 2 Then G.Clit := Join(G.Clit, 'yourself')
      Else If Sp = 3 Then
      Begin
        If Subj.Poss = 'her' Then G.Clit := Join(G.Clit, 'herself')
        Else If Subj.Poss = 'its' Then G.Clit := Join(G.Clit, 'itself')
        Else G.Clit := Join(G.Clit, 'himself');
      End
      Else If Sp = 4 Then G.Clit := Join(G.Clit, 'ourselves')
      Else If Sp = 5 Then G.Clit := Join(G.Clit, 'yourselves')
      Else G.Clit := Join(G.Clit, 'themselves');
    End;

  { complements }
  Ctx.VerbEn := G.MainEn;
  If G.Mode = 'b' Then Ctx.VerbEn := 'be';
  Ctx.BeLike := (G.Mode = 'b') Or (G.Mode = 'p');
  Ctx.Neg := G.Neg;
  Ctx.Motion := IsMotionEn(G.MainEn);
  Ctx.Generic := InList(G.MainEn, 'like love hate prefer enjoy');
  Ctx.PersonSubj := Subj.PersonSubj;
  Ctx.SubjPoss := Subj.Poss;
  If G.Mode = 't' Then Ctx.BeLike := False;
  If (G.Mode = 'b') And (Pred <> '') Then
    Tail := Pred
  Else
    Tail := '';
  Tail := Join(Tail, TailEn(Next, B, Ctx, Freq));
  If G.Freq <> '' Then Freq := Join(G.Freq, Freq);
  If (G.Mode = 'b') And (Subj.Pron = 'il') And (SubjText = 'he') And (Next <= B)
      And
     (FT[Next].Cat = 'A') And InList(FT[Next].En,
         'important possible difficult easy true') Then
    SubjText := 'it';

  BuildEn(G, Sp, Aux, Rest, LexBase, Post, LexTns);
  If Subj.IsWh And (Sp = 3) Then Sp := 3;

  { assemblage }
  DoQ := (IsQ Or Inverted Or EstCe) And Not WhSubject And Not Subj.IsWh;
  If Subj.IsWh And (SubjText <> '') And (Wh = '') Then DoQ := False;
  If PreSubj <> '' Then DoQ := False;
  If WhSubject Then DoQ := False;
  If Embed Then DoQ := False;
  Res := '';
  If DoQ Then
  Begin
    If (Aux = '') Then
    Begin
      Aux := DoFormEn(Sp, LexTns);
      Rest := Join(LexBase, Post);
      Post := '';
      LexBase := '';
    End;
    If G.Neg Then AuxN := NegContract(Aux) Else AuxN := Aux;
    Res := Join(Wh, Join(AuxN, Join(SubjText, Join(Freq, Join(Rest, Join(Post,
        Join(G.Clit, Tail)))))));
    If (G.Neg) And (Aux = 'am') Then Res := Join(Wh, Join('am', Join(SubjText,
        Join('not', Join(Freq, Join(Rest, Join(G.Clit, Tail)))))));
  End
  Else
  Begin
    If Subj.IsWh And (Wh = '') Then Wh := '';
    If G.Neg Then
    Begin
      If Aux = '' Then
      Begin
        Aux := DoFormEn(Sp, LexTns);
        Rest := Join(LexBase, Post);
        Post := '';
        LexBase := '';
      End;
      Res := Join(Wh, Join(SubjText, Join(NegContract(Aux), Join(Freq,
          Join(Rest, Join(G.Clit, Tail))))));
      If Aux = 'am' Then Res := Join(Wh, Join(SubjText, Join('am not',
          Join(Freq, Join(Rest, Join(G.Clit, Tail))))));
    End
    Else If Aux <> '' Then
      Res := Join(Wh, Join(SubjText, Join(Aux, Join(Freq, Join(Rest,
          Join(G.Clit, Tail))))))
    Else
      Res := Join(Wh, Join(SubjText, Join(Freq, Join(FiniteEn(LexBase, LexTns,
          Sp), Join(Post, Join(G.Clit, Tail))))));
  End;
  If Please Then Res := 'please ' + Res;
  TranslateClauseEn := Res;
End;

Function TranslateRelative(A, B: Integer; Plu: Boolean): String;
Begin
  TranslateRelative := TranslateClauseEn(A, B, False, '%', Plu);
End;

{ Virgules, conjonctions et subordonnees. }
Function TranslateTokensEn(A, B: Integer; IsQ: Boolean): String;
Var
  K: Integer;
  Dummy: String;
Begin
  If A > B Then
  Begin
    TranslateTokensEn := '';
    Exit;
  End;
  Dummy := FindPhraseFr(FrPhraseKey(A, B));
  If Dummy <> '' Then
  Begin
    If (A > 1) And (Copy(Dummy, 1, 2) <> 'I ') Then Dummy := LowerFirst(Dummy);
    TranslateTokensEn := Dummy;
    Exit;
  End;
  For K := A + 1 To B - 1 Do
    If (FT[K].Cat = ',') Then
      If ((K - A = 1) And (FT[A].Cat = 'J') And (FT[A].En <> 'please')) Or
         (ClauseHasVerbFr(A, K - 1) And ClauseHasVerbFr(K + 1, B)) Then
      Begin
        TranslateTokensEn := TranslateTokensEn(A, K - 1, False) + ', ' +
            TranslateTokensEn(K + 1, B, IsQ);
        Exit;
      End;
  For K := A + 1 To B - 1 Do
    If (FT[K].Cat = 'C') And ClauseHasVerbFr(A, K - 1) And ClauseHasVerbFr(K +
        1, B) Then
    Begin
      If (FT[K].W = 'mais') Then
        TranslateTokensEn := TranslateTokensEn(A, K - 1, False) + ', ' +
            FT[K].En + ' ' +
                             TranslateTokensEn(K + 1, B, IsQ)
      Else
        TranslateTokensEn := TranslateTokensEn(A, K - 1, False) + ' ' + FT[K].En
            + ' ' +
                             TranslateTokensEn(K + 1, B, IsQ);
      Exit;
    End;
  Dummy := '';
  TranslateTokensEn := TranslateClauseEn(A, B, IsQ, '', False);
End;

{ ------------------------------------------------------------------ }
{ FRANCAIS -> ANGLAIS : phrase complete                               }
{ ------------------------------------------------------------------ }

Function FixEnglish(Const S: String): String;
Var
  Cur, Res, Word1: String;
  P: Integer;
Begin
  Cur := S;
  Res := '';
  While Cur <> '' Do
  Begin
    P := Pos(' ', Cur);
    If P = 0 Then
    Begin
      Word1 := Cur;
      Cur := '';
    End
    Else
    Begin
      Word1 := Copy(Cur, 1, P - 1);
      Delete(Cur, 1, P);
    End;
    If Word1 <> '' Then
    Begin
      If (Word1[1] = ',') Or (Res = '') Then
        Res := Res + Word1
      Else
        Res := Res + ' ' + Word1;
    End;
  End;
  FixEnglish := Res;
End;

Function FrTranslateSentence(Const Sentence: String): String;
Var
  Res: String;
  Question: Boolean;
Begin
  FrTokenize(Sentence);
  If FN = 0 Then
  Begin
    FrTranslateSentence := '';
    Exit;
  End;
  FrClassify;
  Question := FEnd = '?';
  Res := '';
  If (FN = 2) And (FT[1].W = 'ça') And (FT[2].W = 'va') Then
  Begin
    If Question Then Res := 'How are you' Else Res := 'I am fine';
  End;
  If Res = '' Then Res := FindPhraseFr(FrPhraseKey(1, FN));
  If Res = '' Then Res := TranslateTokensEn(1, FN, Question);
  Res := UpperFirst(FixEnglish(Res));
  If FEnd <> #0 Then Res := Res + FEnd;
  FrTranslateSentence := Res;
End;

{ ------------------------------------------------------------------ }
{ ALLEMAND -> ANGLAIS : jetons et classes de mots                     }
{ ------------------------------------------------------------------ }

Type
  TGTok = Record
    W, Orig: String[34];
    Cap, Punct: Boolean;
    Cat: Char;          { voir PickDeCat }
    L, LInf, LPart: Integer;
    Cases: String[4];
    Gen: Char;
    Plu, CanInf, CanPart: Boolean;
    Tns: Char;          { p i c : verbe fini }
    Slot: Integer;
    Kind: Char;
    En, EnObj: String[34];
  End;

Var
  GT: Array[0..MaxTokens + 1] Of TGTok;
  GN: Integer;
  GEnd: Char;
  SourceDe, DeToEn, DePivot, DeEmbed: Boolean;

Function DeLow(Const S: String): String;
Begin
  DeLow := FrLower(S);
End;

Function DeEnglishOf(L: Integer): String;
Var
  Inf: String;
Begin
  DeEnglishOf := '';
  If L <= 0 Then Exit;
  If Lex[L].En <> '' Then
  Begin
    DeEnglishOf := Lex[L].En;
    Exit;
  End;
  Inf := Lex[L].De;
  If Inf = 'sein' Then DeEnglishOf := 'be'
  Else If Inf = 'haben' Then DeEnglishOf := 'have'
  Else If Inf = 'werden' Then DeEnglishOf := 'become'
  Else If Inf = 'können' Then DeEnglishOf := 'can'
  Else If Inf = 'müssen' Then DeEnglishOf := 'must'
  Else If Inf = 'wollen' Then DeEnglishOf := 'want'
  Else If Inf = 'sollen' Then DeEnglishOf := 'should'
  Else If Inf = 'dürfen' Then DeEnglishOf := 'may'
  Else If Inf = 'mögen' Then DeEnglishOf := 'like'
  Else If Inf = 'möchten' Then DeEnglishOf := 'like'
  Else If Inf = 'wissen' Then DeEnglishOf := 'know'
  Else If Inf = 'kennen' Then DeEnglishOf := 'know'
  Else If Inf = 'heißen' Then DeEnglishOf := 'be called'
  Else If Inf = 'suchen' Then DeEnglishOf := 'look for'
  Else If Inf = 'geben' Then DeEnglishOf := 'give'
  Else If Inf = 'machen' Then DeEnglishOf := 'do'
  Else If Inf = 'fahren' Then DeEnglishOf := 'drive'
  Else DeEnglishOf := Inf;
End;

Function DeProperTry(Const Orig, Keys: String): String;
Var
  Rest, Key: String;
  P: Integer;
Begin
  DeProperTry := '';
  Rest := Keys;
  While Rest <> '' Do
  Begin
    P := Pos(' ', Rest);
    If P = 0 Then
    Begin
      Key := Rest;
      Rest := '';
    End
    Else
    Begin
      Key := Copy(Rest, 1, P - 1);
      Delete(Rest, 1, P);
    End;
    If ProperDe(Key) = Orig Then
    Begin
      DeProperTry := UpperFirst(Key);
      Exit;
    End;
  End;
End;

Function DeProperEnOf(Const Orig: String): String;
Var
  R: String;
Begin
  R := DeProperTry(Orig,
      'germany france spain italy england canada switzerland austria belgium p'+
      'ortugal china japan russia brazil mexico india america africa europe');
  If R = '' Then
    R := DeProperTry(Orig,
        'asia australia ireland scotland sweden norway denmark poland greece t'+
        'urkey egypt morocco london brussels geneva moscow vienna lisbon athen'+
        's cairo rome munich cologne prague');
  If Orig = 'Schweiz' Then R := 'Switzerland';
  If Orig = 'USA' Then R := 'the United States';
  DeProperEnOf := R;
End;

Function DeNumberEnOf(Const W: String): String;
Const
  Keys =
      'two three four five six seven eight nine ten eleven twelve thirteen fou'+
      'rteen fifteen sixteen seventeen eighteen nineteen twenty thirty forty f'+
      'ifty sixty seventy eighty ninety hundred thousand';
Var
  Rest, Key: String;
  P: Integer;
Begin
  DeNumberEnOf := '';
  If W = 'eins' Then
  Begin
    DeNumberEnOf := 'one';
    Exit;
  End;
  Rest := Keys;
  While Rest <> '' Do
  Begin
    P := Pos(' ', Rest);
    If P = 0 Then
    Begin
      Key := Rest;
      Rest := '';
    End
    Else
    Begin
      Key := Copy(Rest, 1, P - 1);
      Delete(Rest, 1, P);
    End;
    If DeLow(DeNumber(Key)) = W Then
    Begin
      If Key = 'hundred' Then DeNumberEnOf := 'one hundred'
      Else If Key = 'thousand' Then DeNumberEnOf := 'one thousand'
      Else DeNumberEnOf := Key;
      Exit;
    End;
  End;
End;

{ ---- recherche dans le lexique ---- }

Function DeLexExact(Const W: String; Const Kinds: String): Integer;
Var
  I: Integer;
Begin
  DeLexExact := 0;
  For I := 1 To LexCount Do
    If (Lex[I].De <> '') And (Pos(Lex[I].Pos, Kinds) > 0) And (DeLow(Lex[I].De)
        = W) Then
    Begin
      DeLexExact := I;
      Exit;
    End;
End;

Function DeFindNoun(Const W: String; Var Plu: Boolean): Integer;
Var
  I: Integer;
  C: Char;
  P: Boolean;
Begin
  DeFindNoun := 0;
  C := 'n';
  For I := 1 To LexCount Do
    If (Lex[I].Pos = 'n') And (Lex[I].De <> '') Then
    Begin
      If DeLow(NounDe(I, False, 'n')) = W Then
      Begin
        DeFindNoun := I;
        Plu := False;
        Exit;
      End;
      If DeLow(NounDe(I, True, 'n')) = W Then
      Begin
        DeFindNoun := I;
        Plu := True;
        Exit;
      End;
    End;
  For I := 1 To LexCount Do
    If (Lex[I].Pos = 'n') And (Lex[I].De <> '') Then
      For P := True Downto False Do
        If DeLow(NounDe(I, P, 'd')) = W Then
        Begin
          DeFindNoun := I;
          Plu := P;
          Exit;
        End;
End;

Function DeFindAdj(Const W: String; Var Ending: String): Integer;
Var
  I: Integer;
  Stem, Rest: String;
Begin
  DeFindAdj := 0;
  For I := 1 To LexCount Do
    If (Lex[I].Pos = 'a') And (Lex[I].De <> '') Then
    Begin
      If DeLow(Lex[I].De) = W Then
      Begin
        Ending := '';
        DeFindAdj := I;
        Exit;
      End;
      If Pos('i', Lex[I].DeFlags) = 0 Then
      Begin
        If Lex[I].DePl <> '' Then Stem := DeLow(Lex[I].DePl)
        Else
        Begin
          Stem := DeLow(Lex[I].De);
          If EndsWith(Stem, 'el') And (Length(Stem) > 3) Then Stem := Copy(Stem,
              1, Length(Stem) - 2) + 'l';
        End;
        If (Length(W) > Length(Stem)) And (Copy(W, 1, Length(Stem)) = Stem) Then
        Begin
          Rest := Copy(W, Length(Stem) + 1, 5);
          If (Rest = 'e') Or (Rest = 'en') Or (Rest = 'er') Or (Rest = 'es') Or
              (Rest = 'em') Then
          Begin
            Ending := Rest;
            DeFindAdj := I;
            Exit;
          End;
        End;
      End;
    End;
End;

{ Formes verbales : L = forme conjuguee, LInf, LPart. }
Function DeMatchVerb(Const W: String; Var T: TGTok): Boolean;
Var
  I, S: Integer;
  Form: String;
  Found: Boolean;
Begin
  Found := False;
  T.CanInf := False;
  T.CanPart := False;
  For I := 1 To LexCount Do
    If (Lex[I].Pos = 'v') And (Lex[I].De <> '') Then
    Begin
      If (Lex[I].DeSep <> '') And (T.Kind <> 'A') Then
      Begin
        { forme conjuguee avec le prefixe colle (proposition subordonnee) }
        For S := 1 To 6 Do
          If (T.Kind <> 'A') And (DeLow(Lex[I].DeSep + DeFinite(I, S, 'p')) = W)
              Then
          Begin
            T.L := I;
            T.Tns := 'p';
            T.Slot := S;
            T.Kind := 'A';
          End;
        For S := 1 To 6 Do
          If (T.Kind <> 'A') And (DeLow(Lex[I].DeSep + DeFinite(I, S, 'i')) = W)
              Then
          Begin
            T.L := I;
            T.Tns := 'i';
            T.Slot := S;
            T.Kind := 'A';
          End;
      End;
      If (T.L = 0) Then
      Begin
        For S := 1 To 6 Do
          If (T.L = 0) And (DeLow(DeFinite(I, S, 'p')) = W) Then
          Begin
            T.L := I;
            T.Tns := 'p';
            T.Slot := S;
          End;
        For S := 1 To 6 Do
          If (T.L = 0) And (DeLow(DeFinite(I, S, 'i')) = W) Then
          Begin
            T.L := I;
            T.Tns := 'i';
            T.Slot := S;
          End;
        If IsDeSpecial(Lex[I].De) Then
          For S := 1 To 6 Do
            If (T.L = 0) And (DeLow(DeFinite(I, S, 'c')) = W) Then
            Begin
              T.L := I;
              T.Tns := 'c';
              T.Slot := S;
            End;
      End;
      If (Not T.CanInf) And (DeLow(DeFullInf(I)) = W) Then
      Begin
        T.CanInf := True;
        T.LInf := I;
      End;
      If (Not T.CanPart) And (DeLow(DeParticiple(I)) = W) Then
      Begin
        T.CanPart := True;
        T.LPart := I;
      End;
    End;
  Form := '';
  DeMatchVerb := (T.L > 0) Or T.CanInf Or T.CanPart;
  If Found Then Form := '';
End;

{ ---- tokenisation ---- }

Procedure GPush(Const W, Orig: String; Cap, Punct: Boolean);
Begin
  If GN >= MaxTokens Then Exit;
  Inc(GN);
  FillChar(GT[GN], SizeOf(TGTok), 0);
  GT[GN].W := W;
  GT[GN].Orig := Orig;
  GT[GN].Cap := Cap;
  GT[GN].Punct := Punct;
  GT[GN].Gen := 'n';
  GT[GN].Tns := ' ';
End;

Procedure GSplitWord(Const Raw: String; Cap: Boolean);
Var
  LW: String;
Begin
  LW := DeLow(Raw);
  If LW = 'im' Then
  Begin
    GPush('in', 'in', False, False);
    GPush('dem', 'dem', False, False);
  End
  Else If LW = 'am' Then
  Begin
    GPush('an', 'an', False, False);
    GPush('dem', 'dem', False, False);
  End
  Else If LW = 'zum' Then
  Begin
    GPush('zu', 'zu', False, False);
    GPush('dem', 'dem', False, False);
  End
  Else If LW = 'zur' Then
  Begin
    GPush('zu', 'zu', False, False);
    GPush('der', 'der', False, False);
  End
  Else If LW = 'vom' Then
  Begin
    GPush('von', 'von', False, False);
    GPush('dem', 'dem', False, False);
  End
  Else If LW = 'beim' Then
  Begin
    GPush('bei', 'bei', False, False);
    GPush('dem', 'dem', False, False);
  End
  Else If LW = 'ins' Then
  Begin
    GPush('in', 'in', False, False);
    GPush('das', 'das', False, False);
  End
  Else If LW = 'ans' Then
  Begin
    GPush('an', 'an', False, False);
    GPush('das', 'das', False, False);
  End
  Else
    GPush(LW, Raw, Cap, False);
  If (Cap) And (GN > 0) And (Raw = 'Sie') Then GT[GN].Orig := 'Sie';
End;

Procedure GTokenize(Const S0: String);
Var
  S, Word1: String;
  I, L: Integer;
  C: Char;
  Cap: Boolean;
Begin
  GN := 0;
  GEnd := #0;
  S := '';
  I := 1;
  While I <= Length(S0) Do
  Begin
    If (S0[I] = #$E2) And (I + 2 <= Length(S0)) And (S0[I + 1] = #$80) And
       ((S0[I + 2] = #$99) Or (S0[I + 2] = #$98)) Then
    Begin
      S := S + '''';
      Inc(I, 3);
    End
    Else If (S0[I] = #$C2) And (I < Length(S0)) And
            ((S0[I + 1] = #$AB) Or (S0[I + 1] = #$BB) Or (S0[I + 1] = #$A0))
                Then
    Begin
      S := S + ' ';
      Inc(I, 2);
    End
    Else
    Begin
      S := S + S0[I];
      Inc(I);
    End;
  End;
  L := Length(S);
  I := 1;
  While I <= L Do
  Begin
    C := S[I];
    If FrWordByte(C) Then
    Begin
      Word1 := '';
      While (I <= L) And (FrWordByte(S[I]) Or
            ((S[I] = '-') And (Word1 <> '') And (I < L) And FrWordByte(S[I +
                1]))) Do
      Begin
        Word1 := Word1 + S[I];
        Inc(I);
      End;
      Cap := (Word1 <> '') And (((Word1[1] >= 'A') And (Word1[1] <= 'Z')) Or
             ((Word1[1] = #$C3) And (Length(Word1) > 1) And (Word1[2] >= #$80)
                 And (Word1[2] <= #$9E)));
      GSplitWord(Word1, Cap);
    End
    Else
    Begin
      If (C = '.') Or (C = '?') Or (C = '!') Then
      Begin
        If GEnd = #0 Then GEnd := C;
        If C = '?' Then GEnd := '?';
      End
      Else If (C = ',') Or (C = ';') Or (C = ':') Then
      Begin
        GPush(',', ',', False, True);
        GT[GN].Cat := ',';
      End;
      Inc(I);
    End;
  End;
End;

Procedure GMergeMultiword;
Var
  I, K, Len, L: Integer;
  Key: String;
  Bad, Found: Boolean;
Begin
  I := 1;
  While I < GN Do
  Begin
    Found := False;
    For Len := 3 Downto 2 Do
      If (Not Found) And (I + Len - 1 <= GN) Then
      Begin
        Key := '';
        Bad := False;
        For K := I To I + Len - 1 Do
        Begin
          If GT[K].Punct Then Bad := True;
          Key := Join(Key, GT[K].W);
        End;
        If Not Bad Then
        Begin
          L := DeLexExact(Key, 'rpcj');
          If (L > 0) And (Length(Key) <= 34) Then
          Begin
            Found := True;
            GT[I].W := Key;
            GT[I].Orig := Key;
            For K := I + 1 To GN - Len + 1 Do
              GT[K] := GT[K + Len - 1];
            Dec(GN, Len - 1);
          End;
        End;
      End;
    Inc(I);
  End;
End;

{ ------------------------------------------------------------------ }
{ ALLEMAND -> ANGLAIS : classification                                }
{ ------------------------------------------------------------------ }

{ Determinants : renvoie le genre de determinant (d defini, i indefini, k kein,
  s possessif,
  t demonstratif, q quantificateur, w welch) et la terminaison. }
Function DeDetInfo(Const W: String; Var Kind: Char; Var Ending: String; Var
    Stem: String): Boolean;
Const
  EinStems = 'ein kein mein dein sein ihr unser euer';
  OtherStems = 'dies jed welch all manch solch viel wenig einig mehrer beid';
Var
  Rest, S: String;
  P, K: Integer;
  E: String;
Begin
  DeDetInfo := False;
  Ending := '';
  Stem := '';
  If InList(W, 'der die das den dem des') Then
  Begin
    Kind := 'd';
    Stem := W;
    DeDetInfo := True;
    Exit;
  End;
  Rest := EinStems;
  While Rest <> '' Do
  Begin
    P := Pos(' ', Rest);
    If P = 0 Then
    Begin
      S := Rest;
      Rest := '';
    End
    Else
    Begin
      S := Copy(Rest, 1, P - 1);
      Delete(Rest, 1, P);
    End;
    If (W = S) Or ((Length(W) > Length(S)) And (Copy(W, 1, Length(S)) = S) And
       InList(Copy(W, Length(S) + 1, 3), 'e en em er es')) Then
    Begin
      Ending := Copy(W, Length(S) + 1, 3);
      Stem := S;
      If S = 'ein' Then Kind := 'i'
      Else If S = 'kein' Then Kind := 'k'
      Else Kind := 's';
      DeDetInfo := True;
      Exit;
    End;
  End;
  Rest := OtherStems;
  While Rest <> '' Do
  Begin
    P := Pos(' ', Rest);
    If P = 0 Then
    Begin
      S := Rest;
      Rest := '';
    End
    Else
    Begin
      S := Copy(Rest, 1, P - 1);
      Delete(Rest, 1, P);
    End;
    If (Length(W) > Length(S)) And (Copy(W, 1, Length(S)) = S) And
       InList(Copy(W, Length(S) + 1, 3), 'e en em er es') Then
    Begin
      Ending := Copy(W, Length(S) + 1, 3);
      Stem := S;
      If S = 'dies' Then Kind := 't'
      Else If S = 'welch' Then Kind := 'w'
      Else Kind := 'q';
      DeDetInfo := True;
      Exit;
    End;
  End;
  K := 0;
  E := '';
End;

Function DetCases(Const W: String; Kind: Char; Const Ending: String; Gen: Char;
    Plu: Boolean): String;
Begin
  DetCases := 'nadg';
  If Kind = 'd' Then
  Begin
    If W = 'der' Then
    Begin
      If Plu Then DetCases := 'g'
      Else If Gen = 'f' Then DetCases := 'dg'
      Else DetCases := 'n';
    End
    Else If (W = 'die') Or (W = 'das') Then DetCases := 'na'
    Else If W = 'den' Then
    Begin
      If Plu Then DetCases := 'd' Else DetCases := 'a';
    End
    Else If W = 'dem' Then DetCases := 'd'
    Else DetCases := 'g';
    Exit;
  End;
  If Ending = '' Then
  Begin
    If (Gen = 'm') And (Not Plu) Then DetCases := 'n' Else DetCases := 'na';
  End
  Else If Ending = 'e' Then DetCases := 'na'
  Else If Ending = 'en' Then
  Begin
    If Plu Then DetCases := 'd' Else DetCases := 'a';
  End
  Else If Ending = 'em' Then DetCases := 'd'
  Else If Ending = 'er' Then
  Begin
    If Plu Then DetCases := 'g'
    Else If (Gen = 'm') And (Kind <> 'i') And (Kind <> 'k') And (Kind <> 's')
        Then DetCases := 'n'
    Else DetCases := 'dg';
  End
  Else If Ending = 'es' Then
  Begin
    If (Gen = 'n') And (Kind <> 'i') And (Kind <> 'k') And (Kind <> 's') Then
        DetCases := 'na'
    Else DetCases := 'g';
  End;
End;

Function DePossEn(Const Stem: String): String;
Begin
  If Stem = 'mein' Then DePossEn := 'my'
  Else If Stem = 'dein' Then DePossEn := 'your'
  Else If Stem = 'sein' Then DePossEn := 'his'
  Else If Stem = 'ihr' Then DePossEn := 'her'
  Else If Stem = 'unser' Then DePossEn := 'our'
  Else DePossEn := 'your';
End;

Function DeQuantEn(Const Stem, Ending: String): String;
Begin
  If Stem = 'jed' Then DeQuantEn := 'each'
  Else If Stem = 'all' Then DeQuantEn := 'all'
  Else If Stem = 'manch' Then DeQuantEn := 'some'
  Else If Stem = 'viel' Then DeQuantEn := 'many'
  Else If Stem = 'wenig' Then DeQuantEn := 'few'
  Else If Stem = 'einig' Then DeQuantEn := 'some'
  Else If Stem = 'mehrer' Then DeQuantEn := 'several'
  Else If Stem = 'beid' Then DeQuantEn := 'both'
  Else If Stem = 'solch' Then DeQuantEn := 'such'
  Else DeQuantEn := 'some';
  If Ending = '#' Then DeQuantEn := '';
End;

Function DePronounInfo(Const W: String; Var T: TGTok): Boolean;
Begin
  DePronounInfo := True;
  T.Slot := 0;
  If W = 'ich' Then Begin T.En := 'I'; T.EnObj := 'me'; T.Cases := 'n'; T.Slot
      := 1; End
  Else If W = 'mich' Then Begin T.En := 'me'; T.EnObj := 'me'; T.Cases := 'a';
      T.Slot := 1; End
  Else If W = 'mir' Then Begin T.En := 'me'; T.EnObj := 'me'; T.Cases := 'd';
      T.Slot := 1; End
  Else If W = 'du' Then Begin T.En := 'you'; T.EnObj := 'you'; T.Cases := 'n';
      T.Slot := 2; End
  Else If W = 'dich' Then Begin T.En := 'you'; T.EnObj := 'you'; T.Cases := 'a';
      T.Slot := 2; End
  Else If W = 'dir' Then Begin T.En := 'you'; T.EnObj := 'you'; T.Cases := 'd';
      T.Slot := 2; End
  Else If W = 'er' Then Begin T.En := 'he'; T.EnObj := 'him'; T.Cases := 'n';
      T.Slot := 3; End
  Else If W = 'ihn' Then Begin T.En := 'him'; T.EnObj := 'him'; T.Cases := 'a';
      T.Slot := 3; End
  Else If W = 'ihm' Then Begin T.En := 'him'; T.EnObj := 'him'; T.Cases := 'd';
      T.Slot := 3; End
  Else If W = 'es' Then Begin T.En := 'it'; T.EnObj := 'it'; T.Cases := 'na';
      T.Slot := 3; End
  Else If W = 'wir' Then Begin T.En := 'we'; T.EnObj := 'us'; T.Cases := 'n';
      T.Slot := 4; End
  Else If W = 'uns' Then Begin T.En := 'us'; T.EnObj := 'us'; T.Cases := 'ad';
      T.Slot := 4; End
  Else If W = 'euch' Then Begin T.En := 'you'; T.EnObj := 'you'; T.Cases :=
      'ad'; T.Slot := 5; End
  Else If W = 'ihnen' Then Begin T.En := 'them'; T.EnObj := 'them'; T.Cases :=
      'd'; T.Slot := 6; End
  Else If W = 'sie' Then Begin T.En := 'she'; T.EnObj := 'her'; T.Cases := 'na';
      T.Slot := 3; End
  Else If W = 'man' Then Begin T.En := 'one'; T.EnObj := 'one'; T.Cases := 'n';
      T.Slot := 3; End
  Else DePronounInfo := False;
End;

Function DeWhEn(Const W: String): String;
Begin
  If W = 'wer' Then DeWhEn := 'who'
  Else If W = 'wen' Then DeWhEn := 'whom'
  Else If W = 'wem' Then DeWhEn := 'whom'
  Else If W = 'was' Then DeWhEn := 'what'
  Else If W = 'wo' Then DeWhEn := 'where'
  Else If W = 'wann' Then DeWhEn := 'when'
  Else If (W = 'warum') Or (W = 'wieso') Then DeWhEn := 'why'
  Else If W = 'wie' Then DeWhEn := 'how'
  Else If W = 'woher' Then DeWhEn := 'where from'
  Else If W = 'wohin' Then DeWhEn := 'where'
  Else If W = 'wessen' Then DeWhEn := 'whose'
  Else DeWhEn := '';
End;

Function DeConjEn(Const W: String; Var Sub: Boolean): String;
Begin
  Sub := False;
  DeConjEn := '';
  If W = 'und' Then DeConjEn := 'and'
  Else If W = 'oder' Then DeConjEn := 'or'
  Else If W = 'aber' Then DeConjEn := 'but'
  Else If W = 'sondern' Then DeConjEn := 'but'
  Else If W = 'denn' Then DeConjEn := 'because'
  Else
  Begin
    Sub := True;
    If W = 'weil' Then DeConjEn := 'because'
    Else If W = 'dass' Then DeConjEn := 'that'
    Else If W = 'wenn' Then DeConjEn := 'if'
    Else If W = 'ob' Then DeConjEn := 'whether'
    Else If W = 'als' Then DeConjEn := 'when'
    Else If W = 'obwohl' Then DeConjEn := 'although'
    Else If W = 'während' Then DeConjEn := 'while'
    Else If W = 'bevor' Then DeConjEn := 'before'
    Else If W = 'nachdem' Then DeConjEn := 'after'
    Else If W = 'damit' Then DeConjEn := 'so that'
    Else
      Sub := False;
  End;
End;

Function DePrepEn(Const W: String; L: Integer): String;
Begin
  If W = 'zu' Then DePrepEn := 'to'
  Else If W = 'nach' Then DePrepEn := 'to'
  Else If W = 'von' Then DePrepEn := 'of'
  Else If W = 'bei' Then DePrepEn := 'at'
  Else If W = 'an' Then DePrepEn := 'at'
  Else If W = 'um' Then DePrepEn := 'at'
  Else If W = 'aus' Then DePrepEn := 'from'
  Else If W = 'seit' Then DePrepEn := 'since'
  Else If W = 'für' Then DePrepEn := 'for'
  Else If W = 'mit' Then DePrepEn := 'with'
  Else If W = 'ohne' Then DePrepEn := 'without'
  Else If W = 'auf' Then DePrepEn := 'on'
  Else If W = 'in' Then DePrepEn := 'in'
  Else If W = 'über' Then DePrepEn := 'over'
  Else If W = 'unter' Then DePrepEn := 'under'
  Else If W = 'vor' Then DePrepEn := 'in front of'
  Else If W = 'hinter' Then DePrepEn := 'behind'
  Else If W = 'neben' Then DePrepEn := 'next to'
  Else If W = 'zwischen' Then DePrepEn := 'between'
  Else If W = 'durch' Then DePrepEn := 'through'
  Else If W = 'gegen' Then DePrepEn := 'against'
  Else If W = 'bis' Then DePrepEn := 'until'
  Else If L > 0 Then DePrepEn := Lex[L].En
  Else DePrepEn := W;
End;

Function NextDeIsVerbish(I: Integer): Boolean;
Var
  T: TGTok;
  W: String;
Begin
  NextDeIsVerbish := False;
  If I > GN Then Exit;
  If GT[I].Punct Then Exit;
  If GT[I].Cap Then Exit;
  W := GT[I].W;
  FillChar(T, SizeOf(T), 0);
  NextDeIsVerbish := DeMatchVerb(W, T) And (T.CanInf);
End;

Function NextDeIsNounish(I: Integer): Boolean;
Var
  Plu: Boolean;
  Ending, Stem: String;
  K: Char;
  W: String;
Begin
  NextDeIsNounish := False;
  If I > GN Then Exit;
  If GT[I].Punct Then Exit;
  W := GT[I].W;
  If GT[I].Cap Then
  Begin
    NextDeIsNounish := True;
    Exit;
  End;
  If DeDetInfo(W, K, Ending, Stem) Then
  Begin
    NextDeIsNounish := True;
    Exit;
  End;
  If DeFindAdj(W, Ending) > 0 Then NextDeIsNounish := True;
  Plu := False;
End;

Procedure PickDeCat(I: Integer);
Var
  W, Ending, Stem, Orig: String;
  Kind: Char;
  L, K: Integer;
  Plu, Sub: Boolean;
  T: TGTok;
  PrevCat: Char;
  T2: TGTok;
  HasNoun, HasAdj, HasVerb, HasAdv, HasPrep: Boolean;
  LN, LA, LR, LP: Integer;
  Eg: String;
Begin
  W := GT[I].W;
  Orig := GT[I].Orig;
  If I > 1 Then PrevCat := GT[I - 1].Cat Else PrevCat := '.';
  If GT[I].Punct Then
  Begin
    GT[I].Cat := ',';
    Exit;
  End;
  If IsDigitsOnly(W) Then
  Begin
    GT[I].Cat := 'U';
    GT[I].En := W;
    Exit;
  End;
  If (DeNumberEnOf(W) <> '') And (W <> 'eins') Then
  Begin
    GT[I].Cat := 'U';
    GT[I].En := DeNumberEnOf(W);
    Exit;
  End;
  If W = 'nicht' Then
  Begin
    GT[I].Cat := 'G';
    Exit;
  End;
  If (W = 'nie') Or (W = 'niemals') Then
  Begin
    GT[I].Cat := 'R';
    GT[I].En := 'never';
    Exit;
  End;
  If W = 'sich' Then
  Begin
    GT[I].Cat := 'O';
    Exit;
  End;
  If DeConjEn(W, Sub) <> '' Then
  Begin
    GT[I].Cat := 'C';
    GT[I].En := DeConjEn(W, Sub);
    GT[I].Kind := 'c';
    If Sub Then GT[I].Kind := 'S';
    Exit;
  End;
  If (DeWhEn(W) <> '') And ((W <> 'wie') Or Not ((I < GN) And (GT[I + 1].W =
      'viele'))) Then
  Begin
    GT[I].Cat := 'W';
    GT[I].En := DeWhEn(W);
    Exit;
  End;
  If (W = 'wie') Then
  Begin
    GT[I].Cat := 'W';
    GT[I].En := 'how';
    Exit;
  End;
  { pronoms }
  If DePronounInfo(W, GT[I]) Then
  Begin
    GT[I].Cat := 'P';
    If ((W = 'sie') Or (W = 'ihnen')) And (I > 1) And (Orig[1] = 'S') Or (Orig =
        'Ihnen') Then
    Begin
      GT[I].En := 'you';
      GT[I].EnObj := 'you';
      GT[I].Slot := 6;
    End;
    Exit;
  End;
  If (W = 'ihr') Then
  Begin
    If NextDeIsNounish(I + 1) Then
    Begin
      GT[I].Cat := 'D';
      GT[I].Kind := 's';
      GT[I].En := 'her';
      If (Orig = 'Ihr') And (I > 1) Then GT[I].En := 'your';
      GT[I].Slot := 0;
    End
    Else
    Begin
      GT[I].Cat := 'P';
      GT[I].En := 'you';
      GT[I].EnObj := 'her';
      GT[I].Cases := 'nd';
      GT[I].Slot := 5;
    End;
    Exit;
  End;
  If (W = 'morgen') And (I = 1) Then
  Begin
    GT[I].Cat := 'R';
    GT[I].En := 'tomorrow';
    Exit;
  End;
  If W = 'gern' Then
  Begin
    GT[I].Cat := 'R';
    GT[I].En := 'gladly';
    Exit;
  End;
  If ((W = 'das') Or (W = 'dies') Or (W = 'dieses')) And (Not NextDeIsNounish(I
      + 1)) Then
  Begin
    GT[I].Cat := 'P';
    GT[I].En := 'this';
    GT[I].EnObj := 'this';
    GT[I].Cases := 'na';
    GT[I].Slot := 3;
    Exit;
  End;
  { determinants }
  If DeDetInfo(W, Kind, Ending, Stem) Then
  Begin
    If (W = 'sein') And (Not NextDeIsNounish(I + 1)) Then
      Kind := 'v'
    Else If (W = 'viel') Or (W = 'wenig') Then
      Kind := 'v'
    Else
    Begin
      GT[I].Cat := 'D';
      GT[I].Kind := Kind;
      GT[I].Slot := 0;
      GT[I].Cases := Ending;
      If Kind = 'd' Then GT[I].En := 'the'
      Else If Kind = 'i' Then GT[I].En := 'a'
      Else If Kind = 'k' Then GT[I].En := 'no'
      Else If Kind = 's' Then
      Begin
        GT[I].En := DePossEn(Stem);
        If (Stem = 'ihr') And (Orig = 'Ihr') Then GT[I].En := 'your';
        If (Stem = 'ihr') And (Orig[1] = 'I') Then GT[I].En := 'your';
      End
      Else If Kind = 't' Then GT[I].En := 'this'
      Else If Kind = 'w' Then GT[I].En := 'which'
      Else GT[I].En := DeQuantEn(Stem, Ending);
      GT[I].EnObj := Stem;
      Exit;
    End;
  End;
  { zu + infinitif }
  If (W = 'zu') And NextDeIsVerbish(I + 1) Then
  Begin
    GT[I].Cat := 'K';
    Exit;
  End;
  { classes lexicales }
  FillChar(T, SizeOf(T), 0);
  HasNoun := False;
  LN := 0;
  If GT[I].Cap Or (I = 1) Then
  Begin
    LN := DeFindNoun(W, Plu);
    If (LN = 0) And EndsWith(W, 'in') And (Length(W) > 4) Then LN :=
        DeFindNoun(Copy(W, 1, Length(W) - 2), Plu);
    If (LN = 0) And EndsWith(W, 'innen') And (Length(W) > 7) Then
    Begin
      LN := DeFindNoun(Copy(W, 1, Length(W) - 5), Plu);
      Plu := True;
    End;
    HasNoun := LN > 0;
  End;
  LP := DeLexExact(W, 'p');
  HasPrep := (LP > 0) And Not GT[I].Cap;
  HasVerb := False;
  If Not GT[I].Cap Or (I = 1) Then HasVerb := DeMatchVerb(W, T);
  LA := 0;
  Eg := '';
  If Not GT[I].Cap Then LA := DeFindAdj(W, Eg);
  HasAdj := LA > 0;
  LR := 0;
  If Not GT[I].Cap Or (I = 1) Then LR := DeLexExact(W, 'r');
  HasAdv := LR > 0;
  If GT[I].Cap And (I > 1) And (Not HasNoun) Then
  Begin
    GT[I].Cat := 'Z';
    Exit;
  End;
  { choix }
  If HasNoun And HasVerb And (I = 1) And (GN >= 2) Then
  Begin
    FillChar(T2, SizeOf(T2), 0);
    If DePronounInfo(GT[2].W, T2) Then HasNoun := False;
  End;
  If HasNoun Then
  Begin
    GT[I].Cat := 'N';
    GT[I].L := LN;
    GT[I].Plu := Plu;
    GT[I].Gen := Lex[LN].DeG;
    If GT[I].Gen = 'P' Then
    Begin
      GT[I].Gen := 'n';
      GT[I].Plu := True;
    End;
    GT[I].Cases := 'nadg';
    GT[I].En := Lex[LN].En;
    Exit;
  End;
  If HasPrep Then
  Begin
    GT[I].Cat := 'E';
    GT[I].L := LP;
    GT[I].En := DePrepEn(W, LP);
    Exit;
  End;
  If HasVerb And HasAdj And (PrevCat In ['D', 'A', 'U']) Then HasVerb := False;
  If HasVerb And HasAdv And (PrevCat = 'V') And (T.CanInf = False) Then HasVerb
      := False;
  If HasVerb Then
  Begin
    GT[I].Cat := 'V';
    GT[I].L := T.L;
    GT[I].LInf := T.LInf;
    GT[I].LPart := T.LPart;
    GT[I].CanInf := T.CanInf;
    GT[I].CanPart := T.CanPart;
    GT[I].Tns := T.Tns;
    GT[I].Slot := T.Slot;
    If T.L > 0 Then GT[I].En := DeEnglishOf(T.L)
    Else If T.LInf > 0 Then GT[I].En := DeEnglishOf(T.LInf)
    Else GT[I].En := DeEnglishOf(T.LPart);
    Exit;
  End;
  If HasAdj And HasAdv Then
  Begin
    If (PrevCat = 'V') Or NextDeIsNounish(I + 1) Then HasAdv := False Else
        HasAdj := False;
  End;
  If HasAdj Then
  Begin
    GT[I].Cat := 'A';
    GT[I].L := LA;
    GT[I].En := Lex[LA].En;
    GT[I].Cases := Eg;
    Exit;
  End;
  If HasAdv Then
  Begin
    GT[I].Cat := 'R';
    GT[I].L := LR;
    GT[I].En := Lex[LR].En;
    Exit;
  End;
  K := DeLexExact(W, 'j');
  If K > 0 Then
  Begin
    GT[I].Cat := 'J';
    GT[I].L := K;
    GT[I].En := Lex[K].En;
    Exit;
  End;
  K := DeLexExact(W, 'c');
  If K > 0 Then
  Begin
    GT[I].Cat := 'C';
    GT[I].L := K;
    GT[I].En := Lex[K].En;
    GT[I].Kind := 'c';
    If Lex[K].DeFlags = 'S' Then GT[I].Kind := 'S';
    Exit;
  End;
  GT[I].Cat := 'Z';
End;

Procedure DeClassify;
Var
  I: Integer;
Begin
  GMergeMultiword;
  For I := 1 To GN Do
    PickDeCat(I);
End;

{ ------------------------------------------------------------------ }
{ ALLEMAND -> ANGLAIS : groupes nominaux et elements                  }
{ ------------------------------------------------------------------ }

Type
  TDNP = Record
    Found, Proper, Plural, Neg, Pron: Boolean;
    Used: Integer;
    Kind: Char;
    DetTok, Head, PronTok: Integer;
    Adjs: Array[1..4] Of Integer;
    AdjAdv: Array[1..4] Of String[10];
    AdjN: Integer;
    Gen: Char;
    Cases: String[4];
    Text: String[60];
  End;

  TDItem = Record
    Kind: Char; { N groupe nominal, E groupe prepositionnel, A adjectif, R
        adverbe, G nicht, O reflechi }
    Cases: String[4];
    Text, TextObj: String[90];
    Pron, Plural, Neg, Proper: Boolean;
    First, Last: Integer;
    Person: Integer;
    PronTok: Integer;
    HeadEn: String[30];
  End;

Var
  DI: Array[1..24] Of TDItem;
  DIN: Integer;

Function InterCases(Const A, B: String): String;
Var
  I: Integer;
  R: String;
Begin
  R := '';
  For I := 1 To Length(A) Do
    If Pos(A[I], B) > 0 Then R := R + A[I];
  If R = '' Then R := A;
  InterCases := R;
End;

Function ParseNPIn(Start: Integer; Var NP: TDNP): Boolean;
Var
  I: Integer;
  Ending: String;
Begin
  FillChar(NP, SizeOf(NP), 0);
  NP.Kind := '0';
  NP.Gen := 'm';
  NP.Cases := 'nadg';
  ParseNPIn := False;
  I := Start;
  If I > GN Then Exit;
  If GT[I].Cat = 'P' Then
  Begin
    NP.Found := True;
    NP.Pron := True;
    NP.PronTok := I;
    NP.Cases := GT[I].Cases;
    NP.Used := 1;
    ParseNPIn := True;
    Exit;
  End;
  If GT[I].Cat = 'D' Then
  Begin
    NP.DetTok := I;
    NP.Kind := GT[I].Kind;
    If NP.Kind = 'k' Then NP.Neg := True;
    Inc(I);
  End
  Else If GT[I].Cat = 'U' Then
  Begin
    NP.Kind := 'n';
    NP.Text := GT[I].En;
    NP.Plural := GT[I].En <> 'one';
    Inc(I);
  End;
  While I <= GN Do
  Begin
    If GT[I].Cat = 'A' Then
    Begin
      If NP.AdjN < 4 Then
      Begin
        Inc(NP.AdjN);
        NP.Adjs[NP.AdjN] := I;
      End;
      Inc(I);
    End
    Else If (GT[I].Cat = 'R') And (I < GN) And (GT[I + 1].Cat = 'A') Then
    Begin
      If NP.AdjN < 4 Then
      Begin
        Inc(NP.AdjN);
        NP.Adjs[NP.AdjN] := I + 1;
        NP.AdjAdv[NP.AdjN] := GT[I].En;
      End;
      Inc(I, 2);
    End
    Else
      Break;
  End;
  If (I <= GN) And (GT[I].Cat = 'N') Then
  Begin
    NP.Head := I;
    NP.Plural := GT[I].Plu;
    NP.Gen := GT[I].Gen;
    If (NP.Gen <> 'm') And (NP.Gen <> 'f') Then NP.Gen := 'n';
    Inc(I);
    If NP.DetTok > 0 Then
    Begin
      Ending := GT[NP.DetTok].Cases;
      NP.Cases := DetCases(GT[NP.DetTok].W, NP.Kind, Ending, NP.Gen, NP.Plural);
    End;
  End
  Else If (I <= GN) And (GT[I].Cat = 'Z') Then
  Begin
    NP.Proper := True;
    While (I <= GN) And (GT[I].Cat = 'Z') Do
    Begin
      If DeProperEnOf(GT[I].Orig) <> '' Then
        NP.Text := Join(NP.Text, DeProperEnOf(GT[I].Orig))
      Else
        NP.Text := Join(NP.Text, GT[I].Orig);
      Inc(I);
    End;
  End
  Else If (NP.Kind = 'n') And (NP.AdjN = 0) Then
  Begin
  End
  Else If (NP.Kind = 'q') And (NP.AdjN = 0) Then
  Begin
  End
  Else
    Exit;
  If NP.Kind = 'q' Then NP.Plural := NP.Plural Or InList(GT[NP.DetTok].EnObj,
      'all viel wenig einig mehrer beid');
  NP.Found := True;
  NP.Used := I - Start;
  ParseNPIn := True;
End;

Function DeAdjText(Const NP: TDNP): String;
Var
  K: Integer;
  R, A: String;
Begin
  R := '';
  For K := 1 To NP.AdjN Do
  Begin
    A := GT[NP.Adjs[K]].En;
    If NP.AdjAdv[K] <> '' Then A := NP.AdjAdv[K] + ' ' + A;
    R := Join(R, A);
  End;
  DeAdjText := R;
End;

Function DeNounText(Const NP: TDNP): String;
Var
  N: String;
Begin
  N := GT[NP.Head].En;
  If NP.Plural And ((Not IsMassNoun(N)) Or (NP.Kind = 'n')) Then N :=
      EnPlural(N);
  If IsCalendar(N) And (Not InList(N, 'may')) Then N := UpperFirst(N);
  DeNounText := N;
End;

Function RenderNPIn(Const NP: TDNP; AsObject: Boolean): String;
Var
  Det, Core: String;
Begin
  If NP.Pron Then
  Begin
    If AsObject Then RenderNPIn := GT[NP.PronTok].EnObj Else RenderNPIn :=
        GT[NP.PronTok].En;
    Exit;
  End;
  If NP.Proper Then
  Begin
    RenderNPIn := NP.Text;
    Exit;
  End;
  If NP.Head = 0 Then
  Begin
    If NP.DetTok > 0 Then RenderNPIn := GT[NP.DetTok].En Else RenderNPIn :=
        NP.Text;
    Exit;
  End;
  Core := Join(DeAdjText(NP), DeNounText(NP));
  Det := '';
  Case NP.Kind Of
    'd': Det := 'the';
    'i':
      If Not NP.Plural Then Det := 'a';
    'k':
      If NP.Plural Or IsMassNoun(GT[NP.Head].En) Then Det := 'any' Else Det :=
          'a';
    's': Det := GT[NP.DetTok].En;
    't':
      If NP.Plural Then Det := 'these' Else Det := 'this';
    'n': Det := NP.Text;
    'q', 'w': Det := GT[NP.DetTok].En;
  End;
  If Det = 'a' Then Det := EnArticle(Core);
  RenderNPIn := Join(Det, Core);
End;

{ Les elements d'une proposition : groupes nominaux, prepositionnels, adjectifs,
  adverbes. }
Procedure ParseItems(A, B: Integer);
Var
  I, J: Integer;
  NP, NP2: TDNP;
  Prep, Txt: String;

  Function NewItem(K: Char): Integer;
  Begin
    If DIN < 24 Then Inc(DIN);
    FillChar(DI[DIN], SizeOf(TDItem), 0);
    DI[DIN].Kind := K;
    DI[DIN].Cases := 'nadg';
    DI[DIN].First := I;
    NewItem := DIN;
  End;
Begin
  DIN := 0;
  I := A;
  While I <= B Do
  Begin
    Case GT[I].Cat Of
      'D', 'N', 'U', 'P':
      Begin
        If ParseNPIn(I, NP) Then
        Begin
          J := NewItem('N');
          If (NP.Head > 0) Or NP.Pron Then
            If (I + NP.Used + 1 <= B) And (GT[I + NP.Used].Cat = 'C') And (GT[I
                + NP.Used].Kind = 'c') And
               (GT[I + NP.Used].En <> 'but') And (GT[I + NP.Used + 1].Cat In
                   ['D', 'N', 'U']) And
               ParseNPIn(I + NP.Used + 1, NP2) Then
            Begin
              DI[J].Cases := InterCases(NP.Cases, NP2.Cases);
              DI[J].Text := RenderNPIn(NP, False) + ' ' + GT[I + NP.Used].En +
                  ' ' + RenderNPIn(NP2, False);
              DI[J].TextObj := RenderNPIn(NP, True) + ' ' + GT[I + NP.Used].En +
                  ' ' + RenderNPIn(NP2, True);
              DI[J].Plural := True;
              DI[J].Person := 6;
              DI[J].Last := I + NP.Used + NP2.Used;
              Inc(I, NP.Used + 1 + NP2.Used);
              Continue;
            End;
          DI[J].Cases := NP.Cases;
          DI[J].Text := RenderNPIn(NP, False);
          DI[J].TextObj := RenderNPIn(NP, True);
          DI[J].Pron := NP.Pron;
          DI[J].Plural := NP.Plural;
          DI[J].Neg := NP.Neg;
          DI[J].Proper := NP.Proper;
          DI[J].PronTok := NP.PronTok;
          If NP.Head > 0 Then DI[J].HeadEn := GT[NP.Head].En;
          If NP.Pron Then DI[J].Person := GT[NP.PronTok].Slot Else If NP.Plural
              Then DI[J].Person := 6 Else DI[J].Person := 3;
          DI[J].Last := I + NP.Used - 1;
          Inc(I, NP.Used);
        End
        Else
          Inc(I);
      End;
      'Z':
      Begin
        If ParseNPIn(I, NP) Then
        Begin
          J := NewItem('N');
          DI[J].Text := NP.Text;
          DI[J].TextObj := NP.Text;
          DI[J].Proper := True;
          DI[J].Person := 3;
          DI[J].Last := I + NP.Used - 1;
          Inc(I, NP.Used);
        End
        Else Inc(I);
      End;
      'E':
      Begin
        Prep := GT[I].En;
        J := I + 1;
        If (GT[I].W = 'nach') And (J <= B) And (GT[J].W = 'hause') Then
        Begin
          J := NewItem('E');
          If DePivot Then DI[J].Text := 'to the house' Else DI[J].Text :=
              'home';
          DI[J].Last := I + 1;
          Inc(I, 2);
          Continue;
        End;
        { l'heure : um 7 Uhr }
        If (GT[I].W = 'um') And (J + 1 <= B) And (GT[J].Cat = 'U') And (GT[J +
            1].W = 'uhr') Then
        Begin
          J := NewItem('E');
          Txt := GT[I + 1].En;
          If Txt = 'one' Then Txt := '1' Else If Txt = 'two' Then Txt := '2'
              Else If Txt = 'three' Then Txt := '3'
          Else If Txt = 'four' Then Txt := '4' Else If Txt = 'five' Then Txt :=
              '5' Else If Txt = 'six' Then Txt := '6'
          Else If Txt = 'seven' Then Txt := '7' Else If Txt = 'eight' Then Txt
              := '8' Else If Txt = 'nine' Then Txt := '9'
          Else If Txt = 'ten' Then Txt := '10' Else If Txt = 'eleven' Then Txt
              := '11' Else If Txt = 'twelve' Then Txt := '12';
          DI[J].Text := 'at ' + Txt + ' o''clock';
          DI[J].Last := I + 2;
          Inc(I, 3);
        End
        Else If (J <= B) And (GT[J].Cat In ['D', 'N', 'U', 'P', 'A', 'Z']) And
            ParseNPIn(J, NP) Then
        Begin
          If (NP.Kind = 'd') And (NP.Head > 0) And (Not NP.Plural) And (NP.AdjN
              = 0) And (Not DePivot) And
             InList(Prep, 'to at in') And InList(GT[NP.Head].En,
                 'school church bed work prison university') Then
            NP.Kind := '0';
          If (GT[I].W = 'mit') And (NP.Head > 0) And (Not NP.Plural) And
             InList(GT[NP.Head].En, 'bus car train plane bike boat') Then
          Begin
            Prep := 'by';
            NP.Kind := '0';
          End;
          Txt := RenderNPIn(NP, True);
          If (GT[I].W = 'an') And (NP.Head > 0) Then
            If InList(GT[NP.Head].En,
                'monday tuesday wednesday thursday friday saturday sunday') Then
                Prep := 'on';
          If (GT[I].W = 'nach') And (NP.Head > 0) Then Prep := 'after';
          If (GT[I].W = 'von') And (NP.Head = 0) And NP.Proper Then Prep :=
              'from';
          If (GT[I].W = 'um') Then Prep := 'around';
          If (GT[I].W = 'zu') And (NP.Head > 0) And (GT[NP.Head].En = 'home')
              Then Prep := 'at';
          J := NewItem('E');
          DI[J].Text := Join(Prep, Txt);
          DI[J].Last := I + NP.Used;
          Inc(I, 1 + NP.Used);
        End
        Else
        Begin
          J := NewItem('E');
          DI[J].Text := Prep;
          DI[J].Last := I;
          Inc(I);
        End;
      End;
      'A':
      Begin
        If ParseNPIn(I, NP) Then
        Begin
          J := NewItem('N');
          DI[J].Text := RenderNPIn(NP, False);
          DI[J].TextObj := DI[J].Text;
          DI[J].Person := 3;
          DI[J].Last := I + NP.Used - 1;
          Inc(I, NP.Used);
        End
        Else
        Begin
          J := NewItem('A');
          DI[J].Text := GT[I].En;
          DI[J].Last := I;
          Inc(I);
        End;
      End;
      'R':
      Begin
        If (I < B) And (GT[I + 1].Cat = 'A') And (GT[I].En = 'very') Then
        Begin
          J := NewItem('A');
          DI[J].Text := 'very ' + GT[I + 1].En;
          DI[J].Last := I + 1;
          Inc(I, 2);
        End
        Else
        Begin
          J := NewItem('R');
          DI[J].Text := GT[I].En;
          DI[J].Last := I;
          Inc(I);
        End;
      End;
      'G':
      Begin
        J := NewItem('G');
        DI[J].Last := I;
        Inc(I);
      End;
      'O':
      Begin
        J := NewItem('O');
        DI[J].Last := I;
        Inc(I);
      End;
      'W':
      Begin
        J := NewItem('R');
        DI[J].Text := GT[I].En;
        DI[J].Last := I;
        Inc(I);
      End;
      'C':
      Begin
        J := NewItem('C');
        DI[J].Text := GT[I].En;
        DI[J].Last := I;
        Inc(I);
      End;
    Else
      Inc(I);
    End;
  End;
End;

Const
  PhrasesDeIn: Array[0..39] Of String = (
    'guten morgen|Good morning',
    'guten tag|Hello',
    'guten abend|Good evening',
    'gute nacht|Good night',
    'vielen dank|Thank you very much',
    'danke schön|Thank you very much',
    'bitte schön|You are welcome',
    'wie geht es ihnen|How are you',
    'wie geht es dir|How are you',
    'wie geht''s|How are you',
    'wie heißen sie|What is your name',
    'wie heißt du|What is your name',
    'wie alt sind sie|How old are you',
    'wie alt bist du|How old are you',
    'woher kommen sie|Where are you from',
    'woher kommst du|Where are you from',
    'wie spät ist es|What time is it',
    'wie viel kostet es|How much does it cost',
    'es tut mir leid|I am sorry',
    'ich weiß es nicht|I do not know',
    'ich weiß nicht|I do not know',
    'ich verstehe nicht|I do not understand',
    'ich liebe dich|I love you',
    'ich vermisse dich|I miss you',
    'bis morgen|See you tomorrow',
    'bis bald|See you soon',
    'bis später|See you later',
    'auf wiedersehen|Goodbye',
    'viel glück|Good luck',
    'alles gute zum geburtstag|Happy birthday',
    'frohe weihnachten|Merry Christmas',
    'prost|Cheers',
    'entschuldigung|Excuse me',
    'mir geht es gut|I am fine',
    'es geht mir gut|I am fine',
    'ich habe keine zeit|I do not have time',
    'ich spreche kein deutsch|I do not speak German',
    'sprechen sie englisch|Do you speak English',
    'wo ist die toilette|Where is the bathroom',
    'was ist das|What is this'
  );

Function GPhraseKey(A, B: Integer): String;
Var
  K: Integer;
  R: String;
Begin
  R := '';
  For K := A To B Do
  Begin
    If GT[K].Punct Then
    Begin
      GPhraseKey := '#';
      Exit;
    End;
    R := Join(R, GT[K].W);
  End;
  GPhraseKey := R;
End;

Function FindPhraseDe(Const Key: String): String;
Var
  N: Integer;
  Pair, K: String;
Begin
  FindPhraseDe := '';
  If Key = '#' Then Exit;
  For N := 0 To High(PhrasesDeIn) Do
  Begin
    Pair := PhrasesDeIn[N];
    K := CopyUntil(Pair, '|');
    If K = Key Then
    Begin
      FindPhraseDe := Pair;
      Exit;
    End;
  End;
End;


{ ------------------------------------------------------------------ }
{ ALLEMAND -> ANGLAIS : proposition                                   }
{ ------------------------------------------------------------------ }

Function DeClauseHasFinite(A, B: Integer): Boolean;
Var
  I: Integer;
Begin
  DeClauseHasFinite := False;
  For I := A To B Do
    If (GT[I].Cat = 'V') And (GT[I].L > 0) And (GT[I].Tns In ['p', 'i', 'c'])
        Then
      If (I = 1) Or (GT[I - 1].Cat <> 'K') Then DeClauseHasFinite := True;
End;

Function DeIsFreqEn(Const En: String): Boolean;
Begin
  DeIsFreqEn := InList(En,
      'always never often sometimes usually already just also only still');
End;

Function DeIdiomPred(Const Orig: String): String;
Begin
  If Orig = 'hunger' Then DeIdiomPred := 'hungry'
  Else If Orig = 'durst' Then DeIdiomPred := 'thirsty'
  Else If Orig = 'angst' Then DeIdiomPred := 'afraid'
  Else If Orig = 'recht' Then DeIdiomPred := 'right'
  Else DeIdiomPred := '';
End;

Procedure DeConsume(I: Integer);
Begin
  GT[I].Cat := ' ';
End;

Function TranslateClauseDeIn(A, B: Integer; IsQ, Final: Boolean): String;
Var
  I, J, K, Vf, PV, IV, SubjIdx, PreIdx: Integer;
  Wh, WhText, Res, Tail, SubjText, Aux, Rest, LexBase, Post, Freq, AuxN, VerbEn,
      Pred, Name: String;
  LexTns: Char;
  G: TFVG;
  NP: TDNP;
  Please, Imperative, WhSubject, DoQ, Neg, Gern, HasSubj, IsThere: Boolean;
  Sp, L, L2: Integer;
  Inf, InfStr, ObjTxt, IObj, DObj, Pp, Adv: String;
  Slot: Integer;
  T: TGTok;
  Ending: String;
  Cnt: Integer;
  Embedded: Boolean;
Begin
  TranslateClauseDeIn := '';
  If A > B Then Exit;
  Embedded := DeEmbed;
  DeEmbed := False;
  Please := False;
  Imperative := False;
  WhSubject := False;
  Wh := '';
  WhText := '';
  Freq := '';
  Neg := False;
  Gern := False;
  IsThere := False;

  { mots de liaison et interjections en tete }
  If (GT[A].Cat = 'J') And (A < B) And (GT[A].W = 'bitte') Then
  Begin
    Please := True;
    DeConsume(A);
    Inc(A);
    If (A <= B) And (GT[A].Cat = ',') Then Inc(A);
  End
  Else If (GT[A].Cat = 'J') And (A < B) Then
  Begin
    TranslateClauseDeIn := GT[A].En + ' ' + TranslateClauseDeIn(A + 1, B, IsQ,
        Final);
    Exit;
  End
  Else If (GT[A].Cat = 'J') And (A = B) Then
  Begin
    TranslateClauseDeIn := GT[A].En;
    Exit;
  End;
  If (GT[A].Cat = 'C') And (A < B) Then
  Begin
    TranslateClauseDeIn := GT[A].En + ' ' + TranslateClauseDeIn(A + 1, B, IsQ,
        GT[A].Kind = 'S');
    Exit;
  End;

  { mot interrogatif }
  If (GT[A].Cat = 'W') Then
  Begin
    WhText := GT[A].En;
    Wh := GT[A].W;
    DeConsume(A);
    If (Wh = 'wie') And (A < B) And (GT[A + 1].Cat = 'D') And (GT[A + 1].EnObj =
        'viel') Then
    Begin
      DeConsume(A + 1);
      If ParseNPIn(A + 2, NP) And (NP.Head > 0) Then
      Begin
        NP.Plural := True;
        WhText := 'how many ' + DeNounText(NP);
        For K := A + 2 To A + 1 + NP.Used Do DeConsume(K);
      End
      Else WhText := 'how many';
    End;
  End
  Else If (GT[A].Cat = 'D') And (GT[A].Kind = 'w') Then
  Begin
    If ParseNPIn(A, NP) And (NP.Head > 0) Then
    Begin
      WhText := 'which ' + Join(DeAdjText(NP), DeNounText(NP));
      Wh := 'welch';
      For K := A To A + NP.Used - 1 Do DeConsume(K);
    End;
  End;
  If Wh <> '' Then IsQ := True;

  { le verbe fini }
  Vf := 0;
  If Final Then
  Begin
    For I := A To B Do
      If (GT[I].Cat = 'V') And (GT[I].L > 0) And (GT[I].Tns In ['p', 'i', 'c'])
          And
         ((I = 1) Or (GT[I - 1].Cat <> 'K')) Then Vf := I;
  End
  Else
  Begin
    For I := B Downto A Do
      If (GT[I].Cat = 'V') And (GT[I].L > 0) And (GT[I].Tns In ['p', 'i', 'c'])
          And
         ((I = 1) Or (GT[I - 1].Cat <> 'K')) Then Vf := I;
  End;
  If Vf = 0 Then
  Begin
    { pas de verbe fini : on traduit les elements }
    ParseItems(A, B);
    Res := '';
    For I := 1 To DIN Do Res := Join(Res, DI[I].Text);
    TranslateClauseDeIn := Join(WhText, Res);
    Exit;
  End;
  L := GT[Vf].L;
  Slot := GT[Vf].Slot;
  Inf := Lex[L].De;

  { prefixe separable : on cherche le verbe avec prefixe qui convient }
  If GT[Vf].Kind <> 'A' Then
  Begin
    L2 := 0;
    For K := 1 To LexCount Do
      If (Lex[K].Pos = 'v') And (Lex[K].De = Inf) And (Lex[K].DeSep <> '') And
          (L2 = 0) Then
        For J := A To B Do
          If (J <> Vf) And (L2 = 0) And (GT[J].Cat <> 'V') And (GT[J].Cat <>
              ' ') And
             (GT[J].W = DeLow(Lex[K].DeSep)) And (J > Vf) And (J >= B - 1) Then
          Begin
            L2 := K;
            DeConsume(J);
          End;
    If L2 > 0 Then L := L2
    Else If Lex[L].DeSep <> '' Then
    Begin
      { le prefixe est absent : verbe simple de meme nom }
      For K := 1 To LexCount Do
        If (Lex[K].Pos = 'v') And (Lex[K].De = Inf) And (Lex[K].DeSep = '') And
            (L2 = 0) Then L2 := K;
      If L2 > 0 Then L := L2;
    End;
  End;
  Inf := Lex[L].De;
  VerbEn := DeEnglishOf(L);
  DeConsume(Vf);

  { les autres verbes : participe ou infinitif }
  PV := 0;
  IV := 0;
  For I := A To B Do
    If (GT[I].Cat = 'V') And (I <> Vf) Then
    Begin
      If GT[I].CanPart And (PV = 0) And ((Inf = 'haben') Or (Inf = 'sein')) Then
      Begin
        PV := I;
        DeConsume(I);
      End
      Else If GT[I].CanInf And (IV = 0) Then
      Begin
        IV := I;
        DeConsume(I);
      End
      Else If GT[I].CanPart And (PV = 0) Then
      Begin
        PV := I;
        DeConsume(I);
      End;
    End;
  { "zu" devant un infinitif }
  For I := A To B Do
    If GT[I].Cat = 'K' Then DeConsume(I);

  ParseItems(A, B);

  { le sujet }
  SubjIdx := 0;
  PreIdx := 0;
  For I := 1 To DIN Do
    If (DI[I].Kind In ['N', 'A']) And (PreIdx = 0) And (DI[I].First < Vf) Then
        PreIdx := I;
  If (PreIdx > 0) And (DI[PreIdx].Kind = 'N') And (Pos('n', DI[PreIdx].Cases) >
      0) And
     (Not Final) Then SubjIdx := PreIdx;
  If SubjIdx = 0 Then
    For I := 1 To DIN Do
      If (SubjIdx = 0) And (DI[I].Kind = 'N') And (Pos('n', DI[I].Cases) > 0)
          Then
        If (DI[I].Text <> '') Then SubjIdx := I;
  HasSubj := SubjIdx > 0;

  { forme de la phrase : question, imperatif }
  If (Not Final) And (Wh = '') Then
    If (DIN = 0) Or (DI[1].First > Vf) Then
    Begin
      { verbe en tete }
      If HasSubj Then
      Begin
        If GEnd = '?' Then IsQ := True
        Else If DI[SubjIdx].Pron Then
        Begin
          If (GT[DI[SubjIdx].PronTok].W = 'wir') Or
             ((GT[DI[SubjIdx].PronTok].W = 'sie') And
                 (GT[DI[SubjIdx].PronTok].Orig = 'Sie')) Then
            Imperative := True;
        End;
      End
      Else If GEnd <> '?' Then
        Imperative := True;
    End;
  If (Not Final) And (GEnd = '?') Then IsQ := True;

  { le sujet interrogatif : wer / was }
  If (Wh = 'wer') Or (Wh = 'was') Then
  Begin
    If Not HasSubj Then WhSubject := True
    Else If (Wh = 'was') And (DI[SubjIdx].Text <> '') Then
    Begin
      { "Was ist das ?" : le sujet est le groupe nominal }
    End;
  End;

  { la construction verbale }
  FillChar(G, SizeOf(G), 0);
  G.Mode := 's';
  G.Tns := GT[Vf].Tns;
  If G.Tns = 'c' Then G.Tns := 'c';
  G.MainEn := VerbEn;
  Neg := False;
  For I := 1 To DIN Do
    If DI[I].Kind = 'G' Then Neg := True;
  For I := 1 To DIN Do
    If (DI[I].Kind = 'N') And DI[I].Neg Then Neg := True;
  G.Neg := Neg;
  For I := 1 To DIN Do
    If (DI[I].Kind = 'R') And DeIsFreqEn(DI[I].Text) Then Freq := Join(Freq,
        DI[I].Text);
  For I := 1 To DIN Do
    If (DI[I].Kind = 'R') And (DI[I].Text = 'never') Then
    Begin
      G.Neg := Neg;
    End;
  For I := 1 To GN Do
    If (GT[I].Cat = 'R') And (GT[I].W = 'gern') Then
    Begin
      Gern := True;
    End;
  If (Inf = 'werden') And (GT[Vf].Tns = 'i') Then
    For I := 1 To GN Do
      If (GT[I].Cat = 'A') And (GT[I].En = 'born') Then
      Begin
        Inf := 'sein';
        G.Tns := 'i';
      End;
  If Inf = 'sein' Then
  Begin
    If (PV > 0) And (GT[PV].LPart > 0) And (Lex[GT[PV].LPart].De = 'sein') Then
    Begin
      G.Mode := 'b';
      G.MainEn := 'be';
      G.Perf := G.Tns = 'p';
    End
    Else If (PV > 0) And (GT[PV].LPart > 0) And DeUsesSein(GT[PV].LPart) Then
    Begin
      G.Mode := 'c';
      G.MainEn := DeEnglishOf(GT[PV].LPart);
    End
    Else If PV > 0 Then
    Begin
      G.Mode := 'p';
      G.MainEn := DeEnglishOf(GT[PV].LPart);
    End
    Else
    Begin
      G.Mode := 'b';
      G.MainEn := 'be';
    End;
  End
  Else If Inf = 'haben' Then
  Begin
    If PV > 0 Then
    Begin
      G.Mode := 'c';
      G.MainEn := DeEnglishOf(GT[PV].LPart);
      If (Pos('already', Freq) > 0) Or (Pos('never', Freq) > 0) Or (Pos('just',
          Freq) > 0) Then G.Perf := True;
    End
    Else
    Begin
      G.Mode := 'h';
      G.MainEn := 'have';
    End;
  End
  Else If Inf = 'werden' Then
  Begin
    If PV > 0 Then
    Begin
      G.Mode := 'p';
      G.MainEn := DeEnglishOf(GT[PV].LPart);
    End
    Else If IV > 0 Then
    Begin
      G.Mode := 's';
      G.MainEn := DeEnglishOf(GT[IV].LInf);
      If G.Tns = 'c' Then G.Tns := 'c' Else G.Tns := 'f';
    End
    Else
    Begin
      G.Mode := 's';
      G.MainEn := 'become';
    End;
  End
  Else If InList(Inf, 'können dürfen müssen sollen wollen möchten mögen')
      Then
  Begin
    If IV > 0 Then
    Begin
      G.Mode := 'm';
      G.MainEn := DeEnglishOf(GT[IV].LInf);
      If (Inf = 'können') Or (Inf = 'dürfen') Then G.Modal := 'pouvoir'
      Else If Inf = 'müssen' Then G.Modal := 'devoir'
      Else If Inf = 'sollen' Then
      Begin
        G.Modal := 'devoir';
        If G.Tns = 'p' Then G.Tns := 'c';
      End
      Else
      Begin
        G.Modal := 'vouloir';
        If Inf = 'möchten' Then G.Tns := 'c';
      End;
    End
    Else
    Begin
      If Inf = 'möchten' Then
      Begin
        G.Mode := 's';
        G.MainEn := 'like';
        G.Tns := 'c';
      End
      Else If Inf = 'mögen' Then G.MainEn := 'like'
      Else If Inf = 'wollen' Then G.MainEn := 'want'
      Else
      Begin
        G.Mode := 'm';
        G.MainEn := '';
        If (Inf = 'können') Or (Inf = 'dürfen') Then G.Modal := 'pouvoir'
        Else G.Modal := 'devoir';
      End;
    End;
  End
  Else If (Inf = 'geben') And (SubjIdx > 0) And DI[SubjIdx].Pron And
      (GT[DI[SubjIdx].PronTok].W = 'es') And
          (Slot = 3) And (G.Tns In ['p', 'i']) Then
  Begin
    G.Mode := 't';
    G.MainEn := 'be';
    IsThere := True;
  End;
  If (G.Mode = 's') And Gern And (IV = 0) Then G.MainEn := 'like to ' +
      G.MainEn;
  If (G.Mode = 's') And (G.MainEn = 'rain') And (G.Tns = 'p') And (Not Final)
      Then G.Rain := True;
  If (G.Mode = 's') And (G.MainEn = 'go') And (G.Tns = 'p') And (Wh = 'wohin')
      Then G.Rain := True;
  If (G.Mode = 's') And (G.MainEn = 'listen') Then G.MainEn := 'listen to';
  If (G.Mode = 's') And (G.MainEn = 'drive') Then
    For I := 1 To DIN Do
      If (DI[I].Kind = 'E') And (Copy(DI[I].Text, 1, 3) = 'by ') Then G.MainEn
          := 'go';
  { Wie geht es Ihnen ? }
  If (Wh = 'wie') And (Inf = 'gehen') And (G.Tns = 'p') And HasSubj Then
  Begin
    For I := 1 To DIN Do
      If (I <> SubjIdx) And (DI[I].Kind = 'N') And DI[I].Pron Then
      Begin
        If DI[I].TextObj = 'me' Then Res := 'am I'
        Else If DI[I].TextObj = 'him' Then Res := 'is he'
        Else If DI[I].TextObj = 'her' Then Res := 'is she'
        Else If DI[I].TextObj = 'it' Then Res := 'is it'
        Else If DI[I].TextObj = 'us' Then Res := 'are we'
        Else If DI[I].TextObj = 'them' Then Res := 'are they'
        Else Res := 'are you';
        Adv := '';
        For K := 1 To DIN Do
          If DI[K].Kind = 'R' Then Adv := Join(Adv, DI[K].Text);
        TranslateClauseDeIn := Join('how ' + Res, Adv);
        Exit;
      End;
  End;

  { avoir faim, soif... }
  Pred := '';
  If G.Mode = 'h' Then
    For I := 1 To GN Do
      If (GT[I].Cat In ['N', 'Z', 'A', 'R']) And (DeIdiomPred(GT[I].W) <> '')
          Then
      Begin
        Pred := DeIdiomPred(GT[I].W);
        G.Mode := 'b';
        G.MainEn := 'be';
        DeConsume(I);
        ParseItems(A, B);
        SubjIdx := 0;
        For K := 1 To DIN Do
          If (SubjIdx = 0) And (DI[K].Kind = 'N') And (Pos('n', DI[K].Cases) >
              0) And (DI[K].Text <> '') Then SubjIdx := K;
      End;

  HasSubj := SubjIdx > 0;
  { "Ich heisse X" }
  If Inf = 'heißen' Then
  Begin
    Name := '';
    For I := 1 To DIN Do
      If (DI[I].Kind = 'N') And (I <> SubjIdx) Then Name := Join(Name,
          DI[I].Text);
    If IsQ Then
      TranslateClauseDeIn := 'What is your name'
    Else If (SubjIdx > 0) And DI[SubjIdx].Pron Then
    Begin
      Sp := GT[DI[SubjIdx].PronTok].Slot;
      If Sp = 1 Then Res := 'My name is'
      Else If Sp = 3 Then
      Begin
        If DI[SubjIdx].Text = 'she' Then Res := 'Her name is' Else Res :=
            'His name is';
      End
      Else Res := 'Your name is';
      TranslateClauseDeIn := Join(Res, Name);
    End
    Else If SubjIdx > 0 Then
      TranslateClauseDeIn := Join(Join(DI[SubjIdx].Text, 'is called'), Name)
    Else
      TranslateClauseDeIn := Name;
    Exit;
  End;

  { sujet : texte et personne }
  SubjText := '';
  Sp := 3;
  If HasSubj Then
  Begin
    SubjText := DI[SubjIdx].Text;
    If DI[SubjIdx].Pron Then
    Begin
      Sp := GT[DI[SubjIdx].PronTok].Slot;
      If GT[DI[SubjIdx].PronTok].W = 'sie' Then
      Begin
        If (Slot In [4, 6]) And (SubjText = 'she') Then
        Begin
          SubjText := 'they';
          Sp := 6;
        End;
        If SubjText = 'you' Then Sp := 2;
      End;
      If Sp = 0 Then Sp := 3;
    End
    Else If DI[SubjIdx].Plural Then Sp := 6
    Else Sp := 3;
    If (SubjText = 'you') Then Sp := 2;
    If (SubjText = 'they') Then Sp := 6;
  End;
  If IsThere Then
  Begin
    SubjText := 'there';
    Sp := 3;
    For I := 1 To DIN Do
      If (DI[I].Kind = 'N') And (I <> SubjIdx) And DI[I].Plural Then Sp := 6;
  End;
  If WhSubject Then
  Begin
    SubjText := WhText;
    Sp := 3;
  End;

  { complements }
  IObj := '';
  DObj := '';
  Pp := '';
  Adv := '';
  Cnt := 0;
  For I := 1 To DIN Do
  Begin
    If (I = SubjIdx) Then Continue;
    Case DI[I].Kind Of
      'N':
      Begin
        If DI[I].Text = '' Then Continue;
        ObjTxt := DI[I].TextObj;
        If (G.Mode = 'b') Or (G.Mode = 'p') Then
        Begin
          If (G.Mode = 'b') And (Not DI[I].Pron) And (Not DI[I].Proper) And
              (Pos(' ', ObjTxt) = 0) And
             (DI[I].HeadEn <> '') And (Not IsMassNoun(DI[I].HeadEn)) And (Not
                 DI[I].Plural) And
             (Not IsLanguage(DI[I].HeadEn)) Then
            ObjTxt := EnArticle(ObjTxt) + ' ' + ObjTxt;
          If (G.Mode = 'b') And (DI[I].Cases = 'd') Then DObj := Join(DObj,
              ObjTxt)
          Else DObj := Join(DObj, ObjTxt);
        End
        Else If (Pos('d', DI[I].Cases) > 0) And (Pos('a', DI[I].Cases) = 0) Then
          IObj := Join(IObj, ObjTxt)
        Else If (Pos('d', DI[I].Cases) > 0) And (Pos('n', DI[I].Cases) = 0) And
            (Pos('a', DI[I].Cases) = 0) Then
          IObj := Join(IObj, ObjTxt)
        Else
          DObj := Join(DObj, ObjTxt);
      End;
      'A':
      Begin
        If (SubjIdx > 0) And (DI[SubjIdx].HeadEn = 'weather') And (DI[I].Text =
            'beautiful') Then DI[I].Text := 'nice';
        If (G.Mode = 's') And (DI[I].Text = 'good') Then DI[I].Text := 'well';
        Pred := Join(Pred, DI[I].Text);
      End;
      'E': Pp := Join(Pp, DI[I].Text);
      'R':
        If (Not DeIsFreqEn(DI[I].Text)) And (DI[I].Text <> 'gladly') Then
        Begin
          If DI[I].Text = 'never' Then Freq := Join(Freq, 'never')
          Else Adv := Join(Adv, DI[I].Text);
        End;
      'C':
        If Pred <> '' Then Pred := Join(Pred, DI[I].Text) Else Pp := Join(Pp,
            DI[I].Text);
    End;
    Inc(Cnt);
  End;
  If Neg And InList(G.MainEn, 'like love hate prefer enjoy') Then
  Begin
    If Copy(DObj, 1, 4) = 'any ' Then Delete(DObj, 1, 4);
    If Copy(DObj, 1, 2) = 'a ' Then Delete(DObj, 1, 2);
  End;
  Tail := Join(IObj, Join(DObj, Join(Pred, Join(Pp, Adv))));
  If G.Mode = 'p' Then Tail := Join(Join(IObj, DObj), Join(Pred, Join(Pp,
      Adv)));
  If Please Then Tail := Tail;

  { imperatif }
  If Imperative Then
  Begin
    Res := '';
    If (HasSubj) And DI[SubjIdx].Pron And (GT[DI[SubjIdx].PronTok].W = 'wir')
        And DePivot Then
      Res := 'let us'
    Else If (HasSubj) And DI[SubjIdx].Pron And (GT[DI[SubjIdx].PronTok].W =
        'wir') Then
      Res := 'let''s'
    Else If Neg Then Res := 'don''t'
    Else If Please Then Res := 'please';
    If Neg And (Res = 'let''s') Then Res := 'let''s not';
    If Neg And (Res = 'don''t') And Please Then Res := 'please don''t';
    If (Inf = 'sein') Then VerbEn := 'be';
    TranslateClauseDeIn := Join(Res, Join(Freq, Join(VerbEn, Tail)));
    Exit;
  End;

  BuildEn(G, Sp, Aux, Rest, LexBase, Post, LexTns);
  If WhSubject Then WhText := '';
  DoQ := IsQ And (Not WhSubject) And (Not Embedded);
  If (HasSubj) And (SubjText = '') Then DoQ := False;
  Res := '';
  Wh := WhText;
  If DoQ Then
  Begin
    If Aux = '' Then
    Begin
      Aux := DoFormEn(Sp, LexTns);
      Rest := Join(LexBase, Post);
      Post := '';
      LexBase := '';
    End;
    If Neg Then AuxN := NegContract(Aux) Else AuxN := Aux;
    Res := Join(Wh, Join(AuxN, Join(SubjText, Join(Freq, Join(Rest, Join(Post,
        Tail))))));
  End
  Else
  Begin
    If Neg Then
    Begin
      If Aux = '' Then
      Begin
        Aux := DoFormEn(Sp, LexTns);
        Rest := Join(LexBase, Post);
        Post := '';
        LexBase := '';
      End;
      If Aux = 'am' Then AuxN := 'am not' Else AuxN := NegContract(Aux);
      Res := Join(Wh, Join(SubjText, Join(AuxN, Join(Freq, Join(Rest, Tail)))));
    End
    Else If Aux <> '' Then
      Res := Join(Wh, Join(SubjText, Join(Aux, Join(Freq, Join(Rest, Tail)))))
    Else
      Res := Join(Wh, Join(SubjText, Join(Freq, Join(FiniteEn(LexBase, LexTns,
          Sp), Join(Post, Tail)))));
  End;
  If Please Then Res := 'please ' + Res;
  TranslateClauseDeIn := Res;
End;

{ Proposition infinitive : "ein Buch zu lesen" -> "to read a book" }
Function TranslateInfClauseDe(A, B: Integer): String;
Var
  I, IV: Integer;
  Res: String;
Begin
  IV := 0;
  For I := A To B Do
    If (GT[I].Cat = 'V') And (GT[I].CanInf) And (IV = 0) Then IV := I;
  If IV = 0 Then
  Begin
    TranslateInfClauseDe := '';
    Exit;
  End;
  DeConsume(IV);
  For I := A To B Do
    If GT[I].Cat = 'K' Then DeConsume(I);
  ParseItems(A, B);
  Res := '';
  For I := 1 To DIN Do
    If DI[I].Kind = 'N' Then Res := Join(Res, DI[I].TextObj)
    Else Res := Join(Res, DI[I].Text);
  TranslateInfClauseDe := Join('to ' + DeEnglishOf(GT[IV].LInf), Res);
End;

Function TranslateTokensDeIn(A, B: Integer; IsQ: Boolean): String;
Var
  K: Integer;
  Left, Right, Conj: String;
Begin
  If A > B Then
  Begin
    TranslateTokensDeIn := '';
    Exit;
  End;
  Left := FindPhraseDe(GPhraseKey(A, B));
  If Left <> '' Then
  Begin
    If (A > 1) And (Copy(Left, 1, 2) <> 'I ') Then Left := LowerFirst(Left);
    TranslateTokensDeIn := Left;
    Exit;
  End;
  { virgule : deux propositions, ou proposition + infinitive }
  For K := A + 1 To B - 1 Do
    If GT[K].Cat = ',' Then
    Begin
      If ((K - A = 1) And (GT[A].Cat = 'J')) Or
         (DeClauseHasFinite(A, K - 1) And DeClauseHasFinite(K + 1, B)) Then
      Begin
        If (GT[K + 1].Cat = 'W') And (Not DePivot) And DeClauseHasFinite(A, K -
            1) Then
        Begin
          Left := TranslateTokensDeIn(A, K - 1, False);
          DeEmbed := True;
          TranslateTokensDeIn := Left + ' ' + TranslateClauseDeIn(K + 1, B,
              False, False);
          DeEmbed := False;
        End
        Else If (GT[K + 1].Cat = 'C') And (GT[K + 1].Kind = 'S') Then
          TranslateTokensDeIn := TranslateTokensDeIn(A, K - 1, False) + ' ' +
              TranslateTokensDeIn(K + 1, B, IsQ)
        Else
          TranslateTokensDeIn := TranslateTokensDeIn(A, K - 1, False) + ', ' +
              TranslateTokensDeIn(K + 1, B, IsQ);
        Exit;
      End;
      If DeClauseHasFinite(A, K - 1) And (Not DeClauseHasFinite(K + 1, B)) Then
      Begin
        Right := TranslateInfClauseDe(K + 1, B);
        If Right <> '' Then
        Begin
          TranslateTokensDeIn := TranslateTokensDeIn(A, K - 1, IsQ) + ' ' +
              Right;
          Exit;
        End;
      End;
    End;
  For K := A + 1 To B - 1 Do
    If (GT[K].Cat = 'C') And DeClauseHasFinite(A, K - 1) And DeClauseHasFinite(K
        + 1, B) Then
    Begin
      Conj := GT[K].En;
      If GT[K].Kind = 'S' Then
        TranslateTokensDeIn := TranslateTokensDeIn(A, K - 1, False) + ', ' +
            Conj + ' ' +
                               TranslateClauseDeIn(K + 1, B, False, True)
      Else If Conj = 'but' Then
        TranslateTokensDeIn := TranslateTokensDeIn(A, K - 1, False) + ', ' +
            Conj + ' ' +
                               TranslateTokensDeIn(K + 1, B, IsQ)
      Else
        TranslateTokensDeIn := TranslateTokensDeIn(A, K - 1, False) + ' ' + Conj
            + ' ' +
                               TranslateTokensDeIn(K + 1, B, IsQ);
      Left := '';
      Exit;
    End;
  TranslateTokensDeIn := TranslateClauseDeIn(A, B, IsQ, False);
End;

{ ------------------------------------------------------------------ }
{ ALLEMAND -> ANGLAIS : phrases toutes faites et phrase complete      }
{ ------------------------------------------------------------------ }

Procedure InitDeSpecials;
Const
  List =
      'sein haben werden können müssen wollen sollen dürfen mögen möchten'+
      ' wissen kennen heißen suchen geben';
Var
  Rest, Item: String;
  P, Dummy: Integer;
Begin
  Rest := List;
  While Rest <> '' Do
  Begin
    P := Pos(' ', Rest);
    If P = 0 Then
    Begin
      Item := Rest;
      Rest := '';
    End
    Else
    Begin
      Item := Copy(Rest, 1, P - 1);
      Delete(Rest, 1, P);
    End;
    Dummy := DeVerb(Item);
  End;
End;

{ Traduit une phrase allemande en anglais (avec ponctuation finale). }
Function DeTranslateSentence(Const Sentence: String): String;
Var
  Res: String;
  Question: Boolean;
Begin
  GTokenize(Sentence);
  If GN = 0 Then
  Begin
    DeTranslateSentence := '';
    Exit;
  End;
  DeClassify;
  Question := GEnd = '?';
  Res := FindPhraseDe(GPhraseKey(1, GN));
  If Res = '' Then Res := TranslateTokensDeIn(1, GN, Question);
  Res := UpperFirst(FixEnglish(Res));
  If GEnd <> #0 Then Res := Res + GEnd;
  DeTranslateSentence := Res;
End;

{ ------------------------------------------------------------------ }
{ JAPONAIS (romaji) -> ANGLAIS : lexique, formes et jetons            }
{ ------------------------------------------------------------------ }

Type
  TJa = Record
    Ro: String[26];
    En: String[28];
    Pos: String[3];
  End;

  TJTok = Record
    W, Orig: String[34];
    Cap, Punct: Boolean;
    Cat: Char; { N nom, V verbe, A adjectif, P particule, C copule, R adverbe, W
        interrogatif,
                     J interjection, U nombre, K compteur, L nom relationnel, S
                       pronom, Z inconnu }
    Idx: Integer;
    Form: Char;
    En: String[30];
    Pos: String[3];
  End;

Var
  JaLex: Array[1..720] Of TJa;
  JaCount: Integer;
  JT: Array[0..MaxTokens + 1] Of TJTok;
  JN: Integer;
  JEnd: Char;
  SourceJa: Boolean;

Function ReplaceChar(Const S: String; A, B: Char): String;
Var
  I: Integer;
  R: String;
Begin
  R := S;
  For I := 1 To Length(R) Do
    If R[I] = A Then R[I] := B;
  ReplaceChar := R;
End;

Procedure LoadJapanese;
Var
  I: Integer;
  Line, Item, Ro, Rest: String;
Begin
  JaCount := 0;
  For I := 0 To High(JaData) Do
  Begin
    Line := JaData[I];
    While Line <> '' Do
    Begin
      Item := CopyUntil(Line, ';');
      If (Item <> '') And (JaCount < 720) Then
      Begin
        Ro := CopyUntil(Item, '=');
        Rest := CopyUntil(Item, ':');
        Inc(JaCount);
        JaLex[JaCount].Ro := ReplaceChar(Ro, '_', ' ');
        JaLex[JaCount].En := ReplaceChar(Rest, '_', ' ');
        JaLex[JaCount].Pos := Item;
      End;
    End;
  End;
End;

{ ---- formes verbales japonaises ---- }

Function JaIsVerbPos(Const P: String): Boolean;
Begin
  JaIsVerbPos := (P = 'v1') Or (P = 'v5') Or (P = 'vs') Or (P = 'vk') Or (P =
      'va') Or (P = 'vi');
End;

Function JaVStem(Const Ro, Cls: String): String;
Var
  L: Integer;
Begin
  L := Length(Ro);
  If (Cls = 'v1') Or (Cls = 'vi') Then JaVStem := Copy(Ro, 1, L - 2)
  Else If Cls = 'vs' Then JaVStem := 'shi'
  Else If Cls = 'vk' Then JaVStem := 'ki'
  Else If EndsWith(Ro, 'tsu') Then JaVStem := Copy(Ro, 1, L - 3) + 'chi'
  Else If EndsWith(Ro, 'su') Then JaVStem := Copy(Ro, 1, L - 2) + 'shi'
  Else JaVStem := Copy(Ro, 1, L - 1) + 'i';
End;

Function JaVTe(Const Ro, Cls: String; Past: Boolean): String;
Var
  L: Integer;
  R: String;
Begin
  L := Length(Ro);
  If (Cls = 'v1') Or (Cls = 'vi') Then R := Copy(Ro, 1, L - 2) + 'te'
  Else If Cls = 'vs' Then R := 'shite'
  Else If Cls = 'vk' Then R := 'kite'
  Else If Ro = 'iku' Then R := 'itte'
  Else If EndsWith(Ro, 'tsu') Then R := Copy(Ro, 1, L - 3) + 'tte'
  Else If EndsWith(Ro, 'ru') Then R := Copy(Ro, 1, L - 2) + 'tte'
  Else If EndsWith(Ro, 'mu') Or EndsWith(Ro, 'nu') Or EndsWith(Ro, 'bu') Then R
      := Copy(Ro, 1, L - 2) + 'nde'
  Else If EndsWith(Ro, 'ku') Then R := Copy(Ro, 1, L - 2) + 'ite'
  Else If EndsWith(Ro, 'gu') Then R := Copy(Ro, 1, L - 2) + 'ide'
  Else If EndsWith(Ro, 'su') Then R := Copy(Ro, 1, L - 2) + 'shite'
  Else R := Copy(Ro, 1, L - 1) + 'tte';
  If Past Then
  Begin
    If EndsWith(R, 'e') Then R := Copy(R, 1, Length(R) - 1) + 'a';
  End;
  JaVTe := R;
End;

Function JaVNai(Const Ro, Cls: String): String;
Var
  L: Integer;
  C: Char;
Begin
  L := Length(Ro);
  If (Cls = 'v1') Or (Cls = 'vi') Then JaVNai := Copy(Ro, 1, L - 2) + 'nai'
  Else If Cls = 'vs' Then JaVNai := 'shinai'
  Else If Cls = 'vk' Then JaVNai := 'konai'
  Else If Cls = 'va' Then JaVNai := 'nai'
  Else
  Begin
    C := Ro[L - 1];
    If C In ['a', 'e', 'i', 'o', 'u'] Then JaVNai := Copy(Ro, 1, L - 1) +
        'wanai'
    Else If EndsWith(Ro, 'tsu') Then JaVNai := Copy(Ro, 1, L - 3) + 'tanai'
    Else If EndsWith(Ro, 'su') Then JaVNai := Copy(Ro, 1, L - 2) + 'sanai'
    Else JaVNai := Copy(Ro, 1, L - 1) + 'anai';
    If C = 'k' Then JaVNai := Copy(Ro, 1, L - 2) + 'kanai';
    If C = 'g' Then JaVNai := Copy(Ro, 1, L - 2) + 'ganai';
    If C = 'm' Then JaVNai := Copy(Ro, 1, L - 2) + 'manai';
    If C = 'n' Then JaVNai := Copy(Ro, 1, L - 2) + 'nanai';
    If C = 'b' Then JaVNai := Copy(Ro, 1, L - 2) + 'banai';
    If C = 'r' Then JaVNai := Copy(Ro, 1, L - 2) + 'ranai';
  End;
End;

{ Reconnait un mot comme forme d'un verbe ; Form : d m n a v t p k q i }
Function JaMatchVerb(Const W: String; Var Idx: Integer; Var Form: Char):
    Boolean;
Var
  I: Integer;
  R, Cls, St: String;
Begin
  JaMatchVerb := False;
  For I := 1 To JaCount Do
    If JaIsVerbPos(JaLex[I].Pos) Then
    Begin
      R := JaLex[I].Ro;
      Cls := JaLex[I].Pos;
      St := JaVStem(R, Cls);
      Form := ' ';
      If W = R Then Form := 'd'
      Else If W = St + 'masu' Then Form := 'm'
      Else If W = St + 'masen' Then Form := 'n'
      Else If W = St + 'mashita' Then Form := 'a'
      Else If W = St + 'mashou' Then Form := 'v'
      Else If W = JaVTe(R, Cls, False) Then Form := 't'
      Else If W = JaVTe(R, Cls, True) Then Form := 'p'
      Else If W = JaVNai(R, Cls) Then Form := 'k'
      Else If W = Copy(JaVNai(R, Cls), 1, Length(JaVNai(R, Cls)) - 1) + 'katta'
          Then Form := 'q'
      Else If W = St + 'tai' Then Form := 'i';
      If Form <> ' ' Then
      Begin
        Idx := I;
        JaMatchVerb := True;
        Exit;
      End;
    End;
End;

Function JaAdjStem(Const Ro: String): String;
Begin
  If Ro = 'ii' Then JaAdjStem := 'yo'
  Else JaAdjStem := Copy(Ro, 1, Length(Ro) - 1);
End;

Function JaMatchAdj(Const W: String; Var Idx: Integer; Var Form: Char): Boolean;
Var
  I: Integer;
  St: String;
Begin
  JaMatchAdj := False;
  For I := 1 To JaCount Do
    If (JaLex[I].Pos = 'ia') Or (JaLex[I].Pos = 'ii') Then
    Begin
      St := JaAdjStem(JaLex[I].Ro);
      Form := ' ';
      If W = JaLex[I].Ro Then Form := 'd'
      Else If W = St + 'kunai' Then Form := 'k'
      Else If W = St + 'katta' Then Form := 'p'
      Else If W = St + 'kunakatta' Then Form := 'q'
      Else If W = St + 'ku' Then Form := 'r'
      Else If W = St + 'kute' Then Form := 't';
      If Form <> ' ' Then
      Begin
        Idx := I;
        JaMatchAdj := True;
        Exit;
      End;
    End;
End;

Function JaFind(Const W: String; Const Kinds: String): Integer;
Var
  I: Integer;
Begin
  JaFind := 0;
  For I := 1 To JaCount Do
    If (JaLex[I].Ro = W) And (Pos(JaLex[I].Pos, Kinds) > 0) Then
    Begin
      JaFind := I;
      Exit;
    End;
End;

Function JaIsParticle(Const W: String): Boolean;
Begin
  JaIsParticle := InList(W,
      'wa ga o wo ni de to no e mo ka ya kara made yori ne yo na');
End;

Function JaNumberEn(Const W: String): String;
Begin
  JaNumberEn := '';
  If W = 'ichi' Then JaNumberEn := '1'
  Else If W = 'ni' Then JaNumberEn := '2'
  Else If W = 'san' Then JaNumberEn := '3'
  Else If (W = 'yon') Or (W = 'shi') Then JaNumberEn := '4'
  Else If W = 'go' Then JaNumberEn := '5'
  Else If W = 'roku' Then JaNumberEn := '6'
  Else If (W = 'nana') Or (W = 'shichi') Then JaNumberEn := '7'
  Else If W = 'hachi' Then JaNumberEn := '8'
  Else If (W = 'kyuu') Or (W = 'ku') Then JaNumberEn := '9'
  Else If W = 'juu' Then JaNumberEn := '10'
  Else If W = 'juuichi' Then JaNumberEn := '11'
  Else If W = 'juuni' Then JaNumberEn := '12'
  Else If W = 'hyaku' Then JaNumberEn := '100'
  Else If W = 'sen' Then JaNumberEn := '1000'
  Else If W = 'hitotsu' Then JaNumberEn := '1'
  Else If W = 'futatsu' Then JaNumberEn := '2'
  Else If W = 'mittsu' Then JaNumberEn := '3'
  Else If W = 'yottsu' Then JaNumberEn := '4'
  Else If W = 'itsutsu' Then JaNumberEn := '5'
  Else If W = 'hitori' Then JaNumberEn := '1'
  Else If W = 'futari' Then JaNumberEn := '2';
End;

Function JaCounterEn(Const W: String): String;
Begin
  If W = 'nin' Then JaCounterEn := 'people'
  Else If (W = 'hon') Or (W = 'mai') Or (W = 'ko') Or (W = 'satsu') Or (W =
      'dai') Or (W = 'hiki') Or
          (W = 'kai') Or (W = 'ji') Then JaCounterEn := '*'
  Else JaCounterEn := '';
End;

{ ---- tokenisation ---- }

Procedure JPush(Const W, Orig: String; Cap, Punct: Boolean);
Begin
  If JN >= MaxTokens Then Exit;
  Inc(JN);
  FillChar(JT[JN], SizeOf(TJTok), 0);
  JT[JN].W := W;
  JT[JN].Orig := Orig;
  JT[JN].Cap := Cap;
  JT[JN].Punct := Punct;
End;

Procedure JTokenize(Const S0: String);
Var
  S, Word1: String;
  I, L: Integer;
  C: Char;
  Cap: Boolean;
Begin
  JN := 0;
  JEnd := #0;
  { normalisation : macrons et ponctuation japonaise }
  S := '';
  I := 1;
  While I <= Length(S0) Do
  Begin
    If (S0[I] = #$C4) And (I < Length(S0)) And (S0[I + 1] = #$81) Then begin S
        := S + 'aa'; Inc(I, 2); end
    Else If (S0[I] = #$C4) And (I < Length(S0)) And (S0[I + 1] = #$AB) Then
        begin S := S + 'ii'; Inc(I, 2); end
    Else If (S0[I] = #$C5) And (I < Length(S0)) And (S0[I + 1] = #$AB) Then
        begin S := S + 'uu'; Inc(I, 2); end
    Else If (S0[I] = #$C4) And (I < Length(S0)) And (S0[I + 1] = #$93) Then
        begin S := S + 'ee'; Inc(I, 2); end
    Else If (S0[I] = #$C5) And (I < Length(S0)) And (S0[I + 1] = #$8D) Then
        begin S := S + 'ou'; Inc(I, 2); end
    Else If (S0[I] = #$E3) And (I + 2 <= Length(S0)) And (S0[I + 1] = #$80) And
        (S0[I + 2] = #$82) Then begin S := S + '.'; Inc(I, 3); end
    Else If (S0[I] = #$E3) And (I + 2 <= Length(S0)) And (S0[I + 1] = #$80) And
        (S0[I + 2] = #$81) Then begin S := S + ','; Inc(I, 3); end
    Else If (S0[I] = #$EF) And (I + 2 <= Length(S0)) And (S0[I + 1] = #$BC) And
        (S0[I + 2] = #$9F) Then begin S := S + '?'; Inc(I, 3); end
    Else If (S0[I] = #$EF) And (I + 2 <= Length(S0)) And (S0[I + 1] = #$BC) And
        (S0[I + 2] = #$81) Then begin S := S + '!'; Inc(I, 3); end
    Else If S0[I] = '''' Then Inc(I)
    Else
    Begin
      S := S + S0[I];
      Inc(I);
    End;
  End;
  L := Length(S);
  I := 1;
  While I <= L Do
  Begin
    C := S[I];
    If C In ['A'..'Z', 'a'..'z', '0'..'9'] Then
    Begin
      Word1 := '';
      While (I <= L) And (S[I] In ['A'..'Z', 'a'..'z', '0'..'9']) Do
      Begin
        Word1 := Word1 + S[I];
        Inc(I);
      End;
      Cap := (Word1[1] >= 'A') And (Word1[1] <= 'Z');
      JPush(LowerAscii(Word1), Word1, Cap, False);
    End
    Else
    Begin
      If (C = '.') Or (C = '?') Or (C = '!') Then
      Begin
        If JEnd = #0 Then JEnd := C;
        If C = '?' Then JEnd := '?';
      End
      Else If (C = ',') Or (C = ';') Or (C = ':') Then
      Begin
        JPush(',', ',', False, True);
        JT[JN].Cat := ',';
      End;
      Inc(I);
    End;
  End;
End;

{ Fusionne les locutions du lexique (otoko no hito, ohayou gozaimasu...). }
Procedure JMergeMultiword;
Var
  I, K, Len, L: Integer;
  Key: String;
  Bad, Found: Boolean;
Begin
  I := 1;
  While I < JN Do
  Begin
    Found := False;
    For Len := 4 Downto 2 Do
      If (Not Found) And (I + Len - 1 <= JN) Then
      Begin
        Key := '';
        Bad := False;
        For K := I To I + Len - 1 Do
        Begin
          If JT[K].Punct Then Bad := True;
          Key := Join(Key, JT[K].W);
        End;
        If Not Bad Then
        Begin
          L := JaFind(Key, 'n l r w j s');
          If L > 0 Then
          Begin
            Found := True;
            JT[I].W := Key;
            JT[I].Orig := Key;
            For K := I + 1 To JN - Len + 1 Do
              JT[K] := JT[K + Len - 1];
            Dec(JN, Len - 1);
          End;
        End;
      End;
    Inc(I);
  End;
End;

{ ------------------------------------------------------------------ }
{ JAPONAIS -> ANGLAIS : classification                                }
{ ------------------------------------------------------------------ }

Function JaPronoun(Const W: String; Var En, Poss: String; Var Person: Integer):
    Boolean;
Begin
  JaPronoun := True;
  Person := 3;
  If (W = 'watashi') Or (W = 'boku') Or (W = 'ore') Or (W = 'atashi') Then Begin
      En := 'I'; Poss := 'my'; Person := 1; End
  Else If (W = 'anata') Or (W = 'kimi') Or (W = 'omae') Then Begin En := 'you';
      Poss := 'your'; Person := 2; End
  Else If W = 'kare' Then Begin En := 'he'; Poss := 'his'; End
  Else If W = 'kanojo' Then Begin En := 'she'; Poss := 'her'; End
  Else If (W = 'watashitachi') Or (W = 'bokutachi') Or (W = 'wareware') Then
      Begin En := 'we'; Poss := 'our'; Person := 4; End
  Else If (W = 'anatatachi') Or (W = 'minasan') Then Begin En := 'you'; Poss :=
      'your'; Person := 5; End
  Else If (W = 'karera') Or (W = 'kanojotachi') Then Begin En := 'they'; Poss :=
      'their'; Person := 6; End
  Else If W = 'kore' Then Begin En := 'this'; Poss := 'this'; End
  Else If (W = 'sore') Or (W = 'are') Then Begin En := 'that'; Poss := 'that';
      End
  Else JaPronoun := False;
End;

Procedure JClassify;
Var
  I, Idx: Integer;
  W, En, Poss: String;
  Form: Char;
  P: Integer;
  Next: String;
Begin
  JMergeMultiword;
  For I := 1 To JN Do
  Begin
    W := JT[I].W;
    JT[I].Idx := 0;
    JT[I].Form := ' ';
    If JT[I].Punct Then
    Begin
      JT[I].Cat := ',';
      Continue;
    End;
    If I < JN Then Next := JT[I + 1].W Else Next := '';
    { nombres }
    If IsDigitsOnly(W) Then
    Begin
      JT[I].Cat := 'U';
      JT[I].En := W;
      Continue;
    End;
    If (JaNumberEn(W) <> '') And ((I = 1) Or Not (JT[I - 1].Cat In ['N', 'L',
        'Z', 'S'])) And ((JaCounterEn(Next) <> '') Or (Copy(W, 1, 4) = 'futa')
        Or
       (Copy(W, 1, 4) = 'hito') Or (Copy(W, 1, 4) = 'mitt') Or (Copy(W, 1, 4) =
           'yott') Or
       (Copy(W, 1, 4) = 'itsu') Or ((I < JN) And (Next = 'ji'))) Then
    Begin
      JT[I].Cat := 'U';
      JT[I].En := JaNumberEn(W);
      Continue;
    End;
    If (JaCounterEn(W) <> '') And (I > 1) And (JT[I - 1].Cat = 'U') Then
    Begin
      JT[I].Cat := 'K';
      JT[I].En := JaCounterEn(W);
      Continue;
    End;
    If (W = 'san') Or (W = 'kun') Or (W = 'chan') Or (W = 'sama') Then
    Begin
      If (I > 1) And (JT[I - 1].Cat = 'Z') Then
      Begin
        JT[I].Cat := 'H';
        Continue;
      End;
    End;
    { particules et copule }
    If InList(W, 'desu da deshita datta deshou darou') Then
    Begin
      JT[I].Cat := 'C';
      If (W = 'deshita') Or (W = 'datta') Then JT[I].Form := 'a' Else JT[I].Form
          := 'd';
      Continue;
    End;
    If (W = 'janai') Or (W = 'dewanai') Then
    Begin
      JT[I].Cat := 'C';
      JT[I].Form := 'k';
      Continue;
    End;
    If ((W = 'dewa') Or (W = 'ja')) And (I < JN) And ((Next = 'arimasen') Or
        (Next = 'nai')) Then
    Begin
      JT[I].Cat := 'D';
      Continue;
    End;
    If JaIsParticle(W) And (W <> 'na') Then
    Begin
      JT[I].Cat := 'P';
      Continue;
    End;
    If (W = 'na') And (I > 1) And (JT[I - 1].Cat = 'A') Then
    Begin
      JT[I].Cat := 'P';
      Continue;
    End;
    { pronoms }
    If JaPronoun(W, En, Poss, P) Then
    Begin
      JT[I].Cat := 'S';
      JT[I].En := En;
      JT[I].Idx := P;
      Continue;
    End;
    If (W = 'kono') Or (W = 'sono') Or (W = 'ano') Then
    Begin
      JT[I].Cat := 'T';
      If W = 'kono' Then JT[I].En := 'this' Else JT[I].En := 'that';
      Continue;
    End;
    { lexique exact }
    Idx := JaFind(W, 'n l r w j s');
    If Idx > 0 Then
    Begin
      JT[I].Idx := Idx;
      JT[I].En := JaLex[Idx].En;
      JT[I].Pos := JaLex[Idx].Pos;
      Case JaLex[Idx].Pos[1] Of
        'n', 's': JT[I].Cat := 'N';
        'l': JT[I].Cat := 'L';
        'r': JT[I].Cat := 'R';
        'w': JT[I].Cat := 'W';
        'j': JT[I].Cat := 'J';
      End;
      If JaLex[Idx].Pos = 's' Then JT[I].Cat := 'N';
      Continue;
    End;
    If JaMatchVerb(W, Idx, Form) Then
    Begin
      JT[I].Cat := 'V';
      JT[I].Idx := Idx;
      JT[I].Form := Form;
      JT[I].En := JaLex[Idx].En;
      JT[I].Pos := JaLex[Idx].Pos;
      Continue;
    End;
    If JaMatchAdj(W, Idx, Form) Then
    Begin
      JT[I].Cat := 'A';
      JT[I].Idx := Idx;
      JT[I].Form := Form;
      JT[I].En := JaLex[Idx].En;
      JT[I].Pos := 'ia';
      Continue;
    End;
    Idx := JaFind(W, 'na');
    If Idx > 0 Then
    Begin
      JT[I].Cat := 'A';
      JT[I].Idx := Idx;
      JT[I].Form := 'n';
      JT[I].En := JaLex[Idx].En;
      JT[I].Pos := 'na';
      Continue;
    End;
    JT[I].Cat := 'Z';
  End;
End;

{ ------------------------------------------------------------------ }
{ JAPONAIS -> ANGLAIS : groupes nominaux                              }
{ ------------------------------------------------------------------ }

Type
  TJSeg = Record
    Text: String[110];
    Head: String[30];
    Found, Pron, Plural, IsPerson, IsWh, IsProper, IsTime, IsRel: Boolean;
    Pers: Integer;
    Poss: String[8];
  End;

  TJChunk = Record
    Ptc: String[8];
    Seg: TJSeg;
    First, Last: Integer;
  End;

Var
  JC: Array[1..16] Of TJChunk;
  JCN: Integer;

Function JaFindExact(Const W, P: String): Integer;
Var
  I: Integer;
Begin
  JaFindExact := 0;
  For I := 1 To JaCount Do
    If (JaLex[I].Ro = W) And (JaLex[I].Pos = P) Then
    Begin
      JaFindExact := I;
      Exit;
    End;
End;

Function JaRelPrep(Const En: String): String;
Begin
  If En = 'top' Then JaRelPrep := 'on'
  Else If En = 'bottom' Then JaRelPrep := 'under'
  Else If En = 'inside' Then JaRelPrep := 'in'
  Else If En = 'front' Then JaRelPrep := 'in front of'
  Else If En = 'back' Then JaRelPrep := 'behind'
  Else If En = 'next' Then JaRelPrep := 'next to'
  Else JaRelPrep := 'near';
End;

Function JaIsPersonEn(Const En: String): Boolean;
Begin
  JaIsPersonEn := InList(En,
      'person man woman boy girl child friend family mother father brother sis'+
      'ter son daughter husband wife teacher student doctor nurse employee');
End;

{ Un segment sans 'no' : determinant, adjectifs, nom }
Procedure JaRenderSimple(First, Last: Integer; Const Art: String; Generic:
    Boolean; Var Seg: TJSeg);
Var
  I: Integer;
  Det, Adjs, Noun, Qty, Dem: String;
  Plural, HasNoun, Mass: Boolean;
  Idx: Integer;
Begin
  FillChar(Seg, SizeOf(Seg), 0);
  Det := '';
  Adjs := '';
  Noun := '';
  Qty := '';
  Dem := '';
  Plural := False;
  HasNoun := False;
  I := First;
  While I <= Last Do
  Begin
    Case JT[I].Cat Of
      'U':
      Begin
        Qty := JT[I].En;
        If (I < Last) And (JT[I + 1].Cat = 'K') Then
        Begin
          If JT[I + 1].W = 'ji' Then
          Begin
            Seg.Text := Qty + ' o''clock';
            Seg.IsTime := True;
            Seg.Found := True;
            Exit;
          End;
          If JT[I + 1].En = 'people' Then
          Begin
            Noun := 'people';
            HasNoun := True;
            Seg.IsPerson := True;
          End;
          Inc(I);
        End;
        If (Qty <> '1') And (Qty <> '') Then Plural := True;
        If Qty = '1' Then Qty := '';
      End;
      'T':
        Dem := JT[I].En;
      'S':
      Begin
        If (I = Last) And (First = Last) Then
        Begin
          Seg.Pron := True;
          Seg.Text := JT[I].En;
          Seg.Pers := JT[I].Idx;
          Seg.Found := True;
          If JT[I].En = 'I' Then Seg.Poss := 'my'
          Else If JT[I].En = 'you' Then Seg.Poss := 'your'
          Else If JT[I].En = 'he' Then Seg.Poss := 'his'
          Else If JT[I].En = 'she' Then Seg.Poss := 'her'
          Else If JT[I].En = 'we' Then Seg.Poss := 'our'
          Else If JT[I].En = 'they' Then Seg.Poss := 'their'
          Else Seg.Poss := JT[I].En;
          Exit;
        End;
        Dem := JT[I].En;
      End;
      'A':
      Begin
        If JT[I].Pos = 'ia' Then Adjs := Join(Adjs, JT[I].En)
        Else If JT[I].Pos = 'ii' Then Adjs := Join(Adjs, JT[I].En)
        Else Adjs := Join(Adjs, JT[I].En);
      End;
      'N':
      Begin
        Noun := JT[I].En;
        HasNoun := True;
        Seg.IsPerson := JaIsPersonEn(Noun);
        If (Noun <> '') And (Noun[1] >= 'A') And (Noun[1] <= 'Z') And (Not
            IsLanguage(LowerAscii(Noun))) Then
          Seg.IsProper := True;
      End;
      'Z':
      Begin
        Seg.IsProper := True;
        Noun := Join(Noun, JT[I].Orig);
        HasNoun := True;
      End;
      'W':
      Begin
        Seg.IsWh := True;
        Noun := JT[I].En;
        HasNoun := True;
      End;
      'R':
      Begin
        Seg.IsTime := True;
        Noun := JT[I].En;
        HasNoun := True;
      End;
    End;
    Inc(I);
  End;
  If Not HasNoun Then
  Begin
    Seg.Text := Join(Dem, Join(Qty, Adjs));
    Seg.Found := Seg.Text <> '';
    Exit;
  End;
  Seg.Found := True;
  Seg.Head := Noun;
  Seg.Plural := Plural;
  If Seg.IsWh Or Seg.IsTime Or Seg.IsProper Then
  Begin
    Seg.Text := Noun;
    Exit;
  End;
  Mass := IsMassNoun(Noun) Or IsLanguage(LowerAscii(Noun)) Or InList(Noun,
      'sushi tempura ramen');
  Idx := 0;
  If Generic And (Dem = '') And (Qty = '') Then
  Begin
    Plural := True;
    Seg.Plural := True;
  End;
  If Plural And (Not Mass) And (Noun <> 'people') Then Noun := EnPlural(Noun);
  If Dem <> '' Then Det := Dem
  Else If Qty <> '' Then Det := Qty
  Else If Generic Then Det := ''
  Else If Mass Then Det := ''
  Else If Art = 'a' Then
  Begin
    If Plural Then Det := '' Else Det := 'a';
  End
  Else Det := Art;
  If (Det = '') And (Noun = 'weather') And (Not Generic) Then Det := 'the';
  If Det = 'a' Then Det := EnArticle(Join(Adjs, Noun));
  If IsLanguage(LowerAscii(Noun)) Then Noun := UpperFirst(LowerAscii(Noun));
  Seg.Text := Join(Det, Join(Adjs, Noun));
  Idx := 0;
End;

{ Groupe nominal complet avec 'no' (possessif) et noms relationnels. }
Procedure JaRenderNP(First, Last: Integer; Const Art: String; Generic: Boolean;
    Var Seg: TJSeg);
Var
  I, SegStart, NoPos: Integer;
  Mods, Head: TJSeg;
  ModText, HeadArt: String;
  Rel: Boolean;
Begin
  FillChar(Seg, SizeOf(Seg), 0);
  NoPos := 0;
  For I := First To Last Do
    If (JT[I].Cat = 'P') And (JT[I].W = 'no') Then NoPos := I;
  If NoPos = 0 Then
  Begin
    JaRenderSimple(First, Last, Art, Generic, Seg);
    Exit;
  End;
  { relatif : teeburu no ue }
  If (Last = NoPos + 1) And (JT[Last].Cat = 'L') Then
  Begin
    JaRenderNP(First, NoPos - 1, 'the', False, Mods);
    Seg.Found := True;
    Seg.IsRel := True;
    Seg.Text := JaRelPrep(JT[Last].En) + ' ' + Mods.Text;
    Exit;
  End;
  JaRenderNP(First, NoPos - 1, '', False, Mods);
  If Mods.Pron Then
  Begin
    JaRenderSimple(NoPos + 1, Last, '', Generic, Head);
    If (Not Head.Found) Then Head.Text := '';
    Seg := Head;
    Seg.Text := Join(Mods.Poss, Head.Text);
    Seg.Found := True;
    Exit;
  End;
  ModText := Mods.Text;
  If Mods.IsPerson Or Mods.IsProper Then
  Begin
    JaRenderSimple(NoPos + 1, Last, '', Generic, Head);
    Seg := Head;
    Seg.Text := ModText + '''s ' + Head.Text;
    Seg.Found := True;
    Exit;
  End;
  JaRenderSimple(NoPos + 1, Last, Art, Generic, Head);
  Seg := Head;
  Seg.Text := Join(ModText, Head.Text);
  Seg.Found := True;
End;

{ ------------------------------------------------------------------ }
{ JAPONAIS -> ANGLAIS : proposition                                   }
{ ------------------------------------------------------------------ }

Function JaIsMotion(Const En: String): Boolean;
Begin
  JaIsMotion := InList(En, 'go come return enter arrive get_off leave ride');
End;

Function JaIsVehicle(Const En: String): Boolean;
Begin
  JaIsVehicle := InList(En, 'car train bus plane bike boat');
End;

Function JaPlacePrep(Const Head: String): String;
Begin
  If InList(Head, 'Tokyo Kyoto Osaka Paris London') Then JaPlacePrep := 'in'
  Else JaPlacePrep := 'in';
End;

Function JaChunkWh(Const C: TJChunk): String;
Begin
  If C.Seg.IsWh Then JaChunkWh := C.Seg.Text Else JaChunkWh := '';
End;

Function JaTranslateClause(A, B: Integer; Q: Boolean): String;
Var
  I, K, PredIdx, PendStart, SubjC, ObjC: Integer;
  Seg: TJSeg;
  Ptc: String;
  Role: Char;
  Art: String;
  PKind: Char;
  PIdx: Integer;
  PForm: Char;
  PNeg, PPast, PProg, PReq, PTai, PLet, PPerm, PQ, Done: Boolean;
  PredNP: TJSeg;
  VerbEn, Wh, Res, Tail, ObjTxt, Pp, Adv, Freq, SubjText, Aux, Rest, LexBase,
      Post, AuxN: String;
  LexTns: Char;
  G: TFVG;
  Sp: Integer;
  HasSubj, WhSubject, DoQ, Future, Stative, IsExist, Gen: Boolean;
  Txt, Prep: String;
  IObj: String;
  Too: Boolean;
  Nxt: Integer;
  LocC: Integer;
Begin
  JaTranslateClause := '';
  If A > B Then Exit;
  { particules finales }
  PQ := Q;
  While (B >= A) And (JT[B].Cat = 'P') And InList(JT[B].W, 'ka ne yo') Do
  Begin
    If JT[B].W = 'ka' Then PQ := True;
    Dec(B);
  End;
  If A > B Then Exit;
  If (JT[B].Cat = 'J') And (JT[B].W = 'kudasai') And (B - 2 >= A) And (JT[B -
      1].W = 'o') Then
  Begin
    JaRenderNP(A, B - 2, 'a', False, Seg);
    JaTranslateClause := 'please give me ' + Seg.Text;
    Exit;
  End;
  If (JT[A].Cat = 'J') And (A < B) And (JT[A].W <> 'kudasai') Then
  Begin
    JaTranslateClause := JT[A].En + ', ' + LowerFirst(JaTranslateClause(A + 1,
        B, PQ));
    Exit;
  End;
  If (JT[A].Cat = 'J') And (A = B) Then
  Begin
    JaTranslateClause := JT[A].En;
    Exit;
  End;

  { decoupage en groupes : groupe nominal + particule }
  JCN := 0;
  PendStart := A;
  PredIdx := 0;
  I := A;
  While I <= B Do
  Begin
    Done := False;
    Case JT[I].Cat Of
      'P':
      Begin
        If JT[I].W = 'no' Then
          Done := True
        Else If (JT[I].W = 'na') Then
          Done := True
        Else
        Begin
          If (I > PendStart) And (JCN < 16) Then
          Begin
            Inc(JCN);
            JC[JCN].Ptc := JT[I].W;
            If JC[JCN].Ptc = 'wo' Then JC[JCN].Ptc := 'o';
            JC[JCN].First := PendStart;
            JC[JCN].Last := I - 1;
            Role := 'x';
            If JC[JCN].Ptc = 'wa' Then Role := 's'
            Else If JC[JCN].Ptc = 'ga' Then Role := 'g'
            Else If JC[JCN].Ptc = 'o' Then Role := 'o';
            Art := 'the';
            If Role = 'g' Then Art := 'a';
            If Role = 'o' Then Art := 'a';
            JaRenderNP(PendStart, I - 1, Art, False, JC[JCN].Seg);
            If (Role = 's') And InList(JC[JCN].Seg.Head,
                'mother father sister brother son daughter husband wife') And
               (Copy(JC[JCN].Seg.Text, 1, 4) = 'the ') Then
              JC[JCN].Seg.Text := 'my ' + Copy(JC[JCN].Seg.Text, 5, 100);
            If (Role = 'x') And JC[JCN].Seg.IsPerson And (Not
                JC[JCN].Seg.IsProper) And (Not JC[JCN].Seg.Pron) Then
              JaRenderNP(PendStart, I - 1, 'a', False, JC[JCN].Seg);
            If (Not DePivot) And (Role = 'x') And (JC[JCN].Ptc <> 'to') And
               InList(JC[JCN].Seg.Head, 'school university work church bed')
                   Then
              JaRenderNP(PendStart, I - 1, '', False, JC[JCN].Seg);
          End;
          PendStart := I + 1;
          Done := True;
        End;
      End;
      'V', 'C', 'D':
      Begin
        PredIdx := I;
        Break;
      End;
      'A':
      Begin
        If (I < B) And ((JT[I + 1].Cat In ['N', 'Z', 'S', 'L']) Or ((JT[I +
            1].Cat = 'P') And (JT[I + 1].W = 'na'))) And
           (JT[I].Form In ['d', 'n']) Then
          Done := True
        Else
        Begin
          PredIdx := I;
          Break;
        End;
      End;
      'R':
      Begin
        If (I = PendStart) And ((I = B) Or Not ((JT[I + 1].Cat = 'P') And (JT[I
            + 1].W <> 'no'))) And
           (JCN < 16) Then
        Begin
          Inc(JCN);
          JC[JCN].Ptc := 'r';
          JC[JCN].First := I;
          JC[JCN].Last := I;
          JaRenderSimple(I, I, '', False, JC[JCN].Seg);
          PendStart := I + 1;
          Done := True;
        End;
      End;
    End;
    Inc(I);
  End;

  { predicat }
  PKind := ' ';
  PIdx := 0;
  PForm := ' ';
  PNeg := False;
  PPast := False;
  PProg := False;
  PReq := False;
  PTai := False;
  PLet := False;
  PPerm := False;
  FillChar(PredNP, SizeOf(PredNP), 0);
  VerbEn := '';
  If PredIdx = 0 Then
  Begin
    { pas de predicat : simple groupe nominal }
    JaRenderNP(PendStart, B, '', False, Seg);
    Res := '';
    For K := 1 To JCN Do Res := Join(Res, JC[K].Seg.Text);
    JaTranslateClause := Join(Res, Seg.Text);
    Exit;
  End;
  If JT[PredIdx].Cat = 'V' Then
  Begin
    PKind := 'V';
    PIdx := JT[PredIdx].Idx;
    PForm := JT[PredIdx].Form;
    VerbEn := JT[PredIdx].En;
    { nom + suru }
    If (JT[PredIdx].Pos = 'vs') And (PendStart < PredIdx) And (JT[PredIdx -
        1].Cat = 'N') And
       (JT[PredIdx - 1].Pos = 's') Then
    Begin
      VerbEn := JT[PredIdx - 1].En;
      Dec(PredIdx);
      JT[PredIdx].Cat := ' ';
      Inc(PredIdx);
      PendStart := PendStart;
      If (PredIdx - 1 > PendStart) Then
      Begin
        JaRenderNP(PendStart, PredIdx - 2, '', False, Seg);
        If (JCN < 16) And Seg.Found Then
        Begin
          Inc(JCN);
          JC[JCN].Ptc := 'r';
          JC[JCN].Seg := Seg;
        End;
      End;
      PendStart := PredIdx;
    End
    Else If PendStart < PredIdx Then
    Begin
      JaRenderNP(PendStart, PredIdx - 1, '', False, Seg);
      If (JCN < 16) And Seg.Found Then
      Begin
        Inc(JCN);
        JC[JCN].Ptc := 'r';
        JC[JCN].Seg := Seg;
      End;
    End;
    Case PForm Of
      'n', 'k': PNeg := True;
      'a', 'p': PPast := True;
      'q': Begin PNeg := True; PPast := True; End;
      'v': PLet := True;
      'i': PTai := True;
    End;
    If (PForm = 'n') And (PredIdx < B) And (JT[PredIdx + 1].Cat = 'C') And
        (JT[PredIdx + 1].W = 'deshita') Then PPast := True;
    If (PForm = 't') And (PredIdx < B) Then
    Begin
      If (JT[PredIdx + 1].Cat = 'V') And (JT[PredIdx + 1].Pos = 'vi') Then
      Begin
        PProg := True;
        If JT[PredIdx + 1].Form = 'a' Then PPast := True;
        If JT[PredIdx + 1].Form = 'n' Then PNeg := True;
      End
      Else If (JT[PredIdx + 1].Cat = 'J') And (JT[PredIdx + 1].W = 'kudasai')
          Then
        PReq := True
      Else If (JT[PredIdx + 1].Cat = 'P') And (JT[PredIdx + 1].W = 'mo') Then
        PPerm := True;
    End;
    If PForm = 'p' Then PPast := True;
  End
  Else If JT[PredIdx].Cat = 'A' Then
  Begin
    PKind := 'A';
    PIdx := JT[PredIdx].Idx;
    PForm := JT[PredIdx].Form;
    VerbEn := JT[PredIdx].En;
    If PendStart < PredIdx Then
    Begin
      JaRenderNP(PendStart, PredIdx - 1, '', False, Seg);
      If (JCN < 16) And Seg.Found Then
      Begin
        Inc(JCN);
        JC[JCN].Ptc := 'r';
        JC[JCN].Seg := Seg;
      End;
    End;
    Case PForm Of
      'k': PNeg := True;
      'p': PPast := True;
      'q': Begin PNeg := True; PPast := True; End;
    End;
    If (PredIdx < B) And (JT[PredIdx + 1].Cat = 'C') Then
    Begin
      If JT[PredIdx + 1].Form = 'a' Then PPast := True;
      If JT[PredIdx + 1].Form = 'k' Then PNeg := True;
    End;
    If (PredIdx < B) And (JT[PredIdx + 1].Cat = 'D') Then PNeg := True;
  End
  Else
  Begin
    { copule : le groupe nominal qui precede est le predicat }
    PKind := 'N';
    JaRenderNP(PendStart, PredIdx - 1, 'a', False, PredNP);
    If JT[PredIdx].Cat = 'D' Then
    Begin
      PNeg := True;
      If (PredIdx + 2 <= B) And (JT[PredIdx + 2].W = 'deshita') Then PPast :=
          True;
    End
    Else
    Begin
      If JT[PredIdx].Form = 'a' Then PPast := True;
      If JT[PredIdx].Form = 'k' Then PNeg := True;
    End;
  End;

  { sujet }
  SubjC := 0;
  For K := 1 To JCN Do
    If (SubjC = 0) And (JC[K].Ptc = 'wa') And (Not JC[K].Seg.IsTime) And (Not
        JC[K].Seg.IsRel) Then SubjC := K;
  If (SubjC = 0) And (Not InList(VerbEn,
      'understand like dislike love want can')) Then
    For K := 1 To JCN Do
      If (SubjC = 0) And (JC[K].Ptc = 'ga') Then SubjC := K;
  If SubjC = 0 Then
    For K := 1 To JCN Do
      If (SubjC = 0) And (JC[K].Ptc = 'mo') And (JC[K].Seg.Pron Or
          JC[K].Seg.IsPerson) Then SubjC := K;
  HasSubj := SubjC > 0;
  WhSubject := HasSubj And JC[SubjC].Seg.IsWh;
  Too := HasSubj And (JC[SubjC].Ptc = 'mo');

  { existence : aru / iru }
  IsExist := (PKind = 'V') And ((JT[PredIdx].Pos = 'va') Or (JT[PredIdx].Pos =
      'vi')) And (PForm <> 't');
  Future := False;
  For K := 1 To JCN Do
    If JC[K].Seg.IsTime And ((JC[K].Seg.Head = 'tomorrow') Or (JC[K].Seg.Head =
        'tonight')) Then Future := True;

  FillChar(G, SizeOf(G), 0);
  G.Mode := 's';
  G.Tns := 'p';
  If PPast Then G.Tns := 'i'
  Else If Future And (PKind = 'V') Then G.Tns := 'f';
  G.Neg := PNeg;
  G.MainEn := VerbEn;
  Wh := '';
  Tail := '';
  Pp := '';
  Adv := '';
  IObj := '';
  ObjTxt := '';
  Freq := '';
  Stative := InList(VerbEn, 'live know have wear exist do');
  ObjC := 0;
  For K := 1 To JCN Do
    If (ObjC = 0) And ((JC[K].Ptc = 'o') Or ((JC[K].Ptc = 'ga') And (K <>
        SubjC))) Then ObjC := K;

  If PKind = 'N' Then
  Begin
    G.Mode := 'b';
    G.MainEn := 'be';
    Tail := PredNP.Text;
    If PredNP.IsWh Then
    Begin
      Wh := PredNP.Text;
      Tail := '';
      PQ := True;
    End;
  End
  Else If PKind = 'A' Then
  Begin
    If (JT[PredIdx].Pos = 'ia') And (VerbEn = 'want') And (ObjC > 0) Then
    Begin
      G.Mode := 's';
      G.MainEn := 'want';
    End
    Else If (JT[PredIdx].Pos = 'na') And InList(VerbEn, 'like dislike love') And
        (ObjC > 0) Then
    Begin
      G.Mode := 's';
      G.MainEn := VerbEn;
    End
    Else
    Begin
      G.Mode := 'b';
      G.MainEn := 'be';
      Tail := VerbEn;
      If (InList(VerbEn, 'good bad')) And (JT[PredIdx].Pos = 'na') And (ObjC >
          0) Then Tail := VerbEn + ' at';
    End;
  End
  Else
  Begin
    If PTai Then
    Begin
      G.Mode := 'm';
      G.Modal := 'vouloir';
      G.Tns := 'p';
      If PPast Then G.Tns := 'i';
    End
    Else If PPerm Then
    Begin
      G.Mode := 'm';
      G.Modal := 'pouvoir';
      G.Tns := 'p';
      PQ := True;
    End
    Else If IsExist Then
    Begin
      G.MainEn := 'be';
      G.Mode := 'b';
    End
    Else If PProg And (Not Stative) Then
    Begin
      If PPast Then G.Tns := 'i' Else G.Rain := True;
    End
    Else If InList(VerbEn, 'can') Then
    Begin
      G.Mode := 'm';
      G.Modal := 'pouvoir';
      G.MainEn := 'do';
    End;
  End;

  { complements : un par groupe }
  For K := 1 To JCN Do
  Begin
    If (K = SubjC) Then Continue;
    Seg := JC[K].Seg;
    If Not Seg.Found Then Continue;
    Ptc := JC[K].Ptc;
    If Seg.IsWh And (Ptc = 'kara') Then
    Begin
      Wh := Seg.Text;
      Pp := Join(Pp, 'from');
      Continue;
    End;
    If Seg.IsWh And (Ptc <> 'ga') And (Ptc <> 'wa') Then
    Begin
      If (Ptc = 'o') Or (Ptc = 'ni') Or (Ptc = 'de') Or (Ptc = 'e') Or (Ptc =
          'to') Then
      Begin
        Wh := Seg.Text;
        Continue;
      End;
    End;
    If (Ptc = 'o') Or ((Ptc = 'ga') And (K = ObjC)) Or ((VerbEn = 'meet') And
        (Ptc = 'ni')) Then
    Begin
      If InList(VerbEn, 'like dislike love want') Then
      Begin
        JaRenderNP(JC[K].First, JC[K].Last, '', True, Seg);
      End;
      ObjTxt := Join(ObjTxt, Seg.Text);
    End
    Else If (Ptc = 'ga') And (SubjC = 0) Then
      ObjTxt := ObjTxt
    Else If Ptc = 'ni' Then
    Begin
      If Seg.IsRel Then Pp := Join(Pp, Seg.Text)
      Else If Seg.IsTime Then Pp := Join(Pp, 'at ' + Seg.Text)
      Else If JaIsMotion(VerbEn) Or InList(VerbEn,
          'give show teach send call tell') Then
        IObj := Join(IObj, 'to ' + Seg.Text)
      Else If VerbEn = 'receive' Then Pp := Join(Pp, 'from ' + Seg.Text)
      Else If IsExist Then Pp := Join(Pp, 'in ' + Seg.Text)
      Else Pp := Join(Pp, 'at ' + Seg.Text);
    End
    Else If Ptc = 'e' Then
      Pp := Join(Pp, 'to ' + Seg.Text)
    Else If Ptc = 'de' Then
    Begin
      If JaIsVehicle(Seg.Head) Then Pp := Join(Pp, 'by ' + Seg.Head)
      Else If Seg.IsRel Then Pp := Join(Pp, Seg.Text)
      Else Pp := Join(Pp, 'at ' + Seg.Text);
    End
    Else If Ptc = 'to' Then Pp := Join(Pp, 'with ' + Seg.Text)
    Else If Ptc = 'kara' Then Pp := Join(Pp, 'from ' + Seg.Text)
    Else If Ptc = 'made' Then Pp := Join(Pp, 'to ' + Seg.Text)
    Else If Ptc = 'yori' Then Pp := Join(Pp, 'than ' + Seg.Text)
    Else If Ptc = 'mo' Then Pp := Join(Pp, Seg.Text + ' too')
    Else If (Ptc = 'wa') Then
    Begin
      If Seg.IsTime Then Adv := Join(Adv, Seg.Text) Else Pp := Join(Pp,
          Seg.Text);
    End
    Else If Ptc = 'r' Then
    Begin
      If IsFreqAdverb(Seg.Text) Or (Seg.Text = 'often') Or (Seg.Text =
          'sometimes') Then Freq := Join(Freq, Seg.Text)
      Else If Seg.Text = 'very' Then Freq := Freq
      Else Adv := Join(Adv, Seg.Text);
    End
    Else
      Pp := Join(Pp, Seg.Text);
  End;
  If Too Then Adv := Join(Adv, 'too');
  If (PKind = 'V') And IsExist And (ObjC > 0) Then
  Begin
    { il y a / X a Y }
    If HasSubj And (JC[SubjC].Seg.IsPerson Or JC[SubjC].Seg.Pron) And
        (JC[SubjC].Ptc = 'wa') Then
    Begin
      G.Mode := 'h';
      G.MainEn := 'have';
    End
    Else
    Begin
      G.Mode := 't';
      G.MainEn := 'be';
    End;
  End;
  If (PKind = 'V') And IsExist And (ObjC = 0) And HasSubj And (JC[SubjC].Ptc =
      'ga') Then
  Begin
    { Hon ga arimasu : there is a book }
    G.Mode := 't';
    G.MainEn := 'be';
    ObjTxt := JC[SubjC].Seg.Text;
    HasSubj := False;
  End;
  If (G.Mode = 'b') And IsExist And (Not HasSubj) And (ObjC = 0) Then G.Mode :=
      't';

  { sujet : texte et personne }
  Sp := 3;
  SubjText := '';
  If (G.MainEn = 'rain') And (PKind = 'V') Then
  Begin
    HasSubj := False;
    SubjText := 'it';
    G.Mode := 's';
    If G.Tns = 'p' Then G.Rain := False;
  End;
  If HasSubj Then
  Begin
    Seg := JC[SubjC].Seg;
    SubjText := Seg.Text;
    If Seg.Pron Then Sp := Seg.Pers Else If Seg.Plural Then Sp := 6 Else Sp :=
        3;
    If (Seg.Pron) And (Seg.Text = 'I') Then Sp := 1;
    If (Seg.Pron) And (Seg.Text = 'you') Then Sp := 2;
    If Seg.IsWh Then
    Begin
      WhSubject := True;
      Sp := 3;
    End;
    If (PKind = 'A') Or (PKind = 'N') Then
      If (Seg.Text = 'this') Or (Seg.Text = 'that') Then Sp := 3;
  End
  Else If SubjText = 'it' Then
    Sp := 3
  Else If (PKind = 'A') And InList(VerbEn, 'cold hot warm cool') And (Not PQ)
      Then
  Begin
    SubjText := 'it';
    Sp := 3;
  End
  Else If G.Mode = 't' Then
  Begin
    SubjText := 'there';
    Sp := 3;
  End
  Else If (PKind = 'N') And (Wh <> '') Then
  Begin
    SubjText := 'it';
    Sp := 3;
  End
  Else If PReq Or PLet Then
    SubjText := ''
  Else If PQ Then
  Begin
    SubjText := 'you';
    Sp := 2;
  End
  Else
  Begin
    SubjText := 'I';
    Sp := 1;
  End;
  If (G.Mode = 't') And (ObjTxt <> '') Then
    If Pos('s', ObjTxt) = Length(ObjTxt) Then Sp := 6;

  { imperatif : te kudasai, mashou }
  If PReq Then
  Begin
    Res := Join('please', Join(VerbEn, Join(IObj, Join(ObjTxt, Join(Pp,
        Adv)))));
    JaTranslateClause := Res;
    Exit;
  End;
  If PLet Then
  Begin
    If DePivot Then Res := 'let us' Else Res := 'let''s';
    If PNeg Then Res := Res + ' not';
    JaTranslateClause := Join(Res, Join(VerbEn, Join(IObj, Join(ObjTxt, Join(Pp,
        Adv)))));
    Exit;
  End;
  If (PKind = 'V') And (PForm = 'd') And (Not PQ) And (Not HasSubj) And False
      Then Exit;

  Tail := Join(Tail, Join(IObj, Join(ObjTxt, Join(Pp, Adv))));
  If (G.Mode = 'm') And (PKind = 'V') Then G.MainEn := VerbEn;
  If (G.Mode = 'm') And (G.Modal = 'pouvoir') And (VerbEn = 'do') Then G.MainEn
      := 'do';
  BuildEn(G, Sp, Aux, Rest, LexBase, Post, LexTns);
  DoQ := PQ And (Not WhSubject);
  If WhSubject Then
  Begin
    If Wh = '' Then Wh := '';
  End;
  If DoQ Then
  Begin
    If Aux = '' Then
    Begin
      Aux := DoFormEn(Sp, LexTns);
      Rest := Join(LexBase, Post);
      Post := '';
      LexBase := '';
    End;
    If G.Neg Then AuxN := NegContract(Aux) Else AuxN := Aux;
    Res := Join(Wh, Join(AuxN, Join(SubjText, Join(Freq, Join(Rest, Join(Post,
        Tail))))));
  End
  Else
  Begin
    If G.Neg Then
    Begin
      If Aux = '' Then
      Begin
        Aux := DoFormEn(Sp, LexTns);
        Rest := Join(LexBase, Post);
        Post := '';
        LexBase := '';
      End;
      If Aux = 'am' Then AuxN := 'am not' Else AuxN := NegContract(Aux);
      Res := Join(Wh, Join(SubjText, Join(AuxN, Join(Freq, Join(Rest, Tail)))));
    End
    Else If Aux <> '' Then
      Res := Join(Wh, Join(SubjText, Join(Aux, Join(Freq, Join(Rest, Tail)))))
    Else
      Res := Join(Wh, Join(SubjText, Join(Freq, Join(FiniteEn(LexBase, LexTns,
          Sp), Join(Post, Tail)))));
  End;
  JaTranslateClause := Res;
  Nxt := 0;
  LocC := 0;
  Txt := '';
  Prep := '';
  Gen := False;
End;

{ ------------------------------------------------------------------ }
{ JAPONAIS -> ANGLAIS : phrases toutes faites et phrase complete      }
{ ------------------------------------------------------------------ }

Const
  PhrasesJa: Array[0..17] Of String = (
    'konnichiwa|Hello',
    'konbanwa|Good evening',
    'ohayou gozaimasu|Good morning',
    'ohayou|Good morning',
    'oyasumi nasai|Good night',
    'sayounara|Goodbye',
    'arigatou gozaimasu|Thank you',
    'doumo arigatou gozaimasu|Thank you very much',
    'arigatou|Thank you',
    'sumimasen|Excuse me',
    'gomen nasai|I am sorry',
    'hajimemashite|Nice to meet you',
    'o genki desu ka|How are you',
    'genki desu ka|How are you',
    'genki desu|I am fine',
    'onamae wa nan desu ka|What is your name',
    'douitashimashite|You are welcome',
    'ganbatte|Good luck'
  );

Function JaPhraseKey(A, B: Integer): String;
Var
  K: Integer;
  R: String;
Begin
  R := '';
  For K := A To B Do
  Begin
    If JT[K].Punct Then
    Begin
      JaPhraseKey := '#';
      Exit;
    End;
    R := Join(R, JT[K].W);
  End;
  JaPhraseKey := R;
End;

Function FindPhraseJa(Const Key: String): String;
Var
  N: Integer;
  Pair, K: String;
Begin
  FindPhraseJa := '';
  If Key = '#' Then Exit;
  For N := 0 To High(PhrasesJa) Do
  Begin
    Pair := PhrasesJa[N];
    K := CopyUntil(Pair, '|');
    If K = Key Then
    Begin
      FindPhraseJa := Pair;
      Exit;
    End;
  End;
End;

Function JaHasPredicate(A, B: Integer): Boolean;
Var
  I: Integer;
Begin
  JaHasPredicate := False;
  For I := A To B Do
    If JT[I].Cat In ['V', 'C', 'D'] Then JaHasPredicate := True;
End;

Function JaTranslateTokens(A, B: Integer; Q: Boolean): String;
Var
  K: Integer;
  Key: String;
Begin
  If A > B Then
  Begin
    JaTranslateTokens := '';
    Exit;
  End;
  Key := FindPhraseJa(JaPhraseKey(A, B));
  If Key <> '' Then
  Begin
    If A > 1 Then Key := LowerFirst(Key);
    JaTranslateTokens := Key;
    Exit;
  End;
  For K := A + 1 To B - 1 Do
    If JT[K].Cat = ',' Then
      If ((K - A = 1) And (JT[A].Cat = 'J')) Or (JaHasPredicate(A, K - 1) And
          JaHasPredicate(K + 1, B)) Then
      Begin
        JaTranslateTokens := JaTranslateTokens(A, K - 1, False) + ', ' +
            JaTranslateTokens(K + 1, B, Q);
        Exit;
      End;
  JaTranslateTokens := JaTranslateClause(A, B, Q);
End;

Function JaTranslateSentence(Const Sentence: String): String;
Var
  Res: String;
Begin
  JTokenize(Sentence);
  If JN = 0 Then
  Begin
    JaTranslateSentence := '';
    Exit;
  End;
  JClassify;
  Res := JaTranslateTokens(1, JN, JEnd = '?');
  Res := UpperFirst(FixEnglish(Res));
  If JEnd <> #0 Then Res := Res + JEnd;
  JaTranslateSentence := Res;
End;

{ ------------------------------------------------------------------ }
{ ESPAGNOL -> ANGLAIS : lexique, morphologie et jetons                }
{ ------------------------------------------------------------------ }

Type
  TSp = Record
    Es: String[26];
    En: String[28];
    Pos: String[3];
  End;

  TSTok = Record
    W, Orig: String[34];
    Cap, Punct, Acc: Boolean;
    Cat: Char; { N nom, A adjectif, V verbe, R adverbe, E preposition, C
        conjonction, J interjection,
                      W interrogatif, D article, S possessif, T demonstratif, Q
                        quantificateur, U nombre,
                      P pronom sujet, O pronom complement, G negation, Z inconnu
                        }
    Idx: Integer;
    Tns: Char; { p present, i preterit, m imparfait, f futur, c conditionnel, P
        participe, G gerondif, I infinitif, M imperatif }
    Slot: Integer;
    Plu: Boolean;
    En: String[30];
    Kind: Char;
    Cl: String[8];
  End;

Var
  SpLex: Array[1..1200] Of TSp;
  SpCount: Integer;
  ST: Array[0..MaxTokens + 1] Of TSTok;
  SN: Integer;
  SEnd: Char;
  SourceEs, SourceSq, SourcePt: Boolean;

Procedure LoadSpanish;
Var
  I: Integer;
  Line, Item, Es, Rest: String;
Begin
  SpCount := 0;
  For I := 0 To High(SpData) Do
  Begin
    Line := SpData[I];
    While Line <> '' Do
    Begin
      Item := CopyUntil(Line, ';');
      If (Item <> '') And (SpCount < 900) Then
      Begin
        Es := CopyUntil(Item, '=');
        Rest := CopyUntil(Item, ':');
        Inc(SpCount);
        SpLex[SpCount].Es := ReplaceChar(Es, '_', ' ');
        SpLex[SpCount].En := ReplaceChar(Rest, '_', ' ');
        SpLex[SpCount].Pos := Item;
      End;
    End;
  End;
End;

{ Supprime les accents aigus et le trema (la ñ est conservee). }
Function SpPlain(Const S: String): String;
Var
  I: Integer;
  R: String;
Begin
  R := '';
  I := 1;
  While I <= Length(S) Do
  Begin
    If (S[I] = #$C3) And (I < Length(S)) Then
    Begin
      Case S[I + 1] Of
        #$A1: R := R + 'a';
        #$A9: R := R + 'e';
        #$AD: R := R + 'i';
        #$B3: R := R + 'o';
        #$BA: R := R + 'u';
        #$BC: R := R + 'u';
      Else
        R := R + S[I] + S[I + 1];
      End;
      Inc(I, 2);
    End
    Else
    Begin
      R := R + S[I];
      Inc(I);
    End;
  End;
  SpPlain := R;
End;

{ ---- formes des noms et des adjectifs ---- }

Function SpPlural(Const S: String): String;
Var
  L: Integer;
Begin
  L := Length(S);
  If L = 0 Then
    SpPlural := S
  Else If InList(S, 'lunes martes miercoles jueves viernes') Then
    SpPlural := S
  Else If S[L] In ['a', 'e', 'i', 'o', 'u'] Then
    SpPlural := S + 's'
  Else If S[L] = 'z' Then
    SpPlural := Copy(S, 1, L - 1) + 'ces'
  Else
    SpPlural := S + 'es';
End;

Function SpFindNoun(Const W: String; Var Plu: Boolean): Integer;
Var
  I: Integer;
  P: String;
Begin
  SpFindNoun := 0;
  For I := 1 To SpCount Do
    If SpLex[I].Pos = 'n' Then
    Begin
      P := SpPlain(SpLex[I].Es);
      If P = W Then
      Begin
        Plu := False;
        SpFindNoun := I;
        Exit;
      End;
    End;
  For I := 1 To SpCount Do
    If SpLex[I].Pos = 'n' Then
    Begin
      P := SpPlain(SpLex[I].Es);
      If SpPlural(P) = W Then
      Begin
        Plu := True;
        SpFindNoun := I;
        Exit;
      End;
    End;
End;

Function SpFindAdj(Const W: String; Var Plu: Boolean): Integer;
Var
  I, L: Integer;
  B, Stem: String;
Begin
  SpFindAdj := 0;
  For I := 1 To SpCount Do
    If SpLex[I].Pos = 'a' Then
    Begin
      B := SpPlain(SpLex[I].Es);
      L := Length(B);
      Plu := False;
      If W = B Then
      Begin
        SpFindAdj := I;
        Exit;
      End;
      If B[L] = 'o' Then
      Begin
        Stem := Copy(B, 1, L - 1);
        If W = Stem + 'a' Then Begin SpFindAdj := I; Exit; End;
        Plu := True;
        If (W = Stem + 'os') Or (W = Stem + 'as') Then Begin SpFindAdj := I;
            Exit; End;
        Plu := False;
        If (B = 'bueno') And (W = 'buen') Then Begin SpFindAdj := I; Exit; End;
        If (B = 'malo') And (W = 'mal') Then Begin SpFindAdj := I; Exit; End;
        If (B = 'primero') And (W = 'primer') Then Begin SpFindAdj := I; Exit;
            End;
      End
      Else
      Begin
        Plu := True;
        If W = SpPlural(B) Then Begin SpFindAdj := I; Exit; End;
        Plu := False;
        If W = B + 'a' Then Begin SpFindAdj := I; Exit; End;
        Plu := True;
        If W = B + 'as' Then Begin SpFindAdj := I; Exit; End;
        Plu := False;
        If (B = 'grande') And (W = 'gran') Then Begin SpFindAdj := I; Exit; End;
      End;
    End;
End;

Function SpFind(Const W: String; Const Kinds: String): Integer;
Var
  I: Integer;
  P: String;
Begin
  SpFind := 0;
  For I := 1 To SpCount Do
    If (Pos(SpLex[I].Pos, Kinds) > 0) Then
    Begin
      P := SpPlain(SpLex[I].Es);
      If P = W Then
      Begin
        SpFind := I;
        Exit;
      End;
    End;
End;

{ ---- conjugaison espagnole ---- }

Function SpStemCode(Const Inf: String): Char;
Begin
  If InList(Inf,
      'querer pensar entender sentir empezar cerrar perder preferir tener venir'
      ) Then SpStemCode := 'e'
  Else If InList(Inf,
      'encontrar poder dormir volver mostrar costar recordar morir llover') Then
      SpStemCode := 'o'
  Else If InList(Inf, 'pedir elegir decir') Then SpStemCode := 'i'
  Else If Inf = 'jugar' Then SpStemCode := 'u'
  Else SpStemCode := ' ';
End;

Function SpChangeStem(Const Stem: String; Code: Char): String;
Var
  I: Integer;
Begin
  SpChangeStem := Stem;
  For I := Length(Stem) Downto 1 Do
  Begin
    If (Code = 'e') And (Stem[I] = 'e') Then
    Begin
      SpChangeStem := Copy(Stem, 1, I - 1) + 'ie' + Copy(Stem, I + 1, 20);
      Exit;
    End;
    If (Code = 'i') And (Stem[I] = 'e') Then
    Begin
      SpChangeStem := Copy(Stem, 1, I - 1) + 'i' + Copy(Stem, I + 1, 20);
      Exit;
    End;
    If (Code = 'o') And (Stem[I] = 'o') Then
    Begin
      SpChangeStem := Copy(Stem, 1, I - 1) + 'ue' + Copy(Stem, I + 1, 20);
      Exit;
    End;
    If (Code = 'u') And (Stem[I] = 'u') Then
    Begin
      SpChangeStem := Copy(Stem, 1, I - 1) + 'ue' + Copy(Stem, I + 1, 20);
      Exit;
    End;
  End;
End;

Function SpChangeO(Const Stem: String): String;
Var
  I: Integer;
Begin
  SpChangeO := Stem;
  For I := Length(Stem) Downto 1 Do
    If Stem[I] = 'o' Then
    Begin
      SpChangeO := Copy(Stem, 1, I - 1) + 'u' + Copy(Stem, I + 1, 20);
      Exit;
    End;
End;

Function SpYoPres(Const Inf, Default: String): String;
Begin
  If Inf = 'tener' Then SpYoPres := 'tengo'
  Else If Inf = 'venir' Then SpYoPres := 'vengo'
  Else If Inf = 'poner' Then SpYoPres := 'pongo'
  Else If Inf = 'salir' Then SpYoPres := 'salgo'
  Else If Inf = 'hacer' Then SpYoPres := 'hago'
  Else If Inf = 'decir' Then SpYoPres := 'digo'
  Else If Inf = 'traer' Then SpYoPres := 'traigo'
  Else If Inf = 'conocer' Then SpYoPres := 'conozco'
  Else If Inf = 'conducir' Then SpYoPres := 'conduzco'
  Else If Inf = 'parecer' Then SpYoPres := 'parezco'
  Else If Inf = 'nacer' Then SpYoPres := 'nazco'
  Else If Inf = 'saber' Then SpYoPres := 'se'
  Else If Inf = 'elegir' Then SpYoPres := 'elijo'
  Else If Inf = 'construir' Then SpYoPres := 'construyo'
  Else SpYoPres := Default;
End;

Function SpPretStem(Const Inf: String): String;
Begin
  If Inf = 'tener' Then SpPretStem := 'tuv'
  Else If Inf = 'estar' Then SpPretStem := 'estuv'
  Else If Inf = 'poder' Then SpPretStem := 'pud'
  Else If Inf = 'poner' Then SpPretStem := 'pus'
  Else If Inf = 'saber' Then SpPretStem := 'sup'
  Else If Inf = 'querer' Then SpPretStem := 'quis'
  Else If Inf = 'haber' Then SpPretStem := 'hub'
  Else If Inf = 'venir' Then SpPretStem := 'vin'
  Else If Inf = 'hacer' Then SpPretStem := 'hic'
  Else If Inf = 'decir' Then SpPretStem := 'dij'
  Else If Inf = 'traer' Then SpPretStem := 'traj'
  Else If Inf = 'conducir' Then SpPretStem := 'conduj'
  Else SpPretStem := '';
End;

Function SpFutStem(Const Inf: String): String;
Begin
  If Inf = 'tener' Then SpFutStem := 'tendr'
  Else If Inf = 'venir' Then SpFutStem := 'vendr'
  Else If Inf = 'poner' Then SpFutStem := 'pondr'
  Else If Inf = 'salir' Then SpFutStem := 'saldr'
  Else If Inf = 'hacer' Then SpFutStem := 'har'
  Else If Inf = 'decir' Then SpFutStem := 'dir'
  Else If Inf = 'poder' Then SpFutStem := 'podr'
  Else If Inf = 'querer' Then SpFutStem := 'querr'
  Else If Inf = 'saber' Then SpFutStem := 'sabr'
  Else If Inf = 'haber' Then SpFutStem := 'habr'
  Else SpFutStem := Inf;
End;

Function SpParticiple(Const Inf: String): String;
Var
  Stem: String;
Begin
  Stem := Copy(Inf, 1, Length(Inf) - 2);
  If Inf = 'hacer' Then SpParticiple := 'hecho'
  Else If Inf = 'decir' Then SpParticiple := 'dicho'
  Else If Inf = 'ver' Then SpParticiple := 'visto'
  Else If Inf = 'poner' Then SpParticiple := 'puesto'
  Else If Inf = 'escribir' Then SpParticiple := 'escrito'
  Else If Inf = 'abrir' Then SpParticiple := 'abierto'
  Else If Inf = 'volver' Then SpParticiple := 'vuelto'
  Else If Inf = 'romper' Then SpParticiple := 'roto'
  Else If Inf = 'morir' Then SpParticiple := 'muerto'
  Else If EndsWith(Inf, 'ar') Then SpParticiple := Stem + 'ado'
  Else SpParticiple := Stem + 'ido';
End;

Function SpGerund(Const Inf: String): String;
Var
  Stem: String;
  L: Integer;
Begin
  Stem := Copy(Inf, 1, Length(Inf) - 2);
  L := Length(Stem);
  If Inf = 'ir' Then SpGerund := 'yendo'
  Else If Inf = 'decir' Then SpGerund := 'diciendo'
  Else If Inf = 'venir' Then SpGerund := 'viniendo'
  Else If Inf = 'poder' Then SpGerund := 'pudiendo'
  Else If Inf = 'reir' Then SpGerund := 'riendo'
  Else If Inf = 'dormir' Then SpGerund := 'durmiendo'
  Else If Inf = 'morir' Then SpGerund := 'muriendo'
  Else If Inf = 'pedir' Then SpGerund := 'pidiendo'
  Else If Inf = 'sentir' Then SpGerund := 'sintiendo'
  Else If Inf = 'preferir' Then SpGerund := 'prefiriendo'
  Else If Inf = 'elegir' Then SpGerund := 'eligiendo'
  Else If EndsWith(Inf, 'ar') Then SpGerund := Stem + 'ando'
  Else If (L > 0) And (Stem[L] In ['a', 'e', 'o']) Then SpGerund := Stem +
      'yendo'
  Else If EndsWith(Inf, 'uir') Then SpGerund := Stem + 'yendo'
  Else SpGerund := Stem + 'iendo';
End;

{ Reconnait un mot comme forme d'un verbe : temps et personne. }
Function SpMatchVerb(Const W: String; Var Idx: Integer; Var Tns: Char; Var Slot:
    Integer; PreferPast: Boolean): Boolean;
Const
  EndAr: Array[1..6] Of String[6] = ('o', 'as', 'a', 'amos', 'ais', 'an');
  EndEr: Array[1..6] Of String[6] = ('o', 'es', 'e', 'emos', 'eis', 'en');
  EndIr: Array[1..6] Of String[6] = ('o', 'es', 'e', 'imos', 'is', 'en');
  PretAr: Array[1..6] Of String[8] = ('e', 'aste', 'o', 'amos', 'asteis',
      'aron');
  PretEr: Array[1..6] Of String[8] = ('i', 'iste', 'io', 'imos', 'isteis',
      'ieron');
  PretIrr: Array[1..6] Of String[8] = ('e', 'iste', 'o', 'imos', 'isteis',
      'ieron');
  ImpAr: Array[1..6] Of String[8] = ('aba', 'abas', 'aba', 'abamos', 'abais',
      'aban');
  ImpEr: Array[1..6] Of String[8] = ('ia', 'ias', 'ia', 'iamos', 'iais', 'ian');
  FutE: Array[1..6] Of String[6] = ('e', 'as', 'a', 'emos', 'eis', 'an');
  CondE: Array[1..6] Of String[8] = ('ia', 'ias', 'ia', 'iamos', 'iais', 'ian');
  SerP: Array[1..6] Of String[6] = ('soy', 'eres', 'es', 'somos', 'sois',
      'son');
  SerI: Array[1..6] Of String[6] = ('fui', 'fuiste', 'fue', 'fuimos',
      'fuisteis', 'fueron');
  SerM: Array[1..6] Of String[6] = ('era', 'eras', 'era', 'eramos', 'erais',
      'eran');
  EstarP: Array[1..6] Of String[8] = ('estoy', 'estas', 'esta', 'estamos',
      'estais', 'estan');
  IrP: Array[1..6] Of String[6] = ('voy', 'vas', 'va', 'vamos', 'vais', 'van');
  IrM: Array[1..6] Of String[6] = ('iba', 'ibas', 'iba', 'ibamos', 'ibais',
      'iban');
  HaberP: Array[1..6] Of String[8] = ('he', 'has', 'ha', 'hemos', 'habeis',
      'han');
  DarP: Array[1..6] Of String[6] = ('doy', 'das', 'da', 'damos', 'dais', 'dan');
  DarI: Array[1..6] Of String[8] = ('di', 'diste', 'dio', 'dimos', 'disteis',
      'dieron');
  VerP: Array[1..6] Of String[6] = ('veo', 'ves', 've', 'vemos', 'veis', 'ven');
  VerI: Array[1..6] Of String[8] = ('vi', 'viste', 'vio', 'vimos', 'visteis',
      'vieron');
  VerM: Array[1..6] Of String[8] = ('veia', 'veias', 'veia', 'veiamos',
      'veiais', 'veian');
  OirP: Array[1..6] Of String[6] = ('oigo', 'oyes', 'oye', 'oimos', 'ois',
      'oyen');
Var
  I, Q: Integer;
  Inf, Stem, Cls, Ch, YoP, PS, FS, Form: String;
  Code: Char;
  Found, Past1: Boolean;

  Function TryF(Const Cand: String; T: Char; Sl: Integer): Boolean;
  Begin
    TryF := False;
    If Cand = W Then
    Begin
      Idx := I;
      Tns := T;
      Slot := Sl;
      TryF := True;
    End;
  End;

Begin
  SpMatchVerb := True;
  For I := 1 To SpCount Do
    If SpLex[I].Pos = 'v' Then
    Begin
      Inf := SpPlain(SpLex[I].Es);
      Stem := Copy(Inf, 1, Length(Inf) - 2);
      Cls := Copy(Inf, Length(Inf) - 1, 2);
      Code := SpStemCode(Inf);
      Past1 := False;
      If PreferPast Then
      Begin
        { forme accentuee : preterit ou futur d'abord (llego/llego : llego est
          present, llego accentue = preterit) }
        PS := SpPretStem(Inf);
        For Q := 1 To 6 Do
        Begin
          If Inf = 'ser' Then Form := SerI[Q]
          Else If Inf = 'ir' Then Form := SerI[Q]
          Else If Inf = 'dar' Then Form := DarI[Q]
          Else If Inf = 'ver' Then Form := VerI[Q]
          Else If PS <> '' Then Form := PS + PretIrr[Q]
          Else If Cls = 'ar' Then Form := Stem + PretAr[Q]
          Else Form := Stem + PretEr[Q];
          If (Q In [3, 1]) And (Cls = 'ar') Then
            If TryF(Form, 'i', Q) Then Exit;
          If (Q In [1, 3]) And (Cls <> 'ar') Then
            If TryF(Form, 'i', Q) Then Exit;
        End;
        FS := SpFutStem(Inf);
        For Q := 1 To 6 Do
          If (Q = 1) Or (Q = 3) Then
            If TryF(FS + FutE[Q], 'f', Q) Then Exit;
        { pas de correspondance accentuee : on continue normalement }
        Past1 := True;
      End;
      { present }
      For Q := 1 To 6 Do
      Begin
        Form := '';
        If Inf = 'ser' Then Form := SerP[Q]
        Else If Inf = 'estar' Then Form := EstarP[Q]
        Else If Inf = 'ir' Then Form := IrP[Q]
        Else If Inf = 'haber' Then Form := HaberP[Q]
        Else If Inf = 'dar' Then Form := DarP[Q]
        Else If Inf = 'ver' Then Form := VerP[Q]
        Else If Inf = 'oir' Then Form := OirP[Q]
        Else
        Begin
          If Cls = 'ar' Then Ch := EndAr[Q] Else If Cls = 'er' Then Ch :=
              EndEr[Q] Else Ch := EndIr[Q];
          If (Code <> ' ') And (Q In [2, 3, 6]) Then Form := SpChangeStem(Stem,
              Code) + Ch
          Else If (Code <> ' ') And (Q = 1) Then Form := SpChangeStem(Stem,
              Code) + Ch
          Else Form := Stem + Ch;
          If Q = 1 Then Form := SpYoPres(Inf, Form);
        End;
        If TryF(Form, 'p', Q) Then Exit;
      End;
      If (Inf = 'haber') And TryF('hay', 'p', 3) Then Exit;
      { preterit }
      PS := SpPretStem(Inf);
      For Q := 1 To 6 Do
      Begin
        If Inf = 'ser' Then Form := SerI[Q]
        Else If Inf = 'ir' Then Form := SerI[Q]
        Else If Inf = 'dar' Then Form := DarI[Q]
        Else If Inf = 'ver' Then Form := VerI[Q]
        Else If PS <> '' Then
        Begin
          Form := PS + PretIrr[Q];
          If (Q = 6) And (PS[Length(PS)] = 'j') Then Form := PS + 'eron';
          If (Inf = 'hacer') And (Q = 3) Then Form := 'hizo';
        End
        Else
        Begin
          If Cls = 'ar' Then Form := Stem + PretAr[Q] Else Form := Stem +
              PretEr[Q];
          If (Cls = 'ar') And (Q = 1) Then
          Begin
            If Stem[Length(Stem)] = 'g' Then Form := Stem + 'ue'
            Else If Stem[Length(Stem)] = 'z' Then Form := Copy(Stem, 1,
                Length(Stem) - 1) + 'ce'
            Else If Stem[Length(Stem)] = 'c' Then Form := Copy(Stem, 1,
                Length(Stem) - 1) + 'que';
          End;
          If (Cls = 'ir') And (Q In [3, 6]) And (Code In ['e', 'o', 'i']) Then
          Begin
            If Code = 'o' Then Form := SpChangeO(Stem) Else Form :=
                SpChangeStem(Stem, 'i');
            If Q = 3 Then Form := Form + 'io' Else Form := Form + 'ieron';
          End;
          If (Cls <> 'ar') And (Q In [3, 6]) And (Stem <> '') And
              (Stem[Length(Stem)] In ['a', 'e', 'o', 'u']) Then
          Begin
            If Q = 3 Then Form := Stem + 'yo' Else Form := Stem + 'yeron';
          End;
        End;
        If TryF(Form, 'i', Q) Then Exit;
      End;
      { imparfait }
      For Q := 1 To 6 Do
      Begin
        If Inf = 'ser' Then Form := SerM[Q]
        Else If Inf = 'ir' Then Form := IrM[Q]
        Else If Inf = 'ver' Then Form := VerM[Q]
        Else If Cls = 'ar' Then Form := Stem + ImpAr[Q]
        Else Form := Stem + ImpEr[Q];
        If TryF(Form, 'm', Q) Then Exit;
      End;
      { futur et conditionnel }
      FS := SpFutStem(Inf);
      For Q := 1 To 6 Do
        If TryF(FS + FutE[Q], 'f', Q) Then Exit;
      For Q := 1 To 6 Do
        If TryF(FS + CondE[Q], 'c', Q) Then Exit;
      If TryF(SpParticiple(Inf), 'P', 0) Then Exit;
      If TryF(Copy(SpParticiple(Inf), 1, Length(SpParticiple(Inf)) - 1) + 'a',
          'P', 0) Then Exit;
      If TryF(Copy(SpParticiple(Inf), 1, Length(SpParticiple(Inf)) - 1) + 'os',
          'P', 0) Then Exit;
      If TryF(Copy(SpParticiple(Inf), 1, Length(SpParticiple(Inf)) - 1) + 'as',
          'P', 0) Then Exit;
      If TryF(SpGerund(Inf), 'G', 0) Then Exit;
      If TryF(Inf, 'I', 0) Then Exit;
      { imperatif de politesse (usted / ustedes) et de nosotros }
      YoP := '';
      If Inf = 'ser' Then YoP := 'se' Else If Inf = 'estar' Then YoP := 'est'
          Else If Inf = 'ir' Then YoP := 'vay'
      Else If Inf = 'haber' Then YoP := 'hay' Else If Inf = 'saber' Then YoP :=
          'sep' Else If Inf = 'dar' Then YoP := 'd'
      Else
      Begin
        YoP := SpYoPres(Inf, Stem + 'o');
        If Code <> ' ' Then YoP := SpChangeStem(Stem, Code) + 'o';
        YoP := SpYoPres(Inf, YoP);
        YoP := Copy(YoP, 1, Length(YoP) - 1);
      End;
      If Cls = 'ar' Then
      Begin
        If TryF(YoP + 'e', 'M', 3) Or TryF(YoP + 'en', 'M', 6) Then Exit;
      End
      Else If TryF(YoP + 'a', 'M', 3) Or TryF(YoP + 'an', 'M', 6) Then Exit;
    End;
  SpMatchVerb := False;
  Found := False;
End;

{ ------------------------------------------------------------------ }
{ ESPAGNOL -> ANGLAIS : tokenisation et classification               }
{ ------------------------------------------------------------------ }

Procedure SPush(Const W, Orig: String; Cap, Punct, Acc: Boolean);
Begin
  If SN >= MaxTokens Then Exit;
  Inc(SN);
  FillChar(ST[SN], SizeOf(TSTok), 0);
  ST[SN].W := W;
  ST[SN].Orig := Orig;
  ST[SN].Cap := Cap;
  ST[SN].Punct := Punct;
  ST[SN].Acc := Acc;
  ST[SN].Tns := ' ';
End;

Procedure STokenize(Const S0: String);
Var
  S, Word1, Low, Plain: String;
  I, L: Integer;
  C: Char;
  Cap: Boolean;
Begin
  SN := 0;
  SEnd := #0;
  S := '';
  I := 1;
  While I <= Length(S0) Do
  Begin
    If (S0[I] = #$C2) And (I < Length(S0)) And ((S0[I + 1] = #$BF) Or (S0[I + 1]
        = #$A1)) Then
      Inc(I, 2)
    Else If (S0[I] = #$C2) And (I < Length(S0)) And ((S0[I + 1] = #$AB) Or (S0[I
        + 1] = #$BB) Or (S0[I + 1] = #$A0)) Then
    Begin
      S := S + ' ';
      Inc(I, 2);
    End
    Else If (S0[I] = #$E2) And (I + 2 <= Length(S0)) And (S0[I + 1] = #$80) And
            ((S0[I + 2] = #$99) Or (S0[I + 2] = #$98)) Then
    Begin
      Inc(I, 3);
    End
    Else
    Begin
      S := S + S0[I];
      Inc(I);
    End;
  End;
  L := Length(S);
  I := 1;
  While I <= L Do
  Begin
    C := S[I];
    If FrWordByte(C) Then
    Begin
      Word1 := '';
      While (I <= L) And FrWordByte(S[I]) Do
      Begin
        Word1 := Word1 + S[I];
        Inc(I);
      End;
      Cap := (Word1 <> '') And (((Word1[1] >= 'A') And (Word1[1] <= 'Z')) Or
             ((Word1[1] = #$C3) And (Length(Word1) > 1) And (Word1[2] >= #$80)
                 And (Word1[2] <= #$9E)));
      Low := FrLower(Word1);
      Plain := SpPlain(Low);
      If Plain = 'del' Then
      Begin
        SPush('de', 'de', False, False, False);
        SPush('el', 'el', False, False, False);
      End
      Else If Plain = 'al' Then
      Begin
        SPush('a', 'a', False, False, False);
        SPush('el', 'el', False, False, False);
      End
      Else
        SPush(Plain, Word1, Cap, False, Plain <> Low);
    End
    Else
    Begin
      If (C = '.') Or (C = '?') Or (C = '!') Then
      Begin
        If SEnd = #0 Then SEnd := C;
        If C = '?' Then SEnd := '?';
      End
      Else If (C = ',') Or (C = ';') Or (C = ':') Then
      Begin
        SPush(',', ',', False, True, False);
        ST[SN].Cat := ',';
      End;
      Inc(I);
    End;
  End;
End;

Procedure SMergeMultiword;
Var
  I, K, Len, L: Integer;
  Key: String;
  Bad, Found: Boolean;
Begin
  I := 1;
  While I < SN Do
  Begin
    Found := False;
    For Len := 4 Downto 2 Do
      If (Not Found) And (I + Len - 1 <= SN) Then
      Begin
        Key := '';
        Bad := False;
        For K := I To I + Len - 1 Do
        Begin
          If ST[K].Punct Then Bad := True;
          Key := Join(Key, ST[K].W);
        End;
        If Not Bad Then
        Begin
          L := SpFind(Key, 'rpcjw');
          If L > 0 Then
          Begin
            Found := True;
            ST[I].W := Key;
            ST[I].Orig := Key;
            For K := I + 1 To SN - Len + 1 Do
              ST[K] := ST[K + Len - 1];
            Dec(SN, Len - 1);
          End;
        End;
      End;
    Inc(I);
  End;
End;

Function SpNumberEn(Const W: String): String;
Begin
  SpNumberEn := '';
  If W = 'dos' Then SpNumberEn := 'two'
  Else If W = 'tres' Then SpNumberEn := 'three'
  Else If W = 'cuatro' Then SpNumberEn := 'four'
  Else If W = 'cinco' Then SpNumberEn := 'five'
  Else If W = 'seis' Then SpNumberEn := 'six'
  Else If W = 'siete' Then SpNumberEn := 'seven'
  Else If W = 'ocho' Then SpNumberEn := 'eight'
  Else If W = 'nueve' Then SpNumberEn := 'nine'
  Else If W = 'diez' Then SpNumberEn := 'ten'
  Else If W = 'once' Then SpNumberEn := 'eleven'
  Else If W = 'doce' Then SpNumberEn := 'twelve'
  Else If W = 'trece' Then SpNumberEn := 'thirteen'
  Else If W = 'catorce' Then SpNumberEn := 'fourteen'
  Else If W = 'quince' Then SpNumberEn := 'fifteen'
  Else If W = 'dieciseis' Then SpNumberEn := 'sixteen'
  Else If W = 'diecisiete' Then SpNumberEn := 'seventeen'
  Else If W = 'dieciocho' Then SpNumberEn := 'eighteen'
  Else If W = 'diecinueve' Then SpNumberEn := 'nineteen'
  Else If W = 'veinte' Then SpNumberEn := 'twenty'
  Else If W = 'treinta' Then SpNumberEn := 'thirty'
  Else If W = 'cuarenta' Then SpNumberEn := 'forty'
  Else If W = 'cincuenta' Then SpNumberEn := 'fifty'
  Else If W = 'sesenta' Then SpNumberEn := 'sixty'
  Else If W = 'setenta' Then SpNumberEn := 'seventy'
  Else If W = 'ochenta' Then SpNumberEn := 'eighty'
  Else If W = 'noventa' Then SpNumberEn := 'ninety'
  Else If W = 'cien' Then SpNumberEn := 'one hundred'
  Else If W = 'mil' Then SpNumberEn := 'one thousand';
End;

Function SpPersonOfPron(Const W: String): Integer;
Begin
  If W = 'yo' Then SpPersonOfPron := 1
  Else If W = 'tu' Then SpPersonOfPron := 2
  Else If (W = 'nosotros') Or (W = 'nosotras') Then SpPersonOfPron := 4
  Else If (W = 'vosotros') Or (W = 'vosotras') Then SpPersonOfPron := 5
  Else If (W = 'ellos') Or (W = 'ellas') Or (W = 'ustedes') Then SpPersonOfPron
      := 6
  Else SpPersonOfPron := 3;
End;

Function SpIsStrongCat(Const C: Char): Boolean;
Begin
  SpIsStrongCat := C In ['D', 'S', 'T', 'Q', 'U'];
End;

Function SpNextIsVerbal(I: Integer): Boolean;
Var
  Idx, Sl: Integer;
  T: Char;
Begin
  SpNextIsVerbal := False;
  If I > SN Then Exit;
  If ST[I].Punct Then Exit;
  If InList(ST[I].W, 'no me te se lo la le nos os los las les') Then
  Begin
    SpNextIsVerbal := True;
    Exit;
  End;
  SpNextIsVerbal := SpMatchVerb(ST[I].W, Idx, T, Sl, ST[I].Acc) And (T In ['p',
      'i', 'm', 'f', 'c', 'M']);
End;

Function SpNextIsNounish(I: Integer): Boolean;
Var
  Plu: Boolean;
Begin
  SpNextIsNounish := False;
  If I > SN Then Exit;
  If ST[I].Punct Then Exit;
  SpNextIsNounish := (SpFindNoun(ST[I].W, Plu) > 0) Or (SpFindAdj(ST[I].W, Plu)
      > 0) Or
                     InList(ST[I].W, 'mejor peor mayor menor gran buen mal');
End;

Procedure PickSpCat(I: Integer);
Var
  W: String;
  Prev: Char;
  Idx, Sl: Integer;
  Plu, IsVerb, IsNoun, IsAdj: Boolean;
  T: Char;
  LA, LN: Integer;
  QStart: Boolean;
  Ch2: String;
  Cnt: Integer;
Begin
  W := ST[I].W;
  If I > 1 Then Prev := ST[I - 1].Cat Else Prev := '.';
  QStart := (SEnd = '?') And ((I = 1) Or (ST[I - 1].Cat = ','));
  If ST[I].Punct Then
  Begin
    ST[I].Cat := ',';
    Exit;
  End;
  If IsDigitsOnly(W) Then
  Begin
    ST[I].Cat := 'U';
    ST[I].En := W;
    Exit;
  End;
  If SpNumberEn(W) <> '' Then
  Begin
    ST[I].Cat := 'U';
    ST[I].En := SpNumberEn(W);
    Exit;
  End;
  { negation }
  If W = 'no' Then
  Begin
    ST[I].Cat := 'G';
    Exit;
  End;
  If (W = 'nunca') Or (W = 'jamas') Then
  Begin
    ST[I].Cat := 'R';
    ST[I].En := 'never';
    Exit;
  End;
  If (W = 'nada') Or (W = 'nadie') Or (W = 'tampoco') Then
  Begin
    ST[I].Cat := 'g';
    If W = 'nada' Then ST[I].En := 'anything' Else If W = 'nadie' Then ST[I].En
        := 'anyone' Else ST[I].En := 'either';
    Exit;
  End;
  { pronoms sujets }
  If InList(W,
      'yo ella usted ustedes nosotros nosotras vosotros vosotras ellos ellas')
      Or
     ((W = 'tu') And ST[I].Acc) Or ((W = 'el') And ST[I].Acc) Then
  Begin
    ST[I].Cat := 'P';
    ST[I].Slot := SpPersonOfPron(W);
    If W = 'yo' Then ST[I].En := 'I'
    Else If W = 'tu' Then ST[I].En := 'you'
    Else If W = 'el' Then ST[I].En := 'he'
    Else If W = 'ella' Then ST[I].En := 'she'
    Else If (W = 'usted') Or (W = 'ustedes') Or (W = 'vosotros') Or (W =
        'vosotras') Then ST[I].En := 'you'
    Else If (W = 'nosotros') Or (W = 'nosotras') Then ST[I].En := 'we'
    Else ST[I].En := 'they';
    If W = 'usted' Then ST[I].Slot := 3;
    If W = 'ustedes' Then ST[I].Slot := 6;
    Exit;
  End;
  { pronoms apres une preposition }
  If ((W = 'mi') And ST[I].Acc) Or ((W = 'mi') And (Prev = 'E') And (Not
      SpNextIsNounish(I + 1))) Then
  Begin
    ST[I].Cat := 'B';
    ST[I].En := 'me';
    Exit;
  End;
  If (W = 'ti') Or (W = 'contigo') Or (W = 'conmigo') Then
  Begin
    ST[I].Cat := 'B';
    If W = 'ti' Then ST[I].En := 'you' Else If W = 'contigo' Then ST[I].En :=
        'with you' Else ST[I].En := 'with me';
    Exit;
  End;
  { articles et determinants }
  If InList(W, 'el la los las') Then
  Begin
    If (InList(W, 'lo la los las')) And (Not SpNextIsNounish(I + 1)) And
        SpNextIsVerbal(I + 1) Then
    Begin
      ST[I].Cat := 'O';
      If W = 'lo' Then ST[I].En := 'it' Else If W = 'la' Then ST[I].En := 'her'
          Else ST[I].En := 'them';
      If (W = 'los') Or (W = 'las') Then ST[I].En := 'them';
      Exit;
    End;
    ST[I].Cat := 'D';
    ST[I].Kind := 'd';
    ST[I].Plu := (W = 'los') Or (W = 'las');
    Exit;
  End;
  If W = 'lo' Then
  Begin
    ST[I].Cat := 'O';
    ST[I].En := 'it';
    Exit;
  End;
  If InList(W, 'un una') Then
  Begin
    ST[I].Cat := 'D';
    ST[I].Kind := 'i';
    Exit;
  End;
  If InList(W, 'unos unas') Then
  Begin
    ST[I].Cat := 'D';
    ST[I].Kind := 'i';
    ST[I].Plu := True;
    ST[I].En := 'some';
    Exit;
  End;
  If InList(W,
      'mi mis tu tus su sus nuestro nuestra nuestros nuestras vuestro vuestra '+
      'vuestros vuestras') Then
  Begin
    ST[I].Cat := 'S';
    If (W = 'mi') Or (W = 'mis') Then ST[I].En := 'my'
    Else If (W = 'tu') Or (W = 'tus') Then ST[I].En := 'your'
    Else If (W = 'su') Or (W = 'sus') Then ST[I].En := 'his'
    Else If Copy(W, 1, 3) = 'nue' Then ST[I].En := 'our'
    Else ST[I].En := 'your';
    ST[I].Plu := EndsWith(W, 's');
    Exit;
  End;
  If InList(W,
      'este esta esto estos estas ese esa eso esos esas aquel aquella aquello '+
      'aquellos aquellas') And
     Not (ST[I].Acc Or ((Copy(W, 1, 3) = 'est') And (Not SpNextIsNounish(I + 1))
         And (W <> 'esto'))) Then
  Begin
    ST[I].Cat := 'T';
    If Copy(W, 1, 3) = 'est' Then ST[I].En := 'this' Else ST[I].En := 'that';
    ST[I].Plu := EndsWith(W, 's');
    If ST[I].Plu Then
      If ST[I].En = 'this' Then ST[I].En := 'these' Else ST[I].En := 'those';
    If (W = 'esto') Or (W = 'eso') Or (W = 'aquello') Then ST[I].Kind := 'p';
    Exit;
  End;
  If InList(W,
      'todo toda todos todas cada algunos algunas alguno alguna ningun ninguno'+
      ' ninguna varios varias otros otras bastante demasiado') Or
     ((InList(W, 'mucho mucha muchos muchas poco poca pocos pocas')) And
         SpNextIsNounish(I + 1)) Then
  Begin
    ST[I].Cat := 'Q';
    ST[I].Plu := EndsWith(W, 's');
    If Copy(W, 1, 4) = 'todo' Then ST[I].En := 'all'
    Else If W = 'cada' Then ST[I].En := 'each'
    Else If Copy(W, 1, 4) = 'much' Then ST[I].En := 'many'
    Else If Copy(W, 1, 4) = 'poco' Then ST[I].En := 'few'
    Else If Copy(W, 1, 3) = 'alg' Then ST[I].En := 'some'
    Else If Copy(W, 1, 3) = 'nin' Then ST[I].En := 'no'
    Else If Copy(W, 1, 3) = 'var' Then ST[I].En := 'several'
    Else If Copy(W, 1, 3) = 'otr' Then ST[I].En := 'other'
    Else ST[I].En := 'enough';
    If (ST[I].En = 'many') And (Not ST[I].Plu) Then ST[I].En := 'much';
    Exit;
  End;
  { pronoms complements }
  If (InList(W, 'me nos os les le se') And Not ((W = 'se') And ST[I].Acc)) Or
      ((W = 'te') And (Prev <> 'D')) Then
  Begin
    ST[I].Cat := 'O';
    If W = 'me' Then ST[I].En := 'me'
    Else If W = 'te' Then ST[I].En := 'you'
    Else If W = 'nos' Then ST[I].En := 'us'
    Else If W = 'os' Then ST[I].En := 'you'
    Else If W = 'les' Then ST[I].En := 'them'
    Else If W = 'le' Then ST[I].En := 'him'
    Else ST[I].En := '';
    Exit;
  End;
  { mots grammaticaux du lexique }
  Idx := SpFind(W, 'w');
  If (Idx > 0) And (ST[I].Acc Or QStart) Then
  Begin
    ST[I].Cat := 'W';
    ST[I].Idx := Idx;
    ST[I].En := SpLex[Idx].En;
    Exit;
  End;
  If (W = 'sí') Or ((W = 'si') And ST[I].Acc) Then
  Begin
    ST[I].Cat := 'J';
    ST[I].En := 'Yes';
    Exit;
  End;
  Idx := SpFind(W, 'p');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'E';
    ST[I].Idx := Idx;
    ST[I].En := SpLex[Idx].En;
    Exit;
  End;
  Idx := SpFind(W, 'c');
  If (Idx > 0) And (W = 'como') And (Not ST[I].Acc) And (Not SpNextIsVerbal(I +
      1)) And
     ((I = 1) Or (Prev In ['P', 'G'])) And (Not QStart) Then Idx := 0;
  If (Idx > 0) And Not ((W = 'como') And QStart) Then
  Begin
    ST[I].Cat := 'C';
    ST[I].Idx := Idx;
    ST[I].En := SpLex[Idx].En;
    Exit;
  End;
  Idx := SpFind(W, 'j');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'J';
    ST[I].Idx := Idx;
    ST[I].En := SpLex[Idx].En;
    Exit;
  End;
  Idx := SpFind(W, 'w');
  If (Idx > 0) And Not ((W = 'como') And (Not ST[I].Acc) And (Not QStart) And
      (Not SpNextIsVerbal(I + 1))) Then
  Begin
    ST[I].Cat := 'W';
    ST[I].Idx := Idx;
    ST[I].En := SpLex[Idx].En;
    Exit;
  End;
  Idx := SpFind(W, 'r');
  If Idx > 0 Then
  Begin
    If (Prev = 'D') And ((W = 'mañana') Or (W = 'tarde')) Then
    Begin
      ST[I].Cat := 'N';
      If W = 'mañana' Then ST[I].En := 'morning' Else ST[I].En := 'afternoon';
      Exit;
    End;
    ST[I].Cat := 'R';
    ST[I].Idx := Idx;
    ST[I].En := SpLex[Idx].En;
    If SpNextIsNounish(I + 1) And InList(W, 'mucho poco') Then ST[I].Cat := 'Q';
    Exit;
  End;
  { noms, adjectifs, verbes }
  LN := SpFindNoun(W, Plu);
  IsNoun := LN > 0;
  If IsNoun Then ST[I].Plu := Plu;
  LA := SpFindAdj(W, Plu);
  IsAdj := LA > 0;
  IsVerb := SpMatchVerb(W, Idx, T, Sl, ST[I].Acc);
  If (W = 'te') And (Prev = 'D') Then IsVerb := False;
  If IsVerb And IsNoun And SpIsStrongCat(Prev) Then IsVerb := False;
  If IsVerb And IsAdj And SpIsStrongCat(Prev) Then IsVerb := False;
  If IsVerb And (Not (T In ['p', 'i', 'm', 'f', 'c', 'M'])) And (IsNoun Or
      IsAdj) And (Prev <> 'V') Then IsVerb := False;
  If IsVerb And (T = 'P') And IsAdj Then IsVerb := False;
  If IsVerb Then
  Begin
    SpMatchVerb(W, Idx, T, Sl, ST[I].Acc);
    ST[I].Cat := 'V';
    ST[I].Idx := Idx;
    ST[I].Tns := T;
    ST[I].Slot := Sl;
    ST[I].En := SpLex[Idx].En;
    Exit;
  End;
  If IsNoun And IsAdj Then
  Begin
    If SpIsStrongCat(Prev) Or (Prev = 'A') Then IsAdj := False Else IsNoun :=
        False;
  End;
  If IsNoun Then
  Begin
    SpFindNoun(W, Plu);
    ST[I].Cat := 'N';
    ST[I].Idx := LN;
    ST[I].Plu := Plu;
    ST[I].En := SpLex[LN].En;
    Exit;
  End;
  If IsAdj Then
  Begin
    SpFindAdj(W, Plu);
    ST[I].Cat := 'A';
    ST[I].Idx := LA;
    ST[I].Plu := Plu;
    ST[I].En := SpLex[LA].En;
    Exit;
  End;
  { verbe suivi d'un pronom enclitique : ayudarme, dandolo }
  For Sl := 1 To 11 Do
  Begin
    Case Sl Of
      1: Ch2 := 'me';
      2: Ch2 := 'te';
      3: Ch2 := 'lo';
      4: Ch2 := 'la';
      5: Ch2 := 'le';
      6: Ch2 := 'los';
      7: Ch2 := 'las';
      8: Ch2 := 'les';
      9: Ch2 := 'nos';
      10: Ch2 := 'os';
    Else
      Ch2 := 'se';
    End;
    If (Length(W) > Length(Ch2) + 3) And (Copy(W, Length(W) - Length(Ch2) + 1,
        5) = Ch2) Then
      If SpMatchVerb(Copy(W, 1, Length(W) - Length(Ch2)), Idx, T, Cnt, False)
          And (T In ['I', 'G']) Then
      Begin
        ST[I].Cat := 'V';
        ST[I].Idx := Idx;
        ST[I].Tns := T;
        ST[I].En := SpLex[Idx].En;
        If Ch2 = 'me' Then ST[I].Cl := 'me'
        Else If Ch2 = 'te' Then ST[I].Cl := 'you'
        Else If Ch2 = 'lo' Then ST[I].Cl := 'it'
        Else If Ch2 = 'la' Then ST[I].Cl := 'her'
        Else If Ch2 = 'le' Then ST[I].Cl := 'him'
        Else If (Ch2 = 'los') Or (Ch2 = 'las') Or (Ch2 = 'les') Then ST[I].Cl :=
            'them'
        Else If Ch2 = 'nos' Then ST[I].Cl := 'us'
        Else If Ch2 = 'os' Then ST[I].Cl := 'you'
        Else ST[I].Cl := ' ';
        Exit;
      End;
  End;
  ST[I].Cat := 'Z';
End;

Procedure SpClassify;
Var
  I: Integer;
Begin
  SMergeMultiword;
  For I := 1 To SN Do
    PickSpCat(I);
End;

{ ------------------------------------------------------------------ }
{ ESPAGNOL -> ANGLAIS : groupes nominaux et elements                  }
{ ------------------------------------------------------------------ }

Type
  TSNP = Record
    Found, Proper, Plural, Pron, Poss: Boolean;
    Used: Integer;
    Kind: Char;
    DetTok, Head, PronTok: Integer;
    Adjs: Array[1..4] Of Integer;
    AdjAdv: Array[1..4] Of String[10];
    AdjN: Integer;
    Text: String[70];
    Owner: String[40];
  End;

  TSItem = Record
    Kind: Char; { N groupe nominal, E groupe prepositionnel, A adjectif, R
        adverbe, W interrogatif }
    Text: String[100];
    Pron, Plural, Proper, Person: Boolean;
    First, Last: Integer;
    Pers: Integer;
    HeadEn: String[30];
    Prep: String[16];
  End;

Var
  SI: Array[1..24] Of TSItem;
  SIN: Integer;

Function IsExistLikeMotion(Const En: String): Boolean;
Begin
  IsExistLikeMotion := InList(En,
      'go come arrive return enter leave travel walk run drive');
End;

Function SpPlainOrig(Const S: String): String;
Begin
  SpPlainOrig := S;
End;

Function SpProperEn(Const W, Orig: String): String;
Begin
  If W = 'paris' Then SpProperEn := 'Paris'
  Else If W = 'londres' Then SpProperEn := 'London'
  Else If W = 'roma' Then SpProperEn := 'Rome'
  Else If W = 'lisboa' Then SpProperEn := 'Lisbon'
  Else If W = 'españa' Then SpProperEn := 'Spain'
  Else If W = 'francia' Then SpProperEn := 'France'
  Else If W = 'alemania' Then SpProperEn := 'Germany'
  Else If W = 'italia' Then SpProperEn := 'Italy'
  Else If W = 'inglaterra' Then SpProperEn := 'England'
  Else If W = 'japon' Then SpProperEn := 'Japan'
  Else If W = 'mexico' Then SpProperEn := 'Mexico'
  Else If W = 'estados unidos' Then SpProperEn := 'the United States'
  Else If W = 'nueva york' Then SpProperEn := 'New York'
  Else SpProperEn := SpPlainOrig(Orig);
End;

Function SpIsPersonEn(Const En: String): Boolean;
Begin
  SpIsPersonEn := InList(En,
      'person man woman boy girl child friend family mother father brother sis'+
      'ter son daughter husband wife teacher student doctor nurse people');
End;

Function ParseNPEs(Start: Integer; Var NP: TSNP): Boolean;
Var
  I: Integer;
  Owner: TSNP;
Begin
  FillChar(NP, SizeOf(NP), 0);
  NP.Kind := '0';
  ParseNPEs := False;
  I := Start;
  If I > SN Then Exit;
  If ST[I].Cat In ['P', 'B'] Then
  Begin
    NP.Found := True;
    NP.Pron := True;
    NP.PronTok := I;
    NP.Used := 1;
    ParseNPEs := True;
    Exit;
  End;
  If (ST[I].Cat = 'Q') And (ST[I].En = 'all') And (I < SN) And (ST[I + 1].Cat =
      'D') Then
  Begin
    NP.Kind := 'q';
    NP.DetTok := I;
    Inc(I);
    NP.Plural := ST[I - 1].Plu;
  End;
  Case ST[I].Cat Of
    'D', 'S', 'T', 'Q':
    Begin
      If NP.DetTok = 0 Then
      Begin
        NP.DetTok := I;
        If ST[I].Cat = 'D' Then NP.Kind := ST[I].Kind
        Else If ST[I].Cat = 'S' Then NP.Kind := 's'
        Else If ST[I].Cat = 'T' Then NP.Kind := 't'
        Else NP.Kind := 'q';
        NP.Plural := ST[I].Plu;
      End;
      Inc(I);
    End;
    'U':
    Begin
      NP.DetTok := I;
      NP.Kind := 'n';
      NP.Text := ST[I].En;
      NP.Plural := ST[I].En <> '1';
      Inc(I);
    End;
  End;
  While I <= SN Do
  Begin
    If ST[I].Cat = 'A' Then
    Begin
      If NP.AdjN < 4 Then
      Begin
        Inc(NP.AdjN);
        NP.Adjs[NP.AdjN] := I;
      End;
      Inc(I);
    End
    Else If (ST[I].Cat = 'R') And (I < SN) And (ST[I + 1].Cat = 'A') And
        (ST[I].En = 'very') Then
    Begin
      If NP.AdjN < 4 Then
      Begin
        Inc(NP.AdjN);
        NP.Adjs[NP.AdjN] := I + 1;
        NP.AdjAdv[NP.AdjN] := 'very';
      End;
      Inc(I, 2);
    End
    Else
      Break;
  End;
  If (I <= SN) And (ST[I].Cat = 'N') Then
  Begin
    NP.Head := I;
    NP.Plural := ST[I].Plu;
    If (NP.DetTok > 0) And (ST[NP.DetTok].Cat = 'D') Then
      NP.Plural := ST[NP.DetTok].Plu;
    If (NP.Kind = 'n') And (NP.Text <> '1') Then NP.Plural := True;
    Inc(I);
    While I <= SN Do
    Begin
      If ST[I].Cat = 'A' Then
      Begin
        If NP.AdjN < 4 Then
        Begin
          Inc(NP.AdjN);
          NP.Adjs[NP.AdjN] := I;
        End;
        Inc(I);
      End
      Else If (ST[I].Cat = 'R') And (I < SN) And (ST[I + 1].Cat = 'A') And
          (ST[I].En = 'very') Then
      Begin
        If NP.AdjN < 4 Then
        Begin
          Inc(NP.AdjN);
          NP.Adjs[NP.AdjN] := I + 1;
          NP.AdjAdv[NP.AdjN] := 'very';
        End;
        Inc(I, 2);
      End
      Else
        Break;
    End;
    { le livre de Juan }
    If (I < SN) And (ST[I].Cat = 'E') And (ST[I].W = 'de') And (ST[I + 1].Cat =
        'Z') Then
    Begin
      NP.Poss := True;
      NP.Owner := '';
      Inc(I);
      While (I <= SN) And (ST[I].Cat = 'Z') Do
      Begin
        NP.Owner := Join(NP.Owner, ST[I].Orig);
        Inc(I);
      End;
    End;
  End
  Else If (I <= SN) And (ST[I].Cat = 'Z') Then
  Begin
    NP.Proper := True;
    While (I <= SN) And (ST[I].Cat = 'Z') Do
    Begin
      NP.Text := Join(NP.Text, SpProperEn(ST[I].W, ST[I].Orig));
      Inc(I);
    End;
  End
  Else If (NP.Kind = 'n') And (NP.AdjN = 0) Then
  Begin
  End
  Else If (NP.Kind = 'q') And (NP.AdjN = 0) Then
  Begin
  End
  Else
    Exit;
  NP.Found := True;
  NP.Used := I - Start;
  ParseNPEs := True;
End;

Function SpAdjText(Const NP: TSNP): String;
Var
  K: Integer;
  R, A: String;
Begin
  R := '';
  For K := 1 To NP.AdjN Do
  Begin
    A := ST[NP.Adjs[K]].En;
    If NP.AdjAdv[K] <> '' Then A := NP.AdjAdv[K] + ' ' + A;
    R := Join(R, A);
  End;
  SpAdjText := R;
End;

Function SpNounText(Const NP: TSNP): String;
Var
  N: String;
Begin
  N := ST[NP.Head].En;
  If NP.Plural And ((Not IsMassNoun(N)) Or (NP.Kind = 'n')) And (N <> 'people')
      Then N := EnPlural(N);
  SpNounText := N;
End;

Function SpPossEn(Const SubjPoss: String; Const W: String): String;
Begin
  If W = 'his' Then
  Begin
    If SubjPoss <> '' Then SpPossEn := SubjPoss Else SpPossEn := 'his';
  End
  Else SpPossEn := W;
End;

Function RenderNPEs(Const NP: TSNP; AsObject, Generic: Boolean; Const SubjPoss:
    String): String;
Var
  Det, Core, N: String;
Begin
  If NP.Pron Then
  Begin
    RenderNPEs := ST[NP.PronTok].En;
    If AsObject Then
    Begin
      N := ST[NP.PronTok].En;
      If N = 'I' Then N := 'me'
      Else If N = 'he' Then N := 'him'
      Else If N = 'she' Then N := 'her'
      Else If N = 'we' Then N := 'us'
      Else If N = 'they' Then N := 'them';
      RenderNPEs := N;
    End;
    Exit;
  End;
  If NP.Proper Then
  Begin
    RenderNPEs := NP.Text;
    Exit;
  End;
  If NP.Head = 0 Then
  Begin
    If NP.DetTok > 0 Then RenderNPEs := Join(ST[NP.DetTok].En, NP.Text) Else
        RenderNPEs := NP.Text;
    Exit;
  End;
  Core := Join(SpAdjText(NP), SpNounText(NP));
  If NP.Poss Then
  Begin
    RenderNPEs := NP.Owner + '''s ' + Core;
    Exit;
  End;
  Det := '';
  Case NP.Kind Of
    'd':
      If Generic Or IsLanguage(LowerAscii(ST[NP.Head].En)) Then Det := '' Else
          Det := 'the';
    'i':
    Begin
      If NP.Plural Then
        Det := ST[NP.DetTok].En
      Else
        Det := 'a';
    End;
    's': Det := SpPossEn(SubjPoss, ST[NP.DetTok].En);
    't': Det := ST[NP.DetTok].En;
    'n': Det := NP.Text;
    'q': Det := ST[NP.DetTok].En;
  End;
  If Det = 'a' Then Det := EnArticle(Core);
  If (NP.Head > 0) And (ST[NP.Head].Kind = 'g') And ((Det = 'the') Or (Det =
      'an')) Then Det := '';
  If (NP.Kind = 'q') And (NP.DetTok > 0) And (ST[NP.DetTok].En = 'all') And
      (NP.Head > 0) Then Det := 'all the';
  RenderNPEs := Join(Det, Core);
End;

Const
  PhrasesEs: Array[0..98] Of String = (
    'hola|Hello',
    'buenos dias|Good morning',
    'buenas tardes|Good afternoon',
    'buenas noches|Good night',
    'como estas|How are you',
    'como esta usted|How are you',
    'como estan ustedes|How are you',
    'que tal|How are you',
    'como te llamas|What is your name',
    'como se llama|What is your name',
    'como se llama usted|What is your name',
    'mucho gusto|Nice to meet you',
    'encantado|Nice to meet you',
    'de nada|You are welcome',
    'muchas gracias|Thank you very much',
    'gracias|Thank you',
    'lo siento|I am sorry',
    'perdon|Excuse me',
    'adios|Goodbye',
    'hasta luego|See you later',
    'hasta mañana|See you tomorrow',
    'hasta pronto|See you soon',
    'buena suerte|Good luck',
    'feliz cumpleaños|Happy birthday',
    'feliz navidad|Merry Christmas',
    'salud|Cheers',
    'no entiendo|I do not understand',
    'no se|I do not know',
    'te quiero|I love you',
    'te amo|I love you',
    'cuanto cuesta|How much does it cost',
    'que hora es|What time is it',
    'habla usted ingles|Do you speak English',
    'hablas ingles|Do you speak English',
    'de donde eres|Where are you from',
    'de donde es usted|Where are you from',
    'estoy bien|I am fine',
    'estoy perdido|I am lost',
    'no tengo tiempo|I do not have time',
    'donde esta el baño|Where is the bathroom',
    'pershendetje|Hello',
    'miredita|Hello',
    'mireserdhet|Welcome',
    'mire se vini|Welcome',
    'miremengjes|Good morning',
    'mirembrema|Good evening',
    'naten e mire|Good night',
    'mirupafshim|Goodbye',
    'faleminderit|Thank you',
    'shume faleminderit|Thank you very much',
    'ju lutem|Please',
    'me vjen keq|I am sorry',
    'me falni|Excuse me',
    'si je|How are you',
    'si jeni|How are you',
    'si quhesh|What is your name',
    'si quheni|What is your name',
    'si te quajne|What is your name',
    'une jam mire|I am fine',
    'jam mire|I am fine',
    'nuk e di|I do not know',
    'nuk kuptoj|I do not understand',
    'te dua|I love you',
    'flet anglisht|Do you speak English',
    'a flisni anglisht|Do you speak English',
    'ola|Hello',
    'bom dia|Good morning',
    'boa tarde|Good afternoon',
    'boa noite|Good night',
    'como esta|How are you',
    'como estas|How are you',
    'como vai|How are you',
    'tudo bem|How are you',
    'como se chama|What is your name',
    'como te chamas|What is your name',
    'qual e o seu nome|What is your name',
    'qual e o teu nome|What is your name',
    'prazer|Nice to meet you',
    'muito prazer|Nice to meet you',
    'de nada|You are welcome',
    'obrigado|Thank you',
    'obrigada|Thank you',
    'muito obrigado|Thank you very much',
    'muito obrigada|Thank you very much',
    'por favor|Please',
    'desculpe|Excuse me',
    'com licenca|Excuse me',
    'adeus|Goodbye',
    'ate logo|See you later',
    'ate amanha|See you tomorrow',
    'boa sorte|Good luck',
    'parabens|Congratulations',
    'nao entendo|I do not understand',
    'nao sei|I do not know',
    'eu te amo|I love you',
    'fala ingles|Do you speak English',
    'estou bem|I am fine',
    'de onde es|Where are you from',
    'de onde e|Where are you from'
  );

Function SpPhraseKey(A, B: Integer): String;
Var
  K: Integer;
  R: String;
Begin
  R := '';
  For K := A To B Do
  Begin
    If ST[K].Punct Then
    Begin
      SpPhraseKey := '#';
      Exit;
    End;
    R := Join(R, ST[K].W);
  End;
  SpPhraseKey := R;
End;

Function FindPhraseEs(Const Key: String): String;
Var
  N: Integer;
  Pair, K: String;
Begin
  FindPhraseEs := '';
  If Key = '#' Then Exit;
  For N := 0 To High(PhrasesEs) Do
  Begin
    Pair := PhrasesEs[N];
    K := CopyUntil(Pair, '|');
    If K = Key Then
    Begin
      FindPhraseEs := Pair;
      Exit;
    End;
  End;
End;


{ ------------------------------------------------------------------ }
{ ESPAGNOL -> ANGLAIS : proposition                                   }
{ ------------------------------------------------------------------ }

Function SpTenerIdiom(Const W: String): String;
Begin
  If W = 'hambre' Then SpTenerIdiom := 'hungry'
  Else If W = 'sed' Then SpTenerIdiom := 'thirsty'
  Else If W = 'frio' Then SpTenerIdiom := 'cold'
  Else If W = 'calor' Then SpTenerIdiom := 'hot'
  Else If W = 'sueño' Then SpTenerIdiom := 'sleepy'
  Else If W = 'miedo' Then SpTenerIdiom := 'afraid'
  Else If W = 'razon' Then SpTenerIdiom := 'right'
  Else If W = 'prisa' Then SpTenerIdiom := 'in a hurry'
  Else If W = 'fome' Then SpTenerIdiom := 'hungry'
  Else If W = 'sono' Then SpTenerIdiom := 'sleepy'
  Else If W = 'medo' Then SpTenerIdiom := 'afraid'
  Else If W = 'pressa' Then SpTenerIdiom := 'in a hurry'
  Else If W = 'verguenza' Then SpTenerIdiom := 'ashamed'
  Else If W = 'fame' Then SpTenerIdiom := 'hungry'
  Else If W = 'sete' Then SpTenerIdiom := 'thirsty'
  Else If W = 'freddo' Then SpTenerIdiom := 'cold'
  Else If W = 'caldo' Then SpTenerIdiom := 'hot'
  Else If W = 'sonno' Then SpTenerIdiom := 'sleepy'
  Else If W = 'paura' Then SpTenerIdiom := 'afraid'
  Else If W = 'ragione' Then SpTenerIdiom := 'right'
  Else If W = 'fretta' Then SpTenerIdiom := 'in a hurry'
  Else If W = 'vergogna' Then SpTenerIdiom := 'ashamed'
  Else SpTenerIdiom := '';
End;

Function SpHasFinite(A, B: Integer): Boolean;
Var
  I: Integer;
Begin
  SpHasFinite := False;
  For I := A To B Do
    If (ST[I].Cat = 'V') And (ST[I].Tns In ['p', 'i', 'm', 'f', 'c', 'M']) Then
        SpHasFinite := True;
End;

Function SpIsPlaceInstitution(Const En: String): Boolean;
Begin
  SpIsPlaceInstitution := InList(En, 'school university work church bed home');
End;

{ Elements de la proposition (groupes nominaux, prepositionnels, adjectifs,
  adverbes). }
Procedure ParseItemsEs(A, B: Integer; Const SubjPoss: String; Generic: Boolean);
Var
  I, J, K: Integer;
  NP, NP2: TSNP;
  Prep, Txt: String;

  Function NewItem(C: Char): Integer;
  Begin
    If SIN < 24 Then Inc(SIN);
    FillChar(SI[SIN], SizeOf(TSItem), 0);
    SI[SIN].Kind := C;
    SI[SIN].First := I;
    NewItem := SIN;
  End;
Begin
  SIN := 0;
  I := A;
  While I <= B Do
  Begin
    Case ST[I].Cat Of
      'D', 'S', 'T', 'Q', 'U', 'N', 'P', 'B', 'Z':
      Begin
        If ParseNPEs(I, NP) Then
        Begin
          J := NewItem('N');
          SI[J].Text := RenderNPEs(NP, True, Generic, SubjPoss);
          { coordination : pain et fromage }
          If (I + NP.Used + 1 <= B) And (ST[I + NP.Used].Cat = 'C') And (ST[I +
              NP.Used].En = 'and') And
             (ST[I + NP.Used + 1].Cat In ['D', 'N', 'U', 'P', 'B', 'S', 'T',
                 'Q', 'Z']) And ParseNPEs(I + NP.Used + 1, NP2) Then
          Begin
            SI[J].Text := SI[J].Text + ' and ' + RenderNPEs(NP2, True, Generic,
                SubjPoss);
            SI[J].Plural := True;
            SI[J].Pers := 6;
            SI[J].Last := I + NP.Used + NP2.Used;
            SI[J].Prep := SI[J].Text;
            Inc(I, NP.Used + 1 + NP2.Used);
            Continue;
          End;
          SI[J].Pron := NP.Pron;
          SI[J].Plural := NP.Plural;
          SI[J].Proper := NP.Proper;
          If NP.Pron Then
          Begin
            SI[J].Pers := ST[NP.PronTok].Slot;
            SI[J].Text := RenderNPEs(NP, False, Generic, SubjPoss);
            SI[J].Prep := RenderNPEs(NP, True, Generic, SubjPoss);
          End
          Else
          Begin
            If NP.Plural Then SI[J].Pers := 6 Else SI[J].Pers := 3;
            SI[J].Prep := SI[J].Text;
          End;
          If NP.Head > 0 Then
          Begin
            SI[J].HeadEn := ST[NP.Head].En;
            SI[J].Person := SpIsPersonEn(ST[NP.Head].En);
          End;
          SI[J].Last := I + NP.Used - 1;
          Inc(I, NP.Used);
        End
        Else
          Inc(I);
      End;
      'E':
      Begin
        Prep := ST[I].En;
        { a las ocho }
        If (ST[I].W = 'a') And (I + 2 <= B) And (ST[I + 1].Cat = 'D') And (ST[I
            + 2].Cat = 'U') Then
        Begin
          J := NewItem('E');
          SI[J].Text := 'at ' + ST[I + 2].En + ' o''clock';
          SI[J].Last := I + 2;
          Inc(I, 3);
        End
        Else If (I < B) And ParseNPEs(I + 1, NP) Then
        Begin
          If SourceSq And (NP.Kind = '0') And (Not NP.Plural) And (NP.Head > 0)
              And (Not NP.Proper) And
             (Not IsMassNoun(ST[NP.Head].En)) And (Not
                 IsLanguage(LowerAscii(ST[NP.Head].En))) And
             InList(Prep, 'in on at to from near under behind inside outside')
                 Then
            NP.Kind := 'd';
          Txt := RenderNPEs(NP, True, False, SubjPoss);
          { lieux : sans article pour certaines institutions }
          If (NP.Head > 0) And (NP.Kind = 'd') And (Not NP.Plural) And (NP.AdjN
              = 0) And (Not DePivot) And
             SpIsPlaceInstitution(ST[NP.Head].En) And InList(ST[I].W, 'a en ne')
                 Then
          Begin
            NP.Kind := '0';
            Txt := RenderNPEs(NP, True, False, SubjPoss);
          End;
          If InList(ST[I].W, 'en me') And (NP.Head > 0) And
              InList(ST[NP.Head].En, 'car bus train plane bike boat') Then
          Begin
            Prep := 'by';
            NP.Kind := '0';
            Txt := RenderNPEs(NP, True, False, SubjPoss);
          End;
          If InList(ST[I].W, 'en ne') And (NP.Head > 0) And (ST[NP.Head].En =
              'home') Then Prep := 'at';
          If InList(ST[I].W, 'en ne') And (NP.Head > 0) And
              InList(ST[NP.Head].En, 'table floor wall roof') Then Prep := 'on';
          If (ST[I].W = 'por') And (NP.Head > 0) And InList(ST[NP.Head].En,
              'morning afternoon night evening') Then
          Begin
            Prep := 'in';
            If ST[NP.Head].En = 'night' Then Prep := 'at';
          End;
          If (ST[I].W = 'para') And (NP.Pron Or (NP.Head > 0)) Then Prep :=
              'for';
          J := NewItem('E');
          SI[J].Text := Join(Prep, Txt);
          SI[J].Prep := ST[I].W;
          SI[J].Pron := NP.Pron;
          SI[J].Proper := NP.Proper;
          SI[J].Person := NP.Proper Or NP.Pron Or ((NP.Head > 0) And
              SpIsPersonEn(ST[NP.Head].En));
          SI[J].HeadEn := '';
          If NP.Head > 0 Then SI[J].HeadEn := ST[NP.Head].En;
          SI[J].Pers := 0;
          SI[J].Last := I + NP.Used;
          { texte de l'objet seul, pour "a" personnel }
          SI[J].Pers := I + 1;
          Inc(I, 1 + NP.Used);
        End
        Else If (I < B) And (ST[I].W = 'a') And (ST[I + 1].Cat = 'V') And (ST[I
            + 1].Tns = 'I') Then
        Begin
          { a + infinitif }
          J := NewItem('E');
          SI[J].Text := '';
          SI[J].Last := I;
          Inc(I);
        End
        Else
        Begin
          J := NewItem('E');
          SI[J].Text := Prep;
          SI[J].Last := I;
          Inc(I);
        End;
      End;
      'A':
      Begin
        J := NewItem('A');
        SI[J].Text := ST[I].En;
        SI[J].Last := I;
        { coordination : grand et fort }
        If (I + 2 <= B) And (ST[I + 1].Cat = 'C') And (ST[I + 1].En = 'and') And
            (ST[I + 2].Cat = 'A') Then
        Begin
          SI[J].Text := SI[J].Text + ' and ' + ST[I + 2].En;
          SI[J].Last := I + 2;
          Inc(I, 2);
        End;
        Inc(I);
      End;
      'R':
      Begin
        If (I < B) And (ST[I].En = 'very') And (ST[I + 1].Cat = 'A') Then
        Begin
          J := NewItem('A');
          SI[J].Text := 'very ' + ST[I + 1].En;
          SI[J].Last := I + 1;
          Inc(I, 2);
        End
        Else
        Begin
          J := NewItem('R');
          SI[J].Text := ST[I].En;
          SI[J].Last := I;
          Inc(I);
        End;
      End;
      'W':
      Begin
        J := NewItem('R');
        SI[J].Text := ST[I].En;
        SI[J].Last := I;
        Inc(I);
      End;
      'C':
      Begin
        J := NewItem('C');
        SI[J].Text := ST[I].En;
        SI[J].Last := I;
        Inc(I);
      End;
    Else
      Inc(I);
    End;
  End;
  K := 0;
End;

Function SpNom(Const S: String): String;
Begin
  If S = 'me' Then SpNom := 'I'
  Else If S = 'him' Then SpNom := 'he'
  Else If S = 'her' Then SpNom := 'she'
  Else If S = 'us' Then SpNom := 'we'
  Else If S = 'them' Then SpNom := 'they'
  Else SpNom := S;
End;

Function TranslateClauseEs(A, B: Integer; IsQ: Boolean; Final: Boolean): String;
Var
  I, J, K, Vf, V2, Next, SubjI: Integer;
  Wh, WhText, Res, Tail, ObjTxt, Pp, Adv, Freq, SubjText, Aux, Rest, LexBase,
      Post, AuxN, VerbEn, Pred, Inf, Inf2: String;
  LexTns: Char;
  G: TFVG;
  NP: TSNP;
  Neg, Please, Imperative, LetUs, WhSubject, DoQ, Impers, HasSubj, Never:
      Boolean;
  Sp, Slot: Integer;
  Clit, Experiencer, SubjPoss, Txt, ClitEn: String;
  Tns: Char;
  PreSubj: Integer;
Begin
  TranslateClauseEs := '';
  If A > B Then Exit;
  Tail := '';
  ObjTxt := '';
  Pp := '';
  Adv := '';
  Res := '';
  SubjText := '';
  Txt := '';
  Aux := '';
  Rest := '';
  LexBase := '';
  Post := '';
  AuxN := '';
  VerbEn := '';
  Inf := '';
  Inf2 := '';
  SubjPoss := '';
  Wh := '';
  WhText := '';
  Freq := '';
  Pred := '';
  Neg := False;
  Never := False;
  Imperative := False;
  LetUs := False;
  WhSubject := False;
  Impers := False;
  Please := False;
  Clit := '';
  Experiencer := '';

  { interjection ou conjonction en tete }
  If (ST[A].Cat = 'J') And (A < B) And (ST[A].En = 'please') Then
  Begin
    Please := True;
    ST[A].Cat := ' ';
    Inc(A);
    If (A <= B) And (ST[A].Cat = ',') Then Inc(A);
  End
  Else If (ST[A].Cat = 'J') And (A < B) Then
  Begin
    TranslateClauseEs := ST[A].En + ' ' + TranslateClauseEs(A + 1, B, IsQ,
        Final);
    Exit;
  End
  Else If (ST[A].Cat = 'J') And (A = B) Then
  Begin
    TranslateClauseEs := ST[A].En;
    Exit;
  End;
  If (ST[A].Cat = 'C') And (A < B) Then
  Begin
    TranslateClauseEs := ST[A].En + ' ' + TranslateClauseEs(A + 1, B, IsQ,
        Final);
    Exit;
  End;
  { "por favor" en fin de phrase }
  If (B > A) And (ST[B].Cat = 'J') And (ST[B].En = 'please') Then
  Begin
    Please := True;
    Dec(B);
    If (B > A) And (ST[B].Cat = ',') Then Dec(B);
  End;

  { mot interrogatif }
  If ST[A].Cat = 'W' Then
  Begin
    WhText := ST[A].En;
    Wh := ST[A].W;
    ST[A].Cat := ' ';
    If ((Wh = 'cuantos') Or (Wh = 'cuantas')) And (A < B) And ParseNPEs(A + 1,
        NP) And (NP.Head > 0) Then
    Begin
      NP.Plural := True;
      NP.Kind := '0';
      WhText := 'how many ' + SpNounText(NP);
      For K := A + 1 To A + NP.Used Do ST[K].Cat := ' ';
    End
    Else If (Wh = 'cual') And (A < B) And (ST[A + 1].Cat = 'N') Then
    Begin
      WhText := 'which ' + ST[A + 1].En;
      ST[A + 1].Cat := ' ';
    End
    Else If (Wh = 'que') And (A < B) And (ST[A + 1].Cat = 'N') Then
    Begin
      WhText := 'what ' + ST[A + 1].En;
      ST[A + 1].Cat := ' ';
    End;
    IsQ := True;
  End;

  { verbe fini : le premier verbe conjugue }
  Vf := 0;
  For I := A To B Do
    If (Vf = 0) And (ST[I].Cat = 'V') And (ST[I].Tns In ['p', 'i', 'm', 'f',
        'c', 'M']) Then Vf := I;
  If Vf = 0 Then
  Begin
    ParseItemsEs(A, B, '', False);
    Res := '';
    For I := 1 To SIN Do Res := Join(Res, SI[I].Text);
    TranslateClauseEs := Join(WhText, Res);
    Exit;
  End;
  Slot := ST[Vf].Slot;
  Inf := SpPlain(SpLex[ST[Vf].Idx].Es);
  Tns := ST[Vf].Tns;
  VerbEn := ST[Vf].En;
  ST[Vf].Cat := ' ';

  { negation et pronoms complements avant le verbe }
  For I := A To B Do
  Begin
    If (ST[I].Cat = 'G') And (I < Vf) Then
    Begin
      Neg := True;
      ST[I].Cat := ' ';
    End
    Else If (ST[I].Cat = 'O') And (I < Vf) Then
    Begin
      If InList(ST[I].W, 'me te se') Then Clit := Join(Clit, ':' + ST[I].W)
      Else If (ST[I].W = 'se') Or (ST[I].En = '') Then
        Clit := Join(Clit, ':' + ST[I].W)
      Else
        Clit := Join(Clit, ST[I].En);
      ST[I].Cat := ' ';
    End;
  End;

  { deuxieme verbe : participe, gerondif, infinitif }
  V2 := 0;
  For I := Vf + 1 To B Do
    If (V2 = 0) And (ST[I].Cat = 'V') And (ST[I].Tns In ['P', 'G', 'I']) Then V2
        := I;
  { pronoms enclitiques }
  If (V2 > 0) And (ST[V2].Cl <> '') And (ST[V2].Cl <> ' ') Then Clit :=
      Join(Clit, ST[V2].Cl);
  If (ST[Vf].Cl <> '') And (ST[Vf].Cl <> ' ') Then Clit := Join(Clit,
      ST[Vf].Cl);
  { fui, fue... : ser ou ir }
  If (SpPlain(SpLex[ST[Vf].Idx].Es) = 'ser') And (ST[Vf].Tns = 'i') And (Vf < B)
      And
     (ST[Vf + 1].Cat = 'E') And (ST[Vf + 1].W = 'a') Then
    For I := 1 To SpCount Do
      If SpPlain(SpLex[I].Es) = 'ir' Then
      Begin
        ST[Vf].Idx := I;
        ST[Vf].En := SpLex[I].En;
        Inf := 'ir';
        VerbEn := SpLex[I].En;
      End;

  FillChar(G, SizeOf(G), 0);
  G.Mode := 's';
  Case Tns Of
    'i', 'm': G.Tns := 'i';
    'f': G.Tns := 'f';
    'c': G.Tns := 'c';
  Else
    G.Tns := 'p';
  End;
  G.MainEn := VerbEn;
  If (ST[Vf].Kind = 'g') And (G.Tns = 'p') Then G.Rain := True;
  Next := Vf + 1;

  { tournures }
  If (Inf = 'haber') And ((ST[Vf].W = 'hay') Or ((V2 = 0) And (Slot = 3))) Then
  Begin
    G.Mode := 't';
    G.MainEn := 'be';
    Impers := True;
  End
  Else If (Inf = 'haber') And (V2 > 0) And (ST[V2].Tns = 'P') Then
  Begin
    G.Mode := 'c';
    G.MainEn := ST[V2].En;
    If Tns = 'p' Then G.Perf := True;
    ST[V2].Cat := ' ';
  End
  Else If (Inf = 'estar') And (V2 > 0) And (ST[V2].Tns = 'G') Then
  Begin
    G.Mode := 's';
    G.MainEn := ST[V2].En;
    If G.Tns = 'p' Then G.Rain := True;
    If ST[V2].En = 'rain' Then Impers := True;
    ST[V2].Cat := ' ';
  End
  Else If (Inf = 'ser') And (V2 > 0) And (ST[V2].Tns = 'P') Then
  Begin
    G.Mode := 'p';
    G.MainEn := ST[V2].En;
    ST[V2].Cat := ' ';
  End
  Else If (Inf = 'ir') And (V2 > 0) And (ST[V2].Tns = 'I') And (V2 >= 2) And
      (ST[V2 - 1].W = 'a') Then
  Begin
    If ST[Vf].W = 'vamos' Then
      LetUs := True
    Else
      G.Mode := 'g';
    G.MainEn := ST[V2].En;
    ST[V2 - 1].Cat := ' ';
    ST[V2].Cat := ' ';
  End
  Else If (Inf = 'acabar') And (V2 > 0) And (ST[V2].Tns = 'I') And (ST[V2 - 1].W
      = 'de') Then
  Begin
    G.Mode := 'j';
    G.MainEn := ST[V2].En;
    ST[V2 - 1].Cat := ' ';
    ST[V2].Cat := ' ';
  End
  Else If (Inf = 'tener') And (V2 > 0) And (ST[V2].Tns = 'I') And (V2 >= 2) And
      (ST[V2 - 1].W = 'que') Then
  Begin
    G.Mode := 'm';
    G.Modal := 'devoir';
    G.MainEn := ST[V2].En;
    ST[V2 - 1].Cat := ' ';
    ST[V2].Cat := ' ';
  End
  Else If InList(Inf, 'poder saber querer deber') And (V2 > 0) And (ST[V2].Tns =
      'I') And (ST[V2].Idx <> 0) Then
  Begin
    G.Mode := 'm';
    If (Inf = 'poder') Or (Inf = 'saber') Then G.Modal := 'pouvoir'
    Else If Inf = 'querer' Then G.Modal := 'vouloir'
    Else G.Modal := 'devoir';
    G.MainEn := ST[V2].En;
    ST[V2].Cat := ' ';
  End
  Else If (Inf = 'necesitar') And (V2 > 0) And (ST[V2].Tns = 'I') Then
  Begin
    G.MainEn := 'need to ' + ST[V2].En;
    ST[V2].Cat := ' ';
  End
  Else If (Inf = 'empezar') And (V2 > 0) And (ST[V2].Tns = 'I') And (ST[V2 -
      1].W = 'a') Then
  Begin
    G.MainEn := 'begin to ' + ST[V2].En;
    ST[V2 - 1].Cat := ' ';
    ST[V2].Cat := ' ';
  End
  Else If InList(Inf, 'gustar') Then
  Begin
    G.MainEn := 'like';
  End
  Else If Inf = 'llover' Then
  Begin
    G.MainEn := 'rain';
    If G.Tns = 'p' Then G.Rain := True;
    Impers := True;
  End
  Else If (Inf = 'ser') Or (Inf = 'estar') Then
  Begin
    G.Mode := 'b';
    G.MainEn := 'be';
  End
  Else If Inf = 'tener' Then
  Begin
    G.Mode := 'h';
    G.MainEn := 'have';
  End
  Else If (V2 > 0) And (ST[V2].Tns = 'I') And (Not InList(Inf, 'ir venir')) Then
  Begin
    { verbe + infinitif : aimer lire, preferer partir... }
    G.MainEn := VerbEn + ' to ' + ST[V2].En;
    ST[V2].Cat := ' ';
  End;
  { tener + idiome ; hacer + temps }
  If (G.Mode = 'h') And (Next <= B) Then
  Begin
    If SpTenerIdiom(ST[Next].W) <> '' Then
    Begin
      Pred := SpTenerIdiom(ST[Next].W);
      G.Mode := 'b';
      G.MainEn := 'be';
      ST[Next].Cat := ' ';
    End
    Else If (ST[Next].Cat = 'A') And (SpTenerIdiom(ST[Next].W) <> '') Then
    Begin
      Pred := SpTenerIdiom(ST[Next].W);
      G.Mode := 'b';
      G.MainEn := 'be';
      ST[Next].Cat := ' ';
    End
    Else If (ST[Next].Cat = 'U') And (Next < B) And ((ST[Next + 1].W = 'años')
        Or (ST[Next + 1].W = 'anos')) Then
    Begin
      Pred := ST[Next].En + ' years old';
      G.Mode := 'b';
      G.MainEn := 'be';
      ST[Next].Cat := ' ';
      ST[Next + 1].Cat := ' ';
    End;
  End;
  If (Inf = 'hacer') And (Next <= B) And (ST[Next].Cat = 'A') And
      InList(ST[Next].En, 'cold hot warm') Then
  Begin
    G.Mode := 'b';
    G.MainEn := 'be';
    Impers := True;
  End;
  { esta frio / es caliente sans sujet : temps qu'il fait }
  If ((Inf = 'estar') Or (Inf = 'ser')) And (Slot = 3) And (Next <= B) And
      (ST[Next].Cat = 'A') And
     InList(ST[Next].En, 'cold hot warm') And (Vf > 0) Then
  Begin
    HasSubj := False;
    For I := A To Vf - 1 Do
      If ST[I].Cat In ['P', 'N', 'D', 'Z', 'S', 'T'] Then HasSubj := True;
    If Not HasSubj Then Impers := True;
  End;
  If (Inf = 'ser') And (Next < B) And (ST[Next].Cat = 'D') And (ST[Next + 1].Cat
      = 'U') Then
  Begin
    Pred := ST[Next + 1].En + ' o''clock';
    Impers := True;
    ST[Next].Cat := ' ';
    ST[Next + 1].Cat := ' ';
  End;
  { me llamo X }
  If (Inf = 'llamar') And ((Pos(':', Clit) > 0) Or (ST[Vf].Kind = 'r')) Then
  Begin
    Tail := '';
    For I := Next To B Do
      If ST[I].Cat In ['Z', 'N'] Then Tail := Join(Tail, ST[I].Orig);
    If Slot = 1 Then Res := 'My name is'
    Else If Slot = 2 Then Res := 'Your name is'
    Else Res := 'His name is';
    TranslateClauseEs := Join(Res, Tail);
    Exit;
  End;

  { elements restants }
  ParseItemsEs(A, B, '', False);
  { sujet }
  SubjI := 0;
  PreSubj := 0;
  For I := 1 To SIN Do
    If (SubjI = 0) And (SI[I].Kind = 'N') And (SI[I].First < Vf) Then SubjI :=
        I;
  If (SubjI = 0) And ((Inf = 'gustar') Or ((Wh <> '') And (Wh <> 'quien')) Or
      (((Wh <> '') Or IsQ Or (SEnd = '?')) And (G.Mode = 'b'))) Then
    For I := 1 To SIN Do
      If (SubjI = 0) And (SI[I].Kind = 'N') And (SI[I].First > Vf) Then SubjI :=
          I;
  If (Inf = 'gustar') Then
  Begin
    { le sujet espagnol est ce qui plait : c'est l'objet en anglais }
    Experiencer := Clit;
    If Pos(':', Experiencer) > 0 Then
    Begin
      Experiencer := Copy(Experiencer, Pos(':', Experiencer) + 1, 10);
      If Experiencer = 'me' Then Experiencer := 'me' Else If Experiencer = 'te'
          Then Experiencer := 'you' Else Experiencer := 'him';
    End;
    If Experiencer = '' Then
      For I := 1 To SIN Do
        If (SI[I].Kind = 'E') And (SI[I].Prep = 'a') Then Experiencer :=
            SpNom(Copy(SI[I].Text, 4, 60));
    If Pos(' ', Experiencer) > 0 Then Experiencer := Copy(Experiencer, 1,
        Pos(' ', Experiencer) - 1);
  End;
  HasSubj := SubjI > 0;

  { possessif de la troisieme personne }
  SubjPoss := '';
  If HasSubj Then
  Begin
    If SI[SubjI].Pron Then
    Begin
      If SI[SubjI].Text = 'she' Then SubjPoss := 'her'
      Else If SI[SubjI].Text = 'they' Then SubjPoss := 'their'
      Else If SI[SubjI].Text = 'we' Then SubjPoss := 'our'
      Else If SI[SubjI].Text = 'I' Then SubjPoss := 'my'
      Else If SI[SubjI].Text = 'you' Then SubjPoss := 'your';
    End
    Else If SI[SubjI].Plural Then SubjPoss := 'their';
  End
  Else If Slot = 6 Then SubjPoss := 'their'
  Else If Slot = 4 Then SubjPoss := 'our';

  { sujet implicite }
  If (Inf = 'gustar') Then
  Begin
    SubjText := Experiencer;
    If SubjText = 'me' Then SubjText := 'I' Else If SubjText = 'him' Then
        SubjText := 'he'
    Else If SubjText = 'us' Then SubjText := 'we' Else If SubjText = 'them' Then
        SubjText := 'they';
    If SubjText = '' Then SubjText := 'I';
    Sp := 3;
    If SubjText = 'I' Then Sp := 1 Else If SubjText = 'you' Then Sp := 2
    Else If SubjText = 'we' Then Sp := 4 Else If SubjText = 'they' Then Sp := 6;
  End
  Else If HasSubj Then
  Begin
    SubjText := SI[SubjI].Text;
    Sp := SI[SubjI].Pers;
    If Sp < 1 Then Sp := 3;
    If SubjText = 'you' Then Sp := 2;
    If SubjText = 'I' Then Sp := 1;
  End
  Else If G.Mode = 't' Then
  Begin
    SubjText := 'there';
    Sp := 3;
  End
  Else If Impers Then
  Begin
    SubjText := 'it';
    Sp := 3;
  End
  Else
  Begin
    Sp := Slot;
    If Sp < 1 Then Sp := 3;
    If (ST[Vf].Kind = 'a') And ((SEnd = '?') Or IsQ) Then Sp := 2;
    Case Sp Of
      1: SubjText := 'I';
      2: SubjText := 'you';
      3: SubjText := 'he';
      4: SubjText := 'we';
      5: SubjText := 'you';
    Else
      SubjText := 'they';
    End;
    If (Inf = 'ser') And (Sp = 3) And (Tns = 'p') And (Pred <> '') Then SubjText
        := 'it';
    { es una casa : "it" quand l'attribut est une chose }
    If (Inf = 'ser') And (Sp = 3) And (Wh = '') And (Not IsQ) And (SubjText <>
        'it') Then
      For I := 1 To SIN Do
        If (SI[I].Kind = 'N') And (SI[I].First > Vf) And (Not SI[I].Pron) And
            (Not SI[I].Proper) And
           (SI[I].HeadEn <> '') And (Not SpIsPersonEn(SI[I].HeadEn)) And
               (SubjText = 'he') Then
          SubjText := 'it';
    If Sp = 5 Then Sp := 2;
  End;
  If (SubjText = 'it') Or (SubjText = 'he') Or (SubjText = 'she') Then Sp := 3;
  If (SubjText = 'they') Then Sp := 6;
  If (SubjText = 'we') Then Sp := 4;

  { impératif }
  If (Tns = 'M') Or ((Tns = 'p') And (Not HasSubj) And (Slot = 3) And (Not IsQ)
      And (Wh = '') And
     (SEnd <> '?') And (Vf = A) And (Not Final) And (SpLex[ST[Vf].Idx].Pos =
         'v') And
     (G.Mode = 's') And (Clit = '') And (V2 = 0) And (SIN > 0) And (Not Neg))
         Then
    Imperative := True;
  If Never Then Never := False;

  { negation par les mots negatifs }
  For I := A To B Do
    If (ST[I].Cat = 'g') Then
    Begin
      If ST[I].En = 'either' Then Adv := Join(Adv, 'either') Else ObjTxt :=
          Join(ObjTxt, ST[I].En);
      Neg := True;
    End;
  For I := A To B Do
    If (ST[I].Cat = 'R') And (ST[I].En = 'never') Then
    Begin
      Neg := False;
      Freq := Join(Freq, 'never');
    End;
  G.Neg := Neg;

  { complements }
  Pp := '';
  Txt := '';
  If (Inf = 'gustar') And HasSubj Then
    If ParseNPEs(SI[SubjI].First, NP) Then ObjTxt := RenderNPEs(NP, True, True,
        '');
  For I := 1 To SIN Do
  Begin
    If I = SubjI Then Continue;
    Case SI[I].Kind Of
      'N':
      Begin
        If (G.Mode = 'b') Or (G.Mode = 'p') Then
        Begin
          Txt := SI[I].Text;
          If (G.Mode = 'b') And (Not SI[I].Pron) And (Not SI[I].Proper) And
              (SI[I].HeadEn <> '') And
             (Not SI[I].Plural) And (Pos(' ', Txt) = 0) And (Not
                 IsMassNoun(SI[I].HeadEn)) And
             (Not IsLanguage(LowerAscii(SI[I].HeadEn))) Then
            Txt := EnArticle(Txt) + ' ' + Txt;
          If SI[I].Pron Then Txt := SI[I].Prep;
          Pred := Join(Pred, Txt);
        End
        Else
        Begin
          If SI[I].Pron Then ObjTxt := Join(ObjTxt, SI[I].Prep) Else ObjTxt :=
              Join(ObjTxt, SI[I].Text);
        End;
      End;
      'A':
        Pred := Join(Pred, SI[I].Text);
      'E':
      Begin
        If SI[I].Prep = 'a' Then
        Begin
          { a personnel : directement un objet }
          If SI[I].Person And (Not IsExistLikeMotion(VerbEn)) And (Not
              IsExistLikeMotion(G.MainEn)) And
             (Not InList(G.MainEn,
                 'give send write tell show teach bring say ask offer sell lend'
                 )) And (Inf <> 'gustar') Then
            ObjTxt := Join(ObjTxt, Copy(SI[I].Text, 4, 80))
          Else If Inf = 'gustar' Then
            Pp := Pp
          Else
            Pp := Join(Pp, SI[I].Text);
        End
        Else
          Pp := Join(Pp, SI[I].Text);
      End;
      'R':
        If IsFreqAdverb(SI[I].Text) Or InList(SI[I].Text,
            'often sometimes already also only still') Then
          Freq := Join(Freq, SI[I].Text)
        Else If (SI[I].Text = 'never') Then
          Freq := Freq
        Else
          Adv := Join(Adv, SI[I].Text);
      'C':
        Pp := Join(Pp, SI[I].Text);
    End;
  End;
  Sp := Sp;
  ClitEn := '';
  Txt := Clit;
  While Txt <> '' Do
  Begin
    K := Pos(' ', Txt);
    If K = 0 Then
    Begin
      Inf2 := Txt;
      Txt := '';
    End
    Else
    Begin
      Inf2 := Copy(Txt, 1, K - 1);
      Delete(Txt, 1, K);
    End;
    If (Inf2 <> '') And (Inf2[1] <> ':') Then ClitEn := Join(ClitEn, Inf2);
  End;
  If Inf = 'gustar' Then ClitEn := '';
  Tail := Join(ClitEn, Join(ObjTxt, Join(Pred, Join(Pp, Adv))));
  If Impers And (Inf = 'llover') Then SubjText := 'it';

  If Imperative Or LetUs Then
  Begin
    Res := '';
    If LetUs Then
    Begin
      If DePivot Then Res := 'let us' Else Res := 'let''s';
    End
    Else If Neg Then Res := 'don''t'
    Else If Please Then Res := 'please';
    TranslateClauseEs := Join(Res, Join(Freq, Join(G.MainEn, Tail)));
    Exit;
  End;

  { there are two books }
  If (G.Mode = 't') And (SubjText = 'there') Then
    For I := 1 To SIN Do
      If (SI[I].Kind = 'N') And (I <> SubjI) And SI[I].Plural And (Not
          SI[I].Pron) Then Sp := 6;
  BuildEn(G, Sp, Aux, Rest, LexBase, Post, LexTns);
  WhSubject := False;
  DoQ := IsQ Or (SEnd = '?');
  If (Wh = 'quien') And (Not HasSubj) And (Slot = 3) And (Inf <> 'ser') And (Inf
      <> 'estar') Then
  Begin
    WhSubject := True;
    SubjText := WhText;
    WhText := '';
  End;
  If WhSubject Then DoQ := False;
  If Final Then DoQ := False;
  Res := '';
  Wh := WhText;
  If DoQ Then
  Begin
    If Aux = '' Then
    Begin
      Aux := DoFormEn(Sp, LexTns);
      Rest := Join(LexBase, Post);
      Post := '';
      LexBase := '';
    End;
    If G.Neg Then AuxN := NegContract(Aux) Else AuxN := Aux;
    Res := Join(Wh, Join(AuxN, Join(SubjText, Join(Freq, Join(Rest, Join(Post,
        Tail))))));
  End
  Else
  Begin
    If G.Neg Then
    Begin
      If Aux = '' Then
      Begin
        Aux := DoFormEn(Sp, LexTns);
        Rest := Join(LexBase, Post);
        Post := '';
        LexBase := '';
      End;
      If Aux = 'am' Then AuxN := 'am not' Else AuxN := NegContract(Aux);
      Res := Join(Wh, Join(SubjText, Join(AuxN, Join(Freq, Join(Rest, Tail)))));
    End
    Else If Aux <> '' Then
      Res := Join(Wh, Join(SubjText, Join(Aux, Join(Freq, Join(Rest, Tail)))))
    Else
      Res := Join(Wh, Join(SubjText, Join(Freq, Join(FiniteEn(LexBase, LexTns,
          Sp), Join(Post, Tail)))));
  End;
  If Please Then Res := 'please ' + Res;
  TranslateClauseEs := Res;
  Inf2 := '';
  Slot := Slot + 0;
End;

Function TranslateTokensEs(A, B: Integer; IsQ: Boolean): String;
Var
  K: Integer;
  Key: String;
Begin
  If A > B Then
  Begin
    TranslateTokensEs := '';
    Exit;
  End;
  Key := FindPhraseEs(SpPhraseKey(A, B));
  If Key <> '' Then
  Begin
    If A > 1 Then Key := LowerFirst(Key);
    TranslateTokensEs := Key;
    Exit;
  End;
  For K := A + 1 To B Do
    If (ST[K].Cat = 'W') And SpHasFinite(A, K - 1) And SpHasFinite(K, B) And
        (ST[A].Cat <> 'W') And
       (ST[K - 1].Cat <> ',') Then
    Begin
      If DePivot Then
        TranslateTokensEs := TranslateTokensEs(A, K - 1, False) + ' ' +
            TranslateClauseEs(K, B, True, False)
      Else
        TranslateTokensEs := TranslateTokensEs(A, K - 1, False) + ' ' +
            TranslateClauseEs(K, B, False, True);
      Exit;
    End;
  For K := A + 1 To B - 1 Do
    If ST[K].Cat = ',' Then
      If ((K - A = 1) And (ST[A].Cat = 'J')) Or (SpHasFinite(A, K - 1) And
          SpHasFinite(K + 1, B)) Then
      Begin
        TranslateTokensEs := TranslateTokensEs(A, K - 1, False) + ', ' +
            TranslateTokensEs(K + 1, B, IsQ);
        Exit;
      End;
  For K := A + 1 To B - 1 Do
    If (ST[K].Cat = 'C') And SpHasFinite(A, K - 1) And SpHasFinite(K + 1, B)
        Then
    Begin
      If ST[K].En = 'but' Then
        TranslateTokensEs := TranslateTokensEs(A, K - 1, False) + ', ' +
            ST[K].En + ' ' + TranslateTokensEs(K + 1, B, IsQ)
      Else
        TranslateTokensEs := TranslateTokensEs(A, K - 1, False) + ' ' + ST[K].En
            + ' ' + TranslateTokensEs(K + 1, B, IsQ);
      Exit;
    End;
  TranslateTokensEs := TranslateClauseEs(A, B, IsQ, False);
End;

{ ------------------------------------------------------------------ }
{ ESPAGNOL -> ANGLAIS : phrase complete                               }
{ ------------------------------------------------------------------ }

Function EsTranslateSentence(Const Sentence: String): String;
Var
  Res: String;
  P: String;
Begin
  STokenize(Sentence);
  If SN = 0 Then
  Begin
    EsTranslateSentence := '';
    Exit;
  End;
  SpClassify;
  P := FindPhraseEs(SpPhraseKey(1, SN));
  If P <> '' Then
    Res := P
  Else
    Res := TranslateTokensEs(1, SN, SEnd = '?');
  Res := UpperFirst(FixEnglish(Res));
  If SEnd <> #0 Then Res := Res + SEnd;
  EsTranslateSentence := Res;
End;

{ ------------------------------------------------------------------ }
{ ALBANAIS -> ANGLAIS : lexique, morphologie et classification        }
{ ------------------------------------------------------------------ }
{ Les jetons produits sont ceux de l'espagnol (ST) : la meme analyse de   }
{ proposition sert ensuite pour les deux langues.                        }

Type
  TAl = Record
    Base, BaseOrig, Plu, PluOrig: String[24];
    En: String[28];
    Pos: String[3];
    Key: String[10];
    SpIdx: Integer;
  End;

  TAlIrr = Record
    Key: String[10];
    Pres, Aor: Array[1..6] Of String[10];
    Part: String[10];
  End;

Const
  AlIrrData: Array[0..18] Of String = (
    'jam|jam,je,eshte,jemi,jeni,jane|isha,ishe,ishte,ishim,ishit,ishin|qene',
    'kam|kam,ke,ka,kemi,keni,kane|kisha,kishe,kishte,kishim,kishit,kishin|pasur'
        ,
    'vij|vij,vjen,vjen,vijme,vini,vijne|erdha,erdhe,erdhi,erdhem,erdhet,erdhen'+
        '|ardhur',
    'bej|bej,ben,ben,bejme,beni,bejne|bera,bere,beri,beme,bete,bene|bere',
    'them|them,thua,thote,themi,thoni,thone|thashe,the,tha,thame,thate,thane|t'+
        'hene',
    'shoh|shoh,sheh,sheh,shohim,shihni,shohin|pashe,pe,pa,pame,pate,pane|pare',
    'di|di,di,di,dime,dini,dine|dija,dije,dinte,dinim,dinit,dinin|ditur',
    'dua|dua,do,do,duam,doni,duan|desha,deshe,deshi,deshem,deshet,deshen|dashur'
        ,
    'mund|mund,mund,mund,mundim,mundni,mundin|munda,munde,mundi,mundem,mundet,'+
        'munden|mundur',
    'duhet|duhem,duhesh,duhet,duhemi,duheni,duhen|duhej,duhej,duhej,duhej,duhe'+
        'j,duhej|duhur',
    'jap|jap,jep,jep,japim,jepni,japin|dhashe,dhe,dha,dhame,dhate,dhane|dhene',
    'marr|marr,merr,merr,marrim,merrni,marrin|mora,more,mori,morem,moret,moren'+
        '|marre',
    'dal|dal,del,del,dalim,dilni,dalin|dola,dole,doli,dolem,dolet,dolen|dale',
    'hyj|hyj,hyn,hyn,hyjme,hyni,hyjne|hyra,hyre,hyri,hyme,hyte,hyne|hyre',
    'ha|ha,ha,ha,hame,hani,hane|hengra,hengre,hengri,hengrem,hengret,hengren|n'+
        'grene',
    'pi|pi,pi,pi,pime,pini,pine|piva,pive,piu,pime,pite,pine|pire',
    'fle|fle,fle,fle,fleme,fleni,flene|fjeta,fjete,fjeti,fjetem,fjetet,fjeten|'+
        'fjetur',
    'flas|flas,flet,flet,flasim,flisni,flasin|fola,fole,foli,folem,folet,folen'+
        '|folur',
    'quhem|quhem,quhesh,quhet,quhemi,quheni,quhen|quhesha,quheshe,quhej,quhesh'+
        'im,quheshit,quheshin|quajtur'
  );

Var
  AlLex: Array[1..520] Of TAl;
  AlCount: Integer;
  AlIrr: Array[1..24] Of TAlIrr;
  AlIrrCount: Integer;
  AlLastDef: Boolean;

Function AlPlain(Const S: String): String;
Var
  I: Integer;
  R: String;
Begin
  R := '';
  I := 1;
  While I <= Length(S) Do
  Begin
    If (S[I] = #$C3) And (I < Length(S)) And (S[I + 1] = #$AB) Then
    Begin
      R := R + 'e';
      Inc(I, 2);
    End
    Else If (S[I] = #$C3) And (I < Length(S)) And (S[I + 1] = #$A7) Then
    Begin
      R := R + 'c';
      Inc(I, 2);
    End
    Else
    Begin
      R := R + S[I];
      Inc(I);
    End;
  End;
  AlPlain := R;
End;

Function AlSpIndex(Const Key, En: String): Integer;
Var
  I: Integer;
Begin
  For I := 1 To SpCount Do
    If (SpLex[I].Pos = 'v') And (SpPlain(SpLex[I].Es) = Key) Then
    Begin
      AlSpIndex := I;
      Exit;
    End;
  If SpCount < 1200 Then
  Begin
    Inc(SpCount);
    SpLex[SpCount].Es := Key;
    SpLex[SpCount].En := En;
    SpLex[SpCount].Pos := 'v';
    AlSpIndex := SpCount;
  End
  Else
    AlSpIndex := 1;
End;

Procedure LoadAlbanian;
Var
  I, K: Integer;
  Line, Item, W, Rest, P: String;
  Part: String;
Begin
  AlCount := 0;
  For I := 0 To High(AlData) Do
  Begin
    Line := AlData[I];
    While Line <> '' Do
    Begin
      Item := CopyUntil(Line, ';');
      If (Item <> '') And (AlCount < 520) Then
      Begin
        W := CopyUntil(Item, '=');
        Rest := CopyUntil(Item, ':');
        Inc(AlCount);
        FillChar(AlLex[AlCount], SizeOf(TAl), 0);
        P := CopyUntil(W, '/');
        AlLex[AlCount].BaseOrig := ReplaceChar(P, '_', ' ');
        AlLex[AlCount].Base := AlPlain(AlLex[AlCount].BaseOrig);
        If W <> '' Then
        Begin
          AlLex[AlCount].PluOrig := W;
          AlLex[AlCount].Plu := AlPlain(W);
        End;
        AlLex[AlCount].En := ReplaceChar(Rest, '_', ' ');
        AlLex[AlCount].Pos := CopyUntil(Item, ':');
        AlLex[AlCount].Key := Item;
        If Copy(AlLex[AlCount].Pos, 1, 1) = 'v' Then
        Begin
          If AlLex[AlCount].Key <> '' Then
            AlLex[AlCount].SpIdx := AlSpIndex(AlLex[AlCount].Key,
                AlLex[AlCount].En)
          Else
            AlLex[AlCount].SpIdx := AlSpIndex(AlLex[AlCount].Base,
                AlLex[AlCount].En);
        End;
      End;
    End;
  End;
  AlIrrCount := 0;
  For I := 0 To High(AlIrrData) Do
    If AlIrrCount < 24 Then
    Begin
      Line := AlIrrData[I];
      Inc(AlIrrCount);
      AlIrr[AlIrrCount].Key := CopyUntil(Line, '|');
      Part := CopyUntil(Line, '|');
      For K := 1 To 6 Do AlIrr[AlIrrCount].Pres[K] := CopyUntil(Part, ',');
      Part := CopyUntil(Line, '|');
      For K := 1 To 6 Do AlIrr[AlIrrCount].Aor[K] := CopyUntil(Part, ',');
      AlIrr[AlIrrCount].Part := Line;
    End;
End;

{ ---- formes des noms ---- }

Function AlDefSg(Const B: String; Fem: Boolean): String;
Var
  L: Integer;
Begin
  L := Length(B);
  If Fem Then
  Begin
    If EndsWith(B, 'ër') Then AlDefSg := Copy(B, 1, L - 3) + 'ra'
    Else If EndsWith(B, 'ë') Then AlDefSg := Copy(B, 1, L - 2) + 'a'
    Else If EndsWith(B, 'e') Then AlDefSg := Copy(B, 1, L - 1) + 'ja'
    Else If EndsWith(B, 'a') Then AlDefSg := B + 'ja'
    Else AlDefSg := B + 'a';
  End
  Else
  Begin
    If EndsWith(B, 'ër') Then AlDefSg := Copy(B, 1, L - 3) + 'ri'
    Else If EndsWith(B, 'ë') Then AlDefSg := Copy(B, 1, L - 2) + 'i'
    Else If (B[L] In ['k', 'g', 'h', 'q']) Or EndsWith(B, 'ri') Then AlDefSg :=
        B + 'u'
    Else AlDefSg := B + 'i';
  End;
End;

Function AlNounMatch(Const W: String; I: Integer; Var Plu: Boolean): Boolean;
Var
  Fem: Boolean;
  D, P, Po: String;
  L: Integer;
Begin
  AlNounMatch := False;
  Fem := AlLex[I].Pos = 'nf';
  D := AlPlain(AlDefSg(AlLex[I].BaseOrig, Fem));
  Plu := False;
  AlLastDef := False;
  If W = AlLex[I].Base Then
  Begin
    AlNounMatch := True;
    Exit;
  End;
  AlLastDef := True;
  If (W = D) Or (W = D + 'n') Then
  Begin
    AlNounMatch := True;
    Exit;
  End;
  If Fem And (Length(D) > 1) And ((W = Copy(D, 1, Length(D) - 1) + 'es') Or (W =
      Copy(D, 1, Length(D) - 1) + 'en')) Then
  Begin
    AlNounMatch := True;
    Exit;
  End;
  If (Not Fem) And (W = D + 't') Then
  Begin
    AlNounMatch := True;
    Exit;
  End;
  Plu := True;
  If AlLex[I].Plu <> '' Then P := AlLex[I].Plu Else P := AlLex[I].Base;
  If W = P Then
  Begin
    AlLastDef := False;
    AlNounMatch := True;
    Exit;
  End;
  If (W = P + 't') Or (W = P + 'te') Or (W = P + 'it') Or (W = P + 've') Then
  Begin
    AlNounMatch := True;
    Exit;
  End;
  L := Length(P);
  If (L > 1) And (P[L] In ['a', 'e', 'i', 'o', 'u']) And (W = Copy(P, 1, L - 1)
      + 've') Then
  Begin
    AlNounMatch := True;
    Exit;
  End;
  AlLastDef := False;
  Po := P;
  Plu := False;
End;

Function AlFindNoun(Const W: String; Var Plu: Boolean): Integer;
Var
  I: Integer;
  P: Boolean;
Begin
  AlFindNoun := 0;
  For I := 1 To AlCount Do
    If (Copy(AlLex[I].Pos, 1, 1) = 'n') And (AlLex[I].Pos <> 'na') Then
      If AlNounMatch(W, I, P) Then
      Begin
        Plu := P;
        AlFindNoun := I;
        Exit;
      End;
End;

Function AlFindAdj(Const W: String; Var Plu: Boolean): Integer;
Var
  I: Integer;
  P: String;
Begin
  AlFindAdj := 0;
  For I := 1 To AlCount Do
    If AlLex[I].Pos = 'a' Then
    Begin
      Plu := False;
      If (W = AlLex[I].Base) Or (W = AlLex[I].Base + 'e') Then
      Begin
        AlFindAdj := I;
        Exit;
      End;
      If AlLex[I].Plu <> '' Then P := AlLex[I].Plu Else P := AlLex[I].Base;
      Plu := True;
      If (W = P) Or (W = P + 'e') Then
      Begin
        AlFindAdj := I;
        Exit;
      End;
    End;
End;

Function AlFind(Const W: String; Const Kinds: String): Integer;
Var
  I: Integer;
Begin
  AlFind := 0;
  For I := 1 To AlCount Do
    If (AlLex[I].Base = W) And (Pos(AlLex[I].Pos, Kinds) > 0) Then
    Begin
      AlFind := I;
      Exit;
    End;
End;

{ ---- formes verbales ---- }

Function AlMatchVerb(Const W: String; Var Idx: Integer; Var Tns: Char; Var Slot:
    Integer;
                     Var Ambig: Boolean): Boolean;
Const
  OPres: Array[1..6] Of String[6] = ('oj', 'on', 'on', 'ojme', 'oni', 'ojne');
  OAor: Array[1..6] Of String[6] = ('ova', 'ove', 'oi', 'uam', 'uat', 'uan');
  OImp: Array[1..6] Of String[6] = ('oja', 'onje', 'onte', 'onim', 'onit',
      'onin');
  CPres: Array[1..6] Of String[6] = ('', '', '', 'im', 'ni', 'in');
  CAor: Array[1..6] Of String[6] = ('a', 'e', 'i', 'em', 'et', 'en');
  CImp: Array[1..6] Of String[6] = ('ja', 'je', 'te', 'nim', 'nit', 'nin');
  JPres: Array[1..6] Of String[6] = ('j', 'n', 'n', 'jme', 'ni', 'jne');
  JAor: Array[1..6] Of String[6] = ('va', 've', 'u', 'me', 'te', 'ne');
  JImp: Array[1..6] Of String[6] = ('nja', 'nje', 'nte', 'nim', 'nit', 'nin');
Var
  I, Q, K: Integer;
  Cls, B, Stem, Part: String;
  Found: Boolean;

  Function T(Const F: String; Tn: Char; Sl: Integer): Boolean;
  Begin
    T := False;
    If F = W Then
    Begin
      Idx := I;
      Tns := Tn;
      Slot := Sl;
      T := True;
    End;
  End;

Begin
  AlMatchVerb := True;
  Ambig := False;
  For I := 1 To AlCount Do
    If Copy(AlLex[I].Pos, 1, 1) = 'v' Then
    Begin
      Cls := AlLex[I].Pos;
      B := AlLex[I].Base;
      If Cls = 'vi' Then
      Begin
        For K := 1 To AlIrrCount Do
          If AlIrr[K].Key = B Then
          Begin
            For Q := 1 To 6 Do
              If T(AlIrr[K].Pres[Q], 'p', Q) Then
              Begin
                If (Q = 2) And (AlIrr[K].Pres[2] = AlIrr[K].Pres[3]) Then
                Begin
                  Slot := 3;
                  Ambig := True;
                End;
                If (Q = 1) And (AlIrr[K].Pres[1] = AlIrr[K].Pres[2]) Then
                Begin
                  Slot := 3;
                  Ambig := True;
                End;
                Exit;
              End;
            For Q := 1 To 6 Do
              If T(AlIrr[K].Aor[Q], 'i', Q) Then Exit;
            If T(AlIrr[K].Part, 'P', 0) Then Exit;
          End;
      End
      Else
      Begin
        If Cls = 'vo' Then Stem := Copy(B, 1, Length(B) - 2)
        Else If Cls = 'vj' Then Stem := Copy(B, 1, Length(B) - 1)
        Else Stem := B;
        For Q := 1 To 6 Do
        Begin
          If Cls = 'vo' Then Part := Stem + OPres[Q]
          Else If Cls = 'vj' Then Part := Stem + JPres[Q]
          Else Part := Stem + CPres[Q];
          If T(Part, 'p', Q) Then
          Begin
            If (Q = 2) And (Cls <> 'vc') Then
            Begin
              Slot := 3;
              Ambig := True;
            End;
            If (Cls = 'vc') And (Q In [1, 2, 3]) Then
            Begin
              Slot := 3;
              Ambig := True;
            End;
            Exit;
          End;
        End;
        For Q := 1 To 6 Do
        Begin
          If Cls = 'vo' Then Part := Stem + OAor[Q]
          Else If Cls = 'vj' Then Part := Stem + JAor[Q]
          Else Part := Stem + CAor[Q];
          If T(Part, 'i', Q) Then Exit;
        End;
        For Q := 1 To 6 Do
        Begin
          If Cls = 'vo' Then Part := Stem + OImp[Q]
          Else If Cls = 'vj' Then Part := Stem + JImp[Q]
          Else Part := Stem + CImp[Q];
          If T(Part, 'm', Q) Then Exit;
        End;
        If Cls = 'vo' Then Part := Stem + 'uar'
        Else If Cls = 'vj' Then
        Begin
          If EndsWith(Stem, 'ua') Then Part := Stem + 'r' Else Part := Stem +
              're';
        End
        Else Part := Stem + 'ur';
        If T(Part, 'P', 0) Then Exit;
      End;
    End;
  AlMatchVerb := False;
  Found := False;
End;

{ ------------------------------------------------------------------ }
{ ALBANAIS -> ANGLAIS : jetons et etiquetage                          }
{ ------------------------------------------------------------------ }

Procedure ATokenize(Const S0: String);
Var
  S, Word1, Low, Plain: String;
  I, L: Integer;
  C: Char;
  Cap: Boolean;
Begin
  SN := 0;
  SEnd := #0;
  S := '';
  I := 1;
  While I <= Length(S0) Do
  Begin
    If (S0[I] = #$C2) And (I < Length(S0)) And ((S0[I + 1] = #$AB) Or (S0[I + 1]
        = #$BB) Or (S0[I + 1] = #$A0)) Then
    Begin
      S := S + ' ';
      Inc(I, 2);
    End
    Else If (S0[I] = #$E2) And (I + 2 <= Length(S0)) And (S0[I + 1] = #$80) And
            ((S0[I + 2] = #$99) Or (S0[I + 2] = #$98)) Then
    Begin
      S := S + ' ';
      Inc(I, 3);
    End
    Else
    Begin
      S := S + S0[I];
      Inc(I);
    End;
  End;
  L := Length(S);
  I := 1;
  While I <= L Do
  Begin
    C := S[I];
    If FrWordByte(C) Then
    Begin
      Word1 := '';
      While (I <= L) And FrWordByte(S[I]) Do
      Begin
        Word1 := Word1 + S[I];
        Inc(I);
      End;
      Cap := (Word1 <> '') And (((Word1[1] >= 'A') And (Word1[1] <= 'Z')) Or
             ((Word1[1] = #$C3) And (Length(Word1) > 1) And (Word1[2] >= #$80)
                 And (Word1[2] <= #$9E)));
      Low := FrLower(Word1);
      Plain := AlPlain(Low);
      SPush(Plain, Word1, Cap, False, Plain <> Low);
    End
    Else
    Begin
      If (C = '.') Or (C = '?') Or (C = '!') Then
      Begin
        If SEnd = #0 Then SEnd := C;
        If C = '?' Then SEnd := '?';
      End
      Else If (C = ',') Or (C = ';') Or (C = ':') Then
      Begin
        SPush(',', ',', False, True, False);
        ST[SN].Cat := ',';
      End;
      Inc(I);
    End;
  End;
End;

Procedure AMergeMultiword;
Var
  I, K, Len, L: Integer;
  Key: String;
  Bad, Found: Boolean;
Begin
  I := 1;
  While I < SN Do
  Begin
    Found := False;
    For Len := 4 Downto 2 Do
      If (Not Found) And (I + Len - 1 <= SN) Then
      Begin
        Key := '';
        Bad := False;
        For K := I To I + Len - 1 Do
        Begin
          If ST[K].Punct Then Bad := True;
          Key := Join(Key, ST[K].W);
        End;
        If Not Bad Then
        Begin
          L := AlFind(Key, 'rpcjwnm nf');
          If (L = 0) Then L := AlFind(Key, 'nf');
          If (L = 0) Then L := AlFind(Key, 'nm');
          If (L > 0) Then
          Begin
            Found := True;
            ST[I].W := Key;
            ST[I].Orig := Key;
            For K := I + 1 To SN - Len + 1 Do
              ST[K] := ST[K + Len - 1];
            Dec(SN, Len - 1);
          End;
        End;
      End;
    Inc(I);
  End;
End;

Function AlNumberEn(Const W: String): String;
Begin
  AlNumberEn := '';
  If W = 'dy' Then AlNumberEn := 'two'
  Else If W = 'tre' Then AlNumberEn := 'three'
  Else If W = 'kater' Then AlNumberEn := 'four'
  Else If W = 'pese' Then AlNumberEn := 'five'
  Else If W = 'gjashte' Then AlNumberEn := 'six'
  Else If W = 'shtate' Then AlNumberEn := 'seven'
  Else If W = 'tete' Then AlNumberEn := 'eight'
  Else If W = 'nente' Then AlNumberEn := 'nine'
  Else If W = 'dhjete' Then AlNumberEn := 'ten'
  Else If W = 'njezet' Then AlNumberEn := 'twenty'
  Else If W = 'tridhjete' Then AlNumberEn := 'thirty'
  Else If W = 'dyzet' Then AlNumberEn := 'forty'
  Else If W = 'pesedhjete' Then AlNumberEn := 'fifty'
  Else If W = 'njeqind' Then AlNumberEn := 'one hundred';
End;

Function AlNextVerbal(I: Integer): Boolean;
Var
  Idx, Sl: Integer;
  T: Char;
  Amb: Boolean;
Begin
  AlNextVerbal := False;
  If I > SN Then Exit;
  If ST[I].Punct Then Exit;
  If InList(ST[I].W, 'nuk s mos') Then
  Begin
    AlNextVerbal := True;
    Exit;
  End;
  AlNextVerbal := AlMatchVerb(ST[I].W, Idx, T, Sl, Amb) And (T In ['p', 'i',
      'm']);
End;

Function AlNextNounish(I: Integer): Boolean;
Var
  Plu: Boolean;
Begin
  AlNextNounish := False;
  If I > SN Then Exit;
  If ST[I].Punct Then Exit;
  AlNextNounish := (AlFindNoun(ST[I].W, Plu) > 0) Or (AlFindAdj(ST[I].W, Plu) >
      0) Or
                   InList(ST[I].W, 'nje ky kjo keta keto dy tre');
End;

Procedure AlSetWh(I: Integer);
Begin
  If ST[I].W = 'ku' Then ST[I].W := 'donde'
  Else If ST[I].W = 'kur' Then ST[I].W := 'cuando'
  Else If ST[I].W = 'pse' Then ST[I].W := 'por que'
  Else If ST[I].W = 'si' Then ST[I].W := 'como'
  Else If ST[I].W = 'cfare' Then ST[I].W := 'que'
  Else If ST[I].W = 'kush' Then ST[I].W := 'quien'
  Else If ST[I].W = 'sa' Then ST[I].W := 'cuantos'
  Else If (ST[I].W = 'cili') Or (ST[I].W = 'cila') Then ST[I].W := 'cual';
End;

Procedure AlPickCat(I: Integer);
Var
  W: String;
  Prev: Char;
  Idx, Sl, K: Integer;
  T: Char;
  Amb, Plu: Boolean;
  LN, LA: Integer;
  IsVerb, IsNoun, IsAdj: Boolean;
Begin
  W := ST[I].W;
  If I > 1 Then Prev := ST[I - 1].Cat Else Prev := '.';
  If ST[I].Punct Then
  Begin
    ST[I].Cat := ',';
    Exit;
  End;
  If IsDigitsOnly(W) Then
  Begin
    ST[I].Cat := 'U';
    ST[I].En := W;
    Exit;
  End;
  If AlNumberEn(W) <> '' Then
  Begin
    ST[I].Cat := 'U';
    ST[I].En := AlNumberEn(W);
    Exit;
  End;
  If (W = 'nuk') Or (W = 's') Or (W = 'mos') Then
  Begin
    ST[I].Cat := 'G';
    Exit;
  End;
  If W = 'asnje' Then
  Begin
    ST[I].Cat := 'Q';
    ST[I].En := 'no';
    Exit;
  End;
  If (W = 'kurre') And ((I > 1) Or True) Then
  Begin
    ST[I].Cat := 'R';
    ST[I].En := 'never';
    Exit;
  End;
  If InList(W, 'mua ty atij asaj neve juve atyre') Then
  Begin
    ST[I].Cat := 'B';
    If W = 'mua' Then ST[I].En := 'me'
    Else If (W = 'ty') Or (W = 'juve') Then ST[I].En := 'you'
    Else If W = 'atij' Then ST[I].En := 'him'
    Else If W = 'asaj' Then ST[I].En := 'her'
    Else If W = 'neve' Then ST[I].En := 'us'
    Else ST[I].En := 'them';
    Exit;
  End;
  If ((W = 'mire') Or (W = 'keq')) And (Prev <> 'L') And (Prev <> 'N') And (Prev
      <> 'A') Then
  Begin
    ST[I].Cat := 'R';
    If W = 'mire' Then ST[I].En := 'well' Else ST[I].En := 'badly';
    Exit;
  End;
  If (W = 'para') And ((Prev = 'V') Or (I = SN)) Then
  Begin
    ST[I].Cat := 'N';
    ST[I].En := 'money';
    Exit;
  End;
  { pronoms sujets }
  If InList(W, 'une ti ajo ata ato') Or ((W = 'ai') And Not AlNextNounish(I +
      1)) Or
     ((W = 'ne') And (Not ST[I].Acc) And (Not AlNextNounish(I + 1))) Or
     ((W = 'ju') And Not AlNextVerbal(I + 1)) Then
  Begin
    ST[I].Cat := 'P';
    If W = 'une' Then Begin ST[I].En := 'I'; ST[I].Slot := 1; End
    Else If W = 'ti' Then Begin ST[I].En := 'you'; ST[I].Slot := 2; End
    Else If W = 'ai' Then Begin ST[I].En := 'he'; ST[I].Slot := 3; End
    Else If W = 'ajo' Then Begin ST[I].En := 'she'; ST[I].Slot := 3; End
    Else If W = 'ne' Then Begin ST[I].En := 'we'; ST[I].Slot := 4; End
    Else If W = 'ju' Then Begin ST[I].En := 'you'; ST[I].Slot := 5; End
    Else Begin ST[I].En := 'they'; ST[I].Slot := 6; End;
    Exit;
  End;
  { marqueur du futur et du subjonctif }
  If (W = 'do') And (I < SN) And (ST[I + 1].W = 'te') Then
  Begin
    ST[I].Cat := 'f';
    Exit;
  End;
  If (W = 'te') And (Prev <> 'E') And ((Prev = 'f') Or ((I > 1) And (ST[I -
      1].Cat = 'V')) Or
     ((Prev <> 'V') And AlNextVerbal(I + 1) And (Not ST[I].Acc) And False)) And
         AlNextVerbal(I + 1) Then
  Begin
    ST[I].Cat := 'k';
    Exit;
  End;
  If (W = 'po') And AlNextVerbal(I + 1) Then
  Begin
    ST[I].Cat := 'x';
    Exit;
  End;
  If (W = 'a') And (I = 1) And (SEnd = '?') Then
  Begin
    ST[I].Cat := ' ';
    Exit;
  End;
  { pronoms complements }
  If ((W = 'me') And ST[I].Acc) Or ((W = 'me') And AlNextVerbal(I + 1) And ((I =
      1) Or (Prev In ['P', 'G']))) Then
  Begin
    ST[I].Cat := 'O';
    ST[I].En := 'me';
    Exit;
  End;
  If ((W = 'te') And ST[I].Acc And AlNextVerbal(I + 1)) Or
     ((W = 'te') And AlNextVerbal(I + 1) And ((I = 1) Or (Prev In ['P', 'G'])))
         Then
  Begin
    ST[I].Cat := 'O';
    ST[I].En := 'you';
    Exit;
  End;
  If (W = 'na') And AlNextVerbal(I + 1) Then
  Begin
    ST[I].Cat := 'O';
    ST[I].En := 'us';
    Exit;
  End;
  If (W = 'ju') And AlNextVerbal(I + 1) Then
  Begin
    ST[I].Cat := 'O';
    ST[I].En := 'you';
    Exit;
  End;
  If (W = 'e') And AlNextVerbal(I + 1) Then
  Begin
    ST[I].Cat := 'O';
    ST[I].En := 'it';
    Exit;
  End;
  If (W = 'i') And AlNextVerbal(I + 1) Then
  Begin
    ST[I].Cat := 'O';
    ST[I].En := 'them';
    Exit;
  End;
  { particules de liaison }
  If InList(W, 'i e te se') Then
  Begin
    ST[I].Cat := 'L';
    Exit;
  End;
  { determinants }
  If W = 'nje' Then
  Begin
    ST[I].Cat := 'D';
    ST[I].Kind := 'i';
    ST[I].En := 'a';
    Exit;
  End;
  If InList(W, 'ky kjo keta keto') Then
  Begin
    ST[I].Cat := 'T';
    If (W = 'keta') Or (W = 'keto') Then
    Begin
      ST[I].En := 'these';
      ST[I].Plu := True;
    End
    Else ST[I].En := 'this';
    Exit;
  End;
  If InList(W, 'im ime yt jote tij saj tyre yne jone juaj') Then
  Begin
    ST[I].Cat := 'S';
    ST[I].Kind := 'p';
    If (W = 'im') Or (W = 'ime') Then ST[I].En := 'my'
    Else If (W = 'yt') Or (W = 'jote') Or (W = 'juaj') Then ST[I].En := 'your'
    Else If W = 'tij' Then ST[I].En := 'his'
    Else If W = 'saj' Then ST[I].En := 'her'
    Else If W = 'tyre' Then ST[I].En := 'their'
    Else ST[I].En := 'our';
    Exit;
  End;
  If (W = 'shume') And (I < SN) And (AlFindNoun(ST[I + 1].W, Plu) > 0) Then
  Begin
    ST[I].Cat := 'Q';
    ST[I].En := 'a lot of';
    Exit;
  End;
  If (W = 'cdo') Or (W = 'disa') Then
  Begin
    ST[I].Cat := 'Q';
    If W = 'cdo' Then ST[I].En := 'every' Else Begin ST[I].En := 'some';
        ST[I].Plu := True; End;
    Exit;
  End;
  { mots du lexique }
  Idx := AlFind(W, 'w');
  If (Idx > 0) And ((I = 1) Or (Prev = ',') Or (W <> 'kur')) Then
  Begin
    ST[I].Cat := 'W';
    ST[I].En := AlLex[Idx].En;
    AlSetWh(I);
    Exit;
  End;
  Idx := AlFind(W, 'p');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'E';
    ST[I].En := AlLex[Idx].En;
    Exit;
  End;
  Idx := AlFind(W, 'c');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'C';
    ST[I].En := AlLex[Idx].En;
    Exit;
  End;
  Idx := AlFind(W, 'j');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'J';
    ST[I].En := AlLex[Idx].En;
    Exit;
  End;
  Idx := AlFind(W, 'r');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'R';
    ST[I].En := AlLex[Idx].En;
    Exit;
  End;
  LN := AlFindNoun(W, Plu);
  IsNoun := LN > 0;
  If IsNoun Then ST[I].Plu := Plu;
  LA := AlFindAdj(W, Plu);
  IsAdj := LA > 0;
  IsVerb := AlMatchVerb(W, Idx, T, Sl, Amb);
  If IsVerb And (IsNoun Or IsAdj) And (Prev In ['D', 'S', 'T', 'Q', 'U', 'L'])
      Then IsVerb := False;
  If IsVerb Then
  Begin
    ST[I].Cat := 'V';
    ST[I].Idx := AlLex[Idx].SpIdx;
    ST[I].Tns := T;
    ST[I].Slot := Sl;
    ST[I].En := AlLex[Idx].En;
    If Amb Then ST[I].Kind := 'a';
    If AlLex[Idx].Base = 'quhem' Then ST[I].Kind := 'r';
    Exit;
  End;
  If IsNoun And IsAdj Then
  Begin
    If Prev In ['D', 'S', 'T', 'Q', 'U'] Then IsAdj := False Else IsNoun :=
        False;
  End;
  If IsNoun Then
  Begin
    AlFindNoun(W, Plu);
    ST[I].Cat := 'N';
    ST[I].Plu := Plu;
    ST[I].En := AlLex[LN].En;
    If AlLastDef Then ST[I].Cl := 'D';
    Exit;
  End;
  If IsAdj Then
  Begin
    AlFindAdj(W, Plu);
    ST[I].Cat := 'A';
    ST[I].Plu := Plu;
    ST[I].En := AlLex[LA].En;
    Exit;
  End;
  If (W = 'ne') And (ST[I].Acc Or AlNextNounish(I + 1)) Then
  Begin
    ST[I].Cat := 'E';
    ST[I].En := 'in';
    Exit;
  End;
  If (W = 'ne') Then
  Begin
    ST[I].Cat := 'P';
    ST[I].En := 'we';
    ST[I].Slot := 4;
    Exit;
  End;
  If (W = 'me') Then
  Begin
    ST[I].Cat := 'E';
    ST[I].En := 'with';
    Exit;
  End;
  K := 0;
  ST[I].Cat := 'Z';
End;

Function AlProperEn(Const Orig: String): String;
Var
  L: String;
Begin
  L := AlPlain(FrLower(Orig));
  If L = 'shqiperia' Then AlProperEn := 'Albania'
  Else If L = 'franca' Then AlProperEn := 'France'
  Else If L = 'gjermania' Then AlProperEn := 'Germany'
  Else If L = 'italia' Then AlProperEn := 'Italy'
  Else If L = 'anglia' Then AlProperEn := 'England'
  Else If L = 'spanja' Then AlProperEn := 'Spain'
  Else If L = 'greqia' Then AlProperEn := 'Greece'
  Else If L = 'turqia' Then AlProperEn := 'Turkey'
  Else If L = 'kosova' Then AlProperEn := 'Kosovo'
  Else If L = 'maqedonia' Then AlProperEn := 'Macedonia'
  Else If L = 'zvicra' Then AlProperEn := 'Switzerland'
  Else If L = 'rusia' Then AlProperEn := 'Russia'
  Else If L = 'amerika' Then AlProperEn := 'America'
  Else If L = 'parisi' Then AlProperEn := 'Paris'
  Else If L = 'londra' Then AlProperEn := 'London'
  Else If L = 'roma' Then AlProperEn := 'Rome'
  Else If L = 'tirane' Then AlProperEn := 'Tirana'
  Else If L = 'itali' Then AlProperEn := 'Italy'
  Else If L = 'shqiperi' Then AlProperEn := 'Albania'
  Else If L = 'france' Then AlProperEn := 'France'
  Else If L = 'gjermani' Then AlProperEn := 'Germany'
  Else If L = 'angli' Then AlProperEn := 'England'
  Else If L = 'spanje' Then AlProperEn := 'Spain'
  Else If L = 'greqi' Then AlProperEn := 'Greece'
  Else If L = 'turqi' Then AlProperEn := 'Turkey'
  Else If L = 'rusi' Then AlProperEn := 'Russia'
  Else If L = 'zvicer' Then AlProperEn := 'Switzerland'
  Else If L = 'prishtine' Then AlProperEn := 'Pristina'
  Else If L = 'durres' Then AlProperEn := 'Durres'
  Else AlProperEn := Orig;
End;

Procedure ARemoveConsumed;
Var
  I, J: Integer;
Begin
  J := 0;
  For I := 1 To SN Do
    If Not (ST[I].Cat In [' ', 'L', 'k', 'f', 'x']) Then
    Begin
      Inc(J);
      ST[J] := ST[I];
    End;
  SN := J;
End;

Procedure AlClassify;
Var
  I, J, K, M: Integer;
  Owner: String;
Begin
  AMergeMultiword;
  For I := 1 To SN Do
    AlPickCat(I);
  { lissage : 'do te V' -> futur, 'dua/mund/duhet te V' -> infinitif }
  For I := 1 To SN Do
  Begin
    If (ST[I].Cat = 'f') And (I + 2 <= SN) And (ST[I + 1].Cat = 'k') And (ST[I +
        2].Cat = 'V') Then
      ST[I + 2].Tns := 'f'
    Else If (ST[I].Cat = 'V') And (I + 2 <= SN) And (ST[I + 1].Cat = 'k') And
        (ST[I + 2].Cat = 'V') Then
    Begin
      ST[I + 2].Tns := 'I';
      ST[I].Slot := ST[I + 2].Slot;
      ST[I].Kind := ' ';
    End;
    If (ST[I].Cat = 'x') And (I < SN) And (ST[I + 1].Cat = 'V') Then ST[I +
        1].Kind := 'g';
  End;
  { mots reconnus comme noms propres : majuscule hors de debut de phrase }
  For I := 1 To SN Do
    If (ST[I].Cat = 'Z') Then ST[I].Orig := AlProperEn(ST[I].Orig);
  { possessifs places apres le nom : libri im -> my libri }
  For I := 2 To SN Do
    If (ST[I].Cat = 'S') And (ST[I].Kind = 'p') Then
    Begin
      J := I - 1;
      While (J > 1) And (ST[J].Cat In ['A', 'L']) Do
        Dec(J);
      If ST[J].Cat = 'N' Then
      Begin
        { deplace le possessif devant le nom }
        K := I;
        M := 0;
        ST[0] := ST[K];
        While K > J Do
        Begin
          ST[K] := ST[K - 1];
          Dec(K);
        End;
        ST[J] := ST[0];
        ST[J].Kind := ' ';
        ST[J].Plu := ST[J + 1].Plu;
      End;
    End;
  { genitif : libri i nxenesit -> nxenesit's libri }
  I := 1;
  While I + 2 <= SN Do
  Begin
    If (ST[I].Cat = 'N') And (ST[I + 1].Cat = 'L') And (ST[I + 2].Cat = 'N')
        Then
    Begin
      Owner := ST[I + 2].En;
      If ST[I + 2].Plu Then Owner := EnPlural(Owner) + '''' Else Owner := Owner
          + '''s';
      If ST[I + 2].Cl = 'D' Then Owner := 'the ' + Owner;
      ST[I].En := Owner + ' ' + ST[I].En;
      ST[I].Kind := 'g';
      ST[I + 1].Cat := ' ';
      ST[I + 2].Cat := ' ';
      Inc(I, 3);
    End
    Else If (ST[I].Cat = 'N') And (ST[I + 1].Cat = 'L') And (ST[I + 2].Cat =
        'Z') Then
    Begin
      ST[I].En := ST[I + 2].Orig + '''s ' + ST[I].En;
      ST[I].Kind := 'g';
      ST[I + 1].Cat := ' ';
      ST[I + 2].Cat := ' ';
      Inc(I, 3);
    End
    Else
      Inc(I);
  End;
  { impératif : verbe a la 2e personne du pluriel en tete, ou apres "mos" }
  If (SN >= 1) And (ST[1].Cat = 'V') And (ST[1].Slot = 5) And (ST[1].Tns = 'p')
      Then ST[1].Tns := 'M';
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'G') And (ST[I].W = 'mos') And (ST[I + 1].Cat = 'V') And
        (ST[I + 1].Tns = 'p') Then
    Begin
      ST[I + 1].Tns := 'M';
      ST[I + 1].Slot := 5;
    End;
  { kam + participe : passe compose ; ka sans sujet : il y a }
  For I := 1 To SN Do
    If (ST[I].Cat = 'V') And (SpPlain(SpLex[ST[I].Idx].Es) = 'tener') Then
    Begin
      If (I < SN) And (ST[I + 1].Cat = 'V') And (ST[I + 1].Tns = 'P') Then
      Begin
        ST[I].Idx := AlSpIndex('haber', 'have');
        ST[I].En := 'have';
      End
      Else If (ST[I].Slot = 3) And (I = 1) And (I < SN) And (ST[I + 1].Cat In
          ['D', 'N', 'U', 'Q']) Then
      Begin
        ST[I].Idx := AlSpIndex('haber', 'have');
        ST[I].W := 'hay';
        ST[I].En := 'be';
      End;
    End;
  { clitiques redondants : e shoh Arbenin ; nuk e di ku... }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'O') And ((ST[I].En = 'it') Or (ST[I].En = 'them')) Then
    Begin
      J := I + 1;
      While (J <= SN) And (ST[J].Cat <> 'V') Do Inc(J);
      If J <= SN Then
      Begin
        If SpPlain(SpLex[ST[J].Idx].Es) = 'saber' Then ST[I].Cat := ' '
        Else
          For K := J + 1 To SN Do
            If ST[K].Cat In ['N', 'Z', 'D', 'T', 'S'] Then ST[I].Cat := ' ';
      End;
    End;
  { 'ne' devant un lieu apres un verbe de mouvement : to }
  For I := 1 To SN Do
    If (ST[I].Cat = 'E') And (ST[I].W = 'ne') Then
    Begin
      J := I - 1;
      While (J >= 1) And (ST[J].Cat <> 'V') Do Dec(J);
      If (J >= 1) And InList(ST[J].En, 'go come enter arrive return leave') Then
      Begin
        ST[I].W := 'a';
        ST[I].En := 'to';
      End;
    End;
  { articles definis : le suffixe devient un determinant }
  I := SN;
  While I >= 1 Do
  Begin
    If (ST[I].Cat = 'N') And (ST[I].Cl = 'D') Then
    Begin
      ST[I].Cl := '';
      If Not ((I > 1) And (ST[I - 1].Cat In ['D', 'T', 'S', 'Q', 'U'])) Then
        If SN < MaxTokens Then
        Begin
          For K := SN Downto I Do
            ST[K + 1] := ST[K];
          Inc(SN);
          FillChar(ST[I], SizeOf(TSTok), 0);
          ST[I].Cat := 'D';
          ST[I].Kind := 'd';
          ST[I].En := 'the';
          ST[I].W := 'el';
          ST[I].Plu := ST[I + 1].Plu;
          ST[I].Tns := ' ';
        End;
    End;
    Dec(I);
  End;
  ARemoveConsumed;
End;

{ ------------------------------------------------------------------ }
{ ALBANAIS -> ANGLAIS : phrase complete                              }
{ ------------------------------------------------------------------ }

Function SqTranslateSentence(Const Sentence: String): String;
Var
  Res, P: String;
Begin
  ATokenize(Sentence);
  If SN = 0 Then
  Begin
    SqTranslateSentence := '';
    Exit;
  End;
  AlClassify;
  If SN = 0 Then
  Begin
    SqTranslateSentence := '';
    Exit;
  End;
  P := FindPhraseEs(SpPhraseKey(1, SN));
  If P <> '' Then
    Res := P
  Else
    Res := TranslateTokensEs(1, SN, SEnd = '?');
  Res := UpperFirst(FixEnglish(Res));
  If SEnd <> #0 Then Res := Res + SEnd;
  SqTranslateSentence := Res;
End;

{ ------------------------------------------------------------------ }
{ PORTUGAIS -> ANGLAIS : lexique et morphologie                      }
{ ------------------------------------------------------------------ }
{ Comme pour l'albanais, les jetons produits sont ceux de l'espagnol   }
{ (ST) : la meme analyse de proposition sert pour les deux langues.    }

Type
  TPt = Record
    Orig: String[26];
    F1, F2, F3, F4: String[26];
    En: String[28];
    Pos: String[3];
    Key: String[10];
    Yo: String[16];
    SpIdx: Integer;
  End;

  TPtIrr = Record
    Inf: String[10];
    Pres, Pret, Imp: Array[1..6] Of String[12];
    HasImp: Boolean;
    Fut, Part, Ger: String[12];
  End;

Const
  PtIrrData: Array[0..17] Of String = (
    'ser|sou,es,e,somos,sois,sao|fui,foste,foi,fomos,fostes,foram|era,eras,era'+
        ',eramos,ereis,eram|ser|sido|sendo',
    'estar|estou,estas,esta,estamos,estais,estao|estive,estiveste,esteve,estiv'+
        'emos,estivestes,estiveram|-|estar|estado|estando',
    'ter|tenho,tens,tem,temos,tendes,tem|tive,tiveste,teve,tivemos,tivestes,ti'+
        'veram|-|ter|tido|tendo',
    'haver|hei,has,ha,havemos,haveis,hao|houve,houveste,houve,houvemos,houvest'+
        'es,houveram|-|haver|havido|havendo',
    'ir|vou,vais,vai,vamos,ides,vao|fui,foste,foi,fomos,fostes,foram|ia,ias,ia'+
        ',iamos,ieis,iam|ir|ido|indo',
    'vir|venho,vens,vem,vimos,vindes,vem|vim,vieste,veio,viemos,viestes,vieram'+
        '|vinha,vinhas,vinha,vinhamos,vinheis,vinham|vir|vindo|vindo',
    'fazer|faco,fazes,faz,fazemos,fazeis,fazem|fiz,fizeste,fez,fizemos,fizeste'+
        's,fizeram|-|far|feito|fazendo',
    'dizer|digo,dizes,diz,dizemos,dizeis,dizem|disse,disseste,disse,dissemos,d'+
        'issestes,disseram|-|dir|dito|dizendo',
    'ver|vejo,ves,ve,vemos,vedes,veem|vi,viste,viu,vimos,vistes,viram|via,vias'+
        ',via,viamos,vieis,viam|ver|visto|vendo',
    'dar|dou,das,da,damos,dais,dao|dei,deste,deu,demos,destes,deram|-|dar|dado'+
        '|dando',
    'saber|sei,sabes,sabe,sabemos,sabeis,sabem|soube,soubeste,soube,soubemos,s'+
        'oubestes,souberam|-|saber|sabido|sabendo',
    'poder|posso,podes,pode,podemos,podeis,podem|pude,pudeste,pode,pudemos,pud'+
        'estes,puderam|-|poder|podido|podendo',
    'querer|quero,queres,quer,queremos,quereis,querem|quis,quiseste,quis,quise'+
        'mos,quisestes,quiseram|-|querer|querido|querendo',
    'trazer|trago,trazes,traz,trazemos,trazeis,trazem|trouxe,trouxeste,trouxe,'+
        'trouxemos,trouxestes,trouxeram|-|trar|trazido|trazendo',
    'sair|saio,sais,sai,saimos,sais,saem|sai,saiste,saiu,saimos,saistes,sairam'+
        '|-|sair|saido|saindo',
    'ler|leio,les,le,lemos,ledes,leem|li,leste,leu,lemos,lestes,leram|-|ler|li'+
        'do|lendo',
    'ouvir|ouco,ouves,ouve,ouvimos,ouvis,ouvem|ouvi,ouviste,ouviu,ouvimos,ouvi'+
        'stes,ouviram|-|ouvir|ouvido|ouvindo',
    'abrir|abro,abres,abre,abrimos,abris,abrem|abri,abriste,abriu,abrimos,abri'+
        'stes,abriram|-|abrir|aberto|abrindo'
  );

Var
  PtLex: Array[1..600] Of TPt;
  PtCount: Integer;
  PtIrr: Array[1..24] Of TPtIrr;
  PtIrrCount: Integer;

Function PtPlain(Const S: String): String;
Var
  I: Integer;
  R: String;
Begin
  R := '';
  I := 1;
  While I <= Length(S) Do
  Begin
    If (S[I] = #$C3) And (I < Length(S)) Then
    Begin
      Case S[I + 1] Of
        #$A0, #$A1, #$A2, #$A3, #$80, #$81, #$82, #$83: R := R + 'a';
        #$A7, #$87: R := R + 'c';
        #$A8, #$A9, #$AA, #$88, #$89, #$8A: R := R + 'e';
        #$AC, #$AD, #$8C, #$8D: R := R + 'i';
        #$B2, #$B3, #$B4, #$B5, #$92, #$93, #$94, #$95: R := R + 'o';
        #$B9, #$BA, #$BC, #$99, #$9A, #$9C: R := R + 'u';
      Else
        R := R + S[I] + S[I + 1];
      End;
      Inc(I, 2);
    End
    Else
    Begin
      R := R + S[I];
      Inc(I);
    End;
  End;
  PtPlain := R;
End;

{ Accent aigu, grave ou circonflexe (pas le tilde ni la cedille). }
Function PtStrongAccent(Const S: String): Boolean;
Var
  I: Integer;
Begin
  PtStrongAccent := False;
  I := 1;
  While I < Length(S) Do
  Begin
    If (S[I] = #$C3) And (S[I + 1] In [#$A0, #$A1, #$A2, #$A9, #$AA, #$AD, #$B3,
        #$B4, #$BA]) Then
      PtStrongAccent := True;
    Inc(I);
  End;
End;

Function PtPlural(Const S: String): String;
Var
  L: Integer;
Begin
  L := Length(S);
  If L = 0 Then PtPlural := S
  Else If EndsWith(S, 'ao') Then PtPlural := Copy(S, 1, L - 2) + 'oes'
  Else If S[L] In ['a', 'e', 'i', 'o', 'u'] Then PtPlural := S + 's'
  Else If S[L] = 'l' Then PtPlural := Copy(S, 1, L - 1) + 'is'
  Else If S[L] = 'm' Then PtPlural := Copy(S, 1, L - 1) + 'ns'
  Else If EndsWith(S, 'is') Or EndsWith(S, 'us') Then PtPlural := S
  Else PtPlural := S + 'es';
End;

Procedure LoadPortuguese;
Var
  I, K: Integer;
  Line, Item, W, Rest, P, Part: String;
Begin
  PtCount := 0;
  For I := 0 To High(PtData) Do
  Begin
    Line := PtData[I];
    While Line <> '' Do
    Begin
      Item := CopyUntil(Line, ';');
      If (Item <> '') And (PtCount < 600) Then
      Begin
        W := CopyUntil(Item, '=');
        Rest := CopyUntil(Item, ':');
        Inc(PtCount);
        FillChar(PtLex[PtCount], SizeOf(TPt), 0);
        P := CopyUntil(W, '/');
        PtLex[PtCount].Orig := ReplaceChar(ReplaceChar(P, '_', ' '), '-', ' ');
        PtLex[PtCount].F1 := PtPlain(PtLex[PtCount].Orig);
        If W <> '' Then PtLex[PtCount].F2 := PtPlain(CopyUntil(W, '/'));
        If W <> '' Then PtLex[PtCount].F3 := PtPlain(CopyUntil(W, '/'));
        If W <> '' Then PtLex[PtCount].F4 := PtPlain(W);
        PtLex[PtCount].En := ReplaceChar(Rest, '_', ' ');
        PtLex[PtCount].Pos := CopyUntil(Item, ':');
        PtLex[PtCount].Key := CopyUntil(Item, ':');
        PtLex[PtCount].Yo := PtPlain(Item);
        If PtLex[PtCount].Pos = 'v' Then
        Begin
          If PtLex[PtCount].Key <> '' Then
            PtLex[PtCount].SpIdx := AlSpIndex(PtLex[PtCount].Key,
                PtLex[PtCount].En)
          Else
            PtLex[PtCount].SpIdx := AlSpIndex(PtLex[PtCount].F1,
                PtLex[PtCount].En);
        End;
      End;
    End;
  End;
  PtIrrCount := 0;
  For I := 0 To High(PtIrrData) Do
    If PtIrrCount < 24 Then
    Begin
      Line := PtIrrData[I];
      Inc(PtIrrCount);
      PtIrr[PtIrrCount].Inf := CopyUntil(Line, '|');
      Part := CopyUntil(Line, '|');
      For K := 1 To 6 Do PtIrr[PtIrrCount].Pres[K] := CopyUntil(Part, ',');
      Part := CopyUntil(Line, '|');
      For K := 1 To 6 Do PtIrr[PtIrrCount].Pret[K] := CopyUntil(Part, ',');
      Part := CopyUntil(Line, '|');
      PtIrr[PtIrrCount].HasImp := Part <> '-';
      If PtIrr[PtIrrCount].HasImp Then
        For K := 1 To 6 Do PtIrr[PtIrrCount].Imp[K] := CopyUntil(Part, ',');
      PtIrr[PtIrrCount].Fut := CopyUntil(Line, '|');
      PtIrr[PtIrrCount].Part := CopyUntil(Line, '|');
      PtIrr[PtIrrCount].Ger := Line;
    End;
End;

Function PtFindNoun(Const W: String; Var Plu: Boolean): Integer;
Var
  I: Integer;
  P: String;
Begin
  PtFindNoun := 0;
  For I := 1 To PtCount Do
    If PtLex[I].Pos = 'n' Then
    Begin
      Plu := False;
      If W = PtLex[I].F1 Then
      Begin
        PtFindNoun := I;
        Exit;
      End;
      Plu := True;
      If PtLex[I].F2 <> '' Then P := PtLex[I].F2 Else P :=
          PtPlural(PtLex[I].F1);
      If W = P Then
      Begin
        PtFindNoun := I;
        Exit;
      End;
    End;
  Plu := False;
End;

Function PtFindAdj(Const W: String; Var Plu: Boolean): Integer;
Var
  I, L: Integer;
  B, Fem, PM, PF: String;
Begin
  PtFindAdj := 0;
  For I := 1 To PtCount Do
    If PtLex[I].Pos = 'a' Then
    Begin
      B := PtLex[I].F1;
      L := Length(B);
      If PtLex[I].F2 <> '' Then
      Begin
        Fem := PtLex[I].F2;
        PM := PtLex[I].F3;
        PF := PtLex[I].F4;
      End
      Else
      Begin
        If EndsWith(B, 'o') Then Fem := Copy(B, 1, L - 1) + 'a'
        Else If EndsWith(B, 'es') Or EndsWith(B, 'or') Then Fem := B + 'a'
        Else Fem := B;
        PM := PtPlural(B);
        PF := PtPlural(Fem);
      End;
      Plu := False;
      If (W = B) Or (W = Fem) Then
      Begin
        PtFindAdj := I;
        Exit;
      End;
      Plu := True;
      If (W = PM) Or (W = PF) Then
      Begin
        PtFindAdj := I;
        Exit;
      End;
    End;
  Plu := False;
End;

Function PtFind(Const W: String; Const Kinds: String): Integer;
Var
  I: Integer;
Begin
  PtFind := 0;
  For I := 1 To PtCount Do
    If (PtLex[I].F1 = W) And (Pos(PtLex[I].Pos, Kinds) > 0) Then
    Begin
      PtFind := I;
      Exit;
    End;
End;

{ ---- formes verbales ---- }

Function PtMatchVerb(Const W: String; PreferPast: Boolean; Var Idx: Integer; Var
    Tns: Char;
                     Var Slot: Integer): Boolean;
Const
  PrAr: Array[1..6] Of String[6] = ('o', 'as', 'a', 'amos', 'ais', 'am');
  PrEr: Array[1..6] Of String[6] = ('o', 'es', 'e', 'emos', 'eis', 'em');
  PrIr: Array[1..6] Of String[6] = ('o', 'es', 'e', 'imos', 'is', 'em');
  PtAr: Array[1..6] Of String[8] = ('ei', 'aste', 'ou', 'amos', 'astes',
      'aram');
  PtEr: Array[1..6] Of String[8] = ('i', 'este', 'eu', 'emos', 'estes', 'eram');
  PtIr: Array[1..6] Of String[8] = ('i', 'iste', 'iu', 'imos', 'istes', 'iram');
  ImAr: Array[1..6] Of String[8] = ('ava', 'avas', 'ava', 'avamos', 'aveis',
      'avam');
  ImEr: Array[1..6] Of String[8] = ('ia', 'ias', 'ia', 'iamos', 'ieis', 'iam');
  FuE: Array[1..6] Of String[6] = ('ei', 'as', 'a', 'emos', 'eis', 'ao');
  CoE: Array[1..6] Of String[8] = ('ia', 'ias', 'ia', 'iamos', 'ieis', 'iam');
Var
  I, Q, K, Irr: Integer;
  Inf, Stem, Cls, Form, FS, Part, YoF, Orig: String;
  Ok: Boolean;

  Function T(Const F: String; Tn: Char; Sl: Integer): Boolean;
  Begin
    T := False;
    If (F <> '') And (F = W) Then
    Begin
      Idx := I;
      Tns := Tn;
      Slot := Sl;
      T := True;
    End;
  End;

  Function PretForm(Q: Integer): String;
  Begin
    If Irr > 0 Then PretForm := PtIrr[Irr].Pret[Q]
    Else
    Begin
      If Cls = 'ar' Then PretForm := Stem + PtAr[Q]
      Else If Cls = 'er' Then PretForm := Stem + PtEr[Q]
      Else PretForm := Stem + PtIr[Q];
      If (Q = 1) And (Cls = 'ar') Then
      Begin
        Orig := PtLex[I].Orig;
        If EndsWith(Orig, 'çar') Then PretForm := Copy(Stem, 1, Length(Stem)) +
            'ei'
        Else If EndsWith(Orig, 'car') Then PretForm := Copy(Stem, 1,
            Length(Stem) - 1) + 'quei'
        Else If EndsWith(Orig, 'gar') Then PretForm := Stem + 'uei';
      End;
    End;
  End;

Begin
  PtMatchVerb := True;
  For I := 1 To PtCount Do
    If PtLex[I].Pos = 'v' Then
    Begin
      Inf := PtLex[I].F1;
      If Length(Inf) < 2 Then Continue;
      Cls := Copy(Inf, Length(Inf) - 1, 2);
      Stem := Copy(Inf, 1, Length(Inf) - 2);
      Irr := 0;
      For K := 1 To PtIrrCount Do
        If PtIrr[K].Inf = Inf Then Irr := K;
      If PreferPast Then
      Begin
        For Q := 1 To 6 Do
          If (Q = 1) Or (Q = 3) Then
            If T(PretForm(Q), 'i', Q) Then Exit;
        If Irr > 0 Then FS := PtIrr[Irr].Fut Else FS := Inf;
        For Q := 1 To 6 Do
          If (Q = 1) Or (Q = 3) Or (Q = 6) Then
            If T(FS + FuE[Q], 'f', Q) Then Exit;
      End;
      { present }
      For Q := 1 To 6 Do
      Begin
        If Irr > 0 Then Form := PtIrr[Irr].Pres[Q]
        Else
        Begin
          If Cls = 'ar' Then Form := Stem + PrAr[Q]
          Else If Cls = 'er' Then Form := Stem + PrEr[Q]
          Else Form := Stem + PrIr[Q];
          If (Q = 1) And (PtLex[I].Yo <> '') Then Form := PtLex[I].Yo;
        End;
        If T(Form, 'p', Q) Then Exit;
      End;
      For Q := 1 To 6 Do
        If T(PretForm(Q), 'i', Q) Then Exit;
      For Q := 1 To 6 Do
      Begin
        If (Irr > 0) And PtIrr[Irr].HasImp Then Form := PtIrr[Irr].Imp[Q]
        Else If Irr > 0 Then
        Begin
          If PtIrr[Irr].Inf = 'estar' Then Form := Stem + ImAr[Q]
          Else If Cls = 'ar' Then Form := Stem + ImAr[Q]
          Else Form := Stem + ImEr[Q];
        End
        Else If Cls = 'ar' Then Form := Stem + ImAr[Q]
        Else Form := Stem + ImEr[Q];
        If T(Form, 'm', Q) Then Exit;
      End;
      If Irr > 0 Then FS := PtIrr[Irr].Fut Else FS := Inf;
      For Q := 1 To 6 Do
        If T(FS + FuE[Q], 'f', Q) Then Exit;
      For Q := 1 To 6 Do
        If T(FS + CoE[Q], 'c', Q) Then Exit;
      If Irr > 0 Then Part := PtIrr[Irr].Part
      Else If Cls = 'ar' Then Part := Stem + 'ado'
      Else Part := Stem + 'ido';
      If T(Part, 'P', 0) Or T(Copy(Part, 1, Length(Part) - 1) + 'a', 'P', 0) Or
         T(Copy(Part, 1, Length(Part) - 1) + 'os', 'P', 0) Or T(Copy(Part, 1,
             Length(Part) - 1) + 'as', 'P', 0) Then Exit;
      If Irr > 0 Then Part := PtIrr[Irr].Ger
      Else If Cls = 'ar' Then Part := Stem + 'ando'
      Else If Cls = 'er' Then Part := Stem + 'endo'
      Else Part := Stem + 'indo';
      If T(Part, 'G', 0) Then Exit;
      If T(Inf, 'I', 0) Then Exit;
      { imperatif de politesse (voce) }
      If Irr > 0 Then YoF := PtIrr[Irr].Pres[1]
      Else If PtLex[I].Yo <> '' Then YoF := PtLex[I].Yo
      Else YoF := Stem + 'o';
      If (YoF <> '') And (YoF[Length(YoF)] = 'o') Then
      Begin
        YoF := Copy(YoF, 1, Length(YoF) - 1);
        If Cls = 'ar' Then
        Begin
          If T(YoF + 'e', 'M', 3) Or T(YoF + 'em', 'M', 6) Then Exit;
        End
        Else If T(YoF + 'a', 'M', 3) Or T(YoF + 'am', 'M', 6) Then Exit;
      End;
    End;
  PtMatchVerb := False;
  Ok := False;
End;

{ ------------------------------------------------------------------ }
{ PORTUGAIS -> ANGLAIS : jetons et etiquetage                        }
{ ------------------------------------------------------------------ }

Procedure PtTokenize(Const S0: String);
Var
  S, Word1, Low, Plain: String;
  I, L: Integer;
  C: Char;
  Cap, Hyph: Boolean;
Begin
  SN := 0;
  SEnd := #0;
  Hyph := False;
  S := '';
  I := 1;
  While I <= Length(S0) Do
  Begin
    If (S0[I] = #$C2) And (I < Length(S0)) And ((S0[I + 1] = #$AB) Or (S0[I + 1]
        = #$BB) Or (S0[I + 1] = #$A0) Or
       (S0[I + 1] = #$BF) Or (S0[I + 1] = #$A1)) Then
    Begin
      S := S + ' ';
      Inc(I, 2);
    End
    Else If (S0[I] = #$E2) And (I + 2 <= Length(S0)) And (S0[I + 1] = #$80) And
            ((S0[I + 2] = #$99) Or (S0[I + 2] = #$98)) Then
    Begin
      S := S + ' ';
      Inc(I, 3);
    End
    Else
    Begin
      S := S + S0[I];
      Inc(I);
    End;
  End;
  L := Length(S);
  I := 1;
  While I <= L Do
  Begin
    C := S[I];
    If FrWordByte(C) Then
    Begin
      Word1 := '';
      While (I <= L) And FrWordByte(S[I]) Do
      Begin
        Word1 := Word1 + S[I];
        Inc(I);
      End;
      Cap := (Word1 <> '') And (((Word1[1] >= 'A') And (Word1[1] <= 'Z')) Or
             ((Word1[1] = #$C3) And (Length(Word1) > 1) And (Word1[2] >= #$80)
                 And (Word1[2] <= #$9E)));
      Low := FrLower(Word1);
      Plain := PtPlain(Low);
      SPush(Plain, Word1, Cap, False, PtStrongAccent(Low));
      If Hyph Then ST[SN].Cl := '-';
      Hyph := False;
    End
    Else
    Begin
      If (C = '.') Or (C = '?') Or (C = '!') Then
      Begin
        If SEnd = #0 Then SEnd := C;
        If C = '?' Then SEnd := '?';
      End
      Else If (C = ',') Or (C = ';') Or (C = ':') Then
      Begin
        SPush(',', ',', False, True, False);
        ST[SN].Cat := ',';
      End
      Else If (C = '-') And (I > 1) And (I < L) And FrWordByte(S[I - 1]) And
          FrWordByte(S[I + 1]) Then
        Hyph := True;
      Inc(I);
    End;
  End;
End;

Function PtNumberEn(Const W: String): String;
Begin
  PtNumberEn := '';
  If (W = 'dois') Or (W = 'duas') Then PtNumberEn := 'two'
  Else If W = 'tres' Then PtNumberEn := 'three'
  Else If W = 'quatro' Then PtNumberEn := 'four'
  Else If W = 'cinco' Then PtNumberEn := 'five'
  Else If W = 'seis' Then PtNumberEn := 'six'
  Else If W = 'sete' Then PtNumberEn := 'seven'
  Else If W = 'oito' Then PtNumberEn := 'eight'
  Else If W = 'nove' Then PtNumberEn := 'nine'
  Else If W = 'dez' Then PtNumberEn := 'ten'
  Else If W = 'onze' Then PtNumberEn := 'eleven'
  Else If W = 'doze' Then PtNumberEn := 'twelve'
  Else If W = 'treze' Then PtNumberEn := 'thirteen'
  Else If (W = 'catorze') Or (W = 'quatorze') Then PtNumberEn := 'fourteen'
  Else If W = 'quinze' Then PtNumberEn := 'fifteen'
  Else If (W = 'dezasseis') Or (W = 'dezesseis') Then PtNumberEn := 'sixteen'
  Else If (W = 'dezassete') Or (W = 'dezessete') Then PtNumberEn := 'seventeen'
  Else If W = 'dezoito' Then PtNumberEn := 'eighteen'
  Else If (W = 'dezanove') Or (W = 'dezenove') Then PtNumberEn := 'nineteen'
  Else If W = 'vinte' Then PtNumberEn := 'twenty'
  Else If W = 'trinta' Then PtNumberEn := 'thirty'
  Else If W = 'quarenta' Then PtNumberEn := 'forty'
  Else If W = 'cinquenta' Then PtNumberEn := 'fifty'
  Else If W = 'sessenta' Then PtNumberEn := 'sixty'
  Else If W = 'setenta' Then PtNumberEn := 'seventy'
  Else If W = 'oitenta' Then PtNumberEn := 'eighty'
  Else If W = 'noventa' Then PtNumberEn := 'ninety'
  Else If W = 'cem' Then PtNumberEn := 'one hundred'
  Else If W = 'mil' Then PtNumberEn := 'one thousand';
End;

Function PtNextNounish(I: Integer): Boolean;
Var
  Plu: Boolean;
Begin
  PtNextNounish := False;
  If I > SN Then Exit;
  If ST[I].Punct Then Exit;
  PtNextNounish := (PtFindNoun(ST[I].W, Plu) > 0) Or (PtFindAdj(ST[I].W, Plu) >
      0) Or
                   InList(ST[I].W,
                       'o a os as um uma uns umas meu minha meus minhas teu tu'+
                       'a seu sua nosso nossa este esta esse essa aquele aquel'+
                       'a dois duas tres todo toda cada muitos muitas');
End;

Function PtNextVerbal(I: Integer): Boolean;
Var
  Idx, Sl: Integer;
  T: Char;
Begin
  PtNextVerbal := False;
  If I > SN Then Exit;
  If ST[I].Punct Then Exit;
  If InList(ST[I].W, 'nao me te se lhe lhes nos') Then
  Begin
    PtNextVerbal := True;
    Exit;
  End;
  PtNextVerbal := PtMatchVerb(ST[I].W, ST[I].Acc, Idx, T, Sl) And (T In ['p',
      'i', 'm', 'f', 'c', 'M']);
End;

Procedure PtExpandContractions;
Var
  I, K: Integer;
  E1, E2: String;
  T: TSTok;
Begin
  I := SN;
  While I >= 1 Do
  Begin
    E1 := '';
    E2 := '';
    If Not ST[I].Punct Then
    Begin
      If ST[I].W = 'do' Then Begin E1 := 'de'; E2 := 'o'; End
      Else If ST[I].W = 'da' Then Begin E1 := 'de'; E2 := 'a'; End
      Else If ST[I].W = 'dos' Then Begin E1 := 'de'; E2 := 'os'; End
      Else If ST[I].W = 'das' Then Begin E1 := 'de'; E2 := 'as'; End
      Else If ST[I].W = 'no' Then Begin E1 := 'em'; E2 := 'o'; End
      Else If ST[I].W = 'na' Then Begin E1 := 'em'; E2 := 'a'; End
      Else If ST[I].W = 'nas' Then Begin E1 := 'em'; E2 := 'as'; End
      Else If (ST[I].W = 'nos') And PtNextNounish(I + 1) Then Begin E1 := 'em';
          E2 := 'os'; End
      Else If ST[I].W = 'ao' Then Begin E1 := 'a'; E2 := 'o'; End
      Else If ST[I].W = 'aos' Then Begin E1 := 'a'; E2 := 'os'; End
      Else If (ST[I].W = 'a') And ST[I].Acc Then Begin E1 := 'a'; E2 := 'a'; End
      Else If ST[I].W = 'as' Then
      Begin
        If ST[I].Acc Then Begin E1 := 'a'; E2 := 'as'; End;
      End
      Else If ST[I].W = 'pelo' Then Begin E1 := 'por'; E2 := 'o'; End
      Else If ST[I].W = 'pela' Then Begin E1 := 'por'; E2 := 'a'; End
      Else If ST[I].W = 'pelos' Then Begin E1 := 'por'; E2 := 'os'; End
      Else If ST[I].W = 'pelas' Then Begin E1 := 'por'; E2 := 'as'; End
      Else If ST[I].W = 'num' Then Begin E1 := 'em'; E2 := 'um'; End
      Else If ST[I].W = 'numa' Then Begin E1 := 'em'; E2 := 'uma'; End
      Else If ST[I].W = 'dum' Then Begin E1 := 'de'; E2 := 'um'; End
      Else If ST[I].W = 'duma' Then Begin E1 := 'de'; E2 := 'uma'; End;
    End;
    If (E1 <> '') And (SN < MaxTokens) Then
    Begin
      T := ST[I];
      For K := SN Downto I + 1 Do
        ST[K + 1] := ST[K];
      Inc(SN);
      ST[I] := T;
      ST[I].W := E1;
      ST[I].Orig := E1;
      ST[I].Acc := False;
      ST[I].Kind := 'e';
      ST[I + 1] := T;
      ST[I + 1].W := E2;
      ST[I + 1].Orig := E2;
      ST[I + 1].Acc := False;
      ST[I + 1].Cap := False;
    End;
    Dec(I);
  End;
End;

Procedure PtMergeMultiword;
Var
  I, K, Len, L: Integer;
  Key: String;
  Bad, Found: Boolean;
Begin
  I := 1;
  While I < SN Do
  Begin
    Found := False;
    For Len := 4 Downto 2 Do
      If (Not Found) And (I + Len - 1 <= SN) Then
      Begin
        Key := '';
        Bad := False;
        For K := I To I + Len - 1 Do
        Begin
          If ST[K].Punct Then Bad := True;
          Key := Join(Key, ST[K].W);
        End;
        If Not Bad Then
        Begin
          L := PtFind(Key, 'rpcjwn');
          If L > 0 Then
          Begin
            Found := True;
            ST[I].W := Key;
            ST[I].Orig := Key;
            For K := I + 1 To SN - Len + 1 Do
              ST[K] := ST[K + Len - 1];
            Dec(SN, Len - 1);
          End;
        End;
      End;
    Inc(I);
  End;
End;

Function PtProperEn(Const Orig: String): String;
Var
  L: String;
Begin
  L := PtPlain(FrLower(Orig));
  If L = 'brasil' Then PtProperEn := 'Brazil'
  Else If L = 'franca' Then PtProperEn := 'France'
  Else If L = 'espanha' Then PtProperEn := 'Spain'
  Else If L = 'alemanha' Then PtProperEn := 'Germany'
  Else If L = 'italia' Then PtProperEn := 'Italy'
  Else If L = 'inglaterra' Then PtProperEn := 'England'
  Else If L = 'lisboa' Then PtProperEn := 'Lisbon'
  Else If L = 'londres' Then PtProperEn := 'London'
  Else If L = 'roma' Then PtProperEn := 'Rome'
  Else If L = 'moscovo' Then PtProperEn := 'Moscow'
  Else If L = 'russia' Then PtProperEn := 'Russia'
  Else If L = 'japao' Then PtProperEn := 'Japan'
  Else If L = 'china' Then PtProperEn := 'China'
  Else If L = 'suica' Then PtProperEn := 'Switzerland'
  Else If L = 'belgica' Then PtProperEn := 'Belgium'
  Else If L = 'mocambique' Then PtProperEn := 'Mozambique'
  Else PtProperEn := Orig;
End;

Procedure PtPickCat(I: Integer);
Var
  W: String;
  Prev: Char;
  Idx, Sl: Integer;
  T: Char;
  Plu, QStart, IsVerb, IsNoun, IsAdj: Boolean;
  LN, LA: Integer;
Begin
  W := ST[I].W;
  If I > 1 Then Prev := ST[I - 1].Cat Else Prev := '.';
  QStart := (SEnd = '?') And ((I = 1) Or (ST[I - 1].Cat = ','));
  If ST[I].Punct Then
  Begin
    ST[I].Cat := ',';
    Exit;
  End;
  If IsDigitsOnly(W) Then
  Begin
    ST[I].Cat := 'U';
    ST[I].En := W;
    Exit;
  End;
  If PtNumberEn(W) <> '' Then
  Begin
    ST[I].Cat := 'U';
    ST[I].En := PtNumberEn(W);
    Exit;
  End;
  If ST[I].Kind = 'e' Then
  Begin
    Idx := PtFind(W, 'p');
    ST[I].Cat := 'E';
    ST[I].Kind := ' ';
    If Idx > 0 Then ST[I].En := PtLex[Idx].En Else ST[I].En := W;
    If W = 'em' Then ST[I].W := 'en';
    Exit;
  End;
  If W = 'nao' Then
  Begin
    ST[I].Cat := 'G';
    Exit;
  End;
  If (W = 'nunca') Or (W = 'jamais') Then
  Begin
    ST[I].Cat := 'R';
    ST[I].En := 'never';
    Exit;
  End;
  If (W = 'nada') Or (W = 'ninguem') Then
  Begin
    ST[I].Cat := 'g';
    If W = 'nada' Then ST[I].En := 'anything' Else ST[I].En := 'anyone';
    Exit;
  End;
  { pronoms sujets }
  If InList(W, 'eu ele ela voce eles elas voces vos tu') Or ((W = 'nos') And
      ST[I].Acc) Then
  Begin
    ST[I].Cat := 'P';
    If W = 'eu' Then Begin ST[I].En := 'I'; ST[I].Slot := 1; End
    Else If W = 'tu' Then Begin ST[I].En := 'you'; ST[I].Slot := 2; End
    Else If W = 'ele' Then Begin ST[I].En := 'he'; ST[I].Slot := 3; End
    Else If W = 'ela' Then Begin ST[I].En := 'she'; ST[I].Slot := 3; End
    Else If W = 'voce' Then Begin ST[I].En := 'you'; ST[I].Slot := 3; End
    Else If W = 'voces' Then Begin ST[I].En := 'you'; ST[I].Slot := 6; End
    Else If W = 'vos' Then Begin ST[I].En := 'you'; ST[I].Slot := 5; End
    Else If W = 'nos' Then Begin ST[I].En := 'we'; ST[I].Slot := 4; End
    Else Begin ST[I].En := 'they'; ST[I].Slot := 6; End;
    Exit;
  End;
  { articles, pronoms complements et preposition 'a' }
  If InList(W, 'o a os as') Then
  Begin
    If (W = 'a') And (Prev = 'V') And InList(ST[I - 1].En,
        'go come arrive return leave give say tell ask show send') And
       PtNextNounish(I + 1) Then
    Begin
      ST[I].Cat := 'E';
      ST[I].En := 'to';
      Exit;
    End;
    If (W = 'a') And (I < SN) And (ST[I + 1].Cat = 'V') And False Then Exit;
    If (Not PtNextNounish(I + 1)) And PtNextVerbal(I + 1) Then
    Begin
      ST[I].Cat := 'O';
      If W = 'o' Then ST[I].En := 'it' Else If W = 'a' Then ST[I].En := 'her'
          Else ST[I].En := 'them';
      Exit;
    End;
    If (W = 'a') And (Not PtNextNounish(I + 1)) Then
    Begin
      ST[I].Cat := 'E';
      ST[I].En := 'to';
      Exit;
    End;
    ST[I].Cat := 'D';
    ST[I].Kind := 'd';
    ST[I].Plu := (W = 'os') Or (W = 'as');
    Exit;
  End;
  If InList(W, 'um uma') Then
  Begin
    ST[I].Cat := 'D';
    ST[I].Kind := 'i';
    Exit;
  End;
  If InList(W, 'uns umas') Then
  Begin
    ST[I].Cat := 'D';
    ST[I].Kind := 'i';
    ST[I].Plu := True;
    ST[I].En := 'some';
    Exit;
  End;
  If InList(W,
      'meu minha meus minhas teu tua teus tuas seu sua seus suas nosso nossa n'+
      'ossos nossas vosso vossa vossos vossas') Then
  Begin
    ST[I].Cat := 'S';
    If (W = 'meu') Or (W = 'minha') Or (W = 'meus') Or (W = 'minhas') Then
        ST[I].En := 'my'
    Else If Copy(W, 1, 2) = 'te' Then ST[I].En := 'your'
    Else If Copy(W, 1, 2) = 'tu' Then ST[I].En := 'your'
    Else If Copy(W, 1, 2) = 'se' Then ST[I].En := 'his'
    Else If Copy(W, 1, 2) = 'su' Then ST[I].En := 'his'
    Else If Copy(W, 1, 2) = 'no' Then ST[I].En := 'our'
    Else ST[I].En := 'your';
    ST[I].Plu := EndsWith(W, 's');
    Exit;
  End;
  If InList(W,
      'este esta isto estes estas esse essa isso esses essas aquele aquela aqu'+
      'ilo aqueles aquelas') And
     Not (ST[I].Acc Or ((W = 'esta') And (Not PtNextNounish(I + 1)))) Then
  Begin
    ST[I].Cat := 'T';
    If Copy(W, 1, 3) = 'est' Then ST[I].En := 'this' Else ST[I].En := 'that';
    ST[I].Plu := EndsWith(W, 's') And (W <> 'isto') And (W <> 'isso') And (W <>
        'aquilo');
    If ST[I].Plu Then
      If ST[I].En = 'this' Then ST[I].En := 'these' Else ST[I].En := 'those';
    Exit;
  End;
  If InList(W,
      'todo toda todos todas cada algum alguns alguma algumas nenhum nenhuma v'+
      'arios varias outros outras bastante demais') Or
     (InList(W, 'muito muita muitos muitas pouco pouca poucos poucas') And (I <
         SN) And (PtFindNoun(ST[I + 1].W, Plu) > 0)) Then
  Begin
    ST[I].Cat := 'Q';
    ST[I].Plu := EndsWith(W, 's');
    If Copy(W, 1, 4) = 'todo' Then ST[I].En := 'all'
    Else If W = 'cada' Then ST[I].En := 'each'
    Else If Copy(W, 1, 4) = 'muit' Then ST[I].En := 'many'
    Else If Copy(W, 1, 4) = 'pouc' Then ST[I].En := 'few'
    Else If Copy(W, 1, 3) = 'alg' Then ST[I].En := 'some'
    Else If Copy(W, 1, 3) = 'nen' Then ST[I].En := 'no'
    Else If Copy(W, 1, 3) = 'var' Then ST[I].En := 'several'
    Else If Copy(W, 1, 3) = 'out' Then ST[I].En := 'other'
    Else ST[I].En := 'enough';
    If (ST[I].En = 'many') And (Not ST[I].Plu) Then ST[I].En := 'much';
    Exit;
  End;
  If InList(W, 'me te lhe lhes se') Or ((W = 'nos') And Not ST[I].Acc) Then
  Begin
    ST[I].Cat := 'O';
    If W = 'me' Then ST[I].En := 'me'
    Else If W = 'te' Then ST[I].En := 'you'
    Else If W = 'nos' Then ST[I].En := 'us'
    Else If W = 'lhe' Then ST[I].En := 'him'
    Else If W = 'lhes' Then ST[I].En := 'them'
    Else ST[I].En := '';
    Exit;
  End;
  If (W = 'lo') Or (W = 'la') Or (W = 'los') Or (W = 'las') Then
  Begin
    ST[I].Cat := 'O';
    If W = 'lo' Then ST[I].En := 'him' Else If W = 'la' Then ST[I].En := 'her'
        Else ST[I].En := 'them';
    Exit;
  End;
  { mots grammaticaux du lexique }
  Idx := PtFind(W, 'w');
  If (Idx > 0) And (QStart Or InList(W,
      'onde quem qual quanto quantos quantas')) Then
  Begin
    ST[I].Cat := 'W';
    ST[I].En := PtLex[Idx].En;
    If W = 'onde' Then ST[I].W := 'donde'
    Else If W = 'quem' Then ST[I].W := 'quien'
    Else If W = 'qual' Then ST[I].W := 'cual'
    Else If Copy(W, 1, 5) = 'quant' Then ST[I].W := 'cuantos'
    Else If W = 'quando' Then ST[I].W := 'cuando'
    Else If W = 'como' Then ST[I].W := 'como'
    Else ST[I].W := 'que';
    Exit;
  End;
  If (W = 'por que') Or (W = 'o que') Then
  Begin
    ST[I].Cat := 'W';
    If W = 'o que' Then Begin ST[I].En := 'what'; ST[I].W := 'que'; End Else
        Begin ST[I].En := 'why'; ST[I].W := 'por que'; End;
    Exit;
  End;
  If (W = 'porque') And QStart Then
  Begin
    ST[I].Cat := 'W';
    ST[I].En := 'why';
    ST[I].W := 'por que';
    Exit;
  End;
  If (W = 'e') And ST[I].Acc Then
  Begin
    Idx := 0;
  End
  Else
  Begin
    Idx := PtFind(W, 'p');
    If Idx > 0 Then
    Begin
      ST[I].Cat := 'E';
      ST[I].En := PtLex[Idx].En;
      If W = 'em' Then ST[I].W := 'en';
      If W = 'sem' Then ST[I].W := 'sin';
      Exit;
    End;
    Idx := PtFind(W, 'c');
    If Idx > 0 Then
    Begin
      ST[I].Cat := 'C';
      ST[I].En := PtLex[Idx].En;
      Exit;
    End;
  End;
  Idx := PtFind(W, 'j');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'J';
    ST[I].En := PtLex[Idx].En;
    Exit;
  End;
  Idx := PtFind(W, 'r');
  If Idx > 0 Then
  Begin
    If (Prev = 'D') And ((W = 'tarde') Or (W = 'manha')) Then
    Begin
      ST[I].Cat := 'N';
      If W = 'tarde' Then ST[I].En := 'afternoon' Else ST[I].En := 'morning';
      Exit;
    End;
    ST[I].Cat := 'R';
    ST[I].En := PtLex[Idx].En;
    Exit;
  End;
  LN := PtFindNoun(W, Plu);
  IsNoun := LN > 0;
  If IsNoun Then ST[I].Plu := Plu;
  LA := PtFindAdj(W, Plu);
  IsAdj := LA > 0;
  IsVerb := PtMatchVerb(W, ST[I].Acc, Idx, T, Sl);
  If IsVerb And (IsNoun Or IsAdj) And (Prev In ['D', 'S', 'T', 'Q', 'U']) Then
      IsVerb := False;
  If IsVerb And (T = 'P') And IsAdj Then IsVerb := False;
  If IsVerb And (Not (T In ['p', 'i', 'm', 'f', 'c', 'M'])) And (IsNoun Or
      IsAdj) And (Prev <> 'V') Then IsVerb := False;
  If IsVerb Then
  Begin
    ST[I].Cat := 'V';
    ST[I].Idx := PtLex[Idx].SpIdx;
    ST[I].Tns := T;
    ST[I].Slot := Sl;
    ST[I].En := PtLex[Idx].En;
    Exit;
  End;
  If IsNoun And IsAdj Then
  Begin
    If Prev In ['D', 'S', 'T', 'Q', 'U', 'A'] Then IsAdj := False Else IsNoun :=
        False;
  End;
  If IsNoun Then
  Begin
    PtFindNoun(W, Plu);
    ST[I].Cat := 'N';
    ST[I].Plu := Plu;
    ST[I].En := PtLex[LN].En;
    Exit;
  End;
  If IsAdj Then
  Begin
    PtFindAdj(W, Plu);
    ST[I].Cat := 'A';
    ST[I].Plu := Plu;
    ST[I].En := PtLex[LA].En;
    Exit;
  End;
  ST[I].Cat := 'Z';
End;

{ ------------------------------------------------------------------ }
{ PORTUGAIS -> ANGLAIS : lissage et phrase complete                  }
{ ------------------------------------------------------------------ }

Function PtVerbKey(I: Integer): String;
Begin
  If (I >= 1) And (I <= SN) And (ST[I].Cat = 'V') And (ST[I].Idx > 0) Then
    PtVerbKey := SpPlain(SpLex[ST[I].Idx].Es)
  Else
    PtVerbKey := '';
End;

Procedure PtClassify;
Var
  I, J, K: Integer;
  Key: String;
Begin
  PtExpandContractions;
  PtMergeMultiword;
  For I := 1 To SN Do
  Begin
    If (ST[I].Cl = '-') And InList(ST[I].W, 'o a os as') Then
    Begin
      ST[I].Cat := 'O';
      If ST[I].W = 'o' Then ST[I].En := 'it' Else If ST[I].W = 'a' Then ST[I].En
          := 'her' Else ST[I].En := 'them';
    End
    Else
      PtPickCat(I);
  End;
  For I := 1 To SN Do
    If ST[I].Cat = 'Z' Then ST[I].Orig := PtProperEn(ST[I].Orig);
  { pronoms enclitiques : faze-lo, chamo-me }
  For I := 2 To SN Do
    If (ST[I].Cl = '-') And (ST[I].Cat = 'O') And (ST[I - 1].Cat = 'V') Then
    Begin
      Key := PtVerbKey(I - 1);
      If InList(ST[I].W, 'lo la los las') Then ST[I - 1].Tns := 'I';
      If (Key = 'llamar') And InList(ST[I].W, 'me te se') Then
        ST[I - 1].Kind := 'r'
      Else If ST[I].En <> '' Then
        ST[I - 1].Cl := ST[I].En;
      ST[I].Cat := ' ';
    End;
  { ir + infinitif : on intercale la preposition a }
  I := 1;
  While I < SN Do
  Begin
    If (ST[I].Cat = 'V') And (PtVerbKey(I) = 'ir') And (ST[I].Tns In ['p', 'i',
        'm', 'f']) And
       (ST[I + 1].Cat = 'V') And (ST[I + 1].Tns = 'I') And (SN < MaxTokens) Then
    Begin
      For K := SN Downto I + 1 Do
        ST[K + 1] := ST[K];
      Inc(SN);
      FillChar(ST[I + 1], SizeOf(TSTok), 0);
      ST[I + 1].Cat := 'E';
      ST[I + 1].W := 'a';
      ST[I + 1].Orig := 'a';
      ST[I + 1].En := 'to';
      ST[I + 1].Tns := ' ';
      Inc(I);
    End;
    Inc(I);
  End;
  { quando e que o comboio chega : "e que" disparait apres un mot interrogatif }
  For I := 1 To SN - 2 Do
    If (ST[I].Cat = 'W') And (ST[I + 1].Cat = 'V') And (PtVerbKey(I + 1) =
        'ser') And
       (ST[I + 2].W = 'que') Then
    Begin
      ST[I + 1].Cat := ' ';
      ST[I + 2].Cat := ' ';
    End;
  { onde fica a biblioteca : ficar = etre }
  For I := 2 To SN Do
    If (ST[I].Cat = 'V') And (PtVerbKey(I) = 'ficar') And (ST[I - 1].Cat = 'W')
        Then
    Begin
      ST[I].Idx := AlSpIndex('estar', 'be');
      ST[I].En := 'be';
    End;
  { estar a + infinitif (portugais europeen) : forme progressive }
  For I := 1 To SN - 2 Do
    If (ST[I].Cat = 'V') And (PtVerbKey(I) = 'estar') And (ST[I + 1].Cat = 'E')
        And (ST[I + 1].W = 'a') And
       (ST[I + 2].Cat = 'V') And (ST[I + 2].Tns = 'I') Then
    Begin
      ST[I + 2].Tns := 'G';
      ST[I + 1].Cat := ' ';
    End;
  { ter que / de + infinitif }
  For I := 1 To SN - 2 Do
    If (ST[I].Cat = 'V') And (PtVerbKey(I) = 'tener') And (ST[I + 1].Cat In
        ['E', 'C']) And
       InList(ST[I + 1].W, 'de que') And (ST[I + 2].Cat = 'V') And (ST[I +
           2].Tns = 'I') Then
      ST[I + 1].W := 'que';
  { gostar de, precisar de, lembrar de : la preposition disparait }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'V') And (ST[I + 1].Cat = 'E') And (ST[I + 1].W = 'de') And
       InList(PtVerbKey(I), 'gostar precisar lembrar necesitar') Then
      ST[I + 1].Cat := ' ';
  { il y a : ha / havia / tem sans sujet }
  For I := 1 To SN Do
    If (ST[I].Cat = 'V') And (PtVerbKey(I) = 'haber') And (ST[I].Slot = 3) And
       ((I = SN) Or Not ((ST[I + 1].Cat = 'V') And (ST[I + 1].Tns = 'P'))) Then
      ST[I].W := 'hay'
    Else If (ST[I].Cat = 'V') And (PtVerbKey(I) = 'tener') And (ST[I].Slot = 3)
        And (I = 1) And (I < SN) And
            (ST[I + 1].Cat In ['D', 'N', 'U', 'Q']) Then
    Begin
      ST[I].Idx := AlSpIndex('haber', 'have');
      ST[I].W := 'hay';
      ST[I].En := 'be';
    End;
  { o meu livro : l'article devant un possessif disparait }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'D') And (ST[I].Kind = 'd') And (ST[I + 1].Cat = 'S') Then
        ST[I].Cat := ' ';
  { sao oito horas }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'V') And (PtVerbKey(I) = 'ser') Then
    Begin
      J := I + 1;
      If (ST[J].Cat = 'D') Then Inc(J);
      If (J < SN) And (ST[J].Cat = 'U') And (ST[J + 1].Cat = 'N') And (ST[J +
          1].En = 'hour') Then
      Begin
        If ST[J - 1].Cat = 'D' Then
          ST[J + 1].Cat := ' '
        Else
        Begin
          { un article doit preceder le nombre : "son las ocho" }
          ST[J + 1] := ST[J];
          FillChar(ST[J], SizeOf(TSTok), 0);
          ST[J].Cat := 'D';
          ST[J].Kind := 'd';
          ST[J].En := 'the';
          ST[J].Plu := True;
          ST[J].W := 'las';
          ST[J].Tns := ' ';
        End;
      End;
    End;
  ARemoveConsumed;
End;

Function PtTranslateSentence(Const Sentence: String): String;
Var
  Res, P: String;
Begin
  PtTokenize(Sentence);
  If SN = 0 Then
  Begin
    PtTranslateSentence := '';
    Exit;
  End;
  PtClassify;
  If SN = 0 Then
  Begin
    PtTranslateSentence := '';
    Exit;
  End;
  P := FindPhraseEs(SpPhraseKey(1, SN));
  If P <> '' Then
    Res := P
  Else
    Res := TranslateTokensEs(1, SN, SEnd = '?');
  Res := UpperFirst(FixEnglish(Res));
  If SEnd <> #0 Then Res := Res + SEnd;
  PtTranslateSentence := Res;
End;

{ ------------------------------------------------------------------ }
{ INNU-AIMUN (montagnais) -> FRANCAIS : mots et phrases connus          }
{ ------------------------------------------------------------------ }
{ Ce n'est pas un traducteur a regles : l'innu-aimun est polysynthetique }
{ et aucune description grammaticale libre n'est integree. Le programme   }
{ reconnait des phrases entieres, puis des mots isoles ; un mot inconnu   }
{ est conserve tel quel.                                                  }
{                                                                         }
{ Sources des donnees (licence CC BY-SA 4.0) :                            }
{  - Wiktionnaire anglais, categorie "Montagnais lemmas"                  }
{    (https://en.wiktionary.org/wiki/Category:Montagnais_lemmas) ;        }
{  - Wikipedia francophone, article "Innu-aimun" (exemples, expressions   }
{    courantes, conjugaison de "voir").                                   }
{ Les traductions francaises des gloses anglaises du Wiktionnaire sont    }
{ de l'auteur du programme. Ces donnees restent sous CC BY-SA 4.0.        }

Const
  MoeMax = 400;
  MoePhrMax = 60;

Type
  TMoe = Record
    Key: String[44];
    Fr: String[70];
  End;

  TMoePhr = Record
    Key: String[110];
    Fr: String[110];
  End;

Var
  MoeLex: Array[1..MoeMax] Of TMoe;
  MoeCount: Integer;
  MoePhr: Array[1..MoePhrMax] Of TMoePhr;
  MoePhrCount: Integer;
  SourceMoe: Boolean;

Procedure LoadMoe;
Var
  I: Integer;
  Line, Item, Key: String;
Begin
  MoeCount := 0;
  For I := 0 To High(MoeData) Do
  Begin
    Line := MoeData[I];
    While Line <> '' Do
    Begin
      Item := CopyUntil(Line, ';');
      If (Item <> '') And (MoeCount < MoeMax) Then
      Begin
        Key := CopyUntil(Item, '=');
        Inc(MoeCount);
        MoeLex[MoeCount].Key := Key;
        MoeLex[MoeCount].Fr := Item;
      End;
    End;
  End;
  MoePhrCount := 0;
  For I := 0 To High(MoePhrData) Do
    If MoePhrCount < MoePhrMax Then
    Begin
      Item := MoePhrData[I];
      Inc(MoePhrCount);
      MoePhr[MoePhrCount].Key := CopyUntil(Item, '|');
      MoePhr[MoePhrCount].Fr := Item;
    End;
End;

{ Minuscules, "u" a la place de la lettre modificative, sans ponctuation. }
Function MoeNorm(Const S: String): String;
Var
  I: Integer;
  R: String;
Begin
  R := '';
  I := 1;
  While I <= Length(S) Do
  Begin
    If (S[I] = #$E1) And (I + 2 <= Length(S)) And (S[I + 1] = #$B5) And
       (S[I + 2] = #$98) Then
    Begin
      R := R + 'u';
      Inc(I, 3);
    End
    Else If S[I] In ['.', ',', '?', '!', ';', ':'] Then
      Inc(I)
    Else If (S[I] = ' ') And ((R = '') Or (R[Length(R)] = ' ')) Then
      Inc(I)
    Else
    Begin
      R := R + S[I];
      Inc(I);
    End;
  End;
  If (R <> '') And (R[Length(R)] = ' ') Then Delete(R, Length(R), 1);
  MoeNorm := FrLower(R);
End;

Function MoeLookup(Const W: String): String;
Var
  I: Integer;
  K: String;
Begin
  K := MoeNorm(W);
  MoeLookup := '';
  For I := 1 To MoeCount Do
    If MoeLex[I].Key = K Then
    Begin
      MoeLookup := MoeLex[I].Fr;
      Exit;
    End;
End;

Function MoeTranslateSentence(Const Sentence: String): String;
Var
  I, L: Integer;
  Word1, Res, Found, K: String;
  C: Char;
  EndMark: Char;
Begin
  K := MoeNorm(Sentence);
  For I := 1 To MoePhrCount Do
    If MoePhr[I].Key = K Then
    Begin
      MoeTranslateSentence := MoePhr[I].Fr;
      Exit;
    End;
  Res := '';
  EndMark := #0;
  L := Length(Sentence);
  I := 1;
  While I <= L Do
  Begin
    C := Sentence[I];
    If FrWordByte(C) Then
    Begin
      Word1 := '';
      While (I <= L) And (FrWordByte(Sentence[I]) Or
            (((Sentence[I] = '-') Or (Sentence[I] = '''')) And (Word1 <> '') And
             (I < L) And FrWordByte(Sentence[I + 1]))) Do
      Begin
        Word1 := Word1 + Sentence[I];
        Inc(I);
      End;
      Found := MoeLookup(Word1);
      If Found = '' Then Found := Word1;
      Res := Join(Res, Found);
    End
    Else
    Begin
      If (C = '.') Or (C = '?') Or (C = '!') Then
        EndMark := C
      Else If (C = ',') Or (C = ';') Or (C = ':') Then
        Res := Res + C;
      Inc(I);
    End;
  End;
  Res := UpperFirst(Res);
  If EndMark <> #0 Then Res := Res + EndMark;
  MoeTranslateSentence := Res;
End;

{ ------------------------------------------------------------------ }
{ SUEDOIS -> ANGLAIS : lexique et morphologie                         }
{ ------------------------------------------------------------------ }
{ Comme pour l'albanais et le portugais, les jetons produits sont ceux   }
{ de l'espagnol (ST) : la meme analyse de proposition sert ensuite.      }

Type
  TSv = Record
    Orig: String[26];
    F1, F2, F3, F4: String[24];
    En: String[28];
    Pos: String[3];
    Key: String[10];
    SpIdx: Integer;
  End;

Var
  SvLex: Array[1..700] Of TSv;
  SvCount: Integer;
  SourceSv: Boolean;
  SvLastDef, SvLastGen: Boolean;

{ Les lettres a ring, a et o trema restent telles quelles : les formes }
{ pliees se confondraient (har / har, far / far). }
Function SvPlain(Const S: String): String;
Begin
  SvPlain := S;
End;

Procedure LoadSwedish;
Var
  I: Integer;
  Line, Item, W, Rest, P: String;
Begin
  SvCount := 0;
  For I := 0 To High(SvData) Do
  Begin
    Line := SvData[I];
    While Line <> '' Do
    Begin
      Item := CopyUntil(Line, ';');
      If (Item <> '') And (SvCount < 700) Then
      Begin
        W := CopyUntil(Item, '=');
        Rest := CopyUntil(Item, ':');
        Inc(SvCount);
        FillChar(SvLex[SvCount], SizeOf(TSv), 0);
        P := CopyUntil(W, '/');
        SvLex[SvCount].Orig := ReplaceChar(P, '_', ' ');
        SvLex[SvCount].F1 := SvPlain(SvLex[SvCount].Orig);
        If W <> '' Then SvLex[SvCount].F2 := SvPlain(CopyUntil(W, '/'));
        If W <> '' Then SvLex[SvCount].F3 := SvPlain(CopyUntil(W, '/'));
        If W <> '' Then SvLex[SvCount].F4 := SvPlain(W);
        SvLex[SvCount].En := ReplaceChar(Rest, '_', ' ');
        SvLex[SvCount].Pos := CopyUntil(Item, ':');
        SvLex[SvCount].Key := Item;
        If Copy(SvLex[SvCount].Pos, 1, 1) = 'v' Then
        Begin
          If SvLex[SvCount].Key <> '' Then
            SvLex[SvCount].SpIdx := AlSpIndex(SvLex[SvCount].Key,
                SvLex[SvCount].En)
          Else
            SvLex[SvCount].SpIdx := AlSpIndex('sv:' + SvLex[SvCount].F1,
                SvLex[SvCount].En);
        End;
      End;
    End;
  End;
End;

Function SvIsVowel(C: Char): Boolean;
Begin
  SvIsVowel := C In ['a', 'e', 'i', 'o', 'u', 'y'];
End;

{ Formes d'un nom : [0] singulier, [1] defini, [2] pluriel, [3] pluriel defini }
Procedure SvNounForms(I: Integer; Var Sg, DefSg, Pl, DefPl: String);
Var
  B: String;
  L: Integer;
  Neuter: Boolean;
Begin
  B := SvLex[I].F1;
  L := Length(B);
  Neuter := SvLex[I].Pos = 'nn';
  Sg := B;
  If Neuter Then
  Begin
    If SvIsVowel(B[L]) Then DefSg := B + 't'
    Else If EndsWith(B, 'er') Then DefSg := Copy(B, 1, L - 2) + 'ret'
    Else DefSg := B + 'et';
  End
  Else
  Begin
    If B[L] = 'e' Then DefSg := Copy(B, 1, L - 1) + 'en'
    Else If B[L] = 'a' Then DefSg := Copy(B, 1, L - 1) + 'an'
    Else If SvIsVowel(B[L]) Then DefSg := B + 'n'
    Else DefSg := B + 'en';
  End;
  If SvLex[I].F2 <> '' Then Pl := SvLex[I].F2
  Else If Neuter Then
  Begin
    If SvIsVowel(B[L]) Then Pl := B + 'n' Else Pl := B;
  End
  Else If B[L] = 'a' Then Pl := Copy(B, 1, L - 1) + 'or'
  Else Pl := B + 'ar';
  If (Pl = B) And Neuter Then DefPl := Pl + 'en'
  Else If (Pl = B) And (B[L] = 'e') Then DefPl := Copy(B, 1, L - 1) + 'na'
  Else If EndsWith(Pl, 'n') Then DefPl := Pl + 'a'
  Else DefPl := Pl + 'na';
End;

Function SvFindNounExact(Const W: String; Var Plu, Def: Boolean): Integer;
Var
  I: Integer;
  Sg, D1, Pl, D2: String;
Begin
  SvFindNounExact := 0;
  For I := 1 To SvCount Do
    If (SvLex[I].Pos = 'nu') Or (SvLex[I].Pos = 'nn') Then
    Begin
      SvNounForms(I, Sg, D1, Pl, D2);
      If W = Sg Then Begin Plu := False; Def := False; SvFindNounExact := I;
          Exit; End;
      If W = Pl Then Begin Plu := True; Def := False; SvFindNounExact := I;
          Exit; End;
      If W = D1 Then Begin Plu := False; Def := True; SvFindNounExact := I;
          Exit; End;
      If W = D2 Then Begin Plu := True; Def := True; SvFindNounExact := I; Exit;
          End;
    End;
End;

Function SvFindNoun(Const W: String; Var Plu, Def, Gen: Boolean): Integer;
Var
  I: Integer;
Begin
  Gen := False;
  Def := False;
  SvFindNoun := SvFindNounExact(W, Plu, Def);
  If (SvFindNoun = 0) And (Length(W) > 3) And (W[Length(W)] = 's') Then
  Begin
    I := SvFindNounExact(Copy(W, 1, Length(W) - 1), Plu, Def);
    If I > 0 Then
    Begin
      Gen := True;
      SvFindNoun := I;
    End;
  End;
End;

Function SvFindAdj(Const W: String; Var Plu: Boolean): Integer;
Var
  I: Integer;
  B, N, P: String;
Begin
  SvFindAdj := 0;
  For I := 1 To SvCount Do
    If SvLex[I].Pos = 'a' Then
    Begin
      B := SvLex[I].F1;
      If SvLex[I].F2 <> '' Then N := SvLex[I].F2 Else N := B + 't';
      If SvLex[I].F3 <> '' Then P := SvLex[I].F3 Else P := B + 'a';
      Plu := False;
      If (W = B) Or (W = N) Then
      Begin
        SvFindAdj := I;
        Exit;
      End;
      Plu := True;
      If (W = P) Or (W = 'lilla') And (B = 'liten') Then
      Begin
        SvFindAdj := I;
        Exit;
      End;
    End;
  Plu := False;
End;

Function SvFind(Const W: String; Const Kinds: String): Integer;
Var
  I: Integer;
Begin
  SvFind := 0;
  For I := 1 To SvCount Do
    If (SvLex[I].F1 = W) And (Pos(SvLex[I].Pos, Kinds) > 0) Then
    Begin
      SvFind := I;
      Exit;
    End;
End;

{ Formes verbales : I infinitif, p present, i preterit, P supin, M imperatif }
Function SvMatchVerb(Const W: String; Var Idx: Integer; Var Tns: Char): Boolean;
Var
  I, L: Integer;
  Cls, B, Stem, Pret, Pres: String;

  Function T(Const F: String; Tn: Char): Boolean;
  Begin
    T := False;
    If (F <> '') And (F = W) Then
    Begin
      Idx := I;
      Tns := Tn;
      T := True;
    End;
  End;

Begin
  SvMatchVerb := True;
  For I := 1 To SvCount Do
    If Copy(SvLex[I].Pos, 1, 1) = 'v' Then
    Begin
      Cls := SvLex[I].Pos;
      B := SvLex[I].F1;
      L := Length(B);
      If Cls = 'v4' Then
      Begin
        If T(SvLex[I].F2, 'p') Or T(SvLex[I].F3, 'i') Or T(SvLex[I].F4, 'P') Or
            T(B, 'I') Then Exit;
        If EndsWith(B, 'a') Then Stem := Copy(B, 1, L - 1) Else Stem := B;
        If T(Stem, 'M') Then Exit;
      End
      Else If Cls = 'v3' Then
      Begin
        If T(B, 'I') Or T(B + 'r', 'p') Or T(B + 'dde', 'i') Or T(B + 'tt', 'P')
            Then Exit;
      End
      Else
      Begin
        If EndsWith(B, 'a') Then Stem := Copy(B, 1, L - 1) Else Stem := B;
        If Cls = 'v1' Then
        Begin
          If T(Stem + 'ar', 'p') Or T(Stem + 'ade', 'i') Or T(Stem + 'at', 'P')
              Or T(B, 'I') Then Exit;
        End
        Else
        Begin
          If (Stem <> '') And (Stem[Length(Stem)] In ['k', 'p', 't', 's', 'x'])
              Then Pret := Stem + 'te'
          Else Pret := Stem + 'de';
          If (Stem <> '') And (Stem[Length(Stem)] = 'r') Then Pres := Stem Else
              Pres := Stem + 'er';
          If T(Pres, 'p') Or T(Pret, 'i') Or T(Stem + 't', 'P') Or T(B, 'I')
              Then Exit;
        End;
        If T(Stem, 'M') Then Exit;
      End;
    End;
  SvMatchVerb := False;
End;

{ ------------------------------------------------------------------ }
{ SUEDOIS -> ANGLAIS : jetons et etiquetage                           }
{ ------------------------------------------------------------------ }

Procedure SvTokenize(Const S0: String);
Var
  S, Word1, Low: String;
  I, L: Integer;
  C: Char;
  Cap: Boolean;
Begin
  SN := 0;
  SEnd := #0;
  S := '';
  I := 1;
  While I <= Length(S0) Do
  Begin
    If (S0[I] = #$C2) And (I < Length(S0)) And ((S0[I + 1] = #$AB) Or (S0[I + 1]
        = #$BB) Or (S0[I + 1] = #$A0) Or
       (S0[I + 1] = #$BF) Or (S0[I + 1] = #$A1)) Then
    Begin
      S := S + ' ';
      Inc(I, 2);
    End
    Else If (S0[I] = #$E2) And (I + 2 <= Length(S0)) And (S0[I + 1] = #$80) And
            ((S0[I + 2] = #$99) Or (S0[I + 2] = #$98)) Then
    Begin
      S := S + ' ';
      Inc(I, 3);
    End
    Else
    Begin
      S := S + S0[I];
      Inc(I);
    End;
  End;
  L := Length(S);
  I := 1;
  While I <= L Do
  Begin
    C := S[I];
    If FrWordByte(C) Then
    Begin
      Word1 := '';
      While (I <= L) And FrWordByte(S[I]) Do
      Begin
        Word1 := Word1 + S[I];
        Inc(I);
      End;
      Cap := (Word1 <> '') And (((Word1[1] >= 'A') And (Word1[1] <= 'Z')) Or
             ((Word1[1] = #$C3) And (Length(Word1) > 1) And (Word1[2] >= #$80)
                 And (Word1[2] <= #$9E)));
      Low := FrLower(Word1);
      SPush(SvPlain(Low), Word1, Cap, False, False);
    End
    Else
    Begin
      If (C = '.') Or (C = '?') Or (C = '!') Then
      Begin
        If SEnd = #0 Then SEnd := C;
        If C = '?' Then SEnd := '?';
      End
      Else If (C = ',') Or (C = ';') Or (C = ':') Then
      Begin
        SPush(',', ',', False, True, False);
        ST[SN].Cat := ',';
      End;
      Inc(I);
    End;
  End;
End;

Function SvNumberEn(Const W: String): String;
Begin
  SvNumberEn := '';
  If W = 'noll' Then SvNumberEn := 'zero'
  Else If W = 'två' Then SvNumberEn := 'two'
  Else If W = 'tre' Then SvNumberEn := 'three'
  Else If W = 'fyra' Then SvNumberEn := 'four'
  Else If W = 'fem' Then SvNumberEn := 'five'
  Else If W = 'sex' Then SvNumberEn := 'six'
  Else If W = 'sju' Then SvNumberEn := 'seven'
  Else If W = 'åtta' Then SvNumberEn := 'eight'
  Else If W = 'nio' Then SvNumberEn := 'nine'
  Else If W = 'tio' Then SvNumberEn := 'ten'
  Else If W = 'elva' Then SvNumberEn := 'eleven'
  Else If W = 'tolv' Then SvNumberEn := 'twelve'
  Else If W = 'tretton' Then SvNumberEn := 'thirteen'
  Else If W = 'fjorton' Then SvNumberEn := 'fourteen'
  Else If W = 'femton' Then SvNumberEn := 'fifteen'
  Else If W = 'sexton' Then SvNumberEn := 'sixteen'
  Else If W = 'sjutton' Then SvNumberEn := 'seventeen'
  Else If W = 'arton' Then SvNumberEn := 'eighteen'
  Else If W = 'nitton' Then SvNumberEn := 'nineteen'
  Else If W = 'tjugo' Then SvNumberEn := 'twenty'
  Else If W = 'trettio' Then SvNumberEn := 'thirty'
  Else If W = 'fyrtio' Then SvNumberEn := 'forty'
  Else If W = 'femtio' Then SvNumberEn := 'fifty'
  Else If W = 'sextio' Then SvNumberEn := 'sixty'
  Else If W = 'sjuttio' Then SvNumberEn := 'seventy'
  Else If W = 'åttio' Then SvNumberEn := 'eighty'
  Else If W = 'nittio' Then SvNumberEn := 'ninety'
  Else If W = 'hundra' Then SvNumberEn := 'one hundred'
  Else If W = 'tusen' Then SvNumberEn := 'one thousand';
End;

Function SvProperEn(Const Orig: String): String;
Var
  L: String;
Begin
  L := FrLower(Orig);
  If L = 'sverige' Then SvProperEn := 'Sweden'
  Else If L = 'frankrike' Then SvProperEn := 'France'
  Else If L = 'tyskland' Then SvProperEn := 'Germany'
  Else If L = 'spanien' Then SvProperEn := 'Spain'
  Else If L = 'italien' Then SvProperEn := 'Italy'
  Else If L = 'england' Then SvProperEn := 'England'
  Else If L = 'danmark' Then SvProperEn := 'Denmark'
  Else If L = 'norge' Then SvProperEn := 'Norway'
  Else If L = 'finland' Then SvProperEn := 'Finland'
  Else If L = 'ryssland' Then SvProperEn := 'Russia'
  Else If L = 'kina' Then SvProperEn := 'China'
  Else If L = 'schweiz' Then SvProperEn := 'Switzerland'
  Else If L = 'belgien' Then SvProperEn := 'Belgium'
  Else If L = 'nederländerna' Then SvProperEn := 'the Netherlands'
  Else If L = 'köpenhamn' Then SvProperEn := 'Copenhagen'
  Else If L = 'göteborg' Then SvProperEn := 'Gothenburg'
  Else If L = 'lissabon' Then SvProperEn := 'Lisbon'
  Else If L = 'moskva' Then SvProperEn := 'Moscow'
  Else If L = 'rom' Then SvProperEn := 'Rome'
  Else If L = 'wien' Then SvProperEn := 'Vienna'
  Else If L = 'usa' Then SvProperEn := 'the United States'
  Else SvProperEn := Orig;
End;

Function SvNextNounish(I: Integer): Boolean;
Var
  Plu, Def, Gen: Boolean;
Begin
  SvNextNounish := False;
  If I > SN Then Exit;
  If ST[I].Punct Then Exit;
  SvNextNounish := (SvFindNoun(ST[I].W, Plu, Def, Gen) > 0) Or
      (SvFindAdj(ST[I].W, Plu) > 0) Or
                   InList(ST[I].W,
                       'en ett min mitt mina din ditt dina vår vårt våra ha'+
                       'ns hennes deras denna detta dessa två tre fyra fem se'+
                       'x sju åtta nio tio alla några många varje') Or
                   (SvNumberEn(ST[I].W) <> '');
End;

Function SvNextVerbal(I: Integer): Boolean;
Var
  Idx: Integer;
  T: Char;
Begin
  SvNextVerbal := False;
  If I > SN Then Exit;
  If ST[I].Punct Then Exit;
  If InList(ST[I].W, 'inte aldrig också alltid') Then
  Begin
    SvNextVerbal := True;
    Exit;
  End;
  SvNextVerbal := SvMatchVerb(ST[I].W, Idx, T) And (T In ['p', 'i']);
End;

Procedure SvMergeMultiword;
Var
  I, K, Len, L: Integer;
  Key: String;
  Bad, Found: Boolean;
Begin
  I := 1;
  While I < SN Do
  Begin
    Found := False;
    For Len := 3 Downto 2 Do
      If (Not Found) And (I + Len - 1 <= SN) Then
      Begin
        Key := '';
        Bad := False;
        For K := I To I + Len - 1 Do
        Begin
          If ST[K].Punct Then Bad := True;
          Key := Join(Key, ST[K].W);
        End;
        If Not Bad Then
        Begin
          L := SvFind(Key, 'rpcjw');
          If L > 0 Then
          Begin
            Found := True;
            ST[I].W := Key;
            ST[I].Orig := Key;
            For K := I + 1 To SN - Len + 1 Do
              ST[K] := ST[K + Len - 1];
            Dec(SN, Len - 1);
          End;
        End;
      End;
    Inc(I);
  End;
End;

Procedure SvPickCat(I: Integer);
Var
  W: String;
  Prev: Char;
  Idx, Sl: Integer;
  T: Char;
  Plu, Def, Gen, QStart, IsVerb, IsNoun, IsAdj: Boolean;
  LN, LA: Integer;
Begin
  W := ST[I].W;
  If I > 1 Then Prev := ST[I - 1].Cat Else Prev := '.';
  QStart := (SEnd = '?') And ((I = 1) Or (ST[I - 1].Cat = ','));
  If ST[I].Punct Then
  Begin
    ST[I].Cat := ',';
    Exit;
  End;
  If IsDigitsOnly(W) Then
  Begin
    ST[I].Cat := 'U';
    ST[I].En := W;
    Exit;
  End;
  If SvNumberEn(W) <> '' Then
  Begin
    ST[I].Cat := 'U';
    ST[I].En := SvNumberEn(W);
    Exit;
  End;
  If W = 'inte' Then
  Begin
    ST[I].Cat := 'G';
    Exit;
  End;
  If W = 'aldrig' Then
  Begin
    ST[I].Cat := 'R';
    ST[I].En := 'never';
    Exit;
  End;
  If (W = 'ingenting') Or (W = 'inget') And (Not SvNextNounish(I + 1)) Then
  Begin
    ST[I].Cat := 'g';
    ST[I].En := 'anything';
    Exit;
  End;
  If (W = 'ingen') And (Not SvNextNounish(I + 1)) Then
  Begin
    ST[I].Cat := 'g';
    ST[I].En := 'anyone';
    Exit;
  End;
  { pronoms sujets }
  If InList(W, 'jag du han hon vi ni') Or ((W = 'de') And (Not SvNextNounish(I +
      1))) Then
  Begin
    ST[I].Cat := 'P';
    If W = 'jag' Then Begin ST[I].En := 'I'; ST[I].Slot := 1; End
    Else If W = 'du' Then Begin ST[I].En := 'you'; ST[I].Slot := 2; End
    Else If W = 'han' Then Begin ST[I].En := 'he'; ST[I].Slot := 3; End
    Else If W = 'hon' Then Begin ST[I].En := 'she'; ST[I].Slot := 3; End
    Else If W = 'vi' Then Begin ST[I].En := 'we'; ST[I].Slot := 4; End
    Else If W = 'ni' Then Begin ST[I].En := 'you'; ST[I].Slot := 5; End
    Else Begin ST[I].En := 'they'; ST[I].Slot := 6; End;
    Exit;
  End;
  { den / det / de : article defini devant adjectif ou nom defini }
  If InList(W, 'den det de') Then
  Begin
    If SvNextNounish(I + 1) Then
    Begin
      ST[I].Cat := 'D';
      ST[I].Kind := 'd';
      ST[I].Plu := W = 'de';
    End
    Else
    Begin
      ST[I].Cat := 'P';
      ST[I].En := 'it';
      ST[I].Slot := 3;
    End;
    Exit;
  End;
  If (W = 'man') And (Prev <> 'D') And (Prev <> 'A') And SvNextVerbal(I + 1)
      Then
  Begin
    ST[I].Cat := 'P';
    ST[I].En := 'one';
    ST[I].Slot := 3;
    Exit;
  End;
  { articles indefinis }
  If InList(W, 'en ett') Then
  Begin
    ST[I].Cat := 'D';
    ST[I].Kind := 'i';
    Exit;
  End;
  { possessifs }
  If InList(W,
      'min mitt mina din ditt dina hans hennes dess vår vårt våra deras sin'+
      ' sitt sina') Or
     (InList(W, 'er ert era') And SvNextNounish(I + 1)) Then
  Begin
    ST[I].Cat := 'S';
    If InList(W, 'min mitt mina') Then ST[I].En := 'my'
    Else If InList(W, 'din ditt dina er ert era') Then ST[I].En := 'your'
    Else If W = 'hans' Then ST[I].En := 'his'
    Else If W = 'hennes' Then ST[I].En := 'her'
    Else If W = 'dess' Then ST[I].En := 'its'
    Else If InList(W, 'vår vårt våra') Then ST[I].En := 'our'
    Else If W = 'deras' Then ST[I].En := 'their'
    Else ST[I].En := 'his';
    ST[I].Plu := InList(W, 'mina dina våra era sina');
    Exit;
  End;
  { demonstratifs }
  If InList(W, 'denna detta dessa') Then
  Begin
    ST[I].Cat := 'T';
    If W = 'dessa' Then Begin ST[I].En := 'these'; ST[I].Plu := True; End
    Else ST[I].En := 'this';
    Exit;
  End;
  { quantificateurs }
  If InList(W, 'alla allt varje några något många flera båda inga') Or
     (InList(W, 'mycket') And SvNextNounish(I + 1) And
      (SvFindNoun(ST[I + 1].W, Plu, Def, Gen) > 0)) Or
     (InList(W, 'ingen inget') And SvNextNounish(I + 1)) Then
  Begin
    ST[I].Cat := 'Q';
    If InList(W, 'alla allt') Then ST[I].En := 'all'
    Else If W = 'varje' Then ST[I].En := 'each'
    Else If InList(W, 'några något') Then ST[I].En := 'some'
    Else If W = 'många' Then ST[I].En := 'many'
    Else If W = 'mycket' Then ST[I].En := 'much'
    Else If W = 'flera' Then ST[I].En := 'several'
    Else If W = 'båda' Then ST[I].En := 'both'
    Else ST[I].En := 'no';
    ST[I].Plu := InList(W, 'alla några många flera båda inga');
    Exit;
  End;
  { pronoms complements }
  If InList(W, 'mig dig honom henne oss dem sig') Or (InList(W, 'er') And (Not
      SvNextNounish(I + 1))) Then
  Begin
    ST[I].Cat := 'O';
    If W = 'mig' Then ST[I].En := 'me'
    Else If W = 'dig' Then ST[I].En := 'you'
    Else If W = 'honom' Then ST[I].En := 'him'
    Else If W = 'henne' Then ST[I].En := 'her'
    Else If W = 'oss' Then ST[I].En := 'us'
    Else If W = 'er' Then ST[I].En := 'you'
    Else If W = 'dem' Then ST[I].En := 'them'
    Else ST[I].En := '';
    Exit;
  End;
  { mots interrogatifs }
  Idx := SvFind(W, 'w');
  If (Idx > 0) And (QStart Or ((SEnd = '?') And InList(W,
      'vem varför hur vad vilken vilket vilka hur många hur mycket'))) Then
  Begin
    ST[I].Cat := 'W';
    ST[I].En := SvLex[Idx].En;
    If W = 'var' Then ST[I].W := 'donde'
    Else If W = 'vem' Then ST[I].W := 'quien'
    Else If Copy(W, 1, 4) = 'vilk' Then ST[I].W := 'cual'
    Else If W = 'hur många' Then ST[I].W := 'cuantos'
    Else If W = 'hur mycket' Then ST[I].W := 'cuantos'
    Else If W = 'när' Then ST[I].W := 'cuando'
    Else If W = 'hur' Then ST[I].W := 'como'
    Else If W = 'varför' Then ST[I].W := 'por que'
    Else ST[I].W := 'que';
    Exit;
  End;
  { prepositions, conjonctions, interjections, adverbes }
  Idx := SvFind(W, 'p');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'E';
    ST[I].En := SvLex[Idx].En;
    If W = 'till' Then ST[I].W := 'a'
    Else If W = 'av' Then ST[I].W := 'de'
    Else If (W = 'i') Or (W = 'på') Or (W = 'vid') Then ST[I].W := 'en';
    Exit;
  End;
  Idx := SvFind(W, 'c');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'C';
    ST[I].En := SvLex[Idx].En;
    Exit;
  End;
  Idx := SvFind(W, 'j');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'J';
    ST[I].En := SvLex[Idx].En;
    Exit;
  End;
  Idx := SvFind(W, 'r');
  If (Idx > 0) And Not ((W = 'bra') And ((Prev = 'V') Or SvNextNounish(I + 1)))
      Then
  Begin
    ST[I].Cat := 'R';
    ST[I].En := SvLex[Idx].En;
    Exit;
  End;
  { noms, adjectifs, verbes }
  LN := SvFindNoun(W, Plu, Def, Gen);
  IsNoun := LN > 0;
  If IsNoun Then
  Begin
    ST[I].Plu := Plu;
    ST[I].Cl := '';
    If Def Then ST[I].Cl := 'def';
    If Gen Then ST[I].Cl := ST[I].Cl + 'gen';
  End;
  LA := SvFindAdj(W, Plu);
  IsAdj := LA > 0;
  IsVerb := SvMatchVerb(W, Idx, T);
  If IsVerb And (IsNoun Or IsAdj) And (Prev In ['D', 'S', 'T', 'Q', 'U']) Then
      IsVerb := False;
  If IsVerb And (T = 'P') And IsAdj Then IsVerb := False;
  If IsVerb And (Not (T In ['p', 'i', 'M'])) And (IsNoun Or IsAdj) And (Prev <>
      'V') And (I > 1) Then IsVerb := False;
  If IsVerb Then
  Begin
    ST[I].Cat := 'V';
    ST[I].Idx := SvLex[Idx].SpIdx;
    ST[I].Tns := T;
    ST[I].Slot := 3;
    ST[I].En := SvLex[Idx].En;
    Exit;
  End;
  If IsNoun And IsAdj Then
  Begin
    If Prev In ['D', 'S', 'T', 'Q', 'U', 'A'] Then IsAdj := False Else IsNoun :=
        False;
  End;
  If IsNoun Then
  Begin
    ST[I].Cat := 'N';
    ST[I].En := SvLex[LN].En;
    Exit;
  End;
  If IsAdj Then
  Begin
    SvFindAdj(W, Plu);
    ST[I].Cat := 'A';
    ST[I].Plu := Plu;
    ST[I].En := SvLex[LA].En;
    Exit;
  End;
  ST[I].Cat := 'Z';
End;

{ ------------------------------------------------------------------ }
{ SUEDOIS -> ANGLAIS : lissage et phrase complete                     }
{ ------------------------------------------------------------------ }

Function SvVerbKey(I: Integer): String;
Begin
  If (I >= 1) And (I <= SN) And (ST[I].Cat = 'V') And (ST[I].Idx > 0) Then
    SvVerbKey := SpPlain(SpLex[ST[I].Idx].Es)
  Else
    SvVerbKey := '';
End;

Procedure SvInsert(Pos1: Integer);
Var
  K: Integer;
Begin
  If SN >= MaxTokens Then Exit;
  For K := SN Downto Pos1 Do
    ST[K + 1] := ST[K];
  Inc(SN);
  FillChar(ST[Pos1], SizeOf(TSTok), 0);
  ST[Pos1].Tns := ' ';
End;

{ Fin du groupe nominal sujet qui commence en K (0 s'il n'y en a pas). }
Function SvSubjEnd(K: Integer): Integer;
Var
  J: Integer;
Begin
  SvSubjEnd := 0;
  If (K < 1) Or (K > SN) Then Exit;
  If ST[K].Cat = 'P' Then
  Begin
    SvSubjEnd := K;
    Exit;
  End;
  J := K;
  While (J <= SN) And (ST[J].Cat In ['D', 'S', 'T', 'Q', 'U']) Do Inc(J);
  While (J <= SN) And (ST[J].Cat = 'A') Do Inc(J);
  If (J <= SN) And (ST[J].Cat = 'N') Then
    SvSubjEnd := J
  Else If (J <= SN) And (ST[J].Cat = 'Z') And ST[J].Cap And (J > 1) Then
  Begin
    While (J < SN) And (ST[J + 1].Cat = 'Z') Do Inc(J);
    SvSubjEnd := J;
  End;
End;

{ Vrai si le jeton I-1 termine un groupe nominal sujet (et non un complement). }
Function SvSubjBefore(I: Integer): Boolean;
Var
  J: Integer;
Begin
  SvSubjBefore := False;
  If I <= 1 Then Exit;
  If ST[I - 1].Cat = 'P' Then
  Begin
    SvSubjBefore := True;
    Exit;
  End;
  J := I - 1;
  While (J >= 1) And (ST[J].Cat In ['N', 'A', 'D', 'S', 'T', 'Q', 'U', 'Z']) Do
      Dec(J);
  If J < I - 1 Then
    SvSubjBefore := (J < 1) Or (ST[J].Cat <> 'E');
End;

Procedure SvMoveBlock(From1, To1, Dest: Integer);
{ deplace les jetons From1..To1 juste avant la position Dest (Dest < From1) }
Var
  Tmp: Array[1..12] Of TSTok;
  N, K: Integer;
Begin
  N := To1 - From1 + 1;
  If (N < 1) Or (N > 12) Or (Dest >= From1) Then Exit;
  For K := 1 To N Do Tmp[K] := ST[From1 + K - 1];
  For K := From1 - 1 Downto Dest Do ST[K + N] := ST[K];
  For K := 1 To N Do ST[Dest + K - 1] := Tmp[K];
End;

Procedure SvClassify;
Var
  I, J, K, E: Integer;
  Key, Owner: String;
  Fin: Boolean;
Begin
  SvMergeMultiword;
  For I := 1 To SN Do
    SvPickCat(I);
  For I := 1 To SN Do
    If ST[I].Cat = 'Z' Then ST[I].Orig := SvProperEn(ST[I].Orig);
  { travailler / etre "pa" une banque, une ecole... : at }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'E') And (ST[I].W = 'en') And (ST[I].En = 'on') Then
    Begin
      K := I + 1;
      While (K < SN) And (ST[K].Cat In ['D', 'A', 'S']) Do Inc(K);
      If (ST[K].Cat = 'N') And InList(ST[K].En,
          'bank office school university hospital restaurant shop market stati'+
          'on airport library') Then
        ST[I].En := 'at';
    End;
  { nom propre au genitif : Annas bil -> la voiture d'Anna }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'Z') And ST[I].Cap And (Length(ST[I].Orig) > 3) And
       (ST[I].Orig[Length(ST[I].Orig)] = 's') And
       ((ST[I + 1].Cat = 'N') Or ((ST[I + 1].Cat = 'A') And (I + 2 <= SN) And
           (ST[I + 2].Cat = 'N'))) Then
    Begin
      ST[I].Orig := Copy(ST[I].Orig, 1, Length(ST[I].Orig) - 1);
      ST[I].Cl := 'gen';
    End;
  { genitif : on place le possede devant, puis "de" et le possesseur }
  I := 1;
  While I < SN Do
  Begin
    If (ST[I].Cl <> '') And (Pos('gen', ST[I].Cl) > 0) And (ST[I].Cat In ['N',
        'Z']) Then
    Begin
      K := I + 1;
      While (K <= SN) And (ST[K].Cat = 'A') Do Inc(K);
      If (K <= SN) And (ST[K].Cat = 'N') And (K - I <= 6) And (SN + 2 <=
          MaxTokens) Then
      Begin
        If ST[I].Cat = 'N' Then
        Begin
          Owner := ST[I].En;
          If ST[I].Plu Then Owner := EnPlural(Owner);
          If Pos('def', ST[I].Cl) > 0 Then Owner := 'the ' + Owner;
          ST[I].Orig := Owner;
          ST[I].Cat := 'Z';
          If (I > 1) And (ST[I - 1].Cat = 'D') And (ST[I - 1].Kind = 'd') Then
              ST[I - 1].Cat := ' ';
        End;
        ST[I].Cl := '';
        ST[I].Cap := True;
        { ordre : possede, "de", possesseur }
        ST[0] := ST[I];
        For J := I To K - 1 Do
          ST[J] := ST[J + 1];
        FillChar(ST[K], SizeOf(TSTok), 0);
        ST[K].Tns := ' ';
        ST[K].Cat := 'E';
        ST[K].W := 'de';
        ST[K].Orig := 'de';
        ST[K].En := 'of';
        SvInsert(K + 1);
        Owner := ST[0].Orig;
        ST[K + 1] := ST[0];
        ST[K + 1].Orig := Owner;
      End;
    End;
    Inc(I);
  End;
  { articles definis portes par le suffixe du nom }
  I := 1;
  While I <= SN Do
  Begin
    If (ST[I].Cat = 'N') And (Pos('def', ST[I].Cl) > 0) Then
    Begin
      K := I;
      While (K > 1) And (ST[K - 1].Cat = 'A') Do Dec(K);
      If (K = 1) Or Not (ST[K - 1].Cat In ['D', 'S', 'T', 'Q', 'U']) Then
      Begin
        SvInsert(K);
        ST[K].Cat := 'D';
        ST[K].Kind := 'd';
        ST[K].Plu := ST[I + 1].Plu;
        ST[K].W := 'el';
        ST[K].Orig := 'el';
        Inc(I);
      End;
    End;
    Inc(I);
  End;
  { adjectifs devant un nom sans determinant : nom puis adjectifs }
  I := 1;
  While I < SN Do
  Begin
    If (ST[I].Cat = 'A') And ((I = 1) Or Not (ST[I - 1].Cat In ['D', 'S', 'T',
        'Q', 'U', 'A'])) Then
    Begin
      K := I;
      While (K < SN) And (ST[K].Cat = 'A') Do Inc(K);
      If (ST[K].Cat = 'N') And (K - I <= 3) Then
      Begin
        ST[0] := ST[K];
        For J := K Downto I + 1 Do
          ST[J] := ST[J - 1];
        ST[I] := ST[0];
        I := K;
      End
      Else
        I := K;
    End;
    Inc(I);
  End;
  { att + infinitif }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'C') And (ST[I].W = 'att') And (ST[I + 1].Cat = 'V') And
        (ST[I + 1].Tns = 'I') Then
    Begin
      ST[I].Cat := ' ';
    End;
  { inversion du verbe en seconde position : Idag gar jag -> Idag jag gar }
  I := 1;
  While I < SN Do
  Begin
    If (ST[I].Cat = 'V') And (ST[I].Tns In ['p', 'i']) And (Not SvSubjBefore(I))
        Then
    Begin
      E := SvSubjEnd(I + 1);
      If (E > 0) And (E - I <= 8) Then
        If (SvVerbKey(I) <> 'haber') Or (ST[I + 1].Cat = 'P') Then
        Begin
          SvMoveBlock(I + 1, E, I);
          I := E;
        End;
    End;
    Inc(I);
  End;
  { det finns / det regnar / heter }
  For I := 1 To SN Do
    If ST[I].Cat = 'V' Then
    Begin
      Key := SvVerbKey(I);
      If Key = 'haber' Then
      Begin
        ST[I].W := 'hay';
        ST[I].Slot := 3;
        If (I > 1) And (ST[I - 1].Cat = 'P') And (ST[I - 1].W = 'det') Then ST[I
            - 1].Cat := ' ';
      End
      Else If Key = 'llover' Then
      Begin
        ST[I].Kind := 'g';
        If (I > 1) And (ST[I - 1].Cat = 'P') And (ST[I - 1].W = 'det') Then ST[I
            - 1].Cat := ' ';
      End
      Else If Key = 'llamar' Then
        ST[I].Kind := 'r';
    End;
  { ha + supin : temps compose }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'V') And (SvVerbKey(I) = 'tener') And (ST[I].Tns In ['p',
        'i']) Then
    Begin
      J := I + 1;
      While (J <= SN) And (ST[J].Cat In ['R', 'G', 'P']) Do Inc(J);
      If (J <= SN) And (ST[J].Cat = 'V') And (ST[J].Tns = 'P') Then
      Begin
        ST[I].Idx := AlSpIndex('haber', 'have');
        ST[I].En := 'have';
      End;
    End;
  { negation : inte apres le verbe -> avant le verbe }
  For I := 2 To SN Do
    If (ST[I].Cat = 'G') And (ST[I - 1].Cat = 'V') And (ST[I - 1].Tns In ['p',
        'i', 'M']) Then
    Begin
      Key := '';
      ST[0] := ST[I];
      ST[I] := ST[I - 1];
      ST[I - 1] := ST[0];
    End;
  { impératif en tete de phrase }
  If (SN >= 1) And (ST[1].Cat = 'V') And (ST[1].Tns In ['p', 'i', 'I', 'M']) And
      (SEnd <> '?') And
     (SvVerbKey(1) <> 'haber') Then
  Begin
    ST[1].Tns := 'M';
    ST[1].Slot := 3;
  End;
  { personne du verbe d'apres le sujet qui precede }
  For I := 1 To SN Do
    If (ST[I].Cat = 'V') And (ST[I].Tns In ['p', 'i']) Then
    Begin
      ST[I].Slot := 3;
      J := I - 1;
      While (J >= 1) And (ST[J].Cat In ['R', 'G']) Do Dec(J);
      If (J >= 1) And (ST[J].Cat = 'P') Then
        ST[I].Slot := ST[J].Slot
      Else If (J >= 1) And (ST[J].Cat In ['N', 'Z']) Then
      Begin
        If ST[J].Plu And (ST[J].Cat = 'N') Then ST[I].Slot := 6;
        K := J;
        While (K > 1) And (ST[K - 1].Cat In ['D', 'S', 'T', 'Q', 'U', 'A']) Do
            Dec(K);
        If (K > 2) And (ST[K - 1].Cat = 'C') And (ST[K - 1].En = 'and') Then
            ST[I].Slot := 6;
      End;
    End;
  { tycka om = aimer ; var ligger X ? = ou est X ? }
  For I := 1 To SN Do
    If (ST[I].Cat = 'V') And (ST[I].Idx > 0) Then
    Begin
      If (I < SN) And (ST[I + 1].Cat = 'E') And (ST[I + 1].W = 'om') And
          (SpLex[ST[I].Idx].Es = 'sv:tycka') Then
      Begin
        ST[I].Idx := AlSpIndex('sv:tyckaom', 'like');
        ST[I].En := 'like';
        ST[I + 1].Cat := ' ';
      End
      Else If InList(ST[I].W, 'ligger låg står stod') And (ST[1].Cat = 'W')
          And (ST[1].W = 'donde') Then
      Begin
        ST[I].Idx := AlSpIndex('estar', 'be');
        ST[I].En := 'be';
      End;
    End;
  { ska + infinitif : aller + a + infinitif }
  I := 1;
  While I < SN Do
  Begin
    If (ST[I].Cat = 'V') And (SvVerbKey(I) = 'ir') And InList(ST[I].W,
        'ska skall skulle') And
       (ST[I + 1].Cat = 'V') And (ST[I + 1].Tns = 'I') And (SN < MaxTokens) Then
    Begin
      SvInsert(I + 1);
      ST[I + 1].Cat := 'E';
      ST[I + 1].W := 'a';
      ST[I + 1].Orig := 'a';
      ST[I + 1].En := 'to';
      Inc(I);
    End;
    Inc(I);
  End;
  { lang d'une personne : grand }
  For I := 3 To SN Do
    If (ST[I].Cat = 'A') And (ST[I].En = 'long') And (ST[I - 1].Cat = 'V') And
       ((ST[I - 2].Cat = 'P') Or ((ST[I - 2].Cat = 'N') And SpIsPersonEn(ST[I -
           2].En))) Then
      ST[I].En := 'tall';
  { complement d'une preposition : pronom de groupe nominal }
  For I := 2 To SN Do
    If (ST[I].Cat = 'O') And (ST[I - 1].Cat = 'E') And (ST[I].En <> '') Then
      ST[I].Cat := 'B';
  { quantificateur pluriel : le nom est au pluriel }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'Q') And ST[I].Plu Then
    Begin
      K := I + 1;
      While (K < SN) And (ST[K].Cat = 'A') Do Inc(K);
      If ST[K].Cat = 'N' Then ST[K].Plu := True;
    End;
  { pronoms complements apres le verbe : enclitiques }
  For I := 2 To SN Do
    If (ST[I].Cat = 'O') And (ST[I - 1].Cat = 'V') And (ST[I].En <> '') Then
    Begin
      ST[I - 1].Cl := ST[I].En;
      ST[I].Cat := ' ';
    End;
  ARemoveConsumed;
End;

Function SvTranslateSentence(Const Sentence: String): String;
Var
  Res: String;
Begin
  SvTokenize(Sentence);
  If SN = 0 Then
  Begin
    SvTranslateSentence := '';
    Exit;
  End;
  SvClassify;
  If SN = 0 Then
  Begin
    SvTranslateSentence := '';
    Exit;
  End;
  Res := TranslateTokensEs(1, SN, SEnd = '?');
  { Vad heter du ? -> What is your name ? }
  If (SEnd = '?') And (Length(Res) > 8) And (Copy(Res, Length(Res) - 7, 8) =
      ' name is') Then
    Res := 'What is ' + LowerAscii(Copy(Res, 1, Length(Res) - 8)) + ' name';
  Res := UpperFirst(FixEnglish(Res));
  If SEnd <> #0 Then Res := Res + SEnd;
  SvTranslateSentence := Res;
End;

{ ------------------------------------------------------------------ }
{ DANOIS -> ANGLAIS : lexique et morphologie                         }
{ ------------------------------------------------------------------ }
{ Comme pour l'albanais et le portugais, les jetons produits sont ceux   }
{ de l'espagnol (ST) : la meme analyse de proposition sert ensuite.      }

Type
  TDa = Record
    Orig: String[26];
    F1, F2, F3, F4: String[24];
    En: String[28];
    Pos: String[3];
    Key: String[10];
    SpIdx: Integer;
  End;

Var
  DaLex: Array[1..700] Of TDa;
  DaCount: Integer;
  SourceDa: Boolean;
  DaLastDef, DaLastGen: Boolean;

{ Les lettres a ring, a et o trema restent telles quelles : les formes }
{ pliees se confondraient (har / har, far / far). }
Function DaPlain(Const S: String): String;
Begin
  DaPlain := S;
End;

Procedure LoadDanish;
Var
  I: Integer;
  Line, Item, W, Rest, P: String;
Begin
  DaCount := 0;
  For I := 0 To High(DaData) Do
  Begin
    Line := DaData[I];
    While Line <> '' Do
    Begin
      Item := CopyUntil(Line, ';');
      If (Item <> '') And (DaCount < 700) Then
      Begin
        W := CopyUntil(Item, '=');
        Rest := CopyUntil(Item, ':');
        Inc(DaCount);
        FillChar(DaLex[DaCount], SizeOf(TDa), 0);
        P := CopyUntil(W, '/');
        DaLex[DaCount].Orig := ReplaceChar(P, '_', ' ');
        DaLex[DaCount].F1 := DaPlain(DaLex[DaCount].Orig);
        If W <> '' Then DaLex[DaCount].F2 := DaPlain(CopyUntil(W, '/'));
        If W <> '' Then DaLex[DaCount].F3 := DaPlain(CopyUntil(W, '/'));
        If W <> '' Then DaLex[DaCount].F4 := DaPlain(W);
        DaLex[DaCount].En := ReplaceChar(Rest, '_', ' ');
        DaLex[DaCount].Pos := CopyUntil(Item, ':');
        DaLex[DaCount].Key := Item;
        If Copy(DaLex[DaCount].Pos, 1, 1) = 'v' Then
        Begin
          If DaLex[DaCount].Key <> '' Then
            DaLex[DaCount].SpIdx := AlSpIndex(DaLex[DaCount].Key,
                DaLex[DaCount].En)
          Else
            DaLex[DaCount].SpIdx := AlSpIndex('da:' + DaLex[DaCount].F1,
                DaLex[DaCount].En);
        End;
      End;
    End;
  End;
End;

Function DaIsVowel(C: Char): Boolean;
Begin
  DaIsVowel := C In ['a', 'e', 'i', 'o', 'u', 'y'];
End;

{ Formes d'un nom : singulier, defini, pluriel, pluriel defini }
Procedure DaNounForms(I: Integer; Var Sg, DefSg, Pl, DefPl: String);
Var
  B: String;
  L: Integer;
  Neuter: Boolean;
Begin
  B := DaLex[I].F1;
  L := Length(B);
  Neuter := DaLex[I].Pos = 'nn';
  Sg := B;
  If B[L] = 'e' Then
  Begin
    If Neuter Then DefSg := B + 't' Else DefSg := B + 'n';
  End
  Else If Neuter Then DefSg := B + 'et'
  Else DefSg := B + 'en';
  If DaLex[I].F2 <> '' Then Pl := DaLex[I].F2
  Else If B[L] = 'e' Then Pl := B + 'r'
  Else Pl := B + 'er';
  If EndsWith(Pl, 'e') Or EndsWith(Pl, 'er') Then DefPl := Pl + 'ne'
  Else DefPl := Pl + 'ene';
End;

Function DaFindNounExact(Const W: String; Var Plu, Def: Boolean): Integer;
Var
  I: Integer;
  Sg, D1, Pl, D2: String;
Begin
  DaFindNounExact := 0;
  For I := 1 To DaCount Do
    If (DaLex[I].Pos = 'nu') Or (DaLex[I].Pos = 'nn') Then
    Begin
      DaNounForms(I, Sg, D1, Pl, D2);
      If W = Sg Then Begin Plu := False; Def := False; DaFindNounExact := I;
          Exit; End;
      If W = Pl Then Begin Plu := True; Def := False; DaFindNounExact := I;
          Exit; End;
      If (W = D1) Or ((Length(D1) > Length(Sg)) And (W = Sg + Sg[Length(Sg)] +
          Copy(D1, Length(Sg) + 1, 3))) Then Begin Plu := False; Def := True;
          DaFindNounExact := I; Exit; End;
      If W = D2 Then Begin Plu := True; Def := True; DaFindNounExact := I; Exit;
          End;
    End;
End;

Function DaFindNoun(Const W: String; Var Plu, Def, Gen: Boolean): Integer;
Var
  I: Integer;
Begin
  Gen := False;
  Def := False;
  DaFindNoun := DaFindNounExact(W, Plu, Def);
  If (DaFindNoun = 0) And (Length(W) > 3) And (W[Length(W)] = 's') Then
  Begin
    I := DaFindNounExact(Copy(W, 1, Length(W) - 1), Plu, Def);
    If I > 0 Then
    Begin
      Gen := True;
      DaFindNoun := I;
    End;
  End;
End;

Function DaFindAdj(Const W: String; Var Plu: Boolean): Integer;
Var
  I: Integer;
  B, N, P: String;
Begin
  DaFindAdj := 0;
  For I := 1 To DaCount Do
    If DaLex[I].Pos = 'a' Then
    Begin
      B := DaLex[I].F1;
      If DaLex[I].F2 <> '' Then N := DaLex[I].F2 Else N := B + 't';
      If DaLex[I].F3 <> '' Then P := DaLex[I].F3 Else P := B + 'e';
      Plu := False;
      If (W = B) Or (W = N) Then
      Begin
        DaFindAdj := I;
        Exit;
      End;
      Plu := True;
      If W = P Then
      Begin
        DaFindAdj := I;
        Exit;
      End;
    End;
  Plu := False;
End;

Function DaFind(Const W: String; Const Kinds: String): Integer;
Var
  I: Integer;
Begin
  DaFind := 0;
  For I := 1 To DaCount Do
    If (DaLex[I].F1 = W) And (Pos(DaLex[I].Pos, Kinds) > 0) Then
    Begin
      DaFind := I;
      Exit;
    End;
End;

{ Formes verbales : I infinitif, p present, i preterit, P parfait, M imperatif }
Function DaMatchVerb(Const W: String; Var Idx: Integer; Var Tns: Char): Boolean;
Var
  I, L: Integer;
  Cls, B, Stem: String;

  Function T(Const F: String; Tn: Char): Boolean;
  Begin
    T := False;
    If (F <> '') And (F = W) Then
    Begin
      Idx := I;
      Tns := Tn;
      T := True;
    End;
  End;

Begin
  DaMatchVerb := True;
  For I := 1 To DaCount Do
    If Copy(DaLex[I].Pos, 1, 1) = 'v' Then
    Begin
      Cls := DaLex[I].Pos;
      B := DaLex[I].F1;
      L := Length(B);
      If Cls = 'v4' Then
      Begin
        If T(DaLex[I].F2, 'p') Or T(DaLex[I].F3, 'i') Or T(DaLex[I].F4, 'P') Or
            T(B, 'I') Then Exit;
        If EndsWith(B, 'e') Then Stem := Copy(B, 1, L - 1) Else Stem := B;
        If T(Stem, 'M') Then Exit;
      End
      Else If Cls = 'v3' Then
      Begin
        If T(B, 'I') Or T(B + 'r', 'p') Or T(B + 'ede', 'i') Or T(B + 'et', 'P')
            Then Exit;
      End
      Else
      Begin
        If EndsWith(B, 'e') Then Stem := Copy(B, 1, L - 1) Else Stem := B;
        If Cls = 'v1' Then
        Begin
          If T(Stem + 'er', 'p') Or T(Stem + 'ede', 'i') Or T(Stem + 'et', 'P')
              Or T(B, 'I') Then Exit;
        End
        Else
        Begin
          If T(Stem + 'er', 'p') Or T(Stem + 'te', 'i') Or T(Stem + 't', 'P') Or
              T(B, 'I') Then Exit;
        End;
        If T(Stem, 'M') Then Exit;
      End;
    End;
  DaMatchVerb := False;
End;

{ ------------------------------------------------------------------ }
{ DANOIS -> ANGLAIS : jetons et etiquetage                           }
{ ------------------------------------------------------------------ }

Procedure DaTokenize(Const S0: String);
Var
  S, Word1, Low: String;
  I, L: Integer;
  C: Char;
  Cap: Boolean;
Begin
  SN := 0;
  SEnd := #0;
  S := '';
  I := 1;
  While I <= Length(S0) Do
  Begin
    If (S0[I] = #$C2) And (I < Length(S0)) And ((S0[I + 1] = #$AB) Or (S0[I + 1]
        = #$BB) Or (S0[I + 1] = #$A0) Or
       (S0[I + 1] = #$BF) Or (S0[I + 1] = #$A1)) Then
    Begin
      S := S + ' ';
      Inc(I, 2);
    End
    Else If (S0[I] = #$E2) And (I + 2 <= Length(S0)) And (S0[I + 1] = #$80) And
            ((S0[I + 2] = #$99) Or (S0[I + 2] = #$98)) Then
    Begin
      S := S + ' ';
      Inc(I, 3);
    End
    Else
    Begin
      S := S + S0[I];
      Inc(I);
    End;
  End;
  L := Length(S);
  I := 1;
  While I <= L Do
  Begin
    C := S[I];
    If FrWordByte(C) Then
    Begin
      Word1 := '';
      While (I <= L) And FrWordByte(S[I]) Do
      Begin
        Word1 := Word1 + S[I];
        Inc(I);
      End;
      Cap := (Word1 <> '') And (((Word1[1] >= 'A') And (Word1[1] <= 'Z')) Or
             ((Word1[1] = #$C3) And (Length(Word1) > 1) And (Word1[2] >= #$80)
                 And (Word1[2] <= #$9E)));
      Low := FrLower(Word1);
      SPush(DaPlain(Low), Word1, Cap, False, False);
    End
    Else
    Begin
      If (C = '.') Or (C = '?') Or (C = '!') Then
      Begin
        If SEnd = #0 Then SEnd := C;
        If C = '?' Then SEnd := '?';
      End
      Else If (C = ',') Or (C = ';') Or (C = ':') Then
      Begin
        SPush(',', ',', False, True, False);
        ST[SN].Cat := ',';
      End;
      Inc(I);
    End;
  End;
End;

Function DaNumberEn(Const W: String): String;
Const
  N: Array[0..30] Of String = ('nul', 'to', 'tre', 'fire', 'fem', 'seks', 'syv',
      'otte', 'ni', 'ti',
    'elleve', 'tolv', 'tretten', 'fjorten', 'femten', 'seksten', 'sytten',
        'atten', 'nitten',
    'tyve', 'tredive', 'fyrre', 'halvtreds', 'tres', 'halvfjerds', 'firs',
        'halvfems',
    'hundrede', 'tusind', 'million', 'x');
  E: Array[0..30] Of String = ('zero', 'two', 'three', 'four', 'five', 'six',
      'seven', 'eight', 'nine', 'ten',
    'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen',
        'seventeen', 'eighteen', 'nineteen',
    'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty',
        'ninety',
    'one hundred', 'one thousand', 'one million', 'x');
Var
  K: Integer;
Begin
  DaNumberEn := '';
  For K := 0 To 29 Do
    If W = N[K] Then DaNumberEn := E[K];
End;

Function DaProperEn(Const Orig: String): String;
Var
  L: String;
Begin
  L := FrLower(Orig);
  If L = 'danmark' Then DaProperEn := 'Denmark'
  Else If L = 'sverige' Then DaProperEn := 'Sweden'
  Else If L = 'norge' Then DaProperEn := 'Norway'
  Else If L = 'tyskland' Then DaProperEn := 'Germany'
  Else If L = 'frankrig' Then DaProperEn := 'France'
  Else If L = 'spanien' Then DaProperEn := 'Spain'
  Else If L = 'italien' Then DaProperEn := 'Italy'
  Else If L = 'england' Then DaProperEn := 'England'
  Else If L = 'finland' Then DaProperEn := 'Finland'
  Else If L = 'island' Then DaProperEn := 'Iceland'
  Else If L = 'rusland' Then DaProperEn := 'Russia'
  Else If L = 'kina' Then DaProperEn := 'China'
  Else If L = 'schweiz' Then DaProperEn := 'Switzerland'
  Else If L = 'belgien' Then DaProperEn := 'Belgium'
  Else If L = 'holland' Then DaProperEn := 'the Netherlands'
  Else If L = 'københavn' Then DaProperEn := 'Copenhagen'
  Else If L = 'rom' Then DaProperEn := 'Rome'
  Else If L = 'wien' Then DaProperEn := 'Vienna'
  Else If L = 'moskva' Then DaProperEn := 'Moscow'
  Else DaProperEn := Orig;
End;

Function DaNextNounish(I: Integer): Boolean;
Var
  Plu, Def, Gen: Boolean;
Begin
  DaNextNounish := False;
  If I > SN Then Exit;
  If ST[I].Punct Then Exit;
  DaNextNounish := (DaFindNoun(ST[I].W, Plu, Def, Gen) > 0) Or
      (DaFindAdj(ST[I].W, Plu) > 0) Or
                   InList(ST[I].W,
                       'en et min mit mine din dit dine vores jeres hans hende'+
                       's deres denne dette disse alle nogle mange hver') Or
                   (DaNumberEn(ST[I].W) <> '');
End;

Function DaNextVerbal(I: Integer): Boolean;
Var
  Idx: Integer;
  T: Char;
Begin
  DaNextVerbal := False;
  If I > SN Then Exit;
  If ST[I].Punct Then Exit;
  If InList(ST[I].W, 'ikke aldrig også altid') Then
  Begin
    DaNextVerbal := True;
    Exit;
  End;
  DaNextVerbal := DaMatchVerb(ST[I].W, Idx, T) And (T In ['p', 'i']);
End;

Procedure DaMergeMultiword;
Var
  I, K, Len, L: Integer;
  Key: String;
  Bad, Found: Boolean;
Begin
  I := 1;
  While I < SN Do
  Begin
    Found := False;
    For Len := 3 Downto 2 Do
      If (Not Found) And (I + Len - 1 <= SN) Then
      Begin
        Key := '';
        Bad := False;
        For K := I To I + Len - 1 Do
        Begin
          If ST[K].Punct Then Bad := True;
          Key := Join(Key, ST[K].W);
        End;
        If Not Bad Then
        Begin
          L := DaFind(Key, 'rpcjw');
          If L > 0 Then
          Begin
            Found := True;
            ST[I].W := Key;
            ST[I].Orig := Key;
            For K := I + 1 To SN - Len + 1 Do
              ST[K] := ST[K + Len - 1];
            Dec(SN, Len - 1);
          End;
        End;
      End;
    Inc(I);
  End;
End;

Procedure DaPickCat(I: Integer);
Var
  W: String;
  Prev: Char;
  Idx, Sl: Integer;
  T: Char;
  Plu, Def, Gen, QStart, IsVerb, IsNoun, IsAdj: Boolean;
  LN, LA: Integer;
Begin
  W := ST[I].W;
  If I > 1 Then Prev := ST[I - 1].Cat Else Prev := '.';
  QStart := (SEnd = '?') And ((I = 1) Or (ST[I - 1].Cat = ','));
  If ST[I].Punct Then
  Begin
    ST[I].Cat := ',';
    Exit;
  End;
  If IsDigitsOnly(W) Then
  Begin
    ST[I].Cat := 'U';
    ST[I].En := W;
    Exit;
  End;
  If DaNumberEn(W) <> '' Then
  Begin
    ST[I].Cat := 'U';
    ST[I].En := DaNumberEn(W);
    Exit;
  End;
  If W = 'ikke' Then
  Begin
    ST[I].Cat := 'G';
    Exit;
  End;
  If W = 'aldrig' Then
  Begin
    ST[I].Cat := 'R';
    ST[I].En := 'never';
    Exit;
  End;
  If (W = 'intet') And (Not DaNextNounish(I + 1)) Then
  Begin
    ST[I].Cat := 'g';
    ST[I].En := 'anything';
    Exit;
  End;
  If (W = 'ingen') And (Not DaNextNounish(I + 1)) Then
  Begin
    ST[I].Cat := 'g';
    ST[I].En := 'anyone';
    Exit;
  End;
  { pronoms sujets }
  If InList(W, 'jeg du han hun vi') Or ((W = 'de') And (Not DaNextNounish(I +
      1))) Then
  Begin
    ST[I].Cat := 'P';
    If W = 'jeg' Then Begin ST[I].En := 'I'; ST[I].Slot := 1; End
    Else If W = 'du' Then Begin ST[I].En := 'you'; ST[I].Slot := 2; End
    Else If W = 'han' Then Begin ST[I].En := 'he'; ST[I].Slot := 3; End
    Else If W = 'hun' Then Begin ST[I].En := 'she'; ST[I].Slot := 3; End
    Else If W = 'vi' Then Begin ST[I].En := 'we'; ST[I].Slot := 4; End
    Else Begin ST[I].En := 'they'; ST[I].Slot := 6; End;
    Exit;
  End;
  { den / det / de : article defini devant adjectif ou nom defini }
  If InList(W, 'den det de') Then
  Begin
    If DaNextNounish(I + 1) Then
    Begin
      ST[I].Cat := 'D';
      ST[I].Kind := 'd';
      ST[I].Plu := W = 'de';
    End
    Else
    Begin
      ST[I].Cat := 'P';
      ST[I].En := 'it';
      ST[I].Slot := 3;
    End;
    Exit;
  End;
  If (W = 'man') And (Prev <> 'D') And (Prev <> 'A') And DaNextVerbal(I + 1)
      Then
  Begin
    ST[I].Cat := 'P';
    ST[I].En := 'one';
    ST[I].Slot := 3;
    Exit;
  End;
  { articles indefinis }
  If InList(W, 'en et') Then
  Begin
    ST[I].Cat := 'D';
    ST[I].Kind := 'i';
    Exit;
  End;
  { possessifs }
  If InList(W,
      'min mit mine din dit dine hans hendes dens dets vores deres sin sit sine'
      ) Or
     (InList(W, 'jeres') And DaNextNounish(I + 1)) Then
  Begin
    ST[I].Cat := 'S';
    If InList(W, 'min mit mine') Then ST[I].En := 'my'
    Else If InList(W, 'din dit dine jeres') Then ST[I].En := 'your'
    Else If W = 'hans' Then ST[I].En := 'his'
    Else If W = 'hendes' Then ST[I].En := 'her'
    Else If InList(W, 'dens dets') Then ST[I].En := 'its'
    Else If W = 'vores' Then ST[I].En := 'our'
    Else If W = 'deres' Then ST[I].En := 'their'
    Else ST[I].En := 'his';
    ST[I].Plu := InList(W, 'mine dine jeres sine');
    Exit;
  End;
  { demonstratifs }
  If InList(W, 'denne dette disse') Then
  Begin
    ST[I].Cat := 'T';
    If W = 'disse' Then Begin ST[I].En := 'these'; ST[I].Plu := True; End
    Else ST[I].En := 'this';
    Exit;
  End;
  { quantificateurs }
  If InList(W, 'alle alt hver nogle noget mange flere begge ingen') Or
     (InList(W, 'meget') And DaNextNounish(I + 1) And
      (DaFindNoun(ST[I + 1].W, Plu, Def, Gen) > 0)) Or
     (InList(W, 'ingen intet') And DaNextNounish(I + 1)) Then
  Begin
    ST[I].Cat := 'Q';
    If InList(W, 'alle alt') Then ST[I].En := 'all'
    Else If W = 'hver' Then ST[I].En := 'each'
    Else If InList(W, 'nogle noget') Then ST[I].En := 'some'
    Else If W = 'mange' Then ST[I].En := 'many'
    Else If W = 'meget' Then ST[I].En := 'much'
    Else If W = 'flere' Then ST[I].En := 'several'
    Else If W = 'begge' Then ST[I].En := 'both'
    Else ST[I].En := 'no';
    ST[I].Plu := InList(W, 'alle nogle mange flere begge ingen');
    Exit;
  End;
  { pronoms complements }
  If InList(W, 'mig dig ham hende os jer dem sig') Then
  Begin
    ST[I].Cat := 'O';
    If W = 'mig' Then ST[I].En := 'me'
    Else If W = 'dig' Then ST[I].En := 'you'
    Else If W = 'ham' Then ST[I].En := 'him'
    Else If W = 'hende' Then ST[I].En := 'her'
    Else If W = 'os' Then ST[I].En := 'us'
    Else If W = 'jer' Then ST[I].En := 'you'
    Else If W = 'dem' Then ST[I].En := 'them'
    Else ST[I].En := '';
    Exit;
  End;
  { mots interrogatifs }
  Idx := DaFind(W, 'w');
  If (Idx > 0) And (QStart Or ((SEnd = '?') And InList(W,
      'hvem hvorfor hvordan hvad hvilken hvilket hvilke hvor mange hvor meget'
      ))) Then
  Begin
    ST[I].Cat := 'W';
    ST[I].En := DaLex[Idx].En;
    If W = 'hvor' Then ST[I].W := 'donde'
    Else If W = 'hvem' Then ST[I].W := 'quien'
    Else If Copy(W, 1, 5) = 'hvilk' Then ST[I].W := 'cual'
    Else If W = 'hvor mange' Then ST[I].W := 'cuantos'
    Else If W = 'hvor meget' Then ST[I].W := 'cuantos'
    Else If W = 'hvornår' Then ST[I].W := 'cuando'
    Else If W = 'hvordan' Then ST[I].W := 'como'
    Else If W = 'hvorfor' Then ST[I].W := 'por que'
    Else ST[I].W := 'que';
    Exit;
  End;
  { prepositions, conjonctions, interjections, adverbes }
  Idx := DaFind(W, 'p');
  If (W = 'ved') And (Prev = 'P') Then Idx := 0;
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'E';
    ST[I].En := DaLex[Idx].En;
    If W = 'til' Then ST[I].W := 'a'
    Else If W = 'af' Then ST[I].W := 'de'
    Else If (W = 'i') Or (W = 'på') Or (W = 'ved') Then ST[I].W := 'en';
    Exit;
  End;
  Idx := DaFind(W, 'c');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'C';
    ST[I].En := DaLex[Idx].En;
    Exit;
  End;
  Idx := DaFind(W, 'j');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'J';
    ST[I].En := DaLex[Idx].En;
    Exit;
  End;
  Idx := DaFind(W, 'r');
  If (Idx > 0) And Not ((W = 'godt') And ((Prev = 'V') Or DaNextNounish(I + 1)))
      Then
  Begin
    ST[I].Cat := 'R';
    ST[I].En := DaLex[Idx].En;
    Exit;
  End;
  { noms, adjectifs, verbes }
  LN := DaFindNoun(W, Plu, Def, Gen);
  IsNoun := LN > 0;
  If IsNoun Then
  Begin
    ST[I].Plu := Plu;
    ST[I].Cl := '';
    If Def Then ST[I].Cl := 'def';
    If Gen Then ST[I].Cl := ST[I].Cl + 'gen';
  End;
  LA := DaFindAdj(W, Plu);
  IsAdj := LA > 0;
  IsVerb := DaMatchVerb(W, Idx, T);
  If IsVerb And (IsNoun Or IsAdj) And (Prev In ['D', 'S', 'T', 'Q', 'U']) Then
      IsVerb := False;
  If IsVerb And (T = 'P') And IsAdj Then IsVerb := False;
  If IsVerb And IsNoun And (Prev = 'V') And (T In ['p', 'i']) Then IsVerb :=
      False;
  If IsVerb And (Not (T In ['p', 'i', 'M'])) And (IsNoun Or IsAdj) And (Prev <>
      'V') And (I > 1) Then IsVerb := False;
  If IsVerb Then
  Begin
    ST[I].Cat := 'V';
    ST[I].Idx := DaLex[Idx].SpIdx;
    ST[I].Tns := T;
    ST[I].Slot := 3;
    ST[I].En := DaLex[Idx].En;
    Exit;
  End;
  If IsNoun And IsAdj Then
  Begin
    If Prev In ['D', 'S', 'T', 'Q', 'U', 'A'] Then IsAdj := False Else IsNoun :=
        False;
  End;
  If IsNoun Then
  Begin
    ST[I].Cat := 'N';
    ST[I].En := DaLex[LN].En;
    Exit;
  End;
  If IsAdj Then
  Begin
    DaFindAdj(W, Plu);
    ST[I].Cat := 'A';
    ST[I].Plu := Plu;
    ST[I].En := DaLex[LA].En;
    Exit;
  End;
  ST[I].Cat := 'Z';
End;

{ ------------------------------------------------------------------ }
{ DANOIS -> ANGLAIS : lissage et phrase complete                     }
{ ------------------------------------------------------------------ }

Function DaVerbKey(I: Integer): String;
Begin
  If (I >= 1) And (I <= SN) And (ST[I].Cat = 'V') And (ST[I].Idx > 0) Then
    DaVerbKey := SpPlain(SpLex[ST[I].Idx].Es)
  Else
    DaVerbKey := '';
End;

Procedure DaInsert(Pos1: Integer);
Var
  K: Integer;
Begin
  If SN >= MaxTokens Then Exit;
  For K := SN Downto Pos1 Do
    ST[K + 1] := ST[K];
  Inc(SN);
  FillChar(ST[Pos1], SizeOf(TSTok), 0);
  ST[Pos1].Tns := ' ';
End;

{ Fin du groupe nominal sujet qui commence en K (0 s'il n'y en a pas). }
Function DaSubjEnd(K: Integer): Integer;
Var
  J: Integer;
Begin
  DaSubjEnd := 0;
  If (K < 1) Or (K > SN) Then Exit;
  If ST[K].Cat = 'P' Then
  Begin
    DaSubjEnd := K;
    Exit;
  End;
  J := K;
  While (J <= SN) And (ST[J].Cat In ['D', 'S', 'T', 'Q', 'U']) Do Inc(J);
  While (J <= SN) And (ST[J].Cat = 'A') Do Inc(J);
  If (J <= SN) And (ST[J].Cat = 'N') Then
    DaSubjEnd := J
  Else If (J <= SN) And (ST[J].Cat = 'Z') And ST[J].Cap And (J > 1) Then
  Begin
    While (J < SN) And (ST[J + 1].Cat = 'Z') Do Inc(J);
    DaSubjEnd := J;
  End;
End;

{ Vrai si le jeton I-1 termine un groupe nominal sujet (et non un complement). }
Function DaSubjBefore(I: Integer): Boolean;
Var
  J: Integer;
Begin
  DaSubjBefore := False;
  If I <= 1 Then Exit;
  If ST[I - 1].Cat = 'P' Then
  Begin
    DaSubjBefore := True;
    Exit;
  End;
  J := I - 1;
  While (J >= 1) And (ST[J].Cat In ['N', 'A', 'D', 'S', 'T', 'Q', 'U', 'Z']) Do
      Dec(J);
  If J < I - 1 Then
    DaSubjBefore := (J < 1) Or (ST[J].Cat <> 'E');
End;

Procedure DaMoveBlock(From1, To1, Dest: Integer);
{ deplace les jetons From1..To1 juste avant la position Dest (Dest < From1) }
Var
  Tmp: Array[1..12] Of TSTok;
  N, K: Integer;
Begin
  N := To1 - From1 + 1;
  If (N < 1) Or (N > 12) Or (Dest >= From1) Then Exit;
  For K := 1 To N Do Tmp[K] := ST[From1 + K - 1];
  For K := From1 - 1 Downto Dest Do ST[K + N] := ST[K];
  For K := 1 To N Do ST[Dest + K - 1] := Tmp[K];
End;

Function DaFirstW: String;
Var
  K: Integer;
Begin
  DaFirstW := '';
  For K := 1 To SN Do
    If ST[K].Cat = 'W' Then
    Begin
      DaFirstW := ST[K].W;
      Exit;
    End;
End;

Procedure DaClassify;
Var
  I, J, K, E: Integer;
  Key, Owner: String;
  Fin: Boolean;
Begin
  DaMergeMultiword;
  For I := 1 To SN Do
    DaPickCat(I);
  For I := 1 To SN Do
    If ST[I].Cat = 'Z' Then ST[I].Orig := DaProperEn(ST[I].Orig);
  { travailler / etre "pa" une banque, une ecole... : at }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'E') And (ST[I].W = 'en') And ((ST[I].En = 'on') Or
        (ST[I].En = 'in')) Then
    Begin
      K := I + 1;
      While (K < SN) And (ST[K].Cat In ['D', 'A', 'S']) Do Inc(K);
      If (ST[K].Cat = 'N') And InList(ST[K].En,
          'bank office school university hospital restaurant shop market stati'+
          'on airport library') Then
        ST[I].En := 'at';
    End;
  { gik i skole : aller a l'ecole }
  For I := 2 To SN - 1 Do
    If (ST[I].Cat = 'E') And (ST[I].W = 'en') And ((ST[I].En = 'in') Or
        (ST[I].En = 'at')) And (ST[I + 1].Cat = 'N') And
       (ST[I + 1].En = 'school') And (ST[I - 1].Cat = 'V') And (ST[I - 1].En =
           'go') Then
      ST[I].En := 'to';
  { nom propre au genitif : Annas bil -> la voiture d'Anna }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'Z') And ST[I].Cap And (Length(ST[I].Orig) > 3) And
       (ST[I].Orig[Length(ST[I].Orig)] = 's') And
       ((ST[I + 1].Cat = 'N') Or ((ST[I + 1].Cat = 'A') And (I + 2 <= SN) And
           (ST[I + 2].Cat = 'N'))) Then
    Begin
      ST[I].Orig := Copy(ST[I].Orig, 1, Length(ST[I].Orig) - 1);
      ST[I].Cl := 'gen';
    End;
  { genitif : on place le possede devant, puis "de" et le possesseur }
  I := 1;
  While I < SN Do
  Begin
    If (ST[I].Cl <> '') And (Pos('gen', ST[I].Cl) > 0) And (ST[I].Cat In ['N',
        'Z']) Then
    Begin
      K := I + 1;
      While (K <= SN) And (ST[K].Cat = 'A') Do Inc(K);
      If (K <= SN) And (ST[K].Cat = 'N') And (K - I <= 6) And (SN + 2 <=
          MaxTokens) Then
      Begin
        If ST[I].Cat = 'N' Then
        Begin
          Owner := ST[I].En;
          If ST[I].Plu Then Owner := EnPlural(Owner);
          If Pos('def', ST[I].Cl) > 0 Then Owner := 'the ' + Owner;
          ST[I].Orig := Owner;
          ST[I].Cat := 'Z';
          If (I > 1) And (ST[I - 1].Cat = 'D') And (ST[I - 1].Kind = 'd') Then
              ST[I - 1].Cat := ' ';
        End;
        ST[I].Cl := '';
        ST[I].Cap := True;
        { ordre : possede, "de", possesseur }
        ST[0] := ST[I];
        For J := I To K - 1 Do
          ST[J] := ST[J + 1];
        FillChar(ST[K], SizeOf(TSTok), 0);
        ST[K].Tns := ' ';
        ST[K].Cat := 'E';
        ST[K].W := 'de';
        ST[K].Orig := 'de';
        ST[K].En := 'of';
        DaInsert(K + 1);
        Owner := ST[0].Orig;
        ST[K + 1] := ST[0];
        ST[K + 1].Orig := Owner;
      End;
    End;
    Inc(I);
  End;
  { articles definis portes par le suffixe du nom }
  I := 1;
  While I <= SN Do
  Begin
    If (ST[I].Cat = 'N') And (Pos('def', ST[I].Cl) > 0) Then
    Begin
      K := I;
      While (K > 1) And (ST[K - 1].Cat = 'A') Do Dec(K);
      If (K = 1) Or Not (ST[K - 1].Cat In ['D', 'S', 'T', 'Q', 'U']) Then
      Begin
        DaInsert(K);
        ST[K].Cat := 'D';
        ST[K].Kind := 'd';
        ST[K].Plu := ST[I + 1].Plu;
        ST[K].W := 'el';
        ST[K].Orig := 'el';
        Inc(I);
      End;
    End;
    Inc(I);
  End;
  { adjectifs devant un nom sans determinant : nom puis adjectifs }
  I := 1;
  While I < SN Do
  Begin
    If (ST[I].Cat = 'A') And ((I = 1) Or Not (ST[I - 1].Cat In ['D', 'S', 'T',
        'Q', 'U', 'A'])) Then
    Begin
      K := I;
      While (K < SN) And (ST[K].Cat = 'A') Do Inc(K);
      If (ST[K].Cat = 'N') And (K - I <= 3) Then
      Begin
        ST[0] := ST[K];
        For J := K Downto I + 1 Do
          ST[J] := ST[J - 1];
        ST[I] := ST[0];
        I := K;
      End
      Else
        I := K;
    End;
    Inc(I);
  End;
  { att + infinitif }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'C') And (ST[I].W = 'at') And (ST[I + 1].Cat = 'V') And
        (ST[I + 1].Tns = 'I') Then
    Begin
      ST[I].Cat := ' ';
    End;
  { der er + groupe nominal indefini : il y a }
  For I := 1 To SN - 2 Do
  Begin
    If (ST[I].Cat = 'R') And (ST[I].W = 'der') And (ST[I + 1].Cat = 'V') And
       (DaVerbKey(I + 1) = 'ser') And (ST[I + 1].Tns In ['p', 'i']) And
       (ST[I + 2].Cat In ['D', 'N', 'Q', 'U']) And (ST[I + 2].Kind <> 'd') Then
    Begin
      ST[I].Cat := ' ';
      ST[I + 1].Idx := AlSpIndex('haber', 'have');
    End
    Else If (ST[I].Cat = 'V') And (DaVerbKey(I) = 'ser') And (ST[I].Tns In ['p',
        'i']) And
            (ST[I + 1].Cat = 'R') And (ST[I + 1].W = 'der') And
            (ST[I + 2].Cat In ['D', 'N', 'Q', 'U']) And (ST[I + 2].Kind <> 'd')
                Then
    Begin
      ST[I + 1].Cat := ' ';
      ST[I].Idx := AlSpIndex('haber', 'have');
    End;
  End;
  { inversion du verbe en seconde position : Idag gar jag -> Idag jag gar }
  I := 1;
  While I < SN Do
  Begin
    If (ST[I].Cat = 'V') And (ST[I].Tns In ['p', 'i']) And (Not DaSubjBefore(I))
        Then
    Begin
      E := DaSubjEnd(I + 1);
      If (E > 0) And (E - I <= 8) Then
        If (DaVerbKey(I) <> 'haber') Or (ST[I + 1].Cat = 'P') Then
        Begin
          DaMoveBlock(I + 1, E, I);
          I := E;
        End;
    End;
    Inc(I);
  End;
  { det finns / det regnar / heter }
  For I := 1 To SN Do
    If ST[I].Cat = 'V' Then
    Begin
      Key := DaVerbKey(I);
      If Key = 'haber' Then
      Begin
        ST[I].W := 'hay';
        If (I > 1) And (ST[I - 1].Cat = 'R') And (ST[I - 1].W = 'der') Then ST[I
            - 1].Cat := ' ';
        If (I < SN) And (ST[I + 1].Cat = 'R') And (ST[I + 1].W = 'der') Then
            ST[I + 1].Cat := ' ';
        ST[I].Slot := 3;
        If (I > 1) And (ST[I - 1].Cat = 'P') And (ST[I - 1].W = 'det') Then ST[I
            - 1].Cat := ' ';
      End
      Else If Key = 'llover' Then
      Begin
        ST[I].Kind := 'g';
        If (I > 1) And (ST[I - 1].Cat = 'P') And (ST[I - 1].W = 'det') Then ST[I
            - 1].Cat := ' ';
      End
      Else If Key = 'llamar' Then
        ST[I].Kind := 'r';
    End;
  { ha + supin : temps compose }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'V') And (DaVerbKey(I) = 'tener') And (ST[I].Tns In ['p',
        'i']) Then
    Begin
      J := I + 1;
      While (J <= SN) And (ST[J].Cat In ['R', 'G', 'P']) Do Inc(J);
      If (J <= SN) And (ST[J].Cat = 'V') And (ST[J].Tns = 'P') Then
      Begin
        ST[I].Idx := AlSpIndex('haber', 'have');
        ST[I].En := 'have';
      End;
    End;
  { negation : inte apres le verbe -> avant le verbe }
  For I := 2 To SN Do
    If (ST[I].Cat = 'G') And (ST[I - 1].Cat = 'V') And (ST[I - 1].Tns In ['p',
        'i', 'M']) Then
    Begin
      Key := '';
      ST[0] := ST[I];
      ST[I] := ST[I - 1];
      ST[I - 1] := ST[0];
    End;
  { impératif en tete de phrase }
  If (SN >= 1) And (ST[1].Cat = 'V') And (ST[1].Tns In ['p', 'i', 'I', 'M']) And
      (SEnd <> '?') And
     (DaVerbKey(1) <> 'haber') Then
  Begin
    ST[1].Tns := 'M';
    ST[1].Slot := 3;
  End;
  { personne du verbe d'apres le sujet qui precede }
  For I := 1 To SN Do
    If (ST[I].Cat = 'V') And (ST[I].Tns In ['p', 'i']) Then
    Begin
      ST[I].Slot := 3;
      J := I - 1;
      While (J >= 1) And (ST[J].Cat In ['R', 'G']) Do Dec(J);
      If (J >= 1) And (ST[J].Cat = 'P') Then
        ST[I].Slot := ST[J].Slot
      Else If (J >= 1) And (ST[J].Cat In ['N', 'Z']) Then
      Begin
        If ST[J].Plu And (ST[J].Cat = 'N') Then ST[I].Slot := 6;
        K := J;
        While (K > 1) And (ST[K - 1].Cat In ['D', 'S', 'T', 'Q', 'U', 'A']) Do
            Dec(K);
        If (K > 2) And (ST[K - 1].Cat = 'C') And (ST[K - 1].En = 'and') Then
            ST[I].Slot := 6;
      End;
    End;
  { tage til = aller ; hvad laver du = que fais-tu ; hvordan har du det }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'V') And (ST[I].Idx > 0) Then
    Begin
      If InList(ST[I].W, 'tager tog tage') And (ST[I + 1].Cat = 'E') And (ST[I +
          1].W = 'a') Then
      Begin
        ST[I].Idx := AlSpIndex('ir', 'go');
        ST[I].En := 'go';
      End
      Else If InList(ST[I].W, 'laver lavede') And (DaFirstW = 'que') Then
      Begin
        ST[I].Idx := AlSpIndex('hacer', 'do');
        ST[I].En := 'do';
      End
      Else If (DaVerbKey(I) = 'tener') And (DaFirstW = 'como') And
              (I < SN) And (ST[SN].Cat = 'P') And (ST[SN].W = 'det') Then
      Begin
        ST[I].Idx := AlSpIndex('estar', 'be');
        ST[I].En := 'be';
        ST[SN].Cat := ' ';
      End;
    End;
  { hvor ligger X ? = ou est X ? }
  For I := 1 To SN Do
    If (ST[I].Cat = 'V') And (ST[I].Idx > 0) Then
    Begin
      If False Then
      Begin
      End
      Else If InList(ST[I].W, 'ligger lå står stod') And (ST[1].Cat = 'W') And
          (ST[1].W = 'donde') Then
      Begin
        ST[I].Idx := AlSpIndex('estar', 'be');
        ST[I].En := 'be';
      End;
    End;
  { ska + infinitif : aller + a + infinitif }
  I := 1;
  While I < SN Do
  Begin
    If (ST[I].Cat = 'V') And (DaVerbKey(I) = 'ir') And InList(ST[I].W,
        'skal skulle') And
       (ST[I + 1].Cat = 'V') And (ST[I + 1].Tns = 'I') And (SN < MaxTokens) Then
    Begin
      DaInsert(I + 1);
      ST[I + 1].Cat := 'E';
      ST[I + 1].W := 'a';
      ST[I + 1].Orig := 'a';
      ST[I + 1].En := 'to';
      Inc(I);
    End;
    Inc(I);
  End;
  { lang d'une personne : grand }
  For I := 3 To SN Do
    If (ST[I].Cat = 'A') And (ST[I].En = 'long') And (ST[I - 1].Cat = 'V') And
       ((ST[I - 2].Cat = 'P') Or ((ST[I - 2].Cat = 'N') And SpIsPersonEn(ST[I -
           2].En))) Then
      ST[I].En := 'tall';
  { complement d'une preposition : pronom de groupe nominal }
  For I := 2 To SN Do
    If (ST[I].Cat = 'O') And (ST[I - 1].Cat = 'E') And (ST[I].En <> '') Then
      ST[I].Cat := 'B';
  { quantificateur pluriel : le nom est au pluriel }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'Q') And ST[I].Plu Then
    Begin
      K := I + 1;
      While (K < SN) And (ST[K].Cat = 'A') Do Inc(K);
      If ST[K].Cat = 'N' Then ST[K].Plu := True;
    End;
  { pronoms complements apres le verbe : enclitiques }
  For I := 2 To SN Do
    If (ST[I].Cat = 'O') And (ST[I - 1].Cat = 'V') And (ST[I].En <> '') Then
    Begin
      ST[I - 1].Cl := ST[I].En;
      ST[I].Cat := ' ';
    End;
  ARemoveConsumed;
End;

Function DaTranslateSentence(Const Sentence: String): String;
Var
  Res: String;
Begin
  DaTokenize(Sentence);
  If SN = 0 Then
  Begin
    DaTranslateSentence := '';
    Exit;
  End;
  DaClassify;
  If SN = 0 Then
  Begin
    DaTranslateSentence := '';
    Exit;
  End;
  Res := TranslateTokensEs(1, SN, SEnd = '?');
  { Vad heter du ? -> What is your name ? }
  If (SEnd = '?') And (Length(Res) > 8) And (Copy(Res, Length(Res) - 7, 8) =
      ' name is') Then
    Res := 'What is ' + LowerAscii(Copy(Res, 1, Length(Res) - 8)) + ' name';
  Res := UpperFirst(FixEnglish(Res));
  If SEnd <> #0 Then Res := Res + SEnd;
  DaTranslateSentence := Res;
End;

{ ------------------------------------------------------------------ }
{ ITALIEN -> ANGLAIS : lexique et morphologie                         }
{ ------------------------------------------------------------------ }
{ Comme pour le portugais, les jetons produits sont ceux de l'espagnol   }
{ (ST) : la meme analyse de proposition sert ensuite.                    }

Type
  TIt = Record
    Orig: String[26];
    F1, F2, F3, F4: String[26];
    En: String[28];
    Pos: String[3];
    Key: String[10];
    SpIdx: Integer;
  End;

  TItIrr = Record
    Inf: String[10];
    Pres: Array[1..6] Of String[12];
    Imp: String[60];
    Fut, Part, Ger: String[12];
  End;

Const
  ItIrrData: Array[0..19] Of String = (
    'essere|sono,sei,è,siamo,siete,sono|ero,eri,era,eravamo,eravate,erano|sar'+
        '|stato|essendo',
    'avere|ho,hai,ha,abbiamo,avete,hanno|-|avr|avuto|avendo',
    'andare|vado,vai,va,andiamo,andate,vanno|-|andr|andato|andando',
    'fare|faccio,fai,fa,facciamo,fate,fanno|fac|far|fatto|facendo',
    'dire|dico,dici,dice,diciamo,dite,dicono|dic|dir|detto|dicendo',
    'venire|vengo,vieni,viene,veniamo,venite,vengono|-|verr|venuto|venendo',
    'potere|posso,puoi,può,possiamo,potete,possono|-|potr|potuto|potendo',
    'volere|voglio,vuoi,vuole,vogliamo,volete,vogliono|-|vorr|voluto|volendo',
    'dovere|devo,devi,deve,dobbiamo,dovete,devono|-|dovr|dovuto|dovendo',
    'sapere|so,sai,sa,sappiamo,sapete,sanno|-|sapr|saputo|sapendo',
    'dare|do,dai,dà,diamo,date,danno|-|dar|dato|dando',
    'stare|sto,stai,sta,stiamo,state,stanno|-|star|stato|stando',
    'uscire|esco,esci,esce,usciamo,uscite,escono|-|-|uscito|uscendo',
    'bere|bevo,bevi,beve,beviamo,bevete,bevono|bev|berr|bevuto|bevendo',
    'rimanere|rimango,rimani,rimane,rimaniamo,rimanete,rimangono|-|rimarr|rima'+
        'sto|rimanendo',
    'tenere|tengo,tieni,tiene,teniamo,tenete,tengono|-|terr|tenuto|tenendo',
    'scegliere|scelgo,scegli,sceglie,scegliamo,scegliete,scelgono|-|-|scelto|s'+
        'cegliendo',
    'tradurre|traduco,traduci,traduce,traduciamo,traducete,traducono|traduc|tr'+
        'adurr|tradotto|traducendo',
    'morire|muoio,muori,muore,moriamo,morite,muoiono|-|morr|morto|morendo',
    'piacere|piaccio,piaci,piace,piacciamo,piacete,piacciono|-|piacer|piaciuto'+
        '|piacendo'
  );

Var
  ItLex: Array[1..900] Of TIt;
  ItCount: Integer;
  ItIrr: Array[1..24] Of TItIrr;
  ItIrrCount: Integer;
  SourceIt: Boolean;

Procedure LoadItalian;
Var
  I, K: Integer;
  Line, Item, W, Rest, P, Part: String;
Begin
  ItCount := 0;
  For I := 0 To High(ItData) Do
  Begin
    Line := ItData[I];
    While Line <> '' Do
    Begin
      Item := CopyUntil(Line, ';');
      If (Item <> '') And (ItCount < 900) Then
      Begin
        W := CopyUntil(Item, '=');
        Rest := CopyUntil(Item, ':');
        Inc(ItCount);
        FillChar(ItLex[ItCount], SizeOf(TIt), 0);
        P := CopyUntil(W, '/');
        ItLex[ItCount].Orig := ReplaceChar(P, '_', ' ');
        ItLex[ItCount].F1 := ItLex[ItCount].Orig;
        If W <> '' Then ItLex[ItCount].F2 := CopyUntil(W, '/');
        If W <> '' Then ItLex[ItCount].F3 := CopyUntil(W, '/');
        If W <> '' Then ItLex[ItCount].F4 := W;
        ItLex[ItCount].En := ReplaceChar(Rest, '_', ' ');
        ItLex[ItCount].Pos := CopyUntil(Item, ':');
        ItLex[ItCount].Key := Item;
        If Copy(ItLex[ItCount].Pos, 1, 1) = 'v' Then
        Begin
          If ItLex[ItCount].Key <> '' Then
            ItLex[ItCount].SpIdx := AlSpIndex(ItLex[ItCount].Key,
                ItLex[ItCount].En)
          Else
            ItLex[ItCount].SpIdx := AlSpIndex('it:' + ItLex[ItCount].F1,
                ItLex[ItCount].En);
        End;
      End;
    End;
  End;
  ItIrrCount := 0;
  For I := 0 To High(ItIrrData) Do
    If ItIrrCount < 24 Then
    Begin
      Line := ItIrrData[I];
      Inc(ItIrrCount);
      ItIrr[ItIrrCount].Inf := CopyUntil(Line, '|');
      Part := CopyUntil(Line, '|');
      For K := 1 To 6 Do ItIrr[ItIrrCount].Pres[K] := CopyUntil(Part, ',');
      ItIrr[ItIrrCount].Imp := CopyUntil(Line, '|');
      ItIrr[ItIrrCount].Fut := CopyUntil(Line, '|');
      ItIrr[ItIrrCount].Part := CopyUntil(Line, '|');
      ItIrr[ItIrrCount].Ger := Line;
    End;
End;

Function ItIsVowel(C: Char): Boolean;
Begin
  ItIsVowel := C In ['a', 'e', 'i', 'o', 'u'];
End;

Function ItPlural(Const S: String): String;
Var
  L: Integer;
Begin
  L := Length(S);
  If L < 2 Then ItPlural := S
  Else If EndsWith(S, 'io') Then ItPlural := Copy(S, 1, L - 1)
  Else If EndsWith(S, 'ca') Then ItPlural := Copy(S, 1, L - 2) + 'che'
  Else If EndsWith(S, 'ga') Then ItPlural := Copy(S, 1, L - 2) + 'ghe'
  Else If EndsWith(S, 'co') Then ItPlural := Copy(S, 1, L - 2) + 'chi'
  Else If EndsWith(S, 'go') Then ItPlural := Copy(S, 1, L - 2) + 'ghi'
  Else If S[L] = 'a' Then ItPlural := Copy(S, 1, L - 1) + 'e'
  Else If (S[L] = 'o') Or (S[L] = 'e') Then ItPlural := Copy(S, 1, L - 1) + 'i'
  Else ItPlural := S;
End;

Function ItFindNoun(Const W: String; Var Plu: Boolean): Integer;
Var
  I: Integer;
  P: String;
Begin
  ItFindNoun := 0;
  For I := 1 To ItCount Do
    If ItLex[I].Pos = 'n' Then
    Begin
      Plu := False;
      If W = ItLex[I].F1 Then
      Begin
        ItFindNoun := I;
        Exit;
      End;
      Plu := True;
      If ItLex[I].F2 <> '' Then P := ItLex[I].F2 Else P :=
          ItPlural(ItLex[I].F1);
      If W = P Then
      Begin
        ItFindNoun := I;
        Exit;
      End;
    End;
  Plu := False;
End;

Function ItFindAdj(Const W: String; Var Plu: Boolean): Integer;
Var
  I, L: Integer;
  B, Fem, PM, PF: String;
Begin
  ItFindAdj := 0;
  For I := 1 To ItCount Do
    If ItLex[I].Pos = 'a' Then
    Begin
      B := ItLex[I].F1;
      L := Length(B);
      If ItLex[I].F2 <> '' Then
      Begin
        Fem := ItLex[I].F2;
        PM := ItLex[I].F3;
        PF := ItLex[I].F4;
      End
      Else
      Begin
        If EndsWith(B, 'o') Then Fem := Copy(B, 1, L - 1) + 'a' Else Fem := B;
        PM := ItPlural(B);
        PF := ItPlural(Fem);
        If EndsWith(B, 'co') Then PM := Copy(B, 1, L - 2) + 'chi';
      End;
      Plu := False;
      If (W = B) Or (W = Fem) Then
      Begin
        ItFindAdj := I;
        Exit;
      End;
      Plu := True;
      If (W = PM) Or (W = PF) Then
      Begin
        ItFindAdj := I;
        Exit;
      End;
    End;
  Plu := False;
End;

Function ItFind(Const W: String; Const Kinds: String): Integer;
Var
  I: Integer;
Begin
  ItFind := 0;
  For I := 1 To ItCount Do
    If (ItLex[I].F1 = W) And (Pos(ItLex[I].Pos, Kinds) > 0) Then
    Begin
      ItFind := I;
      Exit;
    End;
End;

{ Ajoute une terminaison en gerant les i et les c/g des verbes en -are. }
Function ItJoin(Const Stem, Ending: String; Cls: String): String;
Begin
  If (Stem <> '') And (Ending <> '') And (Stem[Length(Stem)] = 'i') And
      (Ending[1] = 'i') Then
    ItJoin := Stem + Copy(Ending, 2, 20)
  Else If (Cls = 'are') And (Stem <> '') And (Stem[Length(Stem)] In ['c', 'g'])
      And
          (Ending <> '') And (Ending[1] In ['i', 'e']) Then
    ItJoin := Stem + 'h' + Ending
  Else
    ItJoin := Stem + Ending;
End;

{ Formes verbales : p present, m imparfait, f futur, c conditionnel, P
  participe,
  G gerondif, I infinitif }
Function ItMatchVerb(Const W: String; Var Idx: Integer; Var Tns: Char; Var Slot:
    Integer): Boolean;
Const
  PrAr: Array[1..6] Of String[6] = ('o', 'i', 'a', 'iamo', 'ate', 'ano');
  PrEr: Array[1..6] Of String[6] = ('o', 'i', 'e', 'iamo', 'ete', 'ono');
  PrIr: Array[1..6] Of String[6] = ('o', 'i', 'e', 'iamo', 'ite', 'ono');
  PrIs: Array[1..6] Of String[6] = ('isco', 'isci', 'isce', 'iamo', 'ite',
      'iscono');
  ImA: Array[1..6] Of String[8] = ('avo', 'avi', 'ava', 'avamo', 'avate',
      'avano');
  ImE: Array[1..6] Of String[8] = ('evo', 'evi', 'eva', 'evamo', 'evate',
      'evano');
  ImI: Array[1..6] Of String[8] = ('ivo', 'ivi', 'iva', 'ivamo', 'ivate',
      'ivano');
  FuE: Array[1..6] Of String[6] = ('ò', 'ai', 'à', 'emo', 'ete', 'anno');
  CoE: Array[1..6] Of String[8] = ('ei', 'esti', 'ebbe', 'emmo', 'este',
      'ebbero');
Var
  I, Q, K, Irr, L: Integer;
  Inf, Stem, Cls, Form, FS, Part, Rest: String;

  Function T(Const F: String; Tn: Char; Sl: Integer): Boolean;
  Begin
    T := False;
    If (F <> '') And (F = W) Then
    Begin
      Idx := I;
      Tns := Tn;
      Slot := Sl;
      T := True;
    End;
  End;

Begin
  ItMatchVerb := True;
  For I := 1 To ItCount Do
    If Copy(ItLex[I].Pos, 1, 1) = 'v' Then
    Begin
      Inf := ItLex[I].F1;
      L := Length(Inf);
      If L < 4 Then Continue;
      Cls := Copy(Inf, L - 2, 3);
      Stem := Copy(Inf, 1, L - 3);
      If (Cls = 'rre') Then Stem := Copy(Inf, 1, L - 3);
      Irr := 0;
      For K := 1 To ItIrrCount Do
        If ItIrr[K].Inf = Inf Then Irr := K;
      { infinitif, avec ou sans le e final }
      If T(Inf, 'I', 0) Then Exit;
      If T(Copy(Inf, 1, L - 1), 'I', 0) Then Exit;
      { present }
      For Q := 1 To 6 Do
      Begin
        If Irr > 0 Then Form := ItIrr[Irr].Pres[Q]
        Else If Cls = 'are' Then Form := ItJoin(Stem, PrAr[Q], Cls)
        Else If Cls = 'ere' Then Form := ItJoin(Stem, PrEr[Q], Cls)
        Else If ItLex[I].Pos = 'vi' Then Form := ItJoin(Stem, PrIs[Q], Cls)
        Else Form := ItJoin(Stem, PrIr[Q], Cls);
        If T(Form, 'p', Q) Then Exit;
      End;
      { imparfait }
      For Q := 1 To 6 Do
      Begin
        If Irr > 0 Then
        Begin
          Rest := ItIrr[Irr].Imp;
          If Rest = '-' Then
          Begin
            If Cls = 'are' Then Form := Stem + ImA[Q]
            Else If Cls = 'ire' Then Form := Stem + ImI[Q]
            Else Form := Stem + ImE[Q];
          End
          Else If Pos(',', Rest) > 0 Then
          Begin
            Part := Rest;
            For K := 1 To Q Do Form := CopyUntil(Part, ',');
          End
          Else Form := Rest + ImE[Q];
        End
        Else If Cls = 'are' Then Form := ItJoin(Stem, ImA[Q], Cls)
        Else If Cls = 'ere' Then Form := Stem + ImE[Q]
        Else Form := Stem + ImI[Q];
        If T(Form, 'm', Q) Then Exit;
      End;
      { futur et conditionnel }
      If Irr > 0 Then FS := ItIrr[Irr].Fut Else FS := '-';
      If (FS = '-') Then
      Begin
        If ItLex[I].F3 <> '' Then FS := ItLex[I].F3
        Else If Cls = 'are' Then
        Begin
          FS := Stem + 'er';
          If EndsWith(Stem, 'ci') Or EndsWith(Stem, 'gi') Then FS := Copy(Stem,
              1, Length(Stem) - 1) + 'er'
          Else If Stem[Length(Stem)] In ['c', 'g'] Then FS := Stem + 'her';
        End
        Else If Cls = 'ere' Then FS := Stem + 'er'
        Else FS := Stem + 'ir';
      End;
      For Q := 1 To 6 Do
        If T(FS + FuE[Q], 'f', Q) Then Exit;
      For Q := 1 To 6 Do
        If T(FS + CoE[Q], 'c', Q) Then Exit;
      { participe : quatre formes }
      If Irr > 0 Then Part := ItIrr[Irr].Part
      Else If ItLex[I].F2 <> '' Then Part := ItLex[I].F2
      Else If Cls = 'are' Then Part := Stem + 'ato'
      Else If Cls = 'ere' Then Part := Stem + 'uto'
      Else Part := Stem + 'ito';
      If EndsWith(Part, 'o') Then
      Begin
        Form := Copy(Part, 1, Length(Part) - 1);
        If T(Part, 'P', 0) Or T(Form + 'a', 'P', 0) Or T(Form + 'i', 'P', 0) Or
            T(Form + 'e', 'P', 0) Then Exit;
      End;
      { gerondif }
      If Irr > 0 Then Part := ItIrr[Irr].Ger
      Else If Cls = 'are' Then Part := ItJoin(Stem, 'ando', Cls)
      Else Part := Stem + 'endo';
      If T(Part, 'G', 0) Then Exit;
    End;
  ItMatchVerb := False;
End;

{ ------------------------------------------------------------------ }
{ ITALIEN -> ANGLAIS : jetons et etiquetage                           }
{ ------------------------------------------------------------------ }

Procedure ItTokenize(Const S0: String);
Var
  S, Word1, Low: String;
  I, L: Integer;
  C: Char;
  Cap: Boolean;
Begin
  SN := 0;
  SEnd := #0;
  S := '';
  I := 1;
  While I <= Length(S0) Do
  Begin
    If (S0[I] = #$C2) And (I < Length(S0)) And ((S0[I + 1] = #$AB) Or (S0[I + 1]
        = #$BB) Or (S0[I + 1] = #$A0) Or
       (S0[I + 1] = #$BF) Or (S0[I + 1] = #$A1)) Then
    Begin
      S := S + ' ';
      Inc(I, 2);
    End
    Else If (S0[I] = #$E2) And (I + 2 <= Length(S0)) And (S0[I + 1] = #$80) And
            ((S0[I + 2] = #$99) Or (S0[I + 2] = #$98)) Then
    Begin
      S := S + ' ';
      Inc(I, 3);
    End
    Else
    Begin
      S := S + S0[I];
      Inc(I);
    End;
  End;
  L := Length(S);
  I := 1;
  While I <= L Do
  Begin
    C := S[I];
    If FrWordByte(C) Then
    Begin
      Word1 := '';
      While (I <= L) And FrWordByte(S[I]) Do
      Begin
        Word1 := Word1 + S[I];
        Inc(I);
      End;
      Cap := (Word1 <> '') And (((Word1[1] >= 'A') And (Word1[1] <= 'Z')) Or
             ((Word1[1] = #$C3) And (Length(Word1) > 1) And (Word1[2] >= #$80)
                 And (Word1[2] <= #$9E)));
      Low := FrLower(Word1);
      SPush(Low, Word1, Cap, False, False);
    End
    Else
    Begin
      If (C = '.') Or (C = '?') Or (C = '!') Then
      Begin
        If SEnd = #0 Then SEnd := C;
        If C = '?' Then SEnd := '?';
      End
      Else If (C = ',') Or (C = ';') Or (C = ':') Then
      Begin
        SPush(',', ',', False, True, False);
        ST[SN].Cat := ',';
      End;
      Inc(I);
    End;
  End;
End;

Function ItNumberEn(Const W: String): String;
Const
  N: Array[0..28] Of String = ('zero', 'due', 'tre', 'quattro', 'cinque', 'sei',
      'sette', 'otto',
    'nove', 'dieci', 'undici', 'dodici', 'tredici', 'quattordici', 'quindici',
        'sedici',
    'diciassette', 'diciotto', 'diciannove', 'venti', 'trenta', 'quaranta',
        'cinquanta',
    'sessanta', 'settanta', 'ottanta', 'novanta', 'cento', 'mille');
  E: Array[0..28] Of String = ('zero', 'two', 'three', 'four', 'five', 'six',
      'seven', 'eight',
    'nine', 'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen',
        'sixteen',
    'seventeen', 'eighteen', 'nineteen', 'twenty', 'thirty', 'forty', 'fifty',
    'sixty', 'seventy', 'eighty', 'ninety', 'one hundred', 'one thousand');
Var
  K: Integer;
Begin
  ItNumberEn := '';
  For K := 0 To 28 Do
    If W = N[K] Then ItNumberEn := E[K];
End;

Function ItProperEn(Const Orig: String): String;
Var
  L: String;
Begin
  L := FrLower(Orig);
  If L = 'italia' Then ItProperEn := 'Italy'
  Else If L = 'francia' Then ItProperEn := 'France'
  Else If L = 'spagna' Then ItProperEn := 'Spain'
  Else If L = 'germania' Then ItProperEn := 'Germany'
  Else If L = 'inghilterra' Then ItProperEn := 'England'
  Else If L = 'svizzera' Then ItProperEn := 'Switzerland'
  Else If L = 'portogallo' Then ItProperEn := 'Portugal'
  Else If L = 'grecia' Then ItProperEn := 'Greece'
  Else If L = 'svezia' Then ItProperEn := 'Sweden'
  Else If L = 'danimarca' Then ItProperEn := 'Denmark'
  Else If L = 'norvegia' Then ItProperEn := 'Norway'
  Else If L = 'russia' Then ItProperEn := 'Russia'
  Else If L = 'giappone' Then ItProperEn := 'Japan'
  Else If L = 'cina' Then ItProperEn := 'China'
  Else If L = 'belgio' Then ItProperEn := 'Belgium'
  Else If L = 'brasile' Then ItProperEn := 'Brazil'
  Else If L = 'roma' Then ItProperEn := 'Rome'
  Else If L = 'milano' Then ItProperEn := 'Milan'
  Else If L = 'napoli' Then ItProperEn := 'Naples'
  Else If L = 'venezia' Then ItProperEn := 'Venice'
  Else If L = 'firenze' Then ItProperEn := 'Florence'
  Else If L = 'torino' Then ItProperEn := 'Turin'
  Else If L = 'genova' Then ItProperEn := 'Genoa'
  Else If L = 'parigi' Then ItProperEn := 'Paris'
  Else If L = 'londra' Then ItProperEn := 'London'
  Else If L = 'mosca' Then ItProperEn := 'Moscow'
  Else If L = 'vienna' Then ItProperEn := 'Vienna'
  Else ItProperEn := Orig;
End;

Function ItNextNounish(I: Integer): Boolean;
Var
  Plu: Boolean;
Begin
  ItNextNounish := False;
  If I > SN Then Exit;
  If ST[I].Punct Then Exit;
  ItNextNounish := (ItFindNoun(ST[I].W, Plu) > 0) Or (ItFindAdj(ST[I].W, Plu) >
      0) Or
                   InList(ST[I].W,
                       'il lo la i gli le l un uno una mio mia miei mie tuo tu'+
                       'a tuoi tue') Or
                   InList(ST[I].W,
                       'suo sua suoi sue nostro nostra nostri nostre vostro vo'+
                       'stra vostri vostre') Or
                   InList(ST[I].W,
                       'questo questa questi queste quello quella quelli quell'+
                       'e ogni tutto tutta') Or
                   InList(ST[I].W,
                       'tutti tutte alcuni alcune qualche molto molta molti mo'+
                       'lte poco poca pochi poche') Or
                   (ItNumberEn(ST[I].W) <> '');
End;

Function ItNextVerbal(I: Integer): Boolean;
Var
  Idx, Sl: Integer;
  T: Char;
Begin
  ItNextVerbal := False;
  If I > SN Then Exit;
  If ST[I].Punct Then Exit;
  If InList(ST[I].W, 'non mi ti ci vi si lo la li le gli ne') Then
  Begin
    ItNextVerbal := True;
    Exit;
  End;
  ItNextVerbal := ItMatchVerb(ST[I].W, Idx, T, Sl) And (T In ['p', 'm', 'f',
      'c']);
End;

{ Contractions : del, nel, sul... et articles elides (l', dell') }
Procedure ItExpandContractions;
Var
  I, K: Integer;
  E1, E2, W: String;
  T: TSTok;
Begin
  I := SN;
  While I >= 1 Do
  Begin
    E1 := '';
    E2 := '';
    W := ST[I].W;
    If Not ST[I].Punct Then
    Begin
      If (W = 'del') Or (W = 'dello') Or (W = 'della') Or (W = 'dell') Or (W =
          'dei') Or (W = 'degli') Or (W = 'delle') Then
        E1 := 'di'
      Else If (W = 'al') Or (W = 'allo') Or (W = 'alla') Or (W = 'all') Or (W =
          'ai') Or (W = 'agli') Or (W = 'alle') Then
        E1 := 'a'
      Else If (W = 'dal') Or (W = 'dallo') Or (W = 'dalla') Or (W = 'dall') Or
          (W = 'dai') Or (W = 'dagli') Or (W = 'dalle') Then
        E1 := 'da'
      Else If (W = 'nel') Or (W = 'nello') Or (W = 'nella') Or (W = 'nell') Or
          (W = 'nei') Or (W = 'negli') Or (W = 'nelle') Then
        E1 := 'in'
      Else If (W = 'sul') Or (W = 'sullo') Or (W = 'sulla') Or (W = 'sull') Or
          (W = 'sui') Or (W = 'sugli') Or (W = 'sulle') Then
        E1 := 'su'
      Else If (W = 'col') Or (W = 'coi') Then
        E1 := 'con';
      If E1 <> '' Then
      Begin
        If (W = 'del') Or (W = 'al') Or (W = 'dal') Or (W = 'nel') Or (W =
            'sul') Or (W = 'col') Then E2 := 'il'
        Else If (W = 'dello') Or (W = 'allo') Or (W = 'dallo') Or (W = 'nello')
            Or (W = 'sullo') Then E2 := 'lo'
        Else If (W = 'della') Or (W = 'alla') Or (W = 'dalla') Or (W = 'nella')
            Or (W = 'sulla') Then E2 := 'la'
        Else If (W = 'dell') Or (W = 'all') Or (W = 'dall') Or (W = 'nell') Or
            (W = 'sull') Then E2 := 'l'
        Else If (W = 'dei') Or (W = 'ai') Or (W = 'dai') Or (W = 'nei') Or (W =
            'sui') Or (W = 'coi') Then E2 := 'i'
        Else If (W = 'degli') Or (W = 'agli') Or (W = 'dagli') Or (W = 'negli')
            Or (W = 'sugli') Then E2 := 'gli'
        Else E2 := 'le';
      End;
    End;
    If (E1 <> '') And (SN < MaxTokens) Then
    Begin
      T := ST[I];
      For K := SN Downto I + 1 Do
        ST[K + 1] := ST[K];
      Inc(SN);
      ST[I] := T;
      ST[I].W := E1;
      ST[I].Orig := E1;
      ST[I].Kind := 'e';
      ST[I + 1] := T;
      ST[I + 1].W := E2;
      ST[I + 1].Orig := E2;
      ST[I + 1].Cap := False;
      ST[I + 1].Kind := 'p';
      { "di + article" : possible partitif, repere pour la suite }
      If E1 = 'di' Then ST[I + 1].Cl := 'part';
    End;
    Dec(I);
  End;
End;

Procedure ItMergeMultiword;
Var
  I, K, Len, L: Integer;
  Key: String;
  Bad, Found: Boolean;
Begin
  I := 1;
  While I < SN Do
  Begin
    Found := False;
    For Len := 3 Downto 2 Do
      If (Not Found) And (I + Len - 1 <= SN) Then
      Begin
        Key := '';
        Bad := False;
        For K := I To I + Len - 1 Do
        Begin
          If ST[K].Punct Then Bad := True;
          Key := Join(Key, ST[K].W);
        End;
        If Not Bad Then
        Begin
          L := ItFind(Key, 'rpcjw');
          If L > 0 Then
          Begin
            Found := True;
            ST[I].W := Key;
            ST[I].Orig := Key;
            For K := I + 1 To SN - Len + 1 Do
              ST[K] := ST[K + Len - 1];
            Dec(SN, Len - 1);
          End;
        End;
      End;
    Inc(I);
  End;
End;

{ Verbe a l'infinitif avec pronom enclitique : vederlo, parlarmi }
Function ItEnclitic(Const W: String; Var Idx: Integer; Var Clit: String):
    Boolean;
Var
  Base, Cl, Inf: String;
  L, K, Sl: Integer;
  T: Char;
Begin
  ItEnclitic := False;
  L := Length(W);
  If L < 6 Then Exit;
  Cl := Copy(W, L - 1, 2);
  If Not InList(Cl, 'lo la li le mi ti ci vi ne') Then Exit;
  Base := Copy(W, 1, L - 2);
  If Not EndsWith(Base, 're') Then Base := Base + 'e';
  If Not EndsWith(Base, 're') Then Exit;
  If ItMatchVerb(Base, K, T, Sl) And (T = 'I') Then
  Begin
    Idx := K;
    If Cl = 'lo' Then Clit := 'him'
    Else If Cl = 'la' Then Clit := 'her'
    Else If (Cl = 'li') Or (Cl = 'le') Then Clit := 'them'
    Else If Cl = 'mi' Then Clit := 'me'
    Else If Cl = 'ti' Then Clit := 'you'
    Else If Cl = 'ci' Then Clit := 'us'
    Else If Cl = 'vi' Then Clit := 'you'
    Else Clit := '';
    ItEnclitic := True;
  End;
  Inf := Base;
End;

Procedure ItPickCat(I: Integer);
Var
  W, Clit: String;
  Prev: Char;
  Idx, Sl: Integer;
  T: Char;
  Plu, QStart, IsVerb, IsNoun, IsAdj: Boolean;
  LN, LA: Integer;
Begin
  W := ST[I].W;
  If I > 1 Then Prev := ST[I - 1].Cat Else Prev := '.';
  QStart := (SEnd = '?') And ((I = 1) Or (ST[I - 1].Cat = ','));
  If ST[I].Punct Then
  Begin
    ST[I].Cat := ',';
    Exit;
  End;
  If IsDigitsOnly(W) Then
  Begin
    ST[I].Cat := 'U';
    ST[I].En := W;
    Exit;
  End;
  If (ItNumberEn(W) <> '') And ((W <> 'sei') Or (ItFindNoun(ST[I + 1].W, Plu) >
      0)) Then
  Begin
    ST[I].Cat := 'U';
    ST[I].En := ItNumberEn(W);
    If W = 'cento' Then ST[I].En := 'one hundred';
    Exit;
  End;
  If ST[I].Kind = 'e' Then
  Begin
    Idx := ItFind(W, 'p');
    ST[I].Cat := 'E';
    ST[I].Kind := ' ';
    If Idx > 0 Then ST[I].En := ItLex[Idx].En Else ST[I].En := W;
    If W = 'di' Then ST[I].W := 'de'
    Else If W = 'in' Then ST[I].W := 'en'
    Else If W = 'da' Then ST[I].W := 'desde';
    Exit;
  End;
  If W = 'non' Then
  Begin
    ST[I].Cat := 'G';
    Exit;
  End;
  If W = 'mai' Then
  Begin
    ST[I].Cat := 'R';
    ST[I].En := 'never';
    Exit;
  End;
  If (W = 'niente') Or (W = 'nulla') Then
  Begin
    ST[I].Cat := 'g';
    ST[I].En := 'anything';
    Exit;
  End;
  If (W = 'nessuno') And (Not ItNextNounish(I + 1)) Then
  Begin
    ST[I].Cat := 'g';
    ST[I].En := 'anyone';
    Exit;
  End;
  { pronoms sujets }
  If InList(W, 'io tu lui lei noi voi loro egli ella esso essa') Then
  Begin
    ST[I].Cat := 'P';
    If W = 'io' Then Begin ST[I].En := 'I'; ST[I].Slot := 1; End
    Else If W = 'tu' Then Begin ST[I].En := 'you'; ST[I].Slot := 2; End
    Else If (W = 'lui') Or (W = 'egli') Or (W = 'esso') Then Begin ST[I].En :=
        'he'; ST[I].Slot := 3; End
    Else If (W = 'lei') Or (W = 'ella') Then Begin ST[I].En := 'she'; ST[I].Slot
        := 3; End
    Else If W = 'essa' Then Begin ST[I].En := 'it'; ST[I].Slot := 3; End
    Else If W = 'noi' Then Begin ST[I].En := 'we'; ST[I].Slot := 4; End
    Else If W = 'voi' Then Begin ST[I].En := 'you'; ST[I].Slot := 5; End
    Else Begin ST[I].En := 'they'; ST[I].Slot := 6; End;
    Exit;
  End;
  { articles definis / pronoms clitiques }
  If InList(W, 'il lo la l i gli le') Then
  Begin
    If (InList(W, 'lo la li le gli')) And (Not ItNextNounish(I + 1)) And
        ItNextVerbal(I + 1) Then
    Begin
      ST[I].Cat := 'O';
      If W = 'lo' Then Begin ST[I].En := 'him'; End
      Else If W = 'la' Then Begin ST[I].En := 'her'; End
      Else If W = 'gli' Then Begin ST[I].En := 'him'; ST[I].W := 'le'; End
      Else Begin ST[I].En := 'them'; ST[I].W := 'los'; End;
      Exit;
    End;
    ST[I].Cat := 'D';
    ST[I].Kind := 'd';
    ST[I].Plu := (W = 'i') Or (W = 'gli') Or (W = 'le');
    Exit;
  End;
  If InList(W, 'un uno una') Then
  Begin
    ST[I].Cat := 'D';
    ST[I].Kind := 'i';
    Exit;
  End;
  If InList(W,
      'mio mia miei mie tuo tua tuoi tue suo sua suoi sue nostro nostra nostri'+
      ' nostre vostro vostra vostri vostre') Or
     ((W = 'loro') And False) Then
  Begin
    ST[I].Cat := 'S';
    If Copy(W, 1, 2) = 'mi' Then ST[I].En := 'my'
    Else If Copy(W, 1, 2) = 'tu' Then ST[I].En := 'your'
    Else If Copy(W, 1, 2) = 'su' Then ST[I].En := 'his'
    Else If Copy(W, 1, 2) = 'no' Then ST[I].En := 'our'
    Else ST[I].En := 'your';
    ST[I].Plu := EndsWith(W, 'i') Or EndsWith(W, 'e');
    Exit;
  End;
  If InList(W,
      'questo questa questi queste quello quella quelli quelle quel quei quegli'
      ) Then
  Begin
    ST[I].Cat := 'T';
    If Copy(W, 1, 4) = 'ques' Then ST[I].En := 'this' Else ST[I].En := 'that';
    ST[I].Plu := (W = 'questi') Or (W = 'queste') Or (W = 'quelli') Or (W =
        'quelle') Or (W = 'quei') Or (W = 'quegli');
    If ST[I].Plu Then
      If ST[I].En = 'this' Then ST[I].En := 'these' Else ST[I].En := 'those';
    Exit;
  End;
  { quantificateurs }
  If InList(W,
      'tutto tutta tutti tutte ogni alcuni alcune qualche nessun nessuna parec'+
      'chi parecchie vari varie diversi diverse troppo troppa troppi troppe') Or
     (InList(W, 'molto molta molti molte poco poca pochi poche') And (I < SN)
         And (ItFindNoun(ST[I + 1].W, Plu) > 0)) Then
  Begin
    ST[I].Cat := 'Q';
    ST[I].Plu := EndsWith(W, 'i') Or EndsWith(W, 'e');
    If Copy(W, 1, 4) = 'tutt' Then ST[I].En := 'all'
    Else If W = 'ogni' Then Begin ST[I].En := 'every'; ST[I].Plu := False; End
    Else If Copy(W, 1, 4) = 'molt' Then ST[I].En := 'many'
    Else If Copy(W, 1, 4) = 'poch' Then ST[I].En := 'few'
    Else If Copy(W, 1, 4) = 'poco' Then ST[I].En := 'few'
    Else If Copy(W, 1, 3) = 'alc' Then ST[I].En := 'some'
    Else If W = 'qualche' Then Begin ST[I].En := 'some'; ST[I].Plu := True; End
    Else If Copy(W, 1, 3) = 'nes' Then Begin ST[I].En := 'no'; ST[I].Plu :=
        False; End
    Else If Copy(W, 1, 3) = 'var' Then ST[I].En := 'several'
    Else If Copy(W, 1, 3) = 'div' Then ST[I].En := 'several'
    Else If Copy(W, 1, 3) = 'par' Then ST[I].En := 'several'
    Else ST[I].En := 'too many';
    If (ST[I].En = 'many') And (Not ST[I].Plu) Then ST[I].En := 'much';
    If (ST[I].En = 'few') And (Not ST[I].Plu) Then ST[I].En := 'little';
    If (ST[I].En = 'all') And (Not ST[I].Plu) Then ST[I].Plu := False;
    Exit;
  End;
  { clitiques complements }
  If InList(W, 'mi ti ci vi si ne') Then
  Begin
    ST[I].Cat := 'O';
    If W = 'mi' Then Begin ST[I].En := 'me'; ST[I].W := 'me'; End
    Else If W = 'ti' Then Begin ST[I].En := 'you'; ST[I].W := 'te'; End
    Else If W = 'ci' Then Begin ST[I].En := 'us'; ST[I].W := 'nos'; End
    Else If W = 'vi' Then Begin ST[I].En := 'you'; ST[I].W := 'os'; End
    Else If W = 'si' Then Begin ST[I].En := ''; ST[I].W := 'se'; End
    Else Begin ST[I].En := ''; ST[I].Cat := ' '; End;
    Exit;
  End;
  { mots grammaticaux du lexique }
  Idx := ItFind(W, 'w');
  If (Idx > 0) And (QStart Or InList(W,
      'dove quanto quanti quanta quante quale quali chi')) Then
  Begin
    ST[I].Cat := 'W';
    ST[I].En := ItLex[Idx].En;
    If W = 'dove' Then ST[I].W := 'donde'
    Else If W = 'chi' Then ST[I].W := 'quien'
    Else If Copy(W, 1, 4) = 'qual' Then ST[I].W := 'cual'
    Else If Copy(W, 1, 5) = 'quant' Then ST[I].W := 'cuantos'
    Else If W = 'quando' Then ST[I].W := 'cuando'
    Else If W = 'come' Then ST[I].W := 'como'
    Else If W = 'perché' Then ST[I].W := 'por que'
    Else ST[I].W := 'que';
    Exit;
  End;
  Idx := ItFind(W, 'p');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'E';
    ST[I].En := ItLex[Idx].En;
    If W = 'di' Then ST[I].W := 'de'
    Else If W = 'in' Then ST[I].W := 'en'
    Else If W = 'da' Then ST[I].W := 'desde'
    Else If W = 'senza' Then ST[I].W := 'sin'
    Else If W = 'per' Then ST[I].W := 'para';
    Exit;
  End;
  Idx := ItFind(W, 'c');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'C';
    ST[I].En := ItLex[Idx].En;
    Exit;
  End;
  Idx := ItFind(W, 'j');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'J';
    ST[I].En := ItLex[Idx].En;
    Exit;
  End;
  Idx := ItFind(W, 'r');
  If (Idx > 0) And Not ((W = 'bene') And False) Then
  Begin
    ST[I].Cat := 'R';
    ST[I].En := ItLex[Idx].En;
    If (W = 'molto') And (Not (ItNextNounish(I + 1))) Then ST[I].En := 'very';
    Exit;
  End;
  { noms, adjectifs, verbes }
  LN := ItFindNoun(W, Plu);
  IsNoun := LN > 0;
  If IsNoun Then ST[I].Plu := Plu;
  LA := ItFindAdj(W, Plu);
  IsAdj := LA > 0;
  IsVerb := ItMatchVerb(W, Idx, T, Sl);
  If (Not IsVerb) And ItEnclitic(W, Idx, Clit) Then
  Begin
    ST[I].Cat := 'V';
    ST[I].Idx := ItLex[Idx].SpIdx;
    ST[I].Tns := 'I';
    ST[I].Slot := 0;
    ST[I].En := ItLex[Idx].En;
    ST[I].Cl := Clit;
    Exit;
  End;
  If IsVerb And (IsNoun Or IsAdj) And (Prev In ['D', 'S', 'T', 'Q', 'U', 'E'])
      Then IsVerb := False;
  If IsVerb And (T = 'P') And IsAdj Then IsVerb := False;
  If IsVerb And (Not (T In ['p', 'm', 'f', 'c', 'i'])) And (IsNoun Or IsAdj) And
      (Prev <> 'V') Then IsVerb := False;
  { ho fame, ho freddo : le nom l'emporte apres avere }
  If IsVerb And IsNoun And (Prev = 'V') And (T In ['p', 'm']) Then IsVerb :=
      False;
  If IsVerb Then
  Begin
    ST[I].Cat := 'V';
    ST[I].Idx := ItLex[Idx].SpIdx;
    ST[I].Tns := T;
    ST[I].Slot := Sl;
    ST[I].En := ItLex[Idx].En;
    Exit;
  End;
  If IsNoun And IsAdj Then
  Begin
    If Prev In ['D', 'S', 'T', 'Q', 'U', 'A'] Then IsAdj := False
    Else If (Prev = 'V') And InList(W, 'freddo caldo') Then IsAdj := False
    Else IsNoun := False;
  End;
  If IsNoun Then
  Begin
    ST[I].Cat := 'N';
    ST[I].Plu := Plu;
    ST[I].En := ItLex[LN].En;
    ItFindNoun(W, Plu);
    ST[I].Plu := Plu;
    Exit;
  End;
  If IsAdj Then
  Begin
    ItFindAdj(W, Plu);
    ST[I].Cat := 'A';
    ST[I].Plu := Plu;
    ST[I].En := ItLex[LA].En;
    Exit;
  End;
  ST[I].Cat := 'Z';
End;

{ ------------------------------------------------------------------ }
{ ITALIEN -> ANGLAIS : lissage et phrase complete                     }
{ ------------------------------------------------------------------ }

Function ItVerbKey(I: Integer): String;
Begin
  If (I >= 1) And (I <= SN) And (ST[I].Cat = 'V') And (ST[I].Idx > 0) Then
    ItVerbKey := SpPlain(SpLex[ST[I].Idx].Es)
  Else
    ItVerbKey := '';
End;

Procedure ItClassify;
Var
  I, J, K: Integer;
  Key: String;
Begin
  { elisions : l'amico, c'e, dov'e... }
  For I := 1 To SN Do
    If Not ST[I].Punct Then
    Begin
      Key := ST[I].W;
      If Key = 'c' Then ST[I].W := 'ci'
      Else If Key = 'm' Then ST[I].W := 'mi'
      Else If Key = 't' Then ST[I].W := 'ti'
      Else If Key = 's' Then ST[I].W := 'si'
      Else If Key = 'n' Then ST[I].W := 'ne'
      Else If Key = 'd' Then ST[I].W := 'di'
      Else If Key = 'dov' Then ST[I].W := 'dove'
      Else If Key = 'com' Then ST[I].W := 'come'
      Else If Key = 'cos' Then ST[I].W := 'cosa'
      Else If Key = 'quest' Then ST[I].W := 'questo'
      Else If Key = 'quell' Then ST[I].W := 'quello';
    End;
  For I := 1 To SN Do
    If ST[I].W = 'sia' Then ST[I].W := 'è';
  ItExpandContractions;
  ItMergeMultiword;
  For I := 1 To SN Do
    ItPickCat(I);
  For I := 1 To SN Do
    If ST[I].Cat = 'Z' Then ST[I].Orig := ItProperEn(ST[I].Orig);
  { partitif : mangio del pane = I eat some bread }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'E') And (ST[I].W = 'de') And (ST[I + 1].Cat = 'D') And
        (ST[I + 1].Cl = 'part') And
       ((I = 1) Or (ST[I - 1].Cat In ['V', 'G'])) Then
    Begin
      ST[I].Cat := 'Q';
      ST[I].En := 'some';
      ST[I].Plu := ST[I + 1].Plu;
      ST[I + 1].Cat := ' ';
    End;
  { il mio libro : l'article devant un possessif disparait }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'D') And (ST[I].Kind = 'd') And (ST[I + 1].Cat = 'S') Then
        ST[I].Cat := ' ';
  { ci + essere : il y a }
  For I := 1 To SN - 2 Do
    If (ST[I].Cat = 'O') And (ST[I].W = 'nos') And (ST[I + 1].Cat = 'V') And
        (ItVerbKey(I + 1) = 'ser') And
       (ST[I + 1].Tns In ['p', 'm', 'f', 'c']) And (ST[I + 2].Cat In ['D', 'N',
           'Q', 'U', 'A']) Then
    Begin
      ST[I].Cat := ' ';
      ST[I + 1].Idx := AlSpIndex('haber', 'have');
      ST[I + 1].W := 'hay';
      ST[I + 1].Slot := 3;
    End;
  { fa freddo / fa caldo }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'V') And (ItVerbKey(I) = 'hacer') And InList(ST[I + 1].W,
        'freddo caldo') Then
    Begin
      ST[I].Idx := AlSpIndex('ser', 'be');
      ST[I].En := 'be';
      ST[I].Slot := 3;
      ST[I + 1].Cat := 'A';
      If ST[I + 1].W = 'freddo' Then ST[I + 1].En := 'cold' Else ST[I + 1].En :=
          'hot';
    End;
  { ho vent'anni }
  For I := 1 To SN Do
    If (ST[I].Cat = 'N') And (ST[I].W = 'anni') Then ST[I].W := 'anos';
  { passe compose : ho mangiato = I ate }
  For I := 1 To SN - 1 Do
    If (ST[I].Cat = 'V') And ((ItVerbKey(I) = 'tener') Or (ItVerbKey(I) =
        'ser')) And
       (ST[I].Tns In ['p', 'm', 'f', 'c']) Then
    Begin
      J := I + 1;
      While (J <= SN) And (ST[J].Cat In ['R', 'G']) Do Inc(J);
      If (J <= SN) And (ST[J].Cat = 'V') And (ST[J].Tns = 'P') And
         ((ItVerbKey(I) = 'tener') Or
          InList(ST[J].En,
              'go come arrive return leave go_out enter fall be_born die stay '+
              'be')) Then
      Begin
        If ST[I].Tns = 'p' Then
        Begin
          ST[J].Tns := 'i';
          ST[J].Slot := ST[I].Slot;
          ST[I].Cat := ' ';
        End
        Else
        Begin
          ST[I].Idx := AlSpIndex('haber', 'have');
          ST[I].En := 'have';
        End;
      End;
    End;
  { a Roma, a scuola, all'universita : lieu et non complement de personne }
  For I := 2 To SN - 1 Do
    If (ST[I].Cat = 'E') And (ST[I].W = 'a') And ((ST[I - 1].Cat = 'N') Or (ST[I
        - 1].Cat = 'V')) And
       (Not InList(ST[I - 1].En,
           'go come arrive return leave travel walk run drive enter give send '+
           'write tell show teach bring say ask offer sell call')) Then
    Begin
      K := I + 1;
      If ST[K].Cat = 'D' Then Inc(K);
      If (K <= SN) And ((ST[K].Cat = 'Z') Or ((ST[K].Cat = 'N') And Not
          SpIsPersonEn(ST[K].En))) Then
      Begin
        ST[I].W := 'en';
        If ST[K].Cat = 'Z' Then ST[I].En := 'in' Else ST[I].En := 'at';
      End;
    End;
  { a casa : chez soi }
  For I := 2 To SN - 1 Do
    If (ST[I].Cat = 'E') And InList(ST[I].W, 'a en') And (ST[I - 1].Cat = 'V')
        And
       (ST[I + 1].Cat = 'N') And (ST[I + 1].W = 'casa') Then
    Begin
      ST[I + 1].Cat := 'R';
      If ST[I].En = 'at' Then ST[I + 1].En := 'at home' Else ST[I + 1].En :=
          'home';
      ST[I].Cat := ' ';
    End;
  { jours de la semaine : adverbes de temps }
  For I := 1 To SN Do
    If (ST[I].Cat = 'N') And InList(ST[I].En,
        'Monday Tuesday Wednesday Thursday Friday Saturday Sunday') And
       ((I = 1) Or Not (ST[I - 1].Cat In ['D', 'E', 'S', 'T', 'Q'])) Then
    Begin
      ST[I].Cat := 'R';
      ST[I].En := 'on ' + ST[I].En;
    End;
  { si trova = est situe ; vorrei = je voudrais ; costa : sujet "it" }
  For I := 1 To SN Do
    If ST[I].Cat = 'V' Then
    Begin
      If InList(ST[I].W, 'trova trovano trovavano') And (I > 1) And (ST[I -
          1].Cat = 'O') And (ST[I - 1].W = 'se') Then
      Begin
        ST[I].Idx := AlSpIndex('estar', 'be');
        ST[I].En := 'be';
      End
      Else If (ItVerbKey(I) = 'querer') And (ST[I].Tns = 'c') Then
        ST[I].En := 'like';
    End;
  For I := SN Downto 1 Do
    If (ST[I].Cat = 'V') And (ST[I].En = 'cost') And (ST[I].Slot = 3) And
        (ST[I].Tns In ['p', 'i']) And
       ((I = 1) Or Not (ST[I - 1].Cat In ['P', 'N', 'Z'])) And (SN < MaxTokens)
           Then
    Begin
      For K := SN Downto I Do
        ST[K + 1] := ST[K];
      Inc(SN);
      FillChar(ST[I], SizeOf(TSTok), 0);
      ST[I].Cat := 'P';
      ST[I].W := 'esso';
      ST[I].En := 'it';
      ST[I].Slot := 3;
      ST[I].Tns := ' ';
    End;
  { chi e ? = who is it ? }
  If (SN = 2) And (ST[1].Cat = 'W') And (ST[1].W = 'quien') And (ItVerbKey(2) =
      'ser') Then
  Begin
    SN := 3;
    FillChar(ST[3], SizeOf(TSTok), 0);
    ST[3].Cat := 'P';
    ST[3].W := 'esso';
    ST[3].En := 'it';
    ST[3].Slot := 3;
    ST[3].Tns := ' ';
  End;
  { in cucina, in giardino : article defini sous-entendu }
  I := 1;
  While I < SN Do
  Begin
    If (ST[I].Cat = 'E') And (ST[I].W = 'en') And (ST[I + 1].Cat = 'N') And (Not
        ST[I + 1].Plu) And
       InList(ST[I + 1].En,
           'kitchen bathroom bedroom room garden street city park') And (SN <
           MaxTokens) Then
    Begin
      For K := SN Downto I + 1 Do
        ST[K + 1] := ST[K];
      Inc(SN);
      FillChar(ST[I + 1], SizeOf(TSTok), 0);
      ST[I + 1].Cat := 'D';
      ST[I + 1].Kind := 'd';
      ST[I + 1].W := 'el';
      ST[I + 1].Tns := ' ';
      Inc(I);
    End;
    Inc(I);
  End;
  { piove, nevica }
  For I := 1 To SN Do
    If ST[I].Cat = 'V' Then
    Begin
      Key := ItVerbKey(I);
      If Key = 'llover' Then ST[I].Kind := 'g'
      Else If Key = 'llamar' Then
      Begin
        If (I > 1) And (ST[I - 1].Cat = 'O') And InList(ST[I - 1].W, 'me te se')
            Then ST[I].Kind := 'r';
      End;
    End;
  { impératif : Apri la porta ! }
  If (SN >= 1) And (SEnd = '!') And (ST[1].Cat = 'V') And (ST[1].Tns = 'p') And
      (ST[1].Slot In [2, 5]) Then
    ST[1].Tns := 'M';
  { pronoms apres une preposition : lui, lei sont deja des groupes nominaux }
  K := 0;
  ARemoveConsumed;
End;

Function ItTranslateSentence(Const Sentence: String): String;
Var
  Res: String;
  TimeQ: Boolean;
Begin
  ItTokenize(Sentence);
  If SN = 0 Then
  Begin
    ItTranslateSentence := '';
    Exit;
  End;
  ItClassify;
  If SN = 0 Then
  Begin
    ItTranslateSentence := '';
    Exit;
  End;
  TimeQ := (SN = 3) And (ST[1].Cat = 'W') And InList(ST[2].W, 'ora ore') And
      (ItVerbKey(3) = 'ser');
  Res := TranslateTokensEs(1, SN, SEnd = '?');
  If TimeQ Then Res := 'What time is it';
  { Come ti chiami ? -> What is your name ? }
  If (SEnd = '?') And (Length(Res) > 8) And (Copy(Res, Length(Res) - 7, 8) =
      ' name is') Then
    Res := 'What is ' + LowerAscii(Copy(Res, 1, Length(Res) - 8)) + ' name';
  Res := UpperFirst(FixEnglish(Res));
  If SEnd <> #0 Then Res := Res + SEnd;
  ItTranslateSentence := Res;
End;

{ ------------------------------------------------------------------ }
{ LATIN -> ANGLAIS : lexique et morphologie (declinaisons, conjugaisons) }
{ ------------------------------------------------------------------ }
{ Les cinq declinaisons et les quatre conjugaisons sont engendrees a   }
{ partir de formes de base ; les verbes irreguliers sont dans une table. }
{ Les cas sont codes par des bits : nominatif 1, genitif 2, datif 4,     }
{ accusatif 8, ablatif 16, vocatif 32 ; le pluriel decale de 6 bits.     }

Type
  TLa = Record
    Orig: String[24];
    F1, F2: String[24];
    En: String[28];
    Pos: String[4];
    Key: String[10];
    SpIdx: Integer;
  End;

  TLaIrr = Record
    Inf: String[8];
    Pres, Impf, Fut, Perf: Array[1..6] Of String[10];
    Imp: String[16];
  End;

Const
  LaIrrData: Array[0..5] Of String = (
    'esse|sum,es,est,sumus,estis,sunt|eram,eras,erat,eramus,eratis,erant|ero,e'+
        'ris,erit,erimus,eritis,erunt|fui,fuisti,fuit,fuimus,fuistis,fuerunt|e'+
        's,este',
    'posse|possum,potes,potest,possumus,potestis,possunt|poteram,poteras,poter'+
        'at,poteramus,poteratis,poterant|potero,poteris,poterit,poterimus,pote'+
        'ritis,poterunt|potui,potuisti,potuit,potuimus,potuistis,potuerunt|-',
    'ire|eo,is,it,imus,itis,eunt|ibam,ibas,ibat,ibamus,ibatis,ibant|ibo,ibis,i'+
        'bit,ibimus,ibitis,ibunt|ii,isti,iit,iimus,istis,ierunt|i,ite',
    'ferre|fero,fers,fert,ferimus,fertis,ferunt|ferebam,ferebas,ferebat,fereba'+
        'mus,ferebatis,ferebant|feram,feres,feret,feremus,feretis,ferent|tuli,'+
        'tulisti,tulit,tulimus,tulistis,tulerunt|fer,ferte',
    'velle|volo,vis,vult,volumus,vultis,volunt|volebam,volebas,volebat,volebam'+
        'us,volebatis,volebant|volam,voles,volet,volemus,voletis,volent|volui,'+
        'voluisti,voluit,voluimus,voluistis,voluerunt|-',
    'nolle|nolo,non vis,non vult,nolumus,non vultis,nolunt|nolebam,nolebas,nol'+
        'ebat,nolebamus,nolebatis,nolebant|nolam,noles,nolet,nolemus,noletis,n'+
        'olent|nolui,noluisti,noluit,noluimus,noluistis,noluerunt|noli,nolite'
  );

Var
  LaLex: Array[1..700] Of TLa;
  LaCount: Integer;
  LaIrr: Array[1..8] Of TLaIrr;
  LaIrrCount: Integer;
  SourceLa: Boolean;

Procedure LoadLatin;
Var
  I, K, J: Integer;
  Line, Item, W, Rest, P, Part: String;
Begin
  LaCount := 0;
  For I := 0 To High(LaData) Do
  Begin
    Line := LaData[I];
    While Line <> '' Do
    Begin
      Item := CopyUntil(Line, ';');
      If (Item <> '') And (LaCount < 700) Then
      Begin
        W := CopyUntil(Item, '=');
        Rest := CopyUntil(Item, ':');
        Inc(LaCount);
        FillChar(LaLex[LaCount], SizeOf(TLa), 0);
        P := CopyUntil(W, '/');
        LaLex[LaCount].Orig := ReplaceChar(P, '_', ' ');
        LaLex[LaCount].F1 := FrLower(LaLex[LaCount].Orig);
        LaLex[LaCount].F2 := FrLower(W);
        LaLex[LaCount].En := ReplaceChar(Rest, '_', ' ');
        LaLex[LaCount].Pos := CopyUntil(Item, ':');
        LaLex[LaCount].Key := Item;
        If Copy(LaLex[LaCount].Pos, 1, 1) = 'v' Then
        Begin
          If (LaLex[LaCount].Key <> '') And (LaLex[LaCount].Key <> 'P') Then
            LaLex[LaCount].SpIdx := AlSpIndex(LaLex[LaCount].Key,
                LaLex[LaCount].En)
          Else
            LaLex[LaCount].SpIdx := AlSpIndex('la:' + LaLex[LaCount].F1,
                LaLex[LaCount].En);
        End;
      End;
    End;
  End;
  LaIrrCount := 0;
  For I := 0 To High(LaIrrData) Do
    If LaIrrCount < 8 Then
    Begin
      Line := LaIrrData[I];
      Inc(LaIrrCount);
      LaIrr[LaIrrCount].Inf := CopyUntil(Line, '|');
      Part := CopyUntil(Line, '|');
      For K := 1 To 6 Do LaIrr[LaIrrCount].Pres[K] := CopyUntil(Part, ',');
      Part := CopyUntil(Line, '|');
      For K := 1 To 6 Do LaIrr[LaIrrCount].Impf[K] := CopyUntil(Part, ',');
      Part := CopyUntil(Line, '|');
      For K := 1 To 6 Do LaIrr[LaIrrCount].Fut[K] := CopyUntil(Part, ',');
      Part := CopyUntil(Line, '|');
      For K := 1 To 6 Do LaIrr[LaIrrCount].Perf[K] := CopyUntil(Part, ',');
      LaIrr[LaIrrCount].Imp := Line;
    End;
  J := 0;
End;

Function LaStripEnd(Const S, E: String): String;
Begin
  If EndsWith(S, E) Then LaStripEnd := Copy(S, 1, Length(S) - Length(E)) Else
      LaStripEnd := S;
End;

{ Forme d'un nom ; Num 0 singulier, 1 pluriel ; Cas 1..6 }
Function LaNounForm(I, Num, Cas: Integer): String;
Var
  P, F1, F2, Stem: String;
Begin
  P := LaLex[I].Pos;
  F1 := LaLex[I].F1;
  F2 := LaLex[I].F2;
  LaNounForm := '';
  If P = 'n1' Then
  Begin
    Stem := LaStripEnd(F1, 'a');
    If Num = 0 Then
      Case Cas Of
        1, 5, 6: LaNounForm := Stem + 'a';
        2, 3: LaNounForm := Stem + 'ae';
        4: LaNounForm := Stem + 'am';
      End
    Else
      Case Cas Of
        1, 6: LaNounForm := Stem + 'ae';
        2: LaNounForm := Stem + 'arum';
        3, 5: LaNounForm := Stem + 'is';
        4: LaNounForm := Stem + 'as';
      End;
  End
  Else If P = 'n2m' Then
  Begin
    If EndsWith(F1, 'us') Then Stem := LaStripEnd(F1, 'us') Else Stem :=
        LaStripEnd(F2, 'i');
    If Num = 0 Then
      Case Cas Of
        1: LaNounForm := F1;
        2: LaNounForm := Stem + 'i';
        3, 5: LaNounForm := Stem + 'o';
        4: LaNounForm := Stem + 'um';
        6: If EndsWith(F1, 'us') Then LaNounForm := Stem + 'e' Else LaNounForm
            := F1;
      End
    Else
      Case Cas Of
        1, 6: LaNounForm := Stem + 'i';
        2: LaNounForm := Stem + 'orum';
        3, 5: LaNounForm := Stem + 'is';
        4: LaNounForm := Stem + 'os';
      End;
  End
  Else If P = 'n2n' Then
  Begin
    Stem := LaStripEnd(F1, 'um');
    If Num = 0 Then
      Case Cas Of
        1, 4, 6: LaNounForm := Stem + 'um';
        2: LaNounForm := Stem + 'i';
        3, 5: LaNounForm := Stem + 'o';
      End
    Else
      Case Cas Of
        1, 4, 6: LaNounForm := Stem + 'a';
        2: LaNounForm := Stem + 'orum';
        3, 5: LaNounForm := Stem + 'is';
      End;
  End
  Else If (P = 'n3m') Or (P = 'n3f') Then
  Begin
    Stem := LaStripEnd(F2, 'is');
    If Num = 0 Then
      Case Cas Of
        1, 6: LaNounForm := F1;
        2: LaNounForm := Stem + 'is';
        3: LaNounForm := Stem + 'i';
        4: LaNounForm := Stem + 'em';
        5: LaNounForm := Stem + 'e';
      End
    Else
      Case Cas Of
        1, 4, 6: LaNounForm := Stem + 'es';
        2: LaNounForm := Stem + 'um';
        3, 5: LaNounForm := Stem + 'ibus';
      End;
  End
  Else If P = 'n3n' Then
  Begin
    Stem := LaStripEnd(F2, 'is');
    If Num = 0 Then
      Case Cas Of
        1, 4, 6: LaNounForm := F1;
        2: LaNounForm := Stem + 'is';
        3: LaNounForm := Stem + 'i';
        5: LaNounForm := Stem + 'e';
      End
    Else
      Case Cas Of
        1, 4, 6: LaNounForm := Stem + 'a';
        2: LaNounForm := Stem + 'um';
        3, 5: LaNounForm := Stem + 'ibus';
      End;
  End
  Else If P = 'n4m' Then
  Begin
    Stem := LaStripEnd(F1, 'us');
    If Num = 0 Then
      Case Cas Of
        1, 6: LaNounForm := Stem + 'us';
        2: LaNounForm := Stem + 'us';
        3: LaNounForm := Stem + 'ui';
        4: LaNounForm := Stem + 'um';
        5: LaNounForm := Stem + 'u';
      End
    Else
      Case Cas Of
        1, 4, 6: LaNounForm := Stem + 'us';
        2: LaNounForm := Stem + 'uum';
        3, 5: LaNounForm := Stem + 'ibus';
      End;
  End
  Else If P = 'n4n' Then
  Begin
    Stem := LaStripEnd(F1, 'u');
    If Num = 0 Then
      Case Cas Of
        1, 3, 4, 5, 6: LaNounForm := Stem + 'u';
        2: LaNounForm := Stem + 'us';
      End
    Else
      Case Cas Of
        1, 4, 6: LaNounForm := Stem + 'ua';
        2: LaNounForm := Stem + 'uum';
        3, 5: LaNounForm := Stem + 'ibus';
      End;
  End
  Else If P = 'n5' Then
  Begin
    Stem := LaStripEnd(F1, 'es');
    If Num = 0 Then
      Case Cas Of
        1, 6: LaNounForm := Stem + 'es';
        2, 3: LaNounForm := Stem + 'ei';
        4: LaNounForm := Stem + 'em';
        5: LaNounForm := Stem + 'e';
      End
    Else
      Case Cas Of
        1, 4, 6: LaNounForm := Stem + 'es';
        2: LaNounForm := Stem + 'erum';
        3, 5: LaNounForm := Stem + 'ebus';
      End;
  End;
End;

{ Genre d'un nom }
Function LaNounGender(I: Integer): Char;
Var
  P: String;
Begin
  P := LaLex[I].Pos;
  If LaLex[I].Key = 'm' Then LaNounGender := 'm'
  Else If LaLex[I].Key = 'f' Then LaNounGender := 'f'
  Else If (P = 'n1') Or (P = 'n3f') Or (P = 'n5') Then LaNounGender := 'f'
  Else If (P = 'n2n') Or (P = 'n3n') Or (P = 'n4n') Then LaNounGender := 'n'
  Else LaNounGender := 'm';
End;

{ theme en -i : accusatif pluriel en -is, genitif pluriel en -ium }
Function LaIStem(I: Integer): Boolean;
Var
  F2: String;
  L: Integer;
Begin
  F2 := LaLex[I].F2;
  L := Length(F2);
  LaIStem := False;
  If Copy(LaLex[I].Pos, 1, 2) <> 'n3' Then Exit;
  If LaLex[I].F1 = F2 Then
    LaIStem := True
  Else If (L > 4) And EndsWith(F2, 'is') And (Not (F2[L - 2] In ['a', 'e', 'i',
      'o', 'u', 'r', 'l'])) And
          (Not (F2[L - 3] In ['a', 'e', 'i', 'o', 'u'])) Then
    LaIStem := True;
End;

{ Cherche un nom : masque des cas possibles (12 bits) }
Function LaFindNoun(Const W: String; Var Mask: Integer): Integer;
Var
  I, Num, Cas: Integer;
  F: String;
  M: Integer;
Begin
  LaFindNoun := 0;
  Mask := 0;
  For I := 1 To LaCount Do
    If (Copy(LaLex[I].Pos, 1, 1) = 'n') Then
    Begin
      M := 0;
      For Num := 0 To 1 Do
        For Cas := 1 To 6 Do
        Begin
          F := LaNounForm(I, Num, Cas);
          If (F <> '') And (F = W) Then M := M Or (1 Shl (Num * 6 + Cas - 1));
          { genitif pluriel et accusatif pluriel des themes en -i }
          If (Num = 1) And (Cas = 2) And (F <> '') And LaIStem(I) And
             (W = LaStripEnd(F, 'um') + 'ium') Then M := M Or (1 Shl (Num * 6 +
                 Cas - 1));
          If (Num = 1) And (Cas = 4) And (F <> '') And LaIStem(I) And
             (W = LaStripEnd(F, 'es') + 'is') Then M := M Or (1 Shl (Num * 6 +
                 Cas - 1));
        End;
      If M <> 0 Then
      Begin
        Mask := M;
        LaFindNoun := I;
        Exit;
      End;
    End;
End;

{ Forme d'un adjectif ; genre m, f ou n }
Function LaAdjForm(I: Integer; G: Char; Num, Cas: Integer): String;
Var
  P, F1, F2, Stem: String;
Begin
  P := LaLex[I].Pos;
  F1 := LaLex[I].F1;
  F2 := LaLex[I].F2;
  LaAdjForm := '';
  If P = 'a12' Then
  Begin
    If EndsWith(F1, 'us') Then Stem := LaStripEnd(F1, 'us') Else Stem :=
        LaStripEnd(F2, 'a');
    If G = 'f' Then
    Begin
      LaLex[I].Pos := 'n1';
      LaLex[I].F1 := Stem + 'a';
      LaAdjForm := LaNounForm(I, Num, Cas);
    End
    Else If G = 'n' Then
    Begin
      LaLex[I].Pos := 'n2n';
      LaLex[I].F1 := Stem + 'um';
      LaAdjForm := LaNounForm(I, Num, Cas);
    End
    Else
    Begin
      LaLex[I].Pos := 'n2m';
      If EndsWith(F1, 'us') Then LaLex[I].F2 := '' Else LaLex[I].F2 := Stem +
          'i';
      LaAdjForm := LaNounForm(I, Num, Cas);
      LaLex[I].F2 := F2;
    End;
    LaLex[I].Pos := P;
    LaLex[I].F1 := F1;
  End
  Else If (P = 'a3') Or (P = 'a3x') Then
  Begin
    If P = 'a3' Then Stem := LaStripEnd(F1, 'is') Else Stem := LaStripEnd(F2,
        'is');
    If G = 'n' Then
    Begin
      If Num = 0 Then
        Case Cas Of
          1, 4, 6: If P = 'a3' Then LaAdjForm := Stem + 'e' Else LaAdjForm :=
              F1;
          2: LaAdjForm := Stem + 'is';
          3, 5: LaAdjForm := Stem + 'i';
        End
      Else
        Case Cas Of
          1, 4, 6: LaAdjForm := Stem + 'ia';
          2: LaAdjForm := Stem + 'ium';
          3, 5: LaAdjForm := Stem + 'ibus';
        End;
    End
    Else
    Begin
      If Num = 0 Then
        Case Cas Of
          1, 6: If P = 'a3' Then LaAdjForm := Stem + 'is' Else LaAdjForm := F1;
          2: LaAdjForm := Stem + 'is';
          3, 5: LaAdjForm := Stem + 'i';
          4: LaAdjForm := Stem + 'em';
        End
      Else
        Case Cas Of
          1, 4, 6: LaAdjForm := Stem + 'es';
          2: LaAdjForm := Stem + 'ium';
          3, 5: LaAdjForm := Stem + 'ibus';
        End;
    End;
  End;
End;

{ Cherche un adjectif : masque des cas (12 bits) et genres possibles }
Function LaFindAdj(Const W: String; Var Mask: Integer; Var Gs: String): Integer;
Var
  I, Num, Cas, K: Integer;
  G: Char;
  F: String;
  M, MG: Integer;
  Gset: String;
Begin
  LaFindAdj := 0;
  Mask := 0;
  Gs := '';
  For I := 1 To LaCount Do
    If Copy(LaLex[I].Pos, 1, 1) = 'a' Then
    Begin
      M := 0;
      Gset := '';
      For K := 1 To 3 Do
      Begin
        If K = 1 Then G := 'm' Else If K = 2 Then G := 'f' Else G := 'n';
        MG := 0;
        For Num := 0 To 1 Do
          For Cas := 1 To 6 Do
          Begin
            F := LaAdjForm(I, G, Num, Cas);
            If (F <> '') And (F = W) Then MG := MG Or (1 Shl (Num * 6 + Cas -
                1));
          End;
        If MG <> 0 Then
        Begin
          M := M Or MG;
          Gset := Gset + G;
        End;
      End;
      If M <> 0 Then
      Begin
        Mask := M;
        Gs := Gset;
        LaFindAdj := I;
        Exit;
      End;
    End;
End;

Function LaFind(Const W: String; Const Kinds: String): Integer;
Var
  I: Integer;
Begin
  LaFind := 0;
  For I := 1 To LaCount Do
    If (LaLex[I].F1 = W) And (Pos(LaLex[I].Pos, Kinds) > 0) Then
    Begin
      LaFind := I;
      Exit;
    End;
End;

{ Formes verbales : p present, m imparfait, f futur, i parfait, I infinitif,
  M imperatif }
Function LaMatchVerb(Const W: String; Var Idx: Integer; Var Tns: Char; Var Slot:
    Integer): Boolean;
Const
  PrEnd: Array[1..4, 1..6] Of String[5] = (
    ('o', 'as', 'at', 'amus', 'atis', 'ant'),
    ('eo', 'es', 'et', 'emus', 'etis', 'ent'),
    ('o', 'is', 'it', 'imus', 'itis', 'unt'),
    ('io', 'is', 'it', 'imus', 'itis', 'iunt'));
  PfEnd: Array[1..6] Of String[8] = ('i', 'isti', 'it', 'imus', 'istis',
      'erunt');
  ImEnd: Array[1..6] Of String[4] = ('m', 's', 't', 'mus', 'tis', 'nt');
Var
  I, Q, K, Irr, C: Integer;
  Cls, Stem, F1, Form, Inf: String;

  Function T(Const F: String; Tn: Char; Sl: Integer): Boolean;
  Begin
    T := False;
    If (F <> '') And (F = W) Then
    Begin
      Idx := I;
      Tns := Tn;
      Slot := Sl;
      T := True;
    End;
  End;

Begin
  LaMatchVerb := True;
  For I := 1 To LaCount Do
    If Copy(LaLex[I].Pos, 1, 1) = 'v' Then
    Begin
      F1 := LaLex[I].F1;
      Cls := LaLex[I].Pos;
      Irr := 0;
      For K := 1 To LaIrrCount Do
        If LaIrr[K].Inf = F1 Then Irr := K;
      If Irr > 0 Then
      Begin
        If T(F1, 'I', 0) Then Exit;
        For Q := 1 To 6 Do
          If T(LaIrr[Irr].Pres[Q], 'p', Q) Or T(LaIrr[Irr].Impf[Q], 'm', Q) Or
             T(LaIrr[Irr].Fut[Q], 'f', Q) Or T(LaIrr[Irr].Perf[Q], 'i', Q) Then
                 Exit;
        Inf := LaIrr[Irr].Imp;
        If (Inf <> '-') And (Inf <> '') Then
        Begin
          Form := CopyUntil(Inf, ',');
          If T(Form, 'M', 2) Or T(Inf, 'M', 5) Then Exit;
        End;
      End
      Else
      Begin
        If Cls = 'v1' Then C := 1 Else If Cls = 'v2' Then C := 2 Else If Cls =
            'v3' Then C := 3 Else C := 4;
        If Cls = 'v3i' Then C := 4;
        If Length(F1) < 4 Then Continue;
        Stem := Copy(F1, 1, Length(F1) - 3);
        If Cls = 'v3i' Then Stem := Copy(F1, 1, Length(F1) - 3);
        If T(F1, 'I', 0) Then Exit;
        { present }
        For Q := 1 To 6 Do
        Begin
          If Cls = 'v3i' Then Form := Stem + PrEnd[4, Q]
          Else If Cls = 'v4' Then Form := Stem + PrEnd[4, Q]
          Else Form := Stem + PrEnd[C, Q];
          If T(Form, 'p', Q) Then Exit;
        End;
        { imparfait }
        For Q := 1 To 6 Do
        Begin
          If Cls = 'v1' Then Form := Stem + 'aba' + ImEnd[Q]
          Else If (Cls = 'v2') Or (Cls = 'v3') Then Form := Stem + 'eba' +
              ImEnd[Q]
          Else Form := Stem + 'ieba' + ImEnd[Q];
          If T(Form, 'm', Q) Then Exit;
        End;
        { futur }
        For Q := 1 To 6 Do
        Begin
          If Cls = 'v1' Then
          Begin
            Case Q Of
              1: Form := Stem + 'abo'; 2: Form := Stem + 'abis'; 3: Form := Stem
                  + 'abit';
              4: Form := Stem + 'abimus'; 5: Form := Stem + 'abitis'; 6: Form :=
                  Stem + 'abunt';
            End;
          End
          Else If Cls = 'v2' Then
          Begin
            Case Q Of
              1: Form := Stem + 'ebo'; 2: Form := Stem + 'ebis'; 3: Form := Stem
                  + 'ebit';
              4: Form := Stem + 'ebimus'; 5: Form := Stem + 'ebitis'; 6: Form :=
                  Stem + 'ebunt';
            End;
          End
          Else
          Begin
            If Cls = 'v3' Then
              Case Q Of
                1: Form := Stem + 'am'; 2: Form := Stem + 'es'; 3: Form := Stem
                    + 'et';
                4: Form := Stem + 'emus'; 5: Form := Stem + 'etis'; 6: Form :=
                    Stem + 'ent';
              End
            Else
              Case Q Of
                1: Form := Stem + 'iam'; 2: Form := Stem + 'ies'; 3: Form :=
                    Stem + 'iet';
                4: Form := Stem + 'iemus'; 5: Form := Stem + 'ietis'; 6: Form :=
                    Stem + 'ient';
              End;
          End;
          If T(Form, 'f', Q) Then Exit;
        End;
        { parfait }
        If LaLex[I].F2 <> '' Then
        Begin
          For Q := 1 To 6 Do
            If T(LaLex[I].F2 + PfEnd[Q], 'i', Q) Then Exit;
        End;
        { imperatif }
        If Cls = 'v1' Then
        Begin
          If T(Stem + 'a', 'M', 2) Or T(Stem + 'ate', 'M', 5) Then Exit;
        End
        Else If Cls = 'v2' Then
        Begin
          If T(Stem + 'e', 'M', 2) Or T(Stem + 'ete', 'M', 5) Then Exit;
        End
        Else If (Cls = 'v3') Or (Cls = 'v3i') Then
        Begin
          If T(Stem + 'e', 'M', 2) Or T(Stem + 'ite', 'M', 5) Then Exit;
        End
        Else If T(Stem + 'i', 'M', 2) Or T(Stem + 'ite', 'M', 5) Then Exit;
      End;
    End;
  LaMatchVerb := False;
End;

{ ------------------------------------------------------------------ }
{ LATIN -> ANGLAIS : jetons et etiquetage                             }
{ ------------------------------------------------------------------ }

Var
  LaMk: Array[0..MaxTokens + 2] Of Integer;   { masque des cas du jeton }
  LaGn: Array[0..MaxTokens + 2] Of String[3]; { genres possibles }
  LaQ: Boolean;                                { enclitique -ne : question }

{ Retire les macrons, j -> i. }
Function LaNormalize(Const S: String): String;
Var
  I: Integer;
  R: String;
Begin
  R := '';
  I := 1;
  While I <= Length(S) Do
  Begin
    If (S[I] = #$C4) And (I < Length(S)) And (S[I + 1] In [#$81, #$80]) Then
    Begin
      R := R + 'a';
      Inc(I, 2);
    End
    Else If (S[I] = #$C4) And (I < Length(S)) And (S[I + 1] In [#$93, #$92])
        Then
    Begin
      R := R + 'e';
      Inc(I, 2);
    End
    Else If (S[I] = #$C4) And (I < Length(S)) And (S[I + 1] In [#$AB, #$AA])
        Then
    Begin
      R := R + 'i';
      Inc(I, 2);
    End
    Else If (S[I] = #$C5) And (I < Length(S)) And (S[I + 1] In [#$8D, #$8C])
        Then
    Begin
      R := R + 'o';
      Inc(I, 2);
    End
    Else If (S[I] = #$C5) And (I < Length(S)) And (S[I + 1] In [#$AB, #$AA])
        Then
    Begin
      R := R + 'u';
      Inc(I, 2);
    End
    Else If S[I] = 'j' Then
    Begin
      R := R + 'i';
      Inc(I);
    End
    Else
    Begin
      R := R + S[I];
      Inc(I);
    End;
  End;
  LaNormalize := R;
End;

Function LaKnown(Const W: String): Boolean;
Var
  Mk, Idx, Sl: Integer;
  T: Char;
  Gs: String;
Begin
  LaKnown := (LaFind(W, 'rpcjwa') > 0) Or (LaFindNoun(W, Mk) > 0) Or
      (LaFindAdj(W, Mk, Gs) > 0) Or
             LaMatchVerb(W, Idx, T, Sl);
End;

Procedure LaTokenize(Const S0: String);
Var
  S, Word1, Low, Base: String;
  I, L, K: Integer;
  C: Char;
  Cap: Boolean;
Begin
  SN := 0;
  SEnd := #0;
  LaQ := False;
  S := '';
  I := 1;
  While I <= Length(S0) Do
  Begin
    If (S0[I] = #$C2) And (I < Length(S0)) And ((S0[I + 1] = #$AB) Or (S0[I + 1]
        = #$BB) Or (S0[I + 1] = #$A0)) Then
    Begin
      S := S + ' ';
      Inc(I, 2);
    End
    Else If (S0[I] = #$E2) And (I + 2 <= Length(S0)) And (S0[I + 1] = #$80) And
            ((S0[I + 2] = #$99) Or (S0[I + 2] = #$98)) Then
    Begin
      S := S + ' ';
      Inc(I, 3);
    End
    Else
    Begin
      S := S + S0[I];
      Inc(I);
    End;
  End;
  L := Length(S);
  I := 1;
  While I <= L Do
  Begin
    C := S[I];
    If FrWordByte(C) Then
    Begin
      Word1 := '';
      While (I <= L) And FrWordByte(S[I]) Do
      Begin
        Word1 := Word1 + S[I];
        Inc(I);
      End;
      Cap := (Word1 <> '') And (((Word1[1] >= 'A') And (Word1[1] <= 'Z')) Or
             ((Word1[1] = #$C3) And (Length(Word1) > 1) And (Word1[2] >= #$80)
                 And (Word1[2] <= #$9E)));
      Low := LaNormalize(FrLower(Word1));
      { enclitiques : -que (et), -ne (question) }
      If (Length(Low) > 4) And EndsWith(Low, 'que') And (Not LaKnown(Low)) And
          LaKnown(Copy(Low, 1, Length(Low) - 3)) Then
      Begin
        Base := Copy(Low, 1, Length(Low) - 3);
        SPush('et', 'et', False, False, False);
        SPush(Base, Copy(Word1, 1, Length(Word1) - 3), Cap, False, False);
      End
      Else If (Length(Low) > 3) And EndsWith(Low, 'ne') And (SN = 0) And (Not
          LaKnown(Low)) And
              LaKnown(Copy(Low, 1, Length(Low) - 2)) Then
      Begin
        LaQ := True;
        SPush(Copy(Low, 1, Length(Low) - 2), Copy(Word1, 1, Length(Word1) - 2),
            Cap, False, False);
      End
      Else
        SPush(Low, Word1, Cap, False, False);
    End
    Else
    Begin
      If (C = '.') Or (C = '?') Or (C = '!') Then
      Begin
        If SEnd = #0 Then SEnd := C;
        If C = '?' Then SEnd := '?';
      End
      Else If (C = ',') Or (C = ';') Or (C = ':') Then
      Begin
        SPush(',', ',', False, True, False);
        ST[SN].Cat := ',';
      End;
      Inc(I);
    End;
  End;
  If LaQ Then SEnd := '?';
  K := 0;
End;

Function LaNumberEn(Const W: String): String;
Const
  N: Array[0..14] Of String = ('unus', 'una', 'unum', 'duo', 'duae', 'tres',
      'tria', 'quattuor',
    'quinque', 'sex', 'septem', 'octo', 'novem', 'decem', 'centum');
  E: Array[0..14] Of String = ('one', 'one', 'one', 'two', 'two', 'three',
      'three', 'four',
    'five', 'six', 'seven', 'eight', 'nine', 'ten', 'one hundred');
Var
  K: Integer;
Begin
  LaNumberEn := '';
  For K := 0 To 14 Do
    If W = N[K] Then LaNumberEn := E[K];
  If W = 'mille' Then LaNumberEn := 'one thousand';
End;

Var
  LaAmb: Array[0..MaxTokens + 2] Of Boolean;

Procedure LaPickCat(I: Integer; NoVerb: Boolean);
Var
  W: String;
  Idx, Sl, N, Mk: Integer;
  T: Char;
  Gs: String;
  LN, LA: Integer;
Begin
  W := ST[I].W;
  LaMk[I] := 4095;
  LaGn[I] := 'mfn';
  If ST[I].Punct Then
  Begin
    ST[I].Cat := ',';
    Exit;
  End;
  If IsDigitsOnly(W) Then
  Begin
    ST[I].Cat := 'U';
    ST[I].En := W;
    Exit;
  End;
  If LaNumberEn(W) <> '' Then
  Begin
    ST[I].Cat := 'U';
    ST[I].En := LaNumberEn(W);
    Exit;
  End;
  If W = 'non' Then
  Begin
    ST[I].Cat := 'G';
    Exit;
  End;
  If (W = 'numquam') Or (W = 'nunquam') Then
  Begin
    ST[I].Cat := 'R';
    ST[I].En := 'never';
    Exit;
  End;
  If W = 'nihil' Then
  Begin
    ST[I].Cat := 'g';
    ST[I].En := 'anything';
    Exit;
  End;
  If W = 'nemo' Then
  Begin
    ST[I].Cat := 'g';
    ST[I].En := 'anyone';
    Exit;
  End;
  { pronoms personnels }
  If InList(W, 'ego tu nos vos is ea id') Then
  Begin
    ST[I].Cat := 'P';
    LaMk[I] := 1 Or (1 Shl 6);
    If W = 'ego' Then Begin ST[I].En := 'I'; ST[I].Slot := 1; End
    Else If W = 'tu' Then Begin ST[I].En := 'you'; ST[I].Slot := 2; End
    Else If W = 'nos' Then Begin ST[I].En := 'we'; ST[I].Slot := 4; LaMk[I] :=
        (1 Shl 6) Or (1 Shl 9); End
    Else If W = 'vos' Then Begin ST[I].En := 'you'; ST[I].Slot := 5; LaMk[I] :=
        (1 Shl 6) Or (1 Shl 9); End
    Else If W = 'is' Then Begin ST[I].En := 'he'; ST[I].Slot := 3; End
    Else If W = 'ea' Then Begin ST[I].En := 'she'; ST[I].Slot := 3; End
    Else Begin ST[I].En := 'it'; ST[I].Slot := 3; End;
    Exit;
  End;
  { pronoms complements : accusatif et datif }
  If InList(W, 'me te se eum eam eos eas') Then
  Begin
    ST[I].Cat := 'B';
    LaMk[I] := 8 Or (1 Shl 9);
    If W = 'me' Then ST[I].En := 'me'
    Else If W = 'te' Then ST[I].En := 'you'
    Else If W = 'eum' Then ST[I].En := 'him'
    Else If W = 'eam' Then ST[I].En := 'her'
    Else If W = 'se' Then ST[I].En := 'himself'
    Else ST[I].En := 'them';
    ST[I].Kind := 'a';
    Exit;
  End;
  If InList(W, 'mihi tibi nobis vobis ei eis sibi') Then
  Begin
    ST[I].Cat := 'B';
    LaMk[I] := 4 Or (1 Shl 8);
    If W = 'mihi' Then ST[I].En := 'me'
    Else If W = 'tibi' Then ST[I].En := 'you'
    Else If W = 'nobis' Then ST[I].En := 'us'
    Else If W = 'vobis' Then ST[I].En := 'you'
    Else If W = 'ei' Then ST[I].En := 'him'
    Else ST[I].En := 'them';
    ST[I].Kind := 'd';
    Exit;
  End;
  { demonstratifs }
  If InList(W,
      'hic haec hoc hunc hanc huius huic hi hae hos has his horum harum') Then
  Begin
    If (W = 'hic') And ((I = SN) Or Not (ST[I + 1].Punct)) And False Then Exit;
    ST[I].Cat := 'T';
    ST[I].En := 'this';
    If InList(W, 'hi hae hos has his horum harum') Or ((W = 'haec') And (I < SN)
        And False) Then
    Begin
      ST[I].En := 'these';
      ST[I].Plu := True;
    End;
    Exit;
  End;
  If InList(W,
      'ille illa illud illum illam illius illi illos illas illis illorum illar'+
      'um iste ista istud') Then
  Begin
    ST[I].Cat := 'T';
    ST[I].En := 'that';
    If InList(W, 'illos illas illorum illarum') Then
    Begin
      ST[I].En := 'those';
      ST[I].Plu := True;
    End;
    Exit;
  End;
  { quantificateurs }
  If InList(W,
      'omnes omnia omnibus omnium omnis omne multi multae multa pauci paucae p'+
      'auca nonnulli nonnullae nullus nulla nullum') Then
  Begin
    ST[I].Cat := 'Q';
    ST[I].Plu := InList(W,
        'omnes omnia omnibus omnium multi multae multa pauci paucae pauca nonn'+
        'ulli nonnullae');
    If Copy(W, 1, 4) = 'omne' Then Begin ST[I].En := 'every'; End
    Else If Copy(W, 1, 4) = 'omni' Then
    Begin
      If W = 'omnis' Then ST[I].En := 'every' Else ST[I].En := 'all';
    End
    Else If Copy(W, 1, 4) = 'mult' Then ST[I].En := 'many'
    Else If Copy(W, 1, 4) = 'pauc' Then ST[I].En := 'few'
    Else If Copy(W, 1, 4) = 'nonn' Then ST[I].En := 'some'
    Else ST[I].En := 'no';
    If (ST[I].En = 'all') And (W <> 'omnes') Then ST[I].En := 'all';
    Exit;
  End;
  { mots grammaticaux }
  Idx := LaFind(W, 'w');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'W';
    ST[I].En := LaLex[Idx].En;
    If (W = 'ubi') Or (W = 'quo') Then ST[I].W := 'donde'
    Else If W = 'quis' Then ST[I].W := 'quien'
    Else If W = 'quando' Then ST[I].W := 'cuando'
    Else If W = 'quomodo' Then ST[I].W := 'como'
    Else If W = 'cur' Then ST[I].W := 'por que'
    Else If W = 'quot' Then ST[I].W := 'cuantos'
    Else ST[I].W := 'que';
    Exit;
  End;
  Idx := LaFind(W, 'p');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'E';
    ST[I].En := LaLex[Idx].En;
    If (W = 'ad') Then ST[I].W := 'a'
    Else If W = 'in' Then ST[I].W := 'en'
    Else If InList(W, 'de ex e ab a') Then ST[I].W := 'desde'
    Else If W = 'sine' Then ST[I].W := 'sin';
    Exit;
  End;
  Idx := LaFind(W, 'c');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'C';
    ST[I].En := LaLex[Idx].En;
    Exit;
  End;
  Idx := LaFind(W, 'j');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'J';
    ST[I].En := LaLex[Idx].En;
    Exit;
  End;
  Idx := LaFind(W, 'r');
  If Idx > 0 Then
  Begin
    ST[I].Cat := 'R';
    ST[I].En := LaLex[Idx].En;
    Exit;
  End;
  { verbes, noms, adjectifs }
  LaAmb[I] := False;
  If (Not NoVerb) And LaMatchVerb(W, Idx, T, Sl) Then
  Begin
    LaAmb[I] := (LaFindNoun(W, Mk) > 0) Or (LaFindAdj(W, Mk, Gs) > 0);
    ST[I].Cat := 'V';
    ST[I].Idx := LaLex[Idx].SpIdx;
    ST[I].Tns := T;
    ST[I].Slot := Sl;
    ST[I].En := LaLex[Idx].En;
    Exit;
  End;
  LN := LaFindNoun(W, Mk);
  If LN > 0 Then
  Begin
    ST[I].Cat := 'N';
    ST[I].En := LaLex[LN].En;
    LaMk[I] := Mk;
    LaGn[I] := LaNounGender(LN);
    If LaLex[LN].Key = 'P' Then
    Begin
      ST[I].Cat := 'Z';
      ST[I].Orig := LaLex[LN].En;
      ST[I].Cap := True;
    End;
    Exit;
  End;
  LA := LaFindAdj(W, Mk, Gs);
  If LA > 0 Then
  Begin
    ST[I].Cat := 'A';
    ST[I].En := LaLex[LA].En;
    LaMk[I] := Mk;
    LaGn[I] := Gs;
    If InList(LaLex[LA].En, 'my your our his their') Then ST[I].Cat := 'S';
    Exit;
  End;
  ST[I].Cat := 'Z';
  N := 0;
End;

{ ------------------------------------------------------------------ }
{ LATIN -> ANGLAIS : analyse en groupes, remise en ordre sujet-verbe-   }
{ complements, puis traduction                                         }
{ ------------------------------------------------------------------ }
{ Le latin a un ordre des mots libre : on repere les groupes nominaux, }
{ on attribue a chacun un role d'apres sa terminaison (nominatif =     }
{ sujet, accusatif = objet, datif = "a", genitif = "de", ablatif = avec),}
{ puis on range les jetons dans l'ordre sujet - verbe - complements    }
{ que le moteur de proposition sait traiter.                           }

Const
  LaMaxCh = 32;

Type
  TLaCh = Record
    F, L: Integer;     { plage de jetons du groupe (sans la preposition) }
    PrepTok: Integer;  { jeton de la preposition, 0 sinon }
    Kind: Char; { N groupe nominal, B pronom complement, A adjectif seul, V
        verbe, o autre }
    Mk: Integer;
    Role: Char;
    HeadChunk: Integer;
    Done: Boolean;
  End;

Var
  LaVoc: String;
  LaVocFirst: Boolean;
  LaHold: Array[0..MaxTokens + 2] Of TSTok;
  LaSrc: Array[0..MaxTokens + 2] Of TSTok;
  LaSrcMk: Array[0..MaxTokens + 2] Of Integer;
  LaSrcGn: Array[0..MaxTokens + 2] Of String[3];
  LaSrcN: Integer;
  LaOut: Array[0..MaxTokens + 2] Of TSTok;
  LaOutN: Integer;

Procedure LaEmit(Const T: TSTok);
Begin
  If LaOutN < MaxTokens Then
  Begin
    Inc(LaOutN);
    LaOut[LaOutN] := T;
  End;
End;

Procedure LaEmitSimple(C: Char; Const W, En: String);
Var
  T: TSTok;
Begin
  FillChar(T, SizeOf(T), 0);
  T.Cat := C;
  T.W := W;
  T.Orig := W;
  T.En := En;
  T.Tns := ' ';
  T.Punct := C = ',';
  LaEmit(T);
End;

{ nombre d'un groupe : vrai si pluriel }
Function LaIsPlural(Mk: Integer; Prefer: Boolean): Boolean;
Var
  Sg, Pl: Boolean;
Begin
  Sg := (Mk And 63) <> 0;
  Pl := ((Mk Shr 6) And 63) <> 0;
  If Sg And Not Pl Then LaIsPlural := False
  Else If Pl And Not Sg Then LaIsPlural := True
  Else LaIsPlural := Prefer;
End;

Function LaChunkHasHead(Const Ch: TLaCh; Var HeadI: Integer): Boolean;
Var
  K: Integer;
Begin
  HeadI := 0;
  For K := Ch.F To Ch.L Do
    If LaSrc[K].Cat In ['N', 'Z'] Then HeadI := K;
  LaChunkHasHead := HeadI > 0;
End;

{ Emet un groupe nominal : Mode d defini, i indefini, n sans article }
Procedure LaEmitNP(Const Ch: TLaCh; Mode: Char; Plu: Boolean);
Var
  K, HeadI: Integer;
  HasDet: Boolean;
  T: TSTok;
Begin
  HasDet := False;
  For K := Ch.F To Ch.L Do
    If LaSrc[K].Cat In ['T', 'S', 'Q', 'U'] Then HasDet := True;
  LaChunkHasHead(Ch, HeadI);
  For K := Ch.F To Ch.L Do
    If LaSrc[K].Cat In ['T', 'S', 'Q', 'U'] Then LaEmit(LaSrc[K]);
  If (Not HasDet) And (HeadI > 0) And (LaSrc[HeadI].Cat = 'N') And (Mode <> 'n')
      Then
  Begin
    FillChar(T, SizeOf(T), 0);
    T.Cat := 'D';
    T.Kind := Mode;
    T.W := 'el';
    T.Orig := 'el';
    T.Plu := Plu;
    T.Tns := ' ';
    LaEmit(T);
  End;
  For K := Ch.F To Ch.L Do
    If LaSrc[K].Cat = 'A' Then
    Begin
      T := LaSrc[K];
      T.Plu := Plu;
      LaEmit(T);
    End;
  For K := Ch.F To Ch.L Do
    If LaSrc[K].Cat In ['N', 'Z', 'B', 'P'] Then
    Begin
      T := LaSrc[K];
      T.Plu := Plu;
      LaEmit(T);
    End;
End;

Function LaOwnerText(Const Ch: TLaCh; Plu: Boolean; Definite: Boolean): String;
Var
  K: Integer;
  R, N: String;
Begin
  R := '';
  For K := Ch.F To Ch.L Do
    If LaSrc[K].Cat In ['S', 'T', 'Q', 'U'] Then R := Join(R, LaSrc[K].En);
  If (R = '') And Definite Then R := 'the';
  For K := Ch.F To Ch.L Do
    If LaSrc[K].Cat = 'A' Then R := Join(R, LaSrc[K].En);
  For K := Ch.F To Ch.L Do
    If LaSrc[K].Cat = 'Z' Then
    Begin
      LaOwnerText := LaSrc[K].Orig;
      Exit;
    End
    Else If LaSrc[K].Cat = 'N' Then
    Begin
      N := LaSrc[K].En;
      If Plu Then N := EnPlural(N);
      R := Join(R, N);
    End;
  LaOwnerText := R;
End;

Procedure LaSegment(A, B: Integer);
Var
  Ch: Array[1..LaMaxCh] Of TLaCh;
  NC, I, J, K, Vc, SubjC, PredC, Slot: Integer;
  Mk: Integer;
  HasN, Stop: Boolean;
  SubjPl: Boolean;
  VerbSer: Boolean;
  T: TSTok;
  Txt: String;
  HeadI, Hc: Integer;
  MovedAdj, DatDone, HasVerbSeg: Boolean;

  Function Nomi(M: Integer): Boolean;
  Begin
    Nomi := (M And (1 Or (1 Shl 6))) <> 0;
  End;

  Function NomAgree(M: Integer): Boolean;
  Begin
    If SubjPl Then NomAgree := (M And (1 Shl 6)) <> 0 Else NomAgree := (M And 1)
        <> 0;
  End;

  Procedure CollectNP(Start: Integer; Var Last: Integer; Var M: Integer; Var
      Found: Boolean);
  Var
    P: Integer;
    HaveNoun, HaveProper: Boolean;
    ChG: String;
  Begin
    P := Start;
    M := 4095;
    HaveNoun := False;
    HaveProper := False;
    ChG := '';
    Found := False;
    While P <= B Do
    Begin
      If LaSrc[P].Cat In ['N', 'Z'] Then
      Begin
        { un seul nom par groupe, sauf apposition d'un nom propre }
        If HaveNoun And (Not HaveProper) And (LaSrc[P].Cat = 'N') Then Break;
        If HaveNoun And ((M And LaSrcMk[P]) = 0) Then Break;
        M := M And LaSrcMk[P];
        If (ChG = '') And (LaSrc[P].Cat = 'N') Then ChG := LaSrcGn[P];
        HaveNoun := True;
        If LaSrc[P].Cat = 'Z' Then HaveProper := True;
        Found := True;
      End
      Else If LaSrc[P].Cat = 'A' Then
      Begin
        If (M And LaSrcMk[P]) = 0 Then Break;
        If (ChG <> '') And (Pos(ChG[1], LaSrcGn[P]) = 0) Then Break;
        M := M And LaSrcMk[P];
        Found := True;
      End
      Else If LaSrc[P].Cat In ['T', 'S', 'Q', 'U'] Then
        Found := True
      Else
        Break;
      Inc(P);
    End;
    Last := P - 1;
  End;

  Procedure AddChunk(F, L, Prep: Integer; Kind: Char; M: Integer);
  Begin
    If NC < LaMaxCh Then
    Begin
      Inc(NC);
      FillChar(Ch[NC], SizeOf(TLaCh), 0);
      Ch[NC].F := F;
      Ch[NC].L := L;
      Ch[NC].PrepTok := Prep;
      Ch[NC].Kind := Kind;
      Ch[NC].Mk := M;
      Ch[NC].Role := ' ';
    End;
  End;

  Procedure MaybeConj(Idx: Integer);
  Begin
    If (Idx >= 3) And (Ch[Idx - 1].Kind = 'o') And (LaSrc[Ch[Idx - 1].F].Cat =
        'C') And
       (Not Ch[Idx - 1].Done) And Ch[Idx - 2].Done And (Ch[Idx - 2].Kind =
           Ch[Idx].Kind) Then
    Begin
      LaEmit(LaSrc[Ch[Idx - 1].F]);
      Ch[Idx - 1].Done := True;
    End;
  End;

Begin
  NC := 0;
  I := A;
  While I <= B Do
  Begin
    Case LaSrc[I].Cat Of
      'N', 'Z', 'A', 'T', 'S', 'Q', 'U':
      Begin
        CollectNP(I, K, Mk, HasN);
        If K < I Then K := I;
        HasN := False;
        For J := I To K Do
          If LaSrc[J].Cat In ['N', 'Z'] Then HasN := True;
        If HasN Then AddChunk(I, K, 0, 'N', Mk)
        Else If (LaSrc[I].Cat = 'A') Then AddChunk(I, K, 0, 'A', Mk)
        Else AddChunk(I, K, 0, 'N', 4095);
        I := K + 1;
      End;
      'E':
      Begin
        If (I < B) And (LaSrc[I + 1].Cat In ['N', 'Z', 'A', 'T', 'S', 'Q', 'U'])
            Then
        Begin
          CollectNP(I + 1, K, Mk, HasN);
          If K < I + 1 Then K := I + 1;
          AddChunk(I + 1, K, I, 'N', Mk);
          Ch[NC].Role := 'X';
          I := K + 1;
        End
        Else If (I < B) And (LaSrc[I + 1].Cat = 'B') Then
        Begin
          AddChunk(I + 1, I + 1, I, 'B', 4095);
          Ch[NC].Role := 'X';
          I := I + 2;
        End
        Else
        Begin
          AddChunk(I, I, 0, 'o', 4095);
          Inc(I);
        End;
      End;
      'P':
      Begin
        AddChunk(I, I, 0, 'N', LaSrcMk[I]);
        Inc(I);
      End;
      'B':
      Begin
        AddChunk(I, I, 0, 'B', LaSrcMk[I]);
        Inc(I);
      End;
      'V':
      Begin
        AddChunk(I, I, 0, 'V', 0);
        Inc(I);
      End;
    Else
      AddChunk(I, I, 0, 'o', 4095);
      Inc(I);
    End;
  End;
  { vocatif sans verbe : Puer ! }
  If (NC = 1) And (Ch[1].Kind = 'N') And (Ch[1].PrepTok = 0) Then
  Begin
    HasN := False;
    For I := 1 To NC Do HasN := HasN Or (LaSrc[Ch[1].F].Cat In ['N', 'Z', 'A']);
    If HasN And (A > 1) Or (B < LaSrcN) Then
    Begin
      HasVerbSeg := False;
      For K := A To B Do
        If LaSrc[K].Cat = 'V' Then HasVerbSeg := True;
      If Not HasVerbSeg Then
      Begin
        LaVoc := LaOwnerText(Ch[1], LaIsPlural(Ch[1].Mk, False), False);
        LaVocFirst := A = 1;
        Exit;
      End;
    End;
  End;
  { verbe fini }
  Vc := 0;
  For I := 1 To NC Do
    If (Vc = 0) And (Ch[I].Kind = 'V') And (LaSrc[Ch[I].F].Tns In ['p', 'm',
        'f', 'i', 'M']) Then Vc := I;
  If Vc = 0 Then
    For I := 1 To NC Do
      If (Vc = 0) And (Ch[I].Kind = 'V') Then Vc := I;
  Slot := 3;
  VerbSer := False;
  If Vc > 0 Then
  Begin
    Slot := LaSrc[Ch[Vc].F].Slot;
    If Slot < 1 Then Slot := 3;
    VerbSer := SpPlain(SpLex[LaSrc[Ch[Vc].F].Idx].Es) = 'ser';
  End;
  SubjPl := Slot >= 4;
  { sujet }
  SubjC := 0;
  PredC := 0;
  If (Vc > 0) And (LaSrc[Ch[Vc].F].Tns <> 'M') And (LaSrc[Ch[Vc].F].Tns <> 'I')
      Then
  Begin
    For I := 1 To NC Do
      If (SubjC = 0) And (Ch[I].Kind = 'N') And (Ch[I].PrepTok = 0) And
         ((Slot In [3, 6]) Or (LaSrc[Ch[I].F].Cat = 'P')) Then
        If SubjPl Then
        Begin
          If (Ch[I].Mk And (1 Shl 6)) <> 0 Then SubjC := I;
        End
        Else If (Ch[I].Mk And 1) <> 0 Then SubjC := I;
    If (SubjC = 0) And SubjPl Then
      { coordination : puer et puella currunt }
      For I := 1 To NC - 2 Do
        If (SubjC = 0) And (Ch[I].Kind = 'N') And (Ch[I + 1].Kind = 'o') And
            (LaSrc[Ch[I + 1].F].Cat = 'C') And
           (LaSrc[Ch[I + 1].F].En = 'and') And (Ch[I + 2].Kind = 'N') And
               Nomi(Ch[I].Mk) And Nomi(Ch[I + 2].Mk) Then
          SubjC := I;
    If VerbSer And (SubjC = 0) And (Slot In [1, 2, 4, 5]) Then
      For I := 1 To NC Do
        If (PredC = 0) And (Ch[I].Kind = 'N') And (Ch[I].PrepTok = 0) And
            NomAgree(Ch[I].Mk) And
           (LaSrc[Ch[I].F].Cat <> 'P') And ((Ch[I].Mk And Not ((1 Shl 5) Or (1
               Shl 11))) <> 0) And
           (Ch[I].Mk <> (1 Shl 5)) Then PredC := I;
    If VerbSer And (SubjC > 0) Then
      For I := 1 To NC Do
        If (PredC = 0) And (I <> SubjC) And (Ch[I].Kind = 'N') And
            (Ch[I].PrepTok = 0) And NomAgree(Ch[I].Mk) And
           (I <> SubjC + 2) Then PredC := I;
  End;
  If SubjC > 0 Then Ch[SubjC].Role := 'S';
  If PredC > 0 Then Ch[PredC].Role := 'P';
  { personne 1 ou 2 : un nom au nominatif est un vocatif }
  If (Vc > 0) And (Slot In [1, 2, 4, 5]) Then
    For I := 1 To NC Do
      If (Ch[I].Kind = 'N') And (Ch[I].PrepTok = 0) And (Ch[I].Role = ' ') And
          (LaSrc[Ch[I].F].Cat <> 'P') And
         (LaSrc[Ch[I].L].Cat In ['N', 'Z']) And Nomi(Ch[I].Mk) Then Ch[I].Role
             := 'v';
  DatDone := False;
  { roles des autres groupes nominaux }
  For I := 1 To NC Do
    If (Ch[I].Kind = 'N') And (Ch[I].Role = ' ') And (Ch[I].PrepTok = 0) Then
    Begin
      If (I = SubjC + 2) And (SubjC > 0) And (LaSrc[Ch[SubjC + 1].F].Cat = 'C')
          And (Ch[SubjC + 1].Kind = 'o') And SubjPl
         And (Not Nomi(0)) And Nomi(Ch[I].Mk) Then
        Ch[I].Role := 'c'
      Else If (Ch[I].Mk And Not ((1 Shl 5) Or (1 Shl 11))) = 0 Then Ch[I].Role
          := 'v'
      Else If (Ch[I].Mk And ((1 Shl 3) Or (1 Shl 9))) <> 0 Then Ch[I].Role :=
          'O'
      Else If (Vc > 0) And (Not DatDone) And ((Ch[I].Mk And ((1 Shl 2) Or (1 Shl
          8))) <> 0) And
              InList(LaSrc[Ch[Vc].F].En,
                  'give show tell send teach say offer sell bring') Then
      Begin
        Ch[I].Role := 'I';
        DatDone := True;
      End
      Else If (Ch[I].Mk And ((1 Shl 1) Or (1 Shl 7))) <> 0 Then
      Begin
        { genitif si un groupe nominal voisin peut etre sa tete }
        If ((I > 1) And (Ch[I - 1].Kind = 'N') And (Ch[I - 1].PrepTok = 0)) Or
           ((I < NC) And (Ch[I + 1].Kind = 'N') And (Ch[I + 1].PrepTok = 0)) Or
           ((Ch[I].Mk And ((1 Shl 2) Or (1 Shl 8))) = 0) Then
          Ch[I].Role := 'G'
        Else Ch[I].Role := 'I';
      End
      Else If (Ch[I].Mk And ((1 Shl 2) Or (1 Shl 8))) <> 0 Then Ch[I].Role :=
          'I'
      Else If (Ch[I].Mk And ((1 Shl 4) Or (1 Shl 10))) <> 0 Then Ch[I].Role :=
          'X'
      Else Ch[I].Role := 'O';
    End
    Else If (Ch[I].Kind = 'B') And (Ch[I].PrepTok = 0) Then
    Begin
      If LaSrc[Ch[I].F].Kind = 'd' Then Ch[I].Role := 'I' Else Ch[I].Role :=
          'O';
    End;
  { le groupe au genitif : tete = groupe nominal precedent, sinon suivant }
  For I := 1 To NC Do
    If Ch[I].Role = 'G' Then
    Begin
      Ch[I].HeadChunk := 0;
      If (I > 1) And (Ch[I - 1].Kind = 'N') And (Ch[I - 1].Role <> 'G') Then
          Ch[I].HeadChunk := I - 1
      Else If (I < NC) And (Ch[I + 1].Kind = 'N') And (Ch[I + 1].Role <> 'G')
          Then Ch[I].HeadChunk := I + 1;
      If Ch[I].HeadChunk = 0 Then Ch[I].Role := 'I';
    End;
  { --- emission --- }
  { 1. interjections, conjonctions et interrogatifs de tete }
  For I := 1 To NC Do
    If (Ch[I].Kind = 'o') And (LaSrc[Ch[I].F].Cat In ['J', 'C', 'W', ',']) And
        (Not Ch[I].Done) Then
      If (Ch[I].F = A) Or (LaSrc[Ch[I].F].Cat = 'W') Or ((LaSrc[Ch[I].F].Cat =
          'C') And (I <= 2)) Then
      Begin
        If Not ((SubjC > 0) And (I = SubjC + 1) And (LaSrc[Ch[I].F].En = 'and')
            And SubjPl) Then
        Begin
          LaEmit(LaSrc[Ch[I].F]);
          Ch[I].Done := True;
        End;
      End;
  { puer bonus est : les adjectifs du sujet deviennent l'attribut }
  MovedAdj := False;
  If VerbSer And (SubjC > 0) And (PredC = 0) Then
  Begin
    HasN := False;
    For I := 1 To NC Do
      If (I <> SubjC) And ((Ch[I].Kind = 'A') Or ((Ch[I].Kind = 'N') And
          (Ch[I].PrepTok = 0) And (Ch[I].Role <> 'G'))) Then HasN := True;
    If Not HasN Then
      For K := Ch[SubjC].F To Ch[SubjC].L Do
        If LaSrc[K].Cat = 'A' Then MovedAdj := True;
  End;
  { vocatif }
  For I := 1 To NC Do
    If (Ch[I].Role = 'v') And (Not Ch[I].Done) Then
    Begin
      LaVoc := LaOwnerText(Ch[I], LaIsPlural(Ch[I].Mk, False), False);
      LaVocFirst := Ch[I].F = 1;
      Ch[I].Done := True;
    End;
  { 2. sujet (avec coordination eventuelle) }
  If SubjC > 0 Then
  Begin
    If MovedAdj Then
    Begin
      For K := Ch[SubjC].F To Ch[SubjC].L Do
        If LaSrc[K].Cat = 'A' Then
        Begin
          LaHold[K] := LaSrc[K];
          LaSrc[K].Cat := ' ';
        End;
    End;
    LaEmitNP(Ch[SubjC], 'd', LaIsPlural(Ch[SubjC].Mk, SubjPl));
    Ch[SubjC].Done := True;
    { genitifs attaches }
    For I := 1 To NC Do
      If (Ch[I].Role = 'G') And (Ch[I].HeadChunk = SubjC) And (Not Ch[I].Done)
          Then
      Begin
        Txt := LaOwnerText(Ch[I], LaIsPlural(Ch[I].Mk, False), True);
        LaEmitSimple('E', 'de', 'of');
        FillChar(T, SizeOf(T), 0);
        T.Cat := 'Z';
        T.W := 'x';
        T.Orig := Txt;
        T.Cap := True;
        T.Tns := ' ';
        LaEmit(T);
        Ch[I].Done := True;
      End;
    If (SubjC + 2 <= NC) And (LaSrc[Ch[SubjC + 1].F].Cat = 'C') And (Ch[SubjC +
        1].Kind = 'o') And SubjPl And
       (Ch[SubjC + 2].Kind = 'N') Then
    Begin
      LaEmit(LaSrc[Ch[SubjC + 1].F]);
      Ch[SubjC + 1].Done := True;
      LaEmitNP(Ch[SubjC + 2], 'd', False);
      Ch[SubjC + 2].Done := True;
    End;
  End;
  { 3. negation puis verbe fini puis infinitifs }
  For I := 1 To NC Do
    If (Ch[I].Kind = 'o') And (LaSrc[Ch[I].F].Cat = 'G') And (Not Ch[I].Done)
        Then
    Begin
      LaEmit(LaSrc[Ch[I].F]);
      Ch[I].Done := True;
    End;
  If Vc > 0 Then
  Begin
    LaEmit(LaSrc[Ch[Vc].F]);
    Ch[Vc].Done := True;
  End;
  For I := 1 To NC Do
    If (Ch[I].Kind = 'V') And (Not Ch[I].Done) Then
    Begin
      LaEmit(LaSrc[Ch[I].F]);
      Ch[I].Done := True;
    End;
  { 4. attribut (apres "sum") puis adjectifs seuls }
  If MovedAdj Then
    For K := Ch[SubjC].F To Ch[SubjC].L Do
      If LaSrc[K].Cat = ' ' Then
      Begin
        T := LaHold[K];
        T.Plu := SubjPl;
        LaEmit(T);
      End;
  For I := 1 To NC Do
    If (Ch[I].Role = 'P') And (Not Ch[I].Done) Then
    Begin
      LaEmitNP(Ch[I], 'i', LaIsPlural(Ch[I].Mk, SubjPl));
      Ch[I].Done := True;
    End;
  For I := 1 To NC Do
    If (Ch[I].Kind = 'A') And (Not Ch[I].Done) Then
    Begin
      MaybeConj(I);
      LaEmitNP(Ch[I], 'n', LaIsPlural(Ch[I].Mk, SubjPl));
      Ch[I].Done := True;
    End;
  { 5. objets directs, pronoms }
  For I := 1 To NC Do
    If (Ch[I].Role = 'O') And (Not Ch[I].Done) Then
    Begin
      MaybeConj(I);
      If Ch[I].Kind = 'B' Then
        LaEmit(LaSrc[Ch[I].F])
      Else
      Begin
        If (LaSrc[Ch[I].F].Cat = 'Z') And (Vc > 0) And
           InList(LaSrc[Ch[Vc].F].En,
               'come go arrive return flee walk run travel') Then
          LaEmitSimple('E', 'a', 'to');
        LaEmitNP(Ch[I], 'd', LaIsPlural(Ch[I].Mk, False));
      End;
      Ch[I].Done := True;
      For J := 1 To NC Do
        If (Ch[J].Role = 'G') And (Ch[J].HeadChunk = I) And (Not Ch[J].Done)
            Then
        Begin
          Txt := LaOwnerText(Ch[J], LaIsPlural(Ch[J].Mk, False), True);
          LaEmitSimple('E', 'de', 'of');
          FillChar(T, SizeOf(T), 0);
          T.Cat := 'Z';
          T.W := 'x';
          T.Orig := Txt;
          T.Cap := True;
          T.Tns := ' ';
          LaEmit(T);
          Ch[J].Done := True;
        End;
    End;
  { 6. datif : "a" + groupe }
  For I := 1 To NC Do
    If (Ch[I].Role = 'I') And (Not Ch[I].Done) Then
    Begin
      MaybeConj(I);
      LaEmitSimple('E', 'a', 'to');
      If Ch[I].Kind = 'B' Then LaEmit(LaSrc[Ch[I].F])
      Else LaEmitNP(Ch[I], 'd', LaIsPlural(Ch[I].Mk, False));
      Ch[I].Done := True;
    End;
  { 7. groupes prepositionnels et ablatifs, dans l'ordre latin }
  For I := 1 To NC Do
    If (Ch[I].Role = 'X') And (Not Ch[I].Done) Then
    Begin
      MaybeConj(I);
      If Ch[I].PrepTok > 0 Then
      Begin
        T := LaSrc[Ch[I].PrepTok];
        { in + accusatif : mouvement vers }
        If (T.En = 'in') And ((Ch[I].Mk And ((1 Shl 4) Or (1 Shl 10))) = 0) And
           ((Ch[I].Mk And ((1 Shl 3) Or (1 Shl 9))) <> 0) Then
        Begin
          T.En := 'into';
          T.W := 'a';
        End;
        LaEmit(T);
      End
      Else LaEmitSimple('E', 'con', 'with');
      If Ch[I].Kind = 'B' Then LaEmit(LaSrc[Ch[I].F])
      Else LaEmitNP(Ch[I], 'd', LaIsPlural(Ch[I].Mk, False));
      Ch[I].Done := True;
    End;
  { 8. tout le reste (adverbes, ...) }
  For I := 1 To NC Do
    If Not Ch[I].Done Then
    Begin
      For K := Ch[I].F To Ch[I].L Do LaEmit(LaSrc[K]);
      Ch[I].Done := True;
    End;
  Hc := 0;
  HeadI := 0;
End;

Procedure LaClassify;
Var
  I, A, J, NFin, SaveN: Integer;
  HasFin: Boolean;
Begin
  For I := 1 To SN Do
    LaPickCat(I, False);
  { verbe ou nom (servo, regis...) : un seul verbe fini par proposition }
  NFin := 0;
  For I := 1 To SN Do
    If (ST[I].Cat = 'V') And (ST[I].Tns In ['p', 'm', 'f', 'i', 'M']) Then
        Inc(NFin);
  If NFin > 1 Then
    For I := 1 To SN Do
      If (NFin > 1) And (ST[I].Cat = 'V') And LaAmb[I] Then
      Begin
        LaPickCat(I, True);
        Dec(NFin);
      End;
  { Veni ! = viens ! }
  If SEnd = '!' Then
  Begin
    For I := 1 To SN Do
      If (ST[I].Cat = 'P') And (ST[I].W = 'ego') Then NFin := -100;
    If NFin > -100 Then
      For I := 1 To SN Do
        If (ST[I].Cat = 'V') And (ST[I].Tns = 'i') And (ST[I].Slot = 1) And
            (NFin > -50) Then
        Begin
          ST[I].Tns := 'M';
          ST[I].Slot := 2;
          NFin := -50;
        End;
  End;
  { hic seul : ici }
  For I := 1 To SN Do
    If (ST[I].Cat = 'T') And (ST[I].W = 'hic') And ((I = SN) Or Not (ST[I +
        1].Cat In ['N', 'A', 'Z'])) Then
    Begin
      ST[I].Cat := 'R';
      ST[I].En := 'here';
    End;
  { possessifs : le moteur n'accepte pas "the my book" }
  LaSrcN := SN;
  For I := 0 To SN + 1 Do
  Begin
    LaSrc[I] := ST[I];
    LaSrcMk[I] := LaMk[I];
    LaSrcGn[I] := LaGn[I];
  End;
  LaOutN := 0;
  { decoupage en propositions : virgule ou conjonction apres un verbe fini }
  A := 1;
  HasFin := False;
  I := 1;
  While I <= LaSrcN Do
  Begin
    If (LaSrc[I].Cat = 'V') And (LaSrc[I].Tns In ['p', 'm', 'f', 'i', 'M']) Then
        HasFin := True;
    If LaSrc[I].Cat = ',' Then
    Begin
      SaveN := LaOutN;
      If I > A Then LaSegment(A, I - 1);
      If (LaOutN > SaveN) Or (I = A) Then LaEmitSimple(',', ',', ',');
      A := I + 1;
      HasFin := False;
    End
    Else If (LaSrc[I].Cat = 'C') And HasFin And (I > A) Then
    Begin
      LaSegment(A, I - 1);
      A := I;
      HasFin := False;
    End;
    Inc(I);
  End;
  If A <= LaSrcN Then LaSegment(A, LaSrcN);
  For J := 0 To LaOutN + 1 Do
    ST[J] := LaOut[J];
  SN := LaOutN;
  ARemoveConsumed;
End;

Function LaTranslateSentence(Const Sentence: String): String;
Var
  Res: String;
Begin
  LaVoc := '';
  LaVocFirst := False;
  LaTokenize(Sentence);
  If SN = 0 Then
  Begin
    LaTranslateSentence := '';
    Exit;
  End;
  LaClassify;
  If SN = 0 Then
  Begin
    LaTranslateSentence := '';
    Exit;
  End;
  Res := TranslateTokensEs(1, SN, SEnd = '?');
  If LaVoc <> '' Then
  Begin
    Res := FixEnglish(Res);
    If LaVocFirst Then Res := UpperFirst(LaVoc) + ', ' + Res
    Else Res := Res + ', ' + LaVoc;
  End;
  Res := UpperFirst(FixEnglish(Res));
  If SEnd <> #0 Then Res := Res + SEnd;
  LaTranslateSentence := Res;
End;

{ ------------------------------------------------------------------ }
{ Phrases toutes faites                                               }
{ ------------------------------------------------------------------ }

Const
  Phrases: Array[0..47] Of String = (
    'how are you|Comment allez-vous',
    'how are you doing|Comment allez-vous',
    'how is it going|Comment ça va',
    'how old are you|Quel âge avez-vous',
    'what is your name|Comment vous appelez-vous',
    'where are you from|D''où venez-vous',
    'where do you come from|D''où venez-vous',
    'what time is it|Quelle heure est-il',
    'nice to meet you|Enchanté',
    'pleased to meet you|Enchanté',
    'you are welcome|De rien',
    'never mind|Peu importe',
    'good luck|Bonne chance',
    'happy birthday|Joyeux anniversaire',
    'merry christmas|Joyeux Noël',
    'happy new year|Bonne année',
    'have a nice day|Bonne journée',
    'have a good day|Bonne journée',
    'take care|Prends soin de toi',
    'see you tomorrow|À demain',
    'see you later|À plus tard',
    'see you soon|À bientôt',
    'i miss you|Tu me manques',
    'i love you|Je t''aime',
    'i am sorry|Je suis désolé',
    'i do not know|Je ne sais pas',
    'i do not understand|Je ne comprends pas',
    'i am fine|Je vais bien',
    'i am fine thank you|Je vais bien, merci',
    'how much is it|Combien ça coûte',
    'how much does it cost|Combien ça coûte',
    'how much is this|Combien ça coûte',
    'where is the bathroom|Où sont les toilettes',
    'where is the toilet|Où sont les toilettes',
    'what is this|Qu''est-ce que c''est',
    'what is that|Qu''est-ce que c''est',
    'what is going on|Que se passe-t-il',
    'what happened|Que s''est-il passé',
    'thank you very much|Merci beaucoup',
    'thank you so much|Merci beaucoup',
    'thanks a lot|Merci beaucoup',
    'good night|Bonne nuit',
    'good evening|Bonsoir',
    'i do not speak french|Je ne parle pas français',
    'do you speak english|Parlez-vous anglais',
    'do you speak french|Parlez-vous français',
    'i am lost|Je suis perdu',
    'i do not have time|Je n''ai pas le temps'
  );

  PhrasesDe: Array[0..44] Of String = (
    'how are you|Wie geht es Ihnen',
    'how are you doing|Wie geht es Ihnen',
    'how is it going|Wie geht''s',
    'how old are you|Wie alt sind Sie',
    'what is your name|Wie heißen Sie',
    'where are you from|Woher kommen Sie',
    'where do you come from|Woher kommen Sie',
    'what time is it|Wie spät ist es',
    'nice to meet you|Freut mich',
    'pleased to meet you|Freut mich',
    'you are welcome|Bitte schön',
    'never mind|Macht nichts',
    'good luck|Viel Glück',
    'happy birthday|Alles Gute zum Geburtstag',
    'merry christmas|Frohe Weihnachten',
    'happy new year|Frohes neues Jahr',
    'have a nice day|Einen schönen Tag noch',
    'have a good day|Einen schönen Tag noch',
    'take care|Pass auf dich auf',
    'see you tomorrow|Bis morgen',
    'see you later|Bis später',
    'see you soon|Bis bald',
    'i miss you|Ich vermisse dich',
    'i love you|Ich liebe dich',
    'i am sorry|Es tut mir leid',
    'i do not know|Ich weiß es nicht',
    'i do not understand|Ich verstehe nicht',
    'i am fine|Mir geht es gut',
    'i am fine thank you|Mir geht es gut, danke',
    'how much is it|Wie viel kostet es',
    'how much does it cost|Wie viel kostet es',
    'how much is this|Wie viel kostet das',
    'where is the bathroom|Wo ist die Toilette',
    'where is the toilet|Wo ist die Toilette',
    'what is this|Was ist das',
    'what is that|Was ist das',
    'what is going on|Was ist los',
    'what happened|Was ist passiert',
    'thank you very much|Vielen Dank',
    'thank you so much|Vielen Dank',
    'thanks a lot|Vielen Dank',
    'good night|Gute Nacht',
    'good evening|Guten Abend',
    'i do not speak german|Ich spreche kein Deutsch',
    'do you speak english|Sprechen Sie Englisch'
  );

{ Remplace un groupe de mots du lexique par un seul jeton (a lot of, thank
  you...). }
Procedure MergeMultiword;
Var
  I, K, Len: Integer;
  Key: String;
  Found: Boolean;
Begin
  I := 1;
  While I < TN Do
  Begin
    Found := False;
    For Len := 3 Downto 2 Do
      If (Not Found) And (I + Len - 1 <= TN) Then
      Begin
        Key := '';
        For K := I To I + Len - 1 Do
        Begin
          If Tk[K].Punct Then Key := '#';
          If Key <> '#' Then Key := Join(Key, Tk[K].W);
        End;
        If (Key <> '#') And (FindLex(Key, 'nvarpcj') > 0) Then
        Begin
          Found := True;
          Tk[I].W := Key;
          Tk[I].Orig := Key;
          For K := I + 1 To TN - Len + 1 Do
            Tk[K] := Tk[K + Len - 1];
          Dec(TN, Len - 1);
        End;
      End;
    Inc(I);
  End;
End;

{ ------------------------------------------------------------------ }
{ Phrase complete                                                     }
{ ------------------------------------------------------------------ }

Function TranslateTokens(A, B: Integer; IsQ: Boolean): String;
Var
  K: Integer;
Begin
  If TargetDe Then
  Begin
    TranslateTokens := TranslateTokensDe(A, B, IsQ);
    Exit;
  End;
  If A > B Then
  Begin
    TranslateTokens := '';
    Exit;
  End;
  { virgule entre deux propositions (ou apres une interjection) }
  For K := A + 1 To B - 1 Do
    If (Tk[K].Tag = ',') And (Tk[K].W = ',') Then
      If ((K - A = 1) And (Tk[A].Tag = 'J')) Or
         (ClauseHasVerb(A, K - 1) And ClauseHasVerb(K + 1, B)) Then
      Begin
        TranslateTokens := TranslateTokens(A, K - 1, False) + ', ' +
            TranslateTokens(K + 1, B, IsQ);
        Exit;
      End;
  { conjonction entre deux propositions }
  For K := A + 1 To B - 1 Do
    If (Tk[K].Tag = 'C') And ClauseHasVerb(A, K - 1) And ClauseHasVerb(K + 1, B)
        Then
    Begin
      TranslateTokens := TranslateTokens(A, K - 1, False) + ' ' + ConjOf(K) +
          ' ' +
                         TranslateTokens(K + 1, B, IsQ);
      Exit;
    End;
  TranslateTokens := TranslateClause(A, B, IsQ);
End;

{ Elisions, contractions et ponctuation francaises. }
Function FixFrench(Const S: String): String;
Var
  Words: Array[1..120] Of String[40];
  Count, I: Integer;
  Cur, Res, Next, Joined: String;
  P: Integer;
Begin
  Count := 0;
  Cur := S;
  While Cur <> '' Do
  Begin
    P := Pos(' ', Cur);
    If P = 0 Then
    Begin
      If Count < 120 Then
      Begin
        Inc(Count);
        Words[Count] := Cur;
      End;
      Cur := '';
    End
    Else
    Begin
      If P > 1 Then
        If Count < 120 Then
        Begin
          Inc(Count);
          Words[Count] := Copy(Cur, 1, P - 1);
        End;
      Delete(Cur, 1, P);
    End;
  End;
  { contractions }
  I := 1;
  While I < Count Do
  Begin
    If (Words[I] = 'de') And ((Words[I + 1] = 'le') Or (Words[I + 1] = 'les'))
        Then
    Begin
      If Words[I + 1] = 'le' Then Words[I] := 'du' Else Words[I] := 'des';
      Words[I + 1] := '';
    End
    Else If (Words[I] = 'à') And ((Words[I + 1] = 'le') Or (Words[I + 1] =
        'les')) Then
    Begin
      If Words[I + 1] = 'le' Then Words[I] := 'au' Else Words[I] := 'aux';
      Words[I + 1] := '';
    End;
    Inc(I);
  End;
  { besoin de de l'argent ; besoin du pain }
  For I := 2 To Count - 1 Do
    If Words[I - 1] = 'besoin' Then
    Begin
      If (Words[I] = 'de') And (Words[I + 1] = 'de') Then Words[I + 1] := '';
      If (Words[I] = 'du') Or (Words[I] = 'des') Then Words[I] := 'de';
      If (Words[I] = 'de') And (Copy(Words[I + 1], 1, 2) = 'l''') Then
      Begin
        Words[I + 1] := 'd''' + Copy(Words[I + 1], 3, 40);
        Words[I] := '';
      End;
    End;
  { elisions : je, me, te, se, le, la, de, ne, que, ce, si }
  For I := 1 To Count - 1 Do
  Begin
    If Words[I] <> '' Then
    Begin
      P := I + 1;
      While (P <= Count) And (Words[P] = '') Do
        Inc(P);
      If P <= Count Then
      Begin
        Next := Words[P];
        If (Words[I] = 'je') Or (Words[I] = 'me') Or (Words[I] = 'te') Or
            (Words[I] = 'se') Or
           (Words[I] = 'le') Or (Words[I] = 'la') Or (Words[I] = 'de') Or
               (Words[I] = 'ne') Or
           (Words[I] = 'que') Or (Words[I] = 'ce') Or (Words[I] = 'si') Then
        Begin
          If StartsVowel(Next) Then
          Begin
            If (Words[I] = 'ce') And Not (StartsWith(Next, 'est') Or
                StartsWith(Next, 'ét') Or
               StartsWith(Next, 'ont')) Then
              Joined := ''
            Else If (Words[I] = 'si') And Not ((Next = 'il') Or (Next = 'ils'))
                Then
              Joined := ''
            Else
            Begin
              Joined := Copy(Words[I], 1, Length(Words[I]) - 1);
              If Words[I] = 'que' Then Joined := 'qu';
              Words[P] := Joined + '''' + Next;
              Words[I] := '';
            End;
          End;
        End;
      End;
    End;
  End;
  Res := '';
  For I := 1 To Count Do
    If Words[I] <> '' Then
    Begin
      If (Words[I][1] = ',') Or (Res = '') Then
        Res := Res + Words[I]
      Else
        Res := Res + ' ' + Words[I];
    End;
  FixFrench := Res;
End;

Function TranslateSentence(Const Sentence: String): String;
Var
  Key, Res, Pair, En: String;
  I, N: Integer;
  Question: Boolean;
  SD, SJ, SE, SQ, SP, SV, SDa, SIt, SLa: Boolean;
Begin
  If SourceMoe Then
  Begin
    TranslateSentence := MoeTranslateSentence(Sentence);
    Exit;
  End;
  If SourceDe Or SourceJa Or SourceEs Or SourceSq Or SourcePt Or SourceSv Or
      SourceDa Or SourceIt Or SourceLa Then
  Begin
    If SourceLa Then Res := LaTranslateSentence(Sentence)
    Else If SourceIt Then Res := ItTranslateSentence(Sentence)
    Else If SourceDa Then Res := DaTranslateSentence(Sentence)
    Else If SourceSv Then Res := SvTranslateSentence(Sentence)
    Else If SourcePt Then Res := PtTranslateSentence(Sentence)
    Else If SourceSq Then Res := SqTranslateSentence(Sentence)
    Else If SourceJa Then Res := JaTranslateSentence(Sentence)
    Else If SourceEs Then Res := EsTranslateSentence(Sentence)
    Else Res := DeTranslateSentence(Sentence);
    If Not DeToEn Then
    Begin
      { pivot : langue source -> anglais -> francais }
      SD := SourceDe;
      SJ := SourceJa;
      SE := SourceEs;
      SQ := SourceSq;
      SP := SourcePt;
      SV := SourceSv;
      SDa := SourceDa;
      SIt := SourceIt;
      SLa := SourceLa;
      SourceLa := False;
      SourceIt := False;
      SourceDa := False;
      SourceSv := False;
      SourcePt := False;
      SourceDe := False;
      SourceJa := False;
      SourceEs := False;
      SourceSq := False;
      Res := TranslateSentence(Res);
      SourceDe := SD;
      SourceJa := SJ;
      SourceEs := SE;
      SourceSq := SQ;
      SourcePt := SP;
      SourceSv := SV;
      SourceDa := SDa;
      SourceIt := SIt;
      SourceLa := SLa;
    End;
    TranslateSentence := Res;
    Exit;
  End;
  If TargetEn Then
  Begin
    TranslateSentence := FrTranslateSentence(Sentence);
    Exit;
  End;
  Tokenize(Sentence);
  If TN = 0 Then
  Begin
    TranslateSentence := '';
    Exit;
  End;
  MergeMultiword;
  Question := EndMark = '?';

  Key := '';
  For I := 1 To TN Do
    If Not Tk[I].Punct Then Key := Join(Key, Tk[I].W);
  Res := '';
  If TargetDe Then
  Begin
    For N := 0 To High(PhrasesDe) Do
    Begin
      Pair := PhrasesDe[N];
      En := CopyUntil(Pair, '|');
      If En = Key Then Res := Pair;
    End;
  End
  Else
    For N := 0 To High(Phrases) Do
    Begin
      Pair := Phrases[N];
      En := CopyUntil(Pair, '|');
      If En = Key Then Res := Pair;
    End;
  If (Res = '') And (TN >= 4) And (Tk[1].W = 'my') And (Tk[2].W = 'name') And
      (Tk[3].W = 'is') Then
  Begin
    If TargetDe Then Res := 'Ich heiße' Else Res := 'Je m''appelle';
    For I := 4 To TN Do
      If Tk[I].Punct Then Res := Res + ',' Else Res := Join(Res,
          UpperFirst(Tk[I].Orig));
  End;
  If Res = '' Then
  Begin
    TagTokens;
    Res := TranslateTokens(1, TN, Question);
  End;
  If TargetDe Then
  Begin
    Res := UpperFirst(FixGerman(Res));
    If EndMark <> #0 Then Res := Res + EndMark;
  End
  Else
  Begin
    Res := FixFrench(Res);
    Res := UpperFirst(Res);
    Case EndMark Of
      '?': Res := Res + ' ?';
      '!': Res := Res + ' !';
      '.': Res := Res + '.';
    End;
  End;
  TranslateSentence := Res;
End;

{ Decoupe un texte en phrases et traduit chacune. }
Function TranslateText(Const Text1: String): String;
Var
  I, Start: Integer;
  Res, Piece: String;
  Ends: Boolean;
Begin
  Res := '';
  Start := 1;
  I := 1;
  While I <= Length(Text1) Do
  Begin
    Ends := False;
    If Text1[I] In ['.', '!', '?'] Then
    Begin
      Ends := (I = Length(Text1)) Or (Text1[I + 1] = ' ');
      If (I > 1) And (I < Length(Text1)) And (Text1[I - 1] In ['0'..'9']) And
         (Text1[I + 1] In ['0'..'9']) Then
        Ends := False;
    End;
    If Ends Or (I = Length(Text1)) Then
    Begin
      Piece := Copy(Text1, Start, I - Start + 1);
      Res := Join(Res, TranslateSentence(Piece));
      Start := I + 1;
    End;
    Inc(I);
  End;
  TranslateText := Res;
End;

{ ------------------------------------------------------------------ }
{ Ligne de commande                                                   }
{ ------------------------------------------------------------------ }

Procedure ShowHelp;
Begin
  Writeln('ARGOS-TRANSLATE - Cette commande permet de traduire une phrase d''u'+
      'ne langue a une autre.');
  Writeln;
  Writeln('Syntaxe :');
  Writeln('  ARGOS-TRANSLATE --from-lang en --to-lang fr "Hello, how are you?"'
      );
  Writeln('  ARGOS-TRANSLATE [options] [texte]');
  Writeln('  ARGOS-TRANSLATE /?');
  Writeln;
  Writeln('Options :');
  Writeln('  -f, --from-lang  Code ISO 639-1 de la langue source (en, fr, de, '+
      'ja en romaji, es, sq, pt, sv, da, it, la).');
  Writeln('  -t, --to-lang    Code ISO 639-1 de la langue cible (fr, de, en).');
  Writeln('  -l, --list       Affiche les paires de langues disponibles.');
  Writeln('  -V, --version    Affiche la version.');
  Writeln('  -h, --help, /?   Affiche cette aide.');
  Writeln;
  Writeln('Sans texte, les lignes sont lues sur l''entree standard. Le resulta'+
      't est ecrit en');
  Writeln('UTF-8 (sous Windows, taper d''abord CHCP 65001 pour afficher les ac'+
      'cents).');
  Writeln;
  Writeln('Traducteur a regles anglais -> francais/allemand et francais -> ang'+
      'lais : il traduit les phrases simples ;');
  Writeln('un mot inconnu est conserve tel quel.');
End;

Var
  FromLang, ToLang, TextArg, Line, Argument: String;
  ArgumentIndex: Integer;
  ListOnly: Boolean;

BEGIN
  LoadLexicon;
  LoadIrregular;
  LoadEnglish;
  LoadGermanVerbs;
  LoadJapanese;
  LoadSpanish;
  LoadAlbanian;
  LoadPortuguese;
  LoadSwedish;
  LoadDanish;
  LoadItalian;
  LoadLatin;
  LoadMoe;
  FromLang := '';
  ToLang := '';
  TextArg := '';
  ListOnly := False;
  ArgumentIndex := 1;
  While ArgumentIndex <= ParamCount Do
  Begin
    Argument := ParamStr(ArgumentIndex);
    If (Argument = '/?') Or (Argument = '-?') Or (Argument = '-h') Or (Argument
        = '--help') Then
    Begin
      ShowHelp;
      Halt(0);
    End
    Else If (Argument = '-V') Or (Argument = '--version') Then
    Begin
      Writeln('ARGOS-TRANSLATE (clone en Pascal) ', Version);
      Halt(0);
    End
    Else If (Argument = '-l') Or (Argument = '--list') Then
      ListOnly := True
    Else If (Argument = '-f') Or (Argument = '--from-lang') Then
    Begin
      If ArgumentIndex = ParamCount Then
      Begin
        Writeln('Erreur : code de langue manquant apres ', Argument);
        Halt(2);
      End;
      Inc(ArgumentIndex);
      FromLang := LowerAscii(ParamStr(ArgumentIndex));
    End
    Else If (Argument = '-t') Or (Argument = '--to-lang') Then
    Begin
      If ArgumentIndex = ParamCount Then
      Begin
        Writeln('Erreur : code de langue manquant apres ', Argument);
        Halt(2);
      End;
      Inc(ArgumentIndex);
      ToLang := LowerAscii(ParamStr(ArgumentIndex));
    End
    Else If (Copy(Argument, 1, 1) = '-') And (Length(Argument) > 1) Then
    Begin
      Writeln('Erreur : option non prise en charge : ', Argument);
      Halt(2);
    End
    Else
      TextArg := Join(TextArg, Argument);
    Inc(ArgumentIndex);
  End;

  If ListOnly Then
  Begin
    Writeln('en -> fr');
    Writeln('en -> de');
    Writeln('fr -> en');
    Writeln('de -> fr');
    Writeln('de -> en');
    Writeln('ja -> fr (romaji)');
    Writeln('ja -> en (romaji)');
    Writeln('es -> fr');
    Writeln('es -> en');
    Writeln('sq -> fr (albanais)');
    Writeln('sq -> en (albanais)');
    Writeln('pt -> fr');
    Writeln('pt -> en');
    Writeln('sv -> fr');
    Writeln('sv -> en');
    Writeln('da -> fr');
    Writeln('da -> en');
    Writeln('it -> fr');
    Writeln('it -> en');
    Writeln('la -> fr');
    Writeln('la -> en');
    Writeln('moe -> fr (innu-aimun : phrases et mots connus seulement)');
    Halt(0);
  End;
  If (FromLang = '') Or (ToLang = '') Then
  Begin
    Writeln('Erreur : indiquer --from-lang et --to-lang.');
    Halt(2);
  End;
  If (FromLang = 'inn') Or (FromLang = 'innu') Then FromLang := 'moe';
  If FromLang = 'moe' Then
  Begin
    If ToLang <> 'fr' Then
    Begin
      Writeln('Erreur : l''innu-aimun ne se traduit que vers le francais (moe '+
          '-> fr).');
      Halt(1);
    End;
    SourceMoe := True;
    TargetDe := False;
    TargetEn := False;
  End;
  If Not (SourceMoe Or ((FromLang = 'en') And ((ToLang = 'fr') Or (ToLang =
      'de'))) Or
          ((FromLang = 'fr') And (ToLang = 'en')) Or
          ((FromLang = 'de') And ((ToLang = 'fr') Or (ToLang = 'en'))) Or
          ((FromLang = 'ja') And ((ToLang = 'fr') Or (ToLang = 'en'))) Or
          ((FromLang = 'es') And ((ToLang = 'fr') Or (ToLang = 'en'))) Or
          ((FromLang = 'sq') And ((ToLang = 'fr') Or (ToLang = 'en'))) Or
          ((FromLang = 'pt') And ((ToLang = 'fr') Or (ToLang = 'en'))) Or
          ((FromLang = 'sv') And ((ToLang = 'fr') Or (ToLang = 'en'))) Or
          ((FromLang = 'da') And ((ToLang = 'fr') Or (ToLang = 'en'))) Or
          ((FromLang = 'it') And ((ToLang = 'fr') Or (ToLang = 'en'))) Or
          ((FromLang = 'la') And ((ToLang = 'fr') Or (ToLang = 'en')))) Then
  Begin
    Writeln('Erreur : aucun modele de traduction de ', FromLang, ' vers ',
        ToLang,
            ' n''est installe (paires disponibles : en -> fr, en -> de, fr -> '+
                'en, de -> fr, de -> en, ja -> fr, ja -> en, es -> fr, es -> e'+
                'n, sq -> fr, sq -> en, pt -> fr, pt -> en, sv -> fr, sv -> en'+
                ', da -> fr, da -> en, it -> fr, it -> en, la -> fr, la -> en)'+
                '.');
    Halt(1);
  End;
  SourceDe := FromLang = 'de';
  SourceJa := FromLang = 'ja';
  SourceEs := FromLang = 'es';
  SourceSq := FromLang = 'sq';
  SourcePt := FromLang = 'pt';
  SourceSv := FromLang = 'sv';
  SourceDa := FromLang = 'da';
  SourceIt := FromLang = 'it';
  SourceLa := FromLang = 'la';
  DeToEn := (SourceDe Or SourceJa Or SourceEs Or SourceSq Or SourcePt Or
      SourceSv Or SourceDa Or SourceIt Or SourceLa) And (ToLang = 'en');
  DePivot := (SourceDe Or SourceJa Or SourceEs Or SourceSq Or SourcePt Or
      SourceSv Or SourceDa Or SourceIt Or SourceLa) And (Not DeToEn);
  If SourceDe Then InitDeSpecials;
  TargetDe := ToLang = 'de';
  TargetEn := ToLang = 'en';

  If TextArg <> '' Then
    Writeln(TranslateText(TextArg))
  Else
    While Not Eof(Input) Do
    Begin
      ReadLn(Input, Line);
      If Copy(Line, 1, 3) = #$EF#$BB#$BF Then Delete(Line, 1, 3);
      If Line <> '' Then Writeln(TranslateText(Line)) Else Writeln;
    End;
END.
