{ @author: Sylvain Maltais (support@gladir.com)
  @created: 2026
  @website(https://www.gladir.com/OCR-0)
  @abstract(Target: Free Pascal 3.2)
  @description: Clone de la commande argos-translate (traduction de phrases) :
                argos-translate --from-lang en --to-lang fr "Hello, how are you?"

  Argos Translate utilise des reseaux de neurones (OpenNMT / CTranslate2) dont les
  modeles pesent plusieurs dizaines de Mo : ils ne sont pas realisables en Pascal.
  Ce clone est un traducteur A REGLES, sans reseau : anglais -> francais, anglais ->
  allemand et francais -> anglais.
    - lexique d'environ 600 mots (noms avec genre, adjectifs avec accord et place,
      verbes avec groupe et auxiliaire, adverbes, prepositions) ;
    - verbes irreguliers francais (present, futur, conditionnel, imparfait, participe) ;
    - formes irregulieres anglaises, contractions (don't, I'm, ...), locutions ;
    - analyse d'un groupe nominal (determinant, adjectifs, noms) avec accord ;
    - phrases declaratives, negatives, interrogatives (est-ce que, inversion, mots
      interrogatifs) et imperatives, presents, passes (passe compose), futurs ;
    - post-traitement : elisions (j', l', n'), contractions (du, au, des, aux),
      ponctuation francaise et majuscule initiale.
    - allemand : cas (nominatif, accusatif, datif), declinaison des articles et des
      adjectifs, genre et pluriel des noms, verbes forts/faibles/separables, parfait,
      ordre des mots (verbe en 2e position, verbe final des subordonnees), negation
      (nicht/kein), contractions (im, zum, zur...) et "Sie" de politesse.
    - francais -> anglais : lecture des formes conjuguees (present, futur, conditionnel,
      imparfait, passe compose), pronoms, negation (ne ... pas), inversions et "est-ce
      que", groupes nominaux, puis generation anglaise (auxiliaires do/will/have, -s,
      temps du passe, ordre des adjectifs).
  Il traduit correctement des phrases simples ; une phrase complexe peut donner une
  traduction approximative, et un mot inconnu est conserve tel quel.
}

{$mode objfpc}
{$H-}

Program ARGOSTRANSLATE;

Const
  Version = '1.0';
  MaxLex = 700;
  MaxTokens = 80;
  MaxIrregular = 64;
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
    DeFlags: String[6]; { w faible, s etre, d datif, S subordonnant, i invariable, P pluriel }
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
  LexData: Array[0..90] Of String = (
    'time=temps:nm/temps~Zeit|f/Zeiten;day=jour:nm~Tag|m/Tage;night=nuit:nf~Nacht|f/Nächte;morning=matin:nm~Morgen|m/-;evening=soir:nm~Abend|m/Abende;afternoon=après-midi:nm/après-midi~Nachmittag|m/Nachmittage;',
    'week=semaine:nf~Woche|f/Wochen;month=mois:nm/mois~Monat|m/Monate;year=année:nf~Jahr|n/Jahre;hour=heure:nf~Stunde|f/Stunden;minute=minute:nf~Minute|f/Minuten;second=seconde:nf~Sekunde|f/Sekunden;man=homme:nm~Mann|m/Männer;',
    'woman=femme:nf/femmes~Frau|f/Frauen;child=enfant:nm~Kind|n/Kinder;boy=garçon:nm~Junge|mw/Jungen;girl=fille:nf~Mädchen|n/-;friend=ami:nm~Freund|m/Freunde;family=famille:nf~Familie|f/Familien;mother=mère:nf~Mutter|f/Mütter;',
    'father=père:nm~Vater|m/Väter;brother=frère:nm~Bruder|m/Brüder;sister=sœur:nf~Schwester|f/Schwestern;son=fils:nm/fils~Sohn|m/Söhne;daughter=fille:nf~Tochter|f/Töchter;husband=mari:nm~Ehemann|m/Ehemänner;',
    'wife=femme:nf~Frau|f/Frauen;parent=parent:nm~Elternteil|m/Eltern;people=gens:nm/gens~Leute|P;person=personne:nf~Person|f/Personen;house=maison:nf~Haus|n/Häuser;home=maison:nf~Zuhause|n/-;room=chambre:nf~Zimmer|n/-;',
    'kitchen=cuisine:nf~Küche|f/Küchen;bathroom=salle de bain:nf~Badezimmer|n/-;bedroom=chambre:nf~Schlafzimmer|n/-;door=porte:nf~Tür|f/Türen;window=fenêtre:nf~Fenster|n/-;table=table:nf~Tisch|m/Tische;',
    'chair=chaise:nf~Stuhl|m/Stühle;bed=lit:nm~Bett|n/Betten;toilet=toilettes:nf/toilettes~Toilette|f/Toiletten;floor=sol:nm~Boden|m/Böden;wall=mur:nm~Wand|f/Wände;roof=toit:nm~Dach|n/Dächer;car=voiture:nf~Auto|n/Autos;',
    'bus=bus:nm/bus~Bus|m/Busse;train=train:nm~Zug|m/Züge;plane=avion:nm~Flugzeug|n/Flugzeuge;bike=vélo:nm~Fahrrad|n/Fahrräder;boat=bateau:nm/bateaux~Boot|n/Boote;street=rue:nf~Straße|f/Straßen;road=route:nf~Straße|f/Straßen;',
    'city=ville:nf~Stadt|f/Städte;town=ville:nf~Stadt|f/Städte;village=village:nm~Dorf|n/Dörfer;country=pays:nm/pays~Land|n/Länder;world=monde:nm~Welt|f/Welten;school=école:nf~Schule|f/Schulen;',
    'university=université:nf~Universität|f/Universitäten;class=classe:nf~Klasse|f/Klassen;lesson=leçon:nf~Lektion|f/Lektionen;work=travail:nm/travaux~Arbeit|f/Arbeiten;job=emploi:nm~Job|m/Jobs;',
    'office=bureau:nm/bureaux~Büro|n/Büros;company=entreprise:nf~Firma|f/Firmen;shop=magasin:nm~Geschäft|n/Geschäfte;store=magasin:nm~Laden|m/Läden;market=marché:nm~Markt|m/Märkte;',
    'restaurant=restaurant:nm~Restaurant|n/Restaurants;hotel=hôtel:nm~Hotel|n/Hotels;hospital=hôpital:nm/hôpitaux~Krankenhaus|n/Krankenhäuser;library=bibliothèque:nf~Bibliothek|f/Bibliotheken;bank=banque:nf~Bank|f/Banken;',
    'airport=aéroport:nm~Flughafen|m/Flughäfen;station=gare:nf~Bahnhof|m/Bahnhöfe;museum=musée:nm~Museum|n/Museen;church=église:nf~Kirche|f/Kirchen;theater=théâtre:nm~Theater|n/-;cinema=cinéma:nm~Kino|n/Kinos;',
    'beach=plage:nf~Strand|m/Strände;sea=mer:nf~Meer|n/Meere;ocean=océan:nm~Ozean|m/Ozeane;river=rivière:nf~Fluss|m/Flüsse;lake=lac:nm~See|m/Seen;mountain=montagne:nf~Berg|m/Berge;forest=forêt:nf~Wald|m/Wälder;',
    'garden=jardin:nm~Garten|m/Gärten;park=parc:nm~Park|m/Parks;tree=arbre:nm~Baum|m/Bäume;flower=fleur:nf~Blume|f/Blumen;grass=herbe:nf~Gras|n/Gräser;island=île:nf~Insel|f/Inseln;field=champ:nm~Feld|n/Felder;',
    'bridge=pont:nm~Brücke|f/Brücken;sky=ciel:nm/cieux~Himmel|m/-;star=étoile:nf~Stern|m/Sterne;moon=lune:nf~Mond|m/Monde;sun=soleil:nm~Sonne|f/Sonnen;water=eau:nf/eaux~Wasser|n/-;food=nourriture:nf~Essen|n/-;',
    'bread=pain:nm~Brot|n/Brote;cheese=fromage:nm~Käse|m/-;meat=viande:nf~Fleisch|n/-;fish=poisson:nm~Fisch|m/Fische;apple=pomme:nf~Apfel|m/Äpfel;orange=orange:nf~Orange|f/Orangen;banana=banane:nf~Banane|f/Bananen;',
    'egg=œuf:nm~Ei|n/Eier;rice=riz:nm/riz~Reis|m/-;salad=salade:nf~Salat|m/Salate;soup=soupe:nf~Suppe|f/Suppen;cake=gâteau:nm/gâteaux~Kuchen|m/-;chocolate=chocolat:nm~Schokolade|f/Schokoladen;sugar=sucre:nm~Zucker|m/-;',
    'salt=sel:nm~Salz|n/Salze;vegetable=légume:nm~Gemüse|n/-;fruit=fruit:nm~Obst|n/-;coffee=café:nm~Kaffee|m/Kaffees;tea=thé:nm~Tee|m/Tees;milk=lait:nm~Milch|f/-;wine=vin:nm~Wein|m/Weine;beer=bière:nf~Bier|n/Biere;',
    'juice=jus:nm/jus~Saft|m/Säfte;drink=boisson:nf~Getränk|n/Getränke;breakfast=petit-déjeuner:nm~Frühstück|n/Frühstücke;lunch=déjeuner:nm~Mittagessen|n/-;dinner=dîner:nm~Abendessen|n/-;',
    'meal=repas:nm/repas~Mahlzeit|f/Mahlzeiten;menu=menu:nm~Speisekarte|f/Speisekarten;bill=addition:nf~Rechnung|f/Rechnungen;plate=assiette:nf~Teller|m/-;glass=verre:nm~Glas|n/Gläser;cup=tasse:nf~Tasse|f/Tassen;',
    'bottle=bouteille:nf~Flasche|f/Flaschen;knife=couteau:nm/couteaux~Messer|n/-;fork=fourchette:nf~Gabel|f/Gabeln;spoon=cuillère:nf~Löffel|m/-;money=argent:nm~Geld|n/-;price=prix:nm/prix~Preis|m/Preise;card=carte:nf~Karte|f/Karten;',
    'book=livre:nm~Buch|n/Bücher;letter=lettre:nf~Brief|m/Briefe;newspaper=journal:nm/journaux~Zeitung|f/Zeitungen;magazine=magazine:nm~Zeitschrift|f/Zeitschriften;page=page:nf~Seite|f/Seiten;phone=téléphone:nm~Telefon|n/Telefone;',
    'telephone=téléphone:nm~Telefon|n/Telefone;computer=ordinateur:nm~Computer|m/-;television=télévision:nf~Fernseher|m/-;radio=radio:nf~Radio|n/Radios;camera=appareil photo:nm~Kamera|f/Kameras;internet=internet:nm~Internet|n/-;',
    'email=courriel:nm~E-Mail|f/E-Mails;message=message:nm~Nachricht|f/Nachrichten;website=site web:nm/sites web~Website|f/Websites;name=nom:nm~Name|mw/Namen;language=langue:nf~Sprache|f/Sprachen;word=mot:nm~Wort|n/Wörter;',
    'sentence=phrase:nf~Satz|m/Sätze;question=question:nf~Frage|f/Fragen;answer=réponse:nf~Antwort|f/Antworten;problem=problème:nm~Problem|n/Probleme;idea=idée:nf~Idee|f/Ideen;story=histoire:nf~Geschichte|f/Geschichten;',
    'history=histoire:nf~Geschichte|f/-;news=nouvelles:nf/nouvelles~Nachrichten|P;information=information:nf~Information|f/Informationen;thing=chose:nf~Sache|f/Sachen;place=endroit:nm~Ort|m/Orte;way=chemin:nm~Weg|m/Wege;',
    'reason=raison:nf~Grund|m/Gründe;example=exemple:nm~Beispiel|n/Beispiele;number=numéro:nm~Nummer|f/Nummern;part=partie:nf~Teil|m/Teile;end=fin:nf~Ende|n/Enden;beginning=début:nm~Anfang|m/Anfänge;side=côté:nm~Seite|f/Seiten;',
    'top=haut:nm~Spitze|f/Spitzen;bottom=bas:nm~Boden|m/Böden;music=musique:nf~Musik|f/-;song=chanson:nf~Lied|n/Lieder;movie=film:nm~Film|m/Filme;film=film:nm~Film|m/Filme;game=jeu:nm/jeux~Spiel|n/Spiele;',
    'picture=image:nf~Bild|n/Bilder;photo=photo:nf~Foto|n/Fotos;painting=tableau:nm/tableaux~Gemälde|n/-;art=art:nm~Kunst|f/Künste;dance=danse:nf~Tanz|m/Tänze;party=fête:nf~Party|f/Partys;',
    'gift=cadeau:nm/cadeaux~Geschenk|n/Geschenke;birthday=anniversaire:nm~Geburtstag|m/Geburtstage;holiday=vacances:nf/vacances~Urlaub|m/-;vacation=vacances:nf/vacances~Urlaub|m/-;trip=voyage:nm~Reise|f/Reisen;',
    'journey=voyage:nm~Reise|f/Reisen;ticket=billet:nm~Ticket|n/Tickets;passport=passeport:nm~Reisepass|m/Reisepässe;luggage=bagages:nm/bagages~Gepäck|n/-;bag=sac:nm~Tasche|f/Taschen;suitcase=valise:nf~Koffer|m/-;',
    'key=clé:nf~Schlüssel|m/-;dog=chien:nm~Hund|m/Hunde;cat=chat:nm~Katze|f/Katzen;bird=oiseau:nm/oiseaux~Vogel|m/Vögel;horse=cheval:nm/chevaux~Pferd|n/Pferde;animal=animal:nm/animaux~Tier|n/Tiere;cow=vache:nf~Kuh|f/Kühe;',
    'pig=cochon:nm~Schwein|n/Schweine;mouse=souris:nf/souris~Maus|f/Mäuse;rabbit=lapin:nm~Kaninchen|n/-;weather=temps:nm~Wetter|n/-;rain=pluie:nf~Regen|m/-;snow=neige:nf~Schnee|m/-;wind=vent:nm~Wind|m/Winde;',
    'cloud=nuage:nm~Wolke|f/Wolken;storm=orage:nm~Sturm|m/Stürme;life=vie:nf~Leben|n/-;death=mort:nf~Tod|m/-;love=amour:nm~Liebe|f/-;heart=cœur:nm~Herz|n/Herzen;peace=paix:nf~Frieden|m/-;war=guerre:nf~Krieg|m/Kriege;',
    'freedom=liberté:nf~Freiheit|f/-;head=tête:nf~Kopf|m/Köpfe;hand=main:nf~Hand|f/Hände;foot=pied:nm~Fuß|m/Füße;arm=bras:nm/bras~Arm|m/Arme;leg=jambe:nf~Bein|n/Beine;eye=œil:nm/yeux~Auge|n/Augen;ear=oreille:nf~Ohr|n/Ohren;',
    'nose=nez:nm/nez~Nase|f/Nasen;mouth=bouche:nf~Mund|m/Münder;face=visage:nm~Gesicht|n/Gesichter;hair=cheveux:nm/cheveux~Haare|P;body=corps:nm/corps~Körper|m/-;health=santé:nf~Gesundheit|f/-;doctor=médecin:nm~Arzt|m/Ärzte;',
    'nurse=infirmière:nf~Krankenschwester|f/Krankenschwestern;teacher=professeur:nm~Lehrer|m/-;student=étudiant:nm~Student|mw/Studenten;driver=chauffeur:nm~Fahrer|m/-;worker=ouvrier:nm~Arbeiter|m/-;king=roi:nm~König|m/Könige;',
    'queen=reine:nf~Königin|f/Königinnen;president=président:nm~Präsident|mw/Präsidenten;police=police:nf~Polizei|f/-;soldier=soldat:nm~Soldat|mw/Soldaten;shirt=chemise:nf~Hemd|n/Hemden;dress=robe:nf~Kleid|n/Kleider;',
    'shoe=chaussure:nf~Schuh|m/Schuhe;hat=chapeau:nm/chapeaux~Hut|m/Hüte;coat=manteau:nm/manteaux~Mantel|m/Mäntel;pants=pantalon:nm~Hose|f/Hosen;clothes=vêtements:nm/vêtements~Kleidung|f/-;color=couleur:nf~Farbe|f/Farben;',
    'size=taille:nf~Größe|f/Größen;shape=forme:nf~Form|f/Formen;line=ligne:nf~Linie|f/Linien;point=point:nm~Punkt|m/Punkte;level=niveau:nm/niveaux~Niveau|n/Niveaus;group=groupe:nm~Gruppe|f/Gruppen;',
    'team=équipe:nf~Mannschaft|f/Mannschaften;sport=sport:nm~Sport|m/-;ball=ballon:nm~Ball|m/Bälle;machine=machine:nf~Maschine|f/Maschinen;tool=outil:nm~Werkzeug|n/Werkzeuge;light=lumière:nf~Licht|n/Lichter;',
    'fire=feu:nm/feux~Feuer|n/-;air=air:nm~Luft|f/-;earth=terre:nf~Erde|f/-;stone=pierre:nf~Stein|m/Steine;gold=or:nm~Gold|n/-;paper=papier:nm~Papier|n/Papiere;pen=stylo:nm~Stift|m/Stifte;pencil=crayon:nm~Bleistift|m/Bleistifte;',
    'monday=lundi:nm~Montag|m/Montage;tuesday=mardi:nm~Dienstag|m/Dienstage;wednesday=mercredi:nm~Mittwoch|m/Mittwoche;thursday=jeudi:nm~Donnerstag|m/Donnerstage;friday=vendredi:nm~Freitag|m/Freitage;',
    'saturday=samedi:nm~Samstag|m/Samstage;sunday=dimanche:nm~Sonntag|m/Sonntage;january=janvier:nm~Januar|m/-;february=février:nm~Februar|m/-;march=mars:nm~März|m/-;april=avril:nm~April|m/-;may=mai:nm~Mai|m/-;june=juin:nm~Juni|m/-;',
    'july=juillet:nm~Juli|m/-;august=août:nm~August|m/-;september=septembre:nm~September|m/-;october=octobre:nm~Oktober|m/-;november=novembre:nm~November|m/-;december=décembre:nm~Dezember|m/-;spring=printemps:nm~Frühling|m/-;',
    'summer=été:nm~Sommer|m/-;autumn=automne:nm~Herbst|m/-;winter=hiver:nm~Winter|m/-;good=bon:aB/bonne~gut;bad=mauvais:aB~schlecht;big=grand:aB~groß;large=grand:aB~groß;small=petit:aB~klein;little=petit:aB~klein;',
    'long=long:aB/longue~lang;short=court:aA/courte~kurz;tall=grand:aB~groß;high=haut:aB~hoch/hoh;low=bas:aA/basse~niedrig;new=nouveau:aB/nouvelle/nouveaux~neu;old=vieux:aB/vieille/vieux~alt;young=jeune:aB~jung;',
    'beautiful=beau:aB/belle/beaux~schön;pretty=joli:aB~hübsch;nice=agréable:aA~schön;great=grand:aB~toll;wonderful=merveilleux:aA/merveilleuse~wunderbar;terrible=terrible:aA~schrecklich;delicious=délicieux:aA/délicieuse~lecker;',
    'happy=heureux:aA/heureuse~glücklich;sad=triste:aA~traurig;tired=fatigué:aA~müde;hungry=affamé:aA!faim~hungrig;thirsty=assoiffé:aA!soif~durstig;afraid=effrayé:aA!peur~ängstlich;sleepy=endormi:aA!sommeil~müde;',
    'cold=froid:aA!froid~kalt;hot=chaud:aA!chaud~heiß;warm=chaud:aA~warm;cool=frais:aA/fraîche~kühl;easy=facile:aA~einfach;difficult=difficile:aA~schwierig;hard=difficile:aA~schwer;important=important:aA~wichtig;',
    'interesting=intéressant:aA~interessant;boring=ennuyeux:aA/ennuyeuse~langweilig;funny=drôle:aA~lustig;strange=étrange:aA~seltsam;famous=célèbre:aA~berühmt;popular=populaire:aA~beliebt;expensive=cher:aA/chère~teuer/teur;',
    'cheap=bon marché:aA/bon marché/bon marché~billig;free=gratuit:aA/gratuite~kostenlos;open=ouvert:aA/ouverte~offen;closed=fermé:aA~geschlossen;ready=prêt:aA/prête~fertig;busy=occupé:aA~beschäftigt;sick=malade:aA~krank;',
    'ill=malade:aA~krank;rich=riche:aA~reich;poor=pauvre:aA~arm;clean=propre:aA~sauber;dirty=sale:aA~schmutzig;fast=rapide:aA~schnell;quick=rapide:aA~schnell;slow=lent:aA/lente~langsam;strong=fort:aA/forte~stark;weak=faible:aA~schwach;',
    'heavy=lourd:aA/lourde~schwer;light=léger:aA/légère~leicht;full=plein:aA/pleine~voll;empty=vide:aA~leer;dark=sombre:aA~dunkel/dunkl;bright=brillant:aA~hell;red=rouge:aA~rot;blue=bleu:aA/bleue~blau;green=vert:aA/verte~grün;',
    'yellow=jaune:aA~gelb;black=noir:aA/noire~schwarz;white=blanc:aA/blanche/blancs~weiß;brown=marron:aA/marron/marron~braun;gray=gris:aA/grise~grau;grey=gris:aA/grise~grau;pink=rose:aA~rosa/*;purple=violet:aA/violette~lila/*;',
    'first=premier:aB/première~erst;last=dernier:aB/dernière~letzt;next=prochain:aB/prochaine~nächst;other=autre:aB~ander;same=même:aB~gleich;different=différent:aA/différente~anders;sure=sûr:aA/sûre~sicher;',
    'true=vrai:aA/vraie~wahr;false=faux:aA/fausse~falsch;possible=possible:aA~möglich;special=spécial:aA/spéciale/spéciaux~besonders;proud=fier:aA/fière~stolz;kind=gentil:aA/gentille~freundlich;nervous=nerveux:aA/nerveuse~nervös;',
    'lost=perdu:aA/perdue~verloren;late=en retard:aA/en retard/en retard~spät;lonely=seul:aA/seule~einsam;alone=seul:aA/seule~allein;married=marié:aA/mariée~verheiratet;single=célibataire:aA~ledig;very=très:r~sehr;too=trop:r~zu;',
    'also=aussi:r~auch;always=toujours:r~immer;never=jamais:r~nie;often=souvent:r~oft;sometimes=parfois:r~manchmal;usually=d''habitude:r~normalerweise;now=maintenant:r~jetzt;today=aujourd''hui:r~heute;tomorrow=demain:r~morgen;',
    'yesterday=hier:r~gestern;tonight=ce soir:r~heute Abend;here=ici:r~hier;there=là:r~dort;well=bien:r~gut;quickly=rapidement:r~schnell;slowly=lentement:r~langsam;again=encore:r~wieder;soon=bientôt:r~bald;already=déjà:r~schon;',
    'still=encore:r~noch;just=juste:r~gerade;only=seulement:r~nur;really=vraiment:r~wirklich;much=beaucoup:r~viel;maybe=peut-être:r~vielleicht;perhaps=peut-être:r~vielleicht;together=ensemble:r~zusammen;almost=presque:r~fast;',
    'then=puis:r~dann;early=tôt:r~früh;away=loin:r~weg;everywhere=partout:r~überall;somewhere=quelque part:r~irgendwo;outside=dehors:r~draußen;inside=dedans:r~drinnen;however=cependant:r~jedoch;very much=beaucoup:r~sehr;',
    'a lot=beaucoup:r~viel;a lot of=beaucoup de:r~viele;right now=en ce moment:r~gerade jetzt;of course=bien sûr:r~natürlich;at home=à la maison:r~zu Hause;every day=tous les jours:r~jeden Tag;',
    'all day=toute la journée:r~den ganzen Tag;next week=la semaine prochaine:r~nächste Woche;last night=hier soir:r~gestern Abend;this morning=ce matin:r~heute Morgen;this evening=ce soir:r~heute Abend;',
    'this afternoon=cet après-midi:r~heute Nachmittag;in=dans:p~in|w;on=sur:p~auf|w;at=à:p~an|w;from=de:p~von|d;with=avec:p~mit|d;without=sans:p~ohne|a;for=pour:p~für|a;of=de:p~von|d;about=à propos de:p~über|a;by=par:p~von|d;',
    'under=sous:p~unter|w;over=sur:p~über|w;between=entre:p~zwischen|w;before=avant:p~vor|w;after=après:p~nach|d;during=pendant:p~während|d;since=depuis:p~seit|d;until=jusqu''à:p~bis|a;near=près de:p~in der Nähe von|d;',
    'behind=derrière:p~hinter|w;around=autour de:p~um|a;through=à travers:p~durch|a;into=dans:p~in|a;against=contre:p~gegen|a;across=à travers:p~über|a;toward=vers:p~zu|d;towards=vers:p~zu|d;above=au-dessus de:p~über|w;',
    'below=en dessous de:p~unter|w;next to=à côté de:p~neben|w;in front of=devant:p~vor|w;because of=à cause de:p~wegen|d;instead of=au lieu de:p~statt|d;out of=hors de:p~aus|d;up=en haut:r~hoch;down=en bas:r~runter;off=loin:r~weg;',
    'and=et:c~und;or=ou:c~oder;but=mais:c~aber;because=parce que:c~weil|S;if=si:c~wenn|S;when=quand:c~wenn|S;while=pendant que:c~während|S;so=donc:c~also;that=que:c~dass|S;than=que:c~als;as=comme:c~wie;although=bien que:c~obwohl|S;',
    'hello=bonjour:j~Hallo;hi=salut:j~Hallo;goodbye=au revoir:j~Auf Wiedersehen;bye=au revoir:j~Tschüss;thanks=merci:j~Danke;thank you=merci:j~Danke;please=s''il vous plaît:j~Bitte;sorry=désolé:j~Entschuldigung;',
    'excuse me=excusez-moi:j~Entschuldigung;welcome=bienvenue:j~Willkommen;yes=oui:j~Ja;no=non:j~Nein;okay=d''accord:j~Okay;ok=d''accord:j~Okay;good morning=bonjour:j~Guten Morgen;good evening=bonsoir:j~Guten Abend;',
    'good night=bonne nuit:j~Gute Nacht;good afternoon=bonjour:j~Guten Tag;congratulations=félicitations:j~Herzlichen Glückwunsch;cheers=santé:j~Prost;wow=ouah:j~Wow;oh=oh:j~Oh;love=aimer:ve~lieben|;like=aimer:ve~mögen|;',
    'hate=détester:ve~hassen|;want=vouloir:vxa@~wollen|;need=avoir besoin de:vxa@~brauchen|;go=aller:vxe~gehen|s;come=venir:vxe~kommen|s;see=voir:vxa~sehen|;look=regarder:ve~schauen|;watch=regarder:ve~schauen|;',
    'hear=entendre:vr~hören|;listen=écouter:ve~hören|d+zu;speak=parler:ve~sprechen|;talk=parler:ve~sprechen|;say=dire:vxa~sagen|;tell=dire:vxa~sagen|;ask=demander:ve~fragen|;answer=répondre:vr~antworten|d;',
    'understand=comprendre:vxa~verstehen|;know=savoir:vxa~wissen|;think=penser:ve~denken|;believe=croire:vxa~glauben|;feel=sentir:vxa~fühlen|;find=trouver:ve~finden|;give=donner:ve~geben|;take=prendre:vxa~nehmen|;',
    'make=faire:vxa~machen|;put=mettre:vxa~legen|;get=obtenir:vxa~bekommen|;buy=acheter:vxa~kaufen|;sell=vendre:vr~verkaufen|;pay=payer:vxa~bezahlen|;eat=manger:ve~essen|;drink=boire:vxa~trinken|;cook=cuisiner:ve~kochen|;',
    'sleep=dormir:vxa~schlafen|;live=habiter:ve~wohnen|;work=travailler:ve~arbeiten|;study=étudier:ve~studieren|;learn=apprendre:vxa@à~lernen|;teach=enseigner:ve~unterrichten|;read=lire:vxa~lesen|;write=écrire:vxa~schreiben|;',
    'play=jouer:ve~spielen|;sing=chanter:ve~singen|;dance=danser:ve~tanzen|;run=courir:vxa~laufen|s;walk=marcher:ve~gehen|s;drive=conduire:vxa~fahren|s;travel=voyager:ve~reisen|s;arrive=arriver:vee~kommen|s+an;',
    'leave=partir:vxe~fahren|s+ab;stay=rester:vee~bleiben|s;return=revenir:vxe~kommen|s+zurück;begin=commencer:ve@à~beginnen|;start=commencer:ve@à~fangen|+an;finish=finir:via~beenden|;stop=arrêter:ve@de~hören|+auf;',
    'open=ouvrir:vxa~öffnen|;close=fermer:ve~schließen|;help=aider:ve~helfen|d;wait=attendre:vr~warten|;try=essayer:vxa@de~versuchen|;call=appeler:vxa~rufen|+an;meet=rencontrer:ve~treffen|;visit=visiter:ve~besuchen|;',
    'use=utiliser:ve~benutzen|;forget=oublier:ve~vergessen|;choose=choisir:via~wählen|;decide=décider:ve@de~entscheiden|;hope=espérer:ve~hoffen|;enjoy=apprécier:ve~genießen|;miss=manquer:ve~vermissen|;send=envoyer:vxa~schicken|;',
    'bring=apporter:ve~bringen|;carry=porter:ve~tragen|;wear=porter:ve~tragen|;cost=coûter:ve~kosten|;change=changer:ve~ändern|;seem=sembler:ve~scheinen|;become=devenir:vxe~werden|s;show=montrer:ve~zeigen|;swim=nager:ve~schwimmen|s;',
    'smile=sourire:vxa~lächeln|;laugh=rire:vxa~lachen|;cry=pleurer:ve~weinen|;die=mourir:vxe~sterben|s;win=gagner:ve~gewinnen|;lose=perdre:vr~verlieren|;break=casser:ve~brechen|;build=construire:vxa~bauen|;enter=entrer:vee~betreten|;',
    'wash=laver:ve~waschen|;clean=nettoyer:vxa~putzen|;cut=couper:ve~schneiden|;prefer=préférer:vxa~bevorzugen|;remember=se rappeler:ve~behalten|;receive=recevoir:vxa~erhalten|;fall=tomber:vee~fallen|s;order=commander:ve~bestellen|;',
    'rain=pleuvoir:ve~regnen|;english=anglais:nm~Englisch|n/-;french=français:nm~Französisch|n/-;spanish=espagnol:nm~Spanisch|n/-;german=allemand:nm~Deutsch|n/-;italian=italien:nm~Italienisch|n/-;chinese=chinois:nm~Chinesisch|n/-;',
    'japanese=japonais:nm~Japanisch|n/-;russian=russe:nm~Russisch|n/-;portuguese=portugais:nm~Portugiesisch|n/-;arabic=arabe:nm~Arabisch|n/-;let=laisser:ve~lassen|;near here=près d''ici:r~in der Nähe;happen=arriver:vee~passieren|s;',
    'thank=remercier:ve~danken|d;a bit=un peu:r~ein bisschen;right=juste:aA!raison~richtig;wrong=faux:aA!tort~falsch;capital=capitale:nf~Hauptstadt|f/Hauptstädte;homework=devoir:nm~Hausaufgabe|f/Hausaufgaben;',
    'present=cadeau:nm/cadeaux~Geschenk|n/Geschenke;born=né:aA/née~geboren;help=aide:nf~Hilfe|f/-;last week=la semaine dernière:r~letzte Woche;last year=l''année dernière:r~letztes Jahr;last month=le mois dernier:r~letzten Monat;',
    'next month=le mois prochain:r~nächsten Monat;next year=l''année prochaine:r~nächstes Jahr;this week=cette semaine:r~diese Woche;this year=cette année:r~dieses Jahr;this month=ce mois-ci:r~diesen Monat;',
    'pizza=pizza:nf~Pizza|f/Pizzen;sandwich=sandwich:nm~Sandwich|n/Sandwiches;hamburger=hamburger:nm~Hamburger|m/-;to=à:p~zu|d;'
  );

  { Verbes francais irreguliers : inf|je|tu|il|nous|vous|ils|participe|futur|imparfait }
  IrregularData: Array[0..43] Of String = (
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
    'écrire|écris|écris|écrit|écrivons|écrivez|écrivent|écrit|écrir|écriv',
    'dormir|dors|dors|dort|dormons|dormez|dorment|dormi|dormir|dorm',
    'courir|cours|cours|court|courons|courez|courent|couru|courr|cour',
    'conduire|conduis|conduis|conduit|conduisons|conduisez|conduisent|conduit|conduir|conduis',
    'croire|crois|crois|croit|croyons|croyez|croient|cru|croir|croy',
    'comprendre|comprends|comprends|comprend|comprenons|comprenez|comprennent|compris|comprendr|compren',
    'apprendre|apprends|apprends|apprend|apprenons|apprenez|apprennent|appris|apprendr|appren',
    'ouvrir|ouvre|ouvres|ouvre|ouvrons|ouvrez|ouvrent|ouvert|ouvrir|ouvr',
    'sentir|sens|sens|sent|sentons|sentez|sentent|senti|sentir|sent',
    'partir|pars|pars|part|partons|partez|partent|parti|partir|part',
    'devenir|deviens|deviens|devient|devenons|devenez|deviennent|devenu|deviendr|deven',
    'revenir|reviens|reviens|revient|revenons|revenez|reviennent|revenu|reviendr|reven',
    'sourire|souris|souris|sourit|sourions|souriez|sourient|souri|sourir|souri',
    'rire|ris|ris|rit|rions|riez|rient|ri|rir|ri',
    'mourir|meurs|meurs|meurt|mourons|mourez|meurent|mort|mourr|mour',
    'construire|construis|construis|construit|construisons|construisez|construisent|construit|construir|construis',
    'recevoir|reçois|reçois|reçoit|recevons|recevez|reçoivent|reçu|recevr|recev',
    'obtenir|obtiens|obtiens|obtient|obtenons|obtenez|obtiennent|obtenu|obtiendr|obten',
    'connaître|connais|connais|connaît|connaissons|connaissez|connaissent|connu|connaîtr|connaiss',
    'acheter|achète|achètes|achète|achetons|achetez|achètent|acheté|achèter|achet',
    'appeler|appelle|appelles|appelle|appelons|appelez|appellent|appelé|appeller|appel',
    'préférer|préfère|préfères|préfère|préférons|préférez|préfèrent|préféré|préférer|préfér',
    'envoyer|envoie|envoies|envoie|envoyons|envoyez|envoient|envoyé|enverr|envoy',
    'payer|paie|paies|paie|payons|payez|paient|payé|paier|pay',
    'nettoyer|nettoie|nettoies|nettoie|nettoyons|nettoyez|nettoient|nettoyé|nettoier|nettoy',
    'essayer|essaie|essaies|essaie|essayons|essayez|essaient|essayé|essaier|essay',
    'pleuvoir|pleus|pleus|pleut|pleuvons|pleuvez|pleuvent|plu|pleuvr|pleuv',
    'se rappeler|me rappelle|te rappelles|se rappelle|nous rappelons|vous rappelez|se rappellent|rappelé|rappeller|rappel',
    'avoir besoin de|ai besoin de|as besoin de|a besoin de|avons besoin de|avez besoin de|ont besoin de|eu besoin de|aur besoin de|av besoin de'
  );

  { Formes anglaises irregulieres : forme=base:type }
  EnglishData: Array[0..4] Of String = (
    'went=go:p;gone=go:q;ate=eat:p;eaten=eat:q;saw=see:p;seen=see:q;took=take:p;taken=take:q;made=make:b;said=say:b;came=come:p;got=get:b;gave=give:p;given=give:q;knew=know:p;known=know:q;thought=think:b;told=tell:b;found=find:b;',
    'wrote=write:p;written=write:q;bought=buy:b;spoke=speak:p;spoken=speak:q;ran=run:p;drank=drink:p;drunk=drink:q;slept=sleep:b;met=meet:b;left=leave:b;felt=feel:b;sat=sit:b;stood=stand:b;lost=lose:b;paid=pay:b;heard=hear:b;',
    'brought=bring:b;built=build:b;sold=sell:b;sent=send:b;began=begin:p;begun=begin:q;drove=drive:p;driven=drive:q;read=read:b;put=put:b;cut=cut:b;chose=choose:p;chosen=choose:q;forgot=forget:p;forgotten=forget:q;',
    'understood=understand:b;became=become:p;won=win:b;broke=break:p;broken=break:q;fell=fall:p;fallen=fall:q;swam=swim:p;swum=swim:q;wore=wear:p;worn=wear:q;taught=teach:b;sang=sing:p;sung=sing:q;died=die:b;children=child:n;men=man:n;',
    'women=woman:n;feet=foot:n;teeth=tooth:n;mice=mouse:n;people=person:n;wives=wife:n;knives=knife:n;lives=life:n;leaves=leaf:n;'
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
        If (Length(Spec) > 1) And (Spec[2] = 'w') Then Lex[L].DeFlags := Lex[L].DeFlags + 'w';
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
    If (Lex[LexCount].Pos = 'v') And (Length(Part) > 1) Then Lex[LexCount].Aux := Part[2];
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
              (Copy(S, Length(S) - Length(Suffix) + 1, Length(Suffix)) = Suffix);
End;

Function StartsWith(Const S, Prefix: String): Boolean;
Begin
  StartsWith := Copy(S, 1, Length(Prefix)) = Prefix;
End;

Function IsAspirated(Const W: String): Boolean;
Begin
  IsAspirated := (W = 'haricot') Or (W = 'hasard') Or (W = 'héros') Or (W = 'hibou') Or
                 (W = 'honte') Or (W = 'hôtel hôtel') Or (W = 'hockey') Or (W = 'huit') Or
                 (W = 'hurler') Or (W = 'haut') Or (W = 'haute') Or (W = 'hauteur') Or
                 (W = 'hamburger') Or (W = 'handicap') Or (W = 'hanche') Or (W = 'hanter') Or
                 (W = 'hache') Or (W = 'hamster') Or (W = 'héron') Or (W = 'hiérarchie');
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
  If (C = $C5) And (Length(Word1) >= 2) And ((Ord(Word1[2]) = $93) Or (Ord(Word1[2]) = $92)) Then
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
  Endings: Array[1..6] Of String[6] = ('ais', 'ais', 'ait', 'ions', 'iez', 'aient');
Begin
  ConjugateConditional := FutureStem(Inf) + Endings[Slot];
End;

Function ConjugateImperfect(Const Inf: String; Slot: Integer): String;
Const
  Endings: Array[1..6] Of String[6] = ('ais', 'ais', 'ait', 'ions', 'iez', 'aient');
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
  Else If EndsWith(S, 'on') Or EndsWith(S, 'en') Or EndsWith(S, 'el') Or EndsWith(S, 'et') Then
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

{ Adjectif accorde ; BeforeVowel : pour beau/nouveau/vieux masculin devant voyelle. }
Function AdjForm(L: Integer; Gen: Char; Plural: Boolean; BeforeVowel: Boolean): String;
Var
  Base, Fem, Res: String;
  Invariable: Boolean;
Begin
  Base := Lex[L].Fr;
  Invariable := (Lex[L].Plu <> '') And (Lex[L].Plu = Base) And (Lex[L].Fem = Base);
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
    VForm: Char;        { b base, s 3e personne, g -ing, p passe, q participe, y passe ou participe }
    AdvFr: String[30];
    AdvA: Integer;
  End;

Function IsConsonant(C: Char): Boolean;
Begin
  IsConsonant := (C >= 'a') And (C <= 'z') And Not (C In ['a', 'e', 'i', 'o', 'u']);
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

{ Genre et preposition du lieu : 'm' pour au/du, 'f' pour en/de, 'p' pour aux/des ; ' ' si ce n'est pas un pays. }
Function CountryKind(Const W: String): Char;
Begin
  CountryKind := ' ';
  If (W = 'france') Or (W = 'spain') Or (W = 'germany') Or (W = 'italy') Or (W = 'england') Or
     (W = 'switzerland') Or (W = 'belgium') Or (W = 'china') Or (W = 'russia') Or (W = 'india') Or
     (W = 'america') Or (W = 'africa') Or (W = 'europe') Or (W = 'asia') Or (W = 'australia') Or
     (W = 'ireland') Or (W = 'scotland') Or (W = 'sweden') Or (W = 'norway') Or (W = 'poland') Or
     (W = 'greece') Or (W = 'turkey') Or (W = 'egypt') Or (W = 'algeria') Or (W = 'tunisia') Then
    CountryKind := 'f'
  Else If (W = 'canada') Or (W = 'portugal') Or (W = 'japan') Or (W = 'brazil') Or (W = 'mexico') Or
          (W = 'denmark') Or (W = 'morocco') Or (W = 'quebec') Then
    CountryKind := 'm'
  Else If (W = 'usa') Or (W = 'states') Then
    CountryKind := 'p';
End;

Function IsMealNoun(Const W: String): Boolean;
Begin
  IsMealNoun := (W = 'breakfast') Or (W = 'lunch') Or (W = 'dinner') Or (W = 'coffee') Or
                (W = 'tea') Or (W = 'meal') Or (W = 'drink') Or (W = 'juice') Or (W = 'beer') Or
                (W = 'wine') Or (W = 'milk') Or (W = 'cake') Or (W = 'soup');
End;

Function IsWeatherAdj(Const W: String): Boolean;
Begin
  IsWeatherAdj := (W = 'cold') Or (W = 'hot') Or (W = 'warm') Or (W = 'cool') Or (W = 'nice') Or
                  (W = 'bad') Or (W = 'good') Or (W = 'beautiful') Or (W = 'terrible');
End;

Function IsCalendar(Const W: String): Boolean;
Begin
  IsCalendar := (W = 'monday') Or (W = 'tuesday') Or (W = 'wednesday') Or (W = 'thursday') Or
                (W = 'friday') Or (W = 'saturday') Or (W = 'sunday') Or (W = 'january') Or
                (W = 'february') Or (W = 'march') Or (W = 'april') Or (W = 'may') Or (W = 'june') Or
                (W = 'july') Or (W = 'august') Or (W = 'september') Or (W = 'october') Or
                (W = 'november') Or (W = 'december') Or (W = 'english') Or (W = 'french') Or
                (W = 'german') Or (W = 'spanish') Or (W = 'italian') Or (W = 'chinese') Or
                (W = 'japanese') Or (W = 'russian') Or (W = 'portuguese') Or (W = 'arabic');
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
  IsPronounStem := (W = 'he') Or (W = 'she') Or (W = 'it') Or (W = 'that') Or (W = 'there') Or
                   (W = 'what') Or (W = 'where') Or (W = 'who') Or (W = 'how') Or (W = 'here') Or
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
    AddTok(Copy(W, 1, Length(W) - 3), Copy(Original, 1, Length(W) - 3), Cap, False);
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
      While (I <= L) And (IsWordChar(S[I]) Or ((S[I] = '''') And (I < L) And IsWordChar(S[I + 1]) And
            (I > Start)) Or ((S[I] = '-') And (I < L) And IsWordChar(S[I + 1]))) Do
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

{ Etiquettes : P pronom sujet, O pronom complement, H her, D article, S possessif,
  T demonstratif, Q quantificateur, X auxiliaire, G negation, K to, W interrogatif,
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
  Else If InList(W, 'some any many much few several every each all both no another more enough') Then
    ClosedTag := 'Q'
  Else If InList(W, 'am is are was were be been being have has had having do does did will would shall should can could may might must') Then
    ClosedTag := 'X'
  Else If W = 'not' Then ClosedTag := 'G'
  Else If W = 'to' Then ClosedTag := 'K'
  Else If InList(W, 'what who whom whose which where when why how') Then ClosedTag := 'W'
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
  If (W = 'she') Or (W = 'her') Then PronounGender := 'f' Else PronounGender := 'm';
End;

Function IsMassNoun(Const W: String): Boolean;
Begin
  IsMassNoun := InList(W, 'water bread cheese meat rice sugar salt money coffee tea milk wine beer juice food fruit chocolate soup salad fish time weather rain snow wind music information news help');
End;

Function IsLanguage(Const W: String): Boolean;
Begin
  IsLanguage := InList(W, 'english french german spanish italian chinese japanese russian portuguese arabic');
End;

Function IsProfession(Const W: String): Boolean;
Begin
  IsProfession := InList(W, 'teacher doctor student nurse driver worker president king queen soldier friend');
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
        Else If Tk[I].Cap And (I = 1) And Not (HasN Or HasV Or HasA Or HasR Or (C.P > 0) Or
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
            Else If (Prev = 'R') And (C.VForm In ['s', 'b', 'y']) And (I > 2) And
                    (Tk[I - 2].Tag In ['P', 'N', 'Z', 'X']) Then
              Tk[I].Tag := 'V'
            Else If (Prev = 'E') And (C.VForm = 'g') Then
              Tk[I].Tag := 'V'
            Else If (Prev = '.') And (C.VForm = 'b') And Not C.NPlural Then
              Tk[I].Tag := 'V'
            Else If (C.VForm In ['s', 'g', 'p', 'q', 'y']) And Not C.NPlural Then
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
            If Prev In ['X', 'D', 'S', 'R'] Then Tk[I].Tag := 'A' Else Tk[I].Tag := 'V';
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
           InList(Tk[I - 1].W, 'go goes went going come comes came walk walks walked drive drives drove') Then
        Begin
          Tk[I].Tag := 'E';
          Tk[I + 1].Tag := 'N';
          Tk[I + 1].NPlural := False;
        End
        Else If (Tk[I + 1].Tag = 'V') And (Tk[I + 1].VForm In ['b', 's', 'y']) Then
          Tk[I].Tag := 'K'
        Else If (Tk[I + 1].Tag = 'N') And (Tk[I + 1].V > 0) And (I > 1) And
                InList(Tk[I - 1].W, 'want need like love hate try start begin learn decide hope prefer going wish forget remember stop have has had able') Then
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
    If (Tk[I].W = 'that') And (Tk[I - 1].Tag = 'V') And (Tk[I + 1].Tag In ['P', 'D', 'S', 'T', 'N', 'Z']) Then
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

Function PossessiveFr(Const W: String; Gen: Char; Plural: Boolean; VowelNext: Boolean): String;
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
  DetWord, NumberFr, QuantFr, AdvText, Pre, Post, NounText, Noun1, PossText: String;
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
        If Tk[I].R > 0 Then AdjAdv[AdjCount] := Lex[Tk[I].R].Fr Else AdjAdv[AdjCount] := '';
      End;
      Inc(I, 2);
    End
    Else If (Tk[I].Tag = 'C') And (Tk[I].W = 'and') And (AdjCount > 0) And (I < TN) And
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
  If (DetTag = 'Q') And InList(DetWord, 'many few several both these those') Then Plu := True;
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
        If (K > 1) And (Lex[Adjs[K - 1]].Sub = 'A') And (AdjAdv[K] = '') Then Post := Post + ' et';
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
          If Plu Then Det := 'les' Else If Gen = 'f' Then Det := 'la' Else Det := 'le';
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
          If Plu Then Det := 'les' Else If Gen = 'f' Then Det := 'la' Else Det := 'le';
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
        If (I + 3 <= TN) And (Tk[I + 1].W = 'going') And (Tk[I + 2].Tag = 'K') And
           (Tk[I + 3].Tag = 'V') Then
        Begin
          VG.Going := True;
          Inc(I, 2);
        End
        Else If (I < TN) And (Tk[I + 1].Tag = 'V') And (Tk[I + 1].VForm = 'g') Then
          VG.Prog := True
        Else If (I < TN) And (Tk[I + 1].Tag = 'V') And (Tk[I + 1].VForm In ['q', 'y']) And
                Not VG.Perfect Then
          VG.Passive := True
        Else
          VG.BeMain := True;
      End
      Else If InList(W, 'have has had') Then
      Begin
        J := I + 1;
        While (J < TN) And ((Tk[J].Tag = 'G') Or (Tk[J].W = 'never') Or
              ((Tk[J].Tag = 'R') And InList(Tk[J].W, 'ever already just always often'))) Do
          Inc(J);
        If (J <= TN) And (J > I) And (((Tk[J].Tag = 'V') And (Tk[J].VForm In ['q', 'y', 'p'])) Or
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
    Else If (Tk[I].Tag = 'R') And (I < TN) And (Tk[I + 1].Tag In ['X', 'V']) Then
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
  Else If (En = 'nice') Or (En = 'beautiful') Or (En = 'good') Then WeatherFr := 'beau'
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
      If (W = 'on') And InList(Tk[K].W, 'monday tuesday wednesday thursday friday saturday sunday') Then
      Begin
        TranslatePP := Lex[Tk[K].N].Fr;
        I := K + 1;
        Exit;
      End;
      If (W = 'in') And IsCalendar(Tk[K].W) And Not InList(Tk[K].W, 'monday tuesday wednesday thursday friday saturday sunday english french german spanish italian chinese japanese russian portuguese arabic') Then
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
      If (I <= TN) And ((Tk[I].W = 'o''clock') Or (Tk[I].W = 'oclock')) Then Inc(I);
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
  If W = 'of' Then Kind := 'o' Else If W = 'in' Then Kind := 's' Else Kind := ' ';
  If ParseNP(J, Kind, NP) Then
  Begin
    TranslatePP := PrepW + ' ' + NP.Text;
    I := J + NP.Used;
    Exit;
  End;
  I := J;
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

{ Complements apres le verbe : renvoie le texte, les pronoms complements a placer
  devant le verbe (Clit) ou devant l'infinitif (ClitInf). }
Function TranslateTail(First, Last: Integer; Const VerbEn: String; Var Clit: String;
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
  BeLike := CurVG.BeMain Or InList(VerbEn, 'become seem look feel taste smell sound');
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
        If ParseNP(I, Mode, NP) Then
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
        If (Tk[I].Tag = 'R') And (I < Last) And (Tk[I + 1].Tag = 'A') And BeLike Then
        Begin
          If Tk[I].R > 0 Then AdvText := Lex[Tk[I].R].Fr Else AdvText := Tk[I].Fixed;
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
        If Tk[I].O > 0 Then Res := Join(Res, Lex[Tk[I].O].Fr) Else Res := Join(Res, Tk[I].Orig);
        Inc(I);
      End;
      'J':
      Begin
        If Tk[I].O > 0 Then Res := Join(Res, Lex[Tk[I].O].Fr) Else Res := Join(Res, Tk[I].Orig);
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

{ Verbe dont le passe s'ecrit comme l'infinitif (read, put, cut) : passe seulement avec un indice de temps. }
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

Procedure BuildVerbForms(Const VG: TVG; Const S: TSubj; Const Inf0: String; AuxType: Char;
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
    If PastFlag Then Tense := 'i' Else If VG.Future Then Tense := 'f' Else Tense := 'p';
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
  Else If (VG.Perfect Or ((PastFlag Or MainPast) And Not VG.BeMain And Not VG.HaveMain And
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
  WhW, Wh, Inf0, Fin, Nonfin, Tail, Clit, ClitInf, Res, VerbEn, Idiom, Pre, Pos2: String;
  Subj: TSubj;
  VG: TVG;
  OnInf, Inverted, Imperative, Please, BeLike, Inv, EstCe, WhSubject, NoPas: Boolean;
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
  If (Tk[A].Tag = 'R') And (A < B) And (Tk[A + 1].Tag In ['P', 'D', 'S', 'N', 'Z', 'T', 'Q', 'U']) And
     (Tk[A].R > 0) Then
  Begin
    TranslateClause := Lex[Tk[A].R].Fr + ' ' + TranslateClause(A + 1, B, IsQ);
    Exit;
  End;

  { mot interrogatif }
  If Tk[A].Tag = 'W' Then
  Begin
    WhW := Tk[A].W;
    If (WhW = 'how') And (A < B) And ((Tk[A + 1].W = 'much') Or (Tk[A + 1].W = 'many')) Then
    Begin
      Wh := 'combien';
      If Tk[A + 1].W = 'many' Then Wh := 'combien de';
      Inc(A, 2);
    End
    Else If ((WhW = 'what') Or (WhW = 'which')) And (A < B) And (Tk[A + 1].Tag = 'N') Then
    Begin
      If ParseNP(A + 1, ' ', NP) Then
      Begin
        Wh := QuelFr(NP.Gen, NP.Plu) + ' ' + NounForm(Tk[A + 1].N, Tk[A + 1].NPlural);
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
      If InList(Tk[B - 1].W, 'was were') Then Res := Res + ' y avait-il' Else Res := Res + ' y a-t-il';
      TranslateClause := Res;
      Exit;
    End;
  End;

  { inversion sujet-auxiliaire : on remet la phrase dans l'ordre sujet-auxiliaire }
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
    If (Tk[A].Tag = 'V') And (Tk[A].VForm In ['b', 'y']) And (Wh = '') And Not IsQ Then
      Imperative := True
    Else If (Tk[A].W = 'do') And (A + 2 <= B) And (Tk[A + 1].Tag = 'G') And (Tk[A + 2].Tag = 'V') And
            (Wh = '') Then
    Begin
      Imperative := True;
      Tk[A + 1].Tag := 'G';
    End
    Else If (Tk[A].W = 'let') And (A + 2 <= B) And (Tk[A + 1].W = 'us') And (Tk[A + 2].Tag = 'V') Then
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

  { sujet }
  If Not Inverted Then
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
            ((Subj.IsIt And Not Subj.IsDemo) Or Subj.IsThere) And (Subj.Text <> 'ce') Then
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
  If VG.HaveMain And (Idiom = '') And (Next <= B) And (Tk[Next].Tag In ['N', 'D', 'Q']) Then
  Begin
    K := Next;
    While (K <= B) And Not (Tk[K].Tag = 'N') Do
      Inc(K);
    If (K <= B) And (Tk[K].N > 0) And IsMealNoun(Lex[Tk[K].N].En) Then Inf0 := 'prendre';
  End;
  If (VerbEn = 'know') And (Next <= B) And (Tk[Next].Tag In ['Z', 'O']) Then Inf0 := 'connaître';
  If (VerbEn = 'leave') And (Next <= B) And (Tk[Next].Tag In ['D', 'S', 'N', 'Z', 'O']) Then
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

  { verbes a particule : look for, look at, wait for, listen to, ask for, talk about }
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
    If (Inf0 = 'chercher') Or (Inf0 = 'regarder') Or (Inf0 = 'attendre') Or (Inf0 = 'écouter') Or
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
    If (Not VG.HaveTo) And (VG.MainTok = 0) And Not VG.HaveMain And Not VG.BeMain Then Inf0 := '';
  BuildVerbForms(VG, Subj, Inf0, AuxType, Fin, Nonfin, OnInf);
  If Subj.IsThere Then
  Begin
    If VG.Past Then Tense := 'i' Else If VG.Future Then Tense := 'f' Else If VG.Cond Then Tense := 'c' Else Tense := 'p';
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

  { clitiques : devant l'infinitif apres un modal / aller, sinon devant le verbe }
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
    Else If Subj.IsPron And Subj.IsI And (VG.Modal = 'p') And (Not VG.ModalCond) Then
      Inv := True
    Else If (Not Subj.IsPron) And (Wh <> '') And (Inf0 = 'être') And Not VG.Neg And
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
        If EndsWith(Fin, 'a') Or EndsWith(Fin, 'e') Then FinInv := FinInv + '-t';
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
  Else If ((C = 'm') Or (C = 'n')) And Not (P In ['a', 'e', 'i', 'o', 'u', 'l', 'r', 'h', 'm', 'n']) Then
    NeedsE := True;
End;

Function DeFullInf(L: Integer): String;
Begin
  DeFullInf := Lex[L].DeSep + Lex[L].De;
End;

{ Verbes de modalite et auxiliaires : present (tense p), preterit (i), subjonctif II (c). }
Function DeSpecialForm(Const Inf: String; Slot: Integer; Tense: Char): String;
Const
  Sein: Array[1..6] Of String[6] = ('bin', 'bist', 'ist', 'sind', 'seid', 'sind');
  SeinP: Array[1..6] Of String[6] = ('war', 'warst', 'war', 'waren', 'wart', 'waren');
  Haben: Array[1..6] Of String[6] = ('habe', 'hast', 'hat', 'haben', 'habt', 'haben');
  HabenP: Array[1..6] Of String[8] = ('hatte', 'hattest', 'hatte', 'hatten', 'hattet', 'hatten');
  Werden: Array[1..6] Of String[8] = ('werde', 'wirst', 'wird', 'werden', 'werdet', 'werden');
  WerdenK: Array[1..6] Of String[8] = ('würde', 'würdest', 'würde', 'würden', 'würdet', 'würden');
  Endings: Array[1..6] Of String[3] = ('e', 'est', 'e', 'en', 'et', 'en');
  Endings2: Array[1..6] Of String[3] = ('', 'st', '', 'en', 't', 'en');
Var
  Stem: String;
Begin
  DeSpecialForm := '';
  If Inf = 'sein' Then
  Begin
    If Tense = 'p' Then DeSpecialForm := Sein[Slot] Else If Tense = 'c' Then DeSpecialForm := 'wäre' + Copy(Endings[Slot], 2, 3) Else DeSpecialForm := SeinP[Slot];
    If (Tense = 'c') And (Slot = 2) Then DeSpecialForm := 'wärst';
    If (Tense = 'c') And (Slot = 4) Then DeSpecialForm := 'wären';
    If (Tense = 'c') And (Slot = 5) Then DeSpecialForm := 'wärt';
    If (Tense = 'c') And (Slot = 6) Then DeSpecialForm := 'wären';
    If (Tense = 'c') And (Slot In [1, 3]) Then DeSpecialForm := 'wäre';
  End
  Else If Inf = 'haben' Then
  Begin
    If Tense = 'p' Then DeSpecialForm := Haben[Slot] Else DeSpecialForm := HabenP[Slot];
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
  Else If InList(Inf, 'können müssen wollen sollen dürfen mögen möchten') Then
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
  IsDeSpecial := InList(Inf, 'sein haben werden wissen können müssen wollen sollen dürfen mögen möchten');
End;

{ Forme conjuguee (sans prefixe separable). Tense : p present, i preterit, c subjonctif II. }
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
      If EndsWith(Inf, 'eln') Then DeFinite := Copy(Stem, 1, Length(Stem) - 2) + 'le'
      Else DeFinite := Stem + 'e';
    2:
      If Irr > 0 Then DeFinite := DeIrr[Irr].Du
      Else If NeedsE(Stem) Then DeFinite := Stem + 'est'
      Else If EndsWith(Stem, 's') Or EndsWith(Stem, 'ß') Or EndsWith(Stem, 'x') Or EndsWith(Stem, 'z') Then
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
  HasInseparablePrefix := (Length(Inf) > 6) And (StartsWith(Inf, 'be') Or StartsWith(Inf, 'ver') Or
                          StartsWith(Inf, 'er') Or StartsWith(Inf, 'ent') Or StartsWith(Inf, 'ge') Or
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
  If C = 'a' Then CaseIndex := 2 Else If C = 'd' Then CaseIndex := 3 Else CaseIndex := 1;
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

{ Terminaison de l'adjectif : K = d (apres article defini), m (apres ein-), s (sans article). }
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

Function AdjectiveDe(L: Integer; K: Char; G: Char; Plural: Boolean; C: Char): String;
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
    If EndsWith(Stem, 'el') And (Length(Stem) > 3) Then Stem := Copy(Stem, 1, Length(Stem) - 2) + 'l';
  End;
  AdjectiveDe := Stem + AdjEnding(K, G, Plural, C);
End;

{ Nom allemand : pluriel, datif pluriel, declinaison faible des masculins en -n. }
Function NounDe(L: Integer; Plural: Boolean; C: Char): String;
Var
  S: String;
Begin
  S := Lex[L].De;
  If Plural Then
  Begin
    If (Lex[L].DePl <> '') And (Lex[L].DePl <> '-') Then S := Lex[L].DePl;
    If (C = 'd') And Not (EndsWith(S, 'n') Or EndsWith(S, 's')) Then S := S + 'n';
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
  DeCountryArticle := (W = 'switzerland') Or (W = 'turkey') Or (W = 'usa') Or (W = 'states');
End;

Function IsDeCountry(Const W: String): Boolean;
Begin
  IsDeCountry := InList(W, 'germany france spain italy england canada switzerland austria belgium portugal china japan russia brazil mexico india america africa europe asia australia ireland scotland sweden norway denmark poland greece turkey egypt morocco usa');
End;

{ ------------------------------------------------------------------ }
{ ALLEMAND : groupe nominal                                           }
{ ------------------------------------------------------------------ }

Type
  TDeNP = Record
    Found, Proper, Plural, Neg, Pron: Boolean;
    Used: Integer;
    Kind: Char;           { d defini, i indefini, p possessif, k demonstratif, q quantificateur, n nombre, 0 aucun }
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
  If (R <> '') And (R[1] >= 'A') And (R[1] <= 'Z') Then R[1] := Chr(Ord(R[1]) + 32);
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
        If DeNumber(Tk[I].W) <> '' Then NP.NumText := DeNumber(Tk[I].W) Else NP.NumText := Tk[I].W;
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
        If Tk[I].R > 0 Then NP.AdjAdv[NP.AdjCount] := Lex[Tk[I].R].De Else NP.AdjAdv[NP.AdjCount] := '';
      End;
      Inc(I, 2);
    End
    Else If (Tk[I].Tag = 'C') And (Tk[I].W = 'and') And (NP.AdjCount > 0) And (I < TN) And
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
  If (NP.Kind = 'q') And InList(NP.DetWord, 'many few several both these those some any') Then
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
  If ForceDef And (Kd = '0') And Not Plu And Not IsMassDe(HeadEn(NP)) Then Kd := 'd';
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
    Else If (W = 'her') Or (W = 'she') Or (W = 'them') Or (W = 'they') Then PronounDe := 'sie'
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
  IsMotionVerb := InList(En, 'go come travel return put place bring carry take send throw fly');
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
  If EndsWith(S, 'Arzt') Then FemProfession := Copy(S, 1, Length(S) - 4) + 'Ärztin'
  Else If EndsWith(S, 'Koch') Then FemProfession := Copy(S, 1, Length(S) - 4) + 'Köchin'
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
  If (W = 'to') And (DeCurVerb = 'be') And (I < TN) And (Tk[I + 1].Tag = 'Z') Then W := 'in';
  { verbes a preposition fixe }
  If (W = 'for') And (DeCurVerb = 'wait') Then Begin PrepW := 'auf'; PrepCase := 'a'; End
  Else If (W = 'for') And (DeCurVerb = 'ask') Then Begin PrepW := 'um'; PrepCase := 'a'; End
  Else If (W = 'at') And (DeCurVerb = 'look') Then Begin PrepW := 'auf'; PrepCase := 'a'; End
  Else If (W = 'about') And InList(DeCurVerb, 'talk speak think') Then Begin PrepW := 'über'; PrepCase := 'a'; End;
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
  If (K <= TN) And (Tk[K].N > 0) And ((W = 'in') Or (W = 'at') Or (W = 'on')) Then
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
    If (W = 'on') And InList(Tk[K].W, 'monday tuesday wednesday thursday friday saturday sunday') Then
    Begin
      TranslatePPDe := 'am ' + Lex[Tk[K].N].De;
      I := K + 1;
      Exit;
    End;
    If (W = 'in') And IsCalendar(Tk[K].W) And
       Not InList(Tk[K].W, 'monday tuesday wednesday thursday friday saturday sunday english french german spanish italian chinese japanese russian portuguese arabic') Then
    Begin
      TranslatePPDe := 'im ' + Lex[Tk[K].N].De;
      I := K + 1;
      Exit;
    End;
  End;
  If (Tk[J].Tag = 'U') And (W = 'at') And (DeNumber(Tk[J].W) <> '') And (Tk[J].W[1] > '9') Then
  Begin
    TranslatePPDe := 'um ' + DeNumber(Tk[J].W) + ' Uhr';
    I := J + 1;
    If (I <= TN) And ((Tk[I].W = 'o''clock') Or (Tk[I].W = 'oclock')) Then Inc(I);
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
    If (I <= TN) And ((Tk[I].W = 'o''clock') Or (Tk[I].W = 'oclock')) Then Inc(I);
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
    If (W = 'of') And (I > 1) And InList(Tk[I - 1].W, 'bottle bottles glass glasses cup cups piece pieces kilo kilos liter liters slice slices') Then
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

Procedure ParseTailDe(First, Last: Integer; Const VerbEn: String; BeLike, Neg: Boolean;
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
           InList(VerbEn, 'give send show tell bring teach write say sell lend offer explain listen') And
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
        If InList(PP, 'morgens nachmittags abends nachts') Or (Copy(PP, 1, 3) = 'am ') Or
           (Copy(PP, 1, 3) = 'im ') And (Pos(' ', Copy(PP, 4, 40)) = 0) Then
          T.Advs := Join(T.Advs, PP)
        Else If Copy(PP, 1, 4) = 'mit ' Then
          T.Manner := Join(T.Manner, PP)
        Else
          T.PPs := Join(T.PPs, PP);
      End;
      'N', 'D', 'S', 'T', 'Q', 'U', 'Z':
      Begin
        If (Tk[I].Tag = 'N') And (Tk[I].W = 'home') And ((I = First) Or Not (Tk[I - 1].Tag In ['D', 'S', 'A'])) Then
        Begin
          If Motion Then T.PPs := Join(T.PPs, 'nach Hause') Else T.PPs := Join(T.PPs, 'zu Hause');
          Inc(I);
        End
        Else If ParseNPDe(I, ' ', NP) Then
        Begin
          If BeLike And (T.ObjCount = 0) Then
          Begin
            { complement d'un etre : nominatif }
            If Neg And (NP.Kind In ['i', '0']) And Not NP.Proper Then NP.Neg := True;
            If (NP.Kind = 'i') And (Not NP.Plural) And (NP.AdjCount = 0) And IsProfession(HeadEn(NP)) Then
              NP.Kind := '0';
            If DeFemSubj And (NP.Kind = '0') And (Not NP.Plural) And (NP.Gen = 'm') And
               IsProfession(HeadEn(NP)) Then
              T.Pred := Join(T.Pred, FemProfession(RenderNPDe(NP, 'n', False)))
            Else
              T.Pred := Join(T.Pred, RenderNPDe(NP, 'n', False));
          End
          Else If T.ObjCount < 3 Then
          Begin
            If Neg And (NP.Kind In ['i', '0', 'q']) And Not NP.Proper Then
              If (NP.Kind <> 'q') Or InList(NP.DetWord, 'some any') Then NP.Neg := True;
            If (NP.Kind = '0') And (Not NP.Plural) And (Not NP.Proper) And
               (Not IsMassDe(HeadEn(NP))) And (Not Neg) And Not InList(VerbEn, 'like love hate prefer enjoy') Then
              NP.Kind := '0';
            If (NP.Kind = '0') And (Not NP.Plural) And (Not NP.Proper) And (NP.Head > 0) And
               IsMealNoun(HeadEn(NP)) And InList(VerbEn, 'cook make prepare') Then
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
        If (Tk[I].W = 'very') And (I < Last) And (Tk[I + 1].Tag = 'A') And BeLike Then
        Begin
          T.Pred := Join(T.Pred, 'sehr');
          Inc(I);
        End
        Else If (Tk[I].W = 'too') And (I < Last) And (Tk[I + 1].Tag = 'A') And BeLike Then
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
        If BeLike And (T.Pred <> '') And (I < Last) And InList(Tk[I].W, 'and or but') Then
        Begin
          If Tk[I].W = 'but' Then T.Pred := T.Pred + ', aber'
          Else If Tk[I].W = 'and' Then T.Pred := T.Pred + ' und'
          Else T.Pred := T.Pred + ' oder';
          Inc(I);
          Continue;
        End;
        If InList(Tk[I].W, 'and or') And (I < Last) And (T.ObjCount > 0) And (T.Pred = '') And
           (Not T.Objs[T.ObjCount].IsPron) And (Not T.Objs[T.ObjCount].HasCo) And
           (Tk[I + 1].Tag In ['N', 'D', 'S', 'T', 'Q', 'U', 'A']) And ParseNPDe(I + 1, ' ', NP) Then
        Begin
          T.Objs[T.ObjCount].HasCo := True;
          T.Objs[T.ObjCount].Co := NP;
          If Tk[I].W = 'and' Then T.Objs[T.ObjCount].CoConj := 'und' Else T.Objs[T.ObjCount].CoConj := 'oder';
          Inc(I, 1 + NP.Used);
          Continue;
        End;
        If Tk[I].O > 0 Then T.PPs := Join(T.PPs, LowerFirst(Lex[Tk[I].O].De)) Else T.PPs := Join(T.PPs, Tk[I].Orig);
        Inc(I);
      End;
      ',':
      Begin
        T.PPs := T.PPs + ',';
        Inc(I);
      End;
      'V':
      Begin
        If Tk[I].V > 0 Then T.PPs := Join(T.PPs, DeFullInf(Tk[I].V)) Else T.PPs := Join(T.PPs, Tk[I].Orig);
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
  If (ConjDe <> '') And (ConjDe[1] >= 'A') And (ConjDe[1] <= 'Z') And (Tk[I].Tag = 'C') Then
    ConjDe := LowerFirst(ConjDe);
End;

Function IsSubConj(I: Integer): Boolean;
Begin
  IsSubConj := (Tk[I].Tag = 'C') And (Tk[I].O > 0) And (Lex[Tk[I].O].DeFlags = 'S');
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
    If (Tk[I].Tag = 'X') And InList(Tk[I].W, 'can may could might must should') Then
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
  IsModalDe := InList(Inf, 'können müssen wollen sollen dürfen mögen möchten');
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

Procedure BuildVerbDe(Const VG: TVG; Slot: Integer; L: Integer; ModalWord: String;
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
Function AssembleDe(Const Front, SubjText, Fin, FinPrefix, Rest, Middle, Trail: String;
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

{ Ordre du Mittelfeld : pronoms, adverbes, groupes nominaux (datif avant accusatif). }
Function MiddleDe(Const T: TDeTail; DatVerb, Nicht: Boolean; Const Gern: String): String;
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
        Txt := Join(Txt, Join(T.Objs[K].CoConj, RenderNPDe(T.Objs[K].Co, C, False)));
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

Function TranslateClauseDe(A, B: Integer; IsQ, Final: Boolean; Const FrontIn: String): String;
Var
  I, Aux0, EndSubj, K, Next, L, KPos, KV, InfL, Pass: Integer;
  Front, Wh, WhW, Fin, FinPrefix, Rest, Res, VerbEn, Mid, Trail, Gern, SubjText, ModalWord, Tmp2: String;
  Subj: TDeSubj;
  VG: TVG;
  T, T2: TDeTail;
  Tmp: TTok;
  Imperative, Please, WhSubject, Inverted, DatVerb, Nicht, Motion, ZuType, Opt, HaveIdiom: Boolean;
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
    TranslateClauseDe := ConjDe(A) + ' ' + TranslateClauseDe(A + 1, B, IsQ, Final, Front);
    Exit;
  End
  Else If (Tk[A].Tag = 'J') And (A = B) Then
  Begin
    TranslateClauseDe := UpperFirst(ConjDe(A));
    Exit;
  End;
  If (Tk[A].Tag = 'C') And (A < B) Then
  Begin
    TranslateClauseDe := ConjDe(A) + ' ' + TranslateClauseDe(A + 1, B, IsQ, Final, Front);
    Exit;
  End;

  { adverbe ou groupe prepositionnel de temps en tete : le sujet passe apres le verbe }
  If (Front = '') And (Not Final) And (Tk[A].Tag = 'R') And (A < B) And
     (Tk[A + 1].Tag In ['P', 'D', 'S', 'N', 'Z', 'T', 'Q', 'U']) And (Tk[A].R > 0) Then
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
    If (WhW = 'how') And (A < B) And ((Tk[A + 1].W = 'much') Or (Tk[A + 1].W = 'many')) Then
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
    Else If ((WhW = 'what') Or (WhW = 'which')) And (A < B) And (Tk[A + 1].Tag = 'N') Then
    Begin
      If ParseNPDe(A + 1, ' ', NP) Then
      Begin
        Wh := 'welch' + DerEnding(NP.Gen, NP.Plural, 'n') + ' ' + NounDe(NP.Head, NP.Plural, 'n');
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

  { inversion sujet - auxiliaire : on remet la phrase dans l'ordre sujet - auxiliaire }
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
    If (Tk[A].W = 'let') And (A + 2 <= B) And (Tk[A + 1].W = 'us') And (Tk[A + 2].Tag = 'V') Then
    Begin
      L := Tk[A + 2].V;
      ParseTailDe(A + 3, B, Lex[L].En, False, False, T, IsMotionVerb(Lex[L].En));
      DatVerb := Pos('d', Lex[L].DeFlags) > 0;
      Mid := MiddleDe(T, DatVerb, False, '');
      Res := AssembleDe('', 'wir', DeFinite(L, 4, 'p'), Lex[L].DeSep, '', Mid, '', '1');
      TranslateClauseDe := Res;
      Exit;
    End
    Else If (Tk[A].Tag = 'V') And (Tk[A].VForm In ['b', 'y']) And (Wh = '') And Not IsQ Then
      Imperative := True
    Else If (Tk[A].W = 'be') And (A < B) And (Wh = '') And Not IsQ Then
      Imperative := True
    Else If (Tk[A].W = 'do') And (A + 2 <= B) And (Tk[A + 1].Tag = 'G') And (Tk[A + 2].Tag = 'V') And
            (Wh = '') Then
      Imperative := True
    Else If (Tk[A].W = 'let') And (A + 2 <= B) And (Tk[A + 1].W = 'us') And (Tk[A + 2].Tag = 'V') Then
    Begin
      L := Tk[A + 2].V;
      ParseTailDe(A + 3, B, Lex[L].En, False, False, T, IsMotionVerb(Lex[L].En));
      DatVerb := Pos('d', Lex[L].DeFlags) > 0;
      Mid := MiddleDe(T, DatVerb, False, '');
      Res := AssembleDe('', 'wir', DeFinite(L, 4, 'p'), Lex[L].DeSep, '', Mid, '', '1');
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
    Res := AssembleDe('', 'Sie', DeFinite(L, 6, 'p'), Lex[L].DeSep, '', Mid, '', '1');
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
    TranslateClauseDe := Join(Wh, Join(SubjText, MiddleDe(T, False, False, '')));
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
      If Idiom = 'unrecht' Then Idiom := 'unrecht' Else If Idiom = 'recht' Then Idiom := 'recht';
      VG.BeMain := False;
      VG.HaveMain := True;
      HaveIdiom := True;
      Inc(Next);
    End
    Else If (Tk[Next].Tag = 'A') And InList(Lex[Tk[Next].A].En, 'cold hot warm') And Subj.IsPron And
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
  If VG.HaveMain And (Idiom = '') And (Next <= B) And (Tk[Next].Tag In ['N', 'D', 'Q']) Then
  Begin
    K := Next;
    While (K <= B) And Not (Tk[K].Tag = 'N') Do
      Inc(K);
    If (K <= B) And (Tk[K].N > 0) And IsMealNoun(Lex[Tk[K].N].En) Then
    Begin
      L := DeVerb('essen');
      If InList(Lex[Tk[K].N].En, 'coffee tea water juice milk wine beer') Then L := DeVerb('trinken');
      VerbEn := 'eat';
      VG.HaveMain := False;
    End;
  End;
  If (VerbEn = 'know') And (Next <= B) And (Tk[Next].Tag In ['Z', 'O', 'D', 'S', 'N']) And
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
      If (Tk[K].W = 'by') And (Tk[K + 1].N > 0) And IsVehicle(Lex[Tk[K + 1].N].En) Then
        If VerbEn = 'go' Then L := DeVerb('fahren');

  { verbe de pensee suivi d'une proposition sans "that" }
  If (VG.MainTok > 0) And InList(VerbEn, 'think believe hope say feel know') And (Next < B) Then
    If ParseSubjectDe(Next, Sub2) Then
      If (Next + Sub2.Used <= B) And (Tk[Next + Sub2.Used].Tag In ['X', 'V']) Then
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
      If (KPos = 0) And (Tk[K].Tag = 'K') And (K < B) And (Tk[K + 1].Tag = 'V') Then
      Begin
        KPos := K;
        KV := Tk[K + 1].V;
      End
      Else If (KPos = 0) And (K = Next) And (Tk[K].Tag = 'V') And (Tk[K].VForm = 'g') And
              InList(VerbEn, 'like love hate prefer enjoy start begin stop finish') Then
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
      ParseTailDe(KPos + 2, B, Lex[KV].En, False, False, T2, IsMotionVerb(Lex[KV].En));
      Trail := MiddleDe(T2, Pos('d', Lex[KV].DeFlags) > 0, False, '');
      If Trail <> '' Then Trail := ', ' + Join(Trail, ZuInfDe(KV))
      Else Trail := ZuInfDe(KV);
      If Trail[1] <> ',' Then Trail := ' ' + Trail;
    End
    Else
    Begin
      { modal / gern : un seul groupe, le verbe est celui de l'infinitif }
      ParseTailDe(Next, B, Lex[KV].En, False, Nicht, T, IsMotionVerb(Lex[KV].En));
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
    If (VG.BeMain Or Subj.IsThere) And (VerbEn = 'be') And (Not Subj.IsThere) Then
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
  If VG.Passive And (VerbEn = 'be') And (L > 0) And (Lex[L].De = 'gebären') Then
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
      TranslateClauseDe := Join(Wh + ' geht es ' + PronounDe(Subj.PronW, 'd'), Tmp2);
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
      If (Tk[K].Tag = ',') And (Tk[K].W = ',') And ClauseHasVerb(A + 1, K - 1) And ClauseHasVerb(K + 1, B) Then
      Begin
        SubText := ConjDe(A) + ' ' + TranslateClauseDe(A + 1, K - 1, False, True, '');
        TranslateTokensDe := TranslateClauseDe(K + 1, B, IsQ, False, SubText + ',');
        Exit;
      End;
  For K := A + 1 To B - 1 Do
    If (Tk[K].Tag = ',') And (Tk[K].W = ',') Then
      If ((K - A = 1) And (Tk[A].Tag = 'J')) Or
         (ClauseHasVerb(A, K - 1) And ClauseHasVerb(K + 1, B)) Then
      Begin
        TranslateTokensDe := TranslateTokensDe(A, K - 1, False) + ', ' + TranslateTokensDe(K + 1, B, IsQ);
        Exit;
      End;
  For K := A + 1 To B - 1 Do
    If (Tk[K].Tag = 'C') And ClauseHasVerb(A, K - 1) And ClauseHasVerb(K + 1, B) Then
    Begin
      If IsSubConj(K) Then
        TranslateTokensDe := TranslateTokensDe(A, K - 1, False) + ', ' + ConjDe(K) + ' ' +
                             TranslateClauseDe(K + 1, B, False, True, '')
      Else If (ConjDe(K) = 'aber') Or (ConjDe(K) = 'sondern') Then
        TranslateTokensDe := TranslateTokensDe(A, K - 1, False) + ', ' + ConjDe(K) + ' ' +
                             TranslateTokensDe(K + 1, B, IsQ)
      Else
        TranslateTokensDe := TranslateTokensDe(A, K - 1, False) + ' ' + ConjDe(K) + ' ' +
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
      Else If Words[I] = 'an' Then Begin Words[I] := 'am'; Words[I + 1] := ''; End
      Else If Words[I] = 'zu' Then Begin Words[I] := 'zum'; Words[I + 1] := ''; End
      Else If Words[I] = 'von' Then Begin Words[I] := 'vom'; Words[I + 1] := ''; End
      Else If Words[I] = 'bei' Then Begin Words[I] := 'beim'; Words[I + 1] := ''; End;
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
  Else If InList(W, 'pas jamais rien personne aucun aucune guère') Then FrClosedCat := 'g'
  Else If InList(W, 'je tu il elle on ils elles ça ceci cela') Then FrClosedCat := 'P'
  Else If InList(W, 'nous vous') Then FrClosedCat := 'P'
  Else If InList(W, 'me te se lui y') Then FrClosedCat := 'O'
  Else If InList(W, 'moi toi eux soi') Then FrClosedCat := 'B'
  Else If InList(W, 'le la les un une des du') Then FrClosedCat := 'D'
  Else If InList(W, 'au aux') Then FrClosedCat := 'a'
  Else If InList(W, 'mon ma mes ton ta tes son sa ses notre nos votre vos leur leurs') Then FrClosedCat := 'S'
  Else If InList(W, 'ce cet cette ces') Then FrClosedCat := 'T'
  Else If InList(W, 'tout toute tous toutes chaque plusieurs quelques quelque autre autres même certains') Then FrClosedCat := 'Q'
  Else If InList(W, 'qui que quoi où quand comment pourquoi combien quel quelle quels quelles') Then FrClosedCat := 'W'
  Else If InList(W, 'et ou mais donc car lorsque puisque si parce que') Then FrClosedCat := 'C'
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
  FrKnown := (FrClosedCat(W) <> #0) Or (FrNumberEn(W) <> '') Or (FrLexMatch(W, 'nvarpcj') > 0);
End;

{ ---- Formes des noms, adjectifs et verbes ---- }

Function FrFindNoun(Const W: String; Var Plu: Boolean): Integer;
Var
  I: Integer;
Begin
  FrFindNoun := 0;
  For I := 1 To LexCount Do
    If (Lex[I].Pos = 'n') And (Lex[I].Fr <> '') And (Pos(' ', Lex[I].Fr) = 0) Then
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
    If (Lex[I].Pos = 'a') And (Lex[I].Fr <> '') And (Pos(' ', Lex[I].Fr) = 0) Then
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

{ Cherche la forme verbale W parmi les verbes du lexique et quelques verbes speciaux. }
Function FrMatchVerb(Const W: String; Var Inf: String; Var Tns: Char; Var Slot: Integer;
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
    If Test(Part, 'P', 0) Or Test(Part + 's', 'P', 0) Or Test(Part + 'e', 'P', 0) Or
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
    If EndsWith(W, 'ons') Then Slot := 4 Else If EndsWith(W, 'ez') Then Slot := 5 Else Slot := 2;
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
  If (FN > 0) And Cap And (Not FrKnown(FT[FN].W)) And (Not IsDigitsOnly(FT[FN].W)) Then
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
            ((S0[I + 1] = #$AB) Or (S0[I + 1] = #$BB) Or (S0[I + 1] = #$A0)) Then
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
            (((S[I] = '''') Or (S[I] = '-')) And (Word1 <> '') And (I < L) And FrWordByte(S[I + 1]))) Do
      Begin
        Word1 := Word1 + S[I];
        Inc(I);
      End;
      FrSplit(Word1, (Word1 <> '') And (((Word1[1] >= 'A') And (Word1[1] <= 'Z')) Or
              ((Word1[1] = #$C3) And (Length(Word1) > 1) And (Word1[2] >= #$80) And (Word1[2] <= #$9E))));
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

{ Remplace par un seul jeton les locutions du lexique (a cote de, tous les jours...). }
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
          Else If (FT[K - 1].Orig <> '') And (FT[K - 1].Orig[Length(FT[K - 1].Orig)] = '''') And
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
  IsPersonEn := InList(En, 'man woman boy girl brother sister friend child mother father son daughter husband wife parent teacher student doctor nurse driver worker president king queen soldier person');
End;

{ Verbes qui introduisent un infinitif ou prennent "de" : on les repere par leur infinitif francais. }
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

{ Classes : N nom, A adjectif, V verbe, R adverbe, E preposition, C conjonction, J interjection,
  D article, S possessif, T demonstratif, Q quantificateur, U nombre, P pronom sujet,
  O pronom complement, B pronom disjoint, G ne, g negation, W interrogatif, Z nom propre,
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
        If (Prev = 'N') Or (Prev = 'E') Or ((Prev = 'V') And (FT[I - 1].Inf <> '') And
           InList(FT[I - 1].Inf, 'venir revenir sortir parler rêver dépendre')) Then
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
        If (I < FN) And ((FT[I + 1].W = 'est') Or (FT[I + 1].W = 'sont') Or (FT[I + 1].W = 'était') Or
           (FT[I + 1].W = 'sera') Or (FT[I + 1].W = 'étaient')) Then FT[I].Cat := 'P';
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
        If NextIsVerbOnly(I + 1) And Not NextIsNounish(I + 1) Then FT[I].Cat := 'O';
      End;
      Exit;
    End;
    'W':
    Begin
      FT[I].Cat := 'W';
      If W = 'que' Then
        If (Prev = 'V') Or (Prev = 'N') Or (Prev = 'A') Or (Prev = ',') Or (Prev = 'R') Then
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
  If (Not (C.N > 0)) And (Not (C.A > 0)) And (Not C.HasV) And (C.R = 0) And (C.P = 0) And
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
  Else If (Prev = 'R') And C.HasV And (I > 1) And (FT[I - 1].Cat = 'R') And (I > 2) And
          (FT[I - 2].Cat In ['P', 'O', 'G', 'V']) Then
    FT[I].Cat := 'V'
  Else If (C.R > 0) And (C.N = 0) And (C.A = 0) And (Not C.HasV) Then
    FT[I].Cat := 'R'
  Else If (C.P > 0) And (C.N = 0) And (Not C.HasV) Then
  Begin
    FT[I].Cat := 'E';
    If (C.R > 0) And Not (NextIsNounish(I + 1) Or ((I < FN) And FT[I + 1].Cap)) Then FT[I].Cat := 'R';
  End
  Else If (C.C > 0) And (C.N = 0) And (Not C.HasV) Then
    FT[I].Cat := 'C'
  Else If (C.J > 0) And (C.N = 0) And (Not C.HasV) Then
    FT[I].Cat := 'J'
  Else If C.HasV And ((I = 1) Or (Prev In [',', 'W', 'C', 'R'])) And ((C.N = 0) Or (C.Tns <> 'I')) Then
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
  If (W = 'temps') And (FT[I].Cat = 'N') And (I + 2 <= FN) And (FT[I + 1].Cat = 'V') And
     (FT[I + 1].Inf = 'être') And (FT[I + 2].Cat = 'A') And
     InList(FT[I + 2].En, 'nice bad good beautiful terrible cold hot warm cool') Then
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
  { "nous" et "vous" apres un verbe en tete de phrase : complement (imperatif) ou sujet (inversion) }
End;

{ ------------------------------------------------------------------ }
{ ANGLAIS : formes                                                    }
{ ------------------------------------------------------------------ }

Function EnVowelStart(Const W: String): Boolean;
Begin
  EnVowelStart := False;
  If W = '' Then Exit;
  If InList(Copy(W, 1, 3), 'uni use eur one') Or StartsWith(W, 'one') Or StartsWith(W, 'euro') Then Exit;
  If StartsWith(W, 'hour') Or StartsWith(W, 'honest') Or StartsWith(W, 'heir') Then
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
    If (English[I].Base = Base) And ((English[I].Kind = Kind1) Or (English[I].Kind = Kind2)) Then
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
  If (Not IsVowelCh(W[L])) And (Not (W[L] In ['w', 'x', 'y'])) And IsVowelCh(W[L - 1]) And
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
  Else If EndsWith(Base, 's') Or EndsWith(Base, 'x') Or EndsWith(Base, 'z') Or EndsWith(Base, 'ch') Or
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
  Else If EndsWith(Base, 'e') And (Not EndsWith(Base, 'ee')) Then EnIng := Copy(Base, 1, L - 1) + 'ing'
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
  Else If EndsWith(N, 's') Or EndsWith(N, 'x') Or EndsWith(N, 'z') Or EndsWith(N, 'ch') Or
          EndsWith(N, 'sh') Then
    EnPlural := N + 'es'
  Else If EndsWith(N, 'y') And (L > 1) And (Not IsVowelCh(N[L - 1])) Then
    EnPlural := Copy(N, 1, L - 1) + 'ies'
  Else If InList(N, 'potato tomato hero') Then
    EnPlural := N + 'es'
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
    Kind: Char;           { d defini, i indefini, p partitif, s possessif, t demonstratif, n nombre, q quantificateur, 0 aucun }
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
  IsContainerNoun := InList(En, 'bottle glass cup piece kilo liter slice plate bag box group lot');
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

{ Personne grammaticale anglaise : 1 I, 2 you, 3 he/she/it, 4 we, 5 you (pl), 6 they }
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
  If (FT[I].Cat = 'Q') And InList(FT[I].W, 'tout toute tous toutes') And (I < FN) And
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
    Else If (FT[I].Cat = 'R') And (I < FN) And (FT[I + 1].Cat = 'A') And (FT[I].En <> '') Then
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
      Else If (FT[I].Cat = 'R') And (I < FN) And (FT[I + 1].Cat = 'A') And (FT[I].En <> '') And
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
      Else If (FT[I].Cat = 'C') And (FT[I].W = 'et') And (I < FN) And (FT[I + 1].Cat = 'A') And
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
    If InList(FT[NP.DetTok].W, 'les des mes tes ses nos vos leurs ces') Then NP.Plural := True
    Else If InList(FT[NP.DetTok].W, 'le la un une mon ma ton ta son sa notre votre leur ce cet cette du') Then
      NP.Plural := False;
    If (NP.Kind = 'n') And (NP.NumText <> 'one') Then NP.Plural := True;
    If (NP.Kind = 'q') And InList(FT[NP.DetTok].W, 'tous toutes plusieurs quelques certains') Then NP.Plural := True;
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
  If NP.Plural And ((Not IsMassNoun(N)) Or (NP.Kind = 'n')) Then N := EnPlural(N);
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

Function RenderNPFr(Const NP: TFNP; Generic, Neg: Boolean; Const SubjPoss: String): String;
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
        If NP.Plural Or IsMassNoun(FT[NP.Head].En) Then Det := 'any' Else Det := 'a';
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

{ Groupe prepositionnel. I pointe sur la preposition ; renvoie le texte anglais et avance I. }
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
  If (W = 'à') And (J + 1 <= Last) And (FT[J].Cat = 'U') And (FT[J + 1].L > 0) And
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
    Else If InList(Ctx.VerbEn, 'give send show tell say write bring teach ask sell lend offer speak talk prefer') Then
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
    Else If (J <= Last) And (FT[J].Cat = 'N') And (FT[J].L > 0) And IsTransportNoun(Lex[FT[J].L].En) Then
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
    If (NP.Kind = 'd') And (NP.Head > 0) And (Not NP.Plural) And (NP.AdjN = 0) And
       InList(Prep, 'to at in') And InList(FT[NP.Head].En, 'school church bed work prison university') Then
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
  IsFreqAdverb := InList(En, 'always never often sometimes usually already just');
End;

Function TranslateClauseEn(A, B: Integer; IsQ: Boolean; Const PreSubj: String; PrePlu: Boolean): String; Forward;

Function TailEn(First, Last: Integer; Const Ctx: TFCtx; Var Freq: String): String;
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
          If Ctx.BeLike And (NP.Kind = '0') And (Not NP.Plural) And (Not NP.Proper) And
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
        If (I < Last) And (FT[I + 1].Cat = 'A') And (FT[I].En <> '') And Ctx.BeLike Then
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
          If FT[I].En <> '' Then R := Join(R, FT[I].En) Else R := Join(R, FT[I].Orig);
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
        Else If (FT[I].W = 'de') And (I < Last) And (FT[I + 1].Cat In ['N', 'A']) And
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
        Else If InList(FT[I].W, 'à de') And (I + 2 <= Last) And (FT[I + 1].Cat In ['O', 'B', 'P']) And
                (FT[I + 2].Cat = 'V') And (FT[I + 2].Tns = 'I') And (ObjPronEn(FT[I + 1].W) <> '') Then
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
        Else If InList(FT[I].W, 'à de') And (I < Last) And (FT[I + 1].Cat = 'V') And
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
          R := Join(R, Join(InfinitiveEn(I, Gerund), TailEn(I + 1, Last, C2, Freq)));
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
        If FT[I].En <> '' Then R := Join(R, FT[I].En) Else R := Join(R, FT[I].Orig);
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
        If (FT[I].W <> 'qui') And (FT[I].W <> 'que') And (I < Last) And (I > 1) And
           (FT[I - 1].Cat In ['g', 'V']) Then
        Begin
          EmbedFlag := True;
          R := Join(R, TranslateClauseEn(I, Last, False, '', False));
          EmbedFlag := False;
          I := Last + 1;
        End
        { proposition relative : qui + verbe }
        Else If (FT[I].W = 'qui') And (I < Last) And (FT[I + 1].Cat In ['V', 'O', 'G']) And (I > First) Then
        Begin
          If (FT[I - 1].Cat = 'N') And (FT[I - 1].L > 0) And IsPersonEn(Lex[FT[I - 1].L].En) Then
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

{ Cle d'une suite de jetons : mots en minuscules separes par une espace (sans espace apres une elision). }
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
    Mode: Char;       { s simple, c compose, p passif, m modal, g aller + inf, j venir de, b etre, h avoir, t il y a }
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
    If (FT[I].Cat = 'V') And (FT[I].Tns In ['p', 'f', 'c', 'i', 'M']) Then ClauseHasVerbFr := True;
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
  IsMotionEn := InList(V, 'go come return travel arrive leave walk run fly drive');
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
          If IsFreqAdverb(FT[I].En) Then G.Freq := Join(G.Freq, FT[I].En) Else G.Freq := Join(G.Freq, FT[I].En);
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
    Else If (FT[J].Cat = 'O') And (J < Last) And (FT[J + 1].Cat In ['V', 'O']) Then
    Begin
      If FT[J].W <> 'se' Then G.Clit := Join(G.Clit, ObjPronEn(FT[J].W));
      Inc(J);
    End
    Else
      Break;
  End;
  G.Next := J;
  If G.NeSeen And (J <= Last) Then
    If (FT[J].Cat = 'g') And ((FT[J].W = 'rien') Or (FT[J].W = 'personne')) Then G.Neg := True;
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
        If (G.Tns = 'p') And (Pos('already', G.Freq) + Pos('never', G.Freq) + Pos('just', G.Freq) > 0) Then
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
    If (Inf1 = 'pouvoir') Or (Inf1 = 'devoir') Or (Inf1 = 'vouloir') Or (Inf1 = 'savoir') Then
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
  If (Inf1 = 'venir') And (J < Last) And (FT[J].W = 'de') And (FT[J + 1].Cat = 'V') And
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

{ Construit l'auxiliaire (ou le verbe fini) anglais. Renvoie Aux, ou LexBase/LexTns si le verbe est lexical. }
Procedure BuildEn(Const G: TFVG; Sp: Integer; Var Aux, Rest, LexBase, Post: String; Var LexTns: Char);
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
        'f': begin Aux := 'will'; Rest := 'have ' + InflectFirst(Base, 'q'); end;
        'c': begin Aux := 'would'; Rest := 'have ' + InflectFirst(Base, 'q'); end;
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
  IsWeatherAdjEn := InList(En, 'cold hot warm cool nice bad good beautiful terrible');
End;

Function TranslateClauseEn(A, B: Integer; IsQ: Boolean; Const PreSubj: String; PrePlu: Boolean): String;
Var
  I, J, K, Next, EndSubj, V1: Integer;
  Wh, WhW, Res, Tail, SubjText, Aux, Rest, LexBase, Post, Freq, Front, AuxN, Pre2, Pred, Tmp: String;
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
    TranslateClauseEn := FT[A].En + ' ' + TranslateClauseEn(A + 1, B, IsQ, PreSubj, PrePlu);
    Exit;
  End
  Else If (FT[A].Cat = 'J') And (A = B) Then
  Begin
    TranslateClauseEn := FT[A].En;
    Exit;
  End;
  If (FT[A].Cat = 'C') And (A < B) Then
  Begin
    TranslateClauseEn := FT[A].En + ' ' + TranslateClauseEn(A + 1, B, IsQ, PreSubj, PrePlu);
    Exit;
  End;

  { complement de temps ou de lieu place avant la virgule }
  If (PreSubj = '') And (FT[A].Cat In ['R', 'E', 'a', 'd', 'U', 'D', 'S', 'T']) Then
    For K := A + 1 To B - 1 Do
      If (FT[K].Cat = ',') And ClauseHasVerbFr(K + 1, B) And Not ClauseHasVerbFr(A, K - 1) Then
      Begin
        Ctx.VerbEn := '';
        Ctx.BeLike := False;
        Ctx.Neg := False;
        Ctx.Motion := False;
        Ctx.Generic := False;
        Ctx.PersonSubj := False;
        Ctx.SubjPoss := '';
        Tmp := TailEn(A, K - 1, Ctx, Freq);
        TranslateClauseEn := Tmp + ', ' + TranslateClauseEn(K + 1, B, IsQ, PreSubj, PrePlu);
        Exit;
      End;

  { mot interrogatif }
  If (FT[A].Cat = 'W') And (PreSubj = '') Then
  Begin
    WhW := FT[A].W;
    If (WhW = 'que') And (A + 3 <= B) And (FT[A + 1].W = 'est') And (FT[A + 2].W = 'ce') And
       ((FT[A + 3].W = 'que') Or (FT[A + 3].W = 'qui')) Then
    Begin
      Wh := 'what';
      If FT[A + 3].W = 'qui' Then WhSubject := True Else WhObject := True;
      EstCe := FT[A + 3].W = 'que';
      Inc(A, 4);
    End
    Else If (WhW = 'qui') And (A + 3 <= B) And (FT[A + 1].W = 'est') And (FT[A + 2].W = 'ce') And
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
        If (A < B) And (FT[A + 1].Cat = 'V') And Not ((A + 2 <= B) And (FT[A + 2].Cat = 'P')) Then
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
          ((FT[A + 2].W = 'que') Or (FT[A + 2].W = 'qui')) And (PreSubj = '') Then
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
    If (J <= B) And (FT[J].Cat = 'V') And (FT[J].Tns In ['p', 'f', 'c', 'i']) Then
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
        If FT[J + 1].W = 'je' Then Subj.Poss := 'my' Else If InList(FT[J + 1].W, 'tu vous') Then Subj.Poss := 'your'
        Else If FT[J + 1].W = 'il' Then Subj.Poss := 'his' Else If FT[J + 1].W = 'elle' Then Subj.Poss := 'her'
        Else If InList(FT[J + 1].W, 'nous on') Then Subj.Poss := 'our' Else Subj.Poss := 'their';
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
    Else If (J <= B) And (FT[J].Cat = 'V') And (FT[J].Tns = 'M') And (Wh = '') Then
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
    If (G.Mode = 'b') Or (G.MainEn = 'be') Then Imp := 'be' Else Imp := G.MainEn;
    Res := '';
    If FT[G.V1].Slot = 4 Then
      Res := 'let''s'
    Else If G.Neg Then
      Res := 'don''t'
    Else If Please Then
      Res := 'please';
    If (FT[G.V1].Slot = 4) And G.Neg Then Res := 'let''s not';
    Res := Join(Res, Join(Freq, Join(Imp, Join(G.Clit, Tail))));
    If (G.Neg) And (FT[G.V1].Slot <> 4) And Please Then Res := Join('please', Res);
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
  If (PreSubj = '') And (Not Inverted) And (I < B) And (FT[I].Cat = 'W') And (FT[I].W = 'qui') And
     (FT[I + 1].Cat In ['V', 'G', 'O']) Then
  Begin
    J := I + 1;
    While (J <= B) And Not ((FT[J].Cat = 'V') And (FT[J].Tns In ['p', 'f', 'c', 'i', 'M'])) Do
      Inc(J);
    K := J + 1;
    { fin de la relative : le deuxieme verbe fini }
    While (K <= B) And Not ((FT[K].Cat = 'V') And (FT[K].Tns In ['p', 'f', 'c', 'i', 'M']) And
          (Not (FT[K - 1].Cat In ['G', 'O']) Or (FT[K - 1].Cat = 'G'))) Do
      Inc(K);
    { le verbe principal peut etre precede de ne ou de pronoms }
    EndSubj := K - 1;
    While (EndSubj > J) And (FT[EndSubj].Cat In ['G', 'O']) Do
      Dec(EndSubj);
    If K > B Then EndSubj := B;
    If (Subj.PersonSubj) Then Tmp := 'who' Else Tmp := 'that';
    SubjText := Subj.Text + ' ' + Tmp + ' ' + TranslateClauseEn(I + 1, EndSubj, False, '%', Subj.Plural);
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
  If (G.Mode = 'b') And (Next <= B) And (FT[Next].Cat = 'A') And (FT[Next].En = 'born') And
     (G.Tns = 'p') Then G.Tns := 'i';
  If (G.Mode = 's') And (G.MainEn = 'go') And (WhW = 'où') And (G.Tns = 'p') Then G.Rain := True;

  { il impersonnel et expressions }
  If (Subj.Pron = 'il') And (Not Inverted Or True) Then
  Begin
    If (FT[V1].Inf = 'pleuvoir') Or (FT[V1].Inf = 'falloir') Then SubjText := 'it';
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
    If (G.Mode = 'h') And (FT[Next].Cat = 'U') And (Next < B) And InList(FT[Next + 1].W, 'an ans année années') Then
    Begin
      G.Mode := 'b';
      G.MainEn := 'be';
      Pred := FT[Next].En + ' years old';
      Inc(Next, 2);
    End;
    If (G.Mode = 'h') And (FT[Next].W = 'besoin') And (Next < B) And (FT[Next + 1].W = 'de') Then
    Begin
      G.Mode := 's';
      G.MainEn := 'need';
      Inc(Next, 2);
    End;
  End;
  { le temps qu'il fait }
  If (FT[V1].Inf = 'faire') And (Subj.Pron = 'il') And (Next <= B) And (FT[Next].Cat = 'A') And
     IsWeatherAdjEn(FT[Next].En) Then
  Begin
    G.Mode := 'b';
    G.MainEn := 'be';
    SubjText := 'it';
    Sp := 3;
    If (FT[Next].En = 'beautiful') Or (FT[Next].En = 'good') Then FT[Next].En := 'nice';
  End;
  { il est huit heures }
  If (Subj.Pron = 'il') And (G.Mode = 'b') And (Next < B) And (FT[Next].Cat = 'U') And
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
    If (G.Tns = 'p') And (Next <= B) And (FT[Next].Cat In ['D', 'S', 'T', 'N', 'U']) Then
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
  If (Subj.Pron = 'il') And (G.Mode = 'b') And (Next <= B) And (FT[Next].Cat = 'R') And
     (FT[Next].W = 'tard') Then
    SubjText := 'it';
  { choix du verbe anglais selon les complements }
  If FT[V1].Inf = 'faire' Then
  Begin
    G.MainEn := 'do';
    For K := Next To B Do
      If (FT[K].Cat = 'N') And InList(FT[K].En, 'cake meal mistake noise coffee soup salad') Then G.MainEn := 'make';
  End;
  If (FT[V1].Inf = 'prendre') Then
    For K := Next To B Do
      If (FT[K].Cat = 'N') And IsMealNoun(FT[K].En) Then G.MainEn := 'have';
  If (FT[V1].Inf = 'vouloir') And (G.Tns = 'c') And (G.Mode = 's') Then G.MainEn := 'like';
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
  If (FT[V1].Inf = 'appeler') And (G.Mode = 's') And (Not G.Refl) And (G.CliW <> '') Then
    If ((Subj.Pron = 'je') And (G.CliW = 'me')) Or ((Subj.Pron = 'tu') And (G.CliW = 'te')) Or
       ((Subj.Pron = 'nous') And (G.CliW = 'nous')) Or ((Subj.Pron = 'vous') And (G.CliW = 'vous')) Then
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
    Else If (Subj.Pron = 'tu') Or (Subj.Pron = 'vous') Then Res := 'Your name is'
    Else If Subj.Pron = 'il' Then Res := 'His name is'
    Else If Subj.Pron = 'elle' Then Res := 'Her name is'
    Else Res := 'The name is';
    TranslateClauseEn := Join(Res, Tail);
    Exit;
  End;
  If G.Refl And (G.Mode = 's') And (Lex[FT[V1].L].Fr <> '') And (FT[V1].L > 0) Then
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
  If (G.Mode = 'b') And (Subj.Pron = 'il') And (SubjText = 'he') And (Next <= B) And
     (FT[Next].Cat = 'A') And InList(FT[Next].En, 'important possible difficult easy true') Then
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
    Res := Join(Wh, Join(AuxN, Join(SubjText, Join(Freq, Join(Rest, Join(Post, Join(G.Clit, Tail)))))));
    If (G.Neg) And (Aux = 'am') Then Res := Join(Wh, Join('am', Join(SubjText, Join('not', Join(Freq, Join(Rest, Join(G.Clit, Tail)))))));
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
      Res := Join(Wh, Join(SubjText, Join(NegContract(Aux), Join(Freq, Join(Rest, Join(G.Clit, Tail))))));
      If Aux = 'am' Then Res := Join(Wh, Join(SubjText, Join('am not', Join(Freq, Join(Rest, Join(G.Clit, Tail))))));
    End
    Else If Aux <> '' Then
      Res := Join(Wh, Join(SubjText, Join(Aux, Join(Freq, Join(Rest, Join(G.Clit, Tail))))))
    Else
      Res := Join(Wh, Join(SubjText, Join(Freq, Join(FiniteEn(LexBase, LexTns, Sp), Join(Post, Join(G.Clit, Tail))))));
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
        TranslateTokensEn := TranslateTokensEn(A, K - 1, False) + ', ' + TranslateTokensEn(K + 1, B, IsQ);
        Exit;
      End;
  For K := A + 1 To B - 1 Do
    If (FT[K].Cat = 'C') And ClauseHasVerbFr(A, K - 1) And ClauseHasVerbFr(K + 1, B) Then
    Begin
      If (FT[K].W = 'mais') Then
        TranslateTokensEn := TranslateTokensEn(A, K - 1, False) + ', ' + FT[K].En + ' ' +
                             TranslateTokensEn(K + 1, B, IsQ)
      Else
        TranslateTokensEn := TranslateTokensEn(A, K - 1, False) + ' ' + FT[K].En + ' ' +
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

{ Remplace un groupe de mots du lexique par un seul jeton (a lot of, thank you...). }
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
        TranslateTokens := TranslateTokens(A, K - 1, False) + ', ' + TranslateTokens(K + 1, B, IsQ);
        Exit;
      End;
  { conjonction entre deux propositions }
  For K := A + 1 To B - 1 Do
    If (Tk[K].Tag = 'C') And ClauseHasVerb(A, K - 1) And ClauseHasVerb(K + 1, B) Then
    Begin
      TranslateTokens := TranslateTokens(A, K - 1, False) + ' ' + ConjOf(K) + ' ' +
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
    If (Words[I] = 'de') And ((Words[I + 1] = 'le') Or (Words[I + 1] = 'les')) Then
    Begin
      If Words[I + 1] = 'le' Then Words[I] := 'du' Else Words[I] := 'des';
      Words[I + 1] := '';
    End
    Else If (Words[I] = 'à') And ((Words[I + 1] = 'le') Or (Words[I + 1] = 'les')) Then
    Begin
      If Words[I + 1] = 'le' Then Words[I] := 'au' Else Words[I] := 'aux';
      Words[I + 1] := '';
    End;
    Inc(I);
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
        If (Words[I] = 'je') Or (Words[I] = 'me') Or (Words[I] = 'te') Or (Words[I] = 'se') Or
           (Words[I] = 'le') Or (Words[I] = 'la') Or (Words[I] = 'de') Or (Words[I] = 'ne') Or
           (Words[I] = 'que') Or (Words[I] = 'ce') Or (Words[I] = 'si') Then
        Begin
          If StartsVowel(Next) Then
          Begin
            If (Words[I] = 'ce') And Not (StartsWith(Next, 'est') Or StartsWith(Next, 'ét') Or
               StartsWith(Next, 'ont')) Then
              Joined := ''
            Else If (Words[I] = 'si') And Not ((Next = 'il') Or (Next = 'ils')) Then
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
Begin
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
  If (Res = '') And (TN >= 4) And (Tk[1].W = 'my') And (Tk[2].W = 'name') And (Tk[3].W = 'is') Then
  Begin
    If TargetDe Then Res := 'Ich heiße' Else Res := 'Je m''appelle';
    For I := 4 To TN Do
      If Tk[I].Punct Then Res := Res + ',' Else Res := Join(Res, UpperFirst(Tk[I].Orig));
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
  Writeln('ARGOS-TRANSLATE - Cette commande permet de traduire une phrase d''une langue a une autre.');
  Writeln;
  Writeln('Syntaxe :');
  Writeln('  ARGOS-TRANSLATE --from-lang en --to-lang fr "Hello, how are you?"');
  Writeln('  ARGOS-TRANSLATE [options] [texte]');
  Writeln('  ARGOS-TRANSLATE /?');
  Writeln;
  Writeln('Options :');
  Writeln('  -f, --from-lang  Code ISO 639-1 de la langue source (en, fr).');
  Writeln('  -t, --to-lang    Code ISO 639-1 de la langue cible (fr, de, en).');
  Writeln('  -l, --list       Affiche les paires de langues disponibles.');
  Writeln('  -V, --version    Affiche la version.');
  Writeln('  -h, --help, /?   Affiche cette aide.');
  Writeln;
  Writeln('Sans texte, les lignes sont lues sur l''entree standard. Le resultat est ecrit en');
  Writeln('UTF-8 (sous Windows, taper d''abord CHCP 65001 pour afficher les accents).');
  Writeln;
  Writeln('Traducteur a regles anglais -> francais/allemand et francais -> anglais : il traduit les phrases simples ;');
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
  FromLang := '';
  ToLang := '';
  TextArg := '';
  ListOnly := False;
  ArgumentIndex := 1;
  While ArgumentIndex <= ParamCount Do
  Begin
    Argument := ParamStr(ArgumentIndex);
    If (Argument = '/?') Or (Argument = '-?') Or (Argument = '-h') Or (Argument = '--help') Then
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
    Halt(0);
  End;
  If (FromLang = '') Or (ToLang = '') Then
  Begin
    Writeln('Erreur : indiquer --from-lang et --to-lang.');
    Halt(2);
  End;
  If Not (((FromLang = 'en') And ((ToLang = 'fr') Or (ToLang = 'de'))) Or
          ((FromLang = 'fr') And (ToLang = 'en'))) Then
  Begin
    Writeln('Erreur : aucun modele de traduction de ', FromLang, ' vers ', ToLang,
            ' n''est installe (paires disponibles : en -> fr, en -> de, fr -> en).');
    Halt(1);
  End;
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
