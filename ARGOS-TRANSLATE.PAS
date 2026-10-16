{ @author: Sylvain Maltais (support@gladir.com)
  @created: 2026
  @website(https://www.gladir.com/OCR-0)
  @abstract(Target: Free Pascal 3.2)
  @description: Clone de la commande argos-translate (traduction de phrases) :
                argos-translate --from-lang en --to-lang fr "Hello, how are you?"

  Argos Translate utilise des reseaux de neurones (OpenNMT / CTranslate2) dont les
  modeles pesent plusieurs dizaines de Mo : ils ne sont pas realisables en Pascal.
  Ce clone est un traducteur anglais -> francais A REGLES, sans reseau :
    - lexique d'environ 600 mots (noms avec genre, adjectifs avec accord et place,
      verbes avec groupe et auxiliaire, adverbes, prepositions) ;
    - verbes irreguliers francais (present, futur, conditionnel, imparfait, participe) ;
    - formes irregulieres anglaises, contractions (don't, I'm, ...), locutions ;
    - analyse d'un groupe nominal (determinant, adjectifs, noms) avec accord ;
    - phrases declaratives, negatives, interrogatives (est-ce que, inversion, mots
      interrogatifs) et imperatives, presents, passes (passe compose), futurs ;
    - post-traitement : elisions (j', l', n'), contractions (du, au, des, aux),
      ponctuation francaise et majuscule initiale.
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
  LexData: Array[0..53] Of String = (
    'time=temps:nm/temps;day=jour:nm;night=nuit:nf;morning=matin:nm;evening=soir:nm;afternoon=après-midi:nm/après-midi;week=semaine:nf;month=mois:nm/mois;year=année:nf;hour=heure:nf;minute=minute:nf;second=seconde:nf;man=homme:nm;',
    'woman=femme:nf/femmes;child=enfant:nm;boy=garçon:nm;girl=fille:nf;friend=ami:nm;family=famille:nf;mother=mère:nf;father=père:nm;brother=frère:nm;sister=sœur:nf;son=fils:nm/fils;daughter=fille:nf;husband=mari:nm;wife=femme:nf;',
    'parent=parent:nm;people=gens:nm/gens;person=personne:nf;house=maison:nf;home=maison:nf;room=chambre:nf;kitchen=cuisine:nf;bathroom=salle de bain:nf;bedroom=chambre:nf;door=porte:nf;window=fenêtre:nf;table=table:nf;',
    'chair=chaise:nf;bed=lit:nm;toilet=toilettes:nf/toilettes;floor=sol:nm;wall=mur:nm;roof=toit:nm;car=voiture:nf;bus=bus:nm/bus;train=train:nm;plane=avion:nm;bike=vélo:nm;boat=bateau:nm/bateaux;street=rue:nf;road=route:nf;',
    'city=ville:nf;town=ville:nf;village=village:nm;country=pays:nm/pays;world=monde:nm;school=école:nf;university=université:nf;class=classe:nf;lesson=leçon:nf;work=travail:nm/travaux;job=emploi:nm;office=bureau:nm/bureaux;',
    'company=entreprise:nf;shop=magasin:nm;store=magasin:nm;market=marché:nm;restaurant=restaurant:nm;hotel=hôtel:nm;hospital=hôpital:nm/hôpitaux;library=bibliothèque:nf;bank=banque:nf;airport=aéroport:nm;station=gare:nf;',
    'museum=musée:nm;church=église:nf;theater=théâtre:nm;cinema=cinéma:nm;beach=plage:nf;sea=mer:nf;ocean=océan:nm;river=rivière:nf;lake=lac:nm;mountain=montagne:nf;forest=forêt:nf;garden=jardin:nm;park=parc:nm;tree=arbre:nm;',
    'flower=fleur:nf;grass=herbe:nf;island=île:nf;field=champ:nm;bridge=pont:nm;sky=ciel:nm/cieux;star=étoile:nf;moon=lune:nf;sun=soleil:nm;water=eau:nf/eaux;food=nourriture:nf;bread=pain:nm;cheese=fromage:nm;meat=viande:nf;',
    'fish=poisson:nm;apple=pomme:nf;orange=orange:nf;banana=banane:nf;egg=œuf:nm;rice=riz:nm/riz;salad=salade:nf;soup=soupe:nf;cake=gâteau:nm/gâteaux;chocolate=chocolat:nm;sugar=sucre:nm;salt=sel:nm;vegetable=légume:nm;',
    'fruit=fruit:nm;coffee=café:nm;tea=thé:nm;milk=lait:nm;wine=vin:nm;beer=bière:nf;juice=jus:nm/jus;drink=boisson:nf;breakfast=petit-déjeuner:nm;lunch=déjeuner:nm;dinner=dîner:nm;meal=repas:nm/repas;menu=menu:nm;',
    'bill=addition:nf;plate=assiette:nf;glass=verre:nm;cup=tasse:nf;bottle=bouteille:nf;knife=couteau:nm/couteaux;fork=fourchette:nf;spoon=cuillère:nf;money=argent:nm;price=prix:nm/prix;card=carte:nf;book=livre:nm;letter=lettre:nf;',
    'newspaper=journal:nm/journaux;magazine=magazine:nm;page=page:nf;phone=téléphone:nm;telephone=téléphone:nm;computer=ordinateur:nm;television=télévision:nf;radio=radio:nf;camera=appareil photo:nm;internet=internet:nm;',
    'email=courriel:nm;message=message:nm;website=site web:nm/sites web;name=nom:nm;language=langue:nf;word=mot:nm;sentence=phrase:nf;question=question:nf;answer=réponse:nf;problem=problème:nm;idea=idée:nf;story=histoire:nf;',
    'history=histoire:nf;news=nouvelles:nf/nouvelles;information=information:nf;thing=chose:nf;place=endroit:nm;way=chemin:nm;reason=raison:nf;example=exemple:nm;number=numéro:nm;part=partie:nf;end=fin:nf;beginning=début:nm;',
    'side=côté:nm;top=haut:nm;bottom=bas:nm;music=musique:nf;song=chanson:nf;movie=film:nm;film=film:nm;game=jeu:nm/jeux;picture=image:nf;photo=photo:nf;painting=tableau:nm/tableaux;art=art:nm;dance=danse:nf;party=fête:nf;',
    'gift=cadeau:nm/cadeaux;birthday=anniversaire:nm;holiday=vacances:nf/vacances;vacation=vacances:nf/vacances;trip=voyage:nm;journey=voyage:nm;ticket=billet:nm;passport=passeport:nm;luggage=bagages:nm/bagages;bag=sac:nm;',
    'suitcase=valise:nf;key=clé:nf;dog=chien:nm;cat=chat:nm;bird=oiseau:nm/oiseaux;horse=cheval:nm/chevaux;animal=animal:nm/animaux;cow=vache:nf;pig=cochon:nm;mouse=souris:nf/souris;rabbit=lapin:nm;weather=temps:nm;rain=pluie:nf;',
    'snow=neige:nf;wind=vent:nm;cloud=nuage:nm;storm=orage:nm;life=vie:nf;death=mort:nf;love=amour:nm;heart=cœur:nm;peace=paix:nf;war=guerre:nf;freedom=liberté:nf;head=tête:nf;hand=main:nf;foot=pied:nm;arm=bras:nm/bras;leg=jambe:nf;',
    'eye=œil:nm/yeux;ear=oreille:nf;nose=nez:nm/nez;mouth=bouche:nf;face=visage:nm;hair=cheveux:nm/cheveux;body=corps:nm/corps;health=santé:nf;doctor=médecin:nm;nurse=infirmière:nf;teacher=professeur:nm;student=étudiant:nm;',
    'driver=chauffeur:nm;worker=ouvrier:nm;king=roi:nm;queen=reine:nf;president=président:nm;police=police:nf;soldier=soldat:nm;shirt=chemise:nf;dress=robe:nf;shoe=chaussure:nf;hat=chapeau:nm/chapeaux;coat=manteau:nm/manteaux;',
    'pants=pantalon:nm;clothes=vêtements:nm/vêtements;color=couleur:nf;size=taille:nf;shape=forme:nf;line=ligne:nf;point=point:nm;level=niveau:nm/niveaux;group=groupe:nm;team=équipe:nf;sport=sport:nm;ball=ballon:nm;',
    'machine=machine:nf;tool=outil:nm;light=lumière:nf;fire=feu:nm/feux;air=air:nm;earth=terre:nf;stone=pierre:nf;gold=or:nm;paper=papier:nm;pen=stylo:nm;pencil=crayon:nm;monday=lundi:nm;tuesday=mardi:nm;wednesday=mercredi:nm;',
    'thursday=jeudi:nm;friday=vendredi:nm;saturday=samedi:nm;sunday=dimanche:nm;january=janvier:nm;february=février:nm;march=mars:nm;april=avril:nm;may=mai:nm;june=juin:nm;july=juillet:nm;august=août:nm;september=septembre:nm;',
    'october=octobre:nm;november=novembre:nm;december=décembre:nm;spring=printemps:nm;summer=été:nm;autumn=automne:nm;winter=hiver:nm;good=bon:aB/bonne;bad=mauvais:aB;big=grand:aB;large=grand:aB;small=petit:aB;little=petit:aB;',
    'long=long:aB/longue;short=court:aA/courte;tall=grand:aB;high=haut:aB;low=bas:aA/basse;new=nouveau:aB/nouvelle/nouveaux;old=vieux:aB/vieille/vieux;young=jeune:aB;beautiful=beau:aB/belle/beaux;pretty=joli:aB;nice=agréable:aA;',
    'great=grand:aB;wonderful=merveilleux:aA/merveilleuse;terrible=terrible:aA;delicious=délicieux:aA/délicieuse;happy=heureux:aA/heureuse;sad=triste:aA;tired=fatigué:aA;hungry=affamé:aA!faim;thirsty=assoiffé:aA!soif;',
    'afraid=effrayé:aA!peur;sleepy=endormi:aA!sommeil;cold=froid:aA!froid;hot=chaud:aA!chaud;warm=chaud:aA;cool=frais:aA/fraîche;easy=facile:aA;difficult=difficile:aA;hard=difficile:aA;important=important:aA;',
    'interesting=intéressant:aA;boring=ennuyeux:aA/ennuyeuse;funny=drôle:aA;strange=étrange:aA;famous=célèbre:aA;popular=populaire:aA;expensive=cher:aA/chère;cheap=bon marché:aA/bon marché/bon marché;free=gratuit:aA/gratuite;',
    'open=ouvert:aA/ouverte;closed=fermé:aA;ready=prêt:aA/prête;busy=occupé:aA;sick=malade:aA;ill=malade:aA;rich=riche:aA;poor=pauvre:aA;clean=propre:aA;dirty=sale:aA;fast=rapide:aA;quick=rapide:aA;slow=lent:aA/lente;',
    'strong=fort:aA/forte;weak=faible:aA;heavy=lourd:aA/lourde;light=léger:aA/légère;full=plein:aA/pleine;empty=vide:aA;dark=sombre:aA;bright=brillant:aA;red=rouge:aA;blue=bleu:aA/bleue;green=vert:aA/verte;yellow=jaune:aA;',
    'black=noir:aA/noire;white=blanc:aA/blanche/blancs;brown=marron:aA/marron/marron;gray=gris:aA/grise;grey=gris:aA/grise;pink=rose:aA;purple=violet:aA/violette;first=premier:aB/première;last=dernier:aB/dernière;',
    'next=prochain:aB/prochaine;other=autre:aB;same=même:aB;different=différent:aA/différente;sure=sûr:aA/sûre;true=vrai:aA/vraie;false=faux:aA/fausse;possible=possible:aA;special=spécial:aA/spéciale/spéciaux;',
    'proud=fier:aA/fière;kind=gentil:aA/gentille;nervous=nerveux:aA/nerveuse;lost=perdu:aA/perdue;late=en retard:aA/en retard/en retard;lonely=seul:aA/seule;alone=seul:aA/seule;married=marié:aA/mariée;single=célibataire:aA;',
    'very=très:r;too=trop:r;also=aussi:r;always=toujours:r;never=jamais:r;often=souvent:r;sometimes=parfois:r;usually=d''habitude:r;now=maintenant:r;today=aujourd''hui:r;tomorrow=demain:r;yesterday=hier:r;tonight=ce soir:r;here=ici:r;',
    'there=là:r;well=bien:r;quickly=rapidement:r;slowly=lentement:r;again=encore:r;soon=bientôt:r;already=déjà:r;still=encore:r;just=juste:r;only=seulement:r;really=vraiment:r;much=beaucoup:r;maybe=peut-être:r;',
    'perhaps=peut-être:r;together=ensemble:r;almost=presque:r;then=puis:r;early=tôt:r;away=loin:r;everywhere=partout:r;somewhere=quelque part:r;outside=dehors:r;inside=dedans:r;however=cependant:r;very much=beaucoup:r;',
    'a lot=beaucoup:r;a lot of=beaucoup de:r;right now=en ce moment:r;of course=bien sûr:r;at home=à la maison:r;every day=tous les jours:r;all day=toute la journée:r;next week=la semaine prochaine:r;last night=hier soir:r;',
    'this morning=ce matin:r;this evening=ce soir:r;this afternoon=cet après-midi:r;in=dans:p;on=sur:p;at=à:p;from=de:p;with=avec:p;without=sans:p;for=pour:p;of=de:p;about=à propos de:p;by=par:p;under=sous:p;over=sur:p;',
    'between=entre:p;before=avant:p;after=après:p;during=pendant:p;since=depuis:p;until=jusqu''à:p;near=près de:p;behind=derrière:p;around=autour de:p;through=à travers:p;into=dans:p;against=contre:p;across=à travers:p;',
    'toward=vers:p;towards=vers:p;above=au-dessus de:p;below=en dessous de:p;next to=à côté de:p;in front of=devant:p;because of=à cause de:p;instead of=au lieu de:p;out of=hors de:p;up=en haut:r;down=en bas:r;off=loin:r;and=et:c;',
    'or=ou:c;but=mais:c;because=parce que:c;if=si:c;when=quand:c;while=pendant que:c;so=donc:c;that=que:c;than=que:c;as=comme:c;although=bien que:c;hello=bonjour:j;hi=salut:j;goodbye=au revoir:j;bye=au revoir:j;thanks=merci:j;',
    'thank you=merci:j;please=s''il vous plaît:j;sorry=désolé:j;excuse me=excusez-moi:j;welcome=bienvenue:j;yes=oui:j;no=non:j;okay=d''accord:j;ok=d''accord:j;good morning=bonjour:j;good evening=bonsoir:j;good night=bonne nuit:j;',
    'good afternoon=bonjour:j;congratulations=félicitations:j;cheers=santé:j;wow=ouah:j;oh=oh:j;love=aimer:ve;like=aimer:ve;hate=détester:ve;want=vouloir:vxa@;need=avoir besoin de:vxa@;go=aller:vxe;come=venir:vxe;see=voir:vxa;',
    'look=regarder:ve;watch=regarder:ve;hear=entendre:vr;listen=écouter:ve;speak=parler:ve;talk=parler:ve;say=dire:vxa;tell=dire:vxa;ask=demander:ve;answer=répondre:vr;understand=comprendre:vxa;know=savoir:vxa;think=penser:ve;',
    'believe=croire:vxa;feel=sentir:vxa;find=trouver:ve;give=donner:ve;take=prendre:vxa;make=faire:vxa;put=mettre:vxa;get=obtenir:vxa;buy=acheter:vxa;sell=vendre:vr;pay=payer:vxa;eat=manger:ve;drink=boire:vxa;cook=cuisiner:ve;',
    'sleep=dormir:vxa;live=habiter:ve;work=travailler:ve;study=étudier:ve;learn=apprendre:vxa@à;teach=enseigner:ve;read=lire:vxa;write=écrire:vxa;play=jouer:ve;sing=chanter:ve;dance=danser:ve;run=courir:vxa;walk=marcher:ve;',
    'drive=conduire:vxa;travel=voyager:ve;arrive=arriver:vee;leave=partir:vxe;stay=rester:vee;return=revenir:vxe;begin=commencer:ve@à;start=commencer:ve@à;finish=finir:via;stop=arrêter:ve@de;open=ouvrir:vxa;close=fermer:ve;',
    'help=aider:ve;wait=attendre:vr;try=essayer:vxa@de;call=appeler:vxa;meet=rencontrer:ve;visit=visiter:ve;use=utiliser:ve;forget=oublier:ve;choose=choisir:via;decide=décider:ve@de;hope=espérer:ve;enjoy=apprécier:ve;',
    'miss=manquer:ve;send=envoyer:vxa;bring=apporter:ve;carry=porter:ve;wear=porter:ve;cost=coûter:ve;change=changer:ve;seem=sembler:ve;become=devenir:vxe;show=montrer:ve;swim=nager:ve;smile=sourire:vxa;laugh=rire:vxa;cry=pleurer:ve;',
    'die=mourir:vxe;win=gagner:ve;lose=perdre:vr;break=casser:ve;build=construire:vxa;enter=entrer:vee;wash=laver:ve;clean=nettoyer:vxa;cut=couper:ve;prefer=préférer:vxa;remember=se rappeler:ve;receive=recevoir:vxa;fall=tomber:vee;',
    'order=commander:ve;rain=pleuvoir:ve;english=anglais:nm;french=français:nm;spanish=espagnol:nm;german=allemand:nm;italian=italien:nm;chinese=chinois:nm;japanese=japonais:nm;russian=russe:nm;portuguese=portugais:nm;arabic=arabe:nm;',
    'let=laisser:ve;near here=près d''ici:r;happen=arriver:vee;thank=remercier:ve;a bit=un peu:r;right=juste:aA!raison;wrong=faux:aA!tort;capital=capitale:nf;homework=devoir:nm;present=cadeau:nm/cadeaux;born=né:aA/née;help=aide:nf;',
    'last week=la semaine dernière:r;last year=l''année dernière:r;last month=le mois dernier:r;next month=le mois prochain:r;next year=l''année prochaine:r;this week=cette semaine:r;this year=cette année:r;this month=ce mois-ci:r;',
    'pizza=pizza:nf;sandwich=sandwich:nm;hamburger=hamburger:nm;'
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

Procedure AddLexEntry(Const Item: String);
Var
  Rest, Spec, Part: String;
  E: Integer;
Begin
  If (Item = '') Or (LexCount >= MaxLex) Then Exit;
  Rest := Item;
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
    N, V, A, R, O: Integer;
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
        If (Tk[I + 1].Tag = 'V') And (Tk[I + 1].VForm In ['b', 's', 'y']) Then
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
  I: Integer;
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
        If (I < TN) And (((Tk[I + 1].Tag = 'V') And (Tk[I + 1].VForm In ['q', 'y', 'p'])) Or
           (Tk[I + 1].W = 'been')) Then
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
    MainPast := (Tk[VG.MainTok].VForm In ['p', 'y']) And Not VG.Perfect And Not VG.Prog And
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
  For N := 0 To High(Phrases) Do
  Begin
    Pair := Phrases[N];
    En := CopyUntil(Pair, '|');
    If En = Key Then Res := Pair;
  End;
  If (Res = '') And (TN >= 4) And (Tk[1].W = 'my') And (Tk[2].W = 'name') And (Tk[3].W = 'is') Then
  Begin
    Res := 'Je m''appelle';
    For I := 4 To TN Do
      If Tk[I].Punct Then Res := Res + ',' Else Res := Join(Res, UpperFirst(Tk[I].Orig));
  End;
  If Res = '' Then
  Begin
    TagTokens;
    Res := TranslateTokens(1, TN, Question);
  End;
  Res := FixFrench(Res);
  Res := UpperFirst(Res);
  Case EndMark Of
    '?': Res := Res + ' ?';
    '!': Res := Res + ' !';
    '.': Res := Res + '.';
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
  Writeln('  -f, --from-lang  Code ISO 639-1 de la langue source (en).');
  Writeln('  -t, --to-lang    Code ISO 639-1 de la langue cible (fr).');
  Writeln('  -l, --list       Affiche les paires de langues disponibles.');
  Writeln('  -V, --version    Affiche la version.');
  Writeln('  -h, --help, /?   Affiche cette aide.');
  Writeln;
  Writeln('Sans texte, les lignes sont lues sur l''entree standard. Le resultat est ecrit en');
  Writeln('UTF-8 (sous Windows, taper d''abord CHCP 65001 pour afficher les accents).');
  Writeln;
  Writeln('Traducteur a regles anglais -> francais : il traduit les phrases simples ;');
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
    Halt(0);
  End;
  If (FromLang = '') Or (ToLang = '') Then
  Begin
    Writeln('Erreur : indiquer --from-lang et --to-lang.');
    Halt(2);
  End;
  If (FromLang <> 'en') Or (ToLang <> 'fr') Then
  Begin
    Writeln('Erreur : aucun modele de traduction de ', FromLang, ' vers ', ToLang,
            ' n''est installe (paire disponible : en -> fr).');
    Halt(1);
  End;

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
