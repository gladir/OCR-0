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
  LexData: Array[0..63] Of String = (
    'time=temps:nm/temps;day=jour:nm;night=nuit:nf;morning=matin:nm;evening=soir:nm;afternoon=après-midi:nm/après-midi;week=semaine:nf;month=mois:nm/mois;year=année:nf;hour=heure:nf;minute=minute:nf;second=seconde:nf',
    'man=homme:nm;woman=femme:nf/femmes;child=enfant:nm;boy=garçon:nm;girl=fille:nf;friend=ami:nm;family=famille:nf;mother=mère:nf;father=père:nm;brother=frère:nm;sister=sœur:nf;son=fils:nm/fils;daughter=fille:nf;husband=mari:nm;wife=femme:nf;parent=parent:nm;people=gens:nm/gens;person=personne:nf',
    'house=maison:nf;home=maison:nf;room=chambre:nf;kitchen=cuisine:nf;bathroom=salle de bain:nf;bedroom=chambre:nf;door=porte:nf;window=fenêtre:nf;table=table:nf;chair=chaise:nf;bed=lit:nm;toilet=toilettes:nf/toilettes;floor=sol:nm;wall=mur:nm;roof=toit:nm',
    'car=voiture:nf;bus=bus:nm/bus;train=train:nm;plane=avion:nm;bike=vélo:nm;boat=bateau:nm/bateaux;street=rue:nf;road=route:nf;city=ville:nf;town=ville:nf;village=village:nm;country=pays:nm/pays;world=monde:nm;school=école:nf;university=université:nf;class=classe:nf;lesson=leçon:nf',
    'work=travail:nm/travaux;job=emploi:nm;office=bureau:nm/bureaux;company=entreprise:nf;shop=magasin:nm;store=magasin:nm;market=marché:nm;restaurant=restaurant:nm;hotel=hôtel:nm;hospital=hôpital:nm/hôpitaux;library=bibliothèque:nf;bank=banque:nf;airport=aéroport:nm;station=gare:nf;museum=musée:nm;church=église:nf;theater=théâtre:nm;cinema=cinéma:nm',
    'beach=plage:nf;sea=mer:nf;ocean=océan:nm;river=rivière:nf;lake=lac:nm;mountain=montagne:nf;forest=forêt:nf;garden=jardin:nm;park=parc:nm;tree=arbre:nm;flower=fleur:nf;grass=herbe:nf;island=île:nf;field=champ:nm;bridge=pont:nm;sky=ciel:nm/cieux;star=étoile:nf;moon=lune:nf;sun=soleil:nm',
    'water=eau:nf/eaux;food=nourriture:nf;bread=pain:nm;cheese=fromage:nm;meat=viande:nf;fish=poisson:nm;apple=pomme:nf;orange=orange:nf;banana=banane:nf;egg=œuf:nm;rice=riz:nm/riz;salad=salade:nf;soup=soupe:nf;cake=gâteau:nm/gâteaux;chocolate=chocolat:nm;sugar=sucre:nm;salt=sel:nm;vegetable=légume:nm;fruit=fruit:nm',
    'coffee=café:nm;tea=thé:nm;milk=lait:nm;wine=vin:nm;beer=bière:nf;juice=jus:nm/jus;drink=boisson:nf;breakfast=petit-déjeuner:nm;lunch=déjeuner:nm;dinner=dîner:nm;meal=repas:nm/repas;menu=menu:nm;bill=addition:nf;plate=assiette:nf;glass=verre:nm;cup=tasse:nf;bottle=bouteille:nf;knife=couteau:nm/couteaux;fork=fourchette:nf;spoon=cuillère:nf',
    'money=argent:nm;price=prix:nm/prix;card=carte:nf;book=livre:nm;letter=lettre:nf;newspaper=journal:nm/journaux;magazine=magazine:nm;page=page:nf;phone=téléphone:nm;telephone=téléphone:nm;computer=ordinateur:nm;television=télévision:nf;radio=radio:nf;camera=appareil photo:nm;internet=internet:nm;email=courriel:nm;message=message:nm;website=site web:nm/sites web',
    'name=nom:nm;language=langue:nf;word=mot:nm;sentence=phrase:nf;question=question:nf;answer=réponse:nf;problem=problème:nm;idea=idée:nf;story=histoire:nf;history=histoire:nf;news=nouvelles:nf/nouvelles;information=information:nf;thing=chose:nf;place=endroit:nm;way=chemin:nm;reason=raison:nf;example=exemple:nm;number=numéro:nm;part=partie:nf;end=fin:nf;beginning=début:nm;side=côté:nm;top=haut:nm;bottom=bas:nm',
    'music=musique:nf;song=chanson:nf;movie=film:nm;film=film:nm;game=jeu:nm/jeux;picture=image:nf;photo=photo:nf;painting=tableau:nm/tableaux;art=art:nm;dance=danse:nf;party=fête:nf;gift=cadeau:nm/cadeaux;birthday=anniversaire:nm;holiday=vacances:nf/vacances;vacation=vacances:nf/vacances;trip=voyage:nm;journey=voyage:nm;ticket=billet:nm;passport=passeport:nm;luggage=bagages:nm/bagages;bag=sac:nm;suitcase=valise:nf;key=clé:nf',
    'dog=chien:nm;cat=chat:nm;bird=oiseau:nm/oiseaux;horse=cheval:nm/chevaux;animal=animal:nm/animaux;cow=vache:nf;pig=cochon:nm;mouse=souris:nf/souris;rabbit=lapin:nm;weather=temps:nm;rain=pluie:nf;snow=neige:nf;wind=vent:nm;cloud=nuage:nm;storm=orage:nm;life=vie:nf;death=mort:nf;love=amour:nm;heart=cœur:nm;peace=paix:nf;war=guerre:nf;freedom=liberté:nf',
    'head=tête:nf;hand=main:nf;foot=pied:nm;arm=bras:nm/bras;leg=jambe:nf;eye=œil:nm/yeux;ear=oreille:nf;nose=nez:nm/nez;mouth=bouche:nf;face=visage:nm;hair=cheveux:nm/cheveux;body=corps:nm/corps;health=santé:nf;doctor=médecin:nm;nurse=infirmière:nf;teacher=professeur:nm;student=étudiant:nm;driver=chauffeur:nm;worker=ouvrier:nm;king=roi:nm;queen=reine:nf;president=président:nm;police=police:nf;soldier=soldat:nm',
    'shirt=chemise:nf;dress=robe:nf;shoe=chaussure:nf;hat=chapeau:nm/chapeaux;coat=manteau:nm/manteaux;pants=pantalon:nm;clothes=vêtements:nm/vêtements;color=couleur:nf;size=taille:nf;shape=forme:nf;line=ligne:nf;point=point:nm;level=niveau:nm/niveaux;group=groupe:nm;team=équipe:nf;sport=sport:nm;ball=ballon:nm;level=niveau:nm;machine=machine:nf;tool=outil:nm;light=lumière:nf;fire=feu:nm/feux;air=air:nm;earth=terre:nf;stone=pierre:nf;gold=or:nm;paper=papier:nm;pen=stylo:nm;pencil=crayon:nm',
    'monday=lundi:nm;tuesday=mardi:nm;wednesday=mercredi:nm;thursday=jeudi:nm;friday=vendredi:nm;saturday=samedi:nm;sunday=dimanche:nm;january=janvier:nm;february=février:nm;march=mars:nm;april=avril:nm;may=mai:nm;june=juin:nm;july=juillet:nm;august=août:nm;september=septembre:nm;october=octobre:nm;november=novembre:nm;december=décembre:nm;spring=printemps:nm;summer=été:nm;autumn=automne:nm;winter=hiver:nm',
    'good=bon:aB/bonne;bad=mauvais:aB;big=grand:aB;large=grand:aB;small=petit:aB;little=petit:aB;long=long:aB/longue;short=court:aA/courte;tall=grand:aB;high=haut:aB;low=bas:aA/basse;new=nouveau:aB/nouvelle/nouveaux;old=vieux:aB/vieille/vieux;young=jeune:aB;beautiful=beau:aB/belle/beaux;pretty=joli:aB;nice=agréable:aA;great=grand:aB;wonderful=merveilleux:aA/merveilleuse;terrible=terrible:aA;delicious=délicieux:aA/délicieuse',
    'happy=heureux:aA/heureuse;sad=triste:aA;tired=fatigué:aA;hungry=affamé:aA!faim;thirsty=assoiffé:aA!soif;afraid=effrayé:aA!peur;sleepy=endormi:aA!sommeil;cold=froid:aA!froid;hot=chaud:aA!chaud;warm=chaud:aA;cool=frais:aA/fraîche;easy=facile:aA;difficult=difficile:aA;hard=difficile:aA;important=important:aA;interesting=intéressant:aA;boring=ennuyeux:aA/ennuyeuse;funny=drôle:aA;strange=étrange:aA;famous=célèbre:aA;popular=populaire:aA',
    'expensive=cher:aA/chère;cheap=bon marché:aA/bon marché/bon marché;free=gratuit:aA/gratuite;open=ouvert:aA/ouverte;closed=fermé:aA;ready=prêt:aA/prête;busy=occupé:aA;sick=malade:aA;ill=malade:aA;rich=riche:aA;poor=pauvre:aA;clean=propre:aA;dirty=sale:aA;fast=rapide:aA;quick=rapide:aA;slow=lent:aA/lente;strong=fort:aA/forte;weak=faible:aA;heavy=lourd:aA/lourde;light=léger:aA/légère;full=plein:aA/pleine;empty=vide:aA;dark=sombre:aA;bright=brillant:aA',
    'red=rouge:aA;blue=bleu:aA/bleue;green=vert:aA/verte;yellow=jaune:aA;black=noir:aA/noire;white=blanc:aA/blanche/blancs;brown=marron:aA/marron/marron;gray=gris:aA/grise;grey=gris:aA/grise;pink=rose:aA;purple=violet:aA/violette;first=premier:aB/première;last=dernier:aB/dernière;next=prochain:aB/prochaine;other=autre:aB;same=même:aB;different=différent:aA/différente;sure=sûr:aA/sûre;true=vrai:aA/vraie;false=faux:aA/fausse;possible=possible:aA;special=spécial:aA/spéciale/spéciaux;proud=fier:aA/fière;kind=gentil:aA/gentille;nervous=nerveux:aA/nerveuse;lost=perdu:aA/perdue;late=en retard:aA/en retard/en retard;lonely=seul:aA/seule;alone=seul:aA/seule;married=marié:aA/mariée;single=célibataire:aA;',
    'very=très:r;too=trop:r;also=aussi:r;always=toujours:r;never=jamais:r;often=souvent:r;sometimes=parfois:r;usually=d''habitude:r;now=maintenant:r;today=aujourd''hui:r;tomorrow=demain:r;yesterday=hier:r;tonight=ce soir:r;here=ici:r;there=là:r;well=bien:r;quickly=rapidement:r;slowly=lentement:r;again=encore:r;soon=bientôt:r;already=déjà:r;still=encore:r;just=juste:r;only=seulement:r;really=vraiment:r;much=beaucoup:r;maybe=peut-être:r;perhaps=peut-être:r;together=ensemble:r;almost=presque:r;then=puis:r;early=tôt:r;away=loin:r;everywhere=partout:r;somewhere=quelque part:r;outside=dehors:r;inside=dedans:r;however=cependant:r;very much=beaucoup:r;a lot=beaucoup:r;a little=un peu:r;a lot of=beaucoup de:r;right now=en ce moment:r;of course=bien sûr:r;at home=à la maison:r;every day=tous les jours:r;all day=toute la journée:r;next week=la semaine prochaine:r;last night=hier soir:r;this morning=ce matin:r;this evening=ce soir:r;this afternoon=cet après-midi:r',
    'in=dans:p;on=sur:p;at=à:p;from=de:p;with=avec:p;without=sans:p;for=pour:p;of=de:p;about=à propos de:p;by=par:p;under=sous:p;over=sur:p;between=entre:p;before=avant:p;after=après:p;during=pendant:p;since=depuis:p;until=jusqu''à:p;near=près de:p;behind=derrière:p;around=autour de:p;through=à travers:p;into=dans:p;against=contre:p;across=à travers:p;toward=vers:p;towards=vers:p;above=au-dessus de:p;below=en dessous de:p;next to=à côté de:p;in front of=devant:p;because of=à cause de:p;instead of=au lieu de:p;out of=hors de:p;up=en haut:r;down=en bas:r;off=loin:r',
    'and=et:c;or=ou:c;but=mais:c;because=parce que:c;if=si:c;when=quand:c;while=pendant que:c;so=donc:c;that=que:c;than=que:c;as=comme:c;although=bien que:c;hello=bonjour:j;hi=salut:j;goodbye=au revoir:j;bye=au revoir:j;thanks=merci:j;thank you=merci:j;please=s''il vous plaît:j;sorry=désolé:j;excuse me=excusez-moi:j;welcome=bienvenue:j;yes=oui:j;no=non:j;okay=d''accord:j;ok=d''accord:j;good morning=bonjour:j;good evening=bonsoir:j;good night=bonne nuit:j;good afternoon=bonjour:j;see you=à bientôt:j;congratulations=félicitations:j;cheers=santé:j;wow=ouah:j;oh=oh:j',
    'love=aimer:ve;like=aimer:ve;hate=détester:ve;want=vouloir:vxa@;need=avoir besoin de:vxa@;go=aller:vxe;come=venir:vxe;see=voir:vxa;look=regarder:ve;watch=regarder:ve;hear=entendre:vr;listen=écouter:ve;speak=parler:ve;talk=parler:ve;say=dire:vxa;tell=dire:vxa;ask=demander:ve;answer=répondre:vr;understand=comprendre:vxa;know=savoir:vxa;think=penser:ve;believe=croire:vxa;feel=sentir:vxa;find=trouver:ve;give=donner:ve;take=prendre:vxa;make=faire:vxa;put=mettre:vxa;get=obtenir:vxa;buy=acheter:vxa;sell=vendre:vr;pay=payer:vxa;eat=manger:ve;drink=boire:vxa',
    'cook=cuisiner:ve;sleep=dormir:vxa;live=habiter:ve;work=travailler:ve;study=étudier:ve;learn=apprendre:vxa@à;teach=enseigner:ve;read=lire:vxa;write=écrire:vxa;play=jouer:ve;sing=chanter:ve;dance=danser:ve;run=courir:vxa;walk=marcher:ve;drive=conduire:vxa;travel=voyager:ve;arrive=arriver:vee;leave=partir:vxe;stay=rester:vee;return=revenir:vxe;begin=commencer:ve@à;start=commencer:ve@à;finish=finir:via;stop=arrêter:ve@de;open=ouvrir:vxa;close=fermer:ve;help=aider:ve;wait=attendre:vr;try=essayer:vxa@de;call=appeler:vxa;meet=rencontrer:ve;visit=visiter:ve;use=utiliser:ve;forget=oublier:ve;choose=choisir:via;decide=décider:ve@de;hope=espérer:ve;enjoy=apprécier:ve;miss=manquer:ve;send=envoyer:vxa;bring=apporter:ve;carry=porter:ve;wear=porter:ve;cost=coûter:ve;change=changer:ve;seem=sembler:ve;become=devenir:vxe;show=montrer:ve;swim=nager:ve;smile=sourire:vxa;laugh=rire:vxa;cry=pleurer:ve;die=mourir:vxe;win=gagner:ve;lose=perdre:vr;break=casser:ve;build=construire:vxa;enter=entrer:vee;wash=laver:ve;clean=nettoyer:vxa;cut=couper:ve;prefer=préférer:vxa;remember=se rappeler:ve;receive=recevoir:vxa;fall=tomber:vee;order=commander:ve;learn=apprendre:vxa@à;rain=pleuvoir:ve;'
  );

  { Verbes francais irreguliers : inf|je|tu|il|nous|vous|ils|participe|futur|imparfait }
  IrregularData: Array[0..44] Of String = (
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
  EnglishData: Array[0..3] Of String = (
    'went=go:p;gone=go:q;ate=eat:p;eaten=eat:q;saw=see:p;seen=see:q;took=take:p;taken=take:q;made=make:b;said=say:b;came=come:p;got=get:b;gave=give:p;given=give:q;knew=know:p;known=know:q;thought=think:b;told=tell:b;found=find:b;wrote=write:p;written=write:q;bought=buy:b;spoke=speak:p;spoken=speak:q;ran=run:p;drank=drink:p;drunk=drink:q',
    'slept=sleep:b;met=meet:b;left=leave:b;felt=feel:b;sat=sit:b;stood=stand:b;lost=lose:b;paid=pay:b;heard=hear:b;brought=bring:b;built=build:b;sold=sell:b;sent=send:b;began=begin:p;begun=begin:q;drove=drive:p;driven=drive:q;read=read:b;put=put:b;cut=cut:b;chose=choose:p;chosen=choose:q;forgot=forget:p;forgotten=forget:q;understood=understand:b;became=become:p;won=win:b;broke=break:p;broken=break:q;fell=fall:p;fallen=fall:q;swam=swim:p;swum=swim:q;wore=wear:p;worn=wear:q;taught=teach:b;sang=sing:p;sung=sing:q;died=die:b',
    'children=child:n;men=man:n;women=woman:n;feet=foot:n;teeth=tooth:n;mice=mouse:n;people=person:n;wives=wife:n;knives=knife:n;lives=life:n;leaves=leaf:n;',
    ''
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
  Line, Part: String;
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
  Line, Item, Part: String;
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

BEGIN
  LoadLexicon;
  LoadIrregular;
  LoadEnglish;
  Writeln('lexique ', LexCount, ' irreguliers ', IrregularCount, ' anglais ', EnglishCount);
END.
