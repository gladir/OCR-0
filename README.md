# OCR-0
Suite de commande écrit en Turbo Pascal/Free Pascal pour le OCR (reconnaissance optique de caractères) et les reconnaissances connexes.

<h2>Liste des fichiers</h2>

Voici la liste des différents fichiers proposés dans OCR-0 :

<table>
		<tr>
			<th>Nom</th>
			<th>Description</th>	
		</tr>
		<tr>
			<td><b>ARGOS-TRANSLATE.PAS</b></td>
			<td>Cette commande permet de traduire une phrase entre l'anglais, le français, l'allemand, l'espagnol, l'albanais, le portugais, le suédois, le danois, l'italien, le latin, le gallois, l'afrikaans, le maltais, l'islandais, le néerlandais, le norvégien, le polonais, le roumain, le catalan et le japonais (romaji).</td>
		</tr>        
		<tr>
			<td><b>FASTTEXT.PAS</b></td>
			<td>Cette commande permet de lancer l'outil de classification de texte.</td>
		</tr>
		<tr>
			<td><b>LINGUA-CLI.PAS</b></td>
			<td>Cette commande permet de detecter la langue d'un texte.</td>
		</tr>
		<tr>
			<td><b>SUMY.PAS</b></td>
			<td>Cette commande permet de résumer un texte.</td>
		</tr>
	    <tr>
			<td><b>TESSERACT.PAS</b></td>
			<td>Cette commande permet de lancer la reconnaissance de caracteres d'une image.</td>
		</tr>
	    <tr>
			<td><b>TIKA.PAS</b></td>
			<td>Cette commande permet de detecter le langage humain d'un texte.</td>
		</tr>
</table>

<h2>Compilation</h2>
	
Les fichiers Pascal n'ont aucune dépendances, il suffit de télécharger le fichier désiré et de le compiler avec Free Pascal avec la syntaxe de commande  :

<pre><b>fpc</b> <i>LEFICHIER.PAS</i></pre>
	
Sinon, vous pouvez également le compiler avec le Turbo Pascal à l'aide de la syntaxe de commande suivante :	

<pre><b>tpc</b> <i>LEFICHIER.PAS</i></pre>
	
Par exemple, si vous voulez compiler TIKA.PAS, vous devrez tapez la commande suivante :

<pre><b>fpc</b> TIKA.PAS</pre>

<h2>Licence</h2>
<ul>
 <li>Le code source est publié sous la licence <a href="https://github.com/gladir/OCR-0/blob/main/LICENSE">MIT</a>.</li>
 <li>Le paquet original est publié sous la licence <a href="https://github.com/gladir/OCR-0/blob/main/LICENSE">MIT</a>.</li>
</ul>
