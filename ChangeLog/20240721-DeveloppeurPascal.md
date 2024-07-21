# 20240721 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

## 9h00 - 14h

* ajout du MP3 de la musique du jeu dans le projet
* ajout de l'unité de chargement et lancement de la musique de fond (par copie depuis Sporgloo)
* activation de la boucle de musique du jeu en fonction des paramètres du programme
* faire boite de dialogue pour texte ou autres contenus
* prise en charge du débordement par le haut de l'écran et donc de la fin de partie
* ajouter l'écran de fin de partie
* élimination de "ParentWidth" de uPipeParts.pas
* prise en charge d'une connection de la colonne gauche à la colonne de droite
* prise en charge du score


## 14h45 - 18h30

* déplacement de la zone d'affichage du score en bas de la zone de jeu
* faire écran des crédits du jeu
* correction de la prise en charge des touches du clavier qui n'étaient plus traitées en dehors des actions pendant une partie
* correction de la sortie du plein écran sur Mac avec la touche ESC qui n'était plus interceptée par le programme et rendait la main au fonctionnement normale de macOS plutôt que sortir de l'écran en cours ou du programme.
* création d'un cadre pour dessiner le titre à partir d'un assemblage de tuyaux
* ajout du titre sur le background de l'écran (en affichage vertical)
* correction de l'affichage du score dans la fenêtre de GAME OVER sur Mac (pas assez de lignes pour forcer le TText à s'agrandir ???)
* correction de violations d'accès sur Mac en utilisant la souris sur les boutons graphiques (leur suppression se faisait dans l'événement gérant le clic et la suite plantait, un classique, différer les suppressions d'éléments liés aux événements pour éviter ça)

* released version 1.0 - 20240721
