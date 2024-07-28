# 20240728 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

* mise à jour des dépendances
* regénération de l'unité contenant les fichiers SVG du jeu
* suppression des unités temporaires et du sous dossier "src-temp" remplacées par leur version standard depuis la librairie Delphi Game Engine

## Modifications sur FrameTests

* mise à jour du projet suite à basculement des fonctionnalités GameController et UIElements vers Delphi Game Engine

## Modifications sur Ploumtris

* mise à jour du projet suite à basculement des fonctionnalités GameController et UIElements vers Delphi Game Engine

## Modifications sur ShowPipes

* remplacement de la génération locale des bitmaps recadrés depuis les SVG pour passer par la version standard des librairies

## Modifications sur ShowSVG

* remplacement de la génération locale des bitmaps recadrés depuis les SVG pour passer par la version standard des librairies

## Modifications sur Ploumtris

* remplacement de la génération locale des bitmaps recadrés depuis les SVG pour passer par la version standard des librairies

## Modifications sur FrameTests

* remplacement de la génération locale des bitmaps recadrés depuis les SVG pour passer par la version standard des librairies

## Modifications sur Ploumtris

* correction de l'erreur d'étendue (ERangeError) qui se produisait lorsqu'on laissait la touche "flèche bas" appuyée pendant une partie faisant descendre les pièces bien au delà de la grille. La hauteur de déplacement (vY) exposait et nous entrainait en dehors de la grille du jeu.
* mise à jour des README dans les dossiers d'assets
* ajout de contrôles de débordement de la grille de jeu pour récupérer l'informations sur une cellule et changer sa valeur
* ajout d'une propriété pour prendre en charge l'affichage du score à l'écran plutôt que manipuler directement le TLabel correspondant
* refonte des backgrounds et contours des boites de dialogue : abandon des SVG et de l'idée de gérer le cadre en 9Patch, passage sur des TREctangles standards avec coins arrondis et les bonnes couleurs
* désactivation des HitTest pour les éléments créés par code ou ceux qui l'avaient encore dans les écrans
* recherche d'éléments graphiques pour l'affichage des contrôles à l'écran (pris dans "Input Prompts" de Kenney)
* déplacement du dossier des SVG des puzzle assets et renommage de l'unité qui les contient pour prendre le nom standard
* passage dans tous les programmes pour les adapter à ces changements d'unités et l'ajout des nouvelles images en SVG
* ajout d'un logo GamePad (furtif) en haut à droite de l'écran lorsqu'on en détecte un ou qu'on le perd
* prise en charge du stockage des scores à l'aide de Gamolf.RTL.Scores avec enregistrement lors de l'affichage du "Game Over" (attention, les scores ne sont pas enregistrés en sortie de partie en dehors d'une partie perdue)
* remplacement du bouton "B" du gamepad par "X" pour sortir des écrans et du programme
* ajout d'une barre de statut contextuelles pour connaitre les touches/actions disponibles
* implémentation de cette barre de statut au niveau des ffichages et masquages d'écrans

* publication de la version 1.1 - 20240728 pour Windows et Mac
