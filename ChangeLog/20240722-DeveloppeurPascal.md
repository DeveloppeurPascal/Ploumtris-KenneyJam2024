# 20240722 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

## Modifications sur FrameTests

* activation du bouton "title" de la fiche principale dans le module de gestion du clavier et gamepad
* activation de la librairie UI dans la fiche de test de la création de l'image du titre avec des tuyaux
* renommage du bouton correspondant à l'export de cette image et ajout d'un boite de dialogue confirmant que ça a été fait
* retrait du référencement des SVG de la fiche principale puisqu'il est fait dans l'unité fournissant un bitmap pour un SVG enregistré
* sécurisation de la sortie des fiches par l'intermédiaire de la librairie UI : sur Mac un "close" de la fiche depuis un événement de la fiche est susceptible de générer une violation d'accès puisque je supprime de morceaux d'interface en cours d'utilisation

## Modifications sur Show Pipes

* retrait du référencement des SVG de la fiche principale puisqu'il est fait dans l'unité fournissant un bitmap pour un SVG enregistré

## Modifications sur ShowSVG

* retrait du référencement des SVG de la fiche principale puisqu'il est fait dans l'unité fournissant un bitmap pour un SVG enregistré
