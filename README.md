Résolution numérique du problème à deux corps
====

# Introduction

> Ce programme a été réalisé dans le cadre du [TIPE](https://fr.wikipedia.org/wiki/Travail_d%27initiative_personnelle_encadr%C3%A9) blanc en MPSI en 2010-2011.

Il propose de simuler le déplacement de deux corps soumis aux seuls forces d'interaction gravitationnelle.

# Outil nécessaire
L'ensemble du code source est codé en Caml. 
> Caml Light est une implémentation légère du langage de programmation Caml développé par l'INRIA. Elle est stable et facilement portable. Cette version de Caml permet une programmation fonctionnelle et impérative. Caml Light ne permet pas la programmation orientée objet contrairement à OCaml, son successeur.
> Ce langage est utilisé en classe préparatoires scientifiques françaises (MPSI puis MP option informatique) pour initier les élèves à l'algorithmique.
*source : [Wikipedia](https://fr.wikipedia.org/wiki/Caml_Light)*

L'interpréteur de scripts caml est disponible ici : http://caml.inria.fr/caml-light/

# Le programme
## Lancer le programme
1. Préchargez le corps du programme dans l'interpréteur de scripts CamlLight.
Le corps du programme se trouve dans le fichier [SystemeNonPerturbe.ml](SystemeNonPerturbe.ml)
2. Créez deux corps `corpsA` et `corpsB`.
3. Appelez la fonction `solve corpsA corpsB dtt`.

Avec : 
 - corpsA le premier corps
 - corpsB le deuxième corps
 - dtt le pas de simulation pour l’algorithme de résolution des EDO.

```OCaml
(*Deux masses égales en interaction sur une orbite circulaire*)

let A = {masse = 3e18; rayon = 5; coord_x = 100.; coord_y = 200.; vitesse_x = 0.;  vitesse_y = -500. };;
let B = {masse = 3e18; rayon = 5; coord_x = 500.; coord_y = 200.; vitesse_x = 0.; vitesse_y = 500.};;

solve A B 0.0001;;
```

## Quelques exemples

Les conditions intiales pour les exemples suivants sont disponibles dans le fichier [Exemples.md](Exemples.md)

- Deux masses sans vitesse initiale en interaction gravitationnelle
- Cas d'une masse négligeable devant la seconde
- Deux masses égales en interaction
- Deux masses égales en interaction sur une orbite circulaire
- Centre de masse en Mouvement Rectiligne Uniforme
- Simulation Terre-Lune
- Première vitesse cosmique
    - Echec de lancement
    - Mise en orbite correct
- Deuxième vitesse cosmique
    - Echec de libération
    - Le satellite échappe à l'attraction de la Terre

## Interaction Clavier et description des options :

### Ajustement de la fenêtre graphique :

#### Fonctions de déplacement de la fenêtre graphique
- `z` :   Déplacement de la fenêtre graphique le haut.
- `s` :   Déplacement de la fenêtre graphique le bas.
- `q` :   Déplacement de la fenêtre graphique vers la gauche.
- `d` :   Déplacement de la fenêtre graphique vers la droite.

#### Fonctions de zoom
- `+` : Augmentation du zoom
- `-` : Diminution du zoom

#### Affichage des corps et trajectoires
- `a` : Affichage des trajectoires des corps.
- `c` : Relie les coordonnées des trajectoires entre elles. (Mode continu).
- `m` : Affichage de la particule fictive.
- `g` : Affichage du centre de masse.

#### Mode « Anti-blinker »
- `b` : Réduit le clignotement de l’écran par une pause dans la boucle du programme. Cela a pour principale conséquence de réduire grandement la vitesse de simulation. Le rendu visuel s’en trouve grandement amélioré. 

#### Affichage de l’état de la simulation :
- `t` : Affichage du temps de la simulation.
- `i` : Affichage des informations relatives à la simulation. Cela à pour effet de ralentir la simulation. Un clignotement des informations est possible.

#### Options de simulation :
- `>` : Sens normal de simulation. (temps croissant)    
- `<` : Sens inverse de la simulation. (temps décroissant). Les erreurs s’accumulent.          .
- `r` : Accélère la vitesse la simulation par   en agissant sur le pas d’itération . Plus la vitesse de simulation est importante, plus l’erreur commise lors de la résolution numérique est grande.
- `l` : Ralentit la vitesse de la simulation par  en agissant sur le pas d’itération.

#### Accélération de la simulation
- `x` : Supprime les trajectoires mises en mémoire. La vitesse de simulation est alors
augmentée car il n’y a plus de point à tracer.
- `e` : Elague les listes des trajectoires. Les trajectoires perdent une coordonnée sur deux.
La simulation est alors accélérée jusqu’à ce que le s listes se remplissent de nouveau.
- `h` : Active / Désactive « l ’ hypermode ». Toutes les options susceptibles de ralentir la simulation sont désactivées. La simulation est grandement accélérée. Le programme travaille alors en circuit fermé, aucun résultat n’est affiché à l’écran pendant cette période.

#### Pause :
- Espace (ou toute touche ne correspondant à aucune autre fonction) : Met la simulation en Pause et affiche son état.

#### Arrêt de la simulation :
- `Ecp` : Met fin à la simulation et ferme la fenêtre graphique.

# Documentation

[Simulation Caml du problème à deux corps](Two-Body-problem.pdf).
