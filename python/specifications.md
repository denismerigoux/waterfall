# Specifications

## Passage de slice

La difficulté c'est que les conditions de passage d'une slice à l'autre
ne sont pas forcément défini pas un montant, mais parfois juste par un
pourcentage. Exemple: dans 1 slice on va dire 3 acteurs,

- Acteur 1 20%
- Acteur 2 30%
- Acteur 3 50%

La slice dure jusqu'à ce que l'acteur 3 ait recup 100 000 euros par exemple
Chaque slot n'est pas forcement défini par un montant et chaque changement
de seuil n'est pas le même pour tous les acteurs par exemple

Je pense que pour 1 slot il faut associer 1 pourcentage et 1 cutoff qui est le
max de la case et qui motive le passage d'une tranche à l'autre
Du coup cut off et pourcentage doivent ressortir dans la classe slice.

Parfois le changement de tranche repose sur une
condition qui dépend de combien le distributeur a accumulé mais depuis plusieurs
tranches donc bon

Là on fait le tableau juste ciné, mais en fait il y a un tableau similaire pour la
salle, pour la plateforme etc... et très souvent, les conditions dépendent de valeur
seuils des recettes agrégées sur plusieurs tableaux
