# PAPN – Plan d'exploitation des données 2024 / 2025 (scripts R)

Ces scripts calculent les indicateurs de délais du plan d'exploitation à partir des bases du Port Autonome de Pointe-Noire, de Congo Terminal (NAVIS), des Douanes (SYDONIA, BESC), de Global Access (scanner) et de TBC (Brazzaville).

## Utilisation

1. Copier les bases Excel dans le dossier `data/`, sans renommer les fichiers. Chaque base est repérée par le début de son nom : `Base_1_`, `Base_3_`, etc.
2. Ouvrir R ou RStudio à la racine du projet.
3. Lancer `source("main.R")`, ou `Rscript main.R` en ligne de commande.
4. Les résultats sont écrits dans `outputs/`.

Les paquets manquants sont installés automatiquement : readxl, dplyr, tidyr, lubridate, stringr, stringi, purrr, openxlsx, readr. La version testée est R 4.3. L'exécution complète prend environ 1 à 2 minutes.

**Plan révisé.** Le code suit la version révisée du plan d'exploitation :
- l'indicateur 34 est calculé tous régimes confondus, avec un croisement SH ;
- l'indicateur 35 est supprimé ;
- les indicateurs 36, 37, 43, 44, 45 et 46 sont ajoutés ;
- les indicateurs 39 et 42 ne sont plus calculés.

**Bases reçues en dernier.** Les bases 11 et 12 (NAVIS transbordement) et 13 et 14 (NAVIS export) sont intégrées : indicateurs 15, 44 et 45. Leurs colonnes sont décrites dans `CONFIG$colonnes_navis_tb` et `CONFIG$colonnes_navis_export`.

**Bases encore attendues.** Il suffira de les déposer dans `data/` : la base 17 (SYDONIA bureau BEACH) et la base 18 (SYDONIA export), pour les indicateurs 36, 37, 43 et 46. Le code est écrit et testé sur des fichiers fictifs ; à réception, vérifier les lettres de colonnes et les formats de date dans `00_config.R` (`colonnes_beach`, `colonnes_sydonia_exp`, `format_date_*`). L'indicateur 16 attend la date de dépôt du DMT côté SYDONIA.

## Structure

| Fichier | Rôle |
|---|---|
| `main.R` | Exécute l'ensemble de la chaîne |
| `R/00_config.R` | Paramètres : fichiers, tolérances de rapprochement, règles d'exclusion, filtres navires, tranches |
| `R/01_fonctions.R` | Fonctions : lecture, dates, normalisation des clés, rapprochement, statistiques |
| `R/02_import_preparation.R` | Lecture des bases, typage des dates, normalisation, dédoublonnage |
| `R/03_rapprochements.R` | Tables de correspondance entre bases, avec taux de rapprochement |
| `R/04_indicateurs.R` | Référentiel et calcul des indicateurs, application des règles d'exclusion |
| `R/05_syntheses.R` | Statistiques, comparaison 2024/2025, mensuel, tranches, croisements, contrôles |
| `R/06_export.R` | Classeur Excel de résultats et fichiers de détail |

## Résultats (`outputs/`)

- `PAPN_resultats_indicateurs_AAAAMMJJ.xlsx`, avec les onglets suivants :
  - **Lisez-moi** : rappel de la méthode.
  - **Synthese** : pour chaque indicateur et chaque année, l'effectif, la moyenne, la médiane, les P10, P25, P75 et P90, les exclusions et les alertes.
  - **Comparaison_2024_2025**, **Mensuel**, **Tranches**.
  - **Croisement_SH** (sections et chapitres) et **Croisements_autres** (type de navire, zone de livraison, résultat scanner, taille, régime, destination).
  - **Ind6_global**, **Ind6_par_SH**, **Ind30_non_dedouanes**, **Ind30_mensuel**, **GlobalAccess**.
  - **Rapprochements** : méthode de chaque rapprochement, clé, règle, lignes en entrée, rapprochées et perdues, et indicateurs concernés.
  - **Graphiques**, **Qualite_donnees**, **Anomalies**, **Non_calculables** et **Journal**.
- `PAPN_graphiques_indicateurs.pdf` : un document vectoriel, une page A4 paysage par indicateur, avec l'histogramme des délais (médianes en pointillés), la dispersion par année et l'évolution mensuelle de la médiane. L'axe est tronqué au P95 pour rester lisible, et la part des observations au-delà est indiquée sous chaque graphique.
- `graphiques/indicateur_X.pdf` et `.png` : le même graphique, un fichier par indicateur, à insérer directement dans un rapport. Les PNG sont repris dans l'onglet **Graphiques** du classeur.
- `details/indicateur_X.csv` : une ligne par observation, avec le statut retenu ou exclu. Séparateur « ; », à ouvrir directement dans Excel.
- `papn_objets_analyse.rds` : les principaux objets R, pour des analyses complémentaires.

## Indicateurs calculés

**Avec les données reçues :**
- **2025** : 5 à 15, 17 à 30, 34, 38, 44 et 45.
- **2024** : 9 à 12, 14, 15, 17 à 23, 25 à 29, 34, 38, 44 et 45.

**Dès réception des extractions SYDONIA (bases 17 et 18) :** 36, 37, 43 et 46.

L'onglet **Non_calculables** liste les autres indicateurs et le motif. Il se met à jour automatiquement.

## Règles de méthode (modifiables dans `00_config.R`)

- **Année d'un délai.** Un délai est rattaché à l'année de sa date de référence. Cette date est indiquée pour chaque indicateur (par exemple, date du DAU pour les délais douaniers, accostage pour les délais navires). Les bases 2024 et 2025 sont empilées puis dédoublonnées, ce qui permet de rapprocher correctement les événements à cheval sur deux années.
- **Exclusions.** Pour chaque indicateur et chaque année, on exclut :
  - les délais négatifs, sauf pour les indicateurs 7 et 20 ;
  - les valeurs au-delà du P99.
  
  Pour les indicateurs 7 et 20, un délai négatif a un sens métier : BESC de régul émis après le DAU, ou manifeste enregistré avant l'arrivée. On exclut alors ce qui est sous le P1 et au-dessus du P99.
- **Dates sans heure.** Plusieurs dates n'ont pas d'heure : DAU, apurement du manifeste, TBC, date d'analyse scanner, BESC de régul. Dans ce cas, le délai est calculé en jours calendaires (même jour = 0).
- **Rapprochements.** Ils se font par n° de conteneur, n° de BL ou nom de navire normalisé. Quand un conteneur apparaît sur plusieurs escales, on retient l'occurrence la plus proche dans le temps, avec les tolérances fixées dans la configuration.
- **Escales PAPN.** Une escale regroupe plusieurs lignes. L'arrivée en rade et l'accostage correspondent au premier horodatage, l'appareillage au dernier.

## Unité d'observation

Chaque indicateur compte une unité précise, donnée par la colonne `unite_obs` des onglets de résultats.

| Unité | Indicateurs |
|---|---|
| Déclaration (DAU) | 5, 7, 22, 23, 26 |
| Navire (escale) | 9, 10, 11, 12, 17, 18, 19, 20, 21 |
| Connaissement (BL) | 6, 8 |
| Conteneur | 13, 14, 15, 24, 25, 27, 28, 29, 34, 36, 37, 38, 43, 44, 45, 46 |

**Pourquoi la déclaration plutôt que le conteneur (5, 7, 22, 23, 26).** Dans SYDONIA, tous les conteneurs d'une même déclaration portent exactement les mêmes dates, le même BL et le même SH. C'est vérifié sur 100 % des 4 958 déclarations multi-conteneurs de 2025. Compter au conteneur n'ajoute donc aucune information et donne un poids double ou triple aux grosses déclarations. La colonne `n_conteneurs` indique le nombre de conteneurs couverts. Pour calculer au conteneur, passer `CONFIG$unite_douane` à `"conteneur"`.

Les indicateurs 24 et 25 restent au conteneur : l'analyse d'image et le BAD sont propres à chaque conteneur.

**Escale plutôt que manifeste (20 et 21).** Une escale porte souvent plusieurs manifestes : c'est le cas de 52 % des escales rapprochées, jusqu'à 7 manifestes. Entre le premier et le dernier manifeste d'une même escale, il s'écoule 30 heures en médiane, et jusqu'à 460 heures. Le choix de la règle compte donc : au niveau escale, on retient le premier manifeste enregistré pour l'indicateur 20 et le dernier apurement pour le 21. Pour calculer au manifeste, passer `CONFIG$unite_manifeste` à `"manifeste"`.

**Doublons de la base 3.** Chaque demande de BESC de régul y figure exactement deux fois. Le script supprime les doublons au chargement : 2 114 lignes deviennent 1 057 demandes, soit 1 057 connaissements. L'indicateur 6 est calculé en connaissements distincts : **11,5 %** sur la période commune, et 4,2 % avec la formule initiale du plan.

## Arbitrages du client (retours de septembre 2026)

- **Indicateur 6 :** ratio calculé sur la période commune (20/02 - 30/06/2025) et en connaissements distincts, soit 11,5 %. La formule initiale du plan (4,2 %) est conservée en second.
- **Indicateur 30 :** aucune exclusion. Les 12 311 conteneurs débarqués sans déclaration sont tous retenus, avec la part du transbordement (0,1 %, soit 12 conteneurs) et celle des hors gabarit (410).
- **Indicateur 38 :** calcul E - D sans filtrage, sur tous les mouvements TBC. Le filtre EMBARQUEMENT / KIN reste disponible via `CONFIG$ind38_filtrage_kin`.
- **Indicateurs 35 et 39 :** sans objet, confirmé par le client.
- **Indicateur 42 :** supprimé (dépôt et liquidation le même jour).
- **Escales PAPN :** consolidation validée (premier accostage, dernier appareillage). Le nombre de lignes par escale est suivi dans l'onglet Anomalies, en attendant la réponse de NAVIS.
- **Catégories de conteneurs scannés :** typologie du client, à partir de la colonne C de la base 4.
  1. 20 pieds (20, 20 H.G)
  2. 40 pieds (40, 40 H.G)
  3. 45 pieds
  4. Transbordement (toute mention Transbo)
  5. Autres (H.G seul, combinaisons de tailles)
  
  Une colonne distincte signale la mention H.G.
- **Indicateurs 44 et 45 :** calculés à partir des seules bases NAVIS export (13 et 14), sans SYDONIA.
- **Indicateur 15 :** calculé à partir des seules bases NAVIS transbordement (11 et 12), reçues.
- **Indicateurs 16, 31, 32, 33, 36, 37, 43 et 46 :** en attente des extractions SYDONIA (bases 17 et 18).

## Points à valider avec le client

1. La règle de consolidation des escales PAPN.
2. La règle d'exclusion des valeurs aberrantes (négatifs et P99).
4. L'indicateur 6 : la base BESC de régul ne couvre que la période du 20/02 au 30/06/2025. Le ratio est donc donné de deux façons : selon la formule du plan, et sur la période commune.
5. Les indicateurs 21, 25 et 27 ont une part élevée de délais négatifs. Il faut vérifier la définition des dates avec les producteurs de données.
6. L'indicateur 34, croisement SH : la seule source de SH par conteneur est SYDONIA Bureau Port, qui ne couvre qu'environ 8 % des conteneurs TBC. Ce croisement restera donc très partiel, sauf si la base 17 (BEACH) contient le code SH.
7. L'indicateur 38 : le plan filtre-t-il sur la colonne B (armateur) ? Le calcul actuel retient les mouvements EMBARQUEMENT à destination de KIN.
8. L'indicateur 46 : le plan désigne la colonne N de la base 18, qui sert aussi de date de liquidation pour l'indicateur 43. Quelle colonne contient les conteneurs ?
   - Si la colonne contient des dates, le code le détecte et ne calcule pas l'indicateur.
9. La base 18 est citée pour 2024 comme pour 2025 : s'agit-il d'un seul fichier couvrant les deux années ?
10. L'indicateur 37 : la prise en charge par TBC peut précéder l'apurement de l'IM8 au BEACH. Les délais négatifs sont donc conservés dans ce cas, comme pour les indicateurs 7 et 20.
