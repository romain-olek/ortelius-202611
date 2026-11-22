# =============================================================================
# PAPN – Plan d'exploitation des données 2024 / 2025
# 00_config.R : paramètres de l'analyse
# -----------------------------------------------------------------------------
# Toutes les règles modifiables sont centralisées ici (chemins, tolérances de
# rapprochement, règles d'exclusion, filtres). Modifier ce fichier plutôt que
# le code des autres scripts.
# =============================================================================

CONFIG <- list(

  # --- Dossiers --------------------------------------------------------------
  dir_data = "data",       # dossier contenant les bases Excel
  dir_out  = "outputs",    # dossier de sortie (créé si absent)

  annees = c(2024, 2025),

  # --- Repérage des fichiers (expression régulière sur le nom du fichier) ----
  # Tolère les espaces, tirets ou underscores : "Base 7 - PAPN - 2025.xlsx"
  # comme "Base_7_-_PAPN_-_2025.xlsx". "Base 1 " ne capte pas "Base 10".
  # Les bases absentes sont signalées dans le journal et dans l'onglet
  # "Non_calculables" ; l'analyse tourne quand même sur le reste.
  motifs = list(
    besc_2025      = "^Base[ _-]*1[ _-]",
    regul_2025     = "^Base[ _-]*3[ _-]",
    ga_2025        = "^Base[ _-]*4[ _-]",
    tbc_2025       = "^Base[ _-]*5[ _-]",
    tbc_2024       = "^Base[ _-]*6[ _-]",
    papn_2025      = "^Base[ _-]*7[ _-]",
    papn_2024      = "^Base[ _-]*8[ _-]",
    navis_2025     = "^Base[ _-]*9[ _-]",
    navis_2024     = "^Base[ _-]*10[ _-]",
    navis_tb_2025  = "^Base[ _-]*11[ _-]",   # NAVIS transbordement (ind. 15)
    navis_tb_2024  = "^Base[ _-]*12[ _-]",   # NAVIS transbordement
    navis_exp_2025 = "^Base[ _-]*13[ _-]",   # manquante à ce jour (ind. 44, 45)
    navis_exp_2024 = "^Base[ _-]*14[ _-]",   # manquante à ce jour
    sydonia_2025   = "^Base[ _-]*15[ _-]",
    sydonia_2024   = "^Base[ _-]*16[ _-]",
    beach          = "^Base[ _-]*17[ _-]",   # SYDONIA bureau BEACH Brazzaville (ind. 36, 37) - non reçue
    sydonia_export = "^Base[ _-]*18[ _-]",   # SYDONIA export, bureaux DD141 et DD147, 2024-2025 (ind. 43, 46)
    dmt            = "^Base[ _-]*19[ _-]"    # SYDONIA transbordement : dépôt des DMT 2024-2025 (ind. 16)
  ),

  # --- Colonnes des bases pas encore reçues (lettres Excel du plan) ----------
  # À vérifier à réception des fichiers : le journal signale toute colonne
  # absente.
  colonnes_navis_tb     = list(tc = "A", navire_mere = "B", call_in = "C", imo_mere = "D",
                               dechargement_mere = "H", chargement_feeder = "I", imo_transbo = "K"),
  colonnes_navis_export = list(tc = "A", arrivee_terminal = "B", embarquement = "C",
                               navire = "D", imo = "E", appareillage = "F"),
  colonnes_beach        = list(tc = "C", apurement_im8 = "M", depot_ex3 = "W", sh11 = NA),
  # Base 18 (reçue le 22/11) : J = TYPE_DECL ("EX 1"), O = DATE_LIQUIDATION,
  # R = DATE_IMPRESSION_BAE, A = NUMERO_CONTENEUR, C = COD_BUREAU (DD141 / DD147),
  # M = NUM_DCL, N = DATE_ENREG (identifient la déclaration avec le bureau).
  colonnes_sydonia_exp  = list(type_decl = "J", liquidation = "O", bae = "R", tc = "A",
                               bureau = "C", num_dcl = "M", date_enreg = "N"),

  # --- Formats de date des cellules saisies en texte (ordre lubridate) -------
  # Formats confirmés par le client :
  #   jour/mois/année  : CCC (bases 1, 2, 3), Global Access (base 4), TBC (5, 6)
  #   année/mois/jour  : PAPN (7, 8), NAVIS transbordement et export (11 à 14),
  #                      bases douane en attente (17, 18)
  #   mois/jour/année  : NAVIS import (9, 10), SYDONIA (15, 16)
  # Les cellules au format date Excel (numéros de série) sont lues directement,
  # quel que soit le format indiqué ici. La PAPN (AAAAMMJJ + HHMM) et Global
  # Access (date et heure séparées) ont leur propre lecture.
  format_date_ccc          = "dmy HMS",   # bases 1, 2, 3
  format_date_tbc          = "dmy HMS",   # bases 5, 6
  format_date_navis        = "mdy HMS",   # bases 9, 10
  format_date_sydonia      = "mdy HMS",   # bases 15, 16 (y compris la quittance, col. T)
  format_date_navis_tb     = "Ymd HMS",   # bases 11, 12
  format_date_navis_export = "Ymd HMS",   # bases 13, 14
  format_date_beach        = "Ymd HMS",   # base 17
  format_date_sydonia_exp  = "mdy HMS",   # base 18 (vérifié : 2e nombre > 12 sur 1 586 dates, 1er jamais)
  # Base 19 : DATE_DMT (col. H) au format mois/jour/année (vérifié sur le
  # fichier reçu : 194 573 dates ont un 2e nombre > 12, aucune un 1er > 12) ;
  # DATE_MANIFESTE (col. G) au format année-mois-jour.
  format_date_dmt          = "mdy HMS",   # base 19, col. H
  format_date_dmt_manifeste = "Ymd",      # base 19, col. G

  # --- Consolidation des escales PAPN (bases 7 et 8) -------------------------
  # Plusieurs numéros PAPN (colonne A) peuvent désigner la même escale physique
  # (changement de poste, retour en rade, escale présente dans les deux bases).
  # Deux numéros du même navire sont fusionnés si le second commence avant la
  # fin de l'escale en cours (dernier appareillage) + cette marge, en heures.
  papn_ecart_escale_h = 24,

  # --- Indicateur 9 bis (attente en rade, PAPN x NAVIS) ---------------------
  # Date NAVIS retenue comme fin de l'attente : "accostage" (col. I, conforme
  # au libellé « arrivée en rade -> accostage ») ou "debut_dech" (col. J,
  # début du déchargement, si la référence à la colonne J est confirmée).
  ind9bis_date_navis = "accostage",

  # --- Règle d'exclusion des valeurs aberrantes ------------------------------
  # Pour chaque indicateur et chaque année :
  #  - délais négatifs exclus (sauf indicateurs où un négatif a un sens métier)
  #  - valeurs au-delà du percentile haut exclues
  #  - pour les indicateurs acceptant les négatifs : exclusion sous le
  #    percentile bas et au-dessus du percentile haut
  percentile_haut = 0.99,
  percentile_bas  = 0.01,

  # --- Tolérances de rapprochement entre bases (en jours) --------------------
  tol_syd_navis   = 45,  # conteneur SYDONIA -> escale NAVIS (écart manifeste / accostage)
  tol_papn_navis  = 3,   # escale PAPN <-> escale NAVIS (écart entre dates d'accostage)
  tol_scan_navis  = 60,  # passage scanner -> escale NAVIS (après le 1er vu à quai)
  tol_ga_syd      = 60,  # image scanner -> déclaration SYDONIA (écart DAU / scanner)
  tol_tbc_navis   = 90,  # entrée TBC -> BAD NAVIS (BAD antérieur à l'entrée)
  tol_ex1_navis   = 120, # ind. 46 : liquidation EX1 au plus 120 j avant l'embarquement NAVIS (et au plus 7 j après)
  tol_dmt_navis   = 30,  # DMT (base 19) -> déchargement NAVIS transbo : écart date du manifeste / déchargement

  # --- Indicateur 30 ---------------------------------------------------------
  seuil_non_dedouane_j = 30,

  # --- Filtres types de navires (colonne D PAPN) -----------------------------
  types_pc     = c("PORTE CONTENEURS"),
  types_feeder = c("P/C FEEDER"),
  types_autres = c("CARGO FRIGORIFIQUE", "GENERAL CARGO", "GRUMIER",
                   "ROULIER (RORO)", "TRANSPORTEUR VEHICULES"),

  # --- Catégories de conteneurs scannés (colonne C de la base 4) -------------
  # Typologie validée par le client : le transbordement prime, puis la taille.
  # Les combinaisons non prévues (H.G seul, "40, 45", ...) vont dans "Autres".
  categories_taille_ga = list(
    transbo = "TRANSBO",
    c20     = "^20(,\\s*H\\.?G)?$",
    c40     = "^40(,\\s*H\\.?G)?$",
    c45     = "^45$"),

  # --- Unités d'observation -------------------------------------------------
  # Indicateurs 5, 7, 22, 23 et 26 : "declaration" (par défaut) ou "conteneur".
  # Dans SYDONIA, tous les conteneurs d'une même déclaration portent exactement
  # les mêmes dates : compter au conteneur ne crée pas d'information et
  # surpondère les déclarations multi-conteneurs (26 % des déclarations).
  unite_douane = "declaration",
  # Indicateurs 20 et 21 : "escale" (par défaut) ou "manifeste".
  # Une escale porte souvent plusieurs manifestes (52 % des escales, jusqu'à 7).
  # Au niveau escale : 1er manifeste enregistré (20), dernier apurement (21).
  unite_manifeste = "escale",

  # --- Filtres TBC des indicateurs 34 et 38 ----------------------------------
  # Colonne P = provenance, colonne Q = destination (bases 5 et 6, lues par
  # l'intitulé de colonne). Valeurs normalisées : PNR (Pointe-Noire),
  # BZV (Brazzaville), KIN (Kinshasa). Mettre NULL pour ne pas filtrer.
  filtre_34 = list(provenance = "PNR", destination = "BZV"),
  filtre_38 = list(provenance = "BZV", destination = "KIN"),

  # --- Tranches de délais pour les distributions -----------------------------
  tranches_heures = c(0, 6, 12, 24, 48, 72, 168, Inf),
  tranches_jours  = c(0, 1, 2, 3, 7, 15, 30, Inf)
)
