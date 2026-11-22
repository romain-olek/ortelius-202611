# =============================================================================
# 06_export.R : restitution Excel + fichiers de détail
# =============================================================================

dir.create(CONFIG$dir_out, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(CONFIG$dir_out, "details"), showWarnings = FALSE)

wb <- createWorkbook()
modifyBaseFont(wb, fontName = "Arial", fontSize = 10)

st_titre  <- createStyle(fontSize = 13, textDecoration = "bold", fontColour = "#E8622A")
st_note   <- createStyle(fontColour = "#6F6264", textDecoration = "italic")
st_entete <- createStyle(textDecoration = "bold", fontColour = "#FFFFFF", fgFill = "#E8622A",
                         halign = "center", valign = "center", wrapText = TRUE,
                         border = "TopBottomLeftRight", borderColour = "#CFC7C7")
st_bord   <- createStyle(border = "TopBottomLeftRight", borderColour = "#CFC7C7",
                         borderStyle = "thin")
st_alt    <- createStyle(fgFill = "#F4F1F0")
st_dec    <- createStyle(numFmt = "#,##0.0")
st_ent    <- createStyle(numFmt = "#,##0")
st_pct    <- createStyle(numFmt = "0.0%")
st_date   <- createStyle(numFmt = "mmm yyyy")

cols_pct <- c("part", "part_non_couverte_formule", "part_non_couverte_rapprochement", "part_obs",
              "part_negatifs", "part_regul", "taux", "evol_mediane", "evol_moyenne")
cols_ent <- c("n", "n_retenu", "n_brut", "n_conteneurs", "n_lignes_source", "n_rapprochees",
              "n_perdues", "n_exclus_negatif", "n_exclus_extreme", "n_total",
              "n_rapproche", "n_besc", "n_besc_regul", "regul", "besc", "n_conteneurs", "n_lignes",
              "annee", "n_retenu_2024", "n_retenu_2025", "n_conteneurs_embarques", "n_conteneurs_ex1")

ecrire <- function(nom, df, titre, note = NULL, largeur_max = 60) {
  if (is.null(df) || nrow(df) == 0) return(invisible())
  addWorksheet(wb, nom, gridLines = FALSE)
  
  writeData(wb, nom, titre, startRow = 1)
  addStyle(wb, nom, st_titre, rows = 1, cols = 1)
  if (!is.null(note)) {
    writeData(wb, nom, note, startRow = 2)
    addStyle(wb, nom, st_note, rows = 2, cols = 1)
  }
  
  writeData(wb, nom, df, startRow = 4, headerStyle = st_entete, withFilter = TRUE)
  lignes <- 5:(nrow(df) + 4)
  cols   <- seq_along(df)
  
  # Bordures fines sur toute la plage de données
  addStyle(wb, nom, st_bord, rows = lignes, cols = cols, gridExpand = TRUE, stack = TRUE)
  
  # Lignes alternées (une ligne sur deux)
  lignes_alt <- lignes[seq_along(lignes) %% 2 == 0]
  if (length(lignes_alt) > 0)
    addStyle(wb, nom, st_alt, rows = lignes_alt, cols = cols, gridExpand = TRUE, stack = TRUE)
  
  # Formats numériques par colonne
  for (j in cols) {
    nm <- names(df)[j]
    st <- if (nm %in% cols_pct) st_pct
    else if (nm %in% cols_ent) st_ent
    else if (inherits(df[[j]], "Date")) st_date
    else if (is.numeric(df[[j]])) st_dec
    else NULL
    if (!is.null(st)) addStyle(wb, nom, st, rows = lignes, cols = j, gridExpand = TRUE, stack = TRUE)
  }
  
  largeurs <- pmin(pmax(nchar(names(df)) + 2,
                        map_dbl(df, ~ max(c(0, nchar(as.character(head(.x, 500)))), na.rm = TRUE))),
                   largeur_max)
  setColWidths(wb, nom, cols = cols, widths = pmax(largeurs, 8))
  setRowHeights(wb, nom, rows = 4, heights = 30)
  freezePane(wb, nom, firstActiveRow = 5)
}

# --- Onglet "Lisez-moi" ---------------------------------------------------------
lisezmoi <- tibble(Section = c(
  "Objet", "Unité d'observation", "Données", "Bases absentes", "Année d'un délai", "Unités",
  "Règle d'exclusion", "Dates sans heure", "Formats de date", "Rapprochements", "Consolidation PAPN",
  "Indicateur 6", "Indicateur 7", "Indicateur 9 bis", "Indicateur 16", "Indicateur 30", "Indicateur 34", "Indicateur 38", "Nouvelles bases", "Catégories de conteneurs",
  "Onglets", "Fichiers de détail"),
  Description = c(
    "Calcul des indicateurs de délais du plan d'exploitation des données PAPN 2024/2025.",
    "Colonne unite_obs : ce que compte l'effectif de chaque indicateur. Indicateurs 5, 7, 22, 23 et 26 : la déclaration (tous les conteneurs d'une déclaration portent les mêmes dates dans SYDONIA ; la colonne n_conteneurs donne le nombre de conteneurs couverts). Indicateurs 20 et 21 : l'escale navire (1er manifeste enregistré pour le 20, dernier apurement pour le 21). Indicateurs 6 et 8 : le connaissement. Tous les autres : le conteneur. Options modifiables : CONFIG$unite_douane et CONFIG$unite_manifeste.",
    paste0("Bases lues : ", paste(names(FICHIERS)[!is.na(unlist(FICHIERS))], collapse = ", ")),
    paste0(paste(names(FICHIERS)[is.na(unlist(FICHIERS))], collapse = ", "), " (voir onglet Non_calculables)"),
    "Une observation est affectée à l'année de sa date de référence (colonne date_ref_def de la synthèse). Les bases 2024 et 2025 sont empilées puis dédoublonnées pour rapprocher les événements à cheval sur deux années.",
    "Délais exprimés en heures ou en jours selon l'indicateur (colonne unite).",
    paste0("Par indicateur et par année : exclusion des délais négatifs (sauf indicateurs 7, 20 et 37, où un négatif a un sens) et des valeurs au-delà du P",
           CONFIG$percentile_haut * 100, ". Pour 7, 20 et 37 : exclusion sous le P", CONFIG$percentile_bas * 100,
           " et au-dessus du P", CONFIG$percentile_haut * 100, ". Les effectifs exclus sont détaillés dans la synthèse."),
    "Quand l'une des deux dates n'a pas d'heure (DAU, apurement du manifeste, dates TBC, date d'analyse scanner, BESC de régul), le délai est calculé en jours calendaires (même jour = 0).",
    "Cellules saisies en texte : jour/mois/année pour le CCC (bases 1 à 3), Global Access (4) et TBC (5, 6) ; année/mois/jour pour la PAPN (7, 8), NAVIS transbordement et export (11 à 14) et les bases douane 17 et 18 ; mois/jour/année pour NAVIS import (9, 10) et SYDONIA (15, 16). Les cellules au format date Excel sont lues directement. Les textes non lus sont signalés dans le journal. Formats modifiables dans 00_config.R.",
    "Voir l'onglet Rapprochements : clés utilisées et taux de correspondance. Les indicateurs croisant deux bases ne portent que sur les observations rapprochées.",
    paste0("Deux étapes. 1) Un numéro PAPN (colonne A) regroupe plusieurs lignes : arrivée en rade et accostage = premiers horodatages, appareillage = dernier. 2) Les numéros d'un même navire qui se chevauchent dans le temps (début avant la fin de l'escale en cours + ", CONFIG$papn_ecart_escale_h, " h) sont fusionnés en une seule escale physique : changement de poste, retour en rade, escale présente dans les deux bases. Une escale n'est ainsi comptée qu'une fois (indicateurs 9, 10, 17, 18, 19 et 20). Contrôle dans l'onglet Anomalies : nombre d'escales PAPN porte-conteneurs et feeders comparé à NAVIS."),
    "Base BESC de régul disponible du 20/02 au 30/06/2025 seulement. Ratio retenu : sur la période commune (validé par le client). La formule initiale du plan est donnée en second.",
    "R - C : date du DAU moins date de visa de la régul. Un délai positif signifie que la régul a été visée avant le DAU (cas conforme à la règle) ; un délai négatif, qu'elle l'a été après. Les deux signes sont conservés (exclusion sous le P1 et au-dessus du P99).",
    paste0("Nouvel indicateur : attente en rade des porte-conteneurs et feeders en croisant les deux bases, arrivée en rade PAPN (J&L) -> ", if (CONFIG$ind9bis_date_navis == "accostage") "accostage NAVIS (col. I)" else "début du déchargement NAVIS (col. J)", ". Ne porte que sur les escales PAPN rapprochées d'une escale NAVIS (onglet Rapprochements) ; une observation par escale PAPN."),
    paste0("Déchargement du conteneur du navire mère (NAVIS transbordement, col. H) -> dépôt du DMT (base 19, col. H). Rapprochement par conteneur + navire mère, la date du manifeste devant tomber à ", CONFIG$tol_dmt_navis, " jours au plus du déchargement ; 2e passe sur le conteneur seul quand le nom du navire diffère. Si un conteneur a plusieurs DMT pour la même escale, le premier est retenu. Croisement par agent maritime (col. I de la base 19, intitulée NAVIRE_FEEDER mais contenant l'agent). Le plan signale qu'environ la moitié des DMT manquerait : voir le taux de rapprochement dans l'onglet Rapprochements."),
    "Conteneurs débarqués sans déclaration au moment du scanner (col. W vide), sans aucune exclusion (validé par le client). La part du transbordement et celle des hors gabarit sont indiquées. Devenir reconstitué par la date de livraison NAVIS.",
    "Plan révisé : un seul indicateur tous régimes (IM8 / BLD fusionnés), pour les mouvements TBC de provenance PNR (col. P) et de destination BZV (col. Q) rapprochés de NAVIS. Croisement SH limité aux conteneurs retrouvés dans SYDONIA (et BEACH quand disponible).",
    "E - D (2025) / F - D (2024), soit date de sortie - date d'entrée, pour les mouvements de provenance BZV (col. P) et de destination KIN (col. Q). Filtres modifiables dans 00_config.R (CONFIG$filtre_34 et CONFIG$filtre_38).",
    "Plan révisé : bases 13 / 14 (NAVIS export), 17 (SYDONIA BEACH) et 18 (SYDONIA export) pour les indicateurs 36, 37, 43, 44, 45 et 46. Le code est prêt et s'exécute dès que les fichiers sont déposés dans data/ ; colonnes et formats de date à vérifier dans 00_config.R.",
    "Catégories validées par le client, à partir de la colonne C de la base 4 : 1 - 20 pieds (20, 20 H.G), 2 - 40 pieds (40, 40 H.G), 3 - 45 pieds, 4 - Transbordement (toute mention Transbo), 5 - Autres. Une colonne signale par ailleurs la mention H.G.",
    "Synthese, Comparaison_2024_2025, Mensuel, Tranches, Croisement_SH, Croisement_produit, Croisements_autres, Ind6_global, Ind6_par_SH, Ind30_non_dedouanes, Ind30_mensuel, Ind46_couverture_EX1 (si bases reçues), GlobalAccess, Rapprochements, Qualite_donnees, Anomalies, Non_calculables, Journal.",
    "Dossier outputs/details : une ligne par observation (délai, statut retenu / exclu, dimensions) pour chaque indicateur."
  ))

#ecrire("Lisez-moi", lisezmoi, "PAPN - Plan d'exploitation des données 2024/2025 : méthode", largeur_max = 150)

ecrire("Synthese", SYNTHESE, "Synthèse des indicateurs par année",
       "Statistiques calculées sur les observations retenues (après exclusion des négatifs et valeurs extrêmes).")
ecrire("Comparaison_2024_2025", COMPARAISON, "Comparaison 2024 / 2025",
       "Évolution relative 2025 vs 2024.")
#ecrire("Mensuel", MENSUEL, "Évolution mensuelle (observations retenues)")
#ecrire("Tranches", TRANCHES, "Distribution des délais par tranche")
ecrire("Croisement_SH", CROISEMENTS_SH, "Croisement des délais avec les catégories de produits (SH)",
       "Modalités à faible effectif (n < 30) à interpréter avec prudence.")
ecrire("Croisement_produit", CROISEMENT_PRODUIT,
       "Croisement des délais par catégorie de produit (code SH11 et libellé, SYDONIA bases 15-16, col. H et I)",
       "Indicateurs 22, 23, 24, 25, 26, 34 et 37. Une ligne par indicateur, année et code SH11, triée par effectif décroissant. Modalités à faible effectif (n < 30) à interpréter avec prudence. Pour 34 et 37, la ligne « Non identifié » regroupe les conteneurs absents de SYDONIA Bureau Port.")
ecrire("Croisements_autres", CROISEMENTS_AUT, "Autres croisements (type de navire, zone de livraison, scanner, transit)")

if (!is.null(IND6)) {
  # ecrire("Ind6_global", IND6$global, "Indicateur 6 : part des BESC de régul",
  #        "La base BESC de régul ne couvre qu'une partie de l'année : le ratio sur la période commune est à privilégier.")
  ecrire("Ind6_par_SH", IND6$sh |> rename(n_besc_regul = regul, n_besc = besc),
         "Indicateur 6 : part des BESC de régul par section du SH (période commune)",
         "SH obtenu en rapprochant le n° de BL avec SYDONIA.")
}
if (!is.null(IND30)) {
  ecrire("Ind30_non_dedouanes", bind_rows(
    IND30$synthese,
    IND30$detail |> transmute(indicateur = ventilation, valeur = n_conteneurs)),
    "Indicateur 30 : conteneurs débarqués en 2025 non dédouanés dans les 30 jours")
  # ecrire("Ind30_mensuel", IND30$mensuel |> mutate(mois = as.Date(mois)),
  #        "Indicateur 30 : répartition par mois de débarquement")
}
if (!is.null(IND46)) {
  ecrire("Ind46_couverture_EX1", IND46, "Indicateur 46 : part des conteneurs embarqués non couverts par un DAU EX1",
         "Formule du plan : 1 - conteneurs SYDONIA EX1 (base 18, col. A) / conteneurs embarqués NAVIS (col. A). Années sans base NAVIS export non calculées.")
}
if (!is.null(GA_TABLES)) {
  ga_out <- bind_rows(
    GA_TABLES$resultat |> transmute(tableau = "Résultat d'analyse", modalite = resultat, n, part),
    GA_TABLES$taille |> transmute(tableau = "Taille des conteneurs scannés", modalite = classe_taille, n, part),
    GA_TABLES$resultat_taille |> transmute(tableau = paste("Résultat par taille :", classe_taille),
                                           modalite = resultat, n, part),
    GA_TABLES$resultat_mois |> transmute(tableau = paste("Résultat par mois d'analyse :", format(mois_analyse, "%m/%Y")),
                                         modalite = resultat, n, part))
  ecrire("GlobalAccess", ga_out, "Global Access : conteneurs suspects / non suspects et tailles")
}
# --- Onglet Rapprochements : méthode, lignes conservées et perdues -----------
methode_liens <- tibble(
  rapprochement = c(
    "Principe général",
    "Normalisation des clés",
    "Rapprochement au plus proche",
    "Lignes perdues",
    "Effet sur les indicateurs",
    "Paramètres"),
  base_source = "", base_cible = "", cle = "", regle = "",
  n_lignes_source = NA_real_, n_rapprochees = NA_real_, n_perdues = NA_real_, taux = NA_real_,
  indicateurs_concernes = "",
  commentaire = c(
    "Les indicateurs qui croisent deux bases ne portent que sur les lignes rapprochées. Les autres indicateurs utilisent une seule base et ne perdent aucune ligne à cette étape.",
    "Les n° de conteneur et de BL sont mis en majuscules et débarrassés des espaces et de la ponctuation. Les noms de navires sont en plus débarrassés du suffixe de voyage.",
    "Un conteneur revient plusieurs fois dans l'année : à chaque fois, on retient l'occurrence dont la date est la plus proche, dans la limite de tolérance indiquée dans la colonne regle.",
    "Une ligne est perdue quand la clé est absente de la base cible, ou quand aucune occurrence ne tombe dans la tolérance. Le détail figure dans les lignes suivantes.",
    "La colonne indicateurs_concernes indique quels indicateurs dépendent de chaque rapprochement. Un taux faible y réduit la taille de l'échantillon, sans biaiser le délai mesuré.",
    paste0("Tolérances en jours : SYDONIA vers NAVIS ", CONFIG$tol_syd_navis,
           ", PAPN vers NAVIS ", CONFIG$tol_papn_navis,
           ", scanner vers NAVIS ", CONFIG$tol_scan_navis,
           ", SYDONIA vers scanner ", CONFIG$tol_ga_syd,
           ", TBC vers NAVIS ", CONFIG$tol_tbc_navis,
           ", NAVIS transbordement vers DMT ", CONFIG$tol_dmt_navis,
           ". Modifiables dans 00_config.R.")))

ecrire("Rapprochements", bind_rows(methode_liens, RAPPROCHEMENTS),
       "Rapprochements entre bases : méthode, lignes conservées et lignes perdues",
       "Les six premières lignes expliquent la méthode ; les suivantes donnent, pour chaque rapprochement, la clé, la règle, le nombre de lignes en entrée, rapprochées et perdues.")
ecrire("Qualite_donnees", QUALITE, "Taux de remplissage des colonnes par base")
ecrire("Anomalies", ANOMALIES, "Contrôles de cohérence")
ecrire("Non_calculables", NON_CALCULABLES, "Indicateurs non calculables avec les données reçues")
#ecrire("Journal", tibble(journal = JOURNAL), "Journal d'exécution", largeur_max = 150)

fichier_xlsx <- file.path(CONFIG$dir_out, paste0("PAPN_resultats_indicateurs_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))
saveWorkbook(wb, fichier_xlsx, overwrite = TRUE)
log_msg("Résultats écrits : ", fichier_xlsx)

# --- Détail des observations (une ligne par délai) -------------------------------
walk(unique(obs_all$id), function(i) {
  d <- obs_all |> filter(id == i) |>
    select(-neg_ok, -mois) |>
    select(where(~ !all(is.na(.x))))
  readr::write_excel_csv2(d, file.path(CONFIG$dir_out, "details", paste0("indicateur_", i, ".csv")))
})
saveRDS(list(obs_all = obs_all, synthese = SYNTHESE, ref = REF, papn_esc = papn_esc,
             navis_esc = navis_esc, rapprochements = RAPPROCHEMENTS),
        file.path(CONFIG$dir_out, "papn_objets_analyse.rds"))
log_msg("Détails écrits dans ", file.path(CONFIG$dir_out, "details"))
