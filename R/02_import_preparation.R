# =============================================================================
# 02_import_preparation.R : lecture, typage, normalisation et dédoublonnage
# -----------------------------------------------------------------------------
# Les colonnes sont sélectionnées par leur lettre Excel, comme dans le plan
# d'exploitation, avec contrôle de l'intitulé d'en-tête (alerte dans le journal
# si l'intitulé ne correspond pas).
# Les bases 2024 et 2025 d'une même source sont empilées puis dédoublonnées :
# cela permet de rapprocher correctement les événements à cheval sur deux
# années (ex. navire arrivé fin décembre, DAU déposé en janvier). L'année d'un
# délai est ensuite déterminée par sa date de référence (cf. 04_indicateurs.R).
# =============================================================================

verifier_dossier_data()
FICHIERS <- map(names(CONFIG$motifs), trouver_fichier) |> set_names(names(CONFIG$motifs))
QUALITE <- tibble()

# -----------------------------------------------------------------------------
# PAPN (bases 7 et 8) : escales navires
# -----------------------------------------------------------------------------
map_papn <- list(
  numero      = c("A", "NUMERO"),
  navire      = c("B", "NAVIRE"),
  typenavire  = c("D", "TYPE"),
  quai        = c("E", "QUAI"),
  rade_d      = c("J", "ARRIVEE_RADE"),
  rade_h      = c("L", "ARR_H_RADE"),
  acc_d       = c("M", "DATE_ACCOSTAGE"),
  acc_h       = c("N", "HEURE_ACCOSTAGE"),
  app_d       = c("O", "APPAREILLAGE"),
  app_h       = c("P", "HEURE_APPAREILLAGE")
)

papn_lignes <- map_dfr(c("papn_2024", "papn_2025"), function(cle) {
  df <- lire_base(FICHIERS[[cle]], mot_cle = "NUMERO")
  if (is.null(df)) return(NULL)
  QUALITE <<- bind_rows(QUALITE, remplissage(df, cle))
  selectionner(df, map_papn, cle) |> mutate(source = cle)
})
exiger(papn_lignes, "La base PAPN (bases 7 et 8)")

# Étape 1 : un numéro PAPN = plusieurs lignes (mouvements de quai, doublons).
# Arrivée en rade et accostage = premiers horodatages ; appareillage = dernier.
papn_num <- papn_lignes |>
  mutate(
    numero      = str_trim(numero),
    typenavire  = norm_txt(typenavire),
    arrivee_rade = date_papn(rade_d, rade_h),
    accostage    = date_papn(acc_d, acc_h),
    appareillage = date_papn(app_d, app_h)
  ) |>
  group_by(numero) |>
  summarise(
    navire       = first(navire),
    typenavire   = first(typenavire),
    n_lignes     = n(),
    arrivee_rade = min_na(arrivee_rade),
    accostage    = min_na(accostage),
    appareillage = max_na(appareillage)
  ) |>
  ungroup() |>
  mutate(navire_norm = norm_navire(navire))

# Étape 2 : fusion des numéros qui désignent la même escale physique.
# Même navire (nom normalisé), et début du numéro (arrivée en rade, à défaut
# accostage) antérieur à la fin de l'escale en cours + CONFIG$papn_ecart_escale_h.
# Le numéro conservé est le premier de l'escale (utilisé par les rapprochements).
papn_esc <- papn_num |>
  mutate(debut = coalesce(arrivee_rade, accostage),
         fin   = coalesce(appareillage, accostage, arrivee_rade)) |>
  arrange(navire_norm, debut) |>
  group_by(navire_norm) |>
  mutate(fin_cours = lag(cummax(coalesce(as.numeric(fin), -Inf))),
         nouvelle  = is.na(navire_norm) | is.na(debut) | is.na(fin_cours) |
                     as.numeric(debut) > fin_cours + CONFIG$papn_ecart_escale_h * 3600,
         escale_id = paste(coalesce(navire_norm, numero), cumsum(nouvelle))) |>
  group_by(escale_id) |>
  summarise(
    numeros_papn = paste(unique(numero), collapse = " ; "),
    n_numeros    = n_distinct(numero),
    numero       = first(numero),
    navire       = first(navire),
    navire_norm  = first(navire_norm),
    typenavire   = first(typenavire),
    n_lignes     = sum(n_lignes),
    arrivee_rade = min_na(arrivee_rade),
    accostage    = min_na(accostage),
    appareillage = max_na(appareillage)
  ) |>
  ungroup() |>
  select(-escale_id) |>
  mutate(
    categorie = case_when(
      typenavire %in% CONFIG$types_pc     ~ "Porte-conteneurs",
      typenavire %in% CONFIG$types_feeder ~ "Feeder",
      typenavire %in% CONFIG$types_autres ~ "Autres navires (plan)",
      TRUE                                ~ "Hors périmètre"
    )
  )
log_msg("PAPN : ", nrow(papn_lignes), " lignes -> ", nrow(papn_num), " numéros -> ",
        nrow(papn_esc), " escales physiques (", sum(papn_esc$n_numeros > 1),
        " escales regroupant plusieurs numéros)")

# Diagnostic des lignes multiples sur les navires du périmètre (question posée
# à NAVIS) : une escale peut occuper plusieurs postes à quai, avec en principe
# les mêmes dates d'accostage et d'appareillage.
papn_coherence <- papn_lignes |>
  mutate(typenavire = norm_txt(typenavire)) |>
  filter(typenavire %in% c(CONFIG$types_pc, CONFIG$types_feeder, CONFIG$types_autres)) |>
  group_by(numero) |>
  summarise(n_lignes = n(), n_quais = n_distinct(quai),
            n_accostages = n_distinct(paste(acc_d, acc_h)),
            n_appareillages = n_distinct(paste(app_d, app_h))) |>
  ungroup()
log_msg("PAPN périmètre : ", nrow(papn_coherence), " escales, dont ",
        sum(papn_coherence$n_lignes > 1), " multi-lignes ; dates d'accostage divergentes : ",
        sum(papn_coherence$n_accostages > 1))

# -----------------------------------------------------------------------------
# NAVIS (bases 9 et 10) : conteneurs import
# -----------------------------------------------------------------------------
map_navis <- list(
  tc                 = c("A", "CONTENEUR"),
  navire             = c("C", "NAVIRE"),
  imo                = c("E", "IMO"),
  eta                = c("G", "ETA"),
  etd                = c("H", "ETD"),
  accostage          = c("I", "ACCOSTAGE"),
  debut_dech         = c("J", "DEBUT"),
  premier_vu         = c("K", "PREMIER"),
  dernier_vu         = c("L", "DERNIER"),
  bad                = c("M", "BAD"),
  chargement_remorque = c("N", "REMORQUE"),
  arrivee_zl         = c("O", "ZL"),
  livraison          = c("P", "LIVRAISON"),
  type_livraison     = c("Q", "TYPE LIVRAISON"),
  retour_vide        = c("U", "RETOUR")
)

navis <- map_dfr(c("navis_2024", "navis_2025"), function(cle) {
  df <- lire_base(FICHIERS[[cle]], mot_cle = "NUMERO du conteneur")
  if (is.null(df)) return(NULL)
  QUALITE <<- bind_rows(QUALITE, remplissage(df, cle))
  selectionner(df, map_navis, cle) |> mutate(source = cle)
}) |>
  exiger("La base NAVIS import (bases 9 et 10)") |>
  mutate(
    tc = norm_cle(tc),
    navire_norm = norm_navire(navire),
    imo = str_trim(imo),
    across(c(eta, etd, accostage, debut_dech, premier_vu, dernier_vu, bad,
             chargement_remorque, arrivee_zl, livraison, retour_vide),
           ~ en_date(.x, CONFIG$format_date_navis)),
    type_livraison = norm_txt(type_livraison)
  ) |>
  # dédoublonnage (conteneur x escale) entre les deux extractions
  arrange(tc, accostage, desc(!is.na(livraison))) |>
  distinct(tc, accostage, .keep_all = TRUE) |>
  mutate(id_nav = row_number(),
         escale_navis = paste(coalesce(imo, navire_norm), format(accostage, "%Y%m%d%H%M")))
log_msg("NAVIS : ", nrow(navis), " conteneurs x escale ; ", n_distinct(navis$escale_navis), " escales")

# Escales NAVIS (niveau navire) : premier / dernier vu à quai, début déchargement
navis_esc <- navis |>
  group_by(escale_navis) |>
  summarise(
    navire      = first(navire),
    navire_norm = first(navire_norm),
    imo         = first(imo),
    accostage   = first(accostage),
    debut_dech  = min_na(debut_dech),
    premier_vu  = min_na(premier_vu),
    dernier_vu  = max_na(dernier_vu),
    n_tc        = n()
  ) |>
  ungroup()

# Contrôle de la consolidation PAPN : les escales porte-conteneurs et feeders
# ne devraient pas dépasser celles de NAVIS (environ 350 à 400 par an)
for (a in CONFIG$annees) {
  log_msg("Escales ", a, " : PAPN porte-conteneurs + feeders = ",
          sum(papn_esc$categorie %in% c("Porte-conteneurs", "Feeder") & year(papn_esc$accostage) == a, na.rm = TRUE),
          " ; NAVIS import = ", sum(year(navis_esc$accostage) == a, na.rm = TRUE))
}

# -----------------------------------------------------------------------------
# SYDONIA (bases 15 et 16) : déclarations import Bureau Port
# -----------------------------------------------------------------------------
map_syd <- list(
  instanceid     = c("A", "INSTANCEID"),
  voyage         = c("B", "VOYAGE"),
  tc             = c("C", "CONTENEUR"),
  navire         = c("D", "NAVIRE"),
  bl             = c("G", "BL"),
  sh11           = c("H", "SH"),
  description    = c("I", "DESCRIPTION"),
  num_manifest   = c("J", "MANIFEST"),
  enreg_manifest = c("L", "ENREG_MANIFEST"),
  apur_manifest  = c("M", "APUREMENT"),
  type_decl      = c("N", "TYPE_DECL"),
  dest_im8       = c("P", "DESTINATION"),
  num_dcl        = c("Q", "NUM_DCL"),
  dau            = c("R", "DATE_ENREG"),
  liquidation    = c("S", "LIQUIDATION"),
  quittance      = c("T", "QUITANCE"),
  selectivite    = c("U", "SELECTIVITE"),
  bae            = c("V", "BAE"),
  bas            = c("W", "SORTIE")
)

syd <- map_dfr(c("sydonia_2024", "sydonia_2025"), function(cle) {
  df <- lire_base(FICHIERS[[cle]])
  if (is.null(df)) return(NULL)
  QUALITE <<- bind_rows(QUALITE, remplissage(df, cle))
  selectionner(df, map_syd, cle) |> mutate(source = cle)
}) |>
  exiger("La base SYDONIA (bases 15 et 16)") |>
  mutate(
    tc        = norm_cle(tc),
    bl_norm   = norm_cle(bl),
    type_decl = str_replace_all(str_to_upper(type_decl), "\\s", ""),
    sh2       = str_sub(str_pad(str_extract(sh11, "\\d+"), 11, pad = "0"), 1, 2),
    section_sh = sh_section(sh2),
    across(c(enreg_manifest, apur_manifest, dau, liquidation, selectivite, bae, bas),
           ~ en_date(.x, CONFIG$format_date_sydonia)),
    # La quittance est un texte du type "R-60 du 01/02/2025 11:17:43"
    paiement = en_date(str_extract(quittance, "\\d{1,2}/\\d{1,2}/\\d{4}\\s+\\d{1,2}:\\d{2}(:\\d{2})?"),
                       CONFIG$format_date_sydonia)
  ) |>
  distinct(instanceid, tc, bl_norm, sh11, .keep_all = TRUE) |>
  mutate(id_syd = row_number())
log_msg("SYDONIA : ", nrow(syd), " lignes ; ", n_distinct(syd$instanceid), " déclarations ; types : ",
        paste(names(table(syd$type_decl)), table(syd$type_decl), collapse = ", "))

# Niveau déclaration (un DAU = un INSTANCEID)
syd_dcl <- syd |>
  group_by(instanceid) |>
  summarise(
    type_decl = first(type_decl), num_dcl = first(num_dcl),
    bl_norm = first(bl_norm), num_manifest = first(num_manifest),
    sh2 = first(sh2), section_sh = first(section_sh),
    n_tc = n_distinct(tc),
    across(c(dau, liquidation, paiement, selectivite, bae, bas), ~ first(.x))
  ) |>
  ungroup()

# -----------------------------------------------------------------------------
# BESC (base 1) et BESC de régul (base 3)
# -----------------------------------------------------------------------------
besc <- NULL
if (!is.na(FICHIERS$besc_2025)) {
  df <- lire_base(FICHIERS$besc_2025, colonnes = "A:H")
  QUALITE <- bind_rows(QUALITE, remplissage(df, "besc_2025"))
  besc <- selectionner(df, list(
    statut     = c("A", "STATUS"),
    bl         = c("B", "BL"),
    num_besc   = c("C", "BESC"),
    validation = c("D", "VALIDATION"),
    cargo_type = c("E", "CARGO"),
    navire     = c("F", "NAVIRE"),
    nb_tc      = c("G", "CONTENEUR"),
    evp        = c("H", "EVP")), "besc_2025") |>
    filter(!is.na(statut) | !is.na(num_besc)) |>
    mutate(bl_norm = norm_cle(bl), validation = en_date(validation, CONFIG$format_date_ccc))
  log_msg("BESC : ", nrow(besc), " lignes")
}

regul <- NULL
if (!is.na(FICHIERS$regul_2025)) {
  df <- lire_base(FICHIERS$regul_2025)
  QUALITE <- bind_rows(QUALITE, remplissage(df, "regul_2025"))
  n_av <- nrow(df)
  df <- distinct(df)   # la base 3 contient chaque demande en double
  if (nrow(df) < n_av) log_msg("BESC de régul : ", n_av - nrow(df),
                               " lignes en double supprimées (", n_av, " -> ", nrow(df), ")")
  regul <- selectionner(df, list(
    num_besc = c("A", "BESC"),
    ajoute   = c("B", "AJOUTE"),
    vise     = c("C", "VISE"),
    bl       = c("D", "BL"),
    etat     = c("E", "ETAT")), "regul_2025") |>
    mutate(bl_norm = norm_cle(bl),
           ajoute = en_date(ajoute, CONFIG$format_date_ccc),
           vise = en_date(vise, CONFIG$format_date_ccc))
  log_msg("BESC de régul : ", nrow(regul), " lignes, du ",
          format(min(regul$ajoute, na.rm = TRUE), "%d/%m/%Y"), " au ",
          format(max(regul$ajoute, na.rm = TRUE), "%d/%m/%Y"))
}

# -----------------------------------------------------------------------------
# Global Access – scanner (base 4)
# -----------------------------------------------------------------------------
ga <- NULL
if (!is.na(FICHIERS$ga_2025)) {
  df <- lire_base(FICHIERS$ga_2025)
  QUALITE <- bind_rows(QUALITE, remplissage(df, "ga_2025"))
  ga <- selectionner(df, list(
    tc            = c("B", "CONTENEUR"),
    taille_track  = c("C", "TAILLE"),
    date_scan     = c("D", "DATE_PASSAGE"),
    heure_scan    = c("E", "HEURE_PASSAGE"),
    present_deb   = c("G", "DEBARQUEMENT"),
    present_ana   = c("H", "ANALYSE"),
    code_escale   = c("K", "ESCALE"),
    navire        = c("L", "NAVIRE"),
    voyage        = c("M", "VOYAGE"),
    date_accostage = c("O", "ACCOSTAGE"),
    date_deb      = c("Q", "DEBARQUEMENT"),
    cycle         = c("R", "CYCLE"),
    type_iso      = c("T", "ISO"),
    declaration   = c("W", "DECLARATION"),
    taille        = c("X", "TAILLE"),
    resultat      = c("Y", "RESULTAT"),
    date_analyse  = c("AB", "DATE_ANALYSE")), "ga_2025") |>
    mutate(
      tc = norm_cle(tc),
      scan = en_date(date_scan, "dmy") + coalesce(en_secondes(heure_scan), 0),
      date_accostage = en_date(date_accostage, "dmy HMS"),
      date_deb = en_date(date_deb, "dmy HMS"),
      date_analyse = en_date(date_analyse, "dmy"),
      resultat = case_when(str_to_upper(resultat) == "S" ~ "Suspect",
                           str_to_upper(resultat) == "NS" ~ "Non suspect",
                           TRUE ~ NA_character_),
      # Typologie validée par le client (colonne C, taille_track)
      taille_track_n = str_squish(str_to_upper(sans_accents(taille_track))),
      classe_taille = case_when(
        str_detect(taille_track_n, CONFIG$categories_taille_ga$transbo) ~ "4 - Transbordement",
        str_detect(taille_track_n, CONFIG$categories_taille_ga$c20)     ~ "1 - 20 pieds",
        str_detect(taille_track_n, CONFIG$categories_taille_ga$c40)     ~ "2 - 40 pieds",
        str_detect(taille_track_n, CONFIG$categories_taille_ga$c45)     ~ "3 - 45 pieds",
        TRUE ~ "5 - Autres"),
      hors_gabarit = str_detect(coalesce(taille_track_n, ""), "H\\.?G"),
      declaration = na_if(str_trim(declaration), "")
    ) |>
    mutate(id_ga = row_number())
  log_msg("Global Access : ", nrow(ga), " passages scanner")
}

# -----------------------------------------------------------------------------
# TBC – parc à conteneurs de Brazzaville (bases 5 et 6)
# Les deux fichiers n'ont pas le même ordre de colonnes : sélection par nom.
# -----------------------------------------------------------------------------
lire_tbc <- function(cle) {
  df <- lire_base(FICHIERS[[cle]])
  if (is.null(df)) return(NULL)
  QUALITE <<- bind_rows(QUALITE, remplissage(df, cle))
  n <- str_squish(str_to_upper(sans_accents(names(df))))
  prendre <- function(motif) {
    i <- which(str_detect(n, motif))[1]
    if (is.na(i)) { log_msg("ATTENTION ", cle, " : colonne '", motif, "' introuvable"); return(rep(NA_character_, nrow(df))) }
    df[[i]]
  }
  # Contrôle : le plan désigne la provenance et la destination par les
  # colonnes P et Q (filtres des indicateurs 34 et 38)
  for (l in c("P", "Q")) if (col_idx(l) <= ncol(df))
    log_msg(cle, " : colonne ", l, " = '", names(df)[col_idx(l)], "'")
  tibble(
    tc          = prendre("^CONTENEUR"),
    armateur    = prendre("^ARMATEUR"),
    imp_exp     = prendre("^IMPORT/EXPORT"),
    entree      = prendre("^DATE D'ENTREE"),
    sortie      = prendre("^DATE DE SORTIE"),
    mouvement   = prendre("^MOUVEMENT"),
    provenance  = prendre("^PROVENANCE"),
    destination = prendre("^DESTINATION"),
    acheminement = prendre("ACHEMINEMENT"),
    source = cle
  )
}

tbc <- map_dfr(c("tbc_2024", "tbc_2025"), lire_tbc)
if (nrow(tbc) > 0) {
  tbc <- tbc |>
    mutate(
      tc = norm_cle(tc),
      across(c(imp_exp, mouvement, acheminement), norm_txt),
      # provenance (col. P) et destination (col. Q) : codes PNR / BZV / KIN
      across(c(provenance, destination), norm_lieu),
      entree = en_date(entree, CONFIG$format_date_tbc),
      sortie = en_date(sortie, CONFIG$format_date_tbc),
      # corrections de saisie courantes
      mouvement = case_when(
        str_detect(mouvement, "^EM") & str_detect(mouvement, "Q") ~ "EMBARQUEMENT",
        mouvement %in% c("CHARGEMNT", "HARGEMENT") ~ "CHARGEMENT",
        TRUE ~ mouvement),
      acheminement = case_when(str_detect(acheminement, "^FLEU") ~ "FLEUVE",
                               str_detect(acheminement, "^RO") ~ "ROUTE",
                               str_detect(acheminement, "^RAIL") ~ "RAIL",
                               TRUE ~ acheminement)
    ) |>
    distinct(tc, entree, mouvement, .keep_all = TRUE) |>
    mutate(id_tbc = row_number())
  log_msg("TBC : ", nrow(tbc), " mouvements (2024 + 2025 dédoublonnés)")
  log_msg("TBC provenance x destination : ",
          paste(count(tbc, provenance, destination, sort = TRUE) |> head(10) |>
                  mutate(t = paste0(provenance, " > ", destination, " (", n, ")")) |> pull(t),
                collapse = ", "))
}

# -----------------------------------------------------------------------------
# Bases ajoutées par le plan révisé (non reçues à ce jour)
# Le code s'exécute automatiquement dès que les fichiers sont déposés dans data/.
# Colonnes et formats de date paramétrés dans 00_config.R (à vérifier).
# -----------------------------------------------------------------------------

# NAVIS export (bases 13 et 14) : ind. 44, 45, 46
navis_exp <- map_dfr(c("navis_exp_2024", "navis_exp_2025"), function(cle) {
  df <- lire_base(FICHIERS[[cle]], entete_auto = TRUE)
  if (is.null(df)) return(NULL)
  QUALITE <<- bind_rows(QUALITE, remplissage(df, cle))
  selectionner_config(df, CONFIG$colonnes_navis_export, cle) |> mutate(source = cle)
})
if (nrow(navis_exp) > 0) {
  navis_exp <- navis_exp |>
    mutate(tc = norm_cle(tc),
           across(any_of(c("arrivee_terminal", "embarquement", "appareillage")),
                  ~ en_date(.x, CONFIG$format_date_navis_export))) |>
    distinct(tc, embarquement, .keep_all = TRUE)
  log_msg("NAVIS export : ", nrow(navis_exp), " conteneurs embarqués ; ",
          n_distinct(navis_exp$imo), " navires")
}

# SYDONIA bureau BEACH (base 17) : ind. 36, 37
beach <- NULL
if (!is.na(FICHIERS$beach)) {
  df <- lire_base(FICHIERS$beach, entete_auto = TRUE)
  QUALITE <- bind_rows(QUALITE, remplissage(df, "beach"))
  beach <- selectionner_config(df, CONFIG$colonnes_beach, "beach") |>
    mutate(tc = norm_cle(tc),
           across(any_of(c("apurement_im8", "depot_ex3")), ~ en_date(.x, CONFIG$format_date_beach)))
  if ("sh11" %in% names(beach)) {
    beach <- beach |> mutate(sh2 = str_sub(str_pad(str_extract(sh11, "\\d+"), 11, pad = "0"), 1, 2),
                             section_sh = sh_section(sh2))
  }
  beach <- beach |> distinct() |> mutate(id_beach = row_number())
  log_msg("BEACH : ", nrow(beach), " lignes")
}

# SYDONIA export (base 18) : ind. 43, 46
syd_exp <- NULL; syd_exp_dcl <- NULL
if (!is.na(FICHIERS$sydonia_export)) {
  df <- lire_base(FICHIERS$sydonia_export, entete_auto = TRUE)
  QUALITE <- bind_rows(QUALITE, remplissage(df, "sydonia_export"))
  syd_exp <- selectionner_config(df, CONFIG$colonnes_sydonia_exp, "sydonia_export") |>
    mutate(tc = norm_cle(tc),
           bureau = str_trim(bureau),
           type_decl = str_replace_all(str_to_upper(type_decl), "\\s", ""),   # "EX 1" -> "EX1"
           across(c(liquidation, bae), ~ en_date(.x, CONFIG$format_date_sydonia_exp)),
           id_dcl = paste(bureau, num_dcl, str_sub(date_enreg, 1, 10), sep = "|"))
  log_msg("SYDONIA export : ", nrow(syd_exp), " lignes ; types : ",
          paste(names(table(syd_exp$type_decl)), table(syd_exp$type_decl), collapse = ", "))
  # Niveau déclaration (une liquidation et un BAE par déclaration, vérifié)
  syd_exp_dcl <- syd_exp |>
    group_by(id_dcl, bureau, type_decl) |>
    summarise(liquidation = min_na(liquidation), bae = min_na(bae), n_tc = n_distinct(tc), .groups = "drop")
}

# NAVIS transbordement (bases 11 et 12) : ind. 15
navis_tb <- map_dfr(c("navis_tb_2024", "navis_tb_2025"), function(cle) {
  df <- lire_base(FICHIERS[[cle]], entete_auto = TRUE)
  if (is.null(df)) return(NULL)
  QUALITE <<- bind_rows(QUALITE, remplissage(df, cle))
  selectionner_config(df, CONFIG$colonnes_navis_tb, cle) |> mutate(source = cle)
})
if (nrow(navis_tb) > 0) {
  navis_tb <- navis_tb |>
    mutate(tc = norm_cle(tc),
           across(any_of(c("dechargement_mere", "chargement_feeder")),
                  ~ en_date(.x, CONFIG$format_date_navis_tb))) |>
    distinct(tc, call_in, chargement_feeder, .keep_all = TRUE)
  log_msg("NAVIS transbordement : ", nrow(navis_tb), " mouvements ; ",
          n_distinct(navis_tb$tc), " conteneurs ; ", n_distinct(navis_tb$call_in), " escales navire mère")
}

# -----------------------------------------------------------------------------
# DMT – SYDONIA transbordement (base 19, 2024 et 2025 dans un seul fichier) : ind. 16
# Une ligne par conteneur x BL. La colonne I (intitulée NAVIRE_FEEDER) contient
# en réalité l'agent maritime (MAERSK, NILE DUTCH, DELMAS, SAGA, SDV).
# -----------------------------------------------------------------------------
dmt <- NULL
if (!is.na(FICHIERS$dmt)) {
  df <- lire_base(FICHIERS$dmt)
  QUALITE <- bind_rows(QUALITE, remplissage(df, "dmt"))
  dmt_lignes <- selectionner(df, list(
    voyage         = c("A", "VOYAGE"),
    tc             = c("B", "CONTENEUR"),
    navire_mere    = c("C", "NAVIRE_MERE"),
    bl             = c("E", "BL"),
    num_manifest   = c("F", "MANIFESTE"),
    date_manifeste = c("G", "DATE_MANIFESTE"),
    date_dmt       = c("H", "DATE_DMT"),
    agent_maritime = c("I", "FEEDER")), "dmt") |>
    mutate(tc = norm_cle(tc),
           navire_norm    = norm_navire(navire_mere),
           date_manifeste = en_date(date_manifeste, CONFIG$format_date_dmt_manifeste),
           date_dmt       = en_date(date_dmt, CONFIG$format_date_dmt),
           agent_maritime = str_squish(str_to_upper(agent_maritime)))
  # Un conteneur peut figurer sur plusieurs BL d'une même escale du navire mère :
  # on garde une ligne par conteneur x navire mère x voyage, avec le 1er DMT déposé.
  dmt <- dmt_lignes |>
    group_by(tc, navire_norm, voyage) |>
    summarise(n_dmt_distincts = n_distinct(date_dmt, na.rm = TRUE),
              navire_mere = first(navire_mere), num_manifest = first(num_manifest),
              date_manifeste = min_na(date_manifeste), date_dmt = min_na(date_dmt),
              agent_maritime = first(agent_maritime), n_lignes = n()) |>
    ungroup() |>
    mutate(id_dmt = row_number())
  log_msg("DMT (base 19) : ", nrow(dmt_lignes), " lignes -> ", nrow(dmt),
          " conteneurs x escale navire mère ; sans date de DMT : ", sum(is.na(dmt$date_dmt)),
          " ; plusieurs DMT distincts : ", sum(dmt$n_dmt_distincts > 1))
}
