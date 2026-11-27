# =============================================================================
# 04_indicateurs.R : calcul des indicateurs du plan d'exploitation
# -----------------------------------------------------------------------------
# Chaque indicateur produit une table d'observations :
#   id | date_ref | delai_h (délai en heures) | dimensions de croisement
# L'année d'une observation = année de sa date de référence (date_ref),
# indiquée pour chaque indicateur dans le référentiel ci-dessous.
# Les observations sont ensuite filtrées sur les années prévues par le plan,
# converties dans l'unité de restitution, puis nettoyées (règle d'exclusion).
# =============================================================================

# -----------------------------------------------------------------------------
# Référentiel des indicateurs calculés
# -----------------------------------------------------------------------------
REF <- tribble(
  ~id,  ~trafic,        ~libelle,                                                                                   ~unite,   ~annees,        ~neg_ok, ~sources,                          ~formule,                     ~date_ref,
  "5",  "I&TN",        "Délai entre la délivrance du BESC et le dépôt du DAU (IM4 et IM8)",                          "jours",  list(2025),     FALSE, "Base 1 + SYDONIA",                "R - D (lien par n° BL)",     "Date d'enregistrement du DAU",
  "7",  "I&TN",        "Délai entre la délivrance du BESC de régul et le dépôt du DAU (IM4 et IM8) (positif = régul avant le DAU, cas conforme ; négatif = régul après le DAU)", "jours",  list(2025),     TRUE,  "Base 3 + SYDONIA",                "R - C (DAU - visa de la régul, lien par n° BL)",     "Date d'enregistrement du DAU",
  "8",  "I&TN",        "Délai de délivrance du BESC de régul",                                                        "jours",  list(2025),     FALSE, "Base 3",                          "C - B",                      "Date d'ajout de la demande",
  "9",  "I&TN&TI&TB",  "Attente en rade des porte-conteneurs et feeders (arrivée en rade -> accostage)",               "heures", list(2024, 2025), FALSE, "PAPN",                          "M&N - J&L",                  "Accostage",
  "9bis", "I&TN&TI&TB", "Attente en rade des porte-conteneurs et feeders, PAPN x NAVIS (arrivée en rade PAPN -> accostage NAVIS)", "heures", list(2024, 2025), FALSE, "PAPN + NAVIS",         "I (NAVIS) - J&L (PAPN)",     "Accostage (NAVIS)",
  "10", "I&TN&TI&TB",  "Attente en rade des autres navires (arrivée en rade -> accostage)",                           "heures", list(2024, 2025), FALSE, "PAPN",                          "M&N - J&L",                  "Accostage",
  "11", "I&TN&TI&TB",  "Délai entre l'accostage et le début du déchargement (porte-conteneurs et feeders)",           "heures", list(2024, 2025), FALSE, "PAPN + NAVIS",                  "J (NAVIS) - M&N (PAPN)",     "Accostage (PAPN)",
  "12", "I&TN&TI",     "Délai entre le 1er et le dernier vu à quai (durée de déchargement)",                          "heures", list(2024, 2025), FALSE, "NAVIS",                         "L - K (par escale)",         "1er vu à quai",
  "13", "I&TN&TI",     "Délai entre le 1er vu à quai du navire et le passage au scanner du conteneur",                "heures", list(2025),       FALSE, "NAVIS + Global Access",         "D&E (GA) - K (NAVIS)",       "Passage scanner",
  "14", "I&TN&TI",     "Délai entre l'accostage du navire et l'émission du BAD",                                      "jours",  list(2024, 2025), FALSE, "NAVIS",                         "M - I",                      "Accostage",
  "15", "TB",          "Délai entre le déchargement du navire mère et le chargement sur le feeder",                  "jours",  list(2024, 2025), FALSE, "Bases 11 / 12 (NAVIS transbordement)", "I - H",             "Déchargement du navire mère",
  "16", "TB",          "Délai entre le déchargement du conteneur (navire mère) et le dépôt du DMT par l'agent maritime", "jours", list(2024, 2025), FALSE, "Bases 11 / 12 (NAVIS transbordement) + base 19 (DMT)", "H (base 19) - H (bases 11 / 12), lien conteneur + navire mère", "Déchargement du navire mère",
  "17", "I",           "Temps de séjour au port des porte-conteneurs (accostage -> appareillage)",                    "heures", list(2024, 2025), FALSE, "PAPN",                          "O&P - M&N",                  "Accostage",
  "18", "I",           "Temps de séjour au port des autres navires (accostage -> appareillage)",                      "heures", list(2024, 2025), FALSE, "PAPN",                          "O&P - M&N",                  "Accostage",
  "19", "TB",          "Temps de séjour au port des feeders (accostage -> appareillage)",                             "heures", list(2024, 2025), FALSE, "PAPN",                          "O&P - M&N",                  "Accostage",
  "20", "I&TN&TI&TB",  "Délai entre l'arrivée en rade et l'enregistrement du manifeste (négatif = manifeste avant l'arrivée)", "heures", list(2024, 2025), TRUE, "PAPN + SYDONIA (via NAVIS)", "L (SYDONIA) - J&L (PAPN)", "Arrivée en rade",
  "21", "I&T&TB",      "Délai entre le début du déchargement et l'apurement du manifeste",                            "jours",  list(2024, 2025), FALSE, "NAVIS + SYDONIA",               "M (SYDONIA) - J (NAVIS)",    "Début du déchargement",
  "22", "I",           "Délai entre le dépôt du DAU (IM4) et le paiement",                                            "jours",  list(2024, 2025), FALSE, "SYDONIA",                       "T - R",                      "Date d'enregistrement du DAU",
  "23", "I",           "Délai entre la sélectivité et le BAE (IM4)",                                                   "jours",  list(2024, 2025), FALSE, "SYDONIA",                       "V - U",                      "Date d'enregistrement du DAU",
  "24", "I",           "Délai entre le dépôt du DAU (IM4) et l'analyse de l'image scannée",                           "jours",  list(2025),       FALSE, "SYDONIA + Global Access",       "AB (GA) - R (SYDONIA)",      "Date d'enregistrement du DAU",
  "25", "I",           "Délai entre l'émission du BAE (IM4) et l'émission du BAD",                                    "jours",  list(2024, 2025), FALSE, "SYDONIA + NAVIS",               "M (NAVIS) - V (SYDONIA)",    "Date d'enregistrement du DAU",
  "26", "I",           "Délai entre l'émission du BAE (IM4) et le BAS douane",                                        "jours",  list(2024, 2025), FALSE, "SYDONIA",                       "W - V",                      "Date d'enregistrement du DAU",
  "27", "I",           "Délai entre l'émission du BAD et le chargement sur la remorque",                              "heures", list(2024, 2025), FALSE, "NAVIS",                         "N - M",                      "Émission du BAD",
  "28", "I",           "Délai entre l'émission du BAD et la livraison",                                               "heures", list(2024, 2025), FALSE, "NAVIS",                         "P - M",                      "Émission du BAD",
  "29", "I",           "Délai entre la livraison et le retour du conteneur vide",                                     "jours",  list(2024, 2025), FALSE, "NAVIS",                         "U - P",                      "Livraison",
  "34", "TN&TI",       "Délai moyen de transit entre Congo Terminal (remise du BAD) et l'entrée dans TBC",            "jours",  list(2024, 2025), FALSE, "NAVIS + TBC",                   "D (TBC) - M (NAVIS) ; provenance (col. P) = PNR et destination (col. Q) = BZV",        "Entrée au parc TBC",
  "36", "TI",          "Délai entre l'apurement de l'IM8 au BEACH et le dépôt de l'EX3 au bureau du BEACH",           "jours",  list(2024, 2025), FALSE, "Base 17 (BEACH)",               "W - M",                      "Apurement de l'IM8",
  "37", "TI",          "Délai entre la prise en charge de l'IM8 par le BEACH et la prise en charge du conteneur par TBC", "jours", list(2024, 2025), TRUE, "Base 17 + TBC",               "D (TBC) - M (BEACH)",        "Apurement de l'IM8",
  "38", "TI",          "Délai entre l'arrivée au parc TBC et l'embarquement sur la barge pour Kinshasa",              "jours",  list(2024, 2025), FALSE, "TBC",                           "Sortie - entrée (E - D en 2025 / F - D en 2024) ; provenance (col. P) = BZV et destination (col. Q) = KIN", "Entrée au parc TBC",
  "43", "E",           "Délai entre la liquidation du DAU (EX1) et l'émission du BAE",                                "jours",  list(2024, 2025), FALSE, "Base 18 (SYDONIA export)",       "R - O (EX1, col. J ; par code bureau, col. C)", "Liquidation",
  "44", "E",           "Délai entre l'arrivée du conteneur au terminal et son embarquement",                          "jours",  list(2024, 2025), FALSE, "Bases 13 / 14 (NAVIS export)",  "C - B",                      "Embarquement",
  "45", "E",           "Délai entre l'embarquement des conteneurs et l'appareillage du navire",                       "heures", list(2024, 2025), FALSE, "Bases 13 / 14 (NAVIS export)",  "F - C",                      "Embarquement"
)

if (CONFIG$ind9bis_date_navis == "debut_dech") {
  REF <- REF |> mutate(
    libelle = if_else(id == "9bis", "Délai entre l'arrivée en rade (PAPN) et le début du déchargement (NAVIS), porte-conteneurs et feeders", libelle),
    formule = if_else(id == "9bis", "J (NAVIS) - J&L (PAPN)", formule),
    date_ref = if_else(id == "9bis", "Début du déchargement (NAVIS)", date_ref))
}

# Unité d'observation de chaque indicateur (validée avec le client)
REF <- REF |> mutate(unite_obs = case_when(
  id %in% c("9", "9bis", "10", "11", "12", "17", "18", "19") ~ "Navire (escale)",
  id %in% c("20", "21")                                ~ if (CONFIG$unite_manifeste == "escale")
                                                            "Navire (escale)" else "Manifeste",
  id %in% c("6", "8")                                  ~ "Connaissement (BL)",
  id %in% c("5", "7", "22", "23", "26", "43")          ~ if (CONFIG$unite_douane == "declaration")
                                                            "Déclaration (DAU)" else "Conteneur",
  TRUE                                                 ~ "Conteneur"))

# Précision : jours calendaires quand l'une des dates n'a pas d'heure
REF <- REF |> mutate(precision = if_else(
  id %in% c("5", "7", "8", "21", "22", "24", "34", "37", "38"),
  "Jours calendaires (au moins une date sans heure)", "Horodatage complet"))

OBS <- list()
ajouter <- function(id, df) {
  OBS[[id]] <<- df |> mutate(id = id) |> filter(!is.na(delai_h), !is.na(date_ref))
  log_msg("Indicateur ", id, " : ", nrow(OBS[[id]]), " observations brutes")
}

# Unité d'observation des indicateurs douaniers (cf. CONFIG$unite_douane).
# Au niveau déclaration, n_tc conserve le nombre de conteneurs couverts.
syd_im48 <- if (CONFIG$unite_douane == "declaration") {
  syd_dcl |> filter(type_decl %in% c("IM4", "IM8"))
} else {
  syd |> filter(type_decl %in% c("IM4", "IM8")) |> mutate(n_tc = 1)
}
syd_im4_dcl <- syd_dcl |> filter(type_decl == "IM4")
syd_im4 <- syd |> filter(type_decl == "IM4")       # niveau conteneur (ind. 24, 25)
syd_im4_u <- if (CONFIG$unite_douane == "declaration") {
  syd_im4_dcl
} else {
  syd_im4 |> mutate(n_tc = 1)
}

# --- 5. BESC -> DAU ------------------------------------------------------------
if (!is.null(besc)) {
  besc_bl <- besc |> filter(!is.na(bl_norm), !is.na(validation)) |>
    group_by(bl_norm) |> summarise(validation = min(validation)) |> ungroup()
  ajouter("5", syd_im48 |> inner_join(besc_bl, by = "bl_norm") |>
            transmute(date_ref = dau, delai_h = diff_cal_h(dau, validation),
                      type_decl, sh2, section_sh, n_tc))
}

# --- 7 et 8. BESC de régul ----------------------------------------------------
if (!is.null(regul)) {
  regul_bl <- regul |> filter(!is.na(bl_norm)) |>
    group_by(bl_norm) |> summarise(vise = min_na(vise)) |> ungroup()
  # R - C : date du DAU moins date de visa de la régul. Positif = régul visée
  # avant le DAU (règle) ; négatif = régul visée après le DAU.
  ajouter("7", syd_im48 |> inner_join(regul_bl, by = "bl_norm") |>
            transmute(date_ref = dau, delai_h = diff_cal_h(dau, vise), type_decl, sh2, section_sh, n_tc))
  ajouter("8", regul |> left_join(sh_par_bl, by = "bl_norm") |>
            transmute(date_ref = ajoute, delai_h = diff_cal_h(vise, ajoute), sh2, section_sh))
}

# --- 9, 10, 17, 18, 19. Escales PAPN -------------------------------------------
esc <- papn_esc |> mutate(type_navire = typenavire)
ajouter("9",  esc |> filter(categorie %in% c("Porte-conteneurs", "Feeder")) |>
          transmute(date_ref = accostage, delai_h = diff_h(accostage, arrivee_rade), categorie, type_navire))
ajouter("10", esc |> filter(categorie == "Autres navires (plan)") |>
          transmute(date_ref = accostage, delai_h = diff_h(accostage, arrivee_rade), categorie, type_navire))
ajouter("17", esc |> filter(categorie == "Porte-conteneurs") |>
          transmute(date_ref = accostage, delai_h = diff_h(appareillage, accostage), type_navire))
ajouter("18", esc |> filter(categorie == "Autres navires (plan)") |>
          transmute(date_ref = accostage, delai_h = diff_h(appareillage, accostage), type_navire))
ajouter("19", esc |> filter(categorie == "Feeder") |>
          transmute(date_ref = accostage, delai_h = diff_h(appareillage, accostage), type_navire))

# --- 9 bis. Arrivée en rade (PAPN) -> accostage (NAVIS) --------------------------
# Une observation par escale PAPN : si plusieurs escales NAVIS sont rattachées
# à la même escale PAPN, on retient la plus précoce.
date9bis <- CONFIG$ind9bis_date_navis
ajouter("9bis", navis_esc |>
          inner_join(lien_navis_papn, by = "escale_navis") |>
          filter(categorie %in% c("Porte-conteneurs", "Feeder")) |>
          group_by(numero, categorie) |>
          summarise(fin_attente = min_na(.data[[date9bis]])) |>
          ungroup() |>
          inner_join(papn_esc |> select(numero, arrivee_rade, type_navire = typenavire), by = "numero") |>
          transmute(date_ref = fin_attente, delai_h = diff_h(fin_attente, arrivee_rade),
                    categorie, type_navire))

# --- 11. Accostage (PAPN) -> début déchargement (NAVIS) -------------------------
ajouter("11", navis_esc |> inner_join(lien_navis_papn, by = "escale_navis") |>
          filter(categorie %in% c("Porte-conteneurs", "Feeder")) |>
          transmute(date_ref = accostage_papn, delai_h = diff_h(debut_dech, accostage_papn), categorie))

# --- 12. 1er vu à quai -> dernier vu à quai (par escale NAVIS) -------------------
ajouter("12", navis_esc |> transmute(date_ref = premier_vu, delai_h = diff_h(dernier_vu, premier_vu)))

# --- 13. 1er vu à quai -> passage scanner (par conteneur) ------------------------
if (!is.null(lien_ga_navis)) {
  ajouter("13", ga |> inner_join(lien_ga_navis, by = "id_ga") |>
            transmute(date_ref = scan, delai_h = diff_h(scan, premier_vu_esc), resultat, classe_taille))
}

# --- 14. Accostage -> BAD (par conteneur, NAVIS) --------------------------------
ajouter("14", navis |> transmute(date_ref = accostage, delai_h = diff_h(bad, accostage), zone_livraison = type_livraison))

# --- 20. Arrivée en rade -> enregistrement du manifeste --------------------------
manifestes <- syd |> group_by(num_manifest) |>
  summarise(enreg_manifest = first(enreg_manifest), apur_manifest = first(apur_manifest)) |> ungroup() |>
  inner_join(manifeste_escale, by = "num_manifest")
man20 <- manifestes |> filter(!is.na(numero))
if (CONFIG$unite_manifeste == "escale")
  man20 <- man20 |> group_by(numero) |> summarise(enreg_manifest = min_na(enreg_manifest)) |> ungroup()
ajouter("20", man20 |>
          inner_join(papn_esc |> select(numero, arrivee_rade, categorie), by = "numero") |>
          filter(categorie %in% c("Porte-conteneurs", "Feeder")) |>
          transmute(date_ref = arrivee_rade, delai_h = diff_h(enreg_manifest, arrivee_rade), categorie))

# --- 21. Début déchargement -> apurement du manifeste ----------------------------
man21 <- manifestes
if (CONFIG$unite_manifeste == "escale")
  man21 <- man21 |> group_by(escale_navis) |> summarise(apur_manifest = max_na(apur_manifest)) |> ungroup()
ajouter("21", man21 |>
          inner_join(navis_esc |> select(escale_navis, debut_dech), by = "escale_navis") |>
          transmute(date_ref = debut_dech, delai_h = diff_cal_h(apur_manifest, debut_dech)))

# --- 22, 23, 26. Délais douaniers (par déclaration IM4) --------------------------
ajouter("22", syd_im4_u |> transmute(date_ref = dau, delai_h = diff_cal_h(paiement, dau), sh2, section_sh,
                                     produit_sh11, produit_lib, n_tc))
ajouter("23", syd_im4_u |> transmute(date_ref = dau, delai_h = diff_h(bae, selectivite), sh2, section_sh,
                                     produit_sh11, produit_lib, n_tc))
ajouter("26", syd_im4_u |> transmute(date_ref = dau, delai_h = diff_h(bas, bae), sh2, section_sh,
                                     produit_sh11, produit_lib, n_tc))

# --- 24. DAU -> analyse de l'image scannée (par conteneur IM4) --------------------
if (!is.null(ga)) {
  ajouter("24", syd_im4 |> inner_join(lien_syd_ga, by = "id_syd") |>
            transmute(date_ref = dau, delai_h = diff_cal_h(date_analyse, dau), sh2, section_sh,
                      produit_sh11, produit_lib, resultat))
}

# --- 25. BAE -> BAD (par conteneur IM4) -------------------------------------------
ajouter("25", syd_im4 |> inner_join(lien_syd_navis, by = "id_syd") |>
          inner_join(navis |> select(id_nav, bad, type_livraison), by = "id_nav") |>
          transmute(date_ref = dau, delai_h = diff_h(bad, bae), sh2, section_sh, produit_sh11, produit_lib,
                    zone_livraison = type_livraison))

# --- 27, 28, 29. Enlèvement et livraison (NAVIS) ---------------------------------
ajouter("27", navis |> transmute(date_ref = bad, delai_h = diff_h(chargement_remorque, bad), zone_livraison = type_livraison))
ajouter("28", navis |> transmute(date_ref = bad, delai_h = diff_h(livraison, bad), zone_livraison = type_livraison))
ajouter("29", navis |> transmute(date_ref = livraison, delai_h = diff_h(retour_vide, livraison), zone_livraison = type_livraison))

# Filtre provenance / destination TBC (NULL = pas de filtre sur la colonne)
filtrer_tbc <- function(df, filtre, id) {
  n0 <- nrow(df)
  if (!is.null(filtre$provenance))  df <- df |> filter(provenance %in% filtre$provenance)
  if (!is.null(filtre$destination)) df <- df |> filter(destination %in% filtre$destination)
  log_msg("Indicateur ", id, " : filtre TBC provenance = ", paste(filtre$provenance, collapse = "/"),
          ", destination = ", paste(filtre$destination, collapse = "/"), " -> ", nrow(df), " / ", n0, " mouvements")
  df
}

# --- 34. BAD -> entrée TBC ----------------------------------------------------------
if (!is.null(lien_tbc_navis)) {
  # Filtre du plan : provenance (col. P) = PNR et destination (col. Q) = BZV
  tbc34 <- filtrer_tbc(tbc, CONFIG$filtre_34, "34")
  ajouter("34", tbc34 |> inner_join(lien_tbc_navis, by = "id_tbc") |>
            left_join(sh_par_tc, by = "tc") |>
            transmute(date_ref = entree, delai_h = diff_cal_h(entree, bad),
                      sh2, section_sh, produit_sh11, produit_lib,
                      sens_tbc = imp_exp, destination, acheminement))
}

# --- 38. Entrée TBC -> embarquement barge pour Kinshasa ----------------------------
if (nrow(tbc) > 0) {
  # Filtre du plan : provenance (col. P) = BZV et destination (col. Q) = KIN
  tbc38 <- filtrer_tbc(tbc, CONFIG$filtre_38, "38")
  ajouter("38", tbc38 |> transmute(date_ref = entree, delai_h = diff_cal_h(sortie, entree),
                                   sens_tbc = imp_exp, destination, mouvement_tbc = mouvement))
}

# --- 16. Transbordement : déchargement navire mère -> dépôt du DMT ------------------
if (!is.null(lien_tb_dmt)) {
  ajouter("16", navis_tb_tc |> inner_join(lien_tb_dmt, by = "id_tb") |>
            transmute(date_ref = dechargement_mere, delai_h = diff_h(date_dmt, dechargement_mere),
                      agent_maritime, navire_mere))
}

# --- 15. Transbordement : déchargement navire mère -> chargement feeder -------------
if (nrow(navis_tb) > 0) {
  ajouter("15", navis_tb |> transmute(date_ref = dechargement_mere,
                                      delai_h = diff_h(chargement_feeder, dechargement_mere),
                                      navire_mere))
}

# --- 36, 37. Transit international au BEACH (base 17) ------------------------------
if (!is.null(beach)) {
  dims_sh <- intersect(c("sh2", "section_sh"), names(beach))
  # Indicateur 36 : pas d'indicateur (plan). SYDONIA ne fait pas le lien entre
  # l'IM8 / IM4 et l'EX3 ; possible à la mise en service du T1.
  if (!is.null(lien_beach_tbc)) {
    # Catégorie de produit (SH11 + libellé) : celle de la base 17 (col. H et I),
    # à défaut SYDONIA Bureau Port (bases 15-16) par n° de conteneur
    b37 <- beach |> inner_join(lien_beach_tbc, by = "id_beach")
    if (!"produit_sh11" %in% names(b37)) b37 <- b37 |> mutate(produit_sh11 = NA_character_, produit_lib = NA_character_)
    ajouter("37", b37 |>
              left_join(sh_par_tc |> select(tc, sh11_syd = produit_sh11, lib_syd = produit_lib), by = "tc") |>
              mutate(produit_lib  = if_else(is.na(produit_sh11), lib_syd, produit_lib),
                     produit_sh11 = coalesce(produit_sh11, sh11_syd)) |>
              transmute(date_ref = apurement_im8, delai_h = diff_cal_h(entree, apurement_im8),
                        across(all_of(dims_sh)), produit_sh11, produit_lib))
  }
}

# --- 43. Liquidation -> BAE des DAU EX1 (base 18) -----------------------------------
if (!is.null(syd_exp)) {
  # Filtre EX1 (col. J), R - O, ventilé par code bureau (col. C)
  ex1_u <- if (CONFIG$unite_douane == "declaration") {
    syd_exp_dcl |> filter(type_decl == "EX1")
  } else {
    syd_exp |> filter(type_decl == "EX1") |> mutate(n_tc = 1)
  }
  ajouter("43", ex1_u |> transmute(date_ref = liquidation, delai_h = diff_h(bae, liquidation),
                                   bureau, n_tc))
}

# --- 44, 45. NAVIS export (bases 13 et 14) -------------------------------------------
if (nrow(navis_exp) > 0) {
  ajouter("44", navis_exp |> transmute(date_ref = embarquement,
                                       delai_h = diff_h(embarquement, arrivee_terminal), navire_export = navire))
  ajouter("45", navis_exp |> transmute(date_ref = embarquement,
                                       delai_h = diff_h(appareillage, embarquement), navire_export = navire))
}

# =============================================================================
# Consolidation : année, unité, règle d'exclusion
# =============================================================================
obs_all <- bind_rows(OBS) |>
  mutate(annee = year(date_ref), mois = floor_date(date_ref, "month")) |>
  inner_join(REF |> select(id, unite, annees, neg_ok), by = "id") |>
  filter(map2_lgl(annee, annees, ~ .x %in% .y)) |>
  select(-annees) |>
  mutate(delai = if_else(unite == "jours", delai_h / 24, delai_h))

obs_all <- obs_all |>
  group_by(id, annee) |>
  mutate(
    .hi = quantile(delai[neg_ok | delai >= 0], CONFIG$percentile_haut, na.rm = TRUE),
    .lo = if_else(neg_ok, quantile(delai, CONFIG$percentile_bas, na.rm = TRUE), 0),
    statut = case_when(
      !neg_ok & delai < 0 ~ "Exclu - délai négatif",
      delai > .hi         ~ "Exclu - valeur extrême (> P99)",
      neg_ok & delai < .lo ~ "Exclu - valeur extrême (< P1)",
      TRUE                ~ "Retenu")
  ) |>
  ungroup() |>
  select(-.hi, -.lo)

obs <- obs_all |> filter(statut == "Retenu")
log_msg("Observations : ", nrow(obs_all), " brutes, ", nrow(obs), " retenues")

# =============================================================================
# Indicateurs "non délai" : 6 (part des BESC de régul) et 30 (non dédouanés)
# =============================================================================

# --- 6. Part des BESC de régul -----------------------------------------------------
IND6 <- NULL
if (!is.null(besc) && !is.null(regul)) {
  deb <- min(regul$ajoute, na.rm = TRUE); fin <- max(regul$ajoute, na.rm = TRUE)
  # Unité d'observation : le connaissement (BL), pas la ligne de fichier
  bl_regul <- unique(na.omit(regul$bl_norm))
  bl_besc_an <- unique(na.omit(besc$bl_norm))
  bl_besc_per <- unique(na.omit(besc$bl_norm[besc$validation >= deb & besc$validation < fin + days(1)]))
  n_b_an <- length(bl_besc_an); n_r <- length(bl_regul); n_b_per <- length(bl_besc_per)
  IND6_global <- tibble(
    calcul = c("Même période que la base BESC de régul (retenu, validé par le client)",
               "Formule initiale du plan (BESC de régul sur la période disponible / BESC année entière)"),
    periode = c(paste0(format(deb, "%d/%m/%Y"), " - ", format(fin, "%d/%m/%Y")),
                paste0("Régul : ", format(deb, "%d/%m/%Y"), " - ", format(fin, "%d/%m/%Y"), " ; BESC : année 2025")),
    n_besc_regul = n_r, n_besc = c(n_b_per, n_b_an),
    unite = "Connaissements (BL) distincts",
    part_regul = n_r / (n_r + c(n_b_per, n_b_an)))
  # croisement SH (via BL -> SYDONIA), sur la période commune
  IND6_sh <- bind_rows(
    regul |> distinct(bl_norm) |> left_join(sh_par_bl, by = "bl_norm") |> mutate(type = "regul"),
    besc |> filter(validation >= deb, validation < fin + days(1)) |> distinct(bl_norm) |>
      left_join(sh_par_bl, by = "bl_norm") |> mutate(type = "besc")) |>
    mutate(section_sh = coalesce(section_sh, "SH non identifié (BL absent de SYDONIA)")) |>
    mutate(type = factor(type, levels = c("regul", "besc"))) |>
    count(section_sh, type) |>
    pivot_wider(names_from = type, values_from = n, values_fill = 0, names_expand = TRUE) |>
    mutate(part_regul = regul / (regul + besc)) |>
    arrange(desc(regul + besc))
  IND6 <- list(global = IND6_global, sh = IND6_sh)
  log_msg("Indicateur 6 : ", paste(round(100 * IND6_global$part_regul, 2), collapse = " % / "), " %")
}

# --- 30. Conteneurs débarqués sans déclaration (Global Access) --------------------
IND30 <- NULL
if (!is.null(ga)) {
  # Le client a confirmé qu'il ne faut rien exclure : tous les conteneurs
  # débarqués sans déclaration sont retenus (HG et transbordement compris).
  cand <- ga |> filter(present_deb == "OUI", is.na(declaration))
  # devenir du conteneur : livraison dans NAVIS (même conteneur, accostage proche)
  liv <- lier_proche(cand |> mutate(id_x = id_ga),
                     navis |> select(tc, accostage, livraison),
                     cle = "tc", date_x = "date_accostage", date_y = "accostage",
                     tol_j = CONFIG$tol_papn_navis, id_x = "id_x") |>
    select(id_ga = id_x, livraison)
  cand <- cand |> left_join(liv, by = "id_ga") |>
    mutate(sejour_j = diff_h(livraison, date_deb) / 24,
           fin_obs = max(ga$scan, na.rm = TRUE),
           statut30 = case_when(
             !is.na(sejour_j) & sejour_j > CONFIG$seuil_non_dedouane_j ~ "Livré après plus de 30 jours",
             !is.na(sejour_j) ~ "Livré en 30 jours ou moins",
             is.na(date_deb) ~ "Date de débarquement absente",
             diff_h(fin_obs, date_deb) / 24 > CONFIG$seuil_non_dedouane_j ~ "Non livré à la fin de la période (> 30 j)",
             TRUE ~ "Non livré, débarqué depuis moins de 30 jours"))
  sej <- cand |> filter(!is.na(sejour_j), sejour_j >= 0)
  IND30 <- list(
    synthese = tibble(
      indicateur = c("Conteneurs débarqués sans déclaration au moment du scanner (aucune exclusion)",
                     "... dont transbordement (catégorie 4, colonne C de la base 4)",
                     "... part du transbordement (%)",
                     "... dont hors gabarit (mention H.G dans la colonne C)",
                     "... dont livrés (retrouvés dans NAVIS)",
                     "... dont non dédouanés dans les 30 jours (livrés > 30 j ou non livrés > 30 j)",
                     "Temps de séjour moyen des conteneurs finalement livrés (jours)",
                     "Temps de séjour médian des conteneurs finalement livrés (jours)"),
      valeur = c(nrow(cand),
                 sum(cand$classe_taille == "4 - Transbordement"),
                 100 * mean(cand$classe_taille == "4 - Transbordement"),
                 sum(cand$hors_gabarit, na.rm = TRUE),
                 nrow(sej),
                 sum(cand$statut30 %in% c("Livré après plus de 30 jours", "Non livré à la fin de la période (> 30 j)")),
                 mean(sej$sejour_j), median(sej$sejour_j))),
    detail = bind_rows(
      cand |> count(ventilation = paste("Statut :", statut30), name = "n_conteneurs"),
      cand |> count(ventilation = paste("Catégorie :", classe_taille), name = "n_conteneurs")),
    mensuel = cand |> filter(!is.na(date_deb)) |>
      count(mois = floor_date(date_deb, "month"), statut30) |>
      pivot_wider(names_from = statut30, values_from = n, values_fill = 0))
  log_msg("Indicateur 30 : ", nrow(cand), " conteneurs sans déclaration")
}

# --- 46. Part des conteneurs embarqués non couverts par un DAU EX1 -----------------
# Formule du plan : 1 - conteneurs SYDONIA EX1 (col. A, base 18) / conteneurs
# embarqués NAVIS (col. A, bases 13-14), par année (liquidation / embarquement).
# Contrôle par rapprochement : un conteneur embarqué est couvert s'il figure
# dans un DAU EX1 liquidé entre tol_ex1_navis jours avant et 7 jours après
# son embarquement. Seules les années disposant d'une base NAVIS export sont
# calculées.
IND46 <- NULL
if (!is.null(syd_exp) && nrow(navis_exp) > 0) {
  ex1 <- syd_exp |> filter(type_decl == "EX1", !is.na(tc))
  emb_tc <- navis_exp |> filter(!is.na(tc), !is.na(embarquement)) |>
    group_by(tc) |> summarise(embarquement = min(embarquement), .groups = "drop")
  couverts <- emb_tc |>
    inner_join(ex1 |> select(tc, liquidation), by = "tc", relationship = "many-to-many") |>
    filter(liquidation >= embarquement - days(CONFIG$tol_ex1_navis),
           liquidation <= embarquement + days(7)) |>
    distinct(tc) |> pull(tc)
  annees_navis <- sort(unique(year(emb_tc$embarquement)))
  IND46 <- map_dfr(intersect(CONFIG$annees, annees_navis), function(a) {
    emb_a <- emb_tc |> filter(year(embarquement) == a)
    ex1_a <- ex1 |> filter(year(liquidation) == a)
    n_emb <- nrow(emb_a)
    n_ex1 <- n_distinct(ex1_a$tc)
    n_couv <- sum(emb_a$tc %in% couverts)
    tibble(annee = a,
           n_conteneurs_embarques_navis = n_emb,
           n_conteneurs_ex1_sydonia = n_ex1,
           n_conteneurs_ex1_DD141 = n_distinct(ex1_a$tc[ex1_a$bureau == "DD141"]),
           n_conteneurs_ex1_DD147 = n_distinct(ex1_a$tc[ex1_a$bureau == "DD147"]),
           part_non_couverte_formule = 1 - n_ex1 / n_emb,
           n_embarques_rapproches_ex1 = n_couv,
           part_non_couverte_rapprochement = 1 - n_couv / n_emb,
           part_ex1_retrouves_navis = mean(unique(ex1_a$tc) %in% emb_tc$tc),
           mode_calcul = paste0("Formule du plan : 1 - nb de conteneurs distincts EX1 (base 18, col. A, année de liquidation) / ",
                                "nb de conteneurs distincts embarqués (NAVIS col. A, année d'embarquement). ",
                                "Rapprochement : n° de conteneur, liquidation EX1 entre ", CONFIG$tol_ex1_navis,
                                " j avant et 7 j après l'embarquement."))
  })
  manq <- setdiff(CONFIG$annees, annees_navis)
  if (length(manq) > 0) log_msg("Indicateur 46 : non calculé pour ", paste(manq, collapse = ", "),
                                " (base NAVIS export absente)")
  log_msg("Indicateur 46 : ", paste0(IND46$annee, " = ", round(100 * IND46$part_non_couverte_formule, 1),
                                     " % (formule), ", round(100 * IND46$part_non_couverte_rapprochement, 1),
                                     " % (rapprochement)", collapse = " ; "))
}
