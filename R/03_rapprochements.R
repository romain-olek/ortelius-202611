# =============================================================================
# 03_rapprochements.R : tables de correspondance entre bases
# -----------------------------------------------------------------------------
# Clés utilisées :
#   - conteneur  : NAVIS <-> SYDONIA <-> Global Access <-> TBC
#   - n° de BL   : BESC / BESC de régul <-> SYDONIA
#   - navire     : PAPN <-> NAVIS (nom normalisé + date d'accostage)
# Un même conteneur peut apparaître sur plusieurs escales : on retient à chaque
# fois l'occurrence la plus proche dans le temps (fonction lier_proche).
# Chaque taux de rapprochement est tracé dans l'onglet "Rapprochements".
# =============================================================================

# -----------------------------------------------------------------------------
# A. Escale NAVIS <-> escale PAPN (nom du navire + accostage à +/- 3 jours)
# -----------------------------------------------------------------------------
papn_cible <- papn_esc |>
  filter(categorie %in% c("Porte-conteneurs", "Feeder", "Autres navires (plan)")) |>
  select(numero, navire_norm, accostage_papn = accostage, categorie)

lien_navis_papn <- lier_proche(
  navis_esc |> mutate(id_x = escale_navis),
  papn_cible, cle = "navire_norm", date_x = "accostage", date_y = "accostage_papn",
  tol_j = CONFIG$tol_papn_navis, id_x = "id_x")

# 2e passe pour les noms non reconnus : 8 premiers caractères du nom
reste <- navis_esc |> filter(!escale_navis %in% lien_navis_papn$id_x) |>
  mutate(id_x = escale_navis, nom8 = str_sub(navire_norm, 1, 8))
lien2 <- lier_proche(reste, papn_cible |> mutate(nom8 = str_sub(navire_norm, 1, 8)) |> select(-navire_norm),
                     cle = "nom8", date_x = "accostage", date_y = "accostage_papn",
                     tol_j = CONFIG$tol_papn_navis, id_x = "id_x")
lien_navis_papn <- bind_rows(
  lien_navis_papn |> select(escale_navis = id_x, numero, categorie, accostage_papn),
  lien2 |> select(escale_navis = id_x, numero, categorie, accostage_papn))
noter_lien("Escales NAVIS -> escales PAPN", "NAVIS import (bases 9-10)", "PAPN (bases 7-8)",
           "Nom du navire normalisé", "Accostage le plus proche, écart maximal de 3 jours ; 2e passe sur les 8 premiers caractères du nom",
           nrow(navis_esc), nrow(lien_navis_papn), "11, 20",
           "La PAPN ne porte pas de code IMO : le rapprochement repose sur le nom du navire")

# -----------------------------------------------------------------------------
# B. Conteneur SYDONIA -> conteneur x escale NAVIS
# -----------------------------------------------------------------------------
lien_syd_navis <- lier_proche(
  syd |> mutate(id_x = id_syd),
  navis |> select(tc, id_nav, escale_navis, accostage),
  cle = "tc", date_x = "enreg_manifest", date_y = "accostage",
  tol_j = CONFIG$tol_syd_navis, id_x = "id_x") |>
  select(id_syd = id_x, id_nav, escale_navis)
noter_lien("Lignes SYDONIA -> conteneur x escale NAVIS", "SYDONIA (bases 15-16)", "NAVIS import (bases 9-10)",
           "N° de conteneur", "Escale dont l'accostage est le plus proche de l'enregistrement du manifeste, écart maximal de 45 jours",
           nrow(syd), nrow(lien_syd_navis), "20, 21, 25",
           "Les lignes non rapprochées sont surtout des conteneurs absents de l'extraction NAVIS")

# Manifeste -> escale NAVIS -> escale PAPN (escale majoritaire des conteneurs)
manifeste_escale <- syd |>
  select(id_syd, num_manifest) |>
  inner_join(lien_syd_navis, by = "id_syd") |>
  count(num_manifest, escale_navis, name = "n_tc") |>
  group_by(num_manifest) |>
  slice_max(n_tc, n = 1, with_ties = FALSE) |>
  ungroup() |>
  left_join(lien_navis_papn |> select(escale_navis, numero), by = "escale_navis")
noter_lien("Manifestes SYDONIA -> escale NAVIS", "SYDONIA (bases 15-16)", "NAVIS import (bases 9-10)",
           "N° de manifeste", "Escale majoritaire des conteneurs du manifeste",
           n_distinct(syd$num_manifest), nrow(manifeste_escale), "21", "")
noter_lien("Manifestes SYDONIA -> escale PAPN", "SYDONIA (bases 15-16)", "PAPN (bases 7-8)",
           "N° de manifeste", "Chaînage via NAVIS, puis nom du navire vers la PAPN",
           n_distinct(syd$num_manifest), sum(!is.na(manifeste_escale$numero)), "20", "")

# -----------------------------------------------------------------------------
# C. BL : BESC / BESC de régul <-> SYDONIA
# -----------------------------------------------------------------------------
bl_syd <- unique(na.omit(syd$bl_norm))
if (!is.null(besc)) {
  noter_lien("BL BESC -> SYDONIA", "BESC (base 1)", "SYDONIA (bases 15-16)",
             "N° de BL normalisé", "Correspondance exacte, après suppression des espaces et de la ponctuation",
             n_distinct(besc$bl_norm, na.rm = TRUE), sum(unique(na.omit(besc$bl_norm)) %in% bl_syd), "5, 6",
             "Taux faible : formats de BL hétérogènes, et BESC couvrant aussi d'autres flux")
}
if (!is.null(regul)) {
  noter_lien("BL BESC de régul -> SYDONIA", "BESC de régul (base 3)", "SYDONIA (bases 15-16)",
             "N° de BL normalisé", "Correspondance exacte, après suppression des espaces et de la ponctuation",
             n_distinct(regul$bl_norm, na.rm = TRUE), sum(unique(na.omit(regul$bl_norm)) %in% bl_syd), "6, 7, 8",
             "Base dédoublonnée au chargement : chaque demande y figurait deux fois")
}
# SH par BL (pour les croisements des indicateurs 6 à 8)
sh_par_bl <- syd |>
  filter(!is.na(bl_norm)) |>
  count(bl_norm, sh2, section_sh) |>
  group_by(bl_norm) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(-n)

# -----------------------------------------------------------------------------
# D. Global Access -> NAVIS (scanner postérieur au 1er vu à quai de l'escale)
# -----------------------------------------------------------------------------
lien_ga_navis <- NULL
if (!is.null(ga)) {
  navis_pv <- navis |>
    select(tc, id_nav, escale_navis) |>
    left_join(navis_esc |> select(escale_navis, premier_vu_esc = premier_vu), by = "escale_navis")
  lien_ga_navis <- lier_proche(
    ga |> mutate(id_x = id_ga), navis_pv,
    cle = "tc", date_x = "scan", date_y = "premier_vu_esc",
    tol_j = CONFIG$tol_scan_navis, sens = "avant", id_x = "id_x") |>
    select(id_ga = id_x, id_nav, escale_navis, premier_vu_esc)
  noter_lien("Passages scanner -> NAVIS", "Global Access (base 4)", "NAVIS import (bases 9-10)",
             "N° de conteneur", "Escale dont le 1er vu à quai précède le passage scanner, écart maximal de 60 jours",
             sum(!is.na(ga$scan)), nrow(lien_ga_navis), "13", "")

  # SYDONIA -> image scanner analysée (indicateur 24)
  lien_syd_ga <- lier_proche(
    syd |> mutate(id_x = id_syd),
    ga |> filter(!is.na(date_analyse)) |> select(tc, id_ga, scan, date_analyse, resultat),
    cle = "tc", date_x = "dau", date_y = "scan",
    tol_j = CONFIG$tol_ga_syd, id_x = "id_x") |>
    select(id_syd = id_x, id_ga, date_analyse, resultat)
  noter_lien("Conteneurs SYDONIA IM4 -> image scanner analysée", "SYDONIA (bases 15-16)", "Global Access (base 4)",
             "N° de conteneur", "Passage scanner le plus proche du DAU, écart maximal de 60 jours",
             sum(syd$type_decl == "IM4"), sum(lien_syd_ga$id_syd %in% syd$id_syd[syd$type_decl == "IM4"]), "24",
             "Taux attendu inférieur à 100 % : tous les conteneurs ne sont pas scannés et la base 4 ne couvre que 2025")
}

# -----------------------------------------------------------------------------
# E. TBC -> NAVIS (BAD antérieur à l'entrée au parc de Brazzaville)
# -----------------------------------------------------------------------------
lien_tbc_navis <- NULL
if (nrow(tbc) > 0) {
  lien_tbc_navis <- lier_proche(
    tbc |> mutate(id_x = id_tbc),
    navis |> filter(!is.na(bad)) |> select(tc, id_nav, bad),
    cle = "tc", date_x = "entree", date_y = "bad",
    tol_j = CONFIG$tol_tbc_navis, sens = "avant", marge_j = 1, id_x = "id_x") |>
    select(id_tbc = id_x, id_nav, bad)
  noter_lien("Mouvements TBC -> NAVIS (BAD)", "TBC (bases 5-6)", "NAVIS import (bases 9-10)",
             "N° de conteneur", "BAD antérieur à l'entrée au parc, écart maximal de 90 jours",
             nrow(tbc), nrow(lien_tbc_navis), "34",
             "Seuls les conteneurs importés via Pointe-Noire et présents dans NAVIS peuvent être rapprochés")
}

# -----------------------------------------------------------------------------
# F. Code SH par conteneur (croisement SH de l'indicateur 34, plan révisé)
# Source : SYDONIA Bureau Port, complétée par la base BEACH si elle contient le SH.
# -----------------------------------------------------------------------------
sh_par_tc <- bind_rows(
  syd |> select(tc, sh2, section_sh, produit_sh11, produit_lib),
  if (!is.null(beach) && "sh2" %in% names(beach)) beach |> select(tc, sh2, section_sh)) |>
  filter(!is.na(tc), !is.na(sh2)) |>
  count(tc, sh2, section_sh, produit_sh11, produit_lib) |>
  group_by(tc) |> slice_max(n, n = 1, with_ties = FALSE) |> ungroup() |> select(-n)
if (nrow(tbc) > 0) {
  noter_lien("Conteneurs TBC -> code SH", "TBC (bases 5-6)", "SYDONIA (bases 15-16) et BEACH (base 17)",
             "N° de conteneur", "Code SH le plus fréquent du conteneur",
             n_distinct(tbc$tc), sum(unique(tbc$tc) %in% sh_par_tc$tc), "34, 37 (croisements SH et produit)",
             "Taux faible : les déclarations de transit ne figurent pas dans l'extraction Bureau Port")
}

# -----------------------------------------------------------------------------
# H. NAVIS transbordement (bases 11-12) -> DMT (base 19) : indicateur 16
# Clé du plan : conteneur + navire mère (A&B côté NAVIS, B&C côté base 19).
# La date de rapprochement est la date du manifeste (col. G), proche de
# l'arrivée du navire mère, et non la date du DMT : rapprocher sur la date du
# DMT favoriserait les DMT les plus rapides et biaiserait le délai mesuré.
# 1re passe : conteneur + nom du navire mère normalisé ; 2e passe pour les
# noms écrits différemment : conteneur seul, même tolérance de date.
# -----------------------------------------------------------------------------
lien_tb_dmt <- NULL
navis_tb_tc <- NULL
if (!is.null(dmt) && nrow(navis_tb) > 0) {
  # une observation par conteneur x escale du navire mère
  navis_tb_tc <- navis_tb |>
    mutate(navire_norm = norm_navire(navire_mere)) |>
    group_by(tc, call_in) |>
    summarise(navire_mere = first(navire_mere), navire_norm = first(navire_norm),
              dechargement_mere = min_na(dechargement_mere)) |>
    ungroup() |>
    filter(!is.na(dechargement_mere)) |>
    mutate(id_tb = row_number(), cle_tc_nav = paste(tc, navire_norm))
  dmt_cible <- dmt |> filter(!is.na(date_dmt)) |>
    select(id_dmt, tc, navire_norm_dmt = navire_norm, date_manifeste, date_dmt, agent_maritime) |>
    mutate(cle_tc_nav = paste(tc, navire_norm_dmt))
  l1 <- lier_proche(navis_tb_tc |> mutate(id_x = id_tb), dmt_cible |> select(-tc),
                    cle = "cle_tc_nav", date_x = "dechargement_mere", date_y = "date_manifeste",
                    tol_j = CONFIG$tol_dmt_navis, id_x = "id_x") |>
    transmute(id_tb = id_x, id_dmt, date_dmt, agent_maritime, passe = "Conteneur + navire mère")
  l2 <- lier_proche(navis_tb_tc |> filter(!id_tb %in% l1$id_tb) |> mutate(id_x = id_tb),
                    dmt_cible |> filter(!id_dmt %in% l1$id_dmt) |> select(-cle_tc_nav),
                    cle = "tc", date_x = "dechargement_mere", date_y = "date_manifeste",
                    tol_j = CONFIG$tol_dmt_navis, id_x = "id_x") |>
    transmute(id_tb = id_x, id_dmt, date_dmt, agent_maritime, passe = "Conteneur seul (nom du navire différent)")
  lien_tb_dmt <- bind_rows(l1, l2)
  noter_lien("Conteneurs transbordés NAVIS -> DMT", "NAVIS transbordement (bases 11-12)", "DMT SYDONIA (base 19)",
             "N° de conteneur + nom du navire mère normalisé",
             paste0("Date du manifeste la plus proche du déchargement, écart maximal de ", CONFIG$tol_dmt_navis,
                    " jours ; 2e passe sur le conteneur seul"),
             nrow(navis_tb_tc), nrow(lien_tb_dmt), "16",
             paste0(nrow(l1), " rapprochés en 1re passe, ", nrow(l2), " en 2e passe. ",
                    "Le plan signale qu'environ la moitié des DMT ne figurerait pas dans la base 19"))
}

# -----------------------------------------------------------------------------
# G. BEACH (IM8) -> TBC (indicateur 37)
# -----------------------------------------------------------------------------
lien_beach_tbc <- NULL
if (!is.null(beach) && nrow(tbc) > 0 && "apurement_im8" %in% names(beach)) {
  lien_beach_tbc <- lier_proche(
    beach |> mutate(id_x = id_beach),
    tbc |> select(tc, id_tbc, entree),
    cle = "tc", date_x = "apurement_im8", date_y = "entree",
    tol_j = CONFIG$tol_tbc_navis, id_x = "id_x") |>
    select(id_beach = id_x, id_tbc, entree)
  noter_lien("BEACH (IM8) -> TBC", "BEACH (base 17)", "TBC (bases 5-6)",
             "N° de conteneur", "Mouvement TBC dont la date d'entrée est la plus proche, écart maximal de 90 jours",
             nrow(beach), nrow(lien_beach_tbc), "37", "")
}
