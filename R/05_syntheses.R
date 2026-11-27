# =============================================================================
# 05_syntheses.R : statistiques, distributions et croisements
# =============================================================================

ordre_id <- function(x) order(num_id(x))
meta <- REF |> select(id, trafic, libelle, unite, unite_obs, precision, sources, formule, date_ref_def = date_ref)

# -----------------------------------------------------------------------------
# 1. Synthèse par indicateur et par année
# -----------------------------------------------------------------------------
controle <- obs_all |>
  group_by(id, annee) |>
  summarise(
    n_brut          = n(),
    n_conteneurs    = if (all(is.na(n_tc))) NA_real_ else sum(n_tc, na.rm = TRUE),
    n_exclus_negatif = sum(statut == "Exclu - délai négatif"),
    n_exclus_extreme = sum(str_detect(statut, "extrême")),
    part_negatifs   = mean(delai < 0)
  ) |>
  ungroup()

SYNTHESE <- obs |>
  group_by(id, annee) |>
  summarise(stats_delai(delai)) |>
  ungroup() |>
  right_join(controle, by = c("id", "annee")) |>
  left_join(meta, by = "id") |>
  select(id, trafic, libelle, annee, unite_obs, unite, n_retenu = n, n_conteneurs, moyenne, mediane, p10, p25, p75, p90,
         min, max, n_brut, n_exclus_negatif, n_exclus_extreme, part_negatifs,
         precision, sources, formule, date_ref_def) |>
  left_join(REF |> select(id, neg_ok), by = "id") |>
  mutate(alerte = case_when(
    !neg_ok & part_negatifs > 0.10 ~ "Plus de 10 % de délais négatifs exclus : séquence des dates à vérifier avec le producteur de la donnée",
    n_retenu < 30 ~ "Effectif faible (< 30)",
    TRUE ~ "")) |>
  select(-neg_ok) |>
  relocate(alerte, .after = part_negatifs) |>
  arrange(num_id(id), annee)

# -----------------------------------------------------------------------------
# 2. Comparaison 2024 / 2025
# -----------------------------------------------------------------------------
COMPARAISON <- SYNTHESE |>
  select(id, libelle, unite_obs, unite, annee, n_retenu, moyenne, mediane, p90) |>
  pivot_wider(names_from = annee, values_from = c(n_retenu, moyenne, mediane, p90), names_sep = "_")
for (v in c("n_retenu", "moyenne", "mediane", "p90")) for (a in CONFIG$annees) {
  nm <- paste0(v, "_", a)
  if (!nm %in% names(COMPARAISON)) COMPARAISON[[nm]] <- NA_real_
}
COMPARAISON <- COMPARAISON |>
  mutate(evol_mediane = if_else(mediane_2024 != 0, (mediane_2025 - mediane_2024) / abs(mediane_2024), NA_real_),
         evol_moyenne = if_else(moyenne_2024 != 0, (moyenne_2025 - moyenne_2024) / abs(moyenne_2024), NA_real_)) |>
  select(id, libelle, unite_obs, unite, n_retenu_2024, n_retenu_2025, moyenne_2024, moyenne_2025, evol_moyenne,
         mediane_2024, mediane_2025, evol_mediane, p90_2024, p90_2025) |>
  arrange(num_id(id))

# -----------------------------------------------------------------------------
# 3. Évolution mensuelle
# -----------------------------------------------------------------------------
MENSUEL <- obs |>
  group_by(id, annee, mois = as.Date(mois)) |>
  summarise(n = n(), moyenne = mean(delai), mediane = median(delai),
            p90 = unname(quantile(delai, 0.9))) |>
  ungroup() |>
  left_join(meta |> select(id, libelle, unite, unite_obs), by = "id") |>
  select(id, libelle, unite_obs, unite, annee, mois, n, moyenne, mediane, p90) |>
  arrange(num_id(id), mois)

# -----------------------------------------------------------------------------
# 4. Distribution par tranches
# -----------------------------------------------------------------------------
libelles_tranches <- function(br, u) {
  s <- if (u == "heures") "h" else "j"
  lab <- paste0(head(br, -1), " - ", tail(br, -1), " ", s)
  lab[is.infinite(tail(br, -1))] <- paste0("> ", br[length(br) - 1], " ", s)
  lab
}

TRANCHES <- obs |>
  group_by(id, unite, neg_ok) |>
  group_modify(function(d, k) {
    br <- if (k$unite == "heures") CONFIG$tranches_heures else CONFIG$tranches_jours
    lab <- libelles_tranches(br, k$unite)
    if (k$neg_ok) { br <- c(-Inf, br); lab <- c("< 0 (négatif)", lab) }
    d |> mutate(tranche = cut(delai, breaks = br, labels = lab, right = FALSE, include.lowest = TRUE)) |>
      count(annee, tranche) |>
      group_by(annee) |> mutate(part = n / sum(n)) |> ungroup()
  }) |>
  ungroup() |>
  left_join(meta |> select(id, libelle, unite_obs), by = "id") |>
  select(id, libelle, unite_obs, unite, annee, tranche, n, part) |>
  arrange(num_id(id), annee, tranche)

# -----------------------------------------------------------------------------
# 5. Croisements
# -----------------------------------------------------------------------------
DIMENSIONS <- c(
  section_sh     = "Section du SH",
  sh2            = "Chapitre du SH",
  type_decl      = "Type de déclaration",
  categorie      = "Catégorie de navire",
  type_navire    = "Type de navire (PAPN)",
  zone_livraison = "Zone de livraison (NAVIS col. Q)",
  resultat       = "Résultat de l'analyse scanner",
  classe_taille  = "Taille du conteneur",
  sens_tbc       = "Import / export (TBC)",
  mouvement_tbc  = "Mouvement (TBC)",
  destination    = "Destination (TBC)",
  acheminement   = "Mode d'acheminement (TBC)",
  navire_mere    = "Navire mère (transbordement)",
  navire_export  = "Navire (export)",
  bureau         = "Code bureau (SYDONIA export, col. C)",
  agent_maritime = "Agent maritime (base 19, col. I)"
)

croiser <- function(dim) {
  if (!dim %in% names(obs)) return(NULL)
  obs |>
    filter(!is.na(.data[[dim]])) |>
    group_by(id, annee, modalite = as.character(.data[[dim]])) |>
    summarise(n = n(), moyenne = mean(delai), mediane = median(delai),
              p90 = unname(quantile(delai, 0.9))) |>
    group_by(id, annee) |>
    mutate(part_obs = n / sum(n)) |>
    ungroup() |>
    mutate(dimension = DIMENSIONS[[dim]])
}

CROISEMENTS <- map_dfr(names(DIMENSIONS), croiser) |>
  left_join(meta |> select(id, libelle, unite, unite_obs), by = "id") |>
  select(dimension, id, libelle, unite_obs, unite, annee, modalite, n, part_obs, moyenne, mediane, p90) |>
  arrange(dimension, num_id(id), annee, desc(n))

CROISEMENTS_SH  <- CROISEMENTS |> filter(dimension %in% c("Section du SH", "Chapitre du SH"))
CROISEMENTS_AUT <- CROISEMENTS |> filter(!dimension %in% c("Section du SH", "Chapitre du SH"))

# Catégorie de produit : code SH11 (col. H) et libellé (col. I) de SYDONIA
# (bases 15 et 16). Indicateurs 22, 23, 24, 25, 26, 34 et 37.
# Pour 34 et 37 (conteneurs TBC / BEACH), le produit n'est connu que si le
# conteneur figure dans SYDONIA Bureau Port : les autres observations sont
# gardées sur une ligne "non identifié", pour que part_obs reste sur la base
# de tous les délais retenus.
IDS_PRODUIT <- c("22", "23", "24", "25", "26", "34", "37")
CROISEMENT_PRODUIT <- NULL
if ("produit_sh11" %in% names(obs)) {
  CROISEMENT_PRODUIT <- obs |>
    filter(id %in% IDS_PRODUIT) |>
    mutate(produit_sh11 = coalesce(produit_sh11, "Non identifié"),
           produit_lib  = if_else(produit_sh11 == "Non identifié",
                                  "Conteneur absent de SYDONIA Bureau Port (bases 15-16)", produit_lib)) |>
    group_by(id, annee, produit_sh11, produit_lib) |>
    summarise(n = n(), moyenne = mean(delai), mediane = median(delai),
              p25 = unname(quantile(delai, 0.25)), p75 = unname(quantile(delai, 0.75)),
              p90 = unname(quantile(delai, 0.9)), .groups = "drop") |>
    group_by(id, annee) |>
    mutate(part_obs = n / sum(n)) |>
    ungroup() |>
    left_join(meta |> select(id, libelle, unite, unite_obs), by = "id") |>
    select(id, libelle, unite_obs, unite, annee, produit_sh11, produit_lib,
           n, part_obs, moyenne, mediane, p25, p75, p90) |>
    arrange(num_id(id), annee, desc(n))
}

# -----------------------------------------------------------------------------
# 6. Global Access : résultats d'analyse et tailles
# -----------------------------------------------------------------------------
GA_TABLES <- NULL
if (!is.null(ga)) {
  GA_TABLES <- list(
    resultat = ga |> filter(!is.na(resultat)) |> count(resultat) |> mutate(part = n / sum(n)),
    resultat_mois = ga |> filter(!is.na(resultat), !is.na(date_analyse)) |>
      count(mois_analyse = as.Date(floor_date(date_analyse, "month")), resultat) |>
      group_by(mois_analyse) |> mutate(part = n / sum(n)) |> ungroup(),
    taille = ga |> filter(!is.na(scan)) |> count(classe_taille) |> mutate(part = n / sum(n)),
    resultat_taille = ga |> filter(!is.na(resultat)) |> count(classe_taille, resultat) |>
      group_by(classe_taille) |> mutate(part = n / sum(n)) |> ungroup()
  )
}

# -----------------------------------------------------------------------------
# 7. Contrôles de cohérence des données
# -----------------------------------------------------------------------------
ANOMALIES <- tribble(
  ~base, ~controle, ~n,
  "PAPN", "Escales pertinentes sans date d'arrivée en rade",
    sum(is.na(papn_esc$arrivee_rade) & papn_esc$categorie != "Hors périmètre"),
  "PAPN", "Escales pertinentes sans date d'accostage",
    sum(is.na(papn_esc$accostage) & papn_esc$categorie != "Hors périmètre"),
  "PAPN", "Appareillage antérieur à l'accostage",
    sum(papn_esc$appareillage < papn_esc$accostage, na.rm = TRUE),
  "PAPN", "Arrivée en rade antérieure à 2023",
    sum(year(papn_esc$arrivee_rade) < 2023, na.rm = TRUE),
  "PAPN", "Numéros PAPN distincts (colonne A, avant consolidation)",
    nrow(papn_num),
  "PAPN", "Escales physiques après fusion des numéros d'un même navire",
    nrow(papn_esc),
  "PAPN", "... dont escales regroupant plusieurs numéros PAPN",
    sum(papn_esc$n_numeros > 1),
  "PAPN", "Escales porte-conteneurs et feeders 2025 (PAPN, après consolidation)",
    sum(papn_esc$categorie %in% c("Porte-conteneurs", "Feeder") & year(papn_esc$accostage) == 2025, na.rm = TRUE),
  "NAVIS", "Escales 2025 (NAVIS import, pour comparaison)",
    sum(year(navis_esc$accostage) == 2025, na.rm = TRUE),
  "PAPN", "Numéros du périmètre (PC, feeders, autres navires du plan)",
    nrow(papn_coherence),
  "PAPN", "... dont plusieurs lignes (plusieurs postes à quai pour le même numéro)",
    sum(papn_coherence$n_lignes > 1),
  "PAPN", "... dont dates d'accostage divergentes entre les lignes d'un même numéro",
    sum(papn_coherence$n_accostages > 1),
  "PAPN", "... dont dates d'appareillage divergentes entre les lignes d'un même numéro",
    sum(papn_coherence$n_appareillages > 1),
  "PAPN", "... dont lignes en doublon strict (même poste à quai et mêmes dates)",
    sum(papn_coherence$n_lignes > 1 & papn_coherence$n_quais == 1),
  "NAVIS", "BAD émis en 2026 (enlèvements tardifs ou erreurs de saisie)",
    sum(navis$bad > as.POSIXct("2025-12-31 23:59:59", tz = "UTC"), na.rm = TRUE),
  "NAVIS", "BAD antérieur à l'accostage",
    sum(navis$bad < navis$accostage, na.rm = TRUE),
  "NAVIS", "Chargement remorque antérieur au BAD",
    sum(navis$chargement_remorque < navis$bad, na.rm = TRUE),
  "SYDONIA", "Date de paiement illisible dans la quittance (col. T)",
    sum(is.na(syd$paiement) & !is.na(syd$quittance)),
  "SYDONIA", "BAE antérieur à la sélectivité",
    sum(syd_dcl$bae < syd_dcl$selectivite, na.rm = TRUE),
  "SYDONIA", "Déclarations sans date de sortie (col. W)",
    sum(is.na(syd_dcl$bas)),
  "TBC", "Date de sortie antérieure à la date d'entrée",
    sum(tbc$sortie < tbc$entree, na.rm = TRUE),
  "TBC", "Mouvements PNR > BZV (périmètre de l'indicateur 34)",
    sum(tbc$provenance %in% CONFIG$filtre_34$provenance & tbc$destination %in% CONFIG$filtre_34$destination),
  "TBC", "Mouvements BZV > KIN (périmètre de l'indicateur 38)",
    sum(tbc$provenance %in% CONFIG$filtre_38$provenance & tbc$destination %in% CONFIG$filtre_38$destination),
  "TBC", "Mouvements sans provenance ou sans destination (col. P ou Q vide)",
    sum(is.na(tbc$provenance) | is.na(tbc$destination)),
  "NAVIS transbordement", "Chargement sur le navire de transbordement antérieur au déchargement",
    if (nrow(navis_tb) > 0) sum(navis_tb$chargement_feeder < navis_tb$dechargement_mere, na.rm = TRUE) else NA_real_,
  "NAVIS transbordement", "Mouvements sans date de chargement sur le navire de transbordement",
    if (nrow(navis_tb) > 0) sum(is.na(navis_tb$chargement_feeder)) else NA_real_,
  "NAVIS export", "Embarquement antérieur à l'entrée du conteneur au terminal",
    if (nrow(navis_exp) > 0) sum(navis_exp$embarquement < navis_exp$arrivee_terminal, na.rm = TRUE) else NA_real_,
  "DMT (base 19)", "Conteneurs x escale du navire mère",
    if (!is.null(dmt)) nrow(dmt) else NA_real_,
  "DMT (base 19)", "... sans date de dépôt du DMT (col. H vide)",
    if (!is.null(dmt)) sum(is.na(dmt$date_dmt)) else NA_real_,
  "DMT (base 19)", "... avec plusieurs dates de DMT (1re retenue)",
    if (!is.null(dmt)) sum(dmt$n_dmt_distincts > 1) else NA_real_,
  "DMT (base 19)", "... DMT antérieur à la date du manifeste",
    if (!is.null(dmt)) sum(dmt$date_dmt < dmt$date_manifeste, na.rm = TRUE) else NA_real_,
  "NAVIS transbordement", "Conteneurs transbordés sans DMT retrouvé dans la base 19",
    if (!is.null(lien_tb_dmt)) nrow(navis_tb_tc) - nrow(lien_tb_dmt) else NA_real_,
  "NAVIS export", "Appareillage antérieur à l'embarquement",
    if (nrow(navis_exp) > 0) sum(navis_exp$appareillage < navis_exp$embarquement, na.rm = TRUE) else NA_real_
)

# -----------------------------------------------------------------------------
# 8. Indicateurs non calculables avec les données reçues
# -----------------------------------------------------------------------------
NON_CALCULABLES <- tribble(
  ~id,     ~annees,       ~libelle,                                                                 ~motif,
  "1-4",   "2024, 2025", "Délais DI, assurance, TI, DI -> DAU",                                    "Pas d'indicateur (plan)",
  "5-8",   "2024",       "Indicateurs BESC",                                                        "Pas d'indicateur en 2024 (plan)",
  "13",    "2024",       "1er vu à quai -> scanner",                                                "Pas d'indicateur en 2024 (plan)",
  "15",    "2024, 2025", "Déchargement navire mère -> chargement feeder",                           "Bases 11 / 12 (NAVIS transbordement) non encore déposées dans data/ - code prêt",
  "16",    "2024, 2025", "Déchargement -> dépôt du DMT",                                            "Nécessite les bases 11 / 12 (NAVIS transbordement) et 19 (DMT) dans data/ - code prêt",
  "24",    "2024",       "DAU -> analyse image scannée",                                            "Pas d'indicateur en 2024 (plan)",
  "30",    "2024",       "Conteneurs non dédouanés dans les 30 jours",                              "Pas d'indicateur en 2024 (plan)",
  "31-33", "2024, 2025", "Transit national au BEACH : apurement IM8 -> IM4, DAU -> paiement, sélectivité -> BAE", "En attente de l'extraction SYDONIA du bureau BEACH",
  "35",    "2024, 2025", "Transit sous régime BLD",                                                 "Sans objet (confirmé par le client)",
  "36",    "2024, 2025", "Apurement de l'IM8 au BEACH -> dépôt de l'EX3",                          "Pas d'indicateur (plan) : SYDONIA ne fait pas le lien entre l'IM8 / IM4 et l'EX3 ; possible à la mise en service du T1",
  "37",    "2024, 2025", "Prise en charge de l'IM8 au BEACH -> prise en charge du conteneur par TBC", "Base 17 (IM8 apurés au BEACH) absente de data/ - code prêt",
  "39",    "2024, 2025", "Dépôt EX1 / EX8 -> délivrance BESC export",                               "Sans objet (confirmé par le client : rapprochement CCC / SYDONIA impossible)",
  "40-41", "2024, 2025", "Délivrance DE, TE",                                                       "Pas d'indicateur (plan)",
  "42",    "2024, 2025", "DAU EX1 -> liquidation",                                                  "Sans objet (dépôt et liquidation le même jour)",
  "43",    "2024, 2025", "Liquidation DAU EX1 -> BAE",                                              "En attente de l'extraction SYDONIA export (base 18) - code prêt",
  "44-45", "2024, 2025", "Export : arrivée terminal -> embarquement, embarquement -> appareillage",  "Bases 13 / 14 (NAVIS export) non encore déposées dans data/ - code prêt",
  "46",    "2024",       "% de conteneurs embarqués non couverts par un DAU EX1",                   "Base 14 (NAVIS export 2024) non encore déposée dans data/ - code prêt (2025 calculé si base 13 présente)"
)

# Une fois les bases reçues, les indicateurs effectivement calculés sortent de la liste
ids_calcules <- unique(obs_all$id)
if (!is.null(IND46) && 2024 %in% IND46$annee) ids_calcules <- c(ids_calcules, "46")
deplier <- function(x) { b <- as.integer(str_split(x, "-")[[1]]); as.character(seq(b[1], b[length(b)])) }
NON_CALCULABLES <- NON_CALCULABLES |>
  filter(str_detect(motif, "Pas d'indicateur|supprimé|Supprimé|non précisée") |
           !map_lgl(id, ~ all(deplier(.x) %in% ids_calcules)))
