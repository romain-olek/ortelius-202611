# =============================================================================
# 01_fonctions.R : fonctions utilitaires (lecture, dates, clés, statistiques)
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(purrr)
  library(openxlsx)
})

options(dplyr.summarise.inform = FALSE)

# -----------------------------------------------------------------------------
# Journal
# -----------------------------------------------------------------------------
JOURNAL <- character()
log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%H:%M:%S"), " | ", paste0(...))
  message(msg)
  JOURNAL <<- c(JOURNAL, msg)
}

# -----------------------------------------------------------------------------
# Repérage et lecture des fichiers
# -----------------------------------------------------------------------------
verifier_dossier_data <- function() {
  if (!dir.exists(CONFIG$dir_data)) {
    stop("Dossier de données introuvable : '", CONFIG$dir_data, "'.\n",
         "Répertoire de travail actuel : ", getwd(), "\n",
         "Lancer le script depuis la racine du projet (setwd(...)) ",
         "ou indiquer un chemin complet dans CONFIG$dir_data (00_config.R).")
  }
  f <- list.files(CONFIG$dir_data, pattern = "\\.xlsx?$", ignore.case = TRUE)
  f <- f[!startsWith(f, "~$")]
  if (length(f) == 0) stop("Aucun fichier Excel dans '", normalizePath(CONFIG$dir_data), "'.")
  log_msg("Fichiers Excel trouvés dans ", normalizePath(CONFIG$dir_data), " : ", length(f))
  walk(f, ~ log_msg("   ", .x))
}

trouver_fichier <- function(cle) {
  motif <- CONFIG$motifs[[cle]]
  f <- list.files(CONFIG$dir_data, pattern = motif, full.names = TRUE, ignore.case = TRUE)
  f <- f[grepl("\\.xlsx?$", f, ignore.case = TRUE) & !startsWith(basename(f), "~$")]
  if (length(f) == 0) {
    log_msg("ABSENTE : ", cle, " (motif ", motif, ")")
    return(NA_character_)
  }
  if (length(f) > 1) log_msg("ATTENTION : plusieurs fichiers pour ", cle, ", ", basename(f[1]), " retenu")
  f[1]
}

# Lit une feuille en texte en détectant automatiquement la ligne d'en-tête
# (première ligne dont la 1re colonne contient `mot_cle`).
lire_base <- function(chemin, mot_cle = NULL, colonnes = NULL, entete_auto = FALSE) {
  if (is.na(chemin)) return(NULL)
  skip <- 0
  if (entete_auto && is.null(mot_cle)) {
    # structure inconnue : l'en-tête est la 1re ligne suffisamment remplie
    tete <- read_excel(chemin, col_names = FALSE, n_max = 20, col_types = "text",
                       .name_repair = "minimal")
    rempl <- rowSums(!is.na(tete))
    skip <- which(rempl >= 0.6 * max(rempl))[1] - 1
  }
  if (!is.null(mot_cle)) {
    tete <- read_excel(chemin, col_names = FALSE, n_max = 20, col_types = "text",
                       .name_repair = "minimal")
    ligne <- which(str_detect(str_to_upper(coalesce(tete[[1]], "")),
                              fixed(str_to_upper(mot_cle))))[1]
    if (is.na(ligne)) stop("En-tête introuvable dans ", basename(chemin))
    skip <- ligne - 1
  }
  if (!is.null(colonnes)) {
    df <- read_excel(chemin, range = cell_cols(colonnes), col_types = "text",
                     .name_repair = "minimal")
  } else {
    df <- read_excel(chemin, skip = skip, col_types = "text", .name_repair = "minimal")
  }
  df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
  log_msg("Lu : ", basename(chemin), " -> ", nrow(df), " lignes x ", ncol(df), " colonnes")
  df
}

# Conversion lettre de colonne Excel -> index
col_idx <- function(lettre) {
  v <- utf8ToInt(toupper(lettre)) - 64L
  sum(v * 26^(rev(seq_along(v)) - 1))
}

# Sélectionne des colonnes par lettre Excel (comme dans le plan d'exploitation)
# en vérifiant que l'en-tête correspond bien à ce qui est attendu.
# mapping : list(nouveau_nom = c("LETTRE", "regex_entete_attendu"))
selectionner <- function(df, mapping, nom_base) {
  noms_src <- names(df)
  out <- map_dfc(names(mapping), function(nm) {
    lettre <- mapping[[nm]][1]
    attendu <- mapping[[nm]][2]
    i <- col_idx(lettre)
    if (i > ncol(df)) stop(nom_base, " : colonne ", lettre, " absente")
    entete <- noms_src[i]
    if (!is.na(attendu) &&
        !str_detect(str_to_upper(sans_accents(entete)), regex(attendu, ignore_case = TRUE))) {
      log_msg("ATTENTION ", nom_base, " : colonne ", lettre, " = '", entete,
              "' (attendu : ", attendu, ")")
    }
    tibble(!!nm := df[[i]])
  })
  out
}

sans_accents <- function(x) {
  x <- stringi::stri_trans_general(enc2utf8(as.character(x)), "Latin-ASCII")
  x[is.na(x)] <- ""
  x
}

# -----------------------------------------------------------------------------
# Dates
# -----------------------------------------------------------------------------
# Convertit du texte (ou un numéro de série Excel) en POSIXct UTC.
# `format` : ordre lubridate ("mdy HMS", "dmy", "Ymd HMS", ...).
en_date <- function(x, format) {
  x <- str_trim(as.character(x))
  x[x %in% c("", "NA", "NULL", "-")] <- NA
  res <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
  num <- suppressWarnings(as.numeric(x))
  serie <- !is.na(num) & num > 20000 & num < 80000          # numéro de série Excel
  res[serie] <- as.POSIXct(round((num[serie] - 25569) * 86400),
                           origin = "1970-01-01", tz = "UTC")
  txt <- !serie & !is.na(x)
  if (any(txt)) {
    res[txt] <- suppressWarnings(
      parse_date_time(x[txt], orders = format, tz = "UTC", exact = FALSE, quiet = TRUE))
    # Garde-fou : signale les textes non lus (format de date probablement faux).
    # Une confusion jour/mois sur un jour <= 12 reste indétectable ici.
    echec <- txt & is.na(res)
    if (any(echec)) {
      log_msg("ATTENTION dates (", format, ") : ", sum(echec), " / ", sum(txt),
              " textes non lus, ex. ", paste(head(unique(x[echec]), 3), collapse = " | "))
    }
  }
  res
}

# Heure seule : "23:46:00", "23:46" ou fraction de jour Excel -> secondes
en_secondes <- function(h) {
  h <- str_trim(as.character(h))
  num <- suppressWarnings(as.numeric(h))
  s <- ifelse(!is.na(num) & num >= 0 & num < 1, round(num * 86400), NA_real_)
  p <- str_match(h, "^(\\d{1,2}):(\\d{2})(?::(\\d{2}))?")
  s2 <- as.numeric(p[, 2]) * 3600 + as.numeric(p[, 3]) * 60 + coalesce(as.numeric(p[, 4]), 0)
  coalesce(s, s2)
}

# Date "AAAAMMJJ" + heure "HHMM" (format PAPN)
date_papn <- function(d, h) {
  d <- str_extract(as.character(d), "\\d{8}")
  h <- str_pad(str_extract(as.character(h), "\\d{1,4}"), 4, pad = "0")
  res <- suppressWarnings(ymd_hm(paste(d, h), tz = "UTC", quiet = TRUE))
  res[is.na(d) | is.na(h)] <- NA
  res
}

diff_h <- function(fin, debut) as.numeric(difftime(fin, debut, units = "hours"))

# -----------------------------------------------------------------------------
# Normalisation des clés de rapprochement
# -----------------------------------------------------------------------------
norm_cle <- function(x) {
  x <- str_to_upper(str_replace_all(as.character(x), "[^A-Za-z0-9]", ""))
  x[x == ""] <- NA
  x
}

norm_navire <- function(x) {
  x <- str_to_upper(sans_accents(x))
  x <- str_replace(x, "^[-\\s]+", "")
  x <- str_replace(x, "^M\\s*[/.]?\\s*V\\.?\\s+", "")             # préfixe "M/V", "MV", "M.V."
  x <- str_replace(x, "\\s+(E|V|VOY)\\s*[-:.]?\\s*\\d.*$", "")   # suffixe voyage
  x <- str_replace_all(x, "[^A-Z0-9]", "")
  x[x == ""] <- NA
  x
}

norm_txt <- function(x) {
  x <- str_squish(str_to_upper(as.character(x)))
  x[x == ""] <- NA
  x
}

# Lieux TBC (provenance, destination) : codes PNR, BZV, KIN
norm_lieu <- function(x) {
  x <- str_squish(str_to_upper(sans_accents(x)))
  x[x == ""] <- NA
  case_when(
    is.na(x)                                    ~ NA_character_,
    str_detect(x, "^PNR|^PNT|POINTE")           ~ "PNR",
    str_detect(x, "^BZV|BRAZZA")                ~ "BZV",
    x == "K" | str_detect(x, "^KIN|KINSHASA")   ~ "KIN",
    TRUE                                        ~ x)
}

# Tri numérique des identifiants d'indicateurs ("9bis" placé après "9")
num_id <- function(id) suppressWarnings(as.numeric(str_replace(id, "bis$", ".5")))

# Chapitre et section du Système Harmonisé
sh_section <- function(sh2) {
  ch <- suppressWarnings(as.integer(sh2))
  cut(ch,
      breaks = c(0, 5, 14, 15, 24, 27, 38, 40, 43, 46, 49, 63, 67, 70, 71, 83, 85, 89, 92, 93, 96, 97, 99),
      labels = c("I - Animaux et produits animaux", "II - Produits végétaux",
                 "III - Graisses et huiles", "IV - Industries alimentaires, boissons, tabacs",
                 "V - Produits minéraux", "VI - Produits chimiques",
                 "VII - Plastiques et caoutchouc", "VIII - Peaux et cuirs",
                 "IX - Bois et ouvrages en bois", "X - Papier et carton",
                 "XI - Textiles", "XII - Chaussures, coiffures",
                 "XIII - Pierre, céramique, verre", "XIV - Métaux précieux",
                 "XV - Métaux communs", "XVI - Machines et appareils électriques",
                 "XVII - Matériel de transport", "XVIII - Optique, horlogerie, instruments",
                 "XIX - Armes et munitions", "XX - Marchandises diverses",
                 "XXI - Objets d'art", "Chapitres 98-99 (régimes particuliers)"),
      right = TRUE) |> as.character()
}

# -----------------------------------------------------------------------------
# Rapprochement "au plus proche" sur une clé + une date
# -----------------------------------------------------------------------------
# Pour chaque ligne de `x`, retient la ligne de `y` de même clé dont la date est
# la plus proche (écart absolu <= tol_j), avec contrainte optionnelle de sens :
#  sens = "avant" : date_y <= date_x (+ marge) ; "apres" : date_y >= date_x
lier_proche <- function(x, y, cle, date_x, date_y, tol_j, sens = "tous", marge_j = 0,
                        id_x = "id_x") {
  x2 <- x |> select(all_of(c(id_x, cle, date_x))) |> filter(!is.na(.data[[cle]]), !is.na(.data[[date_x]]))
  y2 <- y |> filter(!is.na(.data[[cle]]), !is.na(.data[[date_y]]))
  j <- inner_join(x2, y2, by = cle, relationship = "many-to-many", suffix = c("", ".y")) |>
    mutate(.ecart = as.numeric(difftime(.data[[date_x]], .data[[date_y]], units = "days")))
  if (sens == "avant") j <- filter(j, .ecart >= -marge_j)
  if (sens == "apres") j <- filter(j, .ecart <= marge_j)
  j |>
    filter(abs(.ecart) <= tol_j) |>
    group_by(.data[[id_x]]) |>
    slice_min(abs(.ecart), n = 1, with_ties = FALSE) |>
    ungroup()
}

# Taux de rapprochement (journalisé et conservé pour l'export)
RAPPROCHEMENTS <- tibble()
noter_lien <- function(libelle, source, cible, cle, regle, n_total, n_lie,
                       indicateurs = "", commentaire = "") {
  RAPPROCHEMENTS <<- bind_rows(RAPPROCHEMENTS, tibble(
    rapprochement = libelle, base_source = source, base_cible = cible,
    cle = cle, regle = regle,
    n_lignes_source = n_total, n_rapprochees = n_lie, n_perdues = n_total - n_lie,
    taux = ifelse(n_total > 0, n_lie / n_total, NA_real_),
    indicateurs_concernes = indicateurs, commentaire = commentaire))
  log_msg("Lien ", libelle, " : ", n_lie, " / ", n_total,
          " (", round(100 * n_lie / max(n_total, 1)), " %)")
}

# -----------------------------------------------------------------------------
# Taux de remplissage d'une base
# -----------------------------------------------------------------------------
remplissage <- function(df, nom_base) {
  if (is.null(df)) return(tibble())
  tibble(base = nom_base, colonne = names(df),
         n_lignes = nrow(df),
         taux_rempli = map_dbl(df, ~ mean(!is.na(.x))))
}

# -----------------------------------------------------------------------------
# Statistiques descriptives d'un délai
# -----------------------------------------------------------------------------
stats_delai <- function(v) {
  tibble(
    n        = length(v),
    moyenne  = if (length(v)) mean(v) else NA_real_,
    mediane  = if (length(v)) median(v) else NA_real_,
    p10      = if (length(v)) unname(quantile(v, 0.10)) else NA_real_,
    p25      = if (length(v)) unname(quantile(v, 0.25)) else NA_real_,
    p75      = if (length(v)) unname(quantile(v, 0.75)) else NA_real_,
    p90      = if (length(v)) unname(quantile(v, 0.90)) else NA_real_,
    min      = if (length(v)) min(v) else NA_real_,
    max      = if (length(v)) max(v) else NA_real_
  )
}

# min / max tolérants aux groupes entièrement vides (conservent la classe date)
min_na <- function(x) if (all(is.na(x))) x[NA_integer_][1] else min(x, na.rm = TRUE)
max_na <- function(x) if (all(is.na(x))) x[NA_integer_][1] else max(x, na.rm = TRUE)

# Écart en jours calendaires (exprimé en heures pour homogénéité) :
# utilisé quand l'une des deux dates n'a pas d'heure (ex. date d'enregistrement
# du DAU, apurement du manifeste, dates TBC), pour éviter des délais négatifs
# artificiels quand les deux événements ont lieu le même jour.
diff_cal_h <- function(fin, debut) as.numeric(as.Date(fin) - as.Date(debut)) * 24

# Sélection par lettres issues de la configuration (colonnes NA ignorées)
selectionner_config <- function(df, lettres, nom_base) {
  lettres <- lettres[!is.na(unlist(lettres))]
  selectionner(df, map(lettres, ~ c(.x, NA)), nom_base)
}

# Arrêt explicite si une base indispensable est vide (fichier non trouvé ou illisible)
exiger <- function(df, nom) {
  if (is.null(df) || nrow(df) == 0) {
    stop(nom, " est vide : fichier(s) non trouvé(s) ou illisible(s). ",
         "Vérifier les lignes 'ABSENTE' du journal et les noms de fichiers dans ",
         normalizePath(CONFIG$dir_data), " (motifs dans CONFIG$motifs).")
  }
  invisible(df)
}
