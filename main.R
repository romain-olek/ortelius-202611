# =============================================================================
# PAPN – Plan d'exploitation des données 2024 / 2025
# main.R : exécute l'ensemble de l'analyse
# -----------------------------------------------------------------------------
# Usage : placer les bases Excel dans le dossier data/, puis
#   - dans RStudio : ouvrir le projet et exécuter source("main.R")
#   - en ligne de commande : Rscript main.R
# Résultats : dossier outputs/
# =============================================================================

# Paquets nécessaires (installation automatique si absents)
paquets <- c("readxl", "dplyr", "tidyr", "lubridate", "stringr", "stringi",
             "purrr", "openxlsx", "readr", "ggplot2")
manquants <- paquets[!vapply(paquets, requireNamespace, logical(1), quietly = TRUE)]
if (length(manquants) > 0) install.packages(manquants)

# Encodage : les scripts sont en UTF-8.
# Si la session est déjà en UTF-8 (cas habituel sur macOS et Linux), on ne
# touche à rien. Sinon on essaie les locales UTF-8 courantes, celles-ci
# variant selon le système (C.UTF-8 n'existe pas sur macOS).
if (.Platform$OS.type != "windows" &&
    !grepl("UTF-8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE)) {
  for (loc in c("C.UTF-8", "en_US.UTF-8", "fr_FR.UTF-8")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
  }
}
executer <- function(f) {
  message("\n==== ", f, " ====")
  source(file.path("R", f), encoding = "UTF-8")
}

t0 <- Sys.time()
executer("00_config.R")
executer("01_fonctions.R")
executer("02_import_preparation.R")
executer("03_rapprochements.R")
executer("04_indicateurs.R")
executer("05_syntheses.R")
executer("06_export.R")
executer("07_graphiques.R")
log_msg("Analyse terminée en ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " minutes")
