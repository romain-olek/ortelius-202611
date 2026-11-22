# =============================================================================
# 07_graphiques.R : graphiques de distribution, exportés en PDF
# -----------------------------------------------------------------------------
# Produit :
#   - outputs/PAPN_graphiques_indicateurs.pdf : un document, une page par
#     indicateur (histogramme, boîte à moustaches par année, évolution
#     mensuelle de la médiane) ;
#   - outputs/graphiques/indicateur_X.pdf : le même graphique, un fichier par
#     indicateur, pour insertion directe dans un rapport ;
#   - outputs/graphiques/indicateur_X.png : version image, intégrée au
#     classeur Excel (onglet Graphiques).
# Les PDF sont vectoriels : ils restent nets à n'importe quelle échelle.
# =============================================================================


suppressPackageStartupMessages(library(ggplot2))

dir_graph <- file.path(CONFIG$dir_out, "graphiques")
dir.create(dir_graph, showWarnings = FALSE, recursive = TRUE)

# --- Palette PAPN -------------------------------------------------------------
# Orange pour 2025 (année mise en avant), bleu ardoise pour 2024 (référence),
# taupe pour les textes secondaires.
# Règle commune aux trois graphiques, pour que chaque année ait la même teinte :
#   - surfaces (barres, boîtes) : couleur de l'année avec la transparence alpha_fill ;
#   - traits et points (médianes, courbes) : couleur de l'année, pleine.
# Les variantes foncées sont réservées aux textes et contours neutres.
pal <- list(
  orange        = "#E8622A",
  orange_fonce  = "#B54C21",
  ardoise       = "#3E5C6B",
  ardoise_fonce = "#2A3F49",   # titres
  taupe         = "#A99D9F",
  taupe_fonce   = "#4A4143",
  taupe_clair   = "#E6E2E2"   # grille
)

couleurs   <- c("2024" = pal$taupe, "2025" = pal$orange)
alpha_fill <- 0.65

theme_papn <- theme_minimal(base_size = 9, base_family = "sans") +
  theme(plot.title    = element_text(face = "bold", size = 11, colour = pal$ardoise_fonce),
        plot.subtitle = element_text(size = 8.5, colour = pal$taupe),
        plot.caption  = element_text(size = 7.5, colour = pal$taupe, hjust = 0),
        axis.title    = element_text(colour = pal$taupe_fonce),
        axis.text     = element_text(colour = pal$taupe),
        panel.grid.major = element_line(colour = pal$taupe_clair, linewidth = 0.3),
        panel.grid.minor = element_blank(),
        legend.text   = element_text(colour = pal$taupe_fonce),
        legend.position = "top", legend.title = element_blank())

# Les distributions sont très étirées : l'axe est tronqué au P95 pour rester
# lisible, la part des observations au-delà est indiquée dans la légende.
graphiques_indicateur <- function(i) {
  d <- obs |> filter(id == i) |> mutate(annee = factor(annee))
  if (nrow(d) < 10) return(NULL)
  info <- REF |> filter(id == i)
  u <- info$unite
  haut <- unname(quantile(d$delai, 0.95, na.rm = TRUE))
  bas <- min(0, unname(quantile(d$delai, 0.01, na.rm = TRUE)))
  part_hors <- mean(d$delai > haut)
  
  st <- SYNTHESE |> filter(id == i) |>
    mutate(txt = paste0(annee, " : n = ", format(n_retenu, big.mark = " "),
                        ", médiane ", round(mediane, 1), " ", u,
                        ", moyenne ", round(moyenne, 1),
                        ", P90 ", round(p90, 1))) |> pull(txt)
  ctrl <- SYNTHESE |> filter(id == i) |>
    mutate(txt = paste0(annee, " : ", n_exclus_negatif, " négatifs et ",
                        n_exclus_extreme, " valeurs extrêmes exclus")) |> pull(txt)
  
  # les classes sont calculées sur la fenêtre affichée, pas sur toute l'étendue
  d_vue <- d |> filter(delai >= bas, delai <= haut)
  med <- d |> group_by(annee) |> summarise(m = median(delai))
  g1 <- ggplot(d_vue, aes(delai, fill = annee)) +
    geom_histogram(bins = 40, position = "identity", alpha = alpha_fill, colour = NA) +
    geom_vline(data = med, aes(xintercept = m, colour = annee), linetype = "dashed", linewidth = 0.8) +
    scale_fill_manual(values = couleurs) +
    scale_colour_manual(values = couleurs, guide = "none") +
    coord_cartesian(xlim = c(bas, haut)) +
    labs(title = paste0("Indicateur ", i, " - ", info$libelle),
         subtitle = paste(st, collapse = "   |   "),
         x = paste0("Délai (", u, ") - traits pointillés : médianes"), y = "Effectif") +
    theme_papn
  
  g2 <- ggplot(d, aes(annee, delai, fill = annee)) +
    geom_boxplot(alpha = alpha_fill, colour = pal$taupe_fonce, outlier.colour = pal$taupe,
                 outlier.size = 0.4, outlier.alpha = 0.35, width = 0.5) +
    scale_fill_manual(values = couleurs, guide = "none") +
    coord_cartesian(ylim = c(bas, haut)) +
    labs(x = NULL, y = paste0("Délai (", u, ")"),
         subtitle = "Dispersion par année (boîte = quartiles, trait = médiane)") +
    theme_papn
  
  m <- MENSUEL |> filter(id == i) |> mutate(annee = factor(annee))
  g3 <- ggplot(m, aes(mois, mediane, colour = annee, group = annee)) +
    geom_line(linewidth = 0.6) + geom_point(size = 1.1) +
    scale_colour_manual(values = couleurs) +
    labs(x = NULL, y = paste0("Médiane (", u, ")"),
         subtitle = "Évolution mensuelle de la médiane",
         caption = paste0("Unité d'observation : ", info$unite_obs,
                          ". Axe tronqué au P95 (", round(100 * part_hors, 1),
                          " % des observations au-delà). ",
                          paste(ctrl, collapse = " ; "), ".\n",
                          "Source : ", info$sources, ". Formule : ", info$formule, ".")) +
    theme_papn
  
  list(g1 = g1, g2 = g2, g3 = g3)
}

# Mise en page : histogramme en haut, boîte et évolution mensuelle en bas
dessiner_page <- function(g) {
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2, heights = c(1.15, 1))))
  vp <- function(r, c) grid::viewport(layout.pos.row = r, layout.pos.col = c)
  print(g$g1, vp = vp(1, 1:2))
  print(g$g2, vp = vp(2, 1))
  print(g$g3, vp = vp(2, 2))
}

ids_graph <- SYNTHESE |> filter(n_retenu >= 10) |> distinct(id) |> pull(id)
ids_graph <- ids_graph[order(num_id(ids_graph))]

fichier_pdf <- file.path(CONFIG$dir_out, "PAPN_graphiques_indicateurs.pdf")
pdf(fichier_pdf, width = 11.7, height = 8.3, onefile = TRUE)   # A4 paysage
walk(ids_graph, function(i) {
  g <- graphiques_indicateur(i)
  if (!is.null(g)) dessiner_page(g)
})
invisible(dev.off())
log_msg("Graphiques : ", fichier_pdf, " (", length(ids_graph), " pages)")

# Un PDF et un PNG par indicateur
walk(ids_graph, function(i) {
  g <- graphiques_indicateur(i)
  if (is.null(g)) return(invisible())
  pdf(file.path(dir_graph, paste0("indicateur_", i, ".pdf")), width = 11.7, height = 8.3)
  dessiner_page(g); invisible(dev.off())
  png(file.path(dir_graph, paste0("indicateur_", i, ".png")), width = 2000, height = 1400, res = 180)
  dessiner_page(g); invisible(dev.off())
})
log_msg("Graphiques par indicateur : ", dir_graph)

# --- Insertion des images dans le classeur Excel ------------------------------
wb2 <- loadWorkbook(fichier_xlsx)
addWorksheet(wb2, "Graphiques", gridLines = FALSE)
writeData(wb2, "Graphiques", "Distribution des indicateurs", startRow = 1)
addStyle(wb2, "Graphiques", st_titre, rows = 1, cols = 1)
writeData(wb2, "Graphiques",
          paste0("Version vectorielle, prête à imprimer ou à insérer dans un rapport : ",
                 basename(fichier_pdf), " (un indicateur par page) et dossier graphiques/ ",
                 "(un PDF et un PNG par indicateur)."), startRow = 2)
addStyle(wb2, "Graphiques", st_note, rows = 2, cols = 1)
ligne <- 4
walk(ids_graph, function(i) {
  f <- file.path(dir_graph, paste0("indicateur_", i, ".png"))
  if (!file.exists(f)) return(invisible())
  insertImage(wb2, "Graphiques", f, startRow = ligne, startCol = 1,
              width = 11.7, height = 8.2, units = "in")
  ligne <<- ligne + 42
})
saveWorkbook(wb2, fichier_xlsx, overwrite = TRUE)
log_msg("Onglet Graphiques ajouté au classeur")