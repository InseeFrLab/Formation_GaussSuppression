stats_secret_GS <- function(masque,str_var_rep,format = c("none", "nb", "val", "both"),
                            digits = 2) {
  
  format <- match.arg(format)
  
  # Totaux
  nb_cell  <- nrow(masque)
  val_cell <- sum(masque[[str_var_rep]], na.rm = TRUE)
  
  # SP
  idx_SP <- masque$primary
  nb_cell_SP  <- sum(idx_SP)
  val_cell_SP <- sum(masque[[str_var_rep]][idx_SP], na.rm = TRUE)
  
  # SSD
  idx_SSD <- !masque$primary & masque$suppressed
  nb_cell_SSD  <- sum(idx_SSD)
  val_cell_SSD <- sum(masque[[str_var_rep]][idx_SSD], na.rm = TRUE)
  
  # Diffusibles
  nb_cell_diff  <- nb_cell - nb_cell_SP - nb_cell_SSD
  val_cell_diff <- val_cell - val_cell_SP - val_cell_SSD
  
  # Pourcentages (arrondis à 2 décimales)
  pourc_nb_SP   <- round(nb_cell_SP / nb_cell * 100, 2)
  pourc_nb_SSD  <- round(nb_cell_SSD / nb_cell * 100, 2)
  pourc_nb_diff <- round(100 - pourc_nb_SP - pourc_nb_SSD, 2)
  
  pourc_val_SP   <- round(val_cell_SP / val_cell * 100, 2)
  pourc_val_SSD  <- round(val_cell_SSD / val_cell * 100, 2)
  pourc_val_diff <- round(100 - pourc_val_SP - pourc_val_SSD, 2)
  
  res <- tibble::tibble(
    secret = c("SP", "SSD", "Diffusibles", "Total"),
    nb = c(nb_cell_SP, nb_cell_SSD, nb_cell_diff, nb_cell),
    pourc_nb = c(pourc_nb_SP, pourc_nb_SSD, pourc_nb_diff, 100),
    val = c(val_cell_SP, val_cell_SSD, val_cell_diff, val_cell),
    pourc_val = c(pourc_val_SP, pourc_val_SSD, pourc_val_diff, 100)
  )
  
  # Mise en forme optionnelle en écriture scientifique
  if (format %in% c("nb", "both")) {
    res$nb <- formatC(signif(res$nb, digits), format = "e", digits = digits)
  }
  
  if (format %in% c("val", "both")) {
    res$val <- formatC(signif(res$val, digits), format = "e", digits = digits)
  }
  
  res
}
