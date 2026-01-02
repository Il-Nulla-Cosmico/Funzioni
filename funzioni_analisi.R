# mie_analisi_funzioni.R

carica_e_prepara_dati <- function(file, sheet, col_y, col_tr, col_plot, col_d, col_t, col_unit) {
  raw <- read_excel(file, sheet = sheet)
  names(raw) <- trimws(names(raw))
  
  # Controllo colonne
  needed <- c(col_y, col_tr, col_plot, col_d, col_t, col_unit)
  missing <- setdiff(needed, names(raw))
  if (length(missing) > 0) stop(paste("Mancano queste colonne:", paste(missing, collapse = ", ")))
  
  dat <- raw %>%
    transmute(
      TR   = factor(trimws(as.character(.data[[col_tr]]))),
      PLOT = factor(trimws(as.character(.data[[col_plot]]))),
      D    = factor(.data[[col_d]]),
      T    = factor(.data[[col_t]]),
      UNIT = factor(.data[[col_unit]]),
      Y    = suppressWarnings(as.numeric(.data[[col_y]]))
    ) %>%
    filter(!is.na(Y))
  
  if (any(dat$Y < 0)) stop("La risposta contiene valori negativi!")
  
  cat("\n--- Struttura dati ---\n")
  print(str(dat))
  return(dat)
}

confronta_modelli <- function(dat_fit) {
  # Poisson
  m_pois <- glmer(Y ~ TR * D + T + (1 | PLOT), data = dat_fit, family = poisson)
  cat("\n================ Poisson GLMM ================\n")
  print(summary(m_pois))
  print(check_overdispersion(m_pois))
  
  # Negative Binomial
  m_nb <- glmer.nb(Y ~ TR * D + T + (1 | PLOT), data = dat_fit)
  cat("\n================ NB GLMM ================\n")
  print(summary(m_nb))
  print(check_overdispersion(m_nb))
  
  return(m_nb) # Restituiamo il NB che è quello che usi per i plot
}

genera_report_grafico <- function(m_nb) {
  # TR Plot
  emm_TR <- emmeans(m_nb, ~ TR, type = "response")
  p1 <- ggplot(as.data.frame(emm_TR), aes(x = TR, y = response)) +
    geom_point(size = 3) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
    theme_bw() + labs(title = "Plot TR")
  
  # T Plot
  emm_T <- emmeans(m_nb, ~ T, type = "response")
  p2 <- ggplot(as.data.frame(emm_T), aes(x = T, y = response, group = 1)) +
    geom_point(size = 3) + geom_line() +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.15) +
    theme_bw() + labs(title = "Plot Tempo")
  
  # T x TR Plot
  emm_TR_T <- emmeans(m_nb, ~ TR | T, type = "response")
  df_TT <- as.data.frame(emm_TR_T)
  df_TT$T_num <- suppressWarnings(as.numeric(as.character(df_TT$T)))
  
  p3 <- ggplot(df_TT, aes(x = T_num, y = response, color = TR, group = TR)) +
    geom_line(linewidth = 1.1) + geom_point(size = 2) +
    theme_bw() + labs(title = "Interazione T x TR")
  
  # Mostra i grafici
  print(p1)
  print(p2)
  print(p3)
}