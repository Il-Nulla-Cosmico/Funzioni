setwd("C:/Users/carme/OneDrive/Desktop/R + Analisi Statistica/Funzioni")

library(readxl)
library(dplyr)
library(lme4)
library(MASS)
library(emmeans)
library(ggplot2)
library(performance)


# Carico le funzioni che abbiamo scritto sopra
source("funzioni_analisi.R")

# --- INPUT ---
FILE_PATH <- "C:/Users/carme/OneDrive/Desktop/dataset.xlsx"

# --- ESECUZIONE ---
# 1. Carico e pulisco (tutto il check NA e colonne è dentro la funzione)
dati <- carica_e_prepara_dati(FILE_PATH, sheet=1, 
                              col_y="Anelli_Necr", 
                              col_tr="Trattamento", 
                              col_plot="Parcella_ID", 
                              col_d="Distanza", 
                              col_t="Tempo", 
                              col_unit="Pianta")

# 2. Lancio i modelli e guardo i risultati (overdispersion inclusa)
modello_migliore <- confronta_modelli(dati)

# 3. Creo tutti i grafici in un colpo solo
genera_report_grafico(modello_migliore)