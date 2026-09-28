# =============================================================================
# Script 04 — Intergovernmental Conflict Index (ICI) Construction via PCA
# =============================================================================
# Position in pipeline: 4 of N
# Purpose: Merges the political/rhetoric panel (Script 03) with the fiscal
#   panel (Script 02), computes lagged fiscal change variables, and
#   constructs the Intergovernmental Conflict Index (ICI) via variance-
#   weighted Principal Component Analysis on three standardized indicators:
#   log rhetoric intensity (Level 1 proxy), constitutional controversy
#   counts (Levels 2-3 proxy), and Federalist Alliance rupture (Level 3
#   proxy). Also constructs a two-component (PC1+PC2 only) alternative
#   index as a robustness check, and generates the PCA biplot and
#   component-contribution table/figures reported in the paper's appendix.
# Input:   Base_Final_Publius_Definitiva.csv (Script 03 output);
#   fiscales_estados_long.csv (Script 02 output)
# Output:  Base_Tesis_MASTER_Weighted_PCA.csv (master analysis panel,
#   includes both the three-component ICI [Indice_Conflictividad_W] and
#   the two-component robustness index [Indice_Conflictividad_2PC]);
#   Figura1_PCA_Biplot.pdf; Figura2_Contribucion_PC1.png;
#   Figura3_Contribucion_PC2.png; Anexo_PCA_Contribuciones.csv
# =============================================================================

library(tidyverse)
library(factoextra)
if (!exists("path_raw")) source(file.path("R", "paths.R"))


# -----------------------------------------------------------------------------
# 1. Load and merge the political and fiscal panels
# -----------------------------------------------------------------------------
# Reads Script 03's political/rhetoric panel and Script 02's fiscal panel,
# joins them on state-year, and computes lagged percentage changes in the
# three transfer categories (zero-shielded via a +1 offset in the
# denominator) plus the log-transformed rhetoric intensity used in the PCA
# below. Rows with non-finite changes or missing PCA inputs are dropped.

print("--- 1. Loading and merging panels ---")

df_politica <- readr::read_csv(path_data("Base_Final_Publius_Definitiva.csv"), show_col_types = FALSE)
df_fiscal   <- readr::read_csv(path_data("fiscales_estados_long.csv"), show_col_types = FALSE)

# FIX (R2): the political panel starts in 1998, so a left_join left the first
# panel year without a fiscal lag, and the is.finite() filter below silently
# dropped all 32 state-years of 1998 from the PCA sample. We now full_join so
# that pre-1998 fiscal observations are present when the lags are computed,
# then restore the political panel's 1998-2024 window afterwards. This
# recovers the 32 lost rows with genuine (rather than zero-filled) lags.
.anio_min_politica <- min(df_politica$ANIO, na.rm = TRUE)

df_merged <- df_politica %>%
  dplyr::full_join(df_fiscal, by = c("ENTIDAD", "ANIO"))

df_fiscal_calc <- df_merged %>%
  arrange(ENTIDAD, ANIO) %>%
  group_by(ENTIDAD) %>%
  mutate(
    # dplyr::lag is qualified deliberately. A bare lag() resolves to stats::lag
    # whenever plm is attached (plm masks dplyr's lag, which in turn masks
    # stats'), and stats::lag applied to a plain numeric vector returns the
    # vector unchanged with a time-series attribute rather than a shifted
    # vector -- so x - lag(x) silently evaluates to 0 for every row. That
    # produces all-zero change variables with no error and no warning, and it
    # happens whenever this script is run in a session where Script 05 or 06
    # has already been sourced. Script 05 already qualifies every call for the
    # same reason.
    Cambio_Ramo28_Pct = (Ramo_28 - dplyr::lag(Ramo_28)) / (dplyr::lag(Ramo_28) + 1),
    Cambio_Ramo33_Pct = (Ramo_33 - dplyr::lag(Ramo_33)) / (dplyr::lag(Ramo_33) + 1),
    Cambio_Convenios_Pct = (Convenios - dplyr::lag(Convenios)) / (dplyr::lag(Convenios) + 1),
    log_Rhetoric = log(Rhetoric_Intensity + 1)
  ) %>%
  ungroup() %>%
  filter(ANIO >= .anio_min_politica)

df_analisis <- df_fiscal_calc %>%
  filter(
    is.finite(Cambio_Ramo28_Pct),
    is.finite(Cambio_Ramo33_Pct),
    is.finite(Cambio_Convenios_Pct),
    !is.na(log_Rhetoric),
    !is.na(n_conflicts),
    !is.na(Rupture_Alliance_Period)
  )

# -----------------------------------------------------------------------------
# 2. Variance-weighted PCA index (three components)
# -----------------------------------------------------------------------------
# Runs PCA on the three standardized conflict indicators and constructs the
# ICI as the sum of all three principal component scores, each weighted by
# its share of total explained variance -- so no variance is discarded and
# no arbitrary component-retention rule is required. Component signs are
# oriented so that higher values consistently indicate more conflict,
# anchored to the loading direction of a theoretically central variable in
# each component (rhetoric for PC1, litigation count for PC2, net loading
# direction for PC3). The raw index is min-max rescaled to [0,1].

print("--- 2. Constructing variance-weighted PCA index ---")

datos_pca <- df_analisis %>% select(log_Rhetoric, n_conflicts, Rupture_Alliance_Period)
pca_model <- prcomp(datos_pca, scale. = TRUE, center = TRUE)
summary(pca_model)

varianzas <- get_eigenvalue(pca_model)$variance.percent / 100
w1 <- varianzas[1]
w2 <- varianzas[2]
w3 <- varianzas[3]

print(paste("Component weights: PC1:", round(w1, 3), "| PC2:", round(w2, 3), "| PC3:", round(w3, 3)))

signo_pc1 <- if_else(pca_model$rotation["log_Rhetoric", "PC1"] < 0, -1, 1)
signo_pc2 <- if_else(pca_model$rotation["n_conflicts", "PC2"] < 0, -1, 1)
carga_neta_pc3 <- sum(pca_model$rotation[, "PC3"])
signo_pc3 <- if_else(carga_neta_pc3 < 0, -1, 1)

df_analisis <- df_analisis %>%
  mutate(
    Score_PC1 = pca_model$x[, "PC1"] * signo_pc1,
    Score_PC2 = pca_model$x[, "PC2"] * signo_pc2,
    Score_PC3 = pca_model$x[, "PC3"] * signo_pc3,
    Indice_Weighted_Raw = (Score_PC1 * w1) + (Score_PC2 * w2) + (Score_PC3 * w3)
  )

df_final <- df_analisis %>%
  mutate(
    Indice_Conflictividad_W = (Indice_Weighted_Raw - min(Indice_Weighted_Raw)) /
      (max(Indice_Weighted_Raw) - min(Indice_Weighted_Raw))
  )

# -----------------------------------------------------------------------------
# 2b. Two-component alternative index (robustness check)
# -----------------------------------------------------------------------------
# Constructs a second index using only PC1 and PC2 (renormalized weights),
# dropping the atheoretical third component, as a robustness check on
# whether PC3 carries meaningful signal or is safely discarded. Reported in
# the paper's Quantitative Evidence section and online appendix.

print("--- 2b. Constructing two-component robustness index ---")

w1_2comp <- w1 / (w1 + w2)
w2_2comp <- w2 / (w1 + w2)

print(paste("Renormalized weights (two components only): PC1:", round(w1_2comp, 3),
            "| PC2:", round(w2_2comp, 3)))

df_final <- df_final %>%
  mutate(
    Indice_Weighted_Raw_2PC = (Score_PC1 * w1_2comp) + (Score_PC2 * w2_2comp),
    Indice_Conflictividad_2PC = (Indice_Weighted_Raw_2PC - min(Indice_Weighted_Raw_2PC)) /
      (max(Indice_Weighted_Raw_2PC) - min(Indice_Weighted_Raw_2PC))
  )

cor_3v2 <- cor(df_final$Indice_Conflictividad_W, df_final$Indice_Conflictividad_2PC)
print(paste("Correlation between three-component and two-component ICI:", round(cor_3v2, 4)))

write_csv(df_final, path_data("Base_Tesis_MASTER_Weighted_PCA.csv"))

# -----------------------------------------------------------------------------
# 3. Diagnostics: fiscal correlates and top conflict state-years
# -----------------------------------------------------------------------------

print("--- 3. Diagnostics: which fiscal variable moves the weighted index? ---")

cor_28   <- cor(df_final$Cambio_Ramo28_Pct, df_final$Indice_Conflictividad_W)
cor_33   <- cor(df_final$Cambio_Ramo33_Pct, df_final$Indice_Conflictividad_W)
cor_conv <- cor(df_final$Cambio_Convenios_Pct, df_final$Indice_Conflictividad_W)

print(paste("Correlation, Ramo 28 (discretionary):", round(cor_28, 4)))
print(paste("Correlation, Ramo 33 (earmarked):", round(cor_33, 4)))
print(paste("Correlation, Convenios (agreements):", round(cor_conv, 4)))

print("--- Top 5 most conflictive state-years (weighted index) ---")
df_final %>%
  arrange(desc(Indice_Conflictividad_W)) %>%
  select(ENTIDAD, ANIO, Indice_Conflictividad_W, n_conflicts, Rhetoric_Intensity) %>%
  head(5) %>%
  print()

# -----------------------------------------------------------------------------
# 4. Visualization: PCA loadings biplot (paper Figure)
# -----------------------------------------------------------------------------
# Correlation-circle plot of variable loadings on the first two dimensions,
# colored by contribution.

print("--- 4. Generating PCA loadings biplot ---")

plot_pca <- fviz_pca_var(pca_model,
                         col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = FALSE,
                         ggtheme = theme_minimal()
) +
  labs(x = paste0("First Dimension (", round(w1 * 100, 1), "% Variance)"),
       y = paste0("Second Dimension (", round(w2 * 100, 1), "% Variance)"))

plot_pca_2 <- fviz_pca_var(pca_model,
                         col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = FALSE,
                         ggtheme = theme_minimal(),
                         axes = c(2,3)
) +
  labs(x = paste0("Second Dimension (", round(w1 * 100, 1), "% Variance)"),
       y = paste0("Third Dimension (", round(w2 * 100, 1), "% Variance)"))

plot_pca_3 <- fviz_pca_var(pca_model,
                           col.var = "contrib",
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = FALSE,
                           ggtheme = theme_minimal(),
                           axes = c(1,3)
) +
  labs(x = paste0("First Dimension (", round(w1 * 100, 1), "% Variance)"),
       y = paste0("Third Dimension (", round(w2 * 100, 1), "% Variance)"))

print(plot_pca)
print(plot_pca_2)
print(plot_pca_3)
ggsave(path_out("Figura1_PCA_Biplot.pdf"), plot = plot_pca, width = 6, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 5. Component contribution table and figures (paper appendix)
# -----------------------------------------------------------------------------
# Extracts each variable's percentage contribution to each principal
# component and exports both the table and per-dimension bar plots.

print("--- 5. Extracting variable contributions ---")

var_info <- get_pca_var(pca_model)
tabla_contribuciones <- as.data.frame(var_info$contrib)
print("--- Variable contribution table (%) ---")
print(tabla_contribuciones)

write_csv(rownames_to_column(tabla_contribuciones, var = "Variable"), path_data("Anexo_PCA_Contribuciones.csv"))

plot_c1 <- fviz_contrib(pca_model, choice = "var", axes = 1, fill = "#FC4E07", color = "black") +
  labs(title = "Variable Contribution to Dimension 1")

plot_c2 <- fviz_contrib(pca_model, choice = "var", axes = 2, fill = "#00AFBB", color = "black") +
  labs(title = "Variable Contribution to Dimension 2")

print(plot_c1)
print(plot_c2)

ggsave(path_out("Figura2_Contribucion_PC1.png"), plot = plot_c1, width = 5, height = 4)
ggsave(path_out("Figura3_Contribucion_PC2.png"), plot = plot_c2, width = 5, height = 4)

print("Contribution tables and figures generated.")