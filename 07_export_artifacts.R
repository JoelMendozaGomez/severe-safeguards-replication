# =============================================================================
# Script 07 — Replication Artifact Export and Verification
# =============================================================================
# Position in pipeline: 7 of 7 (run last, in the same session as Scripts 01-06)
# Purpose: Exports every intermediate quantity needed to reproduce the
#   Intergovernmental Conflict Index (ICI) from its constituent indicators,
#   and verifies that the pipeline reproduces the figures reported in the
#   published article. Written in response to Reviewer 3's request that the
#   archive allow a reader to "reproduce the ICI from the underlying
#   indicators through the final state-year index and then reproduce the
#   reported regressions."
#
# Requires (in memory, from the preceding scripts):
#   pca_model, datos_pca, varianzas, signo_pc1/2/3, df_final   [Script 04]
#   df_clean, m_nivel_TWFE, sens_analysis                      [Script 05]
#
# Output:  ici_state_year_scores.csv        state-year index + raw indicators
#          pca_loadings.csv                rotation matrix (raw and sign-oriented)
#          pca_eigenvalues.csv             eigenvalues, variance shares, ICI weights
#          pca_standardization.csv         centering and scaling constants
#          pca_contributions.csv           variable contributions by component
#          VERIFICATION.txt                published figures vs. reproduced
# =============================================================================

library(tidyverse)
if (!exists("path_raw")) source(file.path("R", "paths.R"))


print("--- 7. Exporting replication artifacts ---")

# -----------------------------------------------------------------------------
# 7.1 Standardization constants
# -----------------------------------------------------------------------------
# prcomp(scale. = TRUE, center = TRUE) standardizes each indicator as
# z = (x - center) / scale, where center is the sample mean and scale the
# sample standard deviation (n-1 denominator). Exported so a reader can
# reproduce the standardized inputs exactly.
pca_standardization <- tibble(
  Variable = names(pca_model$center),
  Center_Mean = as.numeric(pca_model$center),
  Scale_SD    = as.numeric(pca_model$scale)
)
write_csv(pca_standardization, path_data("pca_standardization.csv"))
print(pca_standardization)

# -----------------------------------------------------------------------------
# 7.2 Loadings (rotation matrix), raw and sign-oriented
# -----------------------------------------------------------------------------
# PCA component signs are arbitrary. Script 04 orients each component so that
# higher values indicate more conflict, anchored on a theoretically central
# variable (rhetoric for PC1, litigation for PC2, net loading for PC3). Both
# the raw rotation matrix and the oriented version are exported, since the
# oriented loadings are what the paper's text describes.
signos <- c(PC1 = signo_pc1, PC2 = signo_pc2, PC3 = signo_pc3)

pca_loadings <- as.data.frame(pca_model$rotation) %>%
  rownames_to_column("Variable") %>%
  rename(PC1_raw = PC1, PC2_raw = PC2, PC3_raw = PC3) %>%
  mutate(
    PC1_oriented = PC1_raw * signos[["PC1"]],
    PC2_oriented = PC2_raw * signos[["PC2"]],
    PC3_oriented = PC3_raw * signos[["PC3"]]
  )
write_csv(pca_loadings, path_data("pca_loadings.csv"))
print(pca_loadings)
cat("\nSign orientation applied (PC1, PC2, PC3):", signos, "\n\n")

# -----------------------------------------------------------------------------
# 7.3 Eigenvalues, variance shares, and the ICI weights
# -----------------------------------------------------------------------------
eigvals <- pca_model$sdev^2
pca_eigenvalues <- tibble(
  Component = paste0("PC", seq_along(eigvals)),
  Eigenvalue = eigvals,
  Variance_Share = eigvals / sum(eigvals),
  Variance_Percent = 100 * eigvals / sum(eigvals),
  Cumulative_Percent = cumsum(100 * eigvals / sum(eigvals)),
  ICI_Weight = eigvals / sum(eigvals)   # w_k in the ICI formula
)
write_csv(pca_eigenvalues, path_data("pca_eigenvalues.csv"))
print(pca_eigenvalues)

# -----------------------------------------------------------------------------
# 7.4 Variable contributions
# -----------------------------------------------------------------------------
# Contribution of variable j to component k, in percent: 100 * loading_jk^2,
# since prcomp returns unit-norm rotation vectors.
pca_contributions <- as.data.frame(100 * pca_model$rotation^2) %>%
  rownames_to_column("Variable")
write_csv(pca_contributions, path_data("pca_contributions.csv"))
print(pca_contributions)

# -----------------------------------------------------------------------------
# 7.5 State-year ICI scores
# -----------------------------------------------------------------------------
# The full state-year panel of the dependent variable, alongside the three
# raw indicators and the intermediate component scores, so that the index can
# be checked at every step: raw indicator -> standardized -> component score
# -> variance-weighted sum -> min-max rescaled ICI.
ici_state_year <- df_final %>%
  transmute(
    ENTIDAD, ANIO,
    Rhetoric_Intensity,                 # raw Google Trends annual mean
    log_Rhetoric,                       # log(Rhetoric_Intensity + 1)
    n_conflicts,                        # constitutional controversies filed
    Rupture_Alliance_Period,            # Federalist Alliance membership, 2020-21
    Score_PC1, Score_PC2, Score_PC3,    # sign-oriented component scores
    Indice_Weighted_Raw,                # sum_k w_k * PC_k, before rescaling
    ICI = Indice_Conflictividad_W,      # min-max rescaled to [0,1]
    ICI_2PC = Indice_Conflictividad_2PC # PC1+PC2 only (robustness index)
  ) %>%
  arrange(ENTIDAD, ANIO)

write_csv(ici_state_year, path_data("ici_state_year_scores.csv"))
cat(sprintf("\nici_state_year_scores.csv: %d rows, %d states, %d-%d, ICI range [%.3f, %.3f]\n",
            nrow(ici_state_year), n_distinct(ici_state_year$ENTIDAD),
            min(ici_state_year$ANIO), max(ici_state_year$ANIO),
            min(ici_state_year$ICI), max(ici_state_year$ICI)))

# -----------------------------------------------------------------------------
# 7.6 Verification against the published figures
# -----------------------------------------------------------------------------
# Each value below is a number reported in the article. The check recomputes
# it from the current pipeline and flags any mismatch.

ct_nivel <- lmtest::coeftest(
  m_nivel_TWFE,
  vcov = sandwich::vcovHC(m_nivel_TWFE, method = "arellano",
                          cluster = "group", type = "HC1")
)
b_r28  <- ct_nivel["Log_Lag_R28_Abs", 1]
se_r28 <- ct_nivel["Log_Lag_R28_Abs", 2]

sens_stats <- sensemakr::sensitivity_stats(
  model = m_aux_lm, treatment = "Log_Lag_R28_Abs", q = 1, alpha = 0.05
)

# Strip names/attributes so the comparison table builds cleanly.
num <- function(x) as.numeric(unname(x))

checks <- data.frame(
  Quantity = c(
    "Table 1, Level: coefficient",
    "Table 1, Level: clustered SE",
    "Table 1: N",
    "Table 1, Level: R-squared",
    "PC1 variance explained (%)",
    "PC2 variance explained (%)",
    "PC3 variance explained (%)",
    "Rhetoric contribution to PC1 (%)",
    "Rupture contribution to PC1 (%)",
    "Litigation contribution to PC2 (%)",
    "Robustness Value, q=1 (%)",
    "Robustness Value, q=1 alpha=.05 (%)"
  ),
  Published = c(0.211, 0.052, 700, 0.071,
                35.5, 33.0, 31.5,
                45.1, 41.1, 84.2,
                13.47, 6.48),
  Reproduced = c(
    round(num(b_r28), 3),
    round(num(se_r28), 3),
    num(nobs(m_nivel_TWFE)),
    round(num(summary(m_nivel_TWFE)$r.squared[[1]]), 3),
    round(100 * num(varianzas[1]), 1),
    round(100 * num(varianzas[2]), 1),
    round(100 * num(varianzas[3]), 1),
    round(num(100 * pca_model$rotation["log_Rhetoric", "PC1"]^2), 1),
    round(num(100 * pca_model$rotation["Rupture_Alliance_Period", "PC1"]^2), 1),
    round(num(100 * pca_model$rotation["n_conflicts", "PC2"]^2), 1),
    round(100 * num(sens_stats$rv_q), 2),
    round(100 * num(sens_stats$rv_qa), 2)
  ),
  Tolerance = c(0.001, 0.001, 0, 0.001,
                0.05, 0.05, 0.05,
                0.05, 0.05, 0.05,
                0.01, 0.01),
  stringsAsFactors = FALSE
)

checks <- checks %>%
  mutate(Match = ifelse(abs(Published - Reproduced) <= Tolerance, "OK", "*** MISMATCH ***"))

sink(path_out("VERIFICATION.txt"))
cat("Replication verification -- ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
cat("Severe Safeguards and Fiscal Capacity: The Political Economy of\n")
cat("Intergovernmental Retaliation. Publius: The Journal of Federalism.\n\n")
cat("Each row compares a figure reported in the article against the value\n")
cat("produced by this pipeline in the current session.\n\n")
print(as.data.frame(checks), row.names = FALSE)
cat("\n")
if (all(checks$Match == "OK")) {
  cat("RESULT: all checks passed. The pipeline reproduces every verified figure.\n")
} else {
  cat("RESULT: ", sum(checks$Match != "OK"), " MISMATCH(ES). See rows marked above.\n", sep = "")
}
cat("\n--- Session information ---\n")
print(sessionInfo())
sink()

print(as.data.frame(checks), row.names = FALSE)
if (all(checks$Match == "OK")) {
  cat("\n>>> VERIFICATION PASSED: all reported figures reproduce. <<<\n")
} else {
  warning(sprintf("VERIFICATION: %d mismatch(es); see VERIFICATION.txt",
                  sum(checks$Match != "OK")))
}

writeLines(capture.output(sessionInfo()), path_out("sessionInfo.txt"))
print("--- Artifact export complete ---")
