# =============================================================================
# Script 06 — PCA Components and Fiscal Capacity: A Diagnostic Check
# =============================================================================
# Position in pipeline: 6 of N
# Purpose: (1) Tests whether each of the three principal components underlying
#   the Intergovernmental Conflict Index (Score_PC1, Score_PC2, Score_PC3)
#   is systematically associated with lagged fiscal capacity, via bivariate
#   correlation, a regression controlling for state and year fixed effects,
#   and a tercile comparison. Motivated by visual inspection of the PCA
#   loading plots (Figures A1-A3 in the paper's appendix), which show
#   rhetoric and institutional rupture loading with opposite signs on PC3.
#   Reported in the paper's "Construction of the Dependent Variable"
#   subsection as the empirical basis for interpreting PC3.
#   (2) Constructs alternative ICI versions dropping one component at a time
#   (PC2+PC3, dropping PC1; PC1+PC3, dropping PC2) and re-estimates the
#   paper's main Level Model specification with each, completing the set of
#   "drop one component" checks alongside the PC1+PC2 index (dropping PC3)
#   already reported in the paper's main text (constructed in Script 05,
#   Section 10, as m_nivel_2PC).
# Input:   Requires Score_PC1, Score_PC2, Score_PC3 and w1, w2, w3 (component
#   variance weights, from Script 04); Log_Lag_R28_Abs, ENTIDAD, ANIO, and
#   the full panel/model infrastructure (df_clean, pdf_master, m_nivel_TWFE,
#   se_nivel_TWFE, m_nivel_2PC, se_nivel_2PC) from Script 05. Run after
#   Scripts 01-05 in the same session, with no additional filtering applied
#   to df_clean between Script 05 and this script (the sample size, N=700,
#   must match Table 1 in the paper).
# Output:  Console-printed correlation tests, regression summaries, and
#   tercile tables for all three components; console-printed LaTeX
#   regression tables for the PC2+PC3 robustness check; pc3_fiscal_check_
#   results.csv (tercile means for all three components)
# =============================================================================


library(tidyverse)
if (!exists("path_raw")) source(file.path("R", "paths.R"))


# -----------------------------------------------------------------------------
# 1. Prepare data
# -----------------------------------------------------------------------------
df_pca_check <- df_clean %>%
  select(ENTIDAD, ANIO, Score_PC1, Score_PC2, Score_PC3, Log_Lag_R28_Abs,
         Log_Poblacion, Dependencia_Federal) %>%
  na.omit()

# -----------------------------------------------------------------------------
# 2. Bivariate correlations: each component vs. lagged fiscal capacity
# -----------------------------------------------------------------------------
print(">>> Bivariate correlations: PCA components vs. Log_Lag_R28_Abs <<<")

for (pc in c("Score_PC1", "Score_PC2", "Score_PC3")) {
  cat("\n---", pc, "---\n")
  print(cor.test(df_pca_check[[pc]], df_pca_check$Log_Lag_R28_Abs))
}

# -----------------------------------------------------------------------------
# 3. Regression: does fiscal capacity predict each component, net of FE?
# -----------------------------------------------------------------------------
print(">>> Regressions: each component ~ Fiscal Capacity + State/Year FE <<<")

m_pc1_fiscal <- lm(Score_PC1 ~ Log_Lag_R28_Abs + Log_Poblacion + Dependencia_Federal +
                     factor(ENTIDAD) + factor(ANIO), data = df_pca_check)
m_pc2_fiscal <- lm(Score_PC2 ~ Log_Lag_R28_Abs + Log_Poblacion + Dependencia_Federal +
                     factor(ENTIDAD) + factor(ANIO), data = df_pca_check)
m_pc3_fiscal <- lm(Score_PC3 ~ Log_Lag_R28_Abs + Log_Poblacion + Dependencia_Federal +
                     factor(ENTIDAD) + factor(ANIO), data = df_pca_check)

cat("\n--- Coefficient on Log_Lag_R28_Abs, PC1 ---\n")
print(summary(m_pc1_fiscal)$coefficients["Log_Lag_R28_Abs", ])

cat("\n--- Coefficient on Log_Lag_R28_Abs, PC2 ---\n")
print(summary(m_pc2_fiscal)$coefficients["Log_Lag_R28_Abs", ])

cat("\n--- Coefficient on Log_Lag_R28_Abs, PC3 ---\n")
print(summary(m_pc3_fiscal)$coefficients["Log_Lag_R28_Abs", ])

# Full model summaries, for reference
print(">>> Full model summary: PC1 ~ Fiscal Capacity + State/Year FE <<<")
print(summary(m_pc1_fiscal))
print(">>> Full model summary: PC2 ~ Fiscal Capacity + State/Year FE <<<")
print(summary(m_pc2_fiscal))
print(">>> Full model summary: PC3 ~ Fiscal Capacity + State/Year FE <<<")
print(summary(m_pc3_fiscal))

# -----------------------------------------------------------------------------
# 4. Tercile comparison: mean component score by fiscal capacity tercile
# -----------------------------------------------------------------------------
df_pca_check <- df_pca_check %>%
  mutate(Fiscal_Tercile = ntile(Log_Lag_R28_Abs, 3))

print(">>> Mean component scores by fiscal capacity tercile <<<")
tercile_summary <- df_pca_check %>%
  group_by(Fiscal_Tercile) %>%
  summarise(
    mean_PC1 = mean(Score_PC1, na.rm = TRUE),
    mean_PC2 = mean(Score_PC2, na.rm = TRUE),
    mean_PC3 = mean(Score_PC3, na.rm = TRUE),
    n = n()
  )
print(tercile_summary)

print(">>> F-tests: does mean component score differ across fiscal capacity terciles? <<<")

cat("\n--- PC1 ---\n")
print(summary(lm(Score_PC1 ~ factor(Fiscal_Tercile), data = df_pca_check)))

cat("\n--- PC2 ---\n")
print(summary(lm(Score_PC2 ~ factor(Fiscal_Tercile), data = df_pca_check)))

cat("\n--- PC3 ---\n")
print(summary(lm(Score_PC3 ~ factor(Fiscal_Tercile), data = df_pca_check)))

# -----------------------------------------------------------------------------
# 5. Export tercile summary for reporting
# -----------------------------------------------------------------------------
write_csv(tercile_summary, path_out("pc3_fiscal_check_results.csv"))

# =============================================================================
# 6. Alternative Index: PC2 + PC3 Only (Dropping PC1)
# =============================================================================
# Motivation: Section 3 above shows PC1's bivariate correlation with fiscal
# capacity is the strongest of the three components, but loses significance
# once state and year fixed effects are included (p=0.088), while PC3's
# association survives (p=0.041). This suggests PC1's raw correlation with
# fiscal capacity may be largely cross-sectional (between-state), whereas
# PC3 tracks within-state variation more consistent with the paper's TWFE
# identification strategy. As a complementary check to the PC1+PC2 index
# already reported in the paper (Quantitative Evidence section), this
# constructs a PC2+PC3 index (dropping PC1 instead) and re-estimates the
# main specification, to see whether the core result depends on PC1 in
# particular or is robust to which single component is dropped.
#
# Requires w2, w3 (variance weights from Script 04) and Score_PC2, Score_PC3
# in df_clean, plus the panel/model infrastructure (pdf_master, plm, etc.)
# from Script 05. Run after Script 05 in the same session.

library(plm)
library(sandwich)
library(stargazer)

# -----------------------------------------------------------------------------
# 6.1 Construct the PC2+PC3 index
# -----------------------------------------------------------------------------
w2_23 <- w2 / (w2 + w3)
w3_23 <- w3 / (w2 + w3)

print(paste("Renormalized weights (PC2+PC3 only): PC2:", round(w2_23, 3),
            "| PC3:", round(w3_23, 3)))

df_clean <- df_clean %>%
  mutate(
    Indice_Weighted_Raw_23PC = (Score_PC2 * w2_23) + (Score_PC3 * w3_23),
    Indice_Conflictividad_23PC = (Indice_Weighted_Raw_23PC - min(Indice_Weighted_Raw_23PC)) /
      (max(Indice_Weighted_Raw_23PC) - min(Indice_Weighted_Raw_23PC))
  )

pdf_master <- pdata.frame(df_clean, index = c("ENTIDAD", "ANIO"))

cor_full_v_23 <- cor(df_clean$Indice_Conflictividad_W, df_clean$Indice_Conflictividad_23PC)
print(paste("Correlation, full 3-component ICI vs. PC2+PC3-only index:", round(cor_full_v_23, 4)))

# -----------------------------------------------------------------------------
# 6.2 Re-estimate the main table (Table 1) with the PC2+PC3 index
# -----------------------------------------------------------------------------
m_cambio_23PC <- plm(Indice_Conflictividad_23PC ~ Lag_Cambio_R28 + Lag_Cambio_R33 + Lag_Cambio_Conv +
                       Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal +
                       factor(Eleccion_Estatal) + factor(President_Party),
                     data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))

m_nivel_23PC <- plm(Indice_Conflictividad_23PC ~ Log_Lag_R28_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                      Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal +
                      factor(Eleccion_Estatal) + factor(President_Party),
                    data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))

m_full_23PC <- plm(Indice_Conflictividad_23PC ~ Log_Lag_R28_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                     Lag_Cambio_R28 + Lag_Cambio_R33 + Lag_Cambio_Conv +
                     Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal +
                     factor(Eleccion_Estatal) + factor(President_Party),
                   data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))

m_interaccion_23PC <- plm(Indice_Conflictividad_23PC ~ Log_Lag_R28_Abs * Lag_Cambio_R28 +
                            Log_Lag_R33_Abs * Lag_Cambio_R33 + Log_Lag_Conv_Abs * Lag_Cambio_Conv +
                            Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal +
                            factor(Eleccion_Estatal) + factor(President_Party),
                          data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))

se_cambio_23PC <- sqrt(diag(vcovHC(m_cambio_23PC, method = "arellano", cluster = "group", type = "HC1")))
se_nivel_23PC  <- sqrt(diag(vcovHC(m_nivel_23PC,  method = "arellano", cluster = "group", type = "HC1")))
se_full_23PC   <- sqrt(diag(vcovHC(m_full_23PC,   method = "arellano", cluster = "group", type = "HC1")))
se_interaccion_23PC <- sqrt(diag(vcovHC(m_interaccion_23PC, method = "arellano", cluster = "group", type = "HC1")))

print(">>> TABLE 1 REPLICATION: PC2+PC3 Index Only (PC1 Dropped) <<<")
stargazer(m_cambio_23PC, m_nivel_23PC, m_full_23PC, m_interaccion_23PC,
          se = list(se_cambio_23PC, se_nivel_23PC, se_full_23PC, se_interaccion_23PC),
          type = "latex",
          title = "Robustness: Main Specification with PC2+PC3 Index (PC1 Dropped)",
          column.labels = c("Change (t-1)", "Level (t-1)", "Full (t-1)", "Interaction (t-1)"),
          omit.stat = c("f", "ser", "adj.rsq"), no.space = TRUE)

# -----------------------------------------------------------------------------
# 6.3 Side-by-side comparison: main ICI vs. PC2+PC3-only, Level Model
# -----------------------------------------------------------------------------
print(">>> Side-by-side: Main ICI (3 components) vs. PC2+PC3-only, Level Model <<<")
stargazer(m_nivel_TWFE, m_nivel_23PC,
          se = list(se_nivel_TWFE, se_nivel_23PC),
          type = "latex",
          title = "Robustness: ICI (3 Components) vs. PC2+PC3-Only Index, Level Model",
          column.labels = c("ICI (3 components)", "ICI (PC2+PC3 only)"),
          omit.stat = c("f", "ser", "adj.rsq"), no.space = TRUE)

# =============================================================================
# 7. Alternative Index: PC1 + PC3 Only (Dropping PC2)
# =============================================================================
# Completes the set of "drop one component" checks (PC1+PC2 in the paper's
# main text, PC2+PC3 in Section 6 above, PC1+PC3 here). PC2 is dominated
# almost entirely by litigation counts (80.5% contribution; see Table
# pca_contributions), so this check is the least central to the paper's
# argument about PC3, but completes the full 3-choose-2 comparison.

w1_13 <- w1 / (w1 + w3)
w3_13 <- w3 / (w1 + w3)

print(paste("Renormalized weights (PC1+PC3 only): PC1:", round(w1_13, 3),
            "| PC3:", round(w3_13, 3)))

df_clean <- df_clean %>%
  mutate(
    Indice_Weighted_Raw_13PC = (Score_PC1 * w1_13) + (Score_PC3 * w3_13),
    Indice_Conflictividad_13PC = (Indice_Weighted_Raw_13PC - min(Indice_Weighted_Raw_13PC)) /
      (max(Indice_Weighted_Raw_13PC) - min(Indice_Weighted_Raw_13PC))
  )

pdf_master <- pdata.frame(df_clean, index = c("ENTIDAD", "ANIO"))

cor_full_v_13 <- cor(df_clean$Indice_Conflictividad_W, df_clean$Indice_Conflictividad_13PC)
print(paste("Correlation, full 3-component ICI vs. PC1+PC3-only index:", round(cor_full_v_13, 4)))

m_nivel_13PC <- plm(Indice_Conflictividad_13PC ~ Log_Lag_R28_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                      Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal +
                      factor(Eleccion_Estatal) + factor(President_Party),
                    data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))
se_nivel_13PC <- sqrt(diag(vcovHC(m_nivel_13PC, method = "arellano", cluster = "group", type = "HC1")))

print(">>> Level Model with PC1+PC3 Index (PC2 Dropped) <<<")
stargazer(m_nivel_13PC,
          se = list(se_nivel_13PC),
          type = "latex",
          title = "Robustness: Main Specification with PC1+PC3 Index (PC2 Dropped)",
          column.labels = c("Level (t-1), PC1+PC3 only"),
          omit.stat = c("f", "ser", "adj.rsq"), no.space = TRUE)

# -----------------------------------------------------------------------------
# Construct the missing PC1+PC2 index (dropping PC3) and re-estimate
# -----------------------------------------------------------------------------
w1_12 <- w1 / (w1 + w2)
w2_12 <- w2 / (w1 + w2)

print(paste("Renormalized weights (PC1+PC2 only): PC1:", round(w1_12, 3),
            "| PC2:", round(w2_12, 3)))

df_clean <- df_clean %>%
  mutate(
    Indice_Weighted_Raw_12PC = (Score_PC1 * w1_12) + (Score_PC2 * w2_12),
    Indice_Conflictividad_12PC = (Indice_Weighted_Raw_12PC - min(Indice_Weighted_Raw_12PC)) /
      (max(Indice_Weighted_Raw_12PC) - min(Indice_Weighted_Raw_12PC))
  )

pdf_master <- pdata.frame(df_clean, index = c("ENTIDAD", "ANIO"))

cor_full_v_12 <- cor(df_clean$Indice_Conflictividad_W, df_clean$Indice_Conflictividad_12PC)
print(paste("Correlation, full 3-component ICI vs. PC1+PC2-only index:", round(cor_full_v_12, 4)))

m_nivel_2PC <- plm(Indice_Conflictividad_12PC ~ Log_Lag_R28_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                     Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal +
                     factor(Eleccion_Estatal) + factor(President_Party),
                   data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))
se_nivel_2PC <- sqrt(diag(vcovHC(m_nivel_2PC, method = "arellano", cluster = "group", type = "HC1")))

print(">>> Level Model with PC1+PC2 Index (PC3 Dropped) <<<")
stargazer(m_nivel_2PC,
          se = list(se_nivel_2PC),
          type = "latex",
          title = "Robustness: Main Specification with PC1+PC2 Index (PC3 Dropped)",
          column.labels = c("Level (t-1), PC1+PC2 only"),
          omit.stat = c("f", "ser", "adj.rsq"), no.space = TRUE)

# Ahora sí, la tabla comparativa completa de 4 columnas
print(">>> Full comparison: baseline vs. all three 2-component indices <<<")
stargazer(m_nivel_TWFE, m_nivel_2PC, m_nivel_13PC, m_nivel_23PC,
          se = list(se_nivel_TWFE, se_nivel_2PC, se_nivel_13PC, se_nivel_23PC),
          type = "latex",
          title = "Robustness: ICI Excluding Individual Components",
          column.labels = c("Baseline (3 comp.)", "PC1+PC2 (PC3 dropped)", "PC1+PC3 (PC2 dropped)", "PC2+PC3 (PC1 dropped)"),
          keep = "Log_Lag_R28_Abs",
          omit.stat = c("f", "ser", "adj.rsq"), no.space = TRUE)

# -----------------------------------------------------------------------------
# 7.1 Full four-way comparison: baseline vs. each two-component index
# -----------------------------------------------------------------------------
print(">>> Full comparison: baseline (3 components) vs. all three 2-component indices <<<")
stargazer(m_nivel_TWFE, m_nivel_2PC, m_nivel_13PC, m_nivel_23PC,
          se = list(se_nivel_TWFE, se_nivel_2PC, se_nivel_13PC, se_nivel_23PC),
          type = "latex",
          title = "Robustness: ICI Excluding Individual Components",
          column.labels = c("Baseline (3 comp.)", "PC1+PC2 (PC3 dropped)", "PC1+PC3 (PC2 dropped)", "PC2+PC3 (PC1 dropped)"),
          keep = "Log_Lag_R28_Abs",
          omit.stat = c("f", "ser", "adj.rsq"), no.space = TRUE)
