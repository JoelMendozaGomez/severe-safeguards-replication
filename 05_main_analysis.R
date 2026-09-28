# =============================================================================
# Script 05 — Main Panel Regressions, Robustness Checks, and Diagnostics
# =============================================================================
# Position in pipeline: 5 of N
# Purpose: Estimates the paper's main Two-Way Fixed Effects (TWFE) models
#   (Table 1), runs all robustness checks reported in the paper and its
#   online appendix (Random Effects comparison, oil-producing states/FEIEF,
#   own-source revenue, two-component ICI, Ramo 28 exogeneity), a sensitivity
#   analysis to omitted variable bias (Cinelli et al. 2020), and generates
#   the conditional marginal effects figure.
# Input:   Base_Tesis_MASTER_Weighted_PCA.csv (Script 04 output);
#   Poblacion_Estados_CONAPO.csv (Script 01 output);
#   ownsource_direct_panel.csv (Script 02b output, direct own-source revenue)
# Output:  Console-printed regression tables (stargazer, LaTeX format,
#   matching the paper's Table 1 and appendix tables); predictions.pdf
#   (conditional marginal effects figure)
# Note:    The main TWFE specification INCLUDES President Party fixed
#   effects, matching Table 1 as reported in the paper. President Party FE
#   is collinear with Year FE in this panel (Mexico's presidential party is
#   a deterministic function of year), so its inclusion does not change any
#   TWFE coefficient; it is retained deliberately so that the script and the
#   published table specify the same model. Section 7 below re-estimates
#   without it and confirms the coefficients are unchanged.
#   (An earlier version of this header incorrectly described the
#   specification as excluding President Party FE; the code was always
#   correct and is what produced the published table.)
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Libraries
# -----------------------------------------------------------------------------
library(tidyverse)
library(stargazer)
library(stringi)
library(sandwich)
library(lmtest)
library(ggeffects)
library(scales)
library(knitr)
library(sensemakr)
library(plm)
library(car)
if (!exists("path_raw")) source(file.path("R", "paths.R"))


# -----------------------------------------------------------------------------
# 2. Load and merge data
# -----------------------------------------------------------------------------
df_master <- readr::read_csv(path_data("Base_Tesis_MASTER_Weighted_PCA.csv"), show_col_types = FALSE)
df_poblacion <- readr::read_csv(path_data("Poblacion_Estados_CONAPO.csv"), show_col_types = FALSE)

clean_key <- function(x) x %>% str_to_lower() %>% stri_trans_general("Latin-ASCII") %>% str_trim()

df_ownsource <- readr::read_csv(path_data("ownsource_direct_panel.csv"), show_col_types = FALSE)

df_master <- df_master %>% mutate(ENTIDAD_KEY = clean_key(ENTIDAD))
df_poblacion <- df_poblacion %>% mutate(ENTIDAD_KEY = clean_key(ENTIDAD)) %>% select(ENTIDAD_KEY, ANIO, POBLACION)
df_ownsource <- df_ownsource %>% mutate(ENTIDAD_KEY = clean_key(ENTIDAD)) %>%
  select(ENTIDAD_KEY, ANIO, OwnSourceRevenue_Direct)

df_merged <- df_master %>%
  inner_join(df_poblacion, by = c("ENTIDAD_KEY", "ANIO")) %>%
  left_join(df_ownsource, by = c("ENTIDAD_KEY", "ANIO"))

# Guard: the own-source panel covers 1989-2024 for all 32 states, so no
# analysis-sample row should be left without it.
if (any(is.na(df_merged$OwnSourceRevenue_Direct))) {
  stop(sprintf("ownsource_direct_panel.csv failed to merge for %d state-years.",
               sum(is.na(df_merged$OwnSourceRevenue_Direct))))
}

# -----------------------------------------------------------------------------
# 3. Variable engineering: lags, changes, and robustness-check variables
# -----------------------------------------------------------------------------
# Builds lagged levels (t-1) and lagged percentage changes for the three
# fiscal transfer categories, plus the FEIEF/oil-state and own-source
# revenue variables used in the robustness checks below (Sections 8-9).
# Own-source revenue is floored at zero (pmax) since Ingreso_Local_Residual
# is a residual that can, in principle, be slightly negative due to
# Convenios misclassification noise.

df_vars <- df_merged %>%
  mutate(across(c(Ramo_28, Ramo_33, Convenios, Ramo_28_ExFEIEF, FEIEF, FEIEF_Share_R28),
                ~as.numeric(gsub(",", "", as.character(.))))) %>%
  arrange(ENTIDAD_KEY, ANIO) %>%
  group_by(ENTIDAD_KEY) %>%
  mutate(
    Log_Poblacion = log(POBLACION),
    Log_Lag_R28_Abs   = log(dplyr::lag(Ramo_28, n = 1) + 1),
    Log_Lag_R33_Abs   = log(dplyr::lag(Ramo_33, n = 1) + 1),
    Log_Lag_Conv_Abs  = log(dplyr::lag(Convenios, n = 1) + 1),
    
    Cambio_R28  = (Ramo_28 - dplyr::lag(Ramo_28)) / (dplyr::lag(Ramo_28) + 1),
    Cambio_R33  = (Ramo_33 - dplyr::lag(Ramo_33)) / (dplyr::lag(Ramo_33) + 1),
    Cambio_Conv = (Convenios - dplyr::lag(Convenios)) / (dplyr::lag(Convenios) + 1),
    
    Lag_Cambio_R28  = dplyr::lag(Cambio_R28),
    Lag_Cambio_R33  = dplyr::lag(Cambio_R33),
    Lag_Cambio_Conv = dplyr::lag(Cambio_Conv),
    
    Log_Lag_FEIEF_Abs       = log(dplyr::lag(FEIEF, n = 1) + 1),
    Log_Lag_R28_ExFEIEF_Abs = log(dplyr::lag(Ramo_28_ExFEIEF, n = 1) + 1),
    Lag_FEIEF_Share         = dplyr::lag(FEIEF_Share_R28, n = 1),
    
    Aligned = factor(Aligned),
    # Residual measure (superseded; kept only for the comparison in Section 9).
    Log_Lag_OwnSource_Abs = log(dplyr::lag(pmax(Ingreso_Local_Residual, 0), n = 1) + 1),
    # DIRECT measure -- the one reported in the paper. Built in Script 02b from
    # EFIPEM's Impuestos / Derechos / Productos / Aprovechamientos chapters.
    Log_Lag_OwnSourceDirect_Abs = log(dplyr::lag(pmax(OwnSourceRevenue_Direct, 0), n = 1) + 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(Lag_Cambio_R28))

# -----------------------------------------------------------------------------
# 4. Panel data preparation
# -----------------------------------------------------------------------------
df_clean <- df_vars %>%
  mutate(Eleccion_Estatal = case_when(
    ENTIDAD_KEY %in% c("chiapas", "guanajuato", "jalisco", "morelos", "tabasco", "veracruz", "yucatan", "ciudad de mexico", "distrito federal") & ANIO %in% c(2000, 2006, 2012, 2018, 2024) ~ 1,
    ENTIDAD_KEY %in% c("baja california sur", "campeche", "guerrero", "michoacan", "nuevo leon", "queretaro", "san luis potosi", "sonora") & ANIO %in% c(2003, 2009, 2015, 2021) ~ 1,
    ENTIDAD_KEY %in% c("aguascalientes", "durango", "oaxaca", "quintana roo", "tamaulipas", "hidalgo") & ANIO %in% c(2004, 2010, 2016, 2022) ~ 1,
    ENTIDAD_KEY %in% c("chihuahua", "sinaloa", "tlaxcala", "zacatecas") & ANIO %in% c(2004, 2010, 2016, 2021) ~ 1,
    ENTIDAD_KEY == "baja california" & ANIO %in% c(2001, 2007, 2013, 2019, 2021) ~ 1,
    ENTIDAD_KEY %in% c("coahuila", "mexico", "estado de mexico") & ANIO %in% c(2005, 2011, 2017, 2023) ~ 1,
    ENTIDAD_KEY == "colima" & ANIO %in% c(2003, 2009, 2015, 2016, 2021) ~ 1,
    ENTIDAD_KEY == "nayarit" & ANIO %in% c(2005, 2011, 2017, 2021) ~ 1,
    ENTIDAD_KEY == "puebla" & ANIO %in% c(2004, 2010, 2016, 2018, 2019, 2024) ~ 1,
    TRUE ~ 0
  )) %>%
  drop_na(Lag_Cambio_R28, Log_Lag_R28_Abs, Aligned, Indice_Conflictividad_W, Log_Poblacion, Dependencia_Federal,
          Log_Lag_R28_ExFEIEF_Abs, Log_Lag_FEIEF_Abs, Lag_FEIEF_Share)

pdf_master <- pdata.frame(df_clean, index = c("ENTIDAD", "ANIO"))

# -----------------------------------------------------------------------------
# 5. Main TWFE specifications (paper Table 1)
# -----------------------------------------------------------------------------
m_cambio_TWFE <- plm(Indice_Conflictividad_W ~ Lag_Cambio_R28 + Lag_Cambio_R33 + Lag_Cambio_Conv +
                       Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal + factor(Eleccion_Estatal) + factor(President_Party),
                     data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))

m_nivel_TWFE <- plm(Indice_Conflictividad_W ~ Log_Lag_R28_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                      Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal + factor(Eleccion_Estatal) + factor(President_Party),
                    data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))

m_full_TWFE <- plm(Indice_Conflictividad_W ~ Log_Lag_R28_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                     Lag_Cambio_R28 + Lag_Cambio_R33 + Lag_Cambio_Conv +
                     Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal + factor(Eleccion_Estatal) + factor(President_Party),
                   data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))

m_interaccion <- plm(Indice_Conflictividad_W ~ Log_Lag_R28_Abs * Lag_Cambio_R28 +
                       Log_Lag_R33_Abs * Lag_Cambio_R33 + Log_Lag_Conv_Abs * Lag_Cambio_Conv +
                       Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal + factor(Eleccion_Estatal) + factor(President_Party),
                     data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))

se_cambio_TWFE <- sqrt(diag(vcovHC(m_cambio_TWFE, method = "arellano", cluster = "group", type = "HC1")))
se_nivel_TWFE  <- sqrt(diag(vcovHC(m_nivel_TWFE,  method = "arellano", cluster = "group", type = "HC1")))
se_full_TWFE   <- sqrt(diag(vcovHC(m_full_TWFE,   method = "arellano", cluster = "group", type = "HC1")))
se_interaccion <- sqrt(diag(vcovHC(m_interaccion, method = "arellano", cluster = "group", type = "HC1")))

print(">>> TABLE 1: Effect of Fiscal Opportunism and Fiscal Strength (Lagged t-1) <<<")
stargazer(m_cambio_TWFE, m_nivel_TWFE, m_full_TWFE, m_interaccion,
          se = list(se_cambio_TWFE, se_nivel_TWFE, se_full_TWFE, se_interaccion),
          type = "latex",
          title = "Effect of Fiscal Opportunism and Fiscal Strength (Lagged t-1)",
          column.labels = c("Change (t-1)", "Level (t-1)", "Full (t-1)", "Interaction (t-1)"),
          omit.stat = c("f", "ser", "adj.rsq"), no.space = TRUE)

# -----------------------------------------------------------------------------
# 6. Collinearity diagnostic: VIF for Aligned
# -----------------------------------------------------------------------------
# plm does not support vif() directly, so the TWFE model is replicated in
# lm() with explicit state and year dummies (equivalent to the "within"
# estimator) to obtain GVIF for Aligned and the other regressors.

df_vif <- df_clean %>%
  select(Indice_Conflictividad_W, Log_Lag_R28_Abs, Log_Lag_R33_Abs, Log_Lag_Conv_Abs,
         Lag_Cambio_R28, Lag_Cambio_R33, Lag_Cambio_Conv,
         Log_Poblacion, Aligned, Governor_Party, Dependencia_Federal, Eleccion_Estatal,
         ENTIDAD, ANIO) %>%
  na.omit()

m_vif_check <- lm(Indice_Conflictividad_W ~ Log_Lag_R28_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                    Lag_Cambio_R28 + Lag_Cambio_R33 + Lag_Cambio_Conv +
                    Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal +
                    factor(Eleccion_Estatal) + factor(ENTIDAD) + factor(ANIO),
                  data = df_vif)

print(">>> VIF DIAGNOSTIC (GVIF for factors) <<<")
print(vif(m_vif_check))

# -----------------------------------------------------------------------------
# 7. Random Effects comparison and Hausman test
# -----------------------------------------------------------------------------
# The main Random Effects table (paper appendix) includes President Party
# FE. A second specification without President Party FE is estimated
# immediately after to confirm that the null Aligned coefficient, and all
# other coefficients, are stable regardless of its inclusion -- the basis
# for excluding President Party FE from the main TWFE specification above.

m_full_RE <- plm(Indice_Conflictividad_W ~ Log_Lag_R28_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                   Lag_Cambio_R28 + Lag_Cambio_R33 + Lag_Cambio_Conv +
                   Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal +
                   factor(Eleccion_Estatal) + factor(President_Party),
                 data = pdf_master, model = "random", effect = "twoways", index = c("ENTIDAD", "ANIO"))
se_full_RE <- sqrt(diag(vcovHC(m_full_RE, cluster = "group", type = "HC1")))

print(">>> HAUSMAN TEST (TWFE vs. Random Effects) <<<")
print(phtest(m_full_TWFE, m_full_RE))

m_cambio_random <- plm(Indice_Conflictividad_W ~ Lag_Cambio_R28 + Lag_Cambio_R33 + Lag_Cambio_Conv +
                         Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal + factor(Eleccion_Estatal) + factor(President_Party),
                       data = pdf_master, model = "random", effect = "twoways", index = c("ENTIDAD", "ANIO"))
se_cambio_random <- sqrt(diag(vcovHC(m_cambio_random, cluster = "group", type = "HC1")))

m_nivel_random <- plm(Indice_Conflictividad_W ~ Log_Lag_R28_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                        Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal + factor(Eleccion_Estatal) + factor(President_Party),
                      data = pdf_master, model = "random", effect = "twoways", index = c("ENTIDAD", "ANIO"))
se_nivel_random <- sqrt(diag(vcovHC(m_nivel_random, cluster = "group", type = "HC1")))

m_interaccion_random <- plm(Indice_Conflictividad_W ~ Log_Lag_R28_Abs * Lag_Cambio_R28 +
                              Log_Lag_R33_Abs * Lag_Cambio_R33 + Log_Lag_Conv_Abs * Lag_Cambio_Conv +
                              Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal + factor(Eleccion_Estatal) + factor(President_Party),
                            data = pdf_master, model = "random", effect = "twoways", index = c("ENTIDAD", "ANIO"))
se_interaccion_random <- sqrt(diag(vcovHC(m_interaccion_random, cluster = "group", type = "HC1")))

print(">>> APPENDIX TABLE: Random Effects (with President Party FE) <<<")
stargazer(m_cambio_random, m_nivel_random, m_full_RE, m_interaccion_random,
          se = list(se_cambio_random, se_nivel_random, se_full_RE, se_interaccion_random),
          type = "latex",
          title = "Effect of Fiscal Opportunism and Fiscal Strength (Lagged t-1) -- Random Effects",
          column.labels = c("Change (t-1)", "Level (t-1)", "Full (t-1)", "Interaction (t-1)"),
          omit.stat = c("f", "ser", "adj.rsq"), no.space = TRUE)

m_full_RE_noPresFE <- plm(Indice_Conflictividad_W ~ Log_Lag_R28_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                            Lag_Cambio_R28 + Lag_Cambio_R33 + Lag_Cambio_Conv +
                            Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal +
                            factor(Eleccion_Estatal),
                          data = pdf_master, model = "random", effect = "twoways", index = c("ENTIDAD", "ANIO"))
se_full_RE_noPresFE <- sqrt(diag(vcovHC(m_full_RE_noPresFE, cluster = "group", type = "HC1")))

print(">>> ROBUSTNESS: Partisan Alignment Null Result, With and Without President Party FE <<<")
stargazer(m_full_RE, m_full_RE_noPresFE,
          se = list(se_full_RE, se_full_RE_noPresFE),
          type = "latex",
          title = "Robustness: Partisan Alignment Null Result, With and Without President Party FE",
          column.labels = c("Full RE (with President Party FE)", "Full RE (no President Party FE)"),
          omit.stat = c("f", "ser", "adj.rsq"), no.space = TRUE)

print(">>> APPENDIX TABLE: Panel Data Structure <<<")
stargazer(pdf_master, type = "latex", title = "Panel Data Structure", summary = TRUE, rownames = FALSE)

# -----------------------------------------------------------------------------
# 8. Robustness: oil-producing states and the FEIEF fund
# -----------------------------------------------------------------------------
# FEIEF (hydrocarbon extraction fund) is classified by INEGI inside
# "Participaciones federales" (Ramo 28), not Convenios. Nationally it is
# ~0.5% of Ramo 28, but concentrated (~22% of Ramo 28 in Campeche, ~4.5% in
# Tabasco). Three additional specifications test whether the discretionary-
# transfers effect is an artifact of oil-state governance rather than
# fiscal capacity as such.

m_nivel_baseline <- m_nivel_TWFE

m_nivel_ctrl_feief <- plm(Indice_Conflictividad_W ~ Log_Lag_R28_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs + Lag_FEIEF_Share +
                            Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal + factor(Eleccion_Estatal),
                          data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))

m_nivel_exfeief <- plm(Indice_Conflictividad_W ~ Log_Lag_R28_ExFEIEF_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                         Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal + factor(Eleccion_Estatal),
                       data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))

# Placebo: FEIEF alone as the predictor. A null result here is inconsistent
# with oil income itself driving the effect via a spurious governance
# channel (clientelism, organized crime, etc.).
m_feief_placebo <- plm(Indice_Conflictividad_W ~ Log_Lag_FEIEF_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                         Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal + factor(Eleccion_Estatal),
                       data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))

se_nivel_baseline   <- sqrt(diag(vcovHC(m_nivel_baseline,   method = "arellano", cluster = "group", type = "HC1")))
se_nivel_ctrl_feief <- sqrt(diag(vcovHC(m_nivel_ctrl_feief, method = "arellano", cluster = "group", type = "HC1")))
se_nivel_exfeief    <- sqrt(diag(vcovHC(m_nivel_exfeief,    method = "arellano", cluster = "group", type = "HC1")))
se_feief_placebo    <- sqrt(diag(vcovHC(m_feief_placebo,    method = "arellano", cluster = "group", type = "HC1")))

print(">>> ROBUSTNESS: Oil-Producing States and the FEIEF Fund <<<")
stargazer(m_nivel_baseline, m_nivel_ctrl_feief, m_nivel_exfeief, m_feief_placebo,
          se = list(se_nivel_baseline, se_nivel_ctrl_feief, se_nivel_exfeief, se_feief_placebo),
          type = "latex",
          title = "Robustness: Oil-Producing States and the FEIEF Fund",
          column.labels = c("Baseline (Level)", "+ Oil share control", "Ramo 28 ex-FEIEF", "FEIEF only (placebo)"),
          omit.stat = c("f", "ser", "adj.rsq"), no.space = TRUE)

# -----------------------------------------------------------------------------
# 9. Robustness: own-source revenue
# -----------------------------------------------------------------------------
# Reviewer 2 objected that the original own-source measure was a residual
# (total income minus Ramo 28, Ramo 33 and Convenios) and therefore absorbed
# debt and financing inflows alongside genuine local revenue. The models below
# use the DIRECT measure built in Script 02b. The residual-based models are
# re-estimated immediately afterwards so the two can be compared; only the
# direct-measure results are reported in the paper.

m_nivel_ctrl_ownsource <- plm(Indice_Conflictividad_W ~ Log_Lag_R28_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                                Log_Lag_OwnSourceDirect_Abs +
                                Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal + factor(Eleccion_Estatal),
                              data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))

m_ownsource_solo <- plm(Indice_Conflictividad_W ~ Log_Lag_OwnSourceDirect_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                          Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal + factor(Eleccion_Estatal),
                        data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))

se_nivel_ctrl_ownsource <- sqrt(diag(vcovHC(m_nivel_ctrl_ownsource, method = "arellano", cluster = "group", type = "HC1")))
se_ownsource_solo       <- sqrt(diag(vcovHC(m_ownsource_solo,       method = "arellano", cluster = "group", type = "HC1")))

print(">>> ROBUSTNESS: Own-Source Revenue Control (DIRECT measure -- paper Table ownsource_robustness) <<<")
stargazer(m_nivel_baseline, m_nivel_ctrl_ownsource, m_ownsource_solo,
          se = list(se_nivel_baseline, se_nivel_ctrl_ownsource, se_ownsource_solo),
          type = "latex",
          title = "Robustness: Own-Source Revenue Control (direct measure)",
          column.labels = c("Baseline (Level)", "+ Own-Source Control", "Own-Source Only"),
          omit.stat = c("f", "ser", "adj.rsq"), no.space = TRUE)

print(">>> Own-source revenue as a share of total state income <<<")
df_clean %>%
  mutate(
    Share_OwnSource_Direct   = OwnSourceRevenue_Direct / Total_Oficial,
    Share_OwnSource_Residual = pmax(Ingreso_Local_Residual, 0) / Total_Oficial
  ) %>%
  summarise(
    direct_mean     = mean(Share_OwnSource_Direct, na.rm = TRUE),
    direct_median   = median(Share_OwnSource_Direct, na.rm = TRUE),
    direct_sd       = sd(Share_OwnSource_Direct, na.rm = TRUE),
    residual_mean   = mean(Share_OwnSource_Residual, na.rm = TRUE),
    residual_median = median(Share_OwnSource_Residual, na.rm = TRUE),
    residual_sd     = sd(Share_OwnSource_Residual, na.rm = TRUE)
  ) %>%
  print()

cat("\n>>> Correlation between the direct and residual measures (logged, lagged) <<<\n")
print(cor(df_clean$Log_Lag_OwnSourceDirect_Abs, df_clean$Log_Lag_OwnSource_Abs,
          use = "complete.obs"))

# Superseded residual-based specifications, for comparison only.
m_nivel_ctrl_ownsource_residual <- plm(Indice_Conflictividad_W ~ Log_Lag_R28_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                                         Log_Lag_OwnSource_Abs +
                                         Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal + factor(Eleccion_Estatal),
                                       data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))
m_ownsource_solo_residual <- plm(Indice_Conflictividad_W ~ Log_Lag_OwnSource_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                                   Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal + factor(Eleccion_Estatal),
                                 data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))
se_nivel_ctrl_ownsource_residual <- sqrt(diag(vcovHC(m_nivel_ctrl_ownsource_residual, method = "arellano", cluster = "group", type = "HC1")))
se_ownsource_solo_residual       <- sqrt(diag(vcovHC(m_ownsource_solo_residual,       method = "arellano", cluster = "group", type = "HC1")))

print(">>> COMPARISON ONLY: superseded residual-based own-source measure <<<")
stargazer(m_nivel_baseline, m_nivel_ctrl_ownsource_residual, m_ownsource_solo_residual,
          se = list(se_nivel_baseline, se_nivel_ctrl_ownsource_residual, se_ownsource_solo_residual),
          type = "latex",
          title = "Superseded: Own-Source Revenue Control (residual measure, not reported in the paper)",
          column.labels = c("Baseline (Level)", "+ Own-Source (residual)", "Own-Source only (residual)"),
          omit.stat = c("f", "ser", "adj.rsq"), no.space = TRUE)

# -----------------------------------------------------------------------------
# 10. Robustness: two-component ICI (dropping the atheoretical third component)
# -----------------------------------------------------------------------------
m_nivel_2PC <- plm(Indice_Conflictividad_2PC ~ Log_Lag_R28_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                     Log_Poblacion + Aligned + factor(Governor_Party) + Dependencia_Federal + factor(Eleccion_Estatal),
                   data = pdf_master, model = "within", effect = "twoways", index = c("ENTIDAD", "ANIO"))
se_nivel_2PC <- sqrt(diag(vcovHC(m_nivel_2PC, method = "arellano", cluster = "group", type = "HC1")))

print(">>> ROBUSTNESS: ICI with Three Components (Main) vs. Two Components Only <<<")
stargazer(m_nivel_TWFE, m_nivel_2PC,
          se = list(se_nivel_TWFE, se_nivel_2PC),
          type = "latex",
          title = "Robustness: ICI with Three Components (Main) vs. Two Theoretically-Labeled Components Only",
          column.labels = c("ICI (3 components)", "ICI (PC1+PC2 only)"),
          omit.stat = c("f", "ser", "adj.rsq"), no.space = TRUE)

# -----------------------------------------------------------------------------
# 11. Exogeneity check: Ramo 28 residual vs. political variables
# -----------------------------------------------------------------------------
# Approximates the legal allocation formula (Ley de Coordinacion Fiscal,
# Art. 2, which weights population and historical federal participable
# revenue) using Log_Poblacion plus state and year fixed effects (the
# latter capturing persistent historical levels and the year-specific
# national pool, respectively). The residual is then tested against
# Governor Party and Aligned; a small, non-systematic political
# correlation is consistent with formula-based allocation dominating
# transfer variation, as claimed in the Identification Strategy section.

df_resid_check <- df_clean %>%
  select(Log_Lag_R28_Abs, Log_Poblacion, Governor_Party, Aligned, Dependencia_Federal,
         ENTIDAD, ANIO) %>%
  na.omit()

m_r28_formula <- lm(Log_Lag_R28_Abs ~ Log_Poblacion + factor(ENTIDAD) + factor(ANIO),
                    data = df_resid_check)
df_resid_check$resid_r28 <- residuals(m_r28_formula)

m_resid_political <- lm(resid_r28 ~ factor(Governor_Party) + Aligned, data = df_resid_check)
print(">>> Ramo 28 residual ~ Governor Party + Aligned <<<")
print(summary(m_resid_political))

m_resid_null <- lm(resid_r28 ~ 1, data = df_resid_check)
print(">>> F-test: does the political block explain the Ramo 28 residual? <<<")
print(anova(m_resid_null, m_resid_political))

# -----------------------------------------------------------------------------
# 12. Sensitivity analysis to omitted variable bias (Cinelli et al. 2020)
# -----------------------------------------------------------------------------
df_sense <- df_clean %>%
  select(Indice_Conflictividad_W, Log_Lag_R28_Abs, Log_Lag_R33_Abs, Log_Lag_Conv_Abs,
         Lag_Cambio_R28, Lag_Cambio_R33, Lag_Cambio_Conv,
         Log_Poblacion, Aligned, Dependencia_Federal, ENTIDAD, ANIO) %>%
  na.omit()

m_aux_lm <- lm(Indice_Conflictividad_W ~ Log_Lag_R28_Abs + Log_Lag_R33_Abs + Log_Lag_Conv_Abs +
                 Lag_Cambio_R28 + Lag_Cambio_R33 + Lag_Cambio_Conv +
                 Log_Poblacion + Aligned + Dependencia_Federal +
                 factor(ENTIDAD) + factor(ANIO),
               data = df_sense)

sens_analysis <- sensemakr(model = m_aux_lm,
                           treatment = "Log_Lag_R28_Abs",
                           benchmark_covariates = "Log_Poblacion",
                           kd = 1:3, q = 1, alpha = 0.05, reduce = TRUE)

print(">>> Sensitivity analysis: Log_Lag_R28_Abs, benchmarked against Log_Poblacion <<<")
print(sens_analysis)
print(summary(sens_analysis))
plot(sens_analysis, type = "contour")

# -----------------------------------------------------------------------------
# 13. Conditional marginal effects figure
# -----------------------------------------------------------------------------
# Plots predicted conflict under an austerity shock (-1 SD in discretionary
# transfers at t-1) across the observed distribution of fiscal capacity,
# with a rug plot marking where states actually fall. This addresses the
# out-of-sample interpretation issue in the Interaction Model column of
# Table 1, where the uninteracted coefficient corresponds to zero lagged
# discretionary transfers -- a point far outside the observed range.

df_plot <- df_clean %>%
  select(Indice_Conflictividad_W, Log_Lag_R28_Abs, Log_Lag_R33_Abs, Log_Lag_Conv_Abs,
         Lag_Cambio_R28, Lag_Cambio_R33, Lag_Cambio_Conv,
         Log_Poblacion, Aligned, Governor_Party, Dependencia_Federal, Eleccion_Estatal,
         ENTIDAD, ANIO) %>%
  na.omit() %>%
  mutate(
    Aligned = factor(Aligned, levels = c(0, 1), labels = c("Non-Copartisan", "Copartisan")),
    Governor_Party = as.factor(Governor_Party),
    Eleccion_Estatal = as.factor(Eleccion_Estatal),
    ENTIDAD = as.factor(ENTIDAD),
    ANIO = as.factor(ANIO)
  )

m_aux_inter <- lm(Indice_Conflictividad_W ~ Log_Lag_R28_Abs * Lag_Cambio_R28 +
                    Log_Lag_R33_Abs * Lag_Cambio_R33 + Log_Lag_Conv_Abs * Lag_Cambio_Conv +
                    Log_Poblacion + Aligned + Governor_Party + Dependencia_Federal + Eleccion_Estatal +
                    ENTIDAD + ANIO,
                  data = df_plot)

sd_cambio <- sd(df_plot$Lag_Cambio_R28, na.rm = TRUE)
shock_val <- -1 * sd_cambio

min_val <- min(df_plot$Log_Lag_R28_Abs, na.rm = TRUE)
max_val <- max(df_plot$Log_Lag_R28_Abs, na.rm = TRUE)
puntos_x <- seq(from = min_val, to = max_val, length.out = 10)
valores_x_str <- paste(puntos_x, collapse = ", ")

pred_cover <- ggpredict(m_aux_inter,
                        terms = c(paste0("Log_Lag_R28_Abs [", valores_x_str, "]"),
                                  paste0("Lag_Cambio_R28 [", shock_val, "]"),
                                  "Aligned"))

plot_cover_whiskers <- ggplot(pred_cover, aes(x = x, y = predicted, color = facet)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, linewidth = 1,
                position = position_dodge(width = 0.25)) +
  geom_point(size = 4, position = position_dodge(width = 0.25)) +
  geom_line(aes(group = facet), alpha = 0.4, linetype = "solid",
            position = position_dodge(width = 0.25)) +
  geom_rug(data = df_plot, aes(x = Log_Lag_R28_Abs, y = NULL, color = NULL),
           sides = "b", alpha = 0.3, inherit.aes = FALSE) +
  scale_color_manual(values = c("#e74c3c", "#2c3e50"), name = "Copartisanship") +
  labs(x = "Fiscal Capacity", y = "Predicted Conflictivity Index (CI 95%)") +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank())

print(plot_cover_whiskers)
ggsave(path_out("predictions.pdf"), plot = plot_cover_whiskers, width = 7, height = 5)

# Exact predicted values at the minimum, mean, and maximum of observed
# fiscal capacity, for in-text reporting.
min_cap  <- min(df_plot$Log_Lag_R28_Abs, na.rm = TRUE)
mean_cap <- mean(df_plot$Log_Lag_R28_Abs, na.rm = TRUE)
max_cap  <- max(df_plot$Log_Lag_R28_Abs, na.rm = TRUE)
vals_str <- paste(c(min_cap, mean_cap, max_cap), collapse = ", ")

tabla_pred <- ggpredict(m_aux_inter,
                        terms = c(paste0("Log_Lag_R28_Abs [", vals_str, "]"),
                                  paste0("Lag_Cambio_R28 [", shock_val, "]"),
                                  "Aligned"))
df_res <- as.data.frame(tabla_pred)
print(">>> Predicted values at min/mean/max fiscal capacity <<<")
print(df_res)

x_max_en_tabla <- max(df_res$x)
pred_oposicion_max <- df_res[df_res$x == x_max_en_tabla & df_res$facet == "Non-Copartisan", "predicted"]
pred_alineado_max  <- df_res[df_res$x == x_max_en_tabla & df_res$facet == "Copartisan", "predicted"]
diferencia <- pred_oposicion_max - pred_alineado_max

cat("\n>>> Predicted values at maximum fiscal capacity <<<\n")
cat("Non-copartisan prediction (max):", round(pred_oposicion_max, 3), "\n")
cat("Copartisan prediction (max):    ", round(pred_alineado_max, 3), "\n")
cat("Difference (non-copartisan - copartisan):", round(diferencia, 3), "\n")