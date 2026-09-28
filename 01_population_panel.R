# =============================================================================
# Script 01 — Population Panel Construction (CONAPO)
# =============================================================================
# Position in pipeline: 1 of N
# Purpose: Reads raw CONAPO population estimates by state and builds a
#   balanced state-year population panel (1990-2024) for use as a control
#   variable in the main analysis.
# Input:   Poblacion_01.xlsx (raw CONAPO population table, states x census
#   years, header skipped in first 6 rows)
# Output:  Poblacion_Estados_CONAPO.csv (state-year panel, 1990-2024)
# =============================================================================

library(tidyverse)
library(readxl)
library(zoo)
library(stringi)
if (!exists("path_raw")) source(file.path("R", "paths.R"))


# -----------------------------------------------------------------------------
# Load and reshape raw data
# -----------------------------------------------------------------------------
# CONAPO reports population at census/count years only (1990, 1995, 2000,
# 2005, 2010, 2020); the 2015 intercensal survey is excluded, as it is not
# methodologically comparable to the census years.

raw_pop <- read_excel(path_raw("Poblacion_01.xlsx"), skip = 6, col_names = FALSE)
raw_pop <- raw_pop[, 1:8]
colnames(raw_pop) <- c("ENTIDAD", "TIPO", "1990", "1995", "2000", "2005", "2010", "2020")

df_pop_clean <- raw_pop %>%
  mutate(ENTIDAD = zoo::na.locf(ENTIDAD, na.rm = FALSE)) %>%
  filter(TIPO == "Total") %>%
  filter(!str_detect(ENTIDAD, "Estados Unidos Mexicanos")) %>%
  select(-TIPO)

df_long <- df_pop_clean %>%
  pivot_longer(cols = -ENTIDAD, names_to = "ANIO", values_to = "POBLACION") %>%
  mutate(
    ANIO = as.numeric(ANIO),
    POBLACION = as.numeric(POBLACION)
  )

# -----------------------------------------------------------------------------
# Balance the panel and fill missing years
# -----------------------------------------------------------------------------
# Interior gaps (2010-2020) are linearly interpolated via na.approx(). Years
# after the last observed census (2021-2024) are held constant at the 2020
# level (carry-forward via fill()), not extrapolated, since CONAPO's most
# recent published estimate for our panel's final years is the 2020 count.

df_final_poblacion <- df_long %>%
  group_by(ENTIDAD) %>%
  complete(ANIO = 1990:2024) %>%
  mutate(POBLACION = zoo::na.approx(POBLACION, na.rm = FALSE, rule = 1)) %>%
  fill(POBLACION, .direction = "down") %>%
  mutate(
    ENTIDAD = str_to_lower(ENTIDAD),
    ENTIDAD = stri_trans_general(ENTIDAD, "Latin-ASCII"),
    ENTIDAD = str_trim(ENTIDAD)
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------
# Verification and export
# -----------------------------------------------------------------------------
print("--- Checking 2021-2024 carry-forward fill (Aguascalientes) ---")
tail(df_final_poblacion %>% filter(ENTIDAD == "aguascalientes"), 6)

write_csv(df_final_poblacion, path_data("Poblacion_Estados_CONAPO.csv"))