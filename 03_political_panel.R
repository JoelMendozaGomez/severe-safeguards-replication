# =============================================================================
# Script 03 — Master Panel Construction: Legal, Political, and Rhetoric Data
# =============================================================================
# Position in pipeline: 3 of N
# Purpose: Builds the state-year panel (1998-2024) underlying the political
#   confrontation dimension of the Intergovernmental Conflict Index (ICI).
#   Combines three data sources: (i) constitutional controversies filed by
#   states against the federal government, (ii) political control variables
#   (Federalist Alliance membership, presidential party, governor party,
#   and co-partisan alignment), and (iii) Google Trends search intensity for
#   state-vs-federal-government rhetoric.
# Input:   "Federalism conflicts on the Constitutionality of Laws Dataset
#   Version 1.3.csv" (Rivera Leon dataset of constitutional controversies);
#   PARLAMENTOS.xlsx, sheet 4 (governor party by state-year); Google Trends
#   data retrieved live via the gtrendsR API (cached incrementally in
#   trends_gob_federal_v2.csv)
# Output:  Base_Final_Publius_Definitiva.csv (state-year panel, 1998-2024,
#   columns ENTIDAD, ANIO, and the political/rhetoric variables used in
#   Script 04's PCA index construction)
# Note:    Section 4 queries the live Google Trends API and is therefore
#   time-of-retrieval dependent; Google Trends does not guarantee identical
#   output across query dates. Re-running this script from scratch may
#   produce marginally different Rhetoric_Intensity values than those
#   reported in the paper if the cached trends_gob_federal_v2.csv is not
#   reused.
# =============================================================================

library(tidyverse)
library(readxl)
# gtrendsR is required ONLY if trends_gob_federal_v2.csv is incomplete. The
# archive ships the complete cache (all 32 states), so the live API is never
# called and the package does not depend on gtrendsR being installed.
if (!requireNamespace("gtrendsR", quietly = TRUE)) {
  gtrends <- function(...) stop(
    "Google Trends cache miss and gtrendsR is not installed. Install gtrendsR ",
    "to retrieve the missing states, but note that Google Trends is not ",
    "deterministic across query dates and values may differ from those in the paper."
  )
} else {
  library(gtrendsR)
}
library(lubridate)
library(stringi)
if (!exists("path_raw")) source(file.path("R", "paths.R"))


# -----------------------------------------------------------------------------
# 1. Legal data: constitutional controversies (dependent variable component)
# -----------------------------------------------------------------------------
# Filters the Rivera Leon dataset to cases where a state is plaintiff and the
# federal government is defendant. A subset of case IDs could not be matched
# to a state through the dataset's own fields (source PDFs were unrecoverable
# for automatic parsing); these are corrected manually below via a lookup
# table built from case-by-case verification against the original judgments.

df <- read_csv(path_raw("Federalism conflicts on the Constitutionality of Laws Dataset  Version 1.3 .csv"), show_col_types = FALSE) %>%
  filter(Plaintiff == "S" & Defendant == "F")

ids_manuales <- c("5/2001", "33/2002", "23/2003", "54/2009", "71/2009",
                  "13/2008", "69/2009", "32/2000", "4/2001", "11/2000")
estados_manuales <- c("Mexico City", "Mexico City", "Chihuahua", "Jalisco", "Mexico City",
                      "Mexico City", "Mexico City", "Mexico City", "Mexico City", "Mexico City")

diccionario_casos <- data.frame(Case_ID_Clean = ids_manuales, State_Name = estados_manuales, stringsAsFactors = FALSE)

df_clean <- df %>%
  mutate(Case_ID_Clean = str_replace(`Judgement ID number`, "--", "/")) %>%
  left_join(diccionario_casos, by = "Case_ID_Clean")

# -----------------------------------------------------------------------------
# 2. Panel skeleton (state-year) and conflict counts
# -----------------------------------------------------------------------------
# Builds a balanced state-year grid (32 states x 1998-2024) and merges in the
# annual count of constitutional controversies per state, with zero-filling
# for state-years with no filed cases.

all_states <- c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
                "Coahuila", "Colima", "Chiapas", "Chihuahua", "Mexico City", "Durango",
                "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Mexico", "Michoacan",
                "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla", "Queretaro",
                "Quintana Roo", "San Luis Potosi", "Sinaloa", "Sonora", "Tabasco",
                "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatan", "Zacatecas")

years <- 1998:2024
skeleton <- expand.grid(State_Name = all_states, Year = years)

conteo_conflictos <- df_clean %>%
  filter(!is.na(State_Name)) %>%
  group_by(State_Name, `Year Filed`) %>%
  summarise(n_conflicts = n(), .groups = "drop") %>%
  rename(Year = `Year Filed`)

df_final_panel <- skeleton %>%
  left_join(conteo_conflictos, by = c("State_Name", "Year")) %>%
  mutate(n_conflicts = replace_na(n_conflicts, 0))

# -----------------------------------------------------------------------------
# 3. Political control variables
# -----------------------------------------------------------------------------
# (A) Federalist Alliance membership: binary indicator for the ten-state
#     coalition that withdrew from CONAGO during 2020-2021 (Level 3 proxy
#     in the ICI).
# (B) Presidential party by year (PRI/PAN/PRI/MORENA sexenios, 1998-2024).
# (C) Governor party by state-year, from the PARLAMENTOS source, with state
#     name harmonization to match the panel's naming convention.
# (D) Co-partisan alignment (Aligned): equals 1 when governor and president
#     share the same party. Independent/MC governors are coded as not
#     aligned by construction. PAN-PRD state-years are coded as not aligned
#     by default, reflecting the general absence of formal PAN-PRD
#     legislative coalitions at the federal level over most of the panel;
#     this is a simplifying assumption rather than a case-by-case
#     coalition check.

alianza_members <- c("Aguascalientes", "Chihuahua", "Coahuila", "Colima", "Durango",
                     "Guanajuato", "Jalisco", "Michoacan", "Nuevo Leon", "Tamaulipas")

df_final_panel <- df_final_panel %>%
  mutate(Rupture_Alliance_Period = if_else(State_Name %in% alianza_members & Year >= 2020 & Year <= 2021, 1, 0))

df_final_panel <- df_final_panel %>%
  mutate(President_Party = case_when(
    Year <= 2000 ~ "PRI", Year >= 2001 & Year <= 2012 ~ "PAN",
    Year >= 2013 & Year <= 2018 ~ "PRI", Year >= 2019 ~ "MORENA", TRUE ~ NA_character_
  ))

raw_parlamentos <- read_excel(path_raw("PARLAMENTOS.xlsx"), sheet = 4)

df_parlamentos_clean <- raw_parlamentos %>%
  pivot_longer(cols = -Estados, names_to = "Year_Str", values_to = "Governor_Party") %>%
  mutate(
    Year = as.numeric(Year_Str),
    # Accent-proof harmonisation. The source file stores accented state names;
    # comparing them against accented literals in this script fails silently
    # under a non-UTF-8 locale (e.g. LC_CTYPE=C, common in containers and on
    # HPC), which previously left seven states with a missing Governor_Party
    # and, via the replace_na() below, mis-set Aligned to 0 with no warning.
    # Transliterating both sides to ASCII first makes the join locale-invariant.
    Estados_ASCII = stringi::stri_trans_general(str_trim(Estados), "Latin-ASCII"),
    State_Name = case_when(
      Estados_ASCII %in% c("Ciudad de Mexico", "Distrito Federal", "DF", "CDMX") ~ "Mexico City",
      Estados_ASCII == "Nuevo Leon" ~ "Nuevo Leon",
      Estados_ASCII == "Michoacan" ~ "Michoacan",
      Estados_ASCII == "Yucatan" ~ "Yucatan",
      Estados_ASCII == "San Luis Potosi" ~ "San Luis Potosi",
      Estados_ASCII == "Queretaro" ~ "Queretaro",
      Estados_ASCII == "Estado de Mexico" ~ "Mexico",
      TRUE ~ Estados_ASCII
    )
  ) %>% select(State_Name, Year, Governor_Party)

df_final_panel <- df_final_panel %>%
  left_join(df_parlamentos_clean, by = c("State_Name", "Year")) %>%
  mutate(
    Aligned = if_else(President_Party == Governor_Party, 1, 0),
    Aligned = if_else(Governor_Party %in% c("MC", "Independiente", "INDEPENDIENTE"), 0, Aligned),
    Aligned = replace_na(Aligned, 0)
  )

# Guard: replace_na() above silently codes an unmatched governor as
# non-aligned, so a broken join would corrupt Aligned without any error.
# Stop instead.
.n_gov_na <- sum(is.na(df_final_panel$Governor_Party))
if (.n_gov_na > 0) {
  stop(sprintf(
    paste0("PARLAMENTOS join left %d state-years without a Governor_Party ",
           "(states: %s). Aligned cannot be computed correctly. Check state-name ",
           "harmonisation and the locale (see notes above)."),
    .n_gov_na,
    paste(sort(unique(df_final_panel$State_Name[is.na(df_final_panel$Governor_Party)])),
          collapse = ", ")))
}

# -----------------------------------------------------------------------------
# 4. Google Trends: state-vs-federal-government search intensity (rhetoric)
# -----------------------------------------------------------------------------
# Queries Google Trends for the dyad "[State] Gobierno Federal" per state,
# aggregated to annual means, 1998-2024. Retrieval is incremental and
# resumable: completed states are cached in trends_gob_federal_v2.csv and
# skipped on re-run; only missing states trigger a live API call, with a
# 15-second delay between calls to respect rate limits.

temp_file <- path_raw("trends_gob_federal_v2.csv")

df_search_manual <- tibble(
  State_Name = all_states,
  Search_Term = c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
                  "Coahuila", "Colima", "Chiapas", "Chihuahua", "CDMX", "Durango",
                  "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Estado de México", "Michoacán",
                  "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro",
                  "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco",
                  "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")
)

if (!file.exists(temp_file)) { write_csv(data.frame(), temp_file) }

for (i in 1:nrow(df_search_manual)) {
  base_name <- df_search_manual$State_Name[i]
  term_raw <- df_search_manual$Search_Term[i]
  
  done <- tryCatch(read_csv(temp_file, show_col_types = FALSE), error = function(e) data.frame())
  
  if (nrow(done) > 0 && base_name %in% unique(done$State_Name)) {
    next
  }
  
  search_query <- paste(term_raw, "Gobierno Federal")
  message(paste("Retrieving missing state:", search_query))
  
  tryCatch({
    g_data <- gtrends(keyword = search_query, geo = "MX", time = "all")
    if (!is.null(g_data$interest_over_time)) {
      res <- g_data$interest_over_time %>%
        mutate(hits = as.numeric(replace(hits, hits == "<1", 0)), Year = year(date)) %>%
        filter(Year >= 1998 & Year <= 2024) %>%
        group_by(Year) %>% summarise(Rhetoric_Intensity = mean(hits, na.rm = TRUE)) %>%
        mutate(State_Name = base_name)
    } else {
      res <- data.frame(Year = 2020, Rhetoric_Intensity = 0, State_Name = base_name)
    }
    write_csv(res, temp_file, append = TRUE)
    Sys.sleep(15)
  }, error = function(e) message(paste("Error retrieving:", base_name)))
}

# -----------------------------------------------------------------------------
# 5. Final merge, renaming, and export
# -----------------------------------------------------------------------------
# Merges all three components into the master panel and renames the year and
# state identifier columns (Year -> ANIO, State_Name -> ENTIDAD) to match the
# naming convention used by the fiscal panel built in Script 02, which this
# panel is joined against in Script 04.

df_trends_final <- read_csv(temp_file, show_col_types = FALSE) %>%
  distinct(State_Name, Year, .keep_all = TRUE)

df_master <- df_final_panel %>%
  left_join(df_trends_final, by = c("State_Name", "Year")) %>%
  mutate(Rhetoric_Intensity = replace_na(Rhetoric_Intensity, 0)) %>%
  rename(
    ANIO = Year,
    ENTIDAD = State_Name
  )

print("--- Quality check: Jalisco (2018-2024) ---")
df_master %>%
  filter(ENTIDAD == "Jalisco" & ANIO >= 2018) %>%
  select(ENTIDAD, ANIO, Governor_Party, Rupture_Alliance_Period, Rhetoric_Intensity) %>%
  print()

write_csv(df_master, path_data("Base_Final_Publius_Definitiva.csv"))
print("Process complete. 'Base_Final_Publius_Definitiva.csv' created with columns 'ENTIDAD' and 'ANIO'.")