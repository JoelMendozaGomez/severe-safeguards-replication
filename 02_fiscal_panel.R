# =============================================================================
# Script 02 — Fiscal Transfers Panel Construction (INEGI EFIPEM)
# =============================================================================
# Position in pipeline: 2 of N
# Purpose: Reads raw INEGI EFIPEM state-level public finance CSVs (one file
#   per year), extracts and classifies the federal transfer categories used
#   throughout the analysis (Ramo 28 / discretionary, Ramo 33 / earmarked,
#   Convenios, FEIEF), deflates all monetary figures to constant 2024 pesos,
#   and builds a long-format state-year fiscal panel.
# Input:   efipem_estatal_anual_tr_cifra_YYYY.csv and
#   efipem_cdmx_anual_tr_cifra_YYYY.csv (one per year, raw INEGI EFIPEM
#   income tables); inpc_anual.csv (annual deflator series, see below)
# Output:  fiscales_estados_long.csv (state-year fiscal panel, wide format
#   across transfer categories)
# =============================================================================

library(tidyverse)
library(stringr)
library(stringi)
if (!exists("path_raw")) source(file.path("R", "paths.R"))


# -----------------------------------------------------------------------------
# State catalog (CVEGEO -> state name, INEGI numbering)
# -----------------------------------------------------------------------------
catalogo_estados <- tibble(
  CVEGEO = 1:32,
  ENTIDAD_INEGI = c(
    "Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
    "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua",
    "Ciudad de México", "Durango", "Guanajuato", "Guerrero",
    "Hidalgo", "Jalisco", "México", "Michoacán de Ocampo",
    "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
    "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
    "Sinaloa", "Sonora", "Tabasco", "Tamaulipas",
    "Tlaxcala", "Veracruz de Ignacio de la Llave", "Yucatán", "Zacatecas"
  )
)

# -----------------------------------------------------------------------------
# Locate raw input files (one per year, states + Mexico City)
# -----------------------------------------------------------------------------
archivos <- list.files(
  path = path_raw(),
  pattern = "efipem_(estatal|cdmx)_anual_tr_cifra_\\d{4}\\.csv",
  full.names = TRUE
)

lista_ingresos <- list()

print("--- Processing: Chapter-level totals + explicit Convenios exceptions ---")

# -----------------------------------------------------------------------------
# Deflator series
# Source: Banco de México, SIE, series SP1 (national INPC, general index,
# base = 2nd fortnight of July 2018 = 100). Annual average 2000-2024,
# rebased to constant 2024 pesos (the panel's final year).
# -----------------------------------------------------------------------------

inpc_anual <- read_csv(path_raw("inpc_anual.csv"), show_col_types = FALSE)

# -----------------------------------------------------------------------------
# Main processing loop: read, deflate, filter, and classify each year's file
# -----------------------------------------------------------------------------
for (archivo in archivos) {
  
  anio_detectado <- as.integer(str_extract(basename(archivo), "\\d{4}"))
  temp <- read_csv(archivo, show_col_types = FALSE)
  names(temp) <- toupper(names(temp))
  
  temp_clean <- temp %>%
    filter(TEMA == "Ingresos") %>%
    mutate(
      CVEGEO = as.integer(CVEGEO),
      ANIO = if ("ANIO" %in% names(.)) ANIO else anio_detectado,
      MONTO_NOMINAL = if ("VALOR" %in% names(.)) VALOR else CIFRA,
      CATEGORIA_CLEAN = str_trim(CATEGORIA),
      DESC_CLEAN = str_trim(DESCRIPCION_CATEGORIA)
    ) %>%
    left_join(inpc_anual, by = "ANIO") %>%
    mutate(MONTO_REAL = MONTO_NOMINAL * Deflactor_a_2024) %>%
  filter(
    str_detect(CATEGORIA_CLEAN, regex("Cap.tulo", ignore_case = TRUE)) |
      DESC_CLEAN == "Total de ingresos" |
      str_detect(DESC_CLEAN, regex("Convenio", ignore_case = TRUE)) |
      str_detect(DESC_CLEAN, regex("Fondo de Extracci.n de Hidrocarburos", ignore_case = TRUE))
  ) %>%
  mutate(
    Category_Type = case_when(
      DESC_CLEAN == "Total de ingresos" ~ "Total_Oficial",
      str_detect(DESC_CLEAN, regex("Fondo de Extracci.n de Hidrocarburos", ignore_case = TRUE)) ~ "FEIEF",
      str_detect(DESC_CLEAN, regex("Participaciones", ignore_case = TRUE)) ~ "Ramo_28",
      str_detect(DESC_CLEAN, regex("Aportaciones", ignore_case = TRUE)) ~ "Ramo_33",
      str_detect(DESC_CLEAN, regex("Convenio", ignore_case = TRUE)) ~ "Convenios",
      TRUE ~ "Ingreso_Local_Otros"
    )
  ) %>%
    select(CVEGEO, ANIO, Category_Type, MONTO_REAL)
  
  lista_ingresos[[paste0("anio_", anio_detectado, "_", basename(archivo))]] <- temp_clean
}

# -----------------------------------------------------------------------------
# Aggregate, pivot to wide format, and derive final fiscal variables
# -----------------------------------------------------------------------------
df_fiscal_final <- bind_rows(lista_ingresos) %>%
  group_by(ANIO, CVEGEO, Category_Type) %>%
  summarise(Monto_Millones = sum(MONTO_REAL, na.rm = TRUE) / 1000000, .groups = "drop") %>%
  pivot_wider(
    names_from = Category_Type,
    values_from = Monto_Millones,
    values_fill = 0
  ) %>%
  mutate(
    Federal_Con_Convenios = Ramo_28 + Ramo_33 + Convenios,
    Federal_Constitucional = Ramo_28 + Ramo_33,
    Dependencia_Federal = if_else(Total_Oficial > 0, Federal_Constitucional / Total_Oficial, 0),
    Ingreso_Local_Residual = Total_Oficial - Federal_Con_Convenios,
    Ramo_28_ExFEIEF = Ramo_28 - FEIEF,
    FEIEF_Share_R28 = if_else(Ramo_28 > 0, FEIEF / Ramo_28, 0)
  ) %>%
  
# -----------------------------------------------------------------------
# Merge state names and standardize labels
# -----------------------------------------------------------------------
left_join(catalogo_estados, by = "CVEGEO") %>%
  mutate(
    ENTIDAD_CLEAN = stri_trans_general(str = ENTIDAD_INEGI, id = "Latin-ASCII"),
    ENTIDAD = case_when(
      ENTIDAD_CLEAN == "Coahuila de Zaragoza" ~ "Coahuila",
      ENTIDAD_CLEAN == "Michoacan de Ocampo" ~ "Michoacan",
      ENTIDAD_CLEAN == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
      ENTIDAD_CLEAN == "Mexico" ~ "Mexico",
      ENTIDAD_CLEAN == "Ciudad de Mexico" ~ "Mexico City",
      ENTIDAD_CLEAN == "Nuevo Leon" ~ "Nuevo Leon",
      ENTIDAD_CLEAN == "Queretaro" ~ "Queretaro",
      ENTIDAD_CLEAN == "San Luis Potosi" ~ "San Luis Potosi",
      ENTIDAD_CLEAN == "Yucatan" ~ "Yucatan",
      TRUE ~ ENTIDAD_CLEAN
    )
  ) %>%
  select(ANIO, ENTIDAD, Total_Oficial, Ramo_28, Ramo_28_ExFEIEF, FEIEF, FEIEF_Share_R28,
         Ramo_33, Convenios, Ingreso_Local_Residual, Dependencia_Federal)

# -----------------------------------------------------------------------------
# Verification and export
# -----------------------------------------------------------------------------
print("--- Federal dependence summary (Ramo_28 + Ramo_33 / Total) ---")
summary(df_fiscal_final$Dependencia_Federal)

write_csv(df_fiscal_final, path_data("fiscales_estados_long.csv"))