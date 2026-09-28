# =============================================================================
# Script 02b — Direct Own-Source Revenue Panel (INEGI EFIPEM)
# =============================================================================
# Position in pipeline: 2b of 7 (run after Script 02)
# Purpose: Builds a DIRECT measure of state own-source revenue by summing the
#   four chapter-level INEGI EFIPEM income categories that correspond to
#   genuinely state-generated revenue -- Impuestos (taxes), Derechos (fees),
#   Productos (income from state assets), and Aprovechamientos (other
#   non-tax revenue) -- rather than deriving it as a residual.
#
#   This replaces the residual-based measure used in earlier drafts
#   (Ingreso_Local_Residual = Total de ingresos - Ramo 28 - Ramo 33 -
#   Convenios), which by construction absorbed public debt and financing
#   inflows alongside genuine local revenue. The residual measure is still
#   built in Script 02 and is retained in Script 05 purely as a documented
#   point of comparison; the DIRECT measure is the one reported in the
#   paper (Appendix, Table: ownsource_robustness).
#
#   Deflation is identical to Script 02: nominal values are multiplied by
#   the annual INPC deflator to constant 2024 pesos, then expressed in
#   millions.
#
#   Chapter labels are stable: all four categories appear with byte-identical
#   labels in all 72 raw files (estatal + CDMX, 1989-2024). The script
#   verifies this and stops if a year is missing a category.
#
# Input:   efipem_(estatal|cdmx)_anual_tr_cifra_YYYY.csv (raw INEGI EFIPEM);
#          inpc_anual.csv (annual deflator, Banco de Mexico SIE series SP1)
# Output:  ownsource_direct_panel.csv (state-year panel: the four component
#          categories and their sum, constant 2024 pesos, millions)
# =============================================================================

library(tidyverse)
library(stringr)
library(stringi)
if (!exists("path_raw")) source(file.path("R", "paths.R"))


# -----------------------------------------------------------------------------
# State catalog (identical to Script 02, so ENTIDAD labels merge cleanly)
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
# The four own-source income chapters
# -----------------------------------------------------------------------------
# Matched on the ASCII transliteration of the label so the filter is
# locale-invariant (see the note in Script 03 on silent encoding failures).
CAPITULOS_PROPIOS <- c("Impuestos", "Derechos", "Productos", "Aprovechamientos")

inpc_anual <- read_csv(path_raw("inpc_anual.csv"), show_col_types = FALSE)

archivos <- list.files(
  path = path_raw(),
  pattern = "efipem_(estatal|cdmx)_anual_tr_cifra_\\d{4}\\.csv",
  full.names = TRUE
)

print("--- Building direct own-source revenue panel ---")

lista_propios <- list()

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
      CATEGORIA_CLEAN = stri_trans_general(str_trim(CATEGORIA), "Latin-ASCII"),
      DESC_CLEAN = stri_trans_general(str_trim(DESCRIPCION_CATEGORIA), "Latin-ASCII")
    ) %>%
    left_join(inpc_anual, by = "ANIO") %>%
    mutate(MONTO_REAL = MONTO_NOMINAL * Deflactor_a_2024) %>%
    # Chapter level only. Restricting to "Capitulo" is what prevents
    # double-counting: sub-concepts such as "Aprovechamientos de tipo
    # corriente" appear separately at the Concepto level and are already
    # contained in the chapter total.
    filter(
      str_detect(CATEGORIA_CLEAN, regex("Capitulo", ignore_case = TRUE)),
      DESC_CLEAN %in% CAPITULOS_PROPIOS
    ) %>%
    select(CVEGEO, ANIO, DESC_CLEAN, MONTO_REAL)

  # Guard: every file must contribute all four chapters.
  faltantes <- setdiff(CAPITULOS_PROPIOS, unique(temp_clean$DESC_CLEAN))
  if (length(faltantes) > 0) {
    stop(sprintf("File %s is missing own-source chapter(s): %s",
                 basename(archivo), paste(faltantes, collapse = ", ")))
  }

  lista_propios[[archivo]] <- temp_clean
}

# -----------------------------------------------------------------------------
# Aggregate to state-year, pivot, and sum
# -----------------------------------------------------------------------------
df_ownsource <- bind_rows(lista_propios) %>%
  group_by(ANIO, CVEGEO, DESC_CLEAN) %>%
  summarise(Monto_Millones = sum(MONTO_REAL, na.rm = TRUE) / 1000000,
            .groups = "drop") %>%
  pivot_wider(names_from = DESC_CLEAN, values_from = Monto_Millones,
              values_fill = 0) %>%
  mutate(
    OwnSourceRevenue_Direct = Impuestos + Derechos + Productos + Aprovechamientos
  ) %>%
  left_join(catalogo_estados, by = "CVEGEO") %>%
  mutate(
    ENTIDAD_CLEAN = stri_trans_general(str = ENTIDAD_INEGI, id = "Latin-ASCII"),
    ENTIDAD = case_when(
      ENTIDAD_CLEAN == "Coahuila de Zaragoza" ~ "Coahuila",
      ENTIDAD_CLEAN == "Michoacan de Ocampo" ~ "Michoacan",
      ENTIDAD_CLEAN == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
      ENTIDAD_CLEAN == "Ciudad de Mexico" ~ "Mexico City",
      TRUE ~ ENTIDAD_CLEAN
    )
  ) %>%
  select(ANIO, ENTIDAD, Impuestos, Derechos, Productos, Aprovechamientos,
         OwnSourceRevenue_Direct)

print("--- Direct own-source revenue: summary (millions, constant 2024 MXN) ---")
print(summary(df_ownsource$OwnSourceRevenue_Direct))
cat(sprintf("Rows: %d | States: %d | Years: %d-%d\n",
            nrow(df_ownsource), n_distinct(df_ownsource$ENTIDAD),
            min(df_ownsource$ANIO), max(df_ownsource$ANIO)))

write_csv(df_ownsource, path_data("ownsource_direct_panel.csv"))
