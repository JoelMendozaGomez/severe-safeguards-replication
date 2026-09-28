# -----------------------------------------------------------------------------
# paths.R — directory layout
# -----------------------------------------------------------------------------
# Three helpers keep the repository navigable without hard-coding paths in
# every script. Sourced by 00_run_all.R; each script also sources it if run on
# its own.
#
#   data-raw/   source files as obtained from INEGI, CONAPO, Banco de Mexico
#               and the constitutional-controversies dataset. Never written to.
#   data/       datasets built by the pipeline, including the state-year index.
#   output/     tables, figures, logs, and the verification report.
# -----------------------------------------------------------------------------

DIR_RAW  <- "data-raw"
DIR_DATA <- "data"
DIR_OUT  <- "output"

path_raw  <- function(...) if (missing(..1)) DIR_RAW  else file.path(DIR_RAW,  ...)
path_data <- function(...) if (missing(..1)) DIR_DATA else file.path(DIR_DATA, ...)
path_out  <- function(...) if (missing(..1)) DIR_OUT  else file.path(DIR_OUT,  ...)

for (.d in c(DIR_DATA, DIR_OUT)) dir.create(.d, showWarnings = FALSE, recursive = TRUE)
rm(.d)
