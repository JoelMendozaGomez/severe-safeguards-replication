# =============================================================================
# 00_run_all.R — Master replication script
# =============================================================================
# Severe Safeguards and Fiscal Capacity: The Political Economy of
# Intergovernmental Retaliation. Publius: The Journal of Federalism.
#
# Reproduces every table and figure in the article and its appendices, from
# the raw input files shipped in this archive through to the final estimates.
#
# HOW TO RUN
#   Rscript 00_run_all.R
# or, from an interactive R session with this directory as the working
# directory:
#   source("00_run_all.R")
#
# Expected runtime: 3-6 minutes.
#
# The scripts must run in one session and in order: Scripts 06 and 07 use
# objects created by Scripts 04 and 05 (the PCA model, the component variance
# weights, and the estimated panel models), so running them separately will
# fail. This script is the intended entry point.
#
# Everything is written to the working directory; a full console transcript
# goes to output/run_all_log.txt.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Locale
# -----------------------------------------------------------------------------
# Several source files store accented Spanish state names, and the scripts
# match them against state-name literals. Under a non-UTF-8 collation (for
# example LC_CTYPE=C, the default in many containers and batch schedulers)
# those comparisons can fail silently. Script 03 now transliterates both
# sides to ASCII and stops on an unmatched governor, so the pipeline is
# locale-invariant; this call simply removes the ambiguity up front.
invisible(suppressWarnings(
  Sys.setlocale("LC_CTYPE", Sys.getenv("R_REPL_LOCALE", unset = "C.UTF-8"))
))
options(stringsAsFactors = FALSE, width = 200)

# Directory layout: data-raw/ (sources), data/ (built datasets), output/ (results).
source(file.path("R", "paths.R"))


# -----------------------------------------------------------------------------
# 1. Dependencies
# -----------------------------------------------------------------------------
# gtrendsR is deliberately absent from this list. Script 03 retrieves Google
# Trends data only for states missing from trends_gob_federal_v2.csv, and the
# archive ships the complete cache for all 32 states, so no live API call is
# made. See the README on why the cache, rather than live retrieval, is the
# reproducible input.
required <- c(
  "tidyverse", "readxl", "zoo", "stringi", "lubridate",   # data preparation
  "plm", "sandwich", "lmtest", "car",                      # panel estimation
  "sensemakr",                                             # sensitivity analysis
  "factoextra",                                            # PCA diagnostics
  "stargazer", "knitr", "scales", "ggeffects"              # tables and figures
)

missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing required packages: ", paste(missing, collapse = ", "),
       "\nInstall them with:\n  install.packages(c(",
       paste(sprintf('"%s"', missing), collapse = ", "), "))")
}

cat("All", length(required), "required packages are available.\n")


# -----------------------------------------------------------------------------
# 2. Input check
# -----------------------------------------------------------------------------
efipem <- list.files(path = path_raw(),
                     pattern = "^efipem_(estatal|cdmx)_anual_tr_cifra_\\d{4}\\.csv$")
inputs <- c(
  "inpc_anual.csv",
  "Poblacion_01.xlsx",
  "PARLAMENTOS.xlsx",
  "Federalism conflicts on the Constitutionality of Laws Dataset  Version 1.3 .csv",
  "trends_gob_federal_v2.csv"
)
absent <- inputs[!file.exists(path_raw(inputs))]
if (length(absent) > 0) {
  stop("Missing input file(s) in ", path_raw(), "/: ", paste(absent, collapse = ", "))
}
if (length(efipem) != 72) {
  stop(sprintf("Expected 72 EFIPEM files (36 years x 2 series, 1989-2024); found %d.",
               length(efipem)))
}
cat("All input files present (", length(efipem), "EFIPEM files +",
    length(inputs), "others ).\n\n")



# -----------------------------------------------------------------------------
# 3. Run the pipeline
# -----------------------------------------------------------------------------
scripts <- c(
  "01_population_panel.R",   # CONAPO population panel
  "02_fiscal_panel.R",       # EFIPEM federal transfer panel (Ramo 28/33, Convenios, FEIEF)
  "02b_ownsource_direct.R",  # EFIPEM direct own-source revenue panel
  "03_political_panel.R",    # controversies, party, alignment, Google Trends rhetoric
  "04_ici_construction.R",   # PCA and the Intergovernmental Conflict Index
  "05_main_analysis.R",      # main TWFE models, robustness, sensitivity, figure
  "06_pca_diagnostics.R",    # component-exclusion checks
  "07_export_artifacts.R"    # artifact export and verification
)

log_path <- path_out("run_all_log.txt")
con <- file(log_path, open = "wt")
sink(con, split = TRUE)
sink(con, type = "message")

t_start <- Sys.time()
cat("=========================================================================\n")
cat("Replication run started:", format(t_start, "%Y-%m-%d %H:%M:%S %Z"), "\n")
cat("R version:", R.version.string, "\n")
cat("Working directory:", getwd(), "\n")
cat("=========================================================================\n")

for (sc in scripts) {
  cat("\n\n#########################################################################\n")
  cat("##  ", sc, "\n")
  cat("#########################################################################\n\n")
  t0 <- Sys.time()
  source(sc, echo = FALSE, local = FALSE)
  cat(sprintf("\n[%s completed in %.1f s]\n", sc,
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

cat("\n\n=========================================================================\n")
cat(sprintf("Replication run finished in %.1f minutes.\n",
            as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
cat("Verification summary: ", path_out("VERIFICATION.txt"), "\n", sep = "")
cat("Full transcript:      ", log_path, "\n", sep = "")
cat("=========================================================================\n")

sink(type = "message")
sink()
close(con)

cat("\nDone. See ", path_out("VERIFICATION.txt"), " for the reproduction check and\n",
    log_path, " for the full transcript, including every LaTeX table.\n", sep = "")
