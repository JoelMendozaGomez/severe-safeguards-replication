# Replication package

**Severe Safeguards and Fiscal Capacity: The Political Economy of Intergovernmental Retaliation**
*Publius: The Journal of Federalism*

This archive reproduces every table, figure, and in-text statistic in the article and its appendices, from the raw source files included here through to the final estimates. Nothing is pre-computed: the derived datasets shipped alongside the scripts are regenerated when the pipeline runs, and a verification step compares the result against the published figures.

---

## Quick start

```
Rscript 00_run_all.R
```

Runtime is roughly 3–6 minutes. Two files report the outcome:

| File | Contents |
|---|---|
| `VERIFICATION.txt` | Each figure reported in the article, next to the value this run produced, with a pass/fail flag. |
| `output/run_all_log.txt` | Complete console transcript, including every LaTeX regression table in the form used in the paper. |

`00_run_all.R` is the intended entry point. The scripts must run **in one session and in order**: Scripts 06 and 07 use objects created by Scripts 04 and 05 — the fitted PCA model, the component variance weights, and the estimated panel models — so running them in isolation will fail.

### Requirements

R ≥ 4.1 and the following packages:

```r
install.packages(c(
  "tidyverse", "readxl", "zoo", "stringi", "lubridate",
  "plm", "sandwich", "lmtest", "car", "sensemakr",
  "factoextra", "stargazer", "knitr", "scales", "ggeffects"
))
```

`00_run_all.R` checks for all of them before doing any work and names any that are missing. Developed and verified under R 4.3.3; `sessionInfo.txt` records the exact versions of the verified run.

`gtrendsR` is **not** required — see *Google Trends* below.

---

## Pipeline

| Script | Produces | Purpose |
|---|---|---|
| `01_population_panel.R` | `Poblacion_Estados_CONAPO.csv` | State-year population panel from CONAPO census counts. |
| `02_fiscal_panel.R` | `fiscales_estados_long.csv` | Federal transfer panel from INEGI EFIPEM: Ramo 28, Ramo 33, Convenios, FEIEF; deflated to constant 2024 pesos. |
| `02b_ownsource_direct.R` | `ownsource_direct_panel.csv` | Direct own-source revenue: the Impuestos, Derechos, Productos and Aprovechamientos chapters, summed. |
| `03_political_panel.R` | `Base_Final_Publius_Definitiva.csv` | Constitutional controversies, presidential and gubernatorial party, co-partisan alignment, Google Trends rhetoric. |
| `04_ici_construction.R` | `Base_Tesis_MASTER_Weighted_PCA.csv`, PCA figures | Merges the panels and constructs the Intergovernmental Conflict Index by variance-weighted PCA. |
| `05_main_analysis.R` | Table 1 and all robustness tables, `predictions.pdf` | Main TWFE estimates, Random Effects comparison, FEIEF and own-source robustness, Ramo 28 exogeneity diagnostic, sensitivity analysis, marginal-effects figure. |
| `06_pca_diagnostics.R` | Component-exclusion tables, `pc3_fiscal_check_results.csv` | Re-estimates the main specification dropping each principal component in turn. |
| `07_export_artifacts.R` | The artifact files below, `VERIFICATION.txt` | Exports every intermediate quantity needed to rebuild the index, and verifies the published figures. |

### Index reproduction artifacts

Reviewer 3 asked that a reader be able to reproduce the ICI from its constituent indicators through to the final state-year index, and then reproduce the regressions. Script 07 exports each step:

| File | Contents |
|---|---|
| `pca_standardization.csv` | The centering and scaling constants (mean and SD per indicator), so the standardized inputs can be rebuilt exactly. |
| `pca_loadings.csv` | The full rotation matrix, both raw and sign-oriented (see *Sign orientation*). |
| `pca_eigenvalues.csv` | Eigenvalues, variance shares, cumulative variance, and the weights \(w_k\) used in the index. |
| `pca_contributions.csv` | Each indicator's percentage contribution to each component. |
| `ici_state_year_scores.csv` | The state-year panel: raw indicators, component scores, the weighted raw index, and the final rescaled ICI. |

---

## The index

The Intergovernmental Conflict Index is a variance-weighted sum of **all three** principal components of three standardized indicators:

$$\text{ICI}^{\text{raw}}_{it} = \sum_{k=1}^{3} w_k \cdot \text{PC}_{k,it}, \qquad w_k = \frac{\lambda_k}{\sum_j \lambda_j}$$

where \(\lambda_k\) is the eigenvalue of component \(k\). The raw index is then min–max rescaled to \([0,1]\):

$$\text{ICI}_{it} = \frac{\text{ICI}^{\text{raw}}_{it} - \min(\text{ICI}^{\text{raw}})}{\max(\text{ICI}^{\text{raw}}) - \min(\text{ICI}^{\text{raw}})}$$

Retaining all three components means no variance is discarded and no retention rule has to be justified. The three inputs are logged rhetoric intensity, the count of constitutional controversies, and Federalist Alliance rupture; `CODEBOOK.md` documents each.

**Sign orientation.** Principal component signs are arbitrary. Each component is oriented so higher values indicate more conflict, anchored on a theoretically central variable: rhetoric for PC1, litigation count for PC2, and the net loading direction for PC3. In the verified run no component required flipping, so the oriented and raw loadings coincide; the orientation logic is retained because it is what makes the index well-defined independent of the arbitrary sign `prcomp` happens to return. Both versions are in `pca_loadings.csv`.

**Rescaling is sample-dependent.** The min–max step is computed over the 864-row panel produced by Script 04. Any subset re-scaled on its own will not be numerically identical — a consideration when reusing `ici_state_year_scores.csv` for a different sample.

---

## Data sources

| Input | Source |
|---|---|
| `efipem_estatal_anual_tr_cifra_YYYY.csv`, `efipem_cdmx_anual_tr_cifra_YYYY.csv` (72 files, 1989–2024) | INEGI, *Estadística de Finanzas Públicas Estatales y Municipales* (EFIPEM). |
| `inpc_anual.csv` | Banco de México, SIE series SP1 (national INPC, general index, base = second fortnight of July 2018 = 100), annual averages, rebased to constant 2024 pesos. |
| `Poblacion_01.xlsx` | CONAPO population counts by state, census and count years. |
| `PARLAMENTOS.xlsx` (sheet 4) | Governing party by state-year. |
| `Federalism conflicts on the Constitutionality of Laws Dataset Version 1.3 .csv` | Rivera León, constitutional controversies dataset. |
| `trends_gob_federal_v2.csv` | Google Trends relative search volume, `"[State] Gobierno Federal"`, retrieved via `gtrendsR`. Cached; see below. |

### Google Trends

Script 03 builds the rhetoric indicator from Google Trends. **This archive ships the complete cached series for all 32 states**, and the script skips any state already present in the cache, so a replication run makes no network request and `gtrendsR` need not be installed.

This is deliberate rather than a convenience. Google Trends returns a relative, sampled index that is **not guaranteed to be identical across query dates**, so live retrieval is not a reproducible input: re-querying would produce values that differ from those underlying the published estimates. The cache is the reproducible artifact, and it is the input we report. A reader who deletes it and installs `gtrendsR` can re-retrieve the series, but should expect the rhetoric indicator — and therefore the ICI — to differ somewhat from the published version.

### Panel coverage

The raw fiscal data span 1989–2024 and the political panel 1998–2024, so the merged panel covers **1998–2024 for 32 states** (864 state-years). The estimation sample is **N = 700**: the first two years are lost to the lag structure (the models use both a one-period lag and a lagged first difference), and a small number of further state-years drop out on missing covariates. The article reports results for 2000–2024, which is this estimation window.

---

## Notes for replicators

**Locale.** Several source files store accented Spanish state names, which the scripts match against state-name literals. Under a non-UTF-8 collation — `LC_CTYPE=C`, the default in many containers and batch schedulers — such comparisons can fail *silently*. Script 03 therefore transliterates both sides to ASCII before matching and stops with an explicit error if any state-year is left without a governing party, rather than letting a downstream `replace_na()` quietly recode the alignment variable. The pipeline has been verified to produce identical results under both `C.UTF-8` and `C`. `00_run_all.R` also sets `LC_CTYPE` up front; override it with the `R_REPL_LOCALE` environment variable if needed.

**Masked `lag()`.** Script 04 qualifies every call as `dplyr::lag()`. A bare `lag()` resolves to `stats::lag` whenever `plm` is attached, and `stats::lag` applied to a plain numeric vector returns the vector unchanged with a time-series attribute rather than a shifted vector — so `x - lag(x)` evaluates to 0 for every row, with no error and no warning. Running Script 04 in a session where Script 05 or 06 had already been sourced therefore produced all-zero change variables. Those columns feed a finiteness filter but no estimate, so no published result was affected; the qualified calls remove the trap. Script 05 already qualified its own calls.

**Column and object names are in Spanish** (`ENTIDAD`, `ANIO`, `Ramo_28`, `Indice_Conflictividad_W`). They are load-bearing: the scripts merge on them. `CODEBOOK.md` gives the English gloss for every one.

**Two scripts from the working project are deliberately excluded**: an empty placeholder file, and an exploratory script that produces no quantity reported in the article. Every script in this archive is on the path from raw data to a published result.

**Own-source revenue.** The article reports own-source revenue built **directly** from EFIPEM's tax, fee, duty and *aprovechamiento* chapters (Script 02b). An earlier draft derived it as a residual — total income minus Ramo 28, Ramo 33 and Convenios — which by construction absorbed public debt and financing inflows alongside genuine local revenue. Script 02 still computes the residual measure (`Ingreso_Local_Residual`) and Script 05 re-estimates the residual-based specifications immediately after the reported ones, labelled as superseded, so the two can be compared directly. Only the direct-measure results appear in the paper.

**President Party fixed effects** are included in the main TWFE specification, matching the published Table 1. They are collinear with Year fixed effects in this panel, since Mexico's presidential party is a deterministic function of year, and their inclusion does not change any coefficient; Section 7 of Script 05 re-estimates without them and confirms this. They are retained so that the script and the published table specify the same model.

---

## Licence and citation

Code is released under the MIT licence (`LICENSE`); data under CC BY 4.0 (`LICENSE-DATA`). In short: anyone may reuse either, including commercially, provided they credit the source. The files in `data-raw/` originate with third parties — INEGI, CONAPO, Banco de México, Google, and the constitutional-controversies dataset — and remain subject to those parties' terms; `LICENSE-DATA` lists them.

Please cite both the article and this repository; `CITATION.cff` carries the metadata.

## Contents

```
00_run_all.R                     master script — run this
01_population_panel.R … 07_export_artifacts.R
R/paths.R                        directory layout helpers
README.md                        this file
CODEBOOK.md                      variable definitions and transformations
CITATION.cff                     citation metadata
LICENSE                          MIT, for the code
LICENSE-DATA                     CC BY 4.0, for the data

data-raw/                        sources, as obtained. Never written to.
  efipem_*.csv                   72 EFIPEM files (1989–2024)
  inpc_anual.csv                 INPC deflator
  Poblacion_01.xlsx              CONAPO population
  PARLAMENTOS.xlsx               governing party by state-year
  Federalism conflicts … .csv    constitutional controversies
  trends_gob_federal_v2.csv      cached Google Trends series

data/                            built by the pipeline
  ici_state_year_scores.csv      the state-year index
  pca_loadings.csv               rotation matrix
  pca_eigenvalues.csv            eigenvalues and index weights
  pca_standardization.csv        centering and scaling constants
  pca_contributions.csv          indicator contributions
  ownsource_direct_panel.csv     direct own-source revenue
  fiscales_estados_long.csv      federal transfer panel
  Poblacion_Estados_CONAPO.csv   population panel
  Base_Final_Publius_*.csv       political panel
  Base_Tesis_MASTER_*.csv        merged analysis panel

output/                          results
  VERIFICATION.txt               published vs. reproduced
  run_all_log.txt                full transcript, all LaTeX tables
  sessionInfo.txt                package versions
  predictions.pdf                marginal-effects figure
  Figura*_*.pdf / .png           PCA figures
  pc3_fiscal_check_results.csv   component tercile table
```

`data/` and `output/` are tracked in the repository so the index and the verification report can be inspected without running anything; `00_run_all.R` regenerates both in place.
