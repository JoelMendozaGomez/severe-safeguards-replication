# Codebook

**Severe Safeguards and Fiscal Capacity: The Political Economy of Intergovernmental Retaliation**

Every variable in the archive, with its construction. Names are in Spanish because the scripts merge on them; the English gloss is given for each.

All monetary values are in **millions of constant 2024 Mexican pesos** unless stated otherwise.

---

## 1. Identifiers

| Variable | Gloss | Definition |
|---|---|---|
| `ENTIDAD` | State | State name, ASCII-transliterated, with four shortened to their common form: `Coahuila` (de Zaragoza), `Michoacan` (de Ocampo), `Veracruz` (de Ignacio de la Llave), `Mexico City` (Ciudad de México). `Mexico` denotes the **State of México**, not the country. |
| `ANIO` | Year | Calendar year. |
| `ENTIDAD_KEY` | Join key | Internal only: `ENTIDAD` lowercased, transliterated, trimmed. Used to merge panels whose state-name conventions differ. |
| `CVEGEO` | INEGI state code | 1–32, INEGI numbering. Present in the raw files; dropped once names are merged. |

---

## 2. Deflation

`inpc_anual.csv` carries the annual deflator.

| Variable | Definition |
|---|---|
| `INPC_promedio_anual` | Annual average of the national INPC. Banco de México SIE series SP1, base = second fortnight of July 2018 = 100. |
| `Deflactor_a_2024` | `INPC(2024) / INPC(year)`. Multiplying a nominal amount by this converts it to constant 2024 pesos. |

Every monetary variable below is constructed as
`nominal × Deflactor_a_2024 / 1,000,000`,
i.e. real millions. This applies identically in Scripts 02 and 02b, so the transfer and own-source series are on the same scale.

---

## 3. Population — `Poblacion_Estados_CONAPO.csv`

| Variable | Gloss | Definition |
|---|---|---|
| `POBLACION` | Population | State population. |

**Transformation.** CONAPO reports population at census and count years only (1990, 1995, 2000, 2005, 2010, 2020); the 2015 intercensal survey is excluded as not methodologically comparable. Interior gaps are filled by **linear interpolation** (`zoo::na.approx`). Years after the last observed count (2021–2024) are held **constant at the 2020 level** by carry-forward — not extrapolated — since the 2020 count is CONAPO's most recent published figure for the panel's final years.

`Log_Poblacion` = `log(POBLACION)`, built in Script 05.

---

## 4. Fiscal transfers — `fiscales_estados_long.csv`

Built from the EFIPEM income tables, restricted to `TEMA == "Ingresos"`. Federal transfer categories are identified by their EFIPEM chapter labels; FEIEF and Convenios are picked up by explicit label match because they sit inside broader chapters.

| Variable | Gloss | Definition |
|---|---|---|
| `Total_Oficial` | Total state income | The reported "Total de ingresos" line. |
| `Ramo_28` | Discretionary transfers | *Participaciones federales* — non-conditional; the paper's main independent variable. |
| `Ramo_33` | Earmarked transfers | *Aportaciones federales* — legally tied to specified uses. |
| `Convenios` | Federal agreements | Negotiated transfers under federal–state agreements. |
| `FEIEF` | Hydrocarbon fund | *Fondo de Extracción de Hidrocarburos*. Classified by INEGI **inside** `Ramo_28`, not separately. |
| `Ramo_28_ExFEIEF` | Discretionary ex-oil | `Ramo_28 − FEIEF`. |
| `FEIEF_Share_R28` | Oil share | `FEIEF / Ramo_28`, or 0 when `Ramo_28 = 0`. |
| `Ingreso_Local_Residual` | Own-source (residual) | `Total_Oficial − (Ramo_28 + Ramo_33 + Convenios)`. **Superseded** — see §5. Retained only for the documented comparison in Script 05. |
| `Dependencia_Federal` | Federal dependence | `(Ramo_28 + Ramo_33) / Total_Oficial`, or 0 when total income is 0. Mean ≈ 0.86 in the estimation sample. |

---

## 5. Own-source revenue, direct — `ownsource_direct_panel.csv`

Built in Script 02b by summing four EFIPEM income **chapters**. Restricting to chapter level is what prevents double counting: sub-concepts such as "Aprovechamientos de tipo corriente" appear separately at the *Concepto* level and are already inside the chapter total. All four labels appear identically in all 72 raw files; the script stops if any year is missing one.

| Variable | Gloss |
|---|---|
| `Impuestos` | State taxes |
| `Derechos` | Fees and duties |
| `Productos` | Income from state assets |
| `Aprovechamientos` | Other non-tax revenue |
| `OwnSourceRevenue_Direct` | Sum of the four |

In the estimation sample this averages **8.05%** of total state income (median 7.22%, SD 4.01%). The superseded residual measure averaged 4.89% with a median of 0.44% and SD 7.56% — the low median being the symptom of the problem, since it implied that half of all state-years collected essentially nothing. The two measures correlate at only **0.254** in logs.

---

## 6. Political panel — `Base_Final_Publius_Definitiva.csv`

| Variable | Gloss | Definition |
|---|---|---|
| `n_conflicts` | Controversies filed | Annual count of constitutional controversies in which the state is plaintiff and the federal government defendant (Rivera León dataset, filtered `Plaintiff == "S" & Defendant == "F"`). Zero-filled for state-years with no filing. A small number of case IDs that could not be matched to a state through the dataset's own fields are assigned via a lookup table built by case-by-case verification against the original judgments; the table is in Script 03. |
| `Rupture_Alliance_Period` | Alliance rupture | 1 for the ten Federalist Alliance member states in 2020 and 2021; 0 otherwise. Members: Aguascalientes, Chihuahua, Coahuila, Colima, Durango, Guanajuato, Jalisco, Michoacán, Nuevo León, Tamaulipas. |
| `President_Party` | Presidential party | By sexenio: PRI (≤2000), PAN (2001–2012), PRI (2013–2018), MORENA (≥2019). |
| `Governor_Party` | Governing party | From `PARLAMENTOS.xlsx` sheet 4, matched on ASCII-transliterated state name. |
| `Aligned` | Co-partisanship | 1 when `Governor_Party == President_Party`. MC and independent governors are coded 0 by construction. PAN–PRD state-years are coded 0 by default, reflecting the general absence of formal PAN–PRD federal legislative coalitions over most of the panel — a simplifying assumption, not a case-by-case coalition check. |
| `Rhetoric_Intensity` | Search volume | Annual mean of monthly Google Trends relative search volume for `"[State] Gobierno Federal"`, geography Mexico. `"<1"` readings are set to 0. Zero-filled where the series is absent. |

Script 03 halts with an explicit error if the governing-party merge leaves any state-year unmatched, because the `replace_na()` that follows would otherwise silently recode `Aligned` to 0.

---

## 7. The index — `ici_state_year_scores.csv`

### PCA inputs

Three indicators, standardized by `prcomp(scale. = TRUE, center = TRUE)` — each centered on its sample mean and scaled by its sample SD (n−1 denominator). The constants are exported to `pca_standardization.csv`.

| Variable | Gloss | Taxonomy tier |
|---|---|---|
| `log_Rhetoric` | Logged rhetoric | Tier 1 — `log(Rhetoric_Intensity + 1)` |
| `n_conflicts` | Litigation count | Tiers 2–3, entered untransformed |
| `Rupture_Alliance_Period` | Institutional rupture | Tier 3, binary |

### Component scores and index

| Variable | Definition |
|---|---|
| `Score_PC1`, `Score_PC2`, `Score_PC3` | Component scores, sign-oriented so higher = more conflict (anchor: rhetoric for PC1, litigation for PC2, net loading direction for PC3). |
| `Indice_Weighted_Raw` | `Σ_k w_k · Score_PC_k`, with `w_k = λ_k / Σλ_j`. |
| `ICI` (`Indice_Conflictividad_W`) | `Indice_Weighted_Raw` min–max rescaled to [0,1]. **The dependent variable.** |
| `ICI_2PC` (`Indice_Conflictividad_2PC`) | PC1 + PC2 only, weights renormalized, separately rescaled. Robustness index. |

Component weights in the verified run: PC1 0.3548, PC2 0.3303, PC3 0.3150.

The min–max rescaling is computed over the full 864-row panel. Rescaling a subset on its own gives different numbers.

### Component-exclusion indices

Built in Script 06 on the estimation sample by renormalizing the retained weights and rescaling. Because the rescaling base is the 700-row sample rather than the 864-row panel, `Indice_Conflictividad_12PC` is **not** numerically identical to `Indice_Conflictividad_2PC`, even though both drop PC3. The published component-exclusion table uses the Script 06 versions.

| Variable | Drops |
|---|---|
| `Indice_Conflictividad_12PC` | PC3 |
| `Indice_Conflictividad_13PC` | PC2 |
| `Indice_Conflictividad_23PC` | PC1 |

---

## 8. Estimation variables (Script 05)

Lags are taken **within state**, after sorting by state and year.

| Variable | Definition |
|---|---|
| `Log_Lag_R28_Abs` | `log(lag(Ramo_28) + 1)` — fiscal capacity, the main regressor |
| `Log_Lag_R33_Abs` | `log(lag(Ramo_33) + 1)` |
| `Log_Lag_Conv_Abs` | `log(lag(Convenios) + 1)` |
| `Log_Lag_R28_ExFEIEF_Abs` | `log(lag(Ramo_28_ExFEIEF) + 1)` |
| `Log_Lag_FEIEF_Abs` | `log(lag(FEIEF) + 1)` |
| `Lag_FEIEF_Share` | `lag(FEIEF_Share_R28)` |
| `Log_Lag_OwnSourceDirect_Abs` | `log(lag(pmax(OwnSourceRevenue_Direct, 0)) + 1)` — **reported** |
| `Log_Lag_OwnSource_Abs` | `log(lag(pmax(Ingreso_Local_Residual, 0)) + 1)` — superseded |
| `Cambio_R28` | `(Ramo_28 − lag(Ramo_28)) / (lag(Ramo_28) + 1)` |
| `Lag_Cambio_R28` | `lag(Cambio_R28)` — the grievance/opportunism proxy |
| `Eleccion_Estatal` | 1 in a state's own gubernatorial election year; schedule hard-coded per state in Script 05, covering off-cycle and rescheduled elections. |

**The `+1` offsets.** Inside `log()` the offset keeps zero-valued transfers defined. In the change denominators it shields against division by zero where a category is absent in the base year. Both are applied to variables already in millions, so the offset is economically negligible.

**`pmax(·, 0)`.** Applied to the residual own-source measure, which can go slightly negative from Convenios classification noise. Kept on the direct measure for symmetry; it never binds there, since all four chapters are non-negative.

---

## 9. Specification

Two-way fixed effects (`plm`, `model = "within"`, `effect = "twoways"`), state and year effects. Standard errors are Arellano heteroskedasticity- and autocorrelation-robust, clustered by state (`vcovHC(..., method = "arellano", cluster = "group", type = "HC1")`).

Controls: `Log_Poblacion`, `Aligned`, `factor(Governor_Party)`, `Dependencia_Federal`, `factor(Eleccion_Estatal)`, `factor(President_Party)`.

Estimation sample **N = 700**. From 864 state-years: the lag structure costs the first two years (both a one-period lag and a lagged first difference are used), and a few further rows drop on missing covariates.

Governing-party cell sizes: PRI 325 state-years / 23 states, PAN 197/18, MOR 71/18, PRD 68/8, MC 17/3, PVEM 10/2, PES 7/1, IND 5/1.

---

## 10. Verified figures

`VERIFICATION.txt`, written by Script 07, checks these automatically on every run:

| Quantity | Value |
|---|---|
| Table 1, Level: coefficient on discretionary transfers | 0.211 (SE 0.052), N = 700, R² = 0.071 |
| Variance explained | PC1 35.5%, PC2 33.0%, PC3 31.5% |
| Contributions | rhetoric→PC1 45.1%, rupture→PC1 41.1%, litigation→PC2 84.2% |
| Robustness Value, q = 1 | 13.47% |
| Robustness Value, q = 1, α = 0.05 | 6.48% |

Also reproduced in `output/run_all_log.txt`, as LaTeX tables in published form: the four columns of Table 1; the Random Effects appendix table; the Hausman test (χ² = 50.33, p < 0.001); the FEIEF robustness table (0.211 / 0.210 / 0.188 / −0.002); the own-source table (0.211 / 0.208; own-source 0.012 / 0.018); component exclusion (0.211 / 0.079 / 0.282 / 0.124); the Ramo 28 exogeneity diagnostic (F = 4.74, p < 0.001, 5.2% of residual variance); and the full sensitivity bounds at 1×, 2× and 3× Log Population.
