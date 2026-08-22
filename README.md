# Trademark survival and the reliability of trademark-count innovation indicators

Replication code for the master's thesis *To what extent does the reliability of
trademark-count-based innovation indicators differ between goods and services
once the differential survival of registered marks is accounted for?*

The analysis uses the full United States trademark registry — 3,002,919 marks
registered between 1995 and 2015, followed to March 2024 — to measure how
differently registered goods marks and service marks survive their statutory
maintenance decisions, to build a survival-adjusted count from that difference,
and to test that adjusted indicator against business R&D expenditure at two
levels of analysis.

## How the pipeline is organised

Data construction runs in **Stata**, estimation in **R**. The division is not a
preference: the raw USPTO application file is 2.2 GB and cannot be read
column-selectively into memory by R's `haven`, which materialises the whole
table, whereas Stata streams only the variables required.

Run the scripts in the order below. Each R script consumes the file the Stata
step produces and writes its own tables and figures.

### Stata — data construction (`stata/`)

| Script | What it does |
|---|---|
| `prepare_data.do` | Merges the application-level case file with the classification and international-class tables, derives the primary Nice class and the goods/services label, the registration cohort, and the duration, event and censoring variables. Produces `tm_survival.dta` (~3M rows). |
| `firm_namekey.do` | Defines `namekey`, the company-name normaliser shared by both sides of the firm match: strips legal suffixes, punctuation and common abbreviations. |
| `firm_01_extract_owners.do` | Reduces the 28.3M-row owner file to one owner per mark, keeping the **original registrant** rather than any later assignee. Reads in blocks, since the file does not fit in memory. |
| `firm_02_compustat.do` | Cleans the Compustat extract: keeps `INDL` and `USD` rows, de-duplicates firm-years, builds the firm-level R&D benchmark. |
| `firm_03_match.do` | Matches owners to Compustat on the normalised name and builds the firm-level indicator. |

### R — estimation (`R/`)

| Script | What it produces |
|---|---|
| `build_survival.R` | Kaplan–Meier curves, log-rank test, Cox model (Figures 1 and 3; Tables B2–B5). |
| `survival_discrete.R` | Discrete-time hazards at the maintenance milestones, pooled complementary log-log model (Figure 2; Tables B6–B8). |
| `robustness.R` | Sector gap across measurement horizons and under the alternative goods/services rule (Figure 4; Table B9). |
| `competing_risks.R` | Cumulative incidence by cause; decomposition of the gap (Figures 5–6; Tables B10–B11). |
| `rq2_indicator.R` | Raw versus survival-adjusted counts and the resulting bias (Figures 7–8; Table B12). |
| `rq3_distribution.R` | Bias by cohort and by Nice class; class-by-cohort heat map (Figures 9–11; Tables B13–B14). |
| `rq4_materiality.R` | League-table reordering and the services-share trend (Figures 12–13; Tables B15–B16). |
| `rq4_external_validity.R` | Class-level correlation with allocated industry R&D, worldwide robustness, mechanism (Figure 14; Tables B17–B19). |
| `rq4_external_panel.R` | Class-by-year panel with class-cluster bootstrap and two-way fixed effects (Figure 15; Table B20). |
| `firm_04_validation.R` | Firm-level correlation with observed company R&D (Figure 16; Tables B21–B22). |
| `firm_05_diagnostics.R` | Permutation benchmark, the same firms at three units of analysis, sign stability, US-only robustness (Tables B23–B25). |

R packages used: `haven`, `dplyr`, `tidyr`, `readxl`, `survival`, `ggplot2`,
`knitr`, `sandwich`, `boot`.

## Data availability

The code is released here in full. Of the four data sources, only the two NSF
tables are small enough and free enough of restrictions to be included; the
others are documented and linked.

| Source | Status | Where to get it |
|---|---|---|
| **NSF BERD survey, 2023 release** — domestic R&D by industry (Table 58) and worldwide R&D (Table 57) | **Included**, in `data/`. A work of the United States federal government, published by the National Center for Science and Engineering Statistics. The two files are pinned here because NSF revises its tables between releases, so the exact release used has to travel with the code. | NSF NCSES, publication `nsf25354` |
| **USPTO Trademark Case Files Dataset** | Not included. Public — a work of the United States federal government, distributed openly by the USPTO Office of the Chief Economist — but far too large for a code repository: the application file is 2.2 GB and the owner file 3.1 GB. `prepare_data.do` rebuilds the analysis sample from them. | USPTO Office of the Chief Economist, Economic Research Datasets |
| **ALP trademark concordance** (Zolas, Lybbert & Bhattacharyya) | Not included. Freely distributed for research by the World Intellectual Property Organization; check the terms on the download page before mirroring it. The analysis uses the backward, NAICS-to-Nice direction at the two-digit and three-digit levels (`NAICS_07_2_to_nice.txt`, `NAICS_07_3_to_nice.txt`). | WIPO Economic Research Working Paper series |
| **Compustat North America (Fundamentals Annual)** | **Not included: licensed, and it cannot be redistributed in any form.** Accessed through Wharton Research Data Services under an institutional subscription. The variables used are `gvkey`, `conm`, `cusip`, `naics`, `xrd`, `revt`, `at`, `emp`, for fiscal years 2005–2018. | WRDS, subscription required |

## Running the code

Every script reads the project root from the `THESIS_ROOT` environment
variable and falls back to the current directory, so nothing needs editing:

```
export THESIS_ROOT=/path/to/project
```

The project directory is expected to contain `data_external/` for the source
data and `output/` for the results. To reproduce the analysis, copy the two NSF
tables from `data/` into `data_external/`, download the USPTO dataset and the
ALP concordance there as well, obtain the Compustat extract from WRDS with the
variables listed above, and run the scripts in the order given: the five Stata
routines first, then the R routines.

## Citation

Castello, M. (2026). *To what extent does the reliability of
trademark-count-based innovation indicators differ between goods and services
once the differential survival of registered marks is accounted for?* Master's
thesis.

## Use of AI assistance

The research design, the methodological decisions and the interpretation of the
results are the author's. The code in this repository was written with the
assistance of an AI coding assistant (Claude, by Anthropic), working under the
author's direction; every script was reviewed by the author and every result it
produces was checked against the analysis reported in the thesis. The commit
history records this: commits carry a `Co-Authored-By` trailer where the
assistant contributed to the code.

## Licence

The code in this repository is released under the MIT Licence (see `LICENSE`).
The licence covers the code only; the data sources retain their own terms.
