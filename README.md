# site_comparisons

This repository generates per-site descriptive comparison tables for MELiDOS baseline instruments and exports each table as both **PNG** and **DOCX** files.

## Repository contents

- `scripts/table_generation.qmd`: Main Quarto workflow that loads each questionnaire/data domain and calls table generation for all sites.
- `scripts/table.R`: Core table-building/export functions (`summary_table()`, `table_one_site()`, `table_all_sites()`).
- `scripts/helpers.R`: Site metadata, site label conversion, ordering, and helper styling utilities.
- `scripts/time_summaries.R`: Custom summary functions for clock-time variables used inside `gtsummary::tbl_summary()` statistics.
- `tables/<SITE>/`: Output folder structure. Each site gets one PNG and one DOCX per instrument/domain.

## What gets generated

The script currently builds one set of site-comparison tables for each domain below (labels as used in output):

- Demographics
- Morning sleep diary
- Chronotype
- Wear log
- Wellbeing diary (WHO-5)
- Exercise diary
- Light exposure (mH-LEA) & exercise diary
- Current conditions (EMA)
- Light exposure-related behavior (LEBA)
- Light sensitivity (VLSQ-8)
- Assessment of sleep environment (ASE)
- Acceptability of wearing the device
- Lifestyle & health
- Experience log

For each domain, `table_all_sites()` iterates through all MELiDOS sites and writes files into `tables/<SITE>/`.

---

## How tables are created (step-by-step)

### 1) Setup and load dependencies

In `scripts/table_generation.qmd`, required libraries are loaded (`tidyverse`, `melidosData`, `gt`, `LightLogR`, `gtsummary`, `rlang`), and helper scripts are sourced.

### 2) Load and flatten each instrument

Each section calls:

```r
load_data("<instrument>") |> flatten_data()
```

from `melidosData` to produce an analysis-ready tabular dataset.

### 3) Apply instrument-specific preprocessing

Before table creation, some domains are transformed, for example:

- coercing variables to numeric for continuous summaries,
- converting time/date fields to standardized reference dates,
- computing and formatting chronotype/time-derived variables,
- removing technical columns via `non_columns`.

### 4) Call `table_all_sites()`

Each processed dataset is passed to:

```r
table_all_sites(data, "<Table title>", ...)
```

This loops over all sites in `melidos_sites$site` and calls `table_one_site()`.

### 5) Build per-site comparison table (`summary_table()`)

For each site:

- Site rows are converted to ordered factor labels (`site_conv_mutate()`).
- A two-group comparison is created by collapsing `site` into:
  - the selected site (e.g., `Munich (DE)`), and
  - `Other sites`.
- `gtsummary::tbl_summary(by = site, ...)` builds descriptive summaries.
- `add_difference()` adds effect-size/difference information.
- `add_p()` computes significance tests (see details below).
- `bold_p()`, `add_significance_stars()`, etc., format inference output.
- `as_gt()` converts to a `gt` table and applies site-color styling.

### 6) Export to files (`table_save()`)

Each table is saved as both `png` and `docx` by default.

Filename pattern is:

```text
../tables/<SITE>/<truncated-table-name><YYYY-MM-DD>.<ext>
```

where table names are truncated to 10 characters (with `_` ellipsis) before adding the date.

---

## How `tbl_summary()` is used here (general behavior)

`summary_table()` is a wrapper around `gtsummary::tbl_summary()` with additional MELiDOS-specific defaults.

### Grouping

- `by = site` after recoding to **selected site vs Other sites**.

### Missing data display

- `missing_text = "NA"`.
- Abbreviation note added: `NA = Not available / missing`.

### Summary statistics

Defaults and overrides are passed through `other_arguments` per instrument.

Common patterns include:

- continuous: `"{median} ({p25}, {p75})"`
- categorical: `"{n} ({p}%)"`
- time-like variables (sleep/chronotype): custom templates like
  - `"{time_median} ({nighttime_p25}, {nighttime_p75})"`
  - `"{time_median} ({daytime_p25}, {daytime_p75})"`

These custom tokens map to functions in `scripts/time_summaries.R`.

### Variable type control

Some fields are explicitly set to `continuous` with `type = ... ~ "continuous"` when raw input classes are ambiguous.

---

## Significance tests and p-values

After `tbl_summary()`/`add_difference()`, p-values are added with:

```r
add_p(test.args = all_categorical() ~ list(simulate.p.value = TRUE))
```

### What this means in practice

- `gtsummary` selects test families based on variable type and number of groups.
- For **categorical variables**, the underlying test call is configured with `simulate.p.value = TRUE` (typically relevant to chi-squared-style testing when expected counts are small/sparse).
- P-values are then formatted and emphasized with:
  - `bold_p()`
  - `add_significance_stars()`

### Important interpretation note

Each table is a **single-site-vs-all-other-sites** comparison for many variables. The stars/p-values are convenient screening indicators, but they should be interpreted cautiously due to multiple testing and potential non-independence across site comparisons.

---

## Re-running the workflow

From repository root:

```bash
cd scripts
quarto render table_generation.qmd
```

This regenerates all site/domain tables under `tables/` (overwriting by filename if dates match the same day).

If Quarto is unavailable, you can source the scripts in an R session and run the section logic manually.

## Notes

- Output appearance depends on installed fonts/rendering backend used by `gt::gtsave()` for PNG export.
- The code relies on `melidosData` data objects and helper functions (`load_data()`, `flatten_data()`, labels).
