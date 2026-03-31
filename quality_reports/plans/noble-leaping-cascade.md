# Plan: Table 6 Column Headers + Font Size Reduction
**Status:** DRAFT
**Date:** 2026-03-31

---

## Context

Two display issues in `04_Pilot_Results.tex`:
1. **Table 6** has 14 outcome columns but no column headers — the reader cannot tell what each column represents.
2. **All tables** use the document default font (11pt), making rows feel crowded and wide tables overflow.

---

## Changes — `Project/Analysis/01_analysis.R` (Section 6 only)

### 1. Font size: all table builders

Add `\small` immediately after `\begin{threeparttable}` in every table builder. This shrinks table content uniformly without touching the caption or notes.

Affected locations (one-line addition each):
- `build_generic_table` — line ~765 (`\begin{threeparttable}\n` string)
- `build_panel_regression_table` — same pattern
- `build_channels_table` — same pattern
- `build_exposure_table` — same pattern

For Table 6 specifically (14 columns, already overflows), use `\footnotesize` instead of `\small` by passing it via a new optional `font_size` parameter on `build_generic_table` (default `"\\small"`).

### 2. Table 6 multicolumn header

The `t6` call currently passes `NULL` for `col_headers`, producing no header row.

**Step A:** Add `raw_header` parameter to `build_generic_table`. When provided, it replaces the auto-generated numbered-column + label rows entirely.

**Step B:** Update `t6` call to pass the full PAP header and correct `col_spec`:

```r
t6 <- build_generic_table(
  caption   = "Considerations on Third-Party Reporting and International Exchange of Information",
  label     = "tab:policyconsiderationstpr",
  col_headers = NULL,
  models    = m_pol,
  font_size = "\\footnotesize",
  raw_header = paste0(
    "& \\multicolumn{10}{c||}{Tax Evasion (Revenue \\& Inequality)} & ",
    "\\multicolumn{2}{c||}{Filing Costs} & \\multicolumn{2}{c}{Data Privacy} \\\\\n",
    "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) & (10) & (11) & (12) & (13) & (14) \\\\[0.5em]\n",
    "& \\multicolumn{2}{c|}{reduces tax evasion} & \\multicolumn{2}{c|}{reduces admin. costs} & ",
    "\\multicolumn{2}{c|}{increases tax revenue} & \\multicolumn{2}{c|}{increases fairness} & ",
    "\\multicolumn{2}{c||}{reduces inequality} & \\multicolumn{2}{c||}{reduces filing costs} & ",
    "\\multicolumn{2}{c}{concerned about data privacy} \\\\\n",
    "& TPR & IEI & TPR & IEI & TPR & IEI & TPR & IEI & TPR & IEI & TPR & IEI & TPR & IEI \\\\\n"
  ),
  col_spec_override = "l||cc|cc|cc|cc|cc||cc||cc"
)
```

---

## Files to Modify

| File | Section | Lines (approx.) |
|------|---------|----------------|
| `Project/Analysis/01_analysis.R` | Section 6 only | 742–779 (`build_generic_table`), 787–827 (`build_panel_regression_table`), 956–1027 (`build_channels_table`), 1055–1079 (`build_exposure_table`), t6 call |

No changes to Sections 1–5.

---

## Verification

1. `Rscript Project/Analysis/01_analysis.R` — no errors
2. `xelatex 04_Pilot_Results.tex` — compiles to 13 pages
3. Table 6 in PDF shows 4-row multicolumn header with TPR/IEI pairs
4. All tables visibly smaller font than before
5. Commit and merge to main
