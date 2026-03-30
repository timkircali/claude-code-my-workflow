# Plan: Pilot Analysis — Tax Enforcement Survey
**Status:** DRAFT
**Date:** 2026-03-30

---

## Context

First-pass analysis of the Tax Enforcement Survey pilot data (~14 real responses). Goal is to set up the full analysis pipeline (merge → clean → regress → output) so it runs automatically on the final dataset. The pilot run will produce the same table structure as the PAP, filled with pilot results, saved to `04_Pilot_Results.tex`.

---

## Files

| Role | Path |
|------|------|
| Raw Qualtrics data | `Project/Data/Data_Raw/Qualtrics/Tax Enforcement_March 30, 2026_10.12.csv` |
| Raw Prolific data | `Project/Data/Data_Raw/Prolific/prolific_demographic_export_69b7d04312972f3940c8a2d8.csv` |
| R script | `Project/Analysis/01_analysis.R` (new) |
| Clean dataset | `Project/Data/Data_Processed/data_clean.rds` (new) |
| Time tracker dataset | `Project/Data/Data_Processed/data_time.rds` (new) |
| Results .tex | `Project/Overleaf/Update_Files/04_Pilot_Results.tex` (new) |
| PAP (read-only reference) | `Project/Overleaf/Original_Files/01_Survey_PAP.tex` |

---

## Script Structure: `01_analysis.R`

### Section 0: Setup
- `set.seed(20260330)`
- Load packages: `tidyverse`, `fixest` or `lm`, `modelsummary` / manual table writing
- Define paths

### Section 1: Load & Merge
- Read Qualtrics CSV (skip row 2 — Qualtrics question labels, not data)
- Read Prolific CSV
- Merge on: Qualtrics `PROLIFIC_PID` ↔ Prolific `Participant id`
- **Drop test observations:** rows in Qualtrics with no matching Prolific ID (inner join)
- Result: merged raw dataset

### Section 2: Sample Exclusions (PAP-specified)
Apply in this order:
1. Drop if failed 2+ attention checks (Attention 1–5 columns)
2. Drop if Prolific authenticity check flags bot/suspicious behavior (`Authenticity check: Bots` = "high" risk)
3. Drop if failed both video validation questions (`Video Attention 1` AND `Video Attention 2` wrong, for the relevant treatment arms)
4. Keep a flag variable `excluded_reason` to enable attrition balance checks

### Section 3: Variable Construction
Construct all PAP-defined variables:

**Treatment indicators** (from `ID` column in Qualtrics):
- `treat_control`, `treat_evasion`, `treat_evasion_ineq`, `treat_filing`, `treat_filing_lobby`, `treat_full_info`
- Control = reference category

**Outcome variables** (from Q-columns per PAP mapping):
- Policy support: TPR (9 conditions), IEI, TS, PPR, petitions
- Knowledge: tax gap perception, filing cost perception (time + cost)
- Government views (5 outcomes)
- Considerations on economic problems (8 outcomes)
- Considerations on TPR/IEI policies (14 outcomes)
- Detailed data privacy (9 conditions)
- Considerations on TS/PPR (6 outcomes)
- Tax software provider (5 conditions)
- Exposure (4 outcomes)
- Voluntary disclosure (3 conditions)

**Demographic controls** (X_i^D vector from PAP):
- From Prolific: Age, Sex → Female, Ethnicity → race dummies, Income → Middle/High income, Employment status, Student status
- From Qualtrics: Region, Education, Marital status, Household size/kids/partner, Homeowner, Wealth, Income sources, Political affiliation

### Section 4: Save Datasets
- `data_clean.rds` — all PAP variables, exclusions applied
- `data_time.rds` — ResponseId + all time tracker columns only (`*_First Click`, `*_Last Click`, `*_Page Submit`, `*_Click Count`)

### Section 5: Regressions
**Important caveat:** With ~14 observations and ~45 demographic controls, Specifications 2 and 3 cannot run (negative degrees of freedom). For the pilot:
- **Spec 1 (no controls)** — always run; main pilot result
- **Spec 2 (demographic controls)** — attempt but will likely produce NA coefficients or fail; include with a note
- All 12 PAP table structures will be populated

Run OLS (`lm()`) for each outcome. For panel/conditional outcomes (TPR 9 conditions, voluntary disclosure 3 conditions, data privacy 9 conditions, tax software provider 5 conditions) use stacked long-format regressions with condition dummies.

### Section 6: Output — `04_Pilot_Results.tex`
- Take exact LaTeX table shells from PAP (all 12 tables)
- Fill placeholders (`-`) with actual estimates (coefficient, SE in parentheses, stars)
- Add pilot disclaimer header: *"PILOT RESULTS — N=14. For illustration only. Standard errors unreliable at this sample size."*
- Save to `Project/Overleaf/Update_Files/04_Pilot_Results.tex`
- Table filling: use R string manipulation to write directly into the LaTeX shells (no external table package needed — preserves PAP formatting exactly)

---

## Key PAP Note Flagged During Planning
The `ExperimentalDesign_Details.tex` and PAP are consistent. No anomalies found. One minor observation: Table 11 (`taxsoftwareprovider`) lists "Evasion & Revenue" as a treatment label, but all other tables use "Evasion & Inequality" — this is likely a typo in the PAP. Will flag but not fix in the results document.

---

## Verification
1. `nrow(data_clean)` ≤ 14 (test obs dropped, exclusions applied)
2. `data_time.rds` contains only time tracker columns
3. `04_Pilot_Results.tex` compiles without errors (attempt `xelatex` on a minimal wrapper)
4. All 12 tables present in output file
5. No files written to `Original_Files/` or `Data_Raw/`
