---
date: 2026-03-30
session: Pilot Analysis — Tax Enforcement Survey Experiment
status: completed
---

## Goal

Set up the full analysis pipeline for the Tax Enforcement Survey pilot data:
merge Qualtrics + Prolific, clean sample, construct all PAP variables, run all 12 PAP regressions, output 04_Pilot_Results.tex.

## Key Context

- ~14 real Prolific respondents (pilot run as of 2026-03-30)
- Treatment arms: Control, Evasion, Evasion & Inequality, Filing, Filing & Lobbying, Full Info
- Only 3 arms represented in 14 obs — treatment coefficients NA for empty arms (expected)
- Prolific "Authenticity check: Bots" field: "high" = authentic, "low" = bot → exclude only "low"
- Treatment arm detected via which Video Attention column set has a response (positional mapping)
- Arm order in Qualtrics: Evasion (cols 162/163), Evasion & Inequality (176/177), Filing (190/191), Filing & Lobbying (204/205), Control (Q134/Q135), Full Info (252/253)

## What Was Done

1. Created `Project/Analysis/01_analysis.R` — full pipeline: merge → exclusions → variable construction → save RDS → regressions → LaTeX output
2. Installed tidyverse + broom R packages
3. Fixed bot exclusion logic (high = authentic, not bot)
4. Saved `Project/Data/Data_Processed/data_clean.rds` (14 rows, 110 cols)
5. Saved `Project/Data/Data_Processed/data_time.rds` (14 rows, 319 cols)
6. Wrote and compiled `Project/Overleaf/Update_Files/04_Pilot_Results.tex` (13 pages, all 12 PAP tables)
7. Fixed PAP typo: "Evasion & Revenue" → "Evasion & Inequality" in `Update_Files/01_Survey_PAP.tex`

## Known Issues / Next Steps

- Wide tables (6, 8) have minor overflow — add landscape environment for final version
- Table 2 (conditional TPR support) uses condition means rather than full panel regression — revisit for final analysis
- Pipeline ready to rerun on full dataset — just update the file path for the new data export
- Should commit current state to git
