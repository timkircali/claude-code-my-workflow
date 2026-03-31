---
date: 2026-03-31
session: Analysis output fixes — Tax Enforcement Survey Experiment
status: in-progress
---

## Goal

Fix Section 6 (LaTeX output generation) of `Project/Analysis/01_analysis.R` so that all 12 PAP regression tables compile correctly with treatment effects shown and control vector rows left empty (showing `--`).

## Key Context

- Pilot data: 14 Prolific respondents, 3 treatment arms represented
- User manually fixed Section 5 regressions in previous session; those changes were committed (PR #3)
- Treatment variables are numeric 0/1 (`t_evasion`, `t_evasion_inequality`, etc.) — models run without demographic controls (Spec 1 only)
- Control rows kept in table structure but show `--` since models are fit without controls
- Two bugs fixed in Section 6 only (upper sections untouched per user instruction):
  1. `build_channels_table()` referenced undefined `m_chan`/`channel_rhs` → fixed to use `m_chan_TPR` with correct per-column channel variable logic
  2. `build_exposure_table()` used `_i`-suffixed variable names that don't exist → fixed to match actual variable names

## What Was Done

1. Fixed `build_channels_table()` — rewrote to use `m_chan_TPR`, handles policy-specific channel vars per column
2. Fixed `build_exposure_table()` — removed `_i` suffix from demographic variable names
3. Verified: `Rscript Project/Analysis/01_analysis.R` runs cleanly, 14 rows retained, all 12 tables generated
4. Verified: `xelatex 04_Pilot_Results.tex` compiles to 13-page PDF (overflow warnings on wide tables 6/8 are pre-existing)
5. Committed user's prior changes (PR #3, merged to main)

## Open Items

- Wide tables (6, 8) still overflow — landscape environment needed for final version
- Commit current Section 6 fixes to git


---
**Context compaction (auto) at 21:57**
Check git log and quality_reports/plans/ for current state.
