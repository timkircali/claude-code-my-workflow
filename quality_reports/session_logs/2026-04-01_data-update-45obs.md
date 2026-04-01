---
date: 2026-04-01
session: Data update — April 1 export, N=45
status: in-progress
---

## Goal

Update the analysis pipeline to use the new April 1, 2026 Qualtrics export (45 valid observations, up from 14 in the pilot).

## Key Context

- New Qualtrics file: `Tax Enforcement_April 1, 2026_13.21.csv`
- Prolific file unchanged: `prolific_demographic_export_69b7d04312972f3940c8a2d8.csv`
- Exclusion fix: RETURNED (7) and TIMED-OUT (1) participants must be filtered from Prolific before the inner join
- Final sample: 45 (48 merged → 3 excluded by Prolific status → 45; 0 excluded by attention/bot)
- Channel index vars (`TPR_Tax_Revenue_Plus`, `TPR_Inequality_Minus`, `IEI_Tax_Revenue_Plus`, `IEI_Inequality_Minus`) confirmed present in `data_clean` with 45 non-NA values each — Table 9 should now show estimates

## What Was Done

1. Updated `path_qualtrics` to April 1 export (PR #10)
2. Added `filter(!status %in% c("RETURNED", "TIMED-OUT"))` before Prolific join (PR #11)
3. Verified channel index vars present in `data_clean` — no code change needed
4. PDF compiled: 13 pages, clean

## Open Items

- User to review Table 9 in PDF to confirm channel index rows now show estimates
