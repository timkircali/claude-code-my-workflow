---
paths:
  - "Figures/**/*"
  - "Slides/**/*.tex"
  - "PAP/**/*.tex"
  - "Analysis/**/*.R"
---

# Single Source of Truth: Enforcement Protocol

**This project has two authoritative sources:**

1. **Pre-Analysis Plan `.tex`** — authoritative for study design, hypotheses, estimators, and subgroup definitions. R code must implement exactly what is specified here.
2. **R analysis scripts** — authoritative for all computed results (coefficients, p-values, figures, tables). Slides consume these outputs; never type numbers into slides by hand.

```
PAP .tex  (SOURCE OF TRUTH: study design)
  └── R scripts implement PAP specifications exactly
        └── R outputs: Figures/*.pdf, Figures/*.png, tables/*.tex
              └── Beamer .tex slides \includegraphics / \input these files
                    └── Bibliography_base.bib (shared)

NEVER compute results manually and type them into slides.
NEVER run analyses not pre-specified in the PAP without flagging clearly.
ALWAYS trace a result back to its R script and PAP section.
```

---

## PAP Compliance Protocol

Before running any analysis, verify:
1. The estimator is defined in the PAP
2. The outcome variable is defined in the PAP
3. The sample restriction matches the PAP
4. If deviating from PAP: flag clearly as "unregistered analysis" in code comments and slides

---

## R → Slides Handoff

**Figures:** R saves to `Figures/` as PDF or PNG with transparent background. Beamer uses `\includegraphics`.

**Tables:** R saves `.tex` fragments to a tables output directory. Beamer uses `\input`.

**Never hardcode numbers.** If a coefficient appears in a slide, it must be either:
- `\input`-ed from an R-generated `.tex` file, or
- Clearly marked as approximate/illustrative

---

## Content Fidelity Checklist (Slides)

```
[ ] Every result shown in slides traces to a specific R script output
[ ] Every analysis shown is pre-specified in the PAP (or flagged as unregistered)
[ ] Figure files referenced in .tex exist in Figures/
[ ] Table files referenced in .tex exist in tables output directory
[ ] No hardcoded numerical results in slide source
```
