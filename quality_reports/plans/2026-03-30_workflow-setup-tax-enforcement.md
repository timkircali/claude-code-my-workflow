# Plan: Workflow Configuration — Tax Enforcement Survey Experiment
**Status:** COMPLETED
**Date:** 2026-03-30

---

## Context

The user is starting a new research project (Tax Enforcement Survey experiment) in this repo, which was forked from the academic Claude Code template. All configuration files contain generic placeholders. The goal is to adapt the workflow to this specific project: LaTeX for documentation (including a pre-analysis plan), R for analysis, and Beamer for results slides. No Quarto is needed. The user wants a structured, rigorous workflow with publication-ready visuals and smart memory of decisions.

---

## Approach

Minimal adaptation: fix CLAUDE.md placeholders, adjust rules that reference Quarto/lecture-centric workflows, and tune R conventions for an experiment context. No new folders created (user will customize layout themselves). Save user profile to memory for future sessions.

---

## Files to Modify

### 1. `CLAUDE.md`
- Replace `[YOUR PROJECT NAME]` → `Tax Enforcement Survey Experiment`
- Replace `[YOUR INSTITUTION]` → `[INSTITUTION]` (user to fill in)
- Update folder structure table to reflect actual project (PAP/.tex, Data/, Analysis/, Slides/, Figures/, scripts/)
- Remove Quarto deploy command from Commands section
- Replace Quarto quality score command with R analysis command
- Update "Current Project State" table to reflect the one document that exists: the pre-analysis plan .tex
- Remove Quarto-specific rows from Skills Quick Reference
- Remove Beamer ↔ Quarto sync note from Core Principles
- Add note: R scripts in `scripts/` or `Analysis/`; figures saved to `Figures/` as PDF/PNG

### 2. `.claude/rules/single-source-of-truth.md`
- Soften the Beamer-is-authoritative framing: in this project the PAP .tex is authoritative for study design; R code is authoritative for results; Beamer slides consume both
- Remove TikZ/Quarto-specific sections (not applicable here)
- Add: R outputs (figures, tables) are the bridge between analysis and slides

### 3. `.claude/rules/r-code-conventions.md`
- Update Visual Identity palette placeholder with a neutral palette (user hasn't specified institution colors yet — note they should fill this in)
- Add experiment-specific Common Pitfalls: ITT vs LATE confusion, multiple testing without correction, covariate balance checks, randomization seed documentation
- Add a note: pre-analysis plan is the spec — R code must implement exactly what PAP specifies (no undeclared analyses)

### 4. `MEMORY.md`
- No changes to generic MEMORY.md (per meta-governance: only generic patterns go here)

### 5. Memory system (`.claude/projects/.../memory/`)
- Write `user_profile.md`: user is an empirical researcher running a survey experiment; uses LaTeX + R; Beamer-only (no Quarto); wants rigorous, structured collaboration; publication-ready visuals; check in more often in early sessions
- Write `project_tax_enforcement.md`: project context — Tax Enforcement Survey experiment; PAP exists in .tex format; workflow: R code → results → update Beamer slides; R → LaTeX handoff kept flexible for now

---

## What is NOT changed

- `.claude/settings.json` — already correct (Rscript, xelatex, bibtex, git all allowed)
- `.gitignore` — already correct for R + LaTeX
- Skills files — kept as-is; Quarto-specific skills simply won't be invoked for this project
- Orchestrator, plan-first, session-logging rules — kept as-is (fully applicable)
- Quality thresholds (80/90/95) — kept as-is

---

## Open Questions

- Institution name: user to provide or fill in manually
- Color palette for R figures: to be decided when creating first visualizations
- Exact folder structure for data and analysis: user will decide (kept flexible)

---

## Verification

After implementation:
1. Read CLAUDE.md — confirm all `[BRACKETED]` placeholders are replaced except institution (which user fills)
2. Read r-code-conventions.md — confirm experiment pitfalls are present
3. Read single-source-of-truth.md — confirm Quarto/TikZ sections are removed or scoped
4. Check memory files exist at correct paths
5. No compilation needed (config-only changes)
