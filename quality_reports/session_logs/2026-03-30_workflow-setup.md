---
date: 2026-03-30
session: Workflow setup — Tax Enforcement Survey Experiment
status: completed
---

## Goal

Adapt the forked Claude Code academic workflow template to the Tax Enforcement Survey Experiment project. Fill in CLAUDE.md placeholders, tune R conventions for an experiment context, remove Quarto references (Beamer-only project), and save user profile to memory.

## Key Context

- User: empirical researcher, LaTeX + R + Beamer toolchain, no Quarto
- Project: Tax Enforcement Survey experiment
- PAP exists in .tex format (not yet in repo); R code not yet written; slides not yet created
- Next steps: (1) add PAP to repo, (2) write R analysis code, (3) update Beamer slides with first results
- User wants structured, rigorous collaboration with check-ins in early sessions

## What Was Done

1. Updated `CLAUDE.md`: project name, removed Quarto, added PAP/ folder, adjusted commands and skills table
2. Rewrote `.claude/rules/single-source-of-truth.md`: PAP → R → Beamer pipeline; no hardcoded numbers in slides
3. Updated `.claude/rules/r-code-conventions.md`: experiment-specific pitfalls (ITT/LATE, multiple testing, PAP compliance, randomization seed)
4. Created memory files: `user_profile.md`, `project_tax_enforcement.md`, `MEMORY.md` index

## Open Items

- Institution name not yet provided (placeholder remains in CLAUDE.md line 4)
- Exact folder structure for Data/ and Analysis/ — user to decide
- Color palette for R figures — to be decided at first visualization task
