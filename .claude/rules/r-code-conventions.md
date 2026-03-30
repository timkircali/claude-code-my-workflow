---
paths:
  - "**/*.R"
  - "Figures/**/*.R"
  - "scripts/**/*.R"
  - "Analysis/**/*.R"
---

# R Code Standards

**Standard:** Senior Principal Data Engineer + PhD researcher quality

---

## 1. Reproducibility

- `set.seed()` called ONCE at top (YYYYMMDD format matching randomization date)
- All packages loaded at top via `library()` (not `require()`)
- All paths relative to repository root
- `dir.create(..., recursive = TRUE)` for output directories
- Randomization seed must be documented: link to PAP section that specifies it

## 2. Function Design

- `snake_case` naming, verb-noun pattern
- Roxygen-style documentation
- Default parameters, no magic numbers
- Named return values (lists or tibbles)

## 3. Domain Correctness (Survey Experiment)

- **ITT vs LATE:** Clearly distinguish intent-to-treat from LATE/IV estimates. Never mislabel.
- **Multiple testing:** Apply pre-specified correction (from PAP) when testing multiple outcomes. Document which correction is used and why.
- **Covariate balance:** Balance checks at randomization must use pre-specified covariates from PAP.
- **Randomization seed:** The seed used for treatment assignment must match what is documented in the PAP and/or registration. Document explicitly.
- **Heterogeneous effects:** Only run subgroup analyses that are pre-specified. Flag any post-hoc exploration clearly.
- **Survey weights:** If applicable, use consistently — never mix weighted and unweighted estimates without documentation.
- **PAP compliance:** Every analysis function should have a comment citing the PAP section it implements, e.g. `# PAP Section 4.2: Primary ITT estimate`

## 4. Visual Identity

```r
# --- Update with your institutional palette ---
primary_color  <- "#012169"   # Replace with institution primary color
secondary_color <- "#f2a900"  # Replace with institution secondary color
accent_gray    <- "#525252"
positive_green <- "#15803d"
negative_red   <- "#b91c1c"
```

### Custom Theme
```r
theme_custom <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", color = primary_color),
      legend.position = "bottom"
    )
}
```

### Figure Dimensions for Beamer
```r
ggsave(filepath, width = 12, height = 5, bg = "transparent")
```

## 5. RDS Data Pattern

**Heavy computations saved as RDS; slide rendering loads pre-computed data.**

```r
saveRDS(result, file.path(out_dir, "descriptive_name.rds"))
```

## 6. Common Pitfalls

| Pitfall | Impact | Prevention |
|---------|--------|------------|
| Missing `bg = "transparent"` | White boxes on Beamer slides | Always include in `ggsave()` |
| Hardcoded paths | Breaks on other machines | Use relative paths from repo root |
| ITT labeled as ATE | Misleading inference | Always label estimand explicitly |
| Multiple testing without correction | False positives | Apply pre-specified correction from PAP |
| Wrong randomization seed | Irreproducible treatment assignment | Document seed + cite PAP section |
| Undeclared subgroup analysis | Registration violation | Comment: `# UNREGISTERED — exploratory only` |
| Mixing weighted/unweighted | Inconsistent estimates | Document weighting decision in every function |

## 7. Line Length & Mathematical Exceptions

**Standard:** Keep lines <= 100 characters.

**Exception: Mathematical Formulas** — lines may exceed 100 chars **if and only if:**

1. Breaking the line would harm readability of the math
2. An inline comment explains the mathematical operation
3. The line is in a numerically intensive section

**Quality Gate Impact:**
- Long lines in non-mathematical code: minor penalty (-1 to -2 per line)
- Long lines in documented mathematical sections: no penalty

## 8. Code Quality Checklist

```
[ ] Packages at top via library()
[ ] set.seed() once at top, seed documented + linked to PAP
[ ] All paths relative to repo root
[ ] Functions documented (Roxygen)
[ ] Figures: transparent bg, explicit dimensions (12x5 for Beamer)
[ ] RDS: every computed object saved
[ ] Every analysis cites its PAP section in a comment
[ ] Undeclared analyses flagged explicitly
[ ] Comments explain WHY not WHAT
```
