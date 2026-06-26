# Mean Accuracy May Be Insufficient for Safety Profiling: A Single-Centre Proof-of-Concept Evaluation of Six Large Language Models on Challenging Diagnostic Cases

[![DOI](https://img.shields.io/badge/DOI-pending-blue)](https://doi.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

This repository contains the statistical analysis code and the de-identified LLM response/scoring data for the manuscript:

> **Mean Accuracy May Be Insufficient for Safety Profiling: A Single-Centre Proof-of-Concept Evaluation of Six Large Language Models on Challenging Diagnostic Cases**
>
> Liu F†, Liu Z†, Fei X, He J, Xing J, Li J\*, Chan P\*
>
> *Journal of Medical Systems* (2026), under revision.

## Study Summary

We evaluated six frontier LLMs — DeepSeek R1, Gemini 3 Pro, Claude Sonnet 4.5, GPT-5.1, Grok 4, and DeepSeek V3.1 — on 54 diagnostically challenging emergency-medicine cases drawn from the internal records of a single tertiary academic hospital. Each case was queried three times per model (≥24 h apart, randomised order), and two emergency physicians independently scored all 972 responses on three dimensions: diagnosis, next diagnostic step, and treatment. A pre-specified error taxonomy quantified susceptibility to **anchoring bias, rare-disease oversight, and iatrogenic risk**.

The central argument is that **mean accuracy is insufficient to characterise safety**: statistically equivalent models can carry markedly different deployment risks. Accordingly, the primary analyses are **catastrophic-failure frequency** and **response reproducibility**, with the mean-score mixed model retained as a supporting analysis.

### Key Findings

- **Three-tier performance hierarchy** (linear mixed-effects model, *F*₅,₂₆₅ = 27.89, *p* < 0.001): Top tier (Gemini 3 Pro, DeepSeek R1), Middle tier (Claude Sonnet 4.5, GPT-5.1, Grok 4), Bottom tier (DeepSeek V3.1). These tiers index statistical separability, not clinical distinctness — several cross-tier differences fall within the ±1.5-point MCID (see TOST equivalence below and Supplementary Fig. S7).
- **Open-source parity, tested formally**: DeepSeek R1 (EMM 13.80) is statistically equivalent to Gemini 3 Pro (EMM 13.89) by **two one-sided tests (TOST)** against a pre-specified ±1.5-point margin (Δ = −0.09, i.e. R1 0.09 points below Gemini; 90% CI −0.47 to 0.30; **TOST *p* < 0.001**). Equivalence is established by the equivalence test — not by a non-significant difference.
- **Aggregate scores conceal safety heterogeneity**: catastrophic (dangerous-recommendation; any dimension ≤2) rates ranged from **1.2% (DeepSeek R1) to 7.4% (GPT-5.1)** and differed across models (Fisher–Freeman–Halton omnibus, *p* ≈ 0.009); the two safest models pooled carried a significantly lower dangerous-recommendation rate than the remaining four (5/324 = 1.5% vs 39/648 = 6.0%; Fisher *p* < 0.001; OR 0.25). Across-model anchoring-bias failure varied four-fold (6.9%–26.4%) and rare-disease-recognition failure six-fold (6.1%–37.9%).
- **Inconsistency is a safety hazard**: GPT-5.1, despite a **mid-tier** aggregate score, was the **least reproducible** model (within-case round-to-round SD 2.69, nearly double any other model; all 12 of its dangerous case×model combinations were stochastic single-round failures, none systematic). On an aortic-dissection case (main-text Box 1), models that were stable on average still produced a potentially lethal recommendation on individual rounds.

## Repository Structure

```
├── README.md                          # This file
├── processed_ratings.xlsx             # De-identified rater scores (input data; see Data Description)
├── analysis.R                         # Main statistical analysis script (R 4.5.0)
├── verify_results.R                   # Verification checklist of expected values
└── output/                            # Created automatically when analysis.R runs
    ├── All_Results.xlsx                           # Comprehensive results (one sheet per analysis)
    ├── Fig1_Overall_Performance.png / .pdf        # Main Fig 1 — EMM tiers
    ├── Fig2_Reproducibility_violin.png / .pdf     # Main Fig 2 — within-case round-to-round SD
    ├── Fig3_Performance_by_Difficulty.png         # Main Fig 3 — difficulty stratification
    ├── Fig4_Error_Rates_by_Type.png               # Main Fig 4 — error taxonomy
    ├── FigS1_Correlation_Matrix.png               # Supp Fig S1 — inter-dimension correlation
    ├── FigS2_BlandAltman.png                      # Supp Fig S2 — inter-rater agreement
    ├── FigS3_Violin_Faceted.png                   # Supp Fig S3 — score distributions
    ├── FigS4_Dimension_Performance.png            # Supp Fig S4 — dimension-specific EMMs
    ├── FigS5_Effect_Size_Heatmap.png              # Supp Fig S5 — Cohen's d matrix
    ├── FigS6_Residuals.png                        # Supp Fig S6 — primary-model diagnostics
    ├── FigS7_Equivalence_allpairs_90CI.png / .pdf # Supp Fig S7 — TOST equivalence forest
    └── FigS8_Spaghetti_trajectories.png           # Supp Fig S8 — per-case round trajectories
```

> **Script filename.** The canonical name in this repository is `analysis.R`. If your local copy uses a different filename (e.g. a dated revision suffix), rename it to match — or update the `source()` call below and the references in `verify_results.R`.

## Data Description

### `processed_ratings.xlsx` (included in this repository)

Consistent with the manuscript's Data Availability statement, the **individual rater scores and model-response evaluations — which are sufficient to reproduce every analysis in the paper — are openly available in this repository**. This file contains only numeric scores and no patient-identifiable information. The **case vignettes** (the clinical narratives themselves) are **not** posted publicly to protect patient privacy; de-identified vignettes are available to qualified investigators under a **data use agreement (DUA)**, subject to institutional and ethics-committee approval (see Contact).

| Column    | Description                                    |
|-----------|------------------------------------------------|
| rater     | Rater identifier (1 or 2)                      |
| case_id   | Case number (1–54)                             |
| round     | Query round (1–3; each case independently queried three times, ≥24 h apart) |
| model     | Model identifier (1–6; see mapping below)      |
| diagnosis | Diagnosis score (1–5)                          |
| nextstep  | Next-step recommendation score (1–5)           |
| treatment | Treatment plan score (1–5)                     |

**Model Mapping:**

| ID | Model |
|----|-------|
| 1  | Claude Sonnet 4.5 |
| 2  | GPT-5.1 |
| 3  | Grok 4 |
| 4  | DeepSeek V3.1 |
| 5  | DeepSeek R1 |
| 6  | Gemini 3 Pro |

Exact model versions, API identifiers, access dates, and sampling parameters are reported in **Supplementary Table S3**; the verbatim query prompt is in **Supplementary Box S2**.

## Requirements

### R (version 4.5.0)

```r
readxl, writexl, tidyverse, lme4, lmerTest, emmeans,
ordinal, irr, effectsize, rstatix, ggpubr, ggplot2,
corrplot, performance, car, multcomp
```

Package versions used in the manuscript: `lme4` v1.1-37, `lmerTest` v3.1-3, `emmeans` v1.11.1, `ordinal` v2023.12-4.1, `irr` v0.84.1, `ggplot2` v3.5.2.

## Usage

1. Place `processed_ratings.xlsx` in the same directory as `analysis.R` (the file is provided in this repository).
2. Open R (version 4.5.0 recommended) and set your working directory to the repository root.
3. Run:

```r
source("analysis.R")
```

The script creates the `output/` directory automatically, performs all statistical analyses reported in the manuscript, generates all main and supplementary figures in `output/`, and exports comprehensive results to `output/All_Results.xlsx`.

To check your output against the manuscript's reported values:

```r
source("verify_results.R")
```

## Statistical Methods

| Analysis | Method | R Package |
|----------|--------|-----------|
| Inter-rater reliability | ICC(2,1), quadratic-weighted κ, Bland–Altman | `irr` |
| **Primary model** | Linear mixed-effects model, `Total ~ Model + (1\|Case) + (1\|Case:Model)`, REML, Kenward–Roger df | `lme4`, `lmerTest` |
| Post-hoc comparisons | Tukey-adjusted pairwise contrasts (compact letter display) | `emmeans` |
| **Equivalence** | Two one-sided tests (TOST) vs ±1.5-point MCID (90% CI) | `emmeans` |
| **Catastrophic failure** | Dangerous-rate (any dimension ≤2), Wilson 95% CIs; Fisher–Freeman–Halton omnibus (Monte Carlo, B = 1e5); pooled two-safest-vs-four Fisher with OR; targeted pairwise Fisher, Holm-corrected (8 pre-declared comparisons, §2.6) | base R |
| **Reproducibility** | Per case×model classification across 3 rounds; within-case round-to-round SD | base R / `dplyr` |
| Difficulty stratification | `Total ~ Model * Difficulty + (1\|Case:Model)` | `lme4`, `lmerTest` |
| Error taxonomy | Failure rate (≤3), Wilson 95% CIs, Fisher's exact (Monte Carlo, B = 1e5) | base R |
| Effect sizes | Cohen's d | `effectsize` |
| Non-parametric sensitivity | Friedman test | `rstatix` |
| Ordinal sensitivity | Cumulative link mixed model | `ordinal` |
| Rater sensitivity | Crossed random effects, variance decomposition | `lme4` |

Round (query repetition) is treated as an **exchangeable replicate**, not a fixed-effect trend; a sensitivity analysis adding round as a fixed effect confirms it is negligible (*F*₂,₆₄₆ = 0.59, *p* = 0.56).

The Fisher–Freeman–Halton omnibus is computed in Section 8a-ter of `analysis.R` via `fisher.test(..., simulate.p.value = TRUE, B = 1e5)` on the 2 × 6 (dangerous vs safe) × model table using `set.seed(42)`. Because it is a Monte Carlo estimate, the reported *p* (≈ 0.009) carries small seed-dependent variation across R versions.

## Manuscript–Code Correspondence

| Manuscript Element | Output File |
|--------------------|-------------|
| Table 1 (dataset characteristics) | `output/All_Results.xlsx` (Descriptives) |
| Inter-rater reliability (§3.1) | `FigS1`, `FigS2` |
| Table 2 (EMMs, tiers, high-performance rate) | `Fig1`, sheet `EMM_Table1` |
| Figure 1 (overall performance) | `Fig1_Overall_Performance` |
| Figure 2 (within-case round-to-round variability) | `Fig2_Reproducibility_violin` |
| Equivalence testing (§3.2) | `FigS7_Equivalence_allpairs_90CI`, sheet `TOST_equivalence` |
| Catastrophic failure & reproducibility (§3.3) | `Fig2`, `FigS8`, sheets `Catastrophic_rates`, `Reproducibility`, `Round_variability` |
| Catastrophic between-model tests (§3.3; Table 3 caption) | sheets `Catastrophic_omnibus`, `Catastrophic_pooled`, `Catastrophic_pairwise_Holm` (all in `output/All_Results.xlsx`) |
| Box 1 (Case 24, aortic dissection) | main-text Box 1 / Supplementary Box S1 |
| Table 4 (difficulty decline) | `Fig3`, sheet `Difficulty_decline` |
| Figure 3 (difficulty stratification) | `Fig3_Performance_by_Difficulty` |
| Table 5 (error taxonomy) | `Fig4`, sheets `Error_Rare`, `Error_Anchoring`, `Error_Iatrogenic` |
| Figure 4 (error rates) | `Fig4_Error_Rates_by_Type` |
| Dimension-specific analysis | `FigS3`, `FigS4`, sheet `Dimension_EMMs` |
| Sensitivity analyses (CLMM, rater-adjusted, residuals) | `FigS5`, `FigS6`, sheets `CLMM`, `Rater_sensitivity` |

## Ethics

This study was approved by the Institutional Review Board of Xuanwu Hospital, Capital Medical University (Protocol No. XA-KS2024-017-002), with a waiver of informed consent owing to the retrospective design and the use of fully de-identified clinical data, in accordance with the Declaration of Helsinki.

## Citation

If you use this code or data, please cite:

```
Liu F, Liu Z, Fei X, He J, Xing J, Li J, Chan P. Mean Accuracy May Be Insufficient for Safety Profiling:
A Single-Centre Proof-of-Concept Evaluation of Six Large Language Models on Challenging
Diagnostic Cases. Journal of Medical Systems (2026). [under revision]
```

## License

This project is licensed under the MIT License.

## Contact

- Jia Li — lijia@xwh.ccmu.edu.cn
- Piu Chan — pbchan@hotmail.com
