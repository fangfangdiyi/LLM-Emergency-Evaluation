# Beyond Accuracy: A Systematic Evaluation of Cognitive Biases Susceptibility and Safety Profiles in Open-Source vs. Proprietary LLMs for Emergency Medicine

[![DOI](https://img.shields.io/badge/DOI-pending-blue)](https://doi.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

This repository contains the statistical analysis code and LLM response data for the manuscript:

> **Beyond Accuracy: A Systematic Evaluation of Cognitive Biases Susceptibility and Safety Profiles in Open-Source vs. Proprietary LLMs for Emergency Medicine**
>
> Liu F†, Liu Z†, Fei X, He J, Xing J, Li J\*, Chan P\*
>
> Submitted to *npj Digital Medicine*

## Study Summary

We systematically evaluated six frontier LLMs—DeepSeek R1, Gemini 3 Pro, Claude Sonnet 4.5, GPT-5.1, Grok 4, and DeepSeek V3.1—across 54 complex emergency medicine cases from a major tertiary hospital. Two emergency physicians independently scored 972 model responses on diagnostic accuracy, management appropriateness, and response consistency. A novel error taxonomy quantified susceptibility to anchoring bias, rare disease oversight, and iatrogenic risk neglect.

### Key Findings

- **Three-tier performance hierarchy** (F₅,₉₀₁ = 34.01, p < 0.001): Top tier (Gemini 3 Pro, DeepSeek R1), Middle tier (Claude Sonnet 4.5, GPT-5.1, Grok 4), Bottom tier (DeepSeek V3.1)
- **Open-source parity**: DeepSeek R1 (EMM = 13.80) achieved statistical equivalence to Gemini 3 Pro (EMM = 13.89; Δ = 0.09, p = 0.998)
- **Divergent safety profiles**: Overall failure rates ranged from 7.6% (Gemini 3 Pro and DeepSeek R1) to 30.4% (DeepSeek V3.1), with a six-fold range in rare disease recognition failure rates

## Repository Structure

```
├── README.md                          # This file
├── LLM_Emergency_Analysis.R           # Main statistical analysis script (R 4.5.0)
├── generate_supplementary_tables.py   # Supplementary Table S5 generation (Python 3)
├── verify_results.R                   # Verification checklist of expected values
└── output/                            # Generated figures and tables (after running script)
    ├── LLM_Emergency_All_Results.xlsx # Comprehensive results (19 sheets)
    ├── Model_Summaries.txt            # Detailed model output text
    ├── Fig1_Overall_Performance.png   # Main Figure 1
    ├── Fig1_Overall_Performance.pdf   # Main Figure 1 (vector)
    ├── Fig2_Round_Trajectory.png      # Main Figure 2
    ├── Fig3_Performance_by_Difficulty.png  # Main Figure 3
    ├── Fig4_Error_Rates_by_Type.png   # Main Figure 4
    ├── FigS1_Correlation_Matrix.png   # Supplementary Figure S1
    ├── FigS2_BlandAltman.png          # Supplementary Figure S2
    ├── FigS3_Violin_Faceted.png       # Supplementary Figure S3
    ├── FigS4_Dimension_Performance.png # Supplementary Figure S4
    ├── FigS5_Effect_Size_Heatmap.png  # Supplementary Figure S5
    ├── FigS6_Residuals.png            # Supplementary Figure S6
    └── FigS7_Contrast_Plot.png          # Supplementary Figure S7
```

## Data Description

### `processed_ratings.xlsx` (available upon reasonable request)

The clinicians' evaluation dataset is not included in this repository. To obtain it, please contact the corresponding author (see Contact below). The file contains individual rater scores for all model evaluations:

| Column    | Description                                    |
|-----------|------------------------------------------------|
| rater     | Rater identifier (1 or 2)                      |
| case_id   | Case number (1–54)                             |
| round     | Query round (1–3, each case was independently queried three times, ≥24 hours apart) |
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

## Requirements

### R (version 4.5.0)

Required packages (automatically installed by the script if missing):

```r
readxl, tidyverse, lme4, lmerTest, emmeans, effectsize,
irr, rstatix, ggpubr, corrplot, performance,
psych, car, writexl, ordinal, MuMIn, multcomp
```

### Python (version 3.8+, for Supplementary Table S5 only)

```
pandas, numpy, openpyxl
```

## Usage

### Running the Main Analysis

1. Obtain `processed_ratings.xlsx` from the corresponding author and place it in the same directory as the R script
2. Open R (version 4.5.0 recommended) and set your working directory
3. Run the complete script:

```r
source("LLM_Emergency_Analysis.R")
```

The script will:
- Install any missing packages automatically
- Perform all statistical analyses reported in the manuscript
- Generate all main and supplementary figures in `output/`
- Export comprehensive results to `output/LLM_Emergency_All_Results.xlsx`

### Generating Supplementary Table S1

```bash
python generate_supplementary_tables.py
```

> **Note:** This script requires the individual rater scoring files. Update the file paths at the top of the script to match your local configuration.

## Statistical Methods

| Analysis | Method | R Package |
|----------|--------|-----------|
| Inter-rater reliability | ICC(2,1), weighted kappa, Bland-Altman | `irr` |
| Primary model comparison | Linear mixed-effects model (REML) | `lme4`, `lmerTest` |
| Post-hoc comparisons | Tukey HSD with compact letter display | `emmeans` |
| Effect sizes | Cohen's d | `effectsize` |
| Non-parametric sensitivity | Friedman test | `rstatix` |
| Ordinal sensitivity | Cumulative link mixed model | `ordinal` |
| Error taxonomy | Wilson score CIs, Fisher's exact (Monte Carlo) | base R |
| Rater sensitivity | Crossed random effects, variance decomposition | `lme4` |

## Manuscript–Code Correspondence

| Manuscript Element | Script Section | Output File |
|--------------------|----------------|-------------|
| Inter-rater reliability | Section 3 | `FigS1`, `FigS2` |
| Table 1 (EMMs, tiers) | Sections 6–7 | `Fig1`, `LLM_Emergency_All_Results.xlsx` |
| Figure 1 (overall performance) | Section 7 | `Fig1_Overall_Performance.png` |
| Figure 2 (round trajectory) | Section 8 | `Fig2_Round_Trajectory.png` |
| Table 2 (difficulty decline) | Section 9 | `Fig3_Performance_by_Difficulty.png` |
| Figure 3 (difficulty stratification) | Section 9 | `Fig3_Performance_by_Difficulty.png` |
| Table 3 (error taxonomy) | Section 10 | `Fig4_Error_Rates_by_Type.png` |
| Figure 4 (error rates) | Section 10 | `Fig4_Error_Rates_by_Type.png` |
| Sensitivity analyses | Section 12 | `FigS7_Contrast_Plot.png` |

## Ethics

This study was approved by the Institutional Review Board (Protocol: XA-KS2024-017-002) with waiver of informed consent.

## Citation

If you use this code or data, please cite:

```
Liu F, Liu Z, Fei X, He J, Xing J, Li J, Chan P. Beyond Accuracy: A Systematic 
Evaluation of Cognitive Biases Susceptibility and Safety Profiles in Open-Source vs. Proprietary 
LLMs for Emergency Medicine. npj Digital Medicine (2026). [submitted]
```

## License

This project is licensed under the MIT License.

## Contact

- Jia Li: lijia@xwh.ccmu.edu.cn
- Piu Chan: pbchan@hotmail.com
