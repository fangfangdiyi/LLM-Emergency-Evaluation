# ============================================================================
# Manuscript <-> Code Alignment Verification Checklist
# Run AFTER analysis.R to confirm output matches the values reported in the
# revised manuscript ("Mean Accuracy May Be Insufficient ...").
#
# This file reflects the REVISED analysis:
#   - Primary model:  Total ~ Model + (1|Case) + (1|Case:Model)
#     (query round is an exchangeable replicate, NOT a fixed effect)
#   - Equivalence established by TOST against a +/-1.5-point MCID
#     (NOT by a non-significant p-value)
#   - Primary safety endpoints: catastrophic-failure frequency & reproducibility
# ============================================================================

cat("\n"); cat(strrep("=", 72), "\n")
cat("MANUSCRIPT-CODE ALIGNMENT VERIFICATION (revised analysis)\n")
cat(strrep("=", 72), "\n\n")

# ---------------------------------------------------------------------------
# Expected values reported in the revised manuscript
# ---------------------------------------------------------------------------
expected <- list(

  ## --- Dataset (Table 1) ---
  n_cases        = 54,
  age_median     = 59,
  age_iqr        = c(39.5, 65.8),
  age_range      = c(15, 87),
  n_male         = 34,  pct_male   = 63.0,
  n_female       = 20,  pct_female = 37.0,

  ## --- Inter-rater reliability (Section 3.1) ---
  icc_total      = 0.719,  icc_total_ci = c(0.687, 0.748),
  icc_diagnosis  = 0.700,
  icc_nextstep   = 0.606,
  icc_treatment  = 0.606,
  # quadratic-weighted Cohen's kappa interpreted per Landis & Koch [30];
  # ICC bands per Koo & Li [29].

  ## --- Primary mixed model (Section 3.2) ---
  #  Total ~ Model + (1|Case) + (1|Case:Model)
  f_model        = 27.89,  f_model_df = c(5, 265),     # was 34.01 / (5,901)
  var_case       = 0.810,  var_case_pct       = 17.9,  # variance components
  var_case_model = 0.313,  var_case_model_pct = 6.9,
  var_residual   = 3.409,  var_residual_pct   = 75.2,
  # Sensitivity: adding round as a fixed effect is negligible
  f_round        = 0.59,   f_round_df = c(2, 646),  p_round = 0.56,

  ## --- Estimated marginal means (Table 2) ---
  emm_gemini_ci = c(13.49, 14.29),
  emm_deepseek_r1_ci = c(13.40, 14.21),
  emm_claude_ci = c(12.71, 13.52),
  emm_gpt_ci = c(12.49, 13.30),
  emm_grok_ci = c(12.25, 13.06),
  emm_deepseek_v3_ci = c(11.12, 11.93),

  ## --- Equivalence: DeepSeek R1 vs Gemini 3 Pro (TOST, Section 3.2) ---
  delta_r1_gemini   = -0.09,
  ci90_r1_gemini    = c(-0.47, 0.30),   # 90% CI within +/-1.5 MCID
  p_tost_r1_gemini  = 0.001,            # reported as < 0.001 (NOT p = 0.998)
  equivalent_r1_gemini = TRUE,

  ## --- Cohen's d: DeepSeek R1 vs DeepSeek V3.1 ---
  cohens_d_r1_v3 = 1.22,

  ## --- High-performance rate (>= 13 of 15; Table 2) ---
  hp_gemini = 82.1, hp_r1 = 78.4, hp_gpt = 68.5,
  hp_claude = 66.7, hp_grok = 55.6, hp_v3 = 24.7,

  ## --- PRIMARY SAFETY ENDPOINT 1: catastrophic failure (Section 3.3) ---
  #  dangerous rate = proportion of evaluations with ANY dimension <= 2
  danger_r1     = 1.2,  danger_gemini = 1.9,  danger_claude = 3.1,
  danger_grok   = 6.8,  danger_v3     = 6.8,  danger_gpt    = 7.4,

  ## --- PRIMARY SAFETY ENDPOINT 2: reproducibility (Section 3.3) ---
  #  % of case x model combinations that are STOCHASTICALLY INCONSISTENT
  stoch_gpt   = 66.7, stoch_grok = 37.0, stoch_v3     = 33.3,
  stoch_claude= 27.8, stoch_r1   = 20.4, stoch_gemini = 18.5,

  ## --- Within-case round-to-round SD (feeds Fig 2) ---
  sd_v3     = 1.025, sd_claude = 1.030, sd_r1     = 1.097,
  sd_grok   = 1.128, sd_gemini = 1.360, sd_gpt    = 2.694,   # GPT-5.1 highest

  ## --- Difficulty stratification (Section 3.4; Table 3) ---
  #  reduced random structure: Total ~ Model * Difficulty + (1|Case:Model)
  f_difficulty_interaction = 2.66,  f_difficulty_df = c(10, 306),  # was 3.00 / (10,903)
  p_difficulty             = 0.004,                                # was 0.001
  decline_gemini = -1.28, decline_gpt    = -1.79, decline_r1   = -1.82,
  decline_claude = -1.84, decline_v3     = -3.06, decline_grok = -3.51,

  ## --- Error taxonomy (Table 4) ---
  # overall average failure rate
  error_gemini = 7.6,  error_r1   = 7.6,  error_claude = 12.3,
  error_gpt    = 25.7, error_grok = 26.3, error_v3     = 30.4,
  # rare-disease recognition
  rare_gemini = 6.1,  rare_r1   = 9.1,  rare_claude = 16.7,
  rare_gpt    = 28.8, rare_grok = 33.3, rare_v3     = 37.9,
  # anchoring-bias susceptibility
  anchor_r1   = 6.9,  anchor_claude = 6.9,  anchor_gemini = 9.7,
  anchor_v3   = 22.2, anchor_grok   = 23.6, anchor_gpt    = 26.4,
  # iatrogenic-risk identification
  iatro_r1   = 6.1,  iatro_gemini = 6.1,  iatro_claude = 15.2,
  iatro_gpt  = 18.2, iatro_grok   = 18.2, iatro_v3     = 33.3,

  ## --- Ordinal sensitivity: CLMM odds ratios vs DeepSeek R1 (Section 3.6) ---
  or_gemini = 1.21, or_claude = 0.43, or_gpt = 0.73, or_grok = 0.35, or_v3 = 0.12,

  ## --- Non-parametric sensitivity (data-derived; unchanged) ---
  friedman_chi2 = 98.63, friedman_df = 5,

  ## --- Rater sensitivity (crossed random effects) ---
  rater_var_pct       = 0.0,
  emm_max_change      = 0.000,
  pairwise_unchanged  = 15
)

# ---------------------------------------------------------------------------
# Optional automatic check against the generated results workbook
# ---------------------------------------------------------------------------
xlsx_path <- "output/All_Results.xlsx"

approx_eq <- function(a, b, tol = 0.1) (length(a) && length(b) && !is.na(a) && !is.na(b) && abs(a - b) <= tol)
mark   <- function(ok) if (isTRUE(ok)) "  OK  " else " CHECK"

if (file.exists(xlsx_path) && requireNamespace("readxl", quietly = TRUE)) {
  cat("Auto-verifying headline values against:", xlsx_path, "\n")
  cat(strrep("-", 72), "\n")
  rd <- function(sheet) tryCatch(as.data.frame(readxl::read_excel(xlsx_path, sheet = sheet)),
                                 error = function(e) NULL)
  pick <- function(df, key_col, key, val_col) {
    if (is.null(df) || !(key_col %in% names(df)) || !(val_col %in% names(df))) return(NA_real_)
    r <- df[grepl(key, df[[key_col]], ignore.case = TRUE), ]
    if (!nrow(r)) return(NA_real_)
    suppressWarnings(as.numeric(r[[val_col]][1]))
  }

  icc <- rd("ICC")
  cat(sprintf("[%s] ICC (Total Score) = %.3f  (expected %.3f)\n",
      mark(approx_eq(pick(icc,"Dimension","Total","ICC"), expected$icc_total, 0.01)),
      pick(icc,"Dimension","Total","ICC"), expected$icc_total))

  cr <- rd("Catastrophic_rates")
  for (m in list(c("DeepSeek R1","danger_r1"), c("Gemini 3 Pro","danger_gemini"),
                 c("GPT-5.1","danger_gpt"))) {
    got <- pick(cr,"model_name",m[[1]],"danger_pct"); exp <- expected[[m[[2]]]]
    cat(sprintf("[%s] Dangerous rate (any dim<=2) %-14s = %.1f%%  (expected %.1f%%)\n",
        mark(approx_eq(got, exp, 0.3)), m[[1]], got, exp))
  }

  rv <- rd("Round_variability")
  for (m in list(c("GPT-5.1","sd_gpt"), c("Gemini 3 Pro","sd_gemini"),
                 c("DeepSeek R1","sd_r1"))) {
    got <- pick(rv,"model_name",m[[1]],"mean_within_sd"); exp <- expected[[m[[2]]]]
    cat(sprintf("[%s] Within-case round-to-round SD %-14s = %.3f  (expected %.3f)\n",
        mark(approx_eq(got, exp, 0.05)), m[[1]], got, exp))
  }

  rp <- rd("Reproducibility")
  for (m in list(c("GPT-5.1","stoch_gpt"), c("Gemini 3 Pro","stoch_gemini"))) {
    got <- pick(rp,"model_name",m[[1]],"pct_stochastic"); exp <- expected[[m[[2]]]]
    cat(sprintf("[%s] Stochastic-inconsistency %-14s = %.1f%%  (expected %.1f%%)\n",
        mark(approx_eq(got, exp, 0.5)), m[[1]], got, exp))
  }
  cat(strrep("-", 72), "\n\n")
} else {
  cat("(", xlsx_path, "not found, or 'readxl' unavailable -- skipping auto-check.)\n\n")
}

# ---------------------------------------------------------------------------
cat("This checklist contains the numerical values reported in the revised\n")
cat("manuscript. After running analysis.R, compare your output against these\n")
cat("expected values to confirm reproducibility.\n\n")
cat("Total expected values listed:", length(unlist(expected)), "\n")
cat("\nNote: minor rounding differences (< 0.05 for scores, < 0.5 pp for rates)\n")
cat("are acceptable due to floating-point arithmetic. The primary safety\n")
cat("endpoints are the dangerous rate (any dimension <= 2) and reproducibility;\n")
cat("the mixed model and EMMs are supporting analyses.\n")
