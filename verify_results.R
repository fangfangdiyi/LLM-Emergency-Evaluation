# Manuscript ↔ Code Alignment Verification Checklist
# Run this after LLM_Emergency_Analysis.R to verify all results match

cat("\n")
cat(strrep("=", 70), "\n")
cat("MANUSCRIPT-CODE ALIGNMENT VERIFICATION\n")
cat(strrep("=", 70), "\n\n")

# Expected values from manuscript (for verification after running main script)
expected <- list(
  # Inter-rater reliability
  icc_total = 0.696,
  icc_total_ci = c(0.663, 0.727),
  icc_diagnosis = 0.687,
  icc_nextstep = 0.569,
  icc_treatment = 0.583,
  kappa_range = c(0.57, 0.69),
  ba_bias = -0.01,
  ba_loa = c(-3.6, 3.6),
  
  # Primary analysis
  f_model = 35.53,
  f_model_df = c(5, 901),
  f_interaction = 3.85,
  f_interaction_df = c(10, 901),
  
  # EMMs (Table 2)
  emm_gemini = 13.93,
  emm_deepseek_r1 = 13.86,
  emm_claude = 13.13,
  emm_gpt = 12.93,
  emm_grok = 12.76,
  emm_deepseek_v3 = 11.60,
  
  # Pairwise: R1 vs Gemini
  delta_r1_gemini = 0.07,
  p_r1_gemini = 0.999,
  
  # Cohen's d: R1 vs V3.1
  cohens_d_r1_v3 = 1.32,
  
  # High-performance chi-square
  chisq_high = 152.16,
  chisq_df = 5,
  
  # High-performance rates
  hp_gemini = 82.1,
  hp_r1 = 79.0,
  hp_gpt = 68.5,
  hp_claude = 67.3,
  hp_grok = 55.6,
  hp_v3 = 24.7,
  
  # Round scores (Gemini)
  gemini_r1 = 14.66,
  gemini_r3 = 12.76,
  
  # Round scores (DeepSeek R1)
  r1_round1 = 13.74,
  r1_round2 = 13.95,
  r1_round3 = 13.89,
  
  # Difficulty interaction
  f_difficulty_interaction = 2.73,
  f_difficulty_df = c(10, 903),
  p_difficulty = 0.003,
  
  # Difficulty decline (Table 3)
  decline_gemini = -1.15,
  decline_r1 = -1.65,
  decline_gpt = -1.69,
  decline_claude = -1.80,
  decline_v3 = -2.86,
  decline_grok = -3.20,
  
  # Error taxonomy overall (Table 1)
  error_gemini = 6.4,
  error_r1 = 6.4,
  error_claude = 10.5,
  error_grok = 22.2,
  error_gpt = 25.1,
  error_v3 = 26.3,
  
  # Error taxonomy - rare disease
  rare_gemini = 6.1,
  rare_r1 = 9.1,
  rare_claude = 16.7,
  rare_gpt = 28.8,
  rare_grok = 33.3,
  rare_v3 = 37.9,
  
  # Error taxonomy - anchoring bias
  anchor_r1 = 5.6,
  anchor_claude = 6.9,
  anchor_gemini = 8.3,
  anchor_grok = 20.8,
  anchor_v3 = 20.8,
  anchor_gpt = 26.4,
  
  # Error taxonomy - iatrogenic risk
  iatro_r1 = 3.0,
  iatro_gemini = 3.0,
  iatro_grok = 12.1,
  iatro_claude = 15.2,
  iatro_gpt = 18.2,
  iatro_v3 = 30.3,
  
  # Friedman
  friedman_chi2 = 98.71,
  friedman_df = 5,
  
  # CLMM (vs DeepSeek R1)
  or_gemini = 1.18,
  or_claude = 0.43,
  or_gpt = 0.72,
  or_grok = 0.35,
  or_v3 = 0.12,
  
  # Spearman correlations
  rho_range = c(0.77, 0.83),
  
  # Rater sensitivity
  rater_var_pct = 0.0,
  emm_max_change = 0.000,
  pairwise_unchanged = 15
)

cat("This checklist contains all numerical values reported in the manuscript.\n")
cat("After running LLM_Emergency_Analysis.R, compare your output against\n")
cat("these expected values to verify complete reproducibility.\n\n")

cat("Total expected values to verify:", length(unlist(expected)), "\n")
cat("\nNote: Minor rounding differences (<0.05) are acceptable due to\n")
cat("floating-point arithmetic and random seed differences.\n")
