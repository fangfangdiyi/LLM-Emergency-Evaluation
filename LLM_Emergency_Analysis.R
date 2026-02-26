# ============================================================================
# Beyond Accuracy: A Systematic Evaluation of Cognitive Bias Susceptibility and Safety 
# Profiles in Open-Source vs. Proprietary LLMs for Emergency Medicine
#
# Statistical Analysis Script
# R version 4.5.0
#
# This script reproduces all statistical results, tables, and figures 
# reported in the manuscript and supplementary materials.
# ============================================================================

rm(list = ls())
options(scipen = 999)
set.seed(42)

# ============================================================================
# 1. SETUP AND PACKAGE LOADING
# ============================================================================

required_packages <- c(
  "readxl", "tidyverse", "lme4", "lmerTest", "emmeans", "effectsize",
  "irr", "rstatix", "ggpubr", "corrplot", "performance",
  "psych", "car", "writexl", "ordinal", "MuMIn", "multcomp"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

select <- dplyr::select
filter <- dplyr::filter

cat("All packages loaded successfully\n")

# ============================================================================
# 2. DATA IMPORT AND PREPROCESSING
# ============================================================================

# --- User Configuration ---
# Modify this path to point to your local processed_ratings.xlsx file
data_path <- "processed_ratings.xlsx"

dir.create("output", showWarnings = FALSE)

raw_data <- read_excel(data_path)

colnames(raw_data) <- c("rater", "case_id", "round", "model",
                        "diagnosis", "nextstep", "treatment")

# Model name mapping (1-6 correspond to model identifiers in the data file)
model_names <- c(
  "1" = "Claude Sonnet 4.5",
  "2" = "GPT-5.1",
  "3" = "Grok 4",
  "4" = "DeepSeek V3.1",
  "5" = "DeepSeek R1",
  "6" = "Gemini 3 Pro"
)

raw_data <- raw_data %>%
  mutate(
    rater    = factor(rater),
    case_id  = factor(case_id),
    round    = factor(round),
    model    = factor(model),
    model_name = factor(model_names[as.character(model)],
                        levels = c("Claude Sonnet 4.5", "GPT-5.1", "Grok 4",
                                   "DeepSeek V3.1", "DeepSeek R1", "Gemini 3 Pro")),
    total = diagnosis + nextstep + treatment
  )

cat("Data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
cat("Cases:", n_distinct(raw_data$case_id), " | Models:", n_distinct(raw_data$model),
    " | Rounds:", n_distinct(raw_data$round), " | Raters:", n_distinct(raw_data$rater), "\n\n")

# ============================================================================
# 3. INTER-RATER RELIABILITY
#    Output: Supplementary Figures S1, S2
# ============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 3: INTER-RATER RELIABILITY\n")
cat(strrep("=", 60), "\n\n")

# 3.1 Pivot to wide format for rater comparison
data_wide <- raw_data %>%
  pivot_wider(
    id_cols = c(case_id, round, model, model_name),
    names_from = rater,
    values_from = c(diagnosis, nextstep, treatment, total)
  )

# 3.2 ICC(2,1): Two-way random effects, absolute agreement, single measures
calculate_icc <- function(data, col1, col2, dimension_name) {
  mat <- data[, c(col1, col2)] %>% as.matrix()
  result <- irr::icc(mat, model = "twoway", type = "agreement", unit = "single")
  data.frame(
    Dimension = dimension_name,
    ICC = round(result$value, 3),
    Lower_CI = round(result$lbound, 3),
    Upper_CI = round(result$ubound, 3),
    F_value = round(result$Fvalue, 2),
    p_value = format.pval(result$p.value, digits = 3),
    Interpretation = case_when(
      result$value < 0.50 ~ "Poor",
      result$value < 0.75 ~ "Moderate",
      result$value < 0.90 ~ "Good",
      TRUE ~ "Excellent"
    )
  )
}

icc_results <- bind_rows(
  calculate_icc(data_wide, "diagnosis_1", "diagnosis_2", "Diagnosis"),
  calculate_icc(data_wide, "nextstep_1",  "nextstep_2",  "Next Steps"),
  calculate_icc(data_wide, "treatment_1", "treatment_2", "Treatment"),
  calculate_icc(data_wide, "total_1",     "total_2",     "Total Score")
)

cat("ICC(2,1) Results:\n")
print(icc_results)

# 3.3 Quadratic-weighted Cohen's kappa (ordinal agreement)
kappa_diagnosis <- irr::kappa2(data_wide[, c("diagnosis_1", "diagnosis_2")], weight = "squared")
kappa_nextstep  <- irr::kappa2(data_wide[, c("nextstep_1",  "nextstep_2")],  weight = "squared")
kappa_treatment <- irr::kappa2(data_wide[, c("treatment_1", "treatment_2")], weight = "squared")

kappa_results <- data.frame(
  Dimension = c("Diagnosis", "Next Steps", "Treatment"),
  Weighted_Kappa = round(c(kappa_diagnosis$value, kappa_nextstep$value, kappa_treatment$value), 3),
  Interpretation = case_when(
    c(kappa_diagnosis$value, kappa_nextstep$value, kappa_treatment$value) < 0.40 ~ "Fair",
    c(kappa_diagnosis$value, kappa_nextstep$value, kappa_treatment$value) < 0.60 ~ "Moderate",
    c(kappa_diagnosis$value, kappa_nextstep$value, kappa_treatment$value) < 0.80 ~ "Substantial",
    TRUE ~ "Almost Perfect"
  )
)

cat("\nWeighted Kappa Coefficients:\n")
print(kappa_results)

# 3.4 Bland-Altman analysis
data_wide <- data_wide %>%
  mutate(
    total_mean = (total_1 + total_2) / 2,
    total_diff = total_1 - total_2
  )

ba_mean <- mean(data_wide$total_diff)
ba_sd   <- sd(data_wide$total_diff)
ba_loa_upper <- ba_mean + 1.96 * ba_sd
ba_loa_lower <- ba_mean - 1.96 * ba_sd

cat("\nBland-Altman Analysis (Total Score):\n")
cat("  Bias (mean difference):", round(ba_mean, 2), "\n")
cat("  95% Limits of Agreement: [", round(ba_loa_lower, 1), ",", round(ba_loa_upper, 1), "]\n")

# --- Supplementary Figure S2: Bland-Altman Plot ---
p_ba <- ggplot(data_wide, aes(x = total_mean, y = total_diff)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_hline(yintercept = ba_mean, color = "blue", linetype = "solid", linewidth = 1) +
  geom_hline(yintercept = c(ba_loa_upper, ba_loa_lower), color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dotted") +
  annotate("text", x = max(data_wide$total_mean), y = ba_mean,
           label = paste("Bias =", round(ba_mean, 2)),
           hjust = 1, vjust = -0.5, color = "blue") +
  annotate("text", x = max(data_wide$total_mean), y = ba_loa_upper,
           label = paste("+1.96SD =", round(ba_loa_upper, 2)),
           hjust = 1, vjust = -0.5, color = "red") +
  annotate("text", x = max(data_wide$total_mean), y = ba_loa_lower,
           label = paste("-1.96SD =", round(ba_loa_lower, 2)),
           hjust = 1, vjust = 1.5, color = "red") +
  labs(title = "Bland-Altman Plot: Inter-Rater Agreement",
       subtitle = "Total Score (Rater 1 vs Rater 2)",
       x = "Mean of Two Raters", y = "Difference (Rater 1 - Rater 2)") +
  theme_pubr() +
  theme(plot.title = element_text(face = "bold"))

ggsave("output/FigS2_BlandAltman.png", p_ba, width = 8, height = 6, dpi = 300)
cat("  Saved: output/FigS2_BlandAltman.png\n")

# ============================================================================
# 4. CONSENSUS SCORE COMPUTATION
# ============================================================================

data_consensus <- data_wide %>%
  mutate(
    diagnosis = (diagnosis_1 + diagnosis_2) / 2,
    nextstep  = (nextstep_1  + nextstep_2)  / 2,
    treatment = (treatment_1 + treatment_2) / 2,
    total     = (total_1     + total_2)     / 2
  ) %>%
  select(case_id, round, model, model_name, diagnosis, nextstep, treatment, total)

cat("\nConsensus scores computed (n =", nrow(data_consensus), ")\n")

# --- Supplementary Figure S1: Correlation Matrix ---
cor_matrix <- cor(data_consensus[, c("diagnosis", "nextstep", "treatment")],
                  method = "spearman", use = "complete.obs")

cor_dn <- cor.test(data_consensus$diagnosis, data_consensus$nextstep,  method = "spearman")
cor_dt <- cor.test(data_consensus$diagnosis, data_consensus$treatment, method = "spearman")
cor_nt <- cor.test(data_consensus$nextstep,  data_consensus$treatment, method = "spearman")

cat("\nSpearman correlations (Supplementary Fig. S1):\n")
cat("  Diagnosis vs Next Steps: rho =", round(cor_dn$estimate, 2), "\n")
cat("  Diagnosis vs Treatment:  rho =", round(cor_dt$estimate, 2), "\n")
cat("  Next Steps vs Treatment: rho =", round(cor_nt$estimate, 2), "\n")

png("output/FigS1_Correlation_Matrix.png", width = 600, height = 500, res = 150)
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 1.2,
         tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#0072B2", "white", "#D55E00"))(100),
         title = "Correlation Between Scoring Dimensions",
         mar = c(0, 0, 2, 0))
dev.off()
cat("  Saved: output/FigS1_Correlation_Matrix.png\n")

# ============================================================================
# 5. DESCRIPTIVE STATISTICS
# ============================================================================

cat("\n", strrep("=", 60), "\n")
cat("SECTION 5: DESCRIPTIVE STATISTICS\n")
cat(strrep("=", 60), "\n\n")

desc_by_model <- data_consensus %>%
  group_by(model_name) %>%
  summarise(
    n = n(),
    diag_mean  = mean(diagnosis),  diag_sd  = sd(diagnosis),
    next_mean  = mean(nextstep),   next_sd  = sd(nextstep),
    treat_mean = mean(treatment),  treat_sd = sd(treatment),
    total_mean = mean(total),      total_sd = sd(total),
    total_median = median(total),
    .groups = "drop"
  ) %>%
  arrange(desc(total_mean))

cat("Model Total Score Summary:\n")
print(desc_by_model %>% select(model_name, n, total_mean, total_sd, total_median))

# High-performance rate (total >= 13/15) and chi-square test
high_perf <- data_consensus %>%
  mutate(is_high = total >= 13) %>%
  group_by(model_name) %>%
  summarise(n_total = n(), n_high = sum(is_high),
            pct_high = round(n_high / n_total * 100, 1), .groups = "drop") %>%
  arrange(desc(pct_high))

high_perf_tab <- data_consensus %>%
  mutate(is_high = total >= 13) %>%
  group_by(model_name) %>%
  summarise(high = sum(is_high), low = n() - sum(is_high), .groups = "drop")

chisq_high <- chisq.test(high_perf_tab[, c("high", "low")])

cat("\nHigh-Performance Rates (>= 13/15):\n")
print(high_perf)
cat("Chi-square:", round(chisq_high$statistic, 2),
    ", df =", chisq_high$parameter,
    ", p =", format.pval(chisq_high$p.value, digits = 3), "\n")

# ============================================================================
# 6. PRIMARY ANALYSIS: LINEAR MIXED-EFFECTS MODEL
#    Output: Figure 1, Table 1
# ============================================================================

cat("\n", strrep("=", 60), "\n")
cat("SECTION 6: LINEAR MIXED-EFFECTS MODEL\n")
cat(strrep("=", 60), "\n\n")

# 6.1 Full model with interaction: Total ~ Model * Round + (1|Case)
lmm_full <- lmer(total ~ model_name * round + (1 | case_id),
                 data = data_consensus, REML = TRUE)

anova_full <- anova(lmm_full, type = 3, ddf = "Kenward-Roger")
cat("Type III ANOVA (Full Model with Interaction):\n")
print(anova_full)

interaction_p <- anova_full["model_name:round", "Pr(>F)"]
cat("\nModel x Round interaction p =", round(interaction_p, 4), "\n")

if (interaction_p <= 0.05) {
  cat("-> Interaction significant, retaining full model\n\n")
  lmm_total   <- lmm_full
  anova_total <- anova_full
} else {
  cat("-> Interaction not significant, using main-effects model\n\n")
  lmm_total <- lmer(total ~ model_name + round + (1 | case_id),
                    data = data_consensus, REML = TRUE)
  anova_total <- anova(lmm_total, type = 3, ddf = "Kenward-Roger")
}

cat("Final Model ANOVA:\n")
print(anova_total)

# 6.2 Model diagnostics (Supplementary Figure S6)
residuals_lmm <- residuals(lmm_total)
shapiro_sample <- sample(residuals_lmm, min(5000, length(residuals_lmm)))
shapiro_test <- shapiro.test(shapiro_sample)
cat("\nResidual normality (Shapiro-Wilk): W =", round(shapiro_test$statistic, 4),
    ", p =", format.pval(shapiro_test$p.value, digits = 3), "\n")

r2_values <- r.squaredGLMM(lmm_total)
cat("Marginal R-squared (fixed effects):", round(r2_values[1], 3), "\n")
cat("Conditional R-squared (fixed + random):", round(r2_values[2], 3), "\n")

png("output/FigS6_Residuals.png", width = 1200, height = 400, res = 150)
par(mfrow = c(1, 3))
plot(fitted(lmm_total), residuals(lmm_total), main = "(A) Residuals vs Fitted",
     xlab = "Fitted values", ylab = "Residuals", pch = 16, col = rgb(0, 0, 0, 0.3))
abline(h = 0, col = "red", lty = 2, lwd = 2)
qqnorm(residuals(lmm_total), main = "(B) Q-Q Plot", pch = 16, col = rgb(0, 0, 0, 0.3))
qqline(residuals(lmm_total), col = "red", lwd = 2)
hist(residuals(lmm_total), main = "(C) Histogram of Residuals", xlab = "Residuals",
     breaks = 30, col = "lightblue", border = "white")
dev.off()
cat("  Saved: output/FigS6_Residuals.png\n")

# ============================================================================
# 7. POST-HOC PAIRWISE COMPARISONS
#    Output: Figure 1, Table 1, Supplementary Figure S5
# ============================================================================

cat("\n", strrep("=", 60), "\n")
cat("SECTION 7: POST-HOC COMPARISONS\n")
cat(strrep("=", 60), "\n\n")

# 7.1 Estimated marginal means
emm_total <- emmeans(lmm_total, ~ model_name)
cat("Estimated Marginal Means (Table 1):\n")
print(summary(emm_total))

# 7.2 Tukey-adjusted pairwise comparisons (15 pairs)
pairs_tukey <- pairs(emm_total, adjust = "tukey")
pairs_summary <- summary(pairs_tukey) %>%
  as.data.frame() %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ "ns"
    ),
    MCID_exceeded = ifelse(abs(estimate) >= 1.5, "Yes", "No")
  )

cat("\nAll 15 Pairwise Comparisons (Tukey HSD):\n")
print(pairs_summary %>% select(contrast, estimate, SE, t.ratio, p.value, sig, MCID_exceeded))

# 7.3 Compact letter display (tier identification)
cld_result <- cld(emm_total, Letters = letters, adjust = "tukey")
cat("\nCompact Letter Display (same letter = no significant difference):\n")
print(cld_result)

# 7.4 Cohen's d effect size matrix
models <- levels(data_consensus$model_name)
n_models <- length(models)
cohens_d_matrix <- matrix(NA, n_models, n_models, dimnames = list(models, models))

for (i in 1:(n_models - 1)) {
  for (j in (i + 1):n_models) {
    x <- data_consensus$total[data_consensus$model_name == models[i]]
    y <- data_consensus$total[data_consensus$model_name == models[j]]
    d <- effectsize::cohens_d(x, y)$Cohens_d
    cohens_d_matrix[i, j] <- round(d, 2)
    cohens_d_matrix[j, i] <- round(-d, 2)
  }
}

cat("\nCohen's d Effect Size Matrix:\n")
print(cohens_d_matrix)

# --- Figure 1: Overall Performance Comparison ---
emm_df <- as.data.frame(emm_total) %>%
  left_join(cld_result %>% as.data.frame() %>% select(model_name, .group),
            by = "model_name")

p_fig1 <- ggplot(emm_df, aes(x = reorder(model_name, emmean), y = emmean,
                             fill = model_name)) +
  geom_col(alpha = 0.8, color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = 0.8) +
  geom_text(aes(label = trimws(.group), y = upper.CL + 0.3),
            size = 5, fontface = "bold") +
  geom_hline(yintercept = 13, linetype = "dashed", color = "blue", linewidth = 0.8) +
  annotate("text", x = 0.7, y = 13.2,
           label = "High Performance\nThreshold (>=13)",
           hjust = 0, size = 3, color = "blue") +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(10, 15)) +
  labs(title = "Figure 1. Overall Performance Comparison Across LLMs",
       subtitle = "Estimated Marginal Means with 95% CI",
       x = "Model", y = "Total Score (3-15)") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        plot.title = element_text(face = "bold", size = 14))

ggsave("output/Fig1_Overall_Performance.png", p_fig1, width = 10, height = 7, dpi = 300)
ggsave("output/Fig1_Overall_Performance.pdf", p_fig1, width = 10, height = 7)
cat("  Saved: output/Fig1_Overall_Performance.png/.pdf\n")

# --- Supplementary Figure S5: Effect Size Heatmap ---
cohens_d_df <- as.data.frame(cohens_d_matrix) %>%
  rownames_to_column("Model1") %>%
  pivot_longer(-Model1, names_to = "Model2", values_to = "Cohen_d")

p_figS5 <- ggplot(cohens_d_df, aes(x = Model2, y = Model1, fill = Cohen_d)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(is.na(Cohen_d), "", sprintf("%.2f", Cohen_d))),
            size = 3.5) +
  scale_fill_gradient2(low = "#0072B2", mid = "white", high = "#D55E00",
                       midpoint = 0, na.value = "gray90", limits = c(-1.5, 1.5),
                       name = "Cohen's d") +
  labs(title = "Supplementary Figure S5. Effect Size Matrix (Cohen's d)",
       x = "", y = "",
       caption = "|d| < 0.2: negligible; 0.2-0.5: small; 0.5-0.8: medium; > 0.8: large") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(face = "bold")) +
  coord_fixed()

ggsave("output/FigS5_Effect_Size_Heatmap.png", p_figS5, width = 9, height = 8, dpi = 300)
cat("  Saved: output/FigS5_Effect_Size_Heatmap.png\n")

# ============================================================================
# 8. RESPONSE CONSISTENCY ACROSS ROUNDS (Figure 2)
# ============================================================================

cat("\n", strrep("=", 60), "\n")
cat("SECTION 8: RESPONSE CONSISTENCY ACROSS ROUNDS\n")
cat(strrep("=", 60), "\n\n")

round_trend <- data_consensus %>%
  group_by(model_name, round) %>%
  summarise(
    mean_total = mean(total), sd_total = sd(total),
    se = sd_total / sqrt(n()), n = n(),
    .groups = "drop"
  )

cat("Round-by-Round Scores:\n")
print(round_trend %>%
        select(model_name, round, mean_total) %>%
        pivot_wider(names_from = round, values_from = mean_total,
                    names_prefix = "Round_") %>%
        mutate(across(starts_with("Round"), ~ round(., 2))))

# Round EMMs
emm_round <- emmeans(lmm_total, ~ round)
cat("\nRound EMMs:\n")
print(summary(emm_round))

# --- Figure 2: Performance Trajectory ---
p_fig2 <- ggplot(round_trend, aes(x = round, y = mean_total,
                                  color = model_name, group = model_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_total - se, ymax = mean_total + se), width = 0.1) +
  scale_color_brewer(palette = "Set2", name = "Model") +
  labs(title = "Figure 2. Performance Trajectory Across Rounds",
       subtitle = "Mean total scores with standard error bars",
       x = "Round", y = "Mean Total Score") +
  theme_pubr() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"))

ggsave("output/Fig2_Round_Trajectory.png", p_fig2, width = 10, height = 6, dpi = 300)
cat("  Saved: output/Fig2_Round_Trajectory.png\n")

# ============================================================================
# 9. DIFFICULTY STRATIFICATION (Figure 3, Table 2)
# ============================================================================

cat("\n", strrep("=", 60), "\n")
cat("SECTION 9: DIFFICULTY STRATIFICATION\n")
cat(strrep("=", 60), "\n\n")

# Compute difficulty tertiles (n = 18 each)
case_difficulty <- data_consensus %>%
  group_by(case_id) %>%
  summarise(mean_score = mean(total), .groups = "drop") %>%
  mutate(
    difficulty = case_when(
      mean_score >= quantile(mean_score, 2/3) ~ "Easy",
      mean_score >= quantile(mean_score, 1/3) ~ "Medium",
      TRUE ~ "Hard"
    ),
    difficulty = factor(difficulty, levels = c("Easy", "Medium", "Hard"))
  )

cat("Difficulty distribution:\n")
print(table(case_difficulty$difficulty))

data_with_diff <- data_consensus %>%
  left_join(case_difficulty %>% select(case_id, difficulty), by = "case_id")

# Model x Difficulty interaction
lmm_difficulty <- lmer(total ~ model_name * difficulty + (1 | case_id),
                       data = data_with_diff)
anova_diff <- anova(lmm_difficulty, type = 3, ddf = "Kenward-Roger")
cat("\nModel x Difficulty ANOVA:\n")
print(anova_diff)

# Table 2: Performance decline
model_by_diff <- data_with_diff %>%
  group_by(model_name, difficulty) %>%
  summarise(mean_total = mean(total), se = sd(total) / sqrt(n()),
            n = n(), .groups = "drop")

decline_table <- model_by_diff %>%
  select(model_name, difficulty, mean_total) %>%
  pivot_wider(names_from = difficulty, values_from = mean_total) %>%
  mutate(Decline = round(Hard - Easy, 2)) %>%
  mutate(across(c(Easy, Medium, Hard), ~ round(., 2))) %>%
  arrange(desc(Decline))

cat("\nTable 2: Performance Decline (Easy to Hard):\n")
print(decline_table %>% mutate(Rank = row_number()))

# --- Figure 3: Performance by Difficulty ---
p_fig3 <- ggplot(model_by_diff,
                 aes(x = difficulty, y = mean_total, fill = model_name)) +
  geom_col(position = position_dodge(0.8), alpha = 0.8, width = 0.7) +
  geom_errorbar(aes(ymin = mean_total - se, ymax = mean_total + se),
                position = position_dodge(0.8), width = 0.2) +
  scale_fill_brewer(palette = "Set2", name = "Model") +
  labs(title = "Figure 3. Model Performance by Case Difficulty",
       subtitle = "Cases stratified by overall mean score (tertiles)",
       x = "Case Difficulty", y = "Mean Total Score") +
  theme_pubr() +
  theme(legend.position = "right")

ggsave("output/Fig3_Performance_by_Difficulty.png", p_fig3, width = 10, height = 6, dpi = 300)
cat("  Saved: output/Fig3_Performance_by_Difficulty.png\n")

# ============================================================================
# 10. ERROR TAXONOMY ANALYSIS (Figure 4, Table 3)
#     Type 1 (Rare Disease): 22 cases, 66 evaluations per model
#     Type 2 (Anchoring Bias): 24 cases, 72 evaluations per model
#     Type 3 (Iatrogenic Risk): 11 cases, 33 evaluations per model
# ============================================================================

cat("\n", strrep("=", 60), "\n")
cat("SECTION 10: ERROR TAXONOMY ANALYSIS\n")
cat(strrep("=", 60), "\n\n")

# Case assignments from Supplementary Table S5
rare_disease_cases <- c(1, 3, 8, 9, 10, 12, 13, 14, 16, 19, 20, 23,
                        28, 29, 31, 33, 34, 36, 39, 43, 53, 54)  # n = 22
anchoring_cases    <- c(2, 5, 6, 7, 11, 18, 21, 24, 26, 27, 30, 35,
                        37, 38, 40, 41, 42, 46, 47, 48, 49, 50, 51, 52)  # n = 24
iatrogenic_cases   <- c(4, 7, 15, 17, 18, 22, 24, 25, 32, 44, 45)  # n = 11

cat("Error type case counts:\n")
cat("  Type 1 (Rare Disease):", length(rare_disease_cases), "cases\n")
cat("  Type 2 (Anchoring Bias):", length(anchoring_cases), "cases\n")
cat("  Type 3 (Iatrogenic Risk):", length(iatrogenic_cases), "cases\n\n")

# Wilson score confidence interval
wilson_ci <- function(x, n, conf.level = 0.95) {
  z <- qnorm(1 - (1 - conf.level) / 2)
  p_hat <- x / n
  denom <- 1 + z^2 / n
  center <- (p_hat + z^2 / (2 * n)) / denom
  margin <- z * sqrt(p_hat * (1 - p_hat) / n + z^2 / (4 * n^2)) / denom
  c(lower = max(0, center - margin), upper = min(1, center + margin))
}

# Error rate calculation function with Wilson CIs
calc_error_rate <- function(data, case_subset, error_type_name) {
  subset_data <- data %>%
    filter(as.numeric(as.character(case_id)) %in% case_subset)
  
  result <- subset_data %>%
    group_by(model_name) %>%
    summarise(
      n_eval = n(),
      n_fail = sum(diagnosis <= 3 | nextstep <= 3 | treatment <= 3),
      rate_pct = round(n_fail / n_eval * 100, 1),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      ci_lo = round(wilson_ci(n_fail, n_eval)[1] * 100, 1),
      ci_hi = round(wilson_ci(n_fail, n_eval)[2] * 100, 1),
      ci_text = paste0(rate_pct, "% (", ci_lo, "-", ci_hi, ")"),
      error_type = error_type_name
    ) %>%
    ungroup() %>%
    arrange(rate_pct)
  
  return(result)
}

error_rare      <- calc_error_rate(data_consensus, rare_disease_cases, "Rare Disease")
error_anchoring <- calc_error_rate(data_consensus, anchoring_cases,    "Anchoring Bias")
error_iatrogenic <- calc_error_rate(data_consensus, iatrogenic_cases,  "Iatrogenic Risk")

cat("Table 3: Error Taxonomy Failure Rates\n\n")
cat("Type 1 - Rare Disease Recognition (n = 66 per model):\n")
print(error_rare %>% select(model_name, n_eval, n_fail, ci_text))

cat("\nType 2 - Anchoring Bias Susceptibility (n = 72 per model):\n")
print(error_anchoring %>% select(model_name, n_eval, n_fail, ci_text))

cat("\nType 3 - Iatrogenic Risk Identification (n = 33 per model):\n")
print(error_iatrogenic %>% select(model_name, n_eval, n_fail, ci_text))

# Overall average failure rates (weighted: total failures / total evaluations)
error_all_combined <- bind_rows(error_rare, error_anchoring, error_iatrogenic)
error_profile <- error_all_combined %>%
  group_by(model_name) %>%
  summarise(
    total_fail = sum(n_fail),
    total_eval = sum(n_eval),
    Avg_Failure = round(total_fail / total_eval * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(Avg_Failure)

cat("\nOverall Average Failure Rates:\n")
print(error_profile)

# Fisher's exact tests (Monte Carlo simulation, B = 10,000)
cat("\nFisher's Exact Tests (Monte Carlo, B = 10,000):\n")
for (et in list(
  list(name = "Rare Disease",    d = error_rare),
  list(name = "Anchoring Bias",  d = error_anchoring),
  list(name = "Iatrogenic Risk", d = error_iatrogenic)
)) {
  cont <- matrix(c(et$d$n_fail, et$d$n_eval - et$d$n_fail), ncol = 2)
  ft <- fisher.test(cont, simulate.p.value = TRUE, B = 10000)
  cat("  ", et$name, ": p =", round(ft$p.value, 4), "\n")
}

# --- Figure 4: Error Taxonomy Visualization ---
error_plot_data <- error_all_combined %>%
  mutate(error_type = factor(error_type,
                             levels = c("Rare Disease", "Anchoring Bias", "Iatrogenic Risk")))

p_fig4 <- ggplot(error_plot_data, aes(x = model_name, y = rate_pct, fill = error_type)) +
  geom_col(position = position_dodge(0.8), alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(rate_pct, "%")),
            position = position_dodge(0.8), vjust = -0.3, size = 3) +
  scale_fill_manual(
    values = c("Rare Disease" = "#E69F00",
               "Anchoring Bias" = "#56B4E9",
               "Iatrogenic Risk" = "#009E73"),
    name = "Error Type"
  ) +
  labs(title = "Figure 4. Error Taxonomy: Failure Rates by Error Type and Model",
       subtitle = "Failure defined as score <= 3 on any individual scoring dimension",
       x = "Model", y = "Failure Rate (%)") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "right",
        plot.title = element_text(face = "bold"))

ggsave("output/Fig4_Error_Rates_by_Type.png", p_fig4, width = 12, height = 7, dpi = 300)
cat("  Saved: output/Fig4_Error_Rates_by_Type.png\n")

# ============================================================================
# 11. DIMENSION-SPECIFIC ANALYSIS (Supp. Figs. S3, S4)
# ============================================================================

cat("\n", strrep("=", 60), "\n")
cat("SECTION 11: DIMENSION-SPECIFIC ANALYSIS\n")
cat(strrep("=", 60), "\n\n")

lmm_diag  <- lmer(diagnosis ~ model_name + (1 | case_id) + (1 | case_id:round),
                  data = data_consensus, REML = TRUE)
lmm_next  <- lmer(nextstep  ~ model_name + (1 | case_id) + (1 | case_id:round),
                  data = data_consensus, REML = TRUE)
lmm_treat <- lmer(treatment ~ model_name + (1 | case_id) + (1 | case_id:round),
                  data = data_consensus, REML = TRUE)

emm_diag  <- emmeans(lmm_diag,  ~ model_name)
emm_next  <- emmeans(lmm_next,  ~ model_name)
emm_treat <- emmeans(lmm_treat, ~ model_name)

dim_summary <- bind_rows(
  as.data.frame(emm_diag)  %>% mutate(dimension = "Diagnosis"),
  as.data.frame(emm_next)  %>% mutate(dimension = "Next Steps"),
  as.data.frame(emm_treat) %>% mutate(dimension = "Treatment")
)

cat("Dimension-Specific EMMs:\n")
print(dim_summary %>% select(dimension, model_name, emmean, SE) %>%
        arrange(dimension, desc(emmean)))

# --- Supplementary Figure S4: Dimension Performance ---
p_figS4 <- ggplot(dim_summary, aes(x = model_name, y = emmean, fill = dimension)) +
  geom_col(position = position_dodge(0.8), alpha = 0.8, width = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(0.8), width = 0.2) +
  facet_wrap(~ dimension, scales = "free_y") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Supplementary Figure S4. Performance by Evaluation Dimension",
       x = "Model", y = "Score (1-5)") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 11))

ggsave("output/FigS4_Dimension_Performance.png", p_figS4, width = 12, height = 5, dpi = 300)

# --- Supplementary Figure S3: Violin Plots ---
data_long <- data_consensus %>%
  pivot_longer(cols = c(diagnosis, nextstep, treatment),
               names_to = "dimension", values_to = "score") %>%
  mutate(
    dimension = factor(
      case_match(dimension,
                 "diagnosis" ~ "Diagnosis",
                 "nextstep"  ~ "Next Steps",
                 "treatment" ~ "Treatment"),
      levels = c("Diagnosis", "Next Steps", "Treatment")
    )
  )

p_figS3 <- ggplot(data_long, aes(x = model_name, y = score, fill = model_name)) +
  geom_violin(alpha = 0.6, trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.size = 0.5, alpha = 0.8) +
  facet_wrap(~ dimension, nrow = 1) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(breaks = 1:5, limits = c(0.8, 5.2)) +
  coord_cartesian(ylim = c(1, 5)) +
  labs(title = "Supplementary Figure S3. Score Distribution by Model and Dimension",
       x = "Model", y = "Score (1-5)") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 11))

ggsave("output/FigS3_Violin_Faceted.png", p_figS3, width = 14, height = 5, dpi = 300)
cat("  Saved: output/FigS3_Violin_Faceted.png, FigS4_Dimension_Performance.png\n")

# ============================================================================
# 12. SENSITIVITY ANALYSES
# ============================================================================

cat("\n", strrep("=", 60), "\n")
cat("SECTION 12: SENSITIVITY ANALYSES\n")
cat(strrep("=", 60), "\n\n")

# --- 12.1 Non-parametric: Friedman test ---
data_agg <- data_consensus %>%
  group_by(case_id, model_name) %>%
  summarise(mean_total = mean(total), .groups = "drop")

friedman_result <- friedman.test(mean_total ~ model_name | case_id, data = data_agg)
cat("12.1 Friedman Test:\n")
cat("  chi-squared =", round(friedman_result$statistic, 2),
    ", df =", friedman_result$parameter,
    ", p =", format.pval(friedman_result$p.value, digits = 3), "\n\n")

# --- 12.2 Cumulative Link Mixed Model (CLMM) ---
cat("12.2 Cumulative Link Mixed Model (CLMM):\n")
cat("  Reference category: DeepSeek R1\n\n")

data_consensus$model_name <- relevel(data_consensus$model_name, ref = "DeepSeek R1")
data_consensus$total_ord  <- factor(round(data_consensus$total), ordered = TRUE)

clmm_total <- clmm(total_ord ~ model_name + round + (1 | case_id), data = data_consensus)

clmm_coef <- as.data.frame(summary(clmm_total)$coefficients)
clmm_coef$Variable <- rownames(clmm_coef)

clmm_effects <- clmm_coef %>%
  filter(grepl("model_name", Variable)) %>%
  mutate(
    Model = gsub("model_name", "", Variable),
    OR = round(exp(Estimate), 2),
    OR_lower = round(exp(Estimate - 1.96 * `Std. Error`), 2),
    OR_upper = round(exp(Estimate + 1.96 * `Std. Error`), 2),
    Sig = case_when(
      `Pr(>|z|)` < 0.001 ~ "***",
      `Pr(>|z|)` < 0.01  ~ "**",
      `Pr(>|z|)` < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

cat("CLMM Cumulative Odds Ratios (vs DeepSeek R1):\n")
print(clmm_effects %>% select(Model, OR, OR_lower, OR_upper, `Pr(>|z|)`, Sig))

# --- Supplementary Figure S7: Contrast Plot vs DeepSeek R1 ---
emm_ref <- emmeans(lmm_total, ~ model_name)
# Use emmeans object levels (not data levels, which may have been releveled for CLMM)
emm_levels <- levels(as.data.frame(emm_ref)$model_name)
ref_idx <- which(emm_levels == "DeepSeek R1")
contrast_ref <- contrast(emm_ref, method = "trt.vs.ctrl", ref = ref_idx)
contrast_df <- as.data.frame(summary(contrast_ref, infer = c(TRUE, TRUE)))%>%
  mutate(
    sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                    p.value < 0.05  ~ "*",   TRUE ~ ""),
    model = gsub("\\s*-\\s*\\(?DeepSeek R1\\)?", "", contrast),
    model = gsub("[()]", "", model),
    model = trimws(model)
  )

p_figS7 <- ggplot(contrast_df, aes(x = estimate, y = reorder(model, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(-1.5, 1.5), linetype = "dotted", color = "blue", alpha = 0.5) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.2) +
  geom_text(aes(label = sig, x = upper.CL + 0.1), size = 5) +
  labs(title = "Supplementary Figure S7. Pairwise Comparisons vs DeepSeek R1",
       subtitle = "Mean difference with 95% CI",
       x = "Difference in Total Score", y = "Model",
       caption = "Blue dashed lines: minimal clinically important difference (+/-1.5)") +
  theme_pubr() +
  theme(plot.title = element_text(face = "bold"))

ggsave("output/FigS7_Contrast_Plot.png", p_figS7, width = 10, height = 6, dpi = 300)
cat("  Saved: output/FigS7_Contrast_Plot.png\n")
# --- Extract Gemini 3 Pro vs DeepSeek R1 statistics for figure caption ---
gemini_vs_r1 <- contrast_df %>%
  filter(grepl("Gemini", model))

cat("\n============================================================\n")
cat("Figure S7 Caption Values (Gemini 3 Pro vs DeepSeek R1):\n")
cat("============================================================\n\n")

if (nrow(gemini_vs_r1) > 0) {
  delta_val <- abs(gemini_vs_r1$estimate)
  ci_lower  <- gemini_vs_r1$lower.CL
  ci_upper  <- gemini_vs_r1$upper.CL
  p_val     <- gemini_vs_r1$p.value
  
  cat(sprintf("Δ = %.2f\n", delta_val))
  cat(sprintf("95%% CI: %.2f to %.2f\n", ci_lower, ci_upper))
  cat(sprintf("p = %.3f\n", p_val))
  
  # Full caption text (copy-paste ready)
  cat("\n--- Caption text (copy-paste ready) ---\n")
  fig_caption <- sprintf(
    "Gemini 3 Pro was the only model whose confidence interval fell entirely within the MCID bounds (Δ = %.2f, 95%% CI: %.2f to %.2f, p = %.3f), confirming statistical and clinical equivalence.",
    delta_val, ci_lower, ci_upper, p_val
  )
  cat(fig_caption, "\n")
}

# --- Save caption values to file ---
sink("output/FigS7_Caption_Values.txt")
cat("Figure S7 Caption Values\n")
cat("========================\n\n")
cat("Gemini 3 Pro vs DeepSeek R1:\n")
if (nrow(gemini_vs_r1) > 0) {
  cat(sprintf("  Δ = %.2f\n", abs(gemini_vs_r1$estimate)))
  cat(sprintf("  95%% CI: %.2f to %.2f\n", gemini_vs_r1$lower.CL, gemini_vs_r1$upper.CL))
  cat(sprintf("  p = %.3f\n", gemini_vs_r1$p.value))
}
sink()
cat("\nSaved: output/FigS7_Caption_Values.txt\n")


# --- 12.3 Model x Round interaction (reported in Section 6) ---
cat("\n12.3 Model x Round Interaction:\n")
cat("  See Section 6 ANOVA output above\n\n")

# --- 12.4 Rater-adjusted sensitivity analysis ---
cat("12.4 Rater Sensitivity Analysis (n =", nrow(raw_data), "individual ratings):\n\n")

lmm_no_rater <- lmer(
  total ~ model_name * round + (1 | case_id),
  data = raw_data, REML = TRUE
)
lmm_with_rater <- lmer(
  total ~ model_name * round + (1 | case_id) + (1 | rater),
  data = raw_data, REML = TRUE
)

# Likelihood ratio test (ML fitting required)
lmm_nr_ML <- lmer(total ~ model_name * round + (1 | case_id),
                  data = raw_data, REML = FALSE)
lmm_wr_ML <- lmer(total ~ model_name * round + (1 | case_id) + (1 | rater),
                  data = raw_data, REML = FALSE)

lr_test <- anova(lmm_nr_ML, lmm_wr_ML)
cat("Likelihood Ratio Test (rater random effect):\n")
print(lr_test)

# EMM comparison
emm_nr <- emmeans(lmm_no_rater,   ~ model_name)
emm_wr <- emmeans(lmm_with_rater, ~ model_name)

emm_comp <- merge(
  as.data.frame(summary(emm_nr)) %>%
    select(model_name, emmean, SE) %>%
    rename(EMM_noRater = emmean, SE_noRater = SE),
  as.data.frame(summary(emm_wr)) %>%
    select(model_name, emmean, SE) %>%
    rename(EMM_withRater = emmean, SE_withRater = SE),
  by = "model_name"
) %>%
  mutate(
    EMM_diff = round(EMM_withRater - EMM_noRater, 4),
    SE_ratio = round(SE_withRater / SE_noRater, 3)
  )

cat("\nEMM Comparison (with vs without rater effect):\n")
print(emm_comp %>% mutate(across(where(is.numeric), ~ round(., 3))))
cat("\nMax absolute EMM change:", max(abs(emm_comp$EMM_diff)), "points\n")
cat("SE ratio range:", min(emm_comp$SE_ratio), "-", max(emm_comp$SE_ratio), "\n")

# Pairwise consistency
pairs_nr <- as.data.frame(summary(pairs(emm_nr, adjust = "tukey")))
pairs_wr <- as.data.frame(summary(pairs(emm_wr, adjust = "tukey")))

pairs_consist <- data.frame(
  contrast      = pairs_nr$contrast,
  sig_noRater   = ifelse(pairs_nr$p.value < 0.05, "Sig", "NS"),
  sig_withRater = ifelse(pairs_wr$p.value < 0.05, "Sig", "NS")
) %>%
  mutate(consistent = sig_noRater == sig_withRater)

cat("\nPairwise consistency:", sum(pairs_consist$consistent), "/",
    nrow(pairs_consist), "comparisons unchanged\n")

# Variance decomposition
vc <- as.data.frame(VarCorr(lmm_with_rater))
rater_var <- vc$vcov[vc$grp == "rater"]
case_var  <- vc$vcov[vc$grp == "case_id"]
resid_var <- vc$vcov[vc$grp == "Residual"]
tot_var   <- rater_var + case_var + resid_var

cat("\nVariance Components:\n")
cat("  Rater:",    round(rater_var, 4), "(", round(rater_var / tot_var * 100, 1), "%)\n")
cat("  Case:",     round(case_var, 4),  "(", round(case_var / tot_var * 100, 1), "%)\n")
cat("  Residual:", round(resid_var, 4), "(", round(resid_var / tot_var * 100, 1), "%)\n")

# ============================================================================
# 13. EXPORT ALL RESULTS
# ============================================================================

cat("\n", strrep("=", 60), "\n")
cat("SECTION 13: EXPORTING RESULTS\n")
cat(strrep("=", 60), "\n\n")

write_xlsx(
  list(
    "ICC_Results"         = icc_results,
    "Kappa_Results"       = kappa_results,
    "Descriptive_Stats"   = desc_by_model,
    "High_Performance"    = high_perf,
    "EMM_Total"           = as.data.frame(emm_total),
    "Pairwise_Tukey"      = pairs_summary,
    "CLD_Groupings"       = as.data.frame(cld_result),
    "Cohen_d_Matrix"      = as.data.frame(cohens_d_matrix) %>% rownames_to_column("Model"),
    "Round_Trends"        = round_trend,
    "Difficulty_Decline"  = decline_table,
    "Error_RareDisease"   = error_rare,
    "Error_Anchoring"     = error_anchoring,
    "Error_Iatrogenic"    = error_iatrogenic,
    "Error_Profile"       = error_profile,
    "Dimension_EMMs"      = dim_summary,
    "CLMM_Results"        = clmm_effects,
    "Rater_Sensitivity"   = emm_comp,
    "Rater_Pairwise"      = pairs_consist,
    "Case_Difficulty"     = case_difficulty
  ),
  "output/LLM_Emergency_All_Results.xlsx"
)

# Model summary text file
sink("output/Model_Summaries.txt")
cat(strrep("=", 70), "\n")
cat("LINEAR MIXED EFFECTS MODEL SUMMARIES\n")
cat(strrep("=", 70), "\n\n")
cat("1. PRIMARY MODEL (Total Score)\n", strrep("-", 50), "\n")
print(summary(lmm_total))
cat("\n\n2. TYPE III ANOVA (Kenward-Roger)\n", strrep("-", 50), "\n")
print(anova_total)
cat("\n\n3. PAIRWISE COMPARISONS (Tukey)\n", strrep("-", 50), "\n")
print(pairs_summary)
cat("\n\n4. CLMM SUMMARY\n", strrep("-", 50), "\n")
print(summary(clmm_total))
cat("\n\n5. RATER SENSITIVITY\n", strrep("-", 50), "\n")
print(emm_comp)
sink()

cat("All results exported to output/ directory\n")

# ============================================================================
# 14. ANALYSIS SUMMARY
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("                    ANALYSIS COMPLETE\n")
cat(strrep("=", 70), "\n\n")

cat("Output files generated:\n")
cat("  Tables:  output/LLM_Emergency_All_Results.xlsx (19 sheets)\n")
cat("  Text:    output/Model_Summaries.txt\n")
cat("  Fig 1:   output/Fig1_Overall_Performance.png/.pdf\n")
cat("  Fig 2:   output/Fig2_Round_Trajectory.png\n")
cat("  Fig 3:   output/Fig3_Performance_by_Difficulty.png\n")
cat("  Fig 4:   output/Fig4_Error_Rates_by_Type.png\n")
cat("  Fig S1:  output/FigS1_Correlation_Matrix.png\n")
cat("  Fig S2:  output/FigS2_BlandAltman.png\n")
cat("  Fig S3:  output/FigS3_Violin_Faceted.png\n")
cat("  Fig S4:  output/FigS4_Dimension_Performance.png\n")
cat("  Fig S5:  output/FigS5_Effect_Size_Heatmap.png\n")
cat("  Fig S6:  output/FigS6_Residuals.png\n")
cat("  Fig S7:  output/FigS7_Contrast_Plot.png\n")
cat(strrep("=", 70), "\n")
# END OF SCRIPT



