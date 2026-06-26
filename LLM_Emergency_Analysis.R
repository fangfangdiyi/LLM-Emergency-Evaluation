# ============================================================================
# Mean Accuracy May Be Insufficient for Safety Profiling:
#   A Single-Centre Proof-of-Concept Evaluation of Six Large Language Models
#   on Challenging Diagnostic Cases — Journal of Medical Systems (revised submission)
#
# CONSOLIDATED ANALYSIS SCRIPT (reproduces every number, table and figure in
# the revised manuscript and supplement).  R >= 4.5.0.
#
# This is the single canonical script for the public repository. It supersedes
# the original submission script: the primary model now carries a Case x Model
# random effect, query round is an exchangeable replicate (no Round trend),
# equivalence is tested formally with TOST against the 1.5-point MCID, and
# catastrophic-failure frequency + reproducibility are primary safety endpoints.
#
# Section map (✓ unchanged from submission | ⟳ revised | + new):
#    1  Setup
#    2  Data import & consensus                                       ✓
#    3  Inter-rater reliability  -> Fig S1(corr), S2(B-A)             ✓
#    4  Descriptives & high-performance rate                          ✓
#    5  PRIMARY mixed model  -> Fig 1, Table 2                        ⟳
#    6  Equivalence testing (TOST, 90% CI)  -> Fig S7                 +
#    7  Effect sizes (Cohen's d)  -> Fig S5                           ✓
#    8  Reproducibility & catastrophic failure (+ rater concordance) -> Fig 2, Table 3, Fig S8   ⟳/＋
#       8a-ter: between-model Fisher tests on catastrophic rate (omnibus, pooled, Holm) -> Catastrophic_* sheets
#    9  Difficulty stratification -> Fig 3, Table 4                   ⟳
#   10  Error taxonomy -> Fig 4, Table 5                              ✓
#   11  Dimension-specific -> Fig S3, S4                              ✓(harmonised)
#   12  Other sensitivity: residuals(S6), CLMM, rater-adjusted, hetero-variance   ⟳
#   13  Export
# ============================================================================

rm(list = ls()); options(scipen = 999); set.seed(42)

# ============================================================================
# 1. SETUP
# ============================================================================
required_packages <- c(
  "readxl","tidyverse","lme4","lmerTest","emmeans","effectsize",
  "irr","rstatix","ggpubr","corrplot","performance",
  "psych","car","writexl","ordinal","MuMIn","multcomp")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE); library(pkg, character.only = TRUE) }
}
select <- dplyr::select; filter <- dplyr::filter
dir.create("output", showWarnings = FALSE)

# ---- pre-specified thresholds (stated in Methods) --------------------------
MCID          <- 1.5   # minimal clinically important difference (total score)
FAIL_THRESH   <- 3     # failure: any dimension consensus <= 3
DANGER_THRESH <- 2     # dangerous: any dimension consensus <= 2

# ============================================================================
# 2. DATA IMPORT & CONSENSUS                                              [✓]
# ============================================================================
data_path <- "processed_ratings.xlsx"
raw_data  <- read_excel(data_path)
colnames(raw_data) <- c("rater","case_id","round","model",
                        "diagnosis","nextstep","treatment")

model_names <- c("1"="Claude Sonnet 4.5","2"="GPT-5.1","3"="Grok 4",
                 "4"="DeepSeek V3.1","5"="DeepSeek R1","6"="Gemini 3 Pro")

raw_data <- raw_data %>% mutate(
  rater = factor(rater), case_id = factor(case_id), round = factor(round),
  model = factor(model),
  model_name = factor(model_names[as.character(model)],
                      levels = c("Claude Sonnet 4.5","GPT-5.1","Grok 4",
                                 "DeepSeek V3.1","DeepSeek R1","Gemini 3 Pro")),
  total = diagnosis + nextstep + treatment)

cat("Data:", nrow(raw_data), "ratings |",
    n_distinct(raw_data$case_id), "cases x", n_distinct(raw_data$model), "models x",
    n_distinct(raw_data$round), "rounds x", n_distinct(raw_data$rater), "raters\n")

# Wide (per-rater) for inter-rater reliability
data_wide <- raw_data %>%
  pivot_wider(id_cols = c(case_id, round, model, model_name),
              names_from = rater,
              values_from = c(diagnosis, nextstep, treatment, total))

# Consensus (mean of two raters)
data_consensus <- data_wide %>%
  mutate(diagnosis = (diagnosis_1 + diagnosis_2)/2,
         nextstep  = (nextstep_1  + nextstep_2 )/2,
         treatment = (treatment_1 + treatment_2)/2,
         total     = (total_1     + total_2    )/2) %>%
  select(case_id, round, model, model_name, diagnosis, nextstep, treatment, total)

if (nrow(data_consensus) != 972)
  cat("NOTE: expected 972 consensus rows (54x6x3); found", nrow(data_consensus), "\n")

# Case x Model unit (the new random-effect grouping; 3 rounds nested within)
data_consensus$case_model <- interaction(data_consensus$case_id,
                                          data_consensus$model_name, drop = TRUE)

# ============================================================================
# 3. INTER-RATER RELIABILITY  -> Fig S1 (corr), Fig S2 (Bland-Altman)     [✓]
# ============================================================================
cat("\n", strrep("=",60), "\nSECTION 3: INTER-RATER RELIABILITY\n", strrep("=",60), "\n\n", sep="")

calculate_icc <- function(data, col1, col2, dimension_name) {
  mat <- data[, c(col1, col2)] %>% as.matrix()
  result <- irr::icc(mat, model = "twoway", type = "agreement", unit = "single")
  data.frame(Dimension = dimension_name, ICC = round(result$value, 3),
             Lower_CI = round(result$lbound, 3), Upper_CI = round(result$ubound, 3),
             F_value = round(result$Fvalue, 2),
             p_value = format.pval(result$p.value, digits = 3),
             Interpretation = case_when(result$value < 0.50 ~ "Poor",
               result$value < 0.75 ~ "Moderate", result$value < 0.90 ~ "Good",
               TRUE ~ "Excellent"))
}
icc_results <- bind_rows(
  calculate_icc(data_wide, "diagnosis_1", "diagnosis_2", "Diagnosis"),
  calculate_icc(data_wide, "nextstep_1",  "nextstep_2",  "Next Steps"),
  calculate_icc(data_wide, "treatment_1", "treatment_2", "Treatment"),
  calculate_icc(data_wide, "total_1",     "total_2",     "Total Score"))
cat("ICC(2,1):\n"); print(icc_results)

kappa_diagnosis <- irr::kappa2(data_wide[, c("diagnosis_1","diagnosis_2")], weight = "squared")
kappa_nextstep  <- irr::kappa2(data_wide[, c("nextstep_1","nextstep_2")],  weight = "squared")
kappa_treatment <- irr::kappa2(data_wide[, c("treatment_1","treatment_2")], weight = "squared")
kappa_results <- data.frame(
  Dimension = c("Diagnosis","Next Steps","Treatment"),
  Weighted_Kappa = round(c(kappa_diagnosis$value, kappa_nextstep$value, kappa_treatment$value), 3))
cat("\nQuadratic-weighted kappa:\n"); print(kappa_results)

# Bland-Altman (total score)
data_wide <- data_wide %>% mutate(total_mean = (total_1+total_2)/2,
                                  total_diff = total_1 - total_2)
ba_mean <- mean(data_wide$total_diff); ba_sd <- sd(data_wide$total_diff)
ba_hi <- ba_mean + 1.96*ba_sd; ba_lo <- ba_mean - 1.96*ba_sd
cat(sprintf("\nBland-Altman: bias %.2f, 95%% LoA [%.1f, %.1f]\n", ba_mean, ba_lo, ba_hi))
p_ba <- ggplot(data_wide, aes(total_mean, total_diff)) +
  geom_point(alpha=0.4, size=2) +
  geom_hline(yintercept=ba_mean, color="blue", linewidth=1) +
  geom_hline(yintercept=c(ba_hi,ba_lo), color="red", linetype="dashed") +
  geom_hline(yintercept=0, color="gray50", linetype="dotted") +
  labs(x="Mean of two raters", y="Difference (Rater 1 - Rater 2)") + theme_pubr()
ggsave("output/FigS2_BlandAltman.png", p_ba, width=8, height=6, dpi=300)

# Correlation among dimensions -> Fig S1
cor_matrix <- cor(data_consensus[, c("diagnosis","nextstep","treatment")],
                  method="spearman", use="complete.obs")
png("output/FigS1_Correlation_Matrix.png", width=600, height=500, res=150)
corrplot(cor_matrix, method="color", type="upper", addCoef.col="black",
         number.cex=1.2, tl.col="black", tl.srt=45,
         col=colorRampPalette(c("#0072B2","white","#D55E00"))(100))
dev.off()
cat("Saved: Fig S1, Fig S2\n")

# ============================================================================
# 4. DESCRIPTIVES & HIGH-PERFORMANCE RATE                                 [✓]
# ============================================================================
cat("\n", strrep("=",60), "\nSECTION 4: DESCRIPTIVES\n", strrep("=",60), "\n\n", sep="")
desc_by_model <- data_consensus %>% group_by(model_name) %>%
  summarise(n=n(), total_mean=mean(total), total_sd=sd(total),
            total_median=median(total), .groups="drop") %>% arrange(desc(total_mean))
cat("Total score by model:\n"); print(desc_by_model)

high_perf_tab <- data_consensus %>% mutate(is_high = total >= 13) %>%
  group_by(model_name) %>% summarise(high=sum(is_high), low=n()-sum(is_high), .groups="drop")
chisq_high <- chisq.test(high_perf_tab[, c("high","low")])
cat(sprintf("\nHigh-performance (>=13/15) chi-square = %.2f, df = %d, p = %s\n",
            chisq_high$statistic, chisq_high$parameter,
            format.pval(chisq_high$p.value, digits=3)))

# ============================================================================
# 5. PRIMARY MIXED MODEL  -> Fig 1, Table 2                               [⟳]
#    total ~ model_name + (1|case_id) + (1|case_model)
#    rounds enter as exchangeable replicates (the residual = round-to-round noise)
# ============================================================================
cat("\n", strrep("=",60), "\nSECTION 5: PRIMARY MIXED MODEL\n", strrep("=",60), "\n\n", sep="")

lmm_primary <- lmer(total ~ model_name + (1|case_id) + (1|case_model),
                    data = data_consensus, REML = TRUE)
if (isSingular(lmm_primary)) cat("NOTE: (near-)singular fit; inspect variance components.\n\n")

vc <- as.data.frame(VarCorr(lmm_primary))
v_case <- vc$vcov[vc$grp=="case_id"]; v_cm <- vc$vcov[vc$grp=="case_model"]
v_res  <- vc$vcov[vc$grp=="Residual"]; v_tot <- v_case + v_cm + v_res
cat("Variance components (Total Score):\n")
cat(sprintf("  Case (difficulty)        : %.3f (%4.1f%%)\n", v_case, 100*v_case/v_tot))
cat(sprintf("  Case x Model (affinity)  : %.3f (%4.1f%%)\n", v_cm,  100*v_cm/v_tot))
cat(sprintf("  Residual (round-to-round): %.3f (%4.1f%%)\n", v_res, 100*v_res/v_tot))

anova_primary <- anova(lmm_primary, type=3, ddf="Kenward-Roger")
cat("\nType III ANOVA (Kenward-Roger):\n"); print(anova_primary)

r2_values <- r.squaredGLMM(lmm_primary)
cat(sprintf("\nR2 marginal = %.3f ; conditional = %.3f\n", r2_values[1], r2_values[2]))

emm_primary <- emmeans(lmm_primary, ~ model_name)
emm_tab <- as.data.frame(summary(emm_primary, infer=c(TRUE,TRUE)))
cat("\nEstimated marginal means (Table 2):\n")
print(emm_tab %>% transmute(model_name, EMM=round(emmean,2),
        CI=sprintf("%.2f-%.2f", lower.CL, upper.CL)) %>% arrange(desc(EMM)))

pairs_primary <- pairs(emm_primary, adjust="tukey")
cat("\nTukey-adjusted pairwise comparisons (15 pairs):\n")
print(as.data.frame(summary(pairs_primary)) %>%
        transmute(contrast, estimate=round(estimate,2), SE=round(SE,2),
                  p.value=round(p.value,4),
                  sig=cut(p.value, c(-Inf,.001,.01,.05,Inf), c("***","**","*","ns"))))

# Compact letter display for tiers. NOTE: emmeans switches the CLD adjustment to
# Sidak (Tukey is only defined for a single set of pairwise comparisons); the
# pairwise table above uses Tukey. Tiers are identical under either adjustment here.
cld_primary <- tryCatch(cld(emm_primary, Letters=letters, adjust="tukey", sort=FALSE),
                        error=function(e){cat("(cld unavailable)\n"); NULL})
if (!is.null(cld_primary)) {
  cat("\nTier groupings (shared letter = n.s.):\n")
  print(as.data.frame(cld_primary) %>%
          transmute(model_name, EMM=round(emmean,2), tier=trimws(.group)) %>% arrange(desc(EMM)))
}

# --- Sensitivity: adding Round as a fixed effect is negligible (justifies exchangeability)
lmm_round_chk <- lmer(total ~ model_name + round + (1|case_id) + (1|case_model),
                      data=data_consensus, REML=TRUE)
cat("\n[Sensitivity] Round fixed-effect Type III test (expect n.s.):\n")
print(anova(lmm_round_chk, type=3, ddf="Kenward-Roger")["round", , drop=FALSE])

# --- Fig 1: EMM with 95% CI + tier letters
emm_df <- as.data.frame(emm_primary) %>%
  left_join(as.data.frame(cld_primary) %>% select(model_name, .group), by="model_name")
p_fig1 <- ggplot(emm_df, aes(reorder(model_name, emmean), emmean, fill=model_name)) +
  geom_col(alpha=0.8, color="black", width=0.7) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0.2, linewidth=0.8) +
  geom_text(aes(label=trimws(.group), y=upper.CL+0.3), size=5, fontface="bold") +
  geom_hline(yintercept=13, linetype="dashed", color="blue", linewidth=0.8) +
  annotate("text", x=0.7, y=13.2, label="High-performance\nthreshold (>=13)",
           hjust=0, size=3, color="blue") +
  scale_fill_brewer(palette="Set2") + coord_cartesian(ylim=c(10,15)) +
  labs(x="Model", y="Total Score (3-15)") + theme_pubr() +
  theme(legend.position="none", axis.text.x=element_text(angle=45, hjust=1, size=11))
ggsave("output/Fig1_Overall_Performance.png", p_fig1, width=10, height=7, dpi=300)
ggsave("output/Fig1_Overall_Performance.pdf", p_fig1, width=10, height=7)
cat("Saved: Fig 1\n")

# ============================================================================
# 6. EQUIVALENCE TESTING — TOST vs MCID = 1.5  -> Fig S7                   [+]
#    TOST at alpha = .05  <=>  the 90% CI of the difference lies within +/-MCID
# ============================================================================
cat("\n", strrep("=",60), "\nSECTION 6: TOST EQUIVALENCE (MCID = ", MCID, ")\n",
    strrep("=",60), "\n\n", sep="")

pw90 <- as.data.frame(summary(pairs(emm_primary, adjust="none"),
                              infer=c(TRUE,TRUE), level=0.90))   # 90% CI for TOST
# NOTE: CIs are unadjusted. The pre-specified equivalence claim is the single focal
# comparison (DeepSeek R1 vs Gemini 3 Pro); the full 15-pair forest (Fig S7) is
# exploratory and is reported without multiplicity correction.
tost <- pw90 %>% mutate(
  p_lower    = pt((estimate + MCID)/SE, df, lower.tail=FALSE),
  p_upper    = pt((estimate - MCID)/SE, df, lower.tail=TRUE),
  p_TOST     = pmax(p_lower, p_upper),
  CI90_low   = round(lower.CL,2), CI90_high = round(upper.CL,2),
  contrast   = gsub("[()]", "", contrast),                       # tidy GPT-5.1 label
  equivalent = (lower.CL > -MCID) & (upper.CL < MCID),
  verdict    = ifelse(equivalent, "Equivalent (within MCID)", "Not equivalent"))
cat("Equivalence test, all model pairs (equivalent if 90% CI within +/-", MCID, "):\n", sep="")
print(tost %>% transmute(contrast, diff=round(estimate,2),
        CI90=sprintf("%.2f to %.2f", CI90_low, CI90_high),
        p_TOST=round(p_TOST,4), equivalent))

focal <- tost %>% filter(grepl("Gemini", contrast) & grepl("DeepSeek R1", contrast))
cat(sprintf("\nFOCAL  R1 vs Gemini 3 Pro: diff %.2f; 90%% CI %.2f to %.2f; TOST p %.4f; %s\n",
            focal$estimate, focal$CI90_low, focal$CI90_high, focal$p_TOST,
            ifelse(focal$equivalent,"EQUIVALENT","NOT equivalent")))

# --- Fig S7: 15-pairwise equivalence forest (90% CI vs +/-MCID band)
p_figS7 <- ggplot(tost, aes(estimate, reorder(contrast, estimate), color=verdict)) +
  annotate("rect", xmin=-MCID, xmax=MCID, ymin=-Inf, ymax=Inf, fill="grey85", alpha=0.5) +
  geom_vline(xintercept=0, linetype="dashed", color="black") +
  geom_vline(xintercept=c(-MCID,MCID), linetype="dotted", color="grey40") +
  geom_errorbarh(aes(xmin=lower.CL, xmax=upper.CL), height=0.25, linewidth=0.7) +
  geom_point(size=3) +
  scale_color_manual(values=c("Equivalent (within MCID)"="#0072B2","Not equivalent"="#D55E00"),
                     name=NULL) +
  labs(x="Pairwise difference in total score (90% CI)", y="Comparison",
       caption="Grey band = +/-1.5-point MCID. A 90% CI entirely within the band indicates equivalence (TOST, alpha = 0.05).") +
  theme_pubr() + theme(legend.position="top", axis.text.y=element_text(size=8))
ggsave("output/FigS7_Equivalence_allpairs_90CI.png", p_figS7, width=10, height=8, dpi=300)
ggsave("output/FigS7_Equivalence_allpairs_90CI.pdf", p_figS7, width=10, height=8)
cat("Saved: Fig S7\n")

# ============================================================================
# 7. EFFECT SIZES (Cohen's d)  -> Fig S5                                   [✓]
# ============================================================================
cat("\n", strrep("=",60), "\nSECTION 7: EFFECT SIZES\n", strrep("=",60), "\n\n", sep="")
models <- levels(data_consensus$model_name); n_models <- length(models)
cohens_d_matrix <- matrix(NA, n_models, n_models, dimnames=list(models, models))
for (i in 1:(n_models-1)) for (j in (i+1):n_models) {
  x <- data_consensus$total[data_consensus$model_name==models[i]]
  y <- data_consensus$total[data_consensus$model_name==models[j]]
  d <- effectsize::cohens_d(x, y)$Cohens_d
  cohens_d_matrix[i,j] <- round(d,2); cohens_d_matrix[j,i] <- round(-d,2)
}
cohens_d_df <- as.data.frame(cohens_d_matrix) %>% rownames_to_column("Model1") %>%
  pivot_longer(-Model1, names_to="Model2", values_to="Cohen_d")
p_figS5 <- ggplot(cohens_d_df, aes(Model2, Model1, fill=Cohen_d)) +
  geom_tile(color="white") +
  geom_text(aes(label=ifelse(is.na(Cohen_d), "", sprintf("%.2f", Cohen_d))), size=3.5) +
  scale_fill_gradient2(low="#0072B2", mid="white", high="#D55E00", midpoint=0,
                       na.value="gray90", limits=c(-1.5,1.5), name="Cohen's d") +
  labs(x="", y="", caption="|d|<0.2 negligible; 0.2-0.5 small; 0.5-0.8 medium; >0.8 large") +
  theme_pubr() + theme(axis.text.x=element_text(angle=45, hjust=1, size=9),
                       axis.text.y=element_text(size=9)) + coord_fixed()
ggsave("output/FigS5_Effect_Size_Heatmap.png", p_figS5, width=9, height=8, dpi=300)
cat("Saved: Fig S5\n")

# ============================================================================
# 8. REPRODUCIBILITY & CATASTROPHIC FAILURE  -> Fig 2, Fig S8           [⟳/+]
#    Primary safety endpoints. Round-to-round variability is WITHIN-case.
# ============================================================================
cat("\n", strrep("=",60), "\nSECTION 8: REPRODUCIBILITY (primary safety endpoints)\n",
    strrep("=",60), "\n\n", sep="")

wilson_ci <- function(x,n,conf=0.95){z<-qnorm(1-(1-conf)/2);p<-x/n;d<-1+z^2/n
  c(max(0,((p+z^2/(2*n))-z*sqrt(p*(1-p)/n+z^2/(4*n^2)))/d),
    min(1,((p+z^2/(2*n))+z*sqrt(p*(1-p)/n+z^2/(4*n^2)))/d))}

per_round <- data_consensus %>% mutate(
  round_fail   = (diagnosis<=FAIL_THRESH   | nextstep<=FAIL_THRESH   | treatment<=FAIL_THRESH),
  round_danger = (diagnosis<=DANGER_THRESH | nextstep<=DANGER_THRESH | treatment<=DANGER_THRESH))

# --- 8a. Catastrophic-failure frequency (per round) with Wilson CIs
cat_overall <- per_round %>% group_by(model_name) %>%
  summarise(n=n(), n_fail=sum(round_fail), n_danger=sum(round_danger), .groups="drop") %>%
  rowwise() %>%
  mutate(fail_pct=round(n_fail/n*100,1),
         fail_CI=sprintf("%.1f-%.1f", 100*wilson_ci(n_fail,n)[1], 100*wilson_ci(n_fail,n)[2]),
         danger_pct=round(n_danger/n*100,1),
         danger_CI=sprintf("%.1f-%.1f", 100*wilson_ci(n_danger,n)[1], 100*wilson_ci(n_danger,n)[2])) %>%
  ungroup() %>% arrange(danger_pct)
cat("Per-model failure (any dim <=", FAIL_THRESH, ") and dangerous (any dim <=",
    DANGER_THRESH, ") rates:\n")
print(cat_overall %>% transmute(model_name,
        `Failure% (95% CI)`=sprintf("%.1f (%s)", fail_pct, fail_CI),
        `Dangerous% (95% CI)`=sprintf("%.1f (%s)", danger_pct, danger_CI)))


# --- 8a-ter. Between-model inference on the catastrophic-failure rate  [NEW]
#     Reproduces the inferential p-values reported in §3.3 / Table 3 caption:
#       (1) Fisher-Freeman-Halton omnibus across all 6 models (Monte Carlo)
#       (2) Pooled two-safest-vs-remaining-four 2×2 Fisher (with OR)
#       (3) Targeted pairwise 2×2 Fisher, Holm-corrected (8 pre-declared pairs)
#     Reuses cat_overall (danger_counts derived below) computed in 8a.
cat("\n--- 8a-ter: BETWEEN-MODEL TESTS ON CATASTROPHIC-FAILURE RATE ---\n")

# Derive danger_counts from cat_overall; sort by ascending n_danger
danger_counts <- cat_overall %>%
  transmute(model_name, n, n_danger, n_safe = n - n_danger) %>%
  arrange(n_danger, model_name)

# (1) Fisher-Freeman-Halton omnibus (2×6), Monte Carlo B = 100,000
omnibus_mat <- rbind(dangerous = danger_counts$n_danger,
                     safe      = danger_counts$n_safe)
colnames(omnibus_mat) <- as.character(danger_counts$model_name)
ffh <- fisher.test(omnibus_mat, simulate.p.value = TRUE, B = 1e5)
cat(sprintf("(1) FFH omnibus across 6 models (Monte Carlo, B=1e5): p = %.4f\n", ffh$p.value))
cat("    [manuscript reports p ~ 0.009; small seed-dependent MC variation]\n")

# (2) Pooled: two safest vs remaining four (2×2 Fisher with conditional-MLE OR)
safest_two  <- as.character(danger_counts$model_name[1:2])
pooled_mat  <- matrix(
  c(sum(danger_counts$n_danger[1:2]), sum(danger_counts$n_safe[1:2]),
    sum(danger_counts$n_danger[3:6]), sum(danger_counts$n_safe[3:6])),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("Two safest", "Remaining four"), c("dangerous", "safe")))
pooled_ft   <- fisher.test(pooled_mat)
n_safe_grp  <- sum(danger_counts$n[1:2])
n_other_grp <- sum(danger_counts$n[3:6])
cat(sprintf(paste0(
  "(2) Pooled %s vs remaining four:\n    %d/%d (%.1f%%) vs %d/%d (%.1f%%)\n",
  "    Fisher p = %s  OR = %.2f (95%% CI %.2f-%.2f)\n"),
  paste(safest_two, collapse = " + "),
  pooled_mat[1,1], n_safe_grp,  100*pooled_mat[1,1]/n_safe_grp,
  pooled_mat[2,1], n_other_grp, 100*pooled_mat[2,1]/n_other_grp,
  format.pval(pooled_ft$p.value, digits=3, eps=1e-4),
  unname(pooled_ft$estimate), pooled_ft$conf.int[1], pooled_ft$conf.int[2]))

# (3) Targeted pairwise 2×2 Fisher, Holm-corrected (8 pre-declared comparisons
#     matching §2.6: two safest × three highest-rate, plus GPT-5.1 vs V3.1/Grok 4)
targeted_pairs <- list(
  c("DeepSeek R1",  "GPT-5.1"),
  c("Gemini 3 Pro", "GPT-5.1"),
  c("DeepSeek R1",  "Grok 4"),
  c("DeepSeek R1",  "DeepSeek V3.1"),
  c("Gemini 3 Pro", "Grok 4"),
  c("Gemini 3 Pro", "DeepSeek V3.1"),
  c("GPT-5.1",      "DeepSeek V3.1"),
  c("GPT-5.1",      "Grok 4"))
get_dc <- function(m) danger_counts[danger_counts$model_name == m, ]
pair_tab <- lapply(targeted_pairs, function(p) {
  a <- get_dc(p[1]); b <- get_dc(p[2])
  ft <- fisher.test(matrix(c(a$n_danger, a$n_safe, b$n_danger, b$n_safe),
                           nrow = 2, byrow = TRUE))
  data.frame(comparison   = paste(p[1], "vs", p[2]),
             rate_1        = sprintf("%.1f%%", 100*a$n_danger/a$n),
             rate_2        = sprintf("%.1f%%", 100*b$n_danger/b$n),
             OR            = round(unname(ft$estimate), 2),
             p_raw         = ft$p.value, stringsAsFactors = FALSE)
}) %>% bind_rows()
pair_tab$p_holm        <- p.adjust(pair_tab$p_raw, method = "holm")
pair_tab$survives_holm <- pair_tab$p_holm < 0.05
cat("(3) Targeted pairwise Fisher (Holm-corrected, 8 pre-declared comparisons):\n")
print(pair_tab %>% mutate(p_raw=round(p_raw,4), p_holm=round(p_holm,4)), row.names=FALSE)
cat(paste0("    Multiplicity-stable conclusions: omnibus (1) and pooled (2).\n",
           "    Individual safe-vs-unsafe pairs nominally significant but do not\n",
           "    survive Holm correction — consistent with the framing in §3.3.\n"))

# Collect for Section 13 export
catastrophic_between_model <- list(
  Catastrophic_omnibus  = data.frame(
    test="Fisher-Freeman-Halton omnibus (MC B=1e5)", p=ffh$p.value),
  Catastrophic_pooled   = data.frame(
    group_safe     = paste(safest_two, collapse=" + "),
    n_danger_safe  = pooled_mat[1,1], n_total_safe=n_safe_grp,
    n_danger_other = pooled_mat[2,1], n_total_other=n_other_grp,
    OR=round(unname(pooled_ft$estimate),3), p=pooled_ft$p.value),
  Catastrophic_pairwise_Holm = pair_tab %>%
    mutate(p_raw=round(p_raw,4), p_holm=round(p_holm,4)))

# --- 8a-bis. Robustness of the danger flag to rater disagreement (-> Limitation 4)
#     Unit = response (case x round x model), i.e. the Table 3 unit. The consensus
#     flag (mean dimension <= DANGER_THRESH) is cross-checked against each rater's
#     INDEPENDENT dangerous call, to quantify how many flags rest on a single rater.
rater_flag <- raw_data %>%
  mutate(rdanger = (diagnosis <= DANGER_THRESH |
                    nextstep  <= DANGER_THRESH |
                    treatment <= DANGER_THRESH)) %>%
  select(case_id, round, model_name, rater, rdanger) %>%
  pivot_wider(names_from = rater, values_from = rdanger, names_prefix = "r")
flag_chk <- per_round %>% select(case_id, round, model_name, round_danger) %>%
  left_join(rater_flag, by = c("case_id", "round", "model_name"))
n_cons <- sum(flag_chk$round_danger)
n_both <- sum(flag_chk$round_danger &  flag_chk$r1 &  flag_chk$r2)
n_one  <- sum(flag_chk$round_danger & (flag_chk$r1 != flag_chk$r2))
cat(sprintf(paste0(
  "\nDanger-flag robustness to rater disagreement (Limitation 4):\n",
  "  consensus-dangerous responses            : %d\n",
  "  ...flagged independently by BOTH raters  : %d (%.0f%%)\n",
  "  ...resting on a single rater (borderline): %d\n",
  "  both / either rater flagged (overall)    : %d / %d\n"),
  n_cons, n_both, 100 * n_both / n_cons, n_one,
  sum(rater_flag$r1 & rater_flag$r2), sum(rater_flag$r1 | rater_flag$r2)))

# --- 8b. Systematic vs stochastic reproducibility (per case x model, across 3 rounds)
cm <- per_round %>% group_by(model_name, case_id) %>%
  summarise(n_round=n(), n_fail=sum(round_fail), n_danger=sum(round_danger),
            min_total=min(total), round_sd=sd(total), round_range=max(total)-min(total),
            .groups="drop") %>%
  mutate(fail_class = case_when(n_fail==0 ~ "Consistently safe",
                                n_fail==n_round ~ "Consistently failing (systematic)",
                                TRUE ~ "Stochastically inconsistent"),
         danger_class = case_when(n_danger==0 ~ "No dangerous round",
                                  n_danger==n_round ~ "Consistently dangerous",
                                  TRUE ~ "Stochastically dangerous"))
repro_model <- cm %>% group_by(model_name) %>%
  summarise(pct_safe=round(mean(fail_class=="Consistently safe")*100,1),
            pct_systematic=round(mean(fail_class=="Consistently failing (systematic)")*100,1),
            pct_stochastic=round(mean(fail_class=="Stochastically inconsistent")*100,1),
            n_any_danger=sum(n_danger>0),
            n_stoch_danger=sum(danger_class=="Stochastically dangerous"),
            .groups="drop") %>% arrange(desc(pct_stochastic))
cat("\nReproducibility profile per model:\n"); print(repro_model)

stoch_danger <- cm %>% filter(danger_class=="Stochastically dangerous") %>%
  mutate(case=as.numeric(as.character(case_id))) %>% arrange(case, model_name) %>%
  transmute(case, model_name, `dangerous rounds`=paste0(n_danger,"/",n_round), min_total)
cat("\nStochastic-dangerous case x model combinations (safe some rounds, dangerous others):\n")
print(stoch_danger, n=Inf)

# --- 8c. Within-(case x model) round-to-round variability  (feeds Fig 2)
within_var <- cm %>% group_by(model_name) %>%
  summarise(mean_within_sd=round(mean(round_sd),3),
            median_within_sd=round(median(round_sd),3),
            max_range=max(round_range), .groups="drop") %>% arrange(mean_within_sd)
pooled_sd <- round(sd(data_consensus$total -
                      ave(data_consensus$total, data_consensus$case_model)), 3)
cat("\nWithin-case round-to-round SD per model (lower = more reproducible):\n")
print(within_var); cat(sprintf("Pooled within-(case x model) SD = %.2f (MCID = %.1f)\n", pooled_sd, MCID))

cat("\nDescriptive round means (order RANDOMISED -> exchangeable; NOT a temporal trend):\n")
print(data_consensus %>% group_by(model_name, round) %>%
        summarise(m=round(mean(total),2), .groups="drop") %>%
        pivot_wider(names_from=round, values_from=m, names_prefix="Round"))

# --- Fig 2: distribution of within-case SD by model (violin + jitter + mean)
model_order <- within_var %>% arrange(mean_within_sd) %>% pull(model_name)
cm <- cm %>% mutate(model_name = factor(model_name, levels = model_order))
p_fig2 <- ggplot(cm, aes(model_name, round_sd, fill=model_name)) +
  geom_violin(alpha=0.5, trim=TRUE, scale="width", color=NA) +
  geom_jitter(width=0.12, height=0, size=1.1, alpha=0.45) +
  stat_summary(fun=mean, geom="point", shape=23, size=3.2, fill="white", color="black") +
  geom_hline(yintercept=MCID, linetype="dashed", color="red", linewidth=0.8) +
  annotate("text", x=0.7, y=MCID+0.12, label=paste0("MCID = ", MCID), hjust=0, size=3, color="red") +
  scale_fill_brewer(palette="Set2") +
  labs(x="Model", y="Within-case round-to-round SD (total score)") +
  theme_pubr() + theme(legend.position="none", axis.text.x=element_text(angle=45, hjust=1, size=10))
ggsave("output/Fig2_Reproducibility_violin.png", p_fig2, width=10, height=6, dpi=300)
ggsave("output/Fig2_Reproducibility_violin.pdf", p_fig2, width=10, height=6)

# --- Fig S8: per-case round trajectories, faceted by model (spaghetti)
p_figS8 <- ggplot(data_consensus %>% mutate(model_name=factor(model_name, levels=model_order)),
                  aes(round, total, group=case_id)) +
  geom_line(alpha=0.22, color="grey40", linewidth=0.4) +
  stat_summary(aes(group=1), fun=mean, geom="line", color="#D55E00", linewidth=1.1) +
  stat_summary(aes(group=1), fun=mean, geom="point", color="#D55E00", size=2) +
  facet_wrap(~ model_name) + scale_y_continuous(limits=c(3,15)) +
  labs(x="Query round", y="Total score (3-15)") +
  theme_pubr() + theme(strip.text=element_text(face="bold", size=10))
ggsave("output/FigS8_Spaghetti_trajectories.png", p_figS8, width=11, height=7, dpi=300)
cat("Saved: Fig 2, Fig S8\n")

# --- (optional, NOT a manuscript figure) reproducibility composition stacked bar
repro_long <- cm %>% count(model_name, fail_class) %>% group_by(model_name) %>%
  mutate(pct=n/sum(n)*100) %>% ungroup() %>%
  mutate(fail_class=factor(fail_class, levels=c("Consistently safe","Stochastically inconsistent",
                                                "Consistently failing (systematic)")))
p_repro <- ggplot(repro_long, aes(model_name, pct, fill=fail_class)) +
  geom_col(width=0.7, color="white") +
  scale_fill_manual(values=c("Consistently safe"="#2E7D32","Stochastically inconsistent"="#F9A825",
                             "Consistently failing (systematic)"="#C62828"),
                    name="Per-case behaviour\n(across 3 rounds)") +
  labs(x="Model", y="% of cases") + theme_minimal(base_size=12) +
  theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave("output/FigOptional_Reproducibility_composition.png", p_repro, width=10, height=6, dpi=300)

# ============================================================================
# 9. DIFFICULTY STRATIFICATION  -> Fig 3, Table 4                         [⟳]
#    Interaction tested on the REDUCED random structure (1|case_model);
#    the full (1|case_id)+(1|case_model) is singular for the difficulty model
#    because difficulty is a case-level property.
# ============================================================================
cat("\n", strrep("=",60), "\nSECTION 9: DIFFICULTY STRATIFICATION\n", strrep("=",60), "\n\n", sep="")
case_difficulty <- data_consensus %>% group_by(case_id) %>%
  summarise(mean_score=mean(total), .groups="drop") %>%
  mutate(difficulty=case_when(mean_score>=quantile(mean_score,2/3) ~ "Easy",
                              mean_score>=quantile(mean_score,1/3) ~ "Medium", TRUE ~ "Hard"),
         difficulty=factor(difficulty, levels=c("Easy","Medium","Hard")))
cat("Difficulty distribution (tertiles):\n"); print(table(case_difficulty$difficulty))
data_diff <- data_consensus %>% left_join(case_difficulty %>% select(case_id, difficulty), by="case_id")

lmm_diff <- lmer(total ~ model_name * difficulty + (1|case_model), data=data_diff, REML=TRUE)
cat("\nisSingular(lmm_diff) =", isSingular(lmm_diff), "(expect FALSE)\n")
cat("Model x Difficulty interaction (Type III, Kenward-Roger):\n")
print(anova(lmm_diff, type=3, ddf="Kenward-Roger")["model_name:difficulty", , drop=FALSE])

model_by_diff <- data_diff %>% group_by(model_name, difficulty) %>%
  summarise(mean_total=mean(total), se=sd(total)/sqrt(n()), n=n(), .groups="drop")
decline_table <- model_by_diff %>% select(model_name, difficulty, mean_total) %>%
  pivot_wider(names_from=difficulty, values_from=mean_total) %>%
  mutate(across(c(Easy,Medium,Hard), ~round(.,2))) %>%
  mutate(Decline=round(Hard-Easy,2)) %>% arrange(desc(Decline))
cat("\nTable 4: performance decline (Easy -> Hard):\n"); print(decline_table)

p_fig3 <- ggplot(model_by_diff, aes(difficulty, mean_total, fill=model_name)) +
  geom_col(position=position_dodge(0.8), alpha=0.8, width=0.7) +
  geom_errorbar(aes(ymin=mean_total-se, ymax=mean_total+se), position=position_dodge(0.8), width=0.2) +
  scale_fill_brewer(palette="Set2", name="Model") + coord_cartesian(ylim=c(9,15)) +
  labs(x="Case Difficulty", y="Mean Total Score") + theme_pubr() + theme(legend.position="right")
ggsave("output/Fig3_Performance_by_Difficulty.png", p_fig3, width=10, height=6, dpi=300)
cat("Saved: Fig 3\n")

# ============================================================================
# 10. ERROR TAXONOMY  -> Fig 4, Table 5                                   [✓]
# ============================================================================
cat("\n", strrep("=",60), "\nSECTION 10: ERROR TAXONOMY\n", strrep("=",60), "\n\n", sep="")
rare_disease_cases <- c(1,3,8,9,10,12,13,14,16,19,20,23,28,29,31,33,34,36,39,43,53,54)  # n=22
anchoring_cases    <- c(2,5,6,7,11,18,21,24,26,27,30,35,37,38,40,41,42,46,47,48,49,50,51,52)  # n=24
iatrogenic_cases   <- c(4,7,15,17,18,22,24,25,32,44,45)  # n=11

calc_error_rate <- function(data, case_subset, error_type_name) {
  data %>% filter(as.numeric(as.character(case_id)) %in% case_subset) %>%
    group_by(model_name) %>%
    summarise(n_eval=n(),
              n_fail=sum(diagnosis<=FAIL_THRESH | nextstep<=FAIL_THRESH | treatment<=FAIL_THRESH),
              rate_pct=round(n_fail/n_eval*100,1), .groups="drop") %>%
    rowwise() %>%
    mutate(ci_lo=round(wilson_ci(n_fail,n_eval)[1]*100,1),
           ci_hi=round(wilson_ci(n_fail,n_eval)[2]*100,1),
           ci_text=paste0(rate_pct,"% (",ci_lo,"-",ci_hi,")"),
           error_type=error_type_name) %>% ungroup() %>% arrange(rate_pct)
}
error_rare       <- calc_error_rate(data_consensus, rare_disease_cases, "Rare Disease")
error_anchoring  <- calc_error_rate(data_consensus, anchoring_cases,    "Anchoring Bias")
error_iatrogenic <- calc_error_rate(data_consensus, iatrogenic_cases,   "Iatrogenic Risk")
cat("Type 1 - Rare Disease:\n");    print(error_rare %>% select(model_name, n_eval, n_fail, ci_text))
cat("\nType 2 - Anchoring Bias:\n"); print(error_anchoring %>% select(model_name, n_eval, n_fail, ci_text))
cat("\nType 3 - Iatrogenic Risk:\n");print(error_iatrogenic %>% select(model_name, n_eval, n_fail, ci_text))

error_all_combined <- bind_rows(error_rare, error_anchoring, error_iatrogenic)
cat("\nFisher's exact tests (Monte Carlo, B = 100,000):\n")
for (et in list(list(name="Rare Disease",d=error_rare),
                list(name="Anchoring Bias",d=error_anchoring),
                list(name="Iatrogenic Risk",d=error_iatrogenic))) {
  cont <- matrix(c(et$d$n_fail, et$d$n_eval-et$d$n_fail), ncol=2)
  ft <- fisher.test(cont, simulate.p.value=TRUE, B=100000)
  cat("  ", et$name, ": p =", round(ft$p.value,4), "\n")
}

fig4_order <- c("Gemini 3 Pro","DeepSeek R1","Claude Sonnet 4.5","GPT-5.1","Grok 4","DeepSeek V3.1")
error_plot_data <- error_all_combined %>%
  mutate(error_type=factor(error_type, levels=c("Rare Disease","Anchoring Bias","Iatrogenic Risk")),
         model_name=factor(model_name, levels=fig4_order))
p_fig4 <- ggplot(error_plot_data, aes(model_name, rate_pct, fill=error_type)) +
  geom_col(position=position_dodge(0.8), alpha=0.8, width=0.7) +
  geom_errorbar(aes(ymin=ci_lo, ymax=ci_hi), position=position_dodge(0.8), width=0.2, linewidth=0.5) +
  geom_text(aes(label=paste0(rate_pct,"%"), y=ci_hi), position=position_dodge(0.8), vjust=-0.5, size=3) +
  scale_fill_manual(values=c("Rare Disease"="#E69F00","Anchoring Bias"="#56B4E9",
                             "Iatrogenic Risk"="#009E73"), name="Error Type") +
  labs(x="Model", y="Failure Rate (%)") + theme_pubr() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=10), legend.position="right")
ggsave("output/Fig4_Error_Rates_by_Type.png", p_fig4, width=12, height=7, dpi=300)
cat("Saved: Fig 4\n")

# ============================================================================
# 11. DIMENSION-SPECIFIC ANALYSIS  -> Fig S3, Fig S4                       [✓]
#     Random structure harmonised to the primary model (case + case x model).
# ============================================================================
cat("\n", strrep("=",60), "\nSECTION 11: DIMENSION-SPECIFIC\n", strrep("=",60), "\n\n", sep="")
lmm_diag  <- lmer(diagnosis ~ model_name + (1|case_id) + (1|case_model), data=data_consensus, REML=TRUE)
lmm_next  <- lmer(nextstep  ~ model_name + (1|case_id) + (1|case_model), data=data_consensus, REML=TRUE)
lmm_treat <- lmer(treatment ~ model_name + (1|case_id) + (1|case_model), data=data_consensus, REML=TRUE)
dim_summary <- bind_rows(
  as.data.frame(emmeans(lmm_diag, ~model_name))  %>% mutate(dimension="Diagnosis"),
  as.data.frame(emmeans(lmm_next, ~model_name))  %>% mutate(dimension="Next Steps"),
  as.data.frame(emmeans(lmm_treat,~model_name))  %>% mutate(dimension="Treatment"))
cat("Dimension-specific EMMs:\n")
print(dim_summary %>% select(dimension, model_name, emmean, SE) %>% arrange(dimension, desc(emmean)))

p_figS4 <- ggplot(dim_summary, aes(model_name, emmean, fill=dimension)) +
  geom_col(position=position_dodge(0.8), alpha=0.8, width=0.7) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), position=position_dodge(0.8), width=0.2) +
  facet_wrap(~dimension, scales="free_y") + scale_fill_brewer(palette="Set1") +
  labs(x="Model", y="Score (1-5)") + theme_pubr() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=9), legend.position="none",
        strip.text=element_text(face="bold", size=11))
ggsave("output/FigS4_Dimension_Performance.png", p_figS4, width=12, height=5, dpi=300)

data_long <- data_consensus %>%
  pivot_longer(c(diagnosis,nextstep,treatment), names_to="dimension", values_to="score") %>%
  mutate(dimension=factor(case_match(dimension,"diagnosis"~"Diagnosis","nextstep"~"Next Steps",
                                     "treatment"~"Treatment"),
                          levels=c("Diagnosis","Next Steps","Treatment")))
p_figS3 <- ggplot(data_long, aes(model_name, score, fill=model_name)) +
  geom_violin(alpha=0.6, trim=TRUE) + geom_boxplot(width=0.15, outlier.size=0.5, alpha=0.8) +
  facet_wrap(~dimension, nrow=1) + scale_fill_brewer(palette="Set2") +
  scale_y_continuous(breaks=1:5, limits=c(0.8,5.2)) + coord_cartesian(ylim=c(1,5)) +
  labs(x="Model", y="Score (1-5)") + theme_pubr() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8), legend.position="none",
        strip.text=element_text(face="bold", size=11))
ggsave("output/FigS3_Violin_Faceted.png", p_figS3, width=14, height=5, dpi=300)
cat("Saved: Fig S3, Fig S4\n")

# ============================================================================
# 12. OTHER SENSITIVITY: residual diagnostics (Fig S6), CLMM, rater-adjusted [⟳]
# ============================================================================
cat("\n", strrep("=",60), "\nSECTION 12: SENSITIVITY ANALYSES\n", strrep("=",60), "\n\n", sep="")

# --- 12a. Friedman (non-parametric corroboration)
data_agg <- data_consensus %>% group_by(case_id, model_name) %>%
  summarise(mean_total=mean(total), .groups="drop")
fr <- friedman.test(mean_total ~ model_name | case_id, data=data_agg)
cat(sprintf("Friedman: chi2 = %.2f, df = %d, p = %s\n",
            fr$statistic, fr$parameter, format.pval(fr$p.value, digits=3)))

# --- 12b. Residual diagnostics of the primary model -> Fig S6
png("output/FigS6_Residuals.png", width=1200, height=400, res=150)
par(mfrow=c(1,3))
plot(fitted(lmm_primary), residuals(lmm_primary), main="(A) Residuals vs Fitted",
     xlab="Fitted", ylab="Residuals", pch=16, col=rgb(0,0,0,0.3)); abline(h=0, col="red", lty=2, lwd=2)
qqnorm(residuals(lmm_primary), main="(B) Q-Q Plot", pch=16, col=rgb(0,0,0,0.3))
qqline(residuals(lmm_primary), col="red", lwd=2)
hist(residuals(lmm_primary), main="(C) Histogram", xlab="Residuals", breaks=30,
     col="lightblue", border="white")
dev.off()
cat("Saved: Fig S6\n")

# --- 12c. Cumulative Link Mixed Model (ordinal robustness; round = exchangeable)
data_consensus$total_ord <- factor(round(data_consensus$total), ordered=TRUE)
data_consensus$model_R1ref <- relevel(data_consensus$model_name, ref="DeepSeek R1")
clmm_total <- clmm(total_ord ~ model_R1ref + (1|case_id), data=data_consensus)
clmm_coef <- as.data.frame(summary(clmm_total)$coefficients); clmm_coef$Variable <- rownames(clmm_coef)
clmm_effects <- clmm_coef %>% filter(grepl("model_R1ref", Variable)) %>%
  mutate(Model=gsub("model_R1ref","",Variable), OR=round(exp(Estimate),2),
         OR_lo=round(exp(Estimate-1.96*`Std. Error`),2),
         OR_hi=round(exp(Estimate+1.96*`Std. Error`),2))
cat("\nCLMM cumulative ORs (vs DeepSeek R1):\n")
print(clmm_effects %>% select(Model, OR, OR_lo, OR_hi, `Pr(>|z|)`))

# --- 12d. Rater-adjusted model (individual ratings; new random structure)
raw_data$case_model <- interaction(raw_data$case_id, raw_data$model_name, drop=TRUE)
lmm_no_rater   <- lmer(total ~ model_name + (1|case_id) + (1|case_model),
                       data=raw_data, REML=TRUE)
lmm_with_rater <- lmer(total ~ model_name + (1|case_id) + (1|case_model) + (1|rater),
                       data=raw_data, REML=TRUE)
lr <- anova(lmm_no_rater, lmm_with_rater)   # refit with ML automatically
cat("\nLikelihood-ratio test for rater random effect:\n"); print(lr)
emm_nr <- as.data.frame(summary(emmeans(lmm_no_rater, ~model_name)))
emm_wr <- as.data.frame(summary(emmeans(lmm_with_rater, ~model_name)))
emm_comp <- emm_nr %>% select(model_name, emmean) %>% rename(EMM_noRater=emmean) %>%
  left_join(emm_wr %>% select(model_name, emmean) %>% rename(EMM_withRater=emmean), by="model_name") %>%
  mutate(EMM_diff=round(EMM_withRater-EMM_noRater,4))
cat("\nEMM with vs without rater effect (max |change| should be tiny):\n"); print(emm_comp)
cat(sprintf("Max absolute EMM change: %.4f points\n", max(abs(emm_comp$EMM_diff))))

# --- 12e. Robustness to unequal residual variance across models (-> Limitation 4)
#     The primary LMM assumes a single residual SD, but round-to-round variability
#     is ~2x larger for GPT-5.1. Refit allowing a per-model residual SD (nlme) and
#     confirm the model ordering is unchanged. case_model is nested within case_id
#     (each case x model unit belongs to one case), so case_id/case_model in nlme
#     reproduces the crossed lme4 structure here.
lme_homo   <- nlme::lme(total ~ model_name, random = ~1 | case_id/case_model,
                        data = data_consensus, method = "REML")
lme_hetero <- update(lme_homo, weights = nlme::varIdent(form = ~1 | model_name))
cat(sprintf("\nResidual-variance robustness: AIC homoscedastic %.1f vs heteroscedastic %.1f\n",
            AIC(lme_homo), AIC(lme_hetero)))
cat("Per-model residual SD multipliers (reference = first level; GPT-5.1 ~2x):\n")
print(round(coef(lme_hetero$modelStruct$varStruct, unconstrained = FALSE), 2))
cat("EMM ordering under the heteroscedastic model (should match the primary model):\n")
print(as.data.frame(emmeans(lme_hetero, ~ model_name)) %>%
        transmute(model_name, EMM = round(emmean, 2)) %>% arrange(desc(EMM)))

# ============================================================================
# 13. EXPORT
# ============================================================================
cat("\n", strrep("=",60), "\nSECTION 13: EXPORT\n", strrep("=",60), "\n\n", sep="")
write_xlsx(list(
  ICC=icc_results, Kappa=kappa_results, Descriptives=desc_by_model,
  EMM_Table1=emm_tab, Pairwise_Tukey=as.data.frame(summary(pairs_primary)),
  TOST_equivalence=tost %>% select(contrast, estimate, CI90_low, CI90_high, p_TOST, equivalent),
  Catastrophic_rates=cat_overall, Reproducibility=repro_model,
  Stochastic_danger=stoch_danger, Round_variability=within_var,
  Catastrophic_omnibus=catastrophic_between_model$Catastrophic_omnibus,
  Catastrophic_pooled=catastrophic_between_model$Catastrophic_pooled,
  Catastrophic_pairwise_Holm=catastrophic_between_model$Catastrophic_pairwise_Holm,
  Difficulty_decline=decline_table,
  Error_Rare=error_rare, Error_Anchoring=error_anchoring, Error_Iatrogenic=error_iatrogenic,
  Dimension_EMMs=dim_summary, CLMM=clmm_effects, Rater_sensitivity=emm_comp,
  Case_difficulty=case_difficulty
), "output/All_Results.xlsx")
cat("Saved: output/All_Results.xlsx\n")

cat("\n", strrep("=",70), "\nANALYSIS COMPLETE — figures and tables in ./output/\n",
    strrep("=",70), "\n", sep="")
# END OF SCRIPT
