# ============================================================
# calibration.R
# Standalone calibration model — driven by actual dataset
# ============================================================

library(dplyr)

# ── Load dataset ─────────────────────────────────────────────
movies <- read.csv("data/cleaned_movies.csv")

# ── Shared assumption ────────────────────────────────────────
# Mario 1 earned $517M of its $574.93M total by day 30 = 89.9%
# Source: Box Office Mojo weekly data
mario1_total   <- 574.93
mario1_opening <- 204.60   # 5-day opening, Box Office Mojo
mario1_day30   <- 517.00   # earned by day 30, Box Office Mojo

day30_fraction <- mario1_day30 / mario1_total   # = 0.899

# ============================================================
# ESTIMATE A: Regression Model Output
# ============================================================
# From log-log sequel regression: log(DomesticTotal) ~ log(PrevFilmDomestic) + Year
# Raw output in dollars → divide by 1e6 for millions
# Source: regression.R (friend's model)

model_point <- 668164266 / 1e6
model_lower <- 265605000 / 1e6
model_upper <- 1680854977 / 1e6

est_A_30day <- model_point * day30_fraction

cat("=== Estimate A: Regression Model ===\n")
cat("Point estimate (total):  $", round(model_point, 1), "M\n")
cat("95% PI: $", round(model_lower, 1), "M  to  $", round(model_upper, 1), "M\n")
cat("30-day estimate:         $", round(est_A_30day, 1), "M\n\n")

# ============================================================
# ESTIMATE B: Opening Weekend Multiplier Method
# ============================================================
# Step 1: Compute historical multiplier (total / prev) from dataset
movies <- movies %>%
  mutate(SequelRatio   = DomesticTotal / PrevFilmDomestic,
         RunMultiplier = DomesticTotal / PrevFilmDomestic)

# Step 2: Mario 1 specific multiplier (total domestic / 5-day opening)
# Used directly since we know Mario 1's exact numbers
run_multiplier <- mario1_total / mario1_opening   # = 2.814x

# Step 3: Opening estimate from industry tracking
#   Deadline (Mar 2026): $160M+ 5-day, described as "conservative"
#   Box Office Pro: $160-175M 5-day forecast
#   Polymarket: 65% probability of $200M+ opening weekend
#   → Midpoint estimate: $170M
opening_5day <- 170

est_B_total <- opening_5day * run_multiplier
est_B_30day <- est_B_total * day30_fraction

cat("=== Estimate B: Multiplier Method ===\n")
cat("Mario 1 run multiplier (total/5-day): ", round(run_multiplier, 3), "x\n")
cat("5-day opening estimate:          $", opening_5day, "M\n")
cat("Estimated total domestic:        $", round(est_B_total, 1), "M\n")
cat("30-day estimate:                 $", round(est_B_30day, 1), "M\n\n")

# ============================================================
# ESTIMATE C: Sequel Ratio Method — computed from dataset
# ============================================================
# Step 1: Compute sequel ratio for all 27 films
movies <- movies %>%
  mutate(SequelRatio = DomesticTotal / PrevFilmDomestic)

# Step 2: Overall average ratio across all 27 films
overall_avg_ratio <- mean(movies$SequelRatio)

# Step 3: Illumination-only films from dataset
illumination_films <- c(
  "Despicable Me 2", "Despicable Me 3", "Despicable Me 4",
  "Minions: The Rise of Gru", "Secret Life of Pets 2", "Sing 2"
)
illumination_avg_ratio <- movies %>%
  filter(Movie %in% illumination_films) %>%
  summarise(avg = mean(SequelRatio)) %>%
  pull(avg)

# Step 4: Video game sequel ratio from dataset
videogame_avg_ratio <- movies %>%
  filter(IsVideoGame == 1) %>%
  summarise(avg = mean(SequelRatio)) %>%
  pull(avg)

# Step 5: Large predecessor films (PrevFilmDomestic > 300M) — most comparable to Mario
large_pred_ratio <- movies %>%
  filter(PrevFilmDomestic > 300) %>%
  summarise(avg = mean(SequelRatio)) %>%
  pull(avg)

cat("=== Sequel Ratio Analysis (from dataset) ===\n")
cat("Overall avg ratio (all 27 films):         ", round(overall_avg_ratio,    3), "x\n")
cat("Illumination avg ratio (6 films):         ", round(illumination_avg_ratio,3), "x\n")
cat("Video game sequel avg ratio (2 films):    ", round(videogame_avg_ratio,   3), "x\n")
cat("Large predecessor avg ratio (>$300M prev):", round(large_pred_ratio,      3), "x\n\n")

# Step 6: Applied ratio
# Mario Galaxy is Illumination + video game + large predecessor
# Weight: 50% large predecessor ratio, 30% Illumination, 20% overall
sequel_ratio <- (large_pred_ratio * 0.50) +
  (illumination_avg_ratio * 0.30) +
  (overall_avg_ratio * 0.20)

prev_domestic <- mario1_total   # = 574.93M

est_C_total <- prev_domestic * sequel_ratio
est_C_30day <- est_C_total * day30_fraction

cat("=== Estimate C: Sequel Ratio Method ===\n")
cat("Applied weighted ratio:          ", round(sequel_ratio,  3), "x\n")
cat("Mario 1 domestic:               $", prev_domestic, "M\n")
cat("Estimated total domestic:       $", round(est_C_total, 1), "M\n")
cat("30-day estimate:                $", round(est_C_30day, 1), "M\n\n")

# ============================================================
# TRIANGULATION: Weighted Average
# ============================================================
# B = 0.45 — freshest real-world tracking data
# C = 0.35 — franchise-specific, computed from actual dataset
# A = 0.20 — regression model output

weighted_prediction <- (est_A_30day * 0.20) +
  (est_B_30day * 0.45) +
  (est_C_30day * 0.35)

final_submission <- round(weighted_prediction / 5) * 5

lo <- min(est_A_30day, est_B_30day, est_C_30day)
hi <- max(est_A_30day, est_B_30day, est_C_30day)

cat("=== TRIANGULATION SUMMARY ===\n")
cat("Estimate A (regression):   $", round(est_A_30day,        1), "M  [weight: 20%]\n")
cat("Estimate B (multiplier):   $", round(est_B_30day,        1), "M  [weight: 45%]\n")
cat("Estimate C (sequel ratio): $", round(est_C_30day,        1), "M  [weight: 35%]\n")
cat("-----------------------------------\n")
cat("Weighted average:          $", round(weighted_prediction, 1), "M\n")
cat("Range:                     $", round(lo, 1), "M  to  $", round(hi, 1), "M\n")
cat("Convergence spread:        $", round(hi - lo,             1), "M\n")
cat("===================================\n")
cat("FINAL SUBMISSION:          $", final_submission, "M\n\n")

# ── Summary Table ─────────────────────────────────────────────
calibration_summary <- data.frame(
  Method    = c("A: Regression Model", "B: Multiplier Method", "C: Sequel Ratio"),
  Total_Est = c(round(model_point,  1), round(est_B_total, 1), round(est_C_total, 1)),
  Est_30day = c(round(est_A_30day,  1), round(est_B_30day, 1), round(est_C_30day, 1)),
  Weight    = c("20%", "45%", "35%")
)

print(calibration_summary)

cat("\nFinal 30-day domestic prediction: $", final_submission, "M\n")
cat("95% PI from regression (30-day):  $",
    round(model_lower * day30_fraction, 1), "M  to  $",
    round(model_upper * day30_fraction, 1), "M\n")