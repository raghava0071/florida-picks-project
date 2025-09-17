source("R/load_tuned_weights.R")
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(tidyr)
})

dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs",    recursive = TRUE, showWarnings = FALSE)
dir.create("data/analysis", recursive = TRUE, showWarnings = FALSE)

# 1) Parse PDF -> CSV
if (!file.exists("R/parse_pick2.R")) stop("Missing R/parse_pick2.R")
source("R/parse_pick2.R")
pick2 <- parse_pick2()
message("Parsed rows: ", nrow(pick2))
readr::write_csv(pick2, "data/clean/pick2_history.csv")
message("Wrote data/clean/pick2_history.csv")

# 2) Classic independence predictions
if (file.exists("R/predict_next.R")) {
  source("R/predict_next.R")  # writes outputs/predictions_next_draw*.csv
} else {
  message("Skip: R/predict_next.R not found")
}

# 3) Optional: weekday/blend predictions (only if you have these files)
if (file.exists("R/predict_with_blend.R")) {
  source("R/predict_with_blend.R")  # writes outputs/*_blend.csv
} else if (file.exists("R/predict_weekday_blend.R")) {
  source("R/predict_weekday_blend.R")
} else {
  message("Skip: no blend/weekday predictor script found")
}

# ==== Tuned weights loader (paste above predict_backoff_blend call) ====
alpha_weekday_use <- 0.40
alpha_markov_use  <- 0.35

if (file.exists("data/analysis/weights_backoff.yml")) {
  if (!requireNamespace("yaml", quietly = TRUE)) install.packages("yaml")
  w <- yaml::read_yaml("data/analysis/weights_backoff.yml")
  if (!is.null(w$alpha_weekday)) alpha_weekday_use <- as.numeric(w$alpha_weekday)
  if (!is.null(w$alpha_markov))  alpha_markov_use  <- as.numeric(w$alpha_markov)
}
message(sprintf("Using weights: alpha_weekday=%.2f, alpha_markov=%.2f",
                alpha_weekday_use, alpha_markov_use))
# ======================================================================


# 4) Optional: calibration (only if present)
if (file.exists("R/make_calibration.R"))        source("R/make_calibration.R")
if (file.exists("R/calibrate_probs.R"))         source("R/calibrate_probs.R")
if (file.exists("R/apply_calibration_to_next.R")) source("R/apply_calibration_to_next.R")

# 5) Optional: pattern visuals
if (file.exists("R/analyze_patterns.R")) {
  source("R/analyze_patterns.R")
} else {
  message("Skip: R/analyze_patterns.R not found")
}

message("Updater finished OK.")

if (file.exists("R/apply_calibration_to_next.R")) {
  source("R/apply_calibration_to_next.R")
  if (exists("apply_calibration_to_next")) apply_calibration_to_next()
}
