
# R/load_tuned_weights.R
alpha_weekday_use <- 0.40
alpha_markov_use  <- 0.35

if (file.exists("data/analysis/weights_backoff.yml")) {
  if (!requireNamespace("yaml", quietly = TRUE)) install.packages("yaml", quiet = TRUE)
  w <- yaml::read_yaml("data/analysis/weights_backoff.yml")
  if (!is.null(w$alpha_weekday)) alpha_weekday_use <- as.numeric(w$alpha_weekday)
  if (!is.null(w$alpha_markov))  alpha_markov_use  <- as.numeric(w$alpha_markov)
}

message(sprintf("Using weights: alpha_weekday=%.2f, alpha_markov=%.2f",
                alpha_weekday_use, alpha_markov_use))

