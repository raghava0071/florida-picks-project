# Paired bootstrap uplift (uniform − model) in log-loss
evaluate_uplift <- function(
  backtest_csv = "data/analysis/backtest_draws_freq.csv",
  out_txt      = "outputs/uplift_summary.txt",
  out_csv      = "data/analysis/uplift_bootstrap.csv",
  n_boot       = 2000,
  seed         = 123
) {
  library(dplyr); library(readr); library(purrr); library(tidyr)
  stopifnot(file.exists(backtest_csv))
  df <- readr::read_csv(backtest_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"hit" %in% names(df)) stop("Backtest needs a hit column {0,1}.")
  if (!any(c("draw_id","draw_date") %in% names(df))) df$draw_id <- dplyr::row_number()
  else if (!"draw_id" %in% names(df)) df$draw_id <- dplyr::coalesce(df$draw_id, df$draw_date)

  pcol <- dplyr::case_when(
    "prob" %in% names(df) ~ "prob",
    "p_raw" %in% names(df) ~ "p_raw",
    "p_model" %in% names(df) ~ "p_model",
    TRUE ~ NA_character_
  )
  if (is.na(pcol)) stop("No probability column (prob/p_raw/p_model) in backtest.")

  win <- df |> filter(hit == 1)
  if (nrow(win) == 0) stop("No winning rows (hit==1).")
  eps <- 1e-12; K <- 100
  win <- win |>
    mutate(
      p_model = pmax(.data[[pcol]], eps),
      ll_model = -log(p_model),
      ll_uniform = -log(1/K)
    ) |>
    group_by(draw_id) |> summarize(ll_model = first(ll_model), ll_uniform = first(ll_uniform), .groups="drop")

  gap_obs <- mean(win$ll_uniform - win$ll_model)
  set.seed(seed)
  gaps <- replicate(n_boot, {
    idx <- sample.int(n = nrow(win), replace = TRUE)
    mean(win$ll_uniform[idx] - win$ll_model[idx])
  })
  tibble(gap = gaps) |> write_csv(out_csv)
  ci <- quantile(gaps, c(0.025, 0.975), na.rm = TRUE)
  sig <- (ci[1] > 0) || (ci[2] < 0)
  msg <- paste0(
    "Uplift (uniform − model) log-loss (n draws=", nrow(win), ")\n",
    sprintf("Observed: %.6f\n", gap_obs),
    sprintf("95%% CI: [%.6f, %.6f]\n", ci[1], ci[2]),
    sprintf("Significant?: %s\n", ifelse(sig, "YES", "NO"))
  )
  cat(msg, file = out_txt)
  message("Wrote uplift summary to ", out_txt)
  invisible(list(obs = gap_obs, ci = ci, significant = sig))
}
