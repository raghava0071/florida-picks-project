
make_reliability_and_uplift <- function(bt_path = NULL, out_png = "outputs/reliability_plot.png",
                                        out_txt = "outputs/uplift_summary.txt", bins = 10, B = 1000) {
  suppressPackageStartupMessages({library(readr); library(dplyr); library(ggplot2)})
  # choose a backtest file
  if (is.null(bt_path)) {
    cands <- c("data/analysis/backtest_by_date_freq.csv",
               "data/analysis/backtest_by_date.csv")
    bt_path <- cands[file.exists(cands)][1]
  }
  if (is.na(bt_path)) stop("No backtest file found.")
  bt <- readr::read_csv(bt_path, show_col_types = FALSE)
  names(bt) <- tolower(names(bt))

  # expect columns: prob, hit (1/0). If hit missing, bail.
  stopifnot(all(c("prob","hit") %in% names(bt)))

  # calibration bins
  bt <- bt |> mutate(bin = ntile(prob, bins))
  calib <- bt |>
    group_by(bin) |>
    summarize(p_hat = mean(prob), emp = mean(hit), n = dplyr::n(), .groups = "drop")

  ggsave(out_png,
    ggplot(calib, aes(p_hat, emp)) +
      geom_abline(linetype = 2) +
      geom_point() + geom_line() +
      labs(title = "Reliability (Calibration) Curve",
           x = "Predicted probability (bin avg)",
           y = "Empirical hit rate (bin avg)"),
    width = 6, height = 4, dpi = 150)

  # log-loss vs uniform with bootstrap CIs
  eps <- 1e-15
  ll_model <- function(df) -mean(df$hit * log(pmax(df$prob, eps)) + (1 - df$hit) * log(pmax(1 - df$prob, eps)))
  ll_uniform <- function(df) -mean(df$hit * log(0.01) + (1 - df$hit) * log(0.99))

  base_ll <- ll_model(bt)
  base_uni <- ll_uniform(bt)
  uplift <- base_uni - base_ll

  set.seed(123)
  n <- nrow(bt)
  boots <- replicate(B, {
    idx <- sample.int(n, n, replace = TRUE)
    dfb <- bt[idx, ]
    ll_uniform(dfb) - ll_model(dfb)
  })
  ci <- quantile(boots, c(0.025, 0.975), na.rm = TRUE)

  lines <- c(
    sprintf("Backtest file: %s", bt_path),
    sprintf("Model log-loss: %.4f", base_ll),
    sprintf("Uniform log-loss: %.4f", base_uni),
    sprintf("Uplift (Uniform - Model): %.4f", uplift),
    sprintf("Bootstrap 95%% CI: [%.4f, %.4f]", ci[[1]], ci[[2]])
  )
  writeLines(lines, out_txt)
  message("Wrote: ", out_png, " and ", out_txt)
}

