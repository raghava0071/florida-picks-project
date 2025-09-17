# Reliability / calibration curve with 95% CIs
make_reliability_plot <- function(
  calib_csv = "data/analysis/calibration.csv",
  out_png   = "outputs/reliability_plot.png",
  bins      = 15
) {
  library(dplyr); library(readr); library(ggplot2)
  stopifnot(file.exists(calib_csv))
  df <- readr::read_csv(calib_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"y" %in% names(df) && "hit" %in% names(df)) df$y <- df$hit
  if (!"p_raw" %in% names(df) && "p" %in% names(df)) df$p_raw <- df$p
  if (!all(c("p_raw","y") %in% names(df))) stop("calibration.csv must have p_raw and y (or hit).")

  df <- df |> mutate(bin = cut(p_raw, breaks = seq(0,1,length.out = bins+1), include.lowest = TRUE))
  agg <- df |> group_by(bin) |>
    summarize(p_hat = mean(p_raw), y_bar = mean(y), n = dplyr::n(), .groups="drop") |>
    mutate(
      z = 1.96, denom = 1 + z^2/n,
      center = (y_bar + z^2/(2*n))/denom,
      halfw  = (z * sqrt((y_bar*(1-y_bar)/n) + (z^2/(4*n^2))))/denom,
      lo = pmax(0, center - halfw),
      hi = pmin(1, center + halfw)
    )

  p <- ggplot(agg, aes(p_hat, y_bar)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15) +
    geom_point() +
    labs(title = "Reliability Plot (95% CIs)",
         x = "Predicted probability (bin mean)",
         y = "Empirical hit rate") +
    theme_minimal()
  ggsave(out_png, p, width = 6.5, height = 5, dpi = 150)
  message("Saved reliability plot: ", out_png)
}
