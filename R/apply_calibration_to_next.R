suppressPackageStartupMessages({ library(readr); library(dplyr) })

apply_calibration_to_next <- function(
  model_path = "data/analysis/model_platt.rds",
  in_pairs   = "outputs/top25_pairs_next_draw.csv",
  out_pairs  = "outputs/top25_pairs_next_draw_cal.csv"
) {
  if (!file.exists(model_path)) {
    dir.create(dirname(model_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(list(type="identity", coef=c(0,1), created=Sys.time()), model_path)
    message("No model found; wrote identity model to ", model_path)
  }
  model <- readRDS(model_path)

  cal_fun <- switch(
    model$type,
    "platt"    = function(p) plogis(model$coef[1] + model$coef[2]*qlogis(p)),
    "identity" = function(p) p,
    function(p) p
  )

  if (file.exists(in_pairs)) {
    df <- readr::read_csv(in_pairs, show_col_types = FALSE)
    if (!"p" %in% names(df)) {
      message("No 'p' column in ", in_pairs, "; skipping calibration.")
      return(invisible(NULL))
    }
    df <- df %>% mutate(p_cal = cal_fun(p))
    readr::write_csv(df, out_pairs)
    message("Wrote ", out_pairs)
  } else {
    message("Input predictions file not found: ", in_pairs)
  }
}
