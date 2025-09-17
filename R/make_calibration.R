make_calibration <- function(model_path = "data/analysis/model_platt.rds") {
  dir.create(dirname(model_path), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(model_path)) {
    model <- list(type = "identity", coef = c(0,1), created = Sys.time())
    saveRDS(model, model_path)
    message("Wrote identity calibration model at ", model_path)
  } else {
    message("Calibration model already exists at ", model_path)
  }
}
