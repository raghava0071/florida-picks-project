
make_playlists <- function(q = 0.10, k = 10) {
  suppressPackageStartupMessages({library(readr); library(dplyr); library(tidyr)})

  # 1) Load FDR-approved pairs
  zs_path <- "data/analysis/pair_zscan.csv"
  stopifnot(file.exists(zs_path))
  zs <- readr::read_csv(zs_path, show_col_types = FALSE) |>
    mutate(p_fdr = p.adjust(p_two_sided, method = "BH")) |>
    filter(p_fdr <= q) |>
    select(pair, n, z, p_two_sided, p_fdr)

  if (nrow(zs) == 0) {
    message("No pairs pass FDR at q=", q, ". Writing empty playlists.")
    for (nm in c("playlist_M.csv","playlist_E.csv","playlist_all.csv")) {
      write_csv(tibble(pair = character(), prob = numeric()), file.path("outputs", nm))
    }
    return(invisible(zs))
  }

  # 2) Candidate predictions (use what exists)
  candidates <- c(
    "outputs/predictions_backoff_blend_M.csv",
    "outputs/predictions_backoff_blend_E.csv",
    "outputs/predictions_weekday_blend_M.csv",
    "outputs/predictions_weekday_blend_E.csv",
    "outputs/predictions_next_draw.csv"  # fallback
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) stop("No predictions CSVs found in outputs/. Run update_all.R first.")

  # helper: load/tidy any predictions -> (time, pair, prob)
  load_pred <- function(path) {
    df <- readr::read_csv(path, show_col_types = FALSE)
    nm <- tolower(names(df))
    names(df) <- nm

    # rename p->prob if needed
    if (!("prob" %in% names(df)) && ("p" %in% names(df))) df <- dplyr::rename(df, prob = p)

    # infer time from filename if missing
    tm <- NA_character_
    if (endsWith(path, "_M.csv")) tm <- "M"
    if (endsWith(path, "_E.csv")) tm <- "E"
    if (!"time" %in% names(df)) df$time <- tm

    # build pair if needed
    if (!("pair" %in% names(df))) {
      stopifnot(all(c("d1","d2") %in% names(df)))
      df <- df |> mutate(pair = paste0(d1, d2))
    }

    stopifnot("prob" %in% names(df))
    df |> select(time, pair, prob)
  }

  pred_list <- lapply(unique(existing), function(pth) {
    tryCatch(load_pred(pth), error = function(e) {
      message("Skipping ", pth, " (", conditionMessage(e), ")"); NULL
    })
  })
  preds <- dplyr::bind_rows(Filter(Negate(is.null), pred_list))
  if (nrow(preds) == 0) stop("No usable prediction rows after parsing.")

  # If time still NA (only fallback), duplicate for both
  if (all(is.na(preds$time))) {
    preds <- bind_rows(preds |> mutate(time = "M"),
                       preds |> mutate(time = "E"))
  }

  # 3) Filter to FDR-approved pairs and write playlists
  filt <- preds |>
    inner_join(zs, by = "pair") |>
    arrange(desc(prob))

  dir.create("outputs", showWarnings = FALSE)
  play_M  <- filt |> filter(time == "M") |> slice_head(n = k) |> select(pair, prob, z, p_fdr)
  play_E  <- filt |> filter(time == "E") |> slice_head(n = k) |> select(pair, prob, z, p_fdr)
  play_all<- filt |> arrange(desc(prob)) |> slice_head(n = k) |> select(time, pair, prob, z, p_fdr)

  write_csv(play_M,  "outputs/playlist_M.csv")
  write_csv(play_E,  "outputs/playlist_E.csv")
  write_csv(play_all,"outputs/playlist_all.csv")

  message("Wrote outputs/playlist_M.csv, playlist_E.csv, playlist_all.csv")
  invisible(list(M = play_M, E = play_E, all = play_all, approved = zs))
}

