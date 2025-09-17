suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(lubridate)})

pick2 <- read_csv("data/clean/pick2_history.csv", show_col_types = FALSE) |>
  mutate(draw_date = as.Date(draw_date),
         time = as.character(time),
         d1 = as.integer(d1),
         d2 = as.integer(d2),
         wday = wday(draw_date, label=FALSE)) |>
  arrange(draw_date)

last_date <- max(pick2$draw_date, na.rm = TRUE)
next_wday <- wday(last_date + 1, label=FALSE)

train <- pick2 |> filter(draw_date > last_date - 180, draw_date <= last_date)
weekday_tr <- train |> filter(wday == next_wday)

mk_probs <- function(df, time_code) {
  tr <- df |> filter(time == time_code)
  if (nrow(tr) < 50) return(NULL)
  p1 <- tr |> count(d1) |> complete(d1=0:9, fill=list(n=0)) |>
    arrange(d1) |> mutate(p=(n+1)/(sum(n)+10))
  p2 <- tr |> count(d2) |> complete(d2=0:9, fill=list(n=0)) |>
    arrange(d2) |> mutate(p=(n+1)/(sum(n)+10))
  list(p1=p1, p2=p2)
}

wkM <- mk_probs(weekday_tr, "M"); wkE <- mk_probs(weekday_tr, "E")
glM <- mk_probs(train, "M");     glE <- mk_probs(train, "E")

blend <- function(wk, gl, alpha=0.7) {
  if (is.null(wk)) return(gl)
  if (is.null(gl)) return(wk)
  wk$p1$p <- alpha*wk$p1$p + (1-alpha)*gl$p1$p
  wk$p2$p <- alpha*wk$p2$p + (1-alpha)*gl$p2$p
  wk
}

bM <- blend(wkM, glM, 0.7); bE <- blend(wkE, glE, 0.7)

to_row <- function(tag, probs) {
  if (is.null(probs)) return(NULL)
  tibble(time = tag,
         !!!setNames(as.list(probs$p1$p), paste0("d1_",0:9)),
         !!!setNames(as.list(probs$p2$p), paste0("d2_",0:9)))
}

pred <- bind_rows(to_row("M", bM), to_row("E", bE))
if (is.null(pred) || nrow(pred)==0) stop("No predictions computed.")

readr::write_csv(pred, "outputs/predictions_next_draw_weekday.csv")

grid_top <- function(probs) {
  g <- tidyr::expand_grid(d1=0:9, d2=0:9) |>
    mutate(p = probs$p1$p[match(d1, probs$p1$d1)] * probs$p2$p[match(d2, probs$p2$d2)]) |>
    arrange(desc(p))
  g[1:25,]
}
top <- bind_rows(
  if (!is.null(bM)) grid_top(bM) |> mutate(time="M") else NULL,
  if (!is.null(bE)) grid_top(bE) |> mutate(time="E") else NULL
)
readr::write_csv(top, "outputs/top25_pairs_next_draw_weekday.csv")
message("Wrote outputs/predictions_next_draw_weekday.csv and outputs/top25_pairs_next_draw_weekday.csv")
