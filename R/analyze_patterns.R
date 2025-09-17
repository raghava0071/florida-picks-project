
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(readr)
})

# ensure folders exist
dir.create("data/analysis", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs",       recursive = TRUE, showWarnings = FALSE)

# load data
pick2 <- read_csv("data/clean/pick2_history.csv", show_col_types = FALSE) %>%
  mutate(
    draw_date = as.Date(draw_date),
    time      = as.character(time),
    d1        = as.integer(d1),
    d2        = as.integer(d2)
  )

# ===== 1) Heatmap of standardized residuals (d1 vs d2) =====
tab12    <- table(pick2$d1, pick2$d2)
expected <- suppressWarnings(chisq.test(tab12)$expected)
std_res  <- (tab12 - expected) / sqrt(expected)

res_df <- as.data.frame(as.table(std_res))
names(res_df) <- c("d1","d2","std_resid")
res_df$d1 <- as.integer(as.character(res_df$d1))
res_df$d2 <- as.integer(as.character(res_df$d2))

p <- ggplot(res_df, aes(x = factor(d2), y = factor(d1), fill = std_resid)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.1f", std_resid)), size = 3) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0) +
  labs(title = "Standardized residuals for (d1, d2)",
       x = "d2", y = "d1", fill = "Std resid")

ggsave("outputs/std_resid_heatmap.png", p, width = 6, height = 5, dpi = 150)

# ===== 2) Transitions prev -> next (pair = 10*d1 + d2) =====
transitions <- pick2 %>%
  arrange(time, draw_date) %>%
  group_by(time) %>%
  mutate(
    pair      = d1 * 10 + d2,
    prev_pair = lag(pair),
    next_pair = lead(pair)
  ) %>%
  filter(!is.na(prev_pair), !is.na(next_pair)) %>%
  count(prev_pair, next_pair, name = "n") %>%
  group_by(prev_pair) %>%
  mutate(p = n / sum(n)) %>%
  ungroup() %>%
  rename(prev = prev_pair, `next` =  next_pair)

write_csv(transitions, "data/analysis/transitions.csv")

message("Saved outputs/std_resid_heatmap.png and data/analysis/transitions.csv")

