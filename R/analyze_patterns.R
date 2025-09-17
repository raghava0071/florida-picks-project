suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2)
})

stopifnot(file.exists("data/clean/pick2_history.csv"))
pick2 <- read_csv("data/clean/pick2_history.csv", show_col_types = FALSE) %>%
  mutate(
    draw_date = as.Date(draw_date),
    time = as.character(time),
    d1 = as.integer(d1),
    d2 = as.integer(d2)
  )

# --- Heatmap of standardized residuals ---------------------------------------
tab <- table(pick2$d1, pick2$d2)
exp <- suppressWarnings(chisq.test(tab)$expected)
std <- (tab - exp) / sqrt(exp)
df  <- as.data.frame(as.table(std)); names(df) <- c("d1","d2","std_resid")
df  <- df %>% mutate(d1 = as.integer(as.character(d1)),
                     d2 = as.integer(as.character(d2)))

p <- ggplot(df, aes(x = factor(d2), y = factor(d1), fill = std_resid)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.1f", std_resid)), size = 3) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0) +
  labs(title = "Standardized residuals (d1 vs d2)", x = "d2", y = "d1", fill = "Std resid")
dir.create("outputs", showWarnings = FALSE)
ggsave("outputs/std_resid_heatmap.png", p, width = 7, height = 5, dpi = 120)

# --- Transition table (simple Markov on 2-digit state) -----------------------
transitions <- pick2 %>%
  arrange(time, draw_date) %>%
  group_by(time) %>%
  mutate(prev = d1*10 + d2,
         `next` = lead(prev)) %>%
  filter(!is.na(`next`)) %>%
  count(prev, `next`, name = "n") %>%
  group_by(prev) %>%
  mutate(p = n / sum(n)) %>%
  ungroup()

dir.create("data/analysis", recursive = TRUE, showWarnings = FALSE)
write_csv(transitions, "data/analysis/transitions.csv")

message("Saved outputs/std_resid_heatmap.png and data/analysis/transitions.csv")
