# Florida Picks Project

Work-in-progress README. See GitHub for full docs soon.

# Florida Picks Project

[![Update Pick2 data](https://github.com/raghava0071/florida-picks-project/actions/workflows/update-data.yml/badge.svg)](https://github.com/raghava0071/florida-picks-project/actions/workflows/update-data.yml)
[![CI](https://github.com/raghava0071/florida-picks-project/actions/workflows/ci.yml/badge.svg)](https://github.com/raghava0071/florida-picks-project/actions/workflows/ci.yml)

Clean, reproducible pipeline to fetch and parse Florida Lottery **Pick 2** results into `data/clean/pick2_history.csv`, with daily auto-updates via GitHub Actions.

## Quick start (R)
```r
source("R/parse_pick2.R")
pick2 <- parse_pick2()
head(pick2)
