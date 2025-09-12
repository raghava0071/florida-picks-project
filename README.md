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

# Notes
- This project is for academic/educational use. Always review the lottery’s terms of use before redistribution.
- Parsing logic is defensive: if the source PDF format shifts, the script falls back to regex text parsing.
- Data pipeline:
  1) Download latest `p2.pdf`
  2) Parse into tidy columns: `draw_date`, `time` (M/E), `d1`, `d2`
  3) Append/update `data/clean/pick2_history.csv`
- CI runs basic checks (lint, parse, tiny snapshot test). The daily workflow commits new data if available.

## License
- Code: MIT (do whatever, include the copyright notice).
- Data: © Florida Lottery or respective publisher; used here for research/education.
