dir.create("docs", showWarnings = FALSE)

# Ensure packages are present
if (!requireNamespace("rmarkdown", quietly = TRUE)) install.packages("rmarkdown", repos="https://cloud.r-project.org")
if (!requireNamespace("knitr",      quietly = TRUE)) install.packages("knitr",      repos="https://cloud.r-project.org")

# Make sure pandoc is on PATH (RStudio sets RSTUDIO_PANDOC env var)
if (nzchar(Sys.getenv("RSTUDIO_PANDOC"))) {
  p <- Sys.getenv("RSTUDIO_PANDOC")
  if (!grepl(p, Sys.getenv("PATH"))) Sys.setenv(PATH = paste(p, Sys.getenv("PATH"), sep=.Platform$path.sep))
}

# Render the report (show messages so you see errors if any)
rmarkdown::render("reports_daily.Rmd",
                  output_file = "docs/index.html",
                  quiet = FALSE)



You should now see **both** “Top-25 (raw)” and “Calibrated Top-25” in the report.

---

## 2) Run the Action once & verify the calibrated CSV is committed

**On GitHub → Actions → Update Pick2 data → Run workflow**.  
When it completes, check the repo for `outputs/top25_pairs_next_draw_cal.csv` updated in the last commit (you already changed the workflow to include it).

If it doesn’t commit, re-run locally to confirm the file exists:
```bash
cd ~/Desktop/florida-picks-project
R -q -e 'source("scripts/update_all.R")'
ls -lh outputs/top25_pairs_next_draw_cal.csv
