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
