# Renders the condensed one-page CV (docs/cv-1page.pdf).
#
# What appears here is controlled entirely by content/cv_entries.csv's and
# content/pubs.csv's show_onepage_cv columns -- flip those flags to add or
# remove rows, rather than editing _cv-pagedown-onepage.Rmd. This script
# just renders once and reports the resulting page count so you know if
# your current flag selection still fits on one page.
#
# Same caveats as the other render_cv_*.R scripts: run *after*
# `quarto render`, since that step wipes docs/ clean.
#   quarto render && Rscript render_cv_pagedown_onepage.R

source("_cv_pagedown_setup.R")
library(pdftools)

html_out <- rmarkdown::render(
  "_cv-pagedown-onepage.Rmd",
  output_file = "_cv-pagedown-onepage.html",
  envir = new.env(),
  quiet = TRUE
)
pagedown::chrome_print(html_out, output = "docs/cv-1page.pdf", verbose = 0)
pages <- pdftools::pdf_length(file.path("docs", "cv-1page.pdf"))

if (pages > 1) {
  warning(sprintf(
    "cv-1page.pdf is %d pages, not 1. Set some show_onepage_cv flags to FALSE in content/cv_entries.csv or content/pubs.csv and re-run.",
    pages
  ))
} else {
  cat("One-page CV: 1 page, fits.\n")
}
