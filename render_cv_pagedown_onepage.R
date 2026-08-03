# Renders the condensed one-page CV (docs/cv-1page.pdf), searching for the
# largest number of most-cited publications (and, if needed, awards) that
# still fits on one page.
#
# pagedown::chrome_print() has no way to pass `params` through to
# rmarkdown::render() itself, so this renders to HTML first (with params),
# then prints *that* HTML file to PDF.
#
# Same caveats as the other render_cv_*.R scripts: run *after*
# `quarto render`, since that step wipes docs/ clean.
#   quarto render && Rscript render_cv_pagedown_onepage.R

source("_cv_pagedown_setup.R")
library(pdftools)

render_attempt <- function(n_awards, n_pubs) {
  html_out <- rmarkdown::render(
    "_cv-pagedown-onepage.Rmd",
    params = list(n_awards = n_awards, n_pubs = n_pubs),
    output_file = "_cv-pagedown-onepage.html",
    envir = new.env(),
    quiet = TRUE
  )
  pagedown::chrome_print(html_out, output = "docs/cv-1page.pdf", verbose = 0)
  pdftools::pdf_length(file.path("docs", "cv-1page.pdf"))
}

n_awards <- 6
n_pubs <- 10
pages <- render_attempt(n_awards, n_pubs)

# Shrink the publication count until it fits on one page.
while (pages > 1 && n_pubs > 0) {
  n_pubs <- n_pubs - 1
  pages <- render_attempt(n_awards, n_pubs)
}

# If it still doesn't fit with zero publications, the awards list itself
# is too long -- shrink that too.
while (pages > 1 && n_awards > 0) {
  n_awards <- n_awards - 1
  pages <- render_attempt(n_awards, n_pubs)
}

if (pages > 1) {
  warning("Could not fit the one-page CV onto a single page even with n_awards=0, n_pubs=0.")
} else {
  cat(sprintf(
    "One-page CV: %d awards, %d most-cited publications, %d page(s)\n",
    n_awards, n_pubs, pages
  ))
}
