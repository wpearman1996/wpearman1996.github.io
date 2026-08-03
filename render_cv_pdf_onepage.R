# Renders the condensed one-page CV (docs/cv-1page.pdf), searching for the
# largest number of most-cited publications that still fits on one page.
#
# Same caveats as render_cv_pdf.R: run *after* `quarto render`, since that
# step wipes docs/ clean.
#   quarto render && Rscript render_cv_pdf.R && Rscript render_cv_pdf_onepage.R

source("_cv_render_setup.R")
library(pdftools)

render_attempt <- function(n_distinctions, n_pubs) {
  rmarkdown::render(
    "_cv-pdf-onepage.Rmd",
    output_file = "cv-1page.pdf",
    output_dir = "docs",
    params = list(n_distinctions = n_distinctions, n_pubs = n_pubs),
    quiet = TRUE
  )
  pdftools::pdf_length(file.path("docs", "cv-1page.pdf"))
}

n_distinctions <- 6
n_pubs <- 10
pages <- render_attempt(n_distinctions, n_pubs)

# Shrink the publication count until it fits on one page.
while (pages > 1 && n_pubs > 0) {
  n_pubs <- n_pubs - 1
  pages <- render_attempt(n_distinctions, n_pubs)
}

# If it still doesn't fit with zero publications, the distinctions list
# itself is too long -- shrink that too.
while (pages > 1 && n_distinctions > 0) {
  n_distinctions <- n_distinctions - 1
  pages <- render_attempt(n_distinctions, n_pubs)
}

if (pages > 1) {
  warning("Could not fit the one-page CV onto a single page even with n_distinctions=0, n_pubs=0.")
} else {
  cat(sprintf(
    "One-page CV: %d distinctions, %d most-cited publications, %d page(s)\n",
    n_distinctions, n_pubs, pages
  ))
}
