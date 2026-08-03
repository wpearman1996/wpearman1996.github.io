# Renders the PDF CV (vitae::twentyseconds template) to docs/cv.pdf.
#
# This is a separate step from `quarto render` / `quarto preview` -- vitae's
# output format isn't a native Quarto format, so the source file is named
# _cv-pdf.Rmd (underscore prefix) to keep Quarto's project render from
# touching it. IMPORTANT: `quarto render` wipes and rebuilds the whole docs/
# folder, which would delete this PDF -- always run this script *after*
# `quarto render`, never before:
#   quarto render && Rscript render_cv_pdf.R
#
# Re-run whenever content/cv_*.csv, content/pubs.csv, or _cv-pdf.Rmd change.

pandoc_dir <- file.path(
  "/Applications/quarto/bin/tools",
  if (Sys.info()[["machine"]] == "arm64") "aarch64" else "x86_64"
)
if (nzchar(Sys.which("pandoc")) == FALSE) {
  Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
}

rmarkdown::render("_cv-pdf.Rmd", output_file = "cv.pdf", output_dir = "docs")
