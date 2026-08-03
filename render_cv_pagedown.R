# Renders the pagedownCV-style CV (docs/cv.pdf) via headless Chrome.
#
# Same caveats as the other render_cv_*.R scripts: run *after*
# `quarto render`, since that step wipes docs/ clean.
#   quarto render && Rscript render_cv_pagedown.R

source("_cv_pagedown_setup.R")

pagedown::chrome_print("_cv-pagedown.Rmd", output = "docs/cv.pdf")
