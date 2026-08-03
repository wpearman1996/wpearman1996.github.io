# Shared setup for render_cv_pagedown.R and render_cv_pagedown_onepage.R.

pandoc_dir <- file.path(
  "/Applications/quarto/bin/tools",
  if (Sys.info()[["machine"]] == "arm64") "aarch64" else "x86_64"
)
if (nzchar(Sys.which("pandoc")) == FALSE) {
  Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
}

# pagedown::chrome_print's local preview server 404s on /favicon.ico in a
# way that aborts the print step entirely -- an empty placeholder avoids it.
if (!file.exists("favicon.ico")) file.create("favicon.ico")
