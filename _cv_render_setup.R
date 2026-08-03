# Shared setup for render_cv_pdf.R and render_cv_pdf_onepage.R.

pandoc_dir <- file.path(
  "/Applications/quarto/bin/tools",
  if (Sys.info()[["machine"]] == "arm64") "aarch64" else "x86_64"
)
if (nzchar(Sys.which("pandoc")) == FALSE) {
  Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
}

# The Scholar-stats sidebar box needs a `\scholarstats{...}` setter call
# added to vitae's installed pandoc template (the template path is an
# absolute path inside the R package, not something a local file can
# shadow, unlike twentysecondcv.cls). Patch it in place, idempotently,
# so a vitae reinstall/upgrade doesn't silently drop the feature.
template_path <- system.file(
  "rmarkdown", "templates", "twentyseconds", "resources",
  "twentysecondstemplate.tex",
  package = "vitae"
)
template_lines <- readLines(template_path)
if (!any(grepl("\\\\scholarstats\\{", template_lines, fixed = FALSE))) {
  insert_after <- grep("^\\\\aboutme\\{", template_lines)
  template_lines <- append(
    template_lines,
    "\\scholarstats{$if(scholarstats)$$scholarstats$$endif$}",
    after = insert_after
  )
  writeLines(template_lines, template_path)
  message("Patched vitae's twentyseconds template to support \\scholarstats{}")
}

# Inline R in the YAML header (e.g. `scholarstats`) is evaluated before
# any chunk in the body runs, including the doc's own `setup` chunk -- so
# _common.R needs to already be loaded in this session for that to work.
source("_common.R")
