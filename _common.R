library(htmltools)
library(dplyr)
library(readr)
library(yaml)
library(glue)

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

load_meta <- function() {
  yaml::read_yaml(here::here("content", "cv_meta.yml"))
}

# Fuzzy-match a bibliographic title against the cached Scholar pub list
# (titles differ slightly in punctuation/case between sources) and return
# the citation count, or NA if no confident match is found.
match_scholar_cites <- function(title, scholar_pubs) {
  norm <- function(x) tolower(gsub("[^a-z0-9]", "", tolower(x)))
  target <- norm(title)
  scholar_norm <- norm(scholar_pubs$title)
  hit <- which(scholar_norm == target)
  if (length(hit) == 0) {
    # fall back to a fuzzy match on the first ~40 chars
    short_target <- substr(target, 1, 40)
    hit <- which(startsWith(scholar_norm, short_target) |
                   startsWith(short_target, substr(scholar_norm, 1, 40)))
  }
  if (length(hit) == 0) return(NA_real_)
  as.numeric(scholar_pubs$cites[hit[1]])
}

load_pubs <- function() {
  pubs <- readr::read_csv(here::here("content", "pubs.csv"), show_col_types = FALSE)
  scholar_path <- here::here("content", "scholar_pubs.csv")
  if (file.exists(scholar_path)) {
    scholar_pubs <- readr::read_csv(scholar_path, show_col_types = FALSE)
    pubs$cites <- vapply(pubs$title, match_scholar_cites, double(1), scholar_pubs = scholar_pubs)
  } else {
    pubs$cites <- NA_integer_
  }
  pubs |> arrange(desc(index))
}

format_pub_citation <- function(pub) {
  parts <- c(glue("{pub$authors}"))
  year_str <- glue("({pub$year}).")
  title_str <- glue("{pub$title}.")
  journal_str <- glue("*{pub$journal}*")
  if (!is.na(pub$volume) && nzchar(pub$volume)) {
    journal_str <- glue("{journal_str}, {pub$volume}")
  }
  if (!is.na(pub$pages) && nzchar(pub$pages)) {
    journal_str <- glue("{journal_str}, {pub$pages}")
  }
  journal_str <- glue("{journal_str}.")
  doi_str <- glue("[doi:{pub$doi}](https://doi.org/{pub$doi})")
  paste(pub$authors, year_str, title_str, journal_str, doi_str)
}

make_pub_list <- function(pubs) {
  items <- lapply(seq_len(nrow(pubs)), function(i) make_pub_item(pubs[i, ]))
  htmltools::HTML(paste(unlist(items), collapse = ""))
}

make_pub_item <- function(pub) {
  cite_html <- markdown::renderMarkdown(text = format_pub_citation(pub))
  cite_html <- gsub("^<p>|</p>\\n?$", "", cite_html)
  cites_badge <- ""
  if (!is.na(pub$cites)) {
    cites_badge <- glue::glue(
      '<span class="icon-link" style="background-color:#54807b;">',
      '{pub$cites} citation{ifelse(pub$cites == 1, "", "s")}</span> '
    )
  }
  doi_link <- icon_link(
    icon = "fa-solid fa-arrow-up-right-from-square",
    text = "View",
    url = glue::glue("https://doi.org/{pub$doi}")
  )
  htmltools::HTML(glue::glue(
    '<div class="pub" style="margin-bottom: 1.25rem;">',
    '<div>{cite_html}</div>',
    '<div>{cites_badge}{doi_link}</div>',
    '</div>'
  ))
}

icon_link <- function(icon = NULL, text = NULL, url = NULL, class = "icon-link", target = "_blank") {
  label <- if (!is.null(icon)) htmltools::HTML(paste0('<i class="', icon, '"></i> ', text)) else text
  htmltools::a(href = url, label, class = class, target = target, rel = "noopener")
}

# --- Plain-markdown CV renderers (shared by cv.qmd and cv-pdf.qmd so the
# HTML and PDF versions always stay in sync with the same source data) ---

render_cv_education <- function() {
  ed <- readr::read_csv(here::here("content", "cv_education.csv"), show_col_types = FALSE) |>
    arrange(desc(year))
  glue::glue_collapse(
    glue::glue("**{ed$degree}** — {ed$institution} ({ed$year})"),
    sep = "\n\n"
  )
}

render_cv_positions <- function() {
  pos <- readr::read_csv(here::here("content", "cv_positions.csv"), show_col_types = FALSE)
  glue::glue_collapse(
    glue::glue("**{pos$title}**, {pos$institution} ({pos$dates})"),
    sep = "\n\n"
  )
}

render_cv_distinctions <- function() {
  d <- readr::read_csv(here::here("content", "cv_distinctions.csv"), show_col_types = FALSE)
  glue::glue_collapse(
    glue::glue("- {d$year} — {d$description}"),
    sep = "\n"
  )
}

render_cv_publications <- function() {
  # Uses "**N.**" instead of markdown "N." list syntax, because pandoc
  # auto-increments real ordered lists and ignores our own numbering.
  pubs <- load_pubs()
  lines <- vapply(seq_len(nrow(pubs)), function(i) {
    pub <- pubs[i, ]
    glue::glue("**{pub$index}.** {format_pub_citation(pub)}")
  }, character(1))
  paste(lines, collapse = "\n\n")
}
