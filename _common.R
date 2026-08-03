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

# Bold the CV owner's own name wherever it appears in an author list.
bold_own_name <- function(authors, name = "Pearman, W.S.") {
  gsub(name, glue("**{name}**"), authors, fixed = TRUE)
}

format_pub_citation <- function(pub) {
  authors_str <- bold_own_name(pub$authors)
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
  # Both CVs render via HTML/chrome_print now, so a full DOI can wrap
  # normally -- no more LaTeX unbreakable-token overflow risk.
  doi_str <- glue("[doi:{pub$doi}](https://doi.org/{pub$doi})")
  paste(authors_str, year_str, title_str, journal_str, doi_str)
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
      '<span class="icon-link" style="background-color:#6A737D;">',
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
  ed <- education_tibble()
  glue::glue_collapse(
    glue::glue("**{ed$degree}** — {ed$institution}, {ed$location} ({ed$year})"),
    sep = "\n\n"
  )
}

render_cv_positions <- function() {
  pos <- positions_tibble()
  glue::glue_collapse(
    glue::glue("**{pos$title}**, {pos$institution}, {pos$location} ({pos$dates})"),
    sep = "\n\n"
  )
}

distinctions_tibble <- function(category = NULL) {
  # .env$category (not bare `category`) is required: dplyr's data mask
  # resolves an unqualified name against the data frame's own columns
  # first, and this data frame already has a column called "category" --
  # `filter(category %in% category)` would silently compare the column
  # to itself (always TRUE) instead of to this function's argument.
  d <- readr::read_csv(here::here("content", "cv_distinctions.csv"), show_col_types = FALSE)
  if (!is.null(category)) d <- d |> filter(.data$category %in% .env$category)
  d
}

render_cv_distinctions <- function(top_n = NULL, category = NULL) {
  d <- distinctions_tibble(category = category)
  if (!is.null(top_n)) d <- head(d, top_n)
  glue::glue_collapse(
    glue::glue("- {d$year} — {d$description}"),
    sep = "\n"
  )
}

render_cv_teaching <- function() {
  t <- readr::read_csv(here::here("content", "cv_teaching.csv"), show_col_types = FALSE)
  glue::glue_collapse(
    glue::glue("- {t$description}"),
    sep = "\n"
  )
}

presentations_tibble <- function() {
  readr::read_csv(here::here("content", "cv_presentations.csv"), show_col_types = FALSE) |>
    arrange(desc(year))
}

render_cv_presentations <- function() {
  p <- presentations_tibble()
  glue::glue_collapse(
    glue::glue("- {p$year} — {p$description}"),
    sep = "\n"
  )
}

# Google Scholar summary for the CV PDF sidebar. Plain text with no
# backslashes/LaTeX commands -- this gets substituted into a
# double-quoted YAML string, where a literal "\" would break parsing.
scholar_stats_line <- function() {
  profile_path <- here::here("content", "scholar_profile.csv")
  if (!file.exists(profile_path)) return("")
  p <- readr::read_csv(profile_path, show_col_types = FALSE)
  glue::glue("{p$total_cites} citations | h-index: {p$h_index} | i10-index: {p$i10_index}")
}

# --- Tibbles used directly by _cv-pagedown*.Rmd ---

education_tibble <- function() {
  readr::read_csv(here::here("content", "cv_education.csv"), show_col_types = FALSE) |>
    arrange(desc(year))
}

positions_tibble <- function() {
  readr::read_csv(here::here("content", "cv_positions.csv"), show_col_types = FALSE)
}

# Strip the protocol from a URL, e.g. "https://example.com/x" -> "example.com/x"
strip_protocol <- function(url) sub("^https?://", "", url)

# Last non-empty path segment of a URL, e.g. ".../in/jane-doe/" -> "jane-doe"
url_handle <- function(url) {
  url <- sub("/$", "", url)
  parts <- strsplit(url, "/")[[1]]
  parts[length(parts)]
}

render_cv_publications <- function(top_n = NULL, rank_by = c("index", "cites")) {
  # Uses "**N.**" instead of markdown "N." list syntax, because pandoc
  # auto-increments real ordered lists and ignores our own numbering.
  rank_by <- match.arg(rank_by)
  pubs <- load_pubs()
  if (!is.null(top_n)) {
    if (rank_by == "cites") pubs <- pubs |> arrange(desc(coalesce(cites, -1)))
    pubs <- head(pubs, top_n)
  }
  lines <- vapply(seq_len(nrow(pubs)), function(i) {
    pub <- pubs[i, ]
    glue::glue("**{pub$index}.** {format_pub_citation(pub)}")
  }, character(1))
  paste(lines, collapse = "\n\n")
}
