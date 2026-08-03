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

# --- content/cv_entries.csv: single source of truth for education,
# positions, awards, grants, service, professional development, teaching,
# and talks. Add/edit rows directly in that CSV -- no code changes needed.
# Columns:
#   section        education | position | award | grant | service |
#                  prof-dev | teaching | talk
#   year           display text, e.g. "2023", "2025-present", "2024–"
#   sort_year      numeric year used to order rows (teaching rows that
#                  have no date use 0 and keep their CSV row order)
#   title          the main text (degree, job title, or entry description)
#   where          institution/organisation (education & position only)
#   location       city, country (education & position only)
#   show_full_cv, show_onepage_cv, show_website
#                  TRUE/FALSE -- controls which of the three outputs
#                  (full PDF CV, condensed 1-page PDF CV, website CV page)
#                  include that row. Edit these directly to add or hide
#                  content per view.
cv_entries <- function(section = NULL, view = NULL) {
  d <- readr::read_csv(here::here("content", "cv_entries.csv"), show_col_types = FALSE)
  if (!is.null(section)) d <- d |> filter(.data$section %in% .env$section)
  if (!is.null(view)) {
    col <- paste0("show_", view)
    d <- d[d[[col]], ]
  }
  d |> arrange(desc(sort_year))
}

education_tibble <- function(view = "website") cv_entries("education", view)
positions_tibble <- function(view = "website") cv_entries("position", view)

# Prose renderers for the website's education/position entries (bold
# title, then institution/location/year in one line) -- the PDF CVs
# instead build their own year|title kable() tables straight from
# education_tibble()/positions_tibble() since they show year as its own
# column.
render_cv_education <- function(view = "website") {
  ed <- education_tibble(view)
  if (nrow(ed) == 0) return("")
  glue::glue_collapse(
    glue::glue("**{ed$title}** — {ed$where}, {ed$location} ({ed$year})"),
    sep = "\n\n"
  )
}

render_cv_positions <- function(view = "website") {
  pos <- positions_tibble(view)
  if (nrow(pos) == 0) return("")
  glue::glue_collapse(
    glue::glue("**{pos$title}**, {pos$where}, {pos$location} ({pos$year})"),
    sep = "\n\n"
  )
}

# Generic renderer for the "- year — title" sections (awards, grants,
# service, prof-dev, talks) or the year-less "- title" bullets (teaching).
# Used by the website CV page; the PDF CVs build their own kable() tables
# straight from cv_entries() since they show year/title in separate columns.
render_cv_section <- function(section, view = "full_cv") {
  d <- cv_entries(section, view)
  if (nrow(d) == 0) return("")
  if (section == "teaching") {
    glue::glue_collapse(glue::glue("- {d$title}"), sep = "\n")
  } else {
    glue::glue_collapse(glue::glue("- {d$year} — {d$title}"), sep = "\n")
  }
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

# content/pubs.csv has the same show_full_cv/show_onepage_cv/show_website
# columns as cv_entries.csv -- pass view = NULL to get every publication
# regardless of visibility flags.
load_pubs <- function(view = "full_cv") {
  pubs <- readr::read_csv(here::here("content", "pubs.csv"), show_col_types = FALSE)
  scholar_path <- here::here("content", "scholar_pubs.csv")
  if (file.exists(scholar_path)) {
    scholar_pubs <- readr::read_csv(scholar_path, show_col_types = FALSE)
    pubs$cites <- vapply(pubs$title, match_scholar_cites, double(1), scholar_pubs = scholar_pubs)
  } else {
    pubs$cites <- NA_integer_
  }
  if (!is.null(view)) {
    col <- paste0("show_", view)
    pubs <- pubs[pubs[[col]], ]
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

render_cv_publications <- function(view = "full_cv") {
  # Uses "**N.**" instead of markdown "N." list syntax, because pandoc
  # auto-increments real ordered lists and ignores our own numbering.
  pubs <- load_pubs(view)
  if (nrow(pubs) == 0) return("")
  lines <- vapply(seq_len(nrow(pubs)), function(i) {
    pub <- pubs[i, ]
    glue::glue("**{pub$index}.** {format_pub_citation(pub)}")
  }, character(1))
  paste(lines, collapse = "\n\n")
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

# Strip the protocol from a URL, e.g. "https://example.com/x" -> "example.com/x"
strip_protocol <- function(url) sub("^https?://", "", url)

# Last non-empty path segment of a URL, e.g. ".../in/jane-doe/" -> "jane-doe"
url_handle <- function(url) {
  url <- sub("/$", "", url)
  parts <- strsplit(url, "/")[[1]]
  parts[length(parts)]
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
