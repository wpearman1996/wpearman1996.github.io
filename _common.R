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

# Display headings for the CV sections balanced by allocate_columns().
cv_section_titles <- c(
  award      = "Awards and honours",
  grant      = "Selected major grants",
  talk       = "Selected talks",
  service    = "Selected service",
  "prof-dev" = "Professional development"
)

# --- Manual pagination for the CV's balanced Awards/Grants/Talks/
# Service/Professional-development block --------------------------------
#
# The print engine (paged.js, used by pagedown::html_paged) cannot
# reliably fragment two side-by-side CSS columns across a page break --
# empirically, whichever column is the *second* DOM child defers its
# entire remaining content to the next page even when there's still
# room on the current one (confirmed by swapping which section's
# content went in which column and watching the bug follow the column
# position, not the content). Flex, CSS grid, floats and inline-block
# all showed the same behaviour. To work around it, the page break is
# computed here in R instead: each column's entries are split into a
# chunk that's sized to fit what's actually left on page 1 and a
# remainder chunk, rendered as two separate blocks, so the browser
# never has to fragment either one itself.
#
# The constants below are calibrated against a real rendered PDF via
# pdftools::pdf_data() (measuring actual entry/heading pixel heights),
# not guessed from font metrics. CV_BALANCED_BLOCK_START_PT in
# particular is the measured y-position where this block starts on
# page 1 (i.e. how much space the Research summary/Contact and
# Education/Teaching rows above it use) -- re-measure it if that fixed
# content changes enough to noticeably shift its height; entries added
# to content/cv_entries.csv don't require any recalibration.
CV_ENTRY_PAD_PT <- 18
CV_ENTRY_LINE_PT <- 13.5
CV_CHARS_PER_LINE <- 58
CV_HEADING_HEIGHT_PT <- 29
CV_PAGE_BOTTOM_PT <- 820
CV_BALANCED_BLOCK_START_PT <- 592

# Display headings for the CV sections balanced below.
cv_section_titles <- c(
  award      = "Awards and honours",
  grant      = "Selected major grants",
  talk       = "Selected talks",
  service    = "Selected service",
  "prof-dev" = "Professional development"
)

entry_lines <- function(title) pmax(1L, ceiling(nchar(title) / CV_CHARS_PER_LINE))
entry_height_pt <- function(title) CV_ENTRY_PAD_PT + CV_ENTRY_LINE_PT * entry_lines(title)

section_height_pt <- function(section, view) {
  d <- cv_entries(section, view)
  if (nrow(d) == 0) return(0)
  CV_HEADING_HEIGHT_PT + sum(entry_height_pt(d$title))
}

# Greedily split `sections` (section names, in their canonical reading
# order) between the CV's left/right columns so the two columns end up
# with roughly the same total height -- so a short section (e.g.
# Awards) doesn't leave a column with unused white space while a
# taller one (e.g. Grants) is still full of content. Sections are
# assigned largest-first to whichever column currently has less content
# (a standard longest-processing-time bin-balancing heuristic), then
# each column's members are put back in their original canonical order
# for natural reading. A section's own entries always stay together in
# this step -- only whole sections move between columns -- so the
# balance recomputes automatically whenever content/cv_entries.csv
# changes.
allocate_columns <- function(sections, view) {
  heights <- vapply(sections, section_height_pt, numeric(1), view = view)
  order_by_size <- order(heights, decreasing = TRUE)
  left <- character(0); right <- character(0)
  left_h <- 0; right_h <- 0
  for (i in order_by_size) {
    if (left_h <= right_h) {
      left <- c(left, sections[i]); left_h <- left_h + heights[i]
    } else {
      right <- c(right, sections[i]); right_h <- right_h + heights[i]
    }
  }
  list(left = sections[sections %in% left], right = sections[sections %in% right])
}

# Flatten a column's sections into an ordered list of "blocks" -- one
# per section heading and one per entry row -- each tagged with its
# calibrated height in points, ready for split_column() to cut at a
# page boundary.
column_blocks <- function(sections, view) {
  blocks <- list()
  for (sec in sections) {
    d <- cv_entries(sec, view)
    if (nrow(d) == 0) next
    blocks[[length(blocks) + 1]] <- list(type = "heading", section = sec, height = CV_HEADING_HEIGHT_PT)
    for (i in seq_len(nrow(d))) {
      blocks[[length(blocks) + 1]] <- list(
        type = "entry", section = sec,
        year = d$year[i], title = d$title[i],
        height = entry_height_pt(d$title[i])
      )
    }
  }
  blocks
}

# Split a column's blocks into a "page1" portion that fits within
# budget_pt and a "rest" portion for the following page. Never leaves
# a heading stranded alone at the bottom of page 1 with none of its
# entries following it. If the cut falls in the middle of a section,
# "rest" gets that section's heading again with a "(cont.)" suffix so
# it isn't missing a label.
split_column <- function(blocks, budget_pt) {
  used <- 0; cut <- 0
  for (i in seq_along(blocks)) {
    used_next <- used + blocks[[i]]$height
    if (used_next > budget_pt) break
    used <- used_next
    cut <- i
  }
  if (cut > 0 && blocks[[cut]]$type == "heading") cut <- cut - 1
  page1 <- if (cut > 0) blocks[seq_len(cut)] else list()
  rest <- if (cut < length(blocks)) blocks[(cut + 1):length(blocks)] else list()
  if (length(rest) > 0 && rest[[1]]$type == "entry") {
    sec <- rest[[1]]$section
    rest <- c(list(list(type = "heading", section = sec, height = CV_HEADING_HEIGHT_PT, cont = TRUE)), rest)
  }
  list(page1 = page1, rest = rest)
}

# Render a list of blocks (from column_blocks()/split_column()) as a
# markdown/HTML string -- headings become "# ..." lines, consecutive
# entries are grouped into one <table> each.
blocks_to_html <- function(blocks) {
  if (length(blocks) == 0) return("")
  out <- character(0)
  rows <- character(0)
  flush_table <- function() {
    if (length(rows) == 0) return(invisible())
    out[[length(out) + 1]] <<- paste0("<table><tbody>", paste(rows, collapse = ""), "</tbody></table>")
    rows <<- character(0)
  }
  for (b in blocks) {
    if (b$type == "heading") {
      flush_table()
      label <- cv_section_titles[[b$section]]
      if (isTRUE(b$cont)) label <- paste0(label, " (cont.)")
      out[[length(out) + 1]] <- paste0("\n\n# ", label, "\n\n")
    } else {
      rows[[length(rows) + 1]] <- glue::glue("<tr><td>{b$year}</td><td>{b$title}</td></tr>")
    }
  }
  flush_table()
  paste(out, collapse = "")
}

# Emit one balanced-columns row (pandoc fenced divs) pairing a left and
# right list of blocks, for cat()-ing in a results='asis' chunk.
render_balanced_block <- function(left_blocks, right_blocks) {
  paste0(
    "\n\n::: {.balanced-columns}\n\n",
    ":::::: {.balanced-col .left}\n\n", blocks_to_html(left_blocks), "\n\n::::::\n\n",
    ":::::: {.balanced-col .right}\n\n", blocks_to_html(right_blocks), "\n\n::::::\n\n",
    ":::\n\n"
  )
}

render_cv_publications <- function(view = "full_cv") {
  # Uses "**N.**" instead of markdown "N." list syntax, because pandoc
  # auto-increments real ordered lists and ignores our own numbering.
  #
  # Numbers count down from nrow(pubs) based on position in the list
  # (already sorted newest-first), not pub$index -- for the full list
  # this reproduces the same numbers as pub$index (1..N with no gaps),
  # but for a filtered view (e.g. the one-page CV's subset) it stays
  # continuous instead of skipping the numbers of excluded papers.
  pubs <- load_pubs(view)
  n <- nrow(pubs)
  if (n == 0) return("")
  lines <- vapply(seq_len(n), function(i) {
    pub <- pubs[i, ]
    glue::glue("**{n - i + 1}.** {format_pub_citation(pub)}")
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
