# Fetches Google Scholar metrics and caches them to content/scholar_*.csv.
# Google Scholar rate-limits/blocks scripted requests, so the site reads
# from these cached files rather than hitting Scholar on every render.
# Re-run this script manually every so often to refresh the numbers:
#   Rscript refresh_scholar_data.R

library(scholar)
library(readr)
library(dplyr)

id <- yaml::read_yaml("content/cv_meta.yml")$scholar_id

profile <- get_profile(id)
readr::write_csv(
  tibble::tibble(
    name = profile$name,
    affiliation = profile$affiliation,
    total_cites = profile$total_cites,
    h_index = profile$h_index,
    i10_index = profile$i10_index,
    fetched_at = as.character(Sys.time())
  ),
  "content/scholar_profile.csv"
)

citation_history <- get_citation_history(id)
readr::write_csv(citation_history, "content/scholar_citation_history.csv")

pubs <- get_publications(id) |>
  select(title, author, journal, year, cites, pubid) |>
  arrange(desc(cites))
readr::write_csv(pubs, "content/scholar_pubs.csv")

cat("Scholar data refreshed:\n")
cat(" - total citations:", profile$total_cites, "\n")
cat(" - h-index:", profile$h_index, "\n")
cat(" - i10-index:", profile$i10_index, "\n")
