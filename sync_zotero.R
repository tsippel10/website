#!/usr/bin/env Rscript
# Sync the CV's publication bibliography from the local Zotero database.
#
# Reads ~/Zotero/zotero.sqlite (via a temp copy, so it works while Zotero is
# open) and writes Bibliography/cv_publications.bib containing:
#
#   * every journal article, book chapter, and report authored by T. Sippel, and
#   * anonymous / institution-authored reports the user contributed to but is not
#     personally credited on (matched by the whitelists below).
#
# Entries are de-duplicated by title, keeping the most complete record. CV.qmd
# reads the resulting .bib. Run this whenever Zotero changes:  Rscript sync_zotero.R
#
# R/tidyverse port of sync_zotero.py.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
})

HOME      <- path.expand("~")
ZOTERO_DB <- file.path(HOME, "Zotero", "zotero.sqlite")
# Resolve paths relative to this script, mirroring the Python __file__ logic.
script_dir <- tryCatch(
  dirname(normalizePath(sub("^--file=", "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]))),
  error = function(e) getwd()
)
if (is.na(script_dir) || length(script_dir) == 0) script_dir <- getwd()
OUT_BIB <- file.path(script_dir, "Bibliography", "cv_publications.bib")

AUTHOR_SURNAME <- "Sippel"
# Institution-authored reports to include even though Sippel isn't a named author
ANON_AUTHORS <- c("National Academies of Sciences", "Informal Offshore Wind Energy Group")
ANON_TITLES  <- c("National Assessment by the Nature Record")

# "Selected Publications" on the CV. The source of truth is a Zotero tag: add
# the tag below to an item in Zotero and it is flagged (keywords = {selected}).
SELECTED_TAG <- "cv-selected"
# Temporary seed of the currently highlighted publications, matched by a
# distinctive lower-case fragment of the title. This makes Selected Publications
# work before the Zotero tags are applied; once every item above is tagged in
# Zotero, this list can be emptied. The Zotero tag always takes precedence.
SELECTED_SEED <- c(
  "offshore renewable energy development on the west coast",
  "national assessment by the nature record",
  "oregon floating offshore wind energy roadmap",
  "post-release survival of shortfin mako sharks",
  "analysis of increasing the required vms ping rate",
  "searching for m: is there more information",
  "state-space surplus production model",
  "stock assessment of blue shark in the north pacific ocean using stock synthesis",
  "direct estimates of gear selectivity",
  "dynamic ocean management",
  "using movement data from electronic tags in fisheries stock assessment",
  "size and sex of shortfin mako sharks from us and japanese",
  "horizontal and vertical dynamics of swordfish",
  "back to the wild",
  "investigating behaviour and population dynamics of striped marlin",
  "near real time satellite tracking of striped marlin",
  "southern bluefin tuna electronic tagging results",
  "mesoscale movements in the short-tailed stingray",
  "movements and habitat utilization during a summer and autumn"
)

# Zotero item type -> BibTeX entry type
TYPE_MAP <- c(journalArticle = "article", bookSection = "incollection",
              report = "techreport", book = "book")
# Zotero field -> BibTeX field (order matters: earlier keys win when two map to
# the same BibTeX field, e.g. issue before reportNumber -> number)
FIELD_MAP <- c(
  publicationTitle = "journal", bookTitle = "booktitle", institution = "institution",
  publisher = "publisher", volume = "volume", issue = "number",
  reportNumber = "number", pages = "pages", place = "address", DOI = "doi"
)

# --- helpers ---------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

# Coalesce a missing/NA list-column cell to a default value.
or_empty <- function(x, default) {
  if (is.null(x) || (is.atomic(x) && length(x) == 1L && is.na(x))) default else x
}

# Look up a named field, defaulting to "" when absent.
fld <- function(fields, name) {
  if (name %in% names(fields)) fields[[name]] else ""
}

sanitize <- function(val) str_trim(str_replace_all(val, "[{}]", ""))

author_string <- function(authors) {
  parts <- pmap_chr(authors, function(first, last) {
    if (nzchar(first)) paste0(last, ", ", first) else paste0("{", last, "}")
  })
  paste(parts, collapse = " and ")
}

year_of <- function(fields) {
  m <- str_extract(fld(fields, "date"), "\\d{4}")
  if (is.na(m) || m == "0000") return(NA_character_)  # skip undated / placeholder
  m
}

is_included <- function(item) {
  typ <- item$type
  authors_flat <- str_c(item$authors$last, ", ", item$authors$first, collapse = " and ")
  if (is.na(authors_flat)) authors_flat <- ""
  title <- fld(item$fields, "title")
  sippel <- typ %in% c("journalArticle", "bookSection", "report") &&
    str_detect(str_to_lower(authors_flat), fixed(str_to_lower(AUTHOR_SURNAME)))
  anon <- typ %in% c("book", "report") && (
    any(str_detect(str_to_lower(authors_flat), fixed(str_to_lower(ANON_AUTHORS)))) ||
    any(str_detect(str_to_lower(title), fixed(str_to_lower(ANON_TITLES)))))
  isTRUE(sippel) || isTRUE(anon)
}

is_selected <- function(item) {
  tags_lower <- str_to_lower(item$tags)
  if (str_to_lower(SELECTED_TAG) %in% tags_lower) return(TRUE)
  title <- str_to_lower(fld(item$fields, "title"))
  any(str_detect(title, fixed(SELECTED_SEED)))
}

to_record <- function(item) {
  f <- item$fields
  yr <- year_of(f)
  if (is.na(yr)) return(NULL)
  rec <- list(`_type` = unname(TYPE_MAP[[item$type]]),
              title   = sanitize(fld(f, "title")),
              year    = yr)
  if (nrow(item$authors) > 0) rec$author <- author_string(item$authors)
  for (zf in names(FIELD_MAP)) {
    bf <- FIELD_MAP[[zf]]
    if (zf %in% names(f) && is.null(rec[[bf]])) {
      v <- sanitize(f[[zf]])
      if (nzchar(v) && !(v %in% c("-", "–"))) rec[[bf]] <- v
    }
  }
  if (is_selected(item)) rec$keywords <- "selected"
  rec
}

completeness <- function(rec) {
  keys <- c("author", "journal", "booktitle", "institution", "publisher",
            "volume", "number", "pages", "doi")
  sum(map_lgl(keys, ~ !is.null(rec[[.x]]) && nzchar(rec[[.x]])))
}

# --- load ------------------------------------------------------------------

load_items <- function(con) {
  items <- dbGetQuery(con, "
    SELECT i.itemID, it.typeName AS type, i.dateModified AS modified
    FROM items i
    JOIN itemTypes it ON it.itemTypeID = i.itemTypeID
    LEFT JOIN deletedItems di ON di.itemID = i.itemID
    WHERE di.itemID IS NULL
      AND it.typeName IN ('journalArticle','bookSection','report','book')
  ") |> as_tibble()
  if (nrow(items) == 0) return(items)
  ids <- paste(items$itemID, collapse = ",")

  item_data <- dbGetQuery(con, sprintf("
    SELECT d.itemID, f.fieldName, v.value
    FROM itemData d
    JOIN itemDataValues v ON v.valueID = d.valueID
    JOIN fields f ON f.fieldID = d.fieldID
    WHERE d.itemID IN (%s)", ids)) |> as_tibble()

  creators <- dbGetQuery(con, sprintf("
    SELECT ic.itemID, ic.orderIndex, c.firstName AS first, c.lastName AS last
    FROM itemCreators ic
    JOIN creators c ON c.creatorID = ic.creatorID
    JOIN creatorTypes ct ON ct.creatorTypeID = ic.creatorTypeID
    WHERE ic.itemID IN (%s) AND ct.creatorType = 'author'
    ORDER BY ic.itemID, ic.orderIndex", ids)) |> as_tibble()

  item_tags <- dbGetQuery(con, sprintf("
    SELECT it.itemID, t.name
    FROM itemTags it
    JOIN tags t ON t.tagID = it.tagID
    WHERE it.itemID IN (%s)", ids)) |> as_tibble()

  # Nest each item's fields / authors / tags into list-columns.
  fields_by_item <- item_data |>
    group_by(itemID) |>
    summarise(fields = list(set_names(value, fieldName)), .groups = "drop")

  authors_by_item <- creators |>
    arrange(itemID, orderIndex) |>
    mutate(first = str_trim(coalesce(first, "")),
           last  = str_trim(coalesce(last, ""))) |>
    group_by(itemID) |>
    summarise(authors = list(tibble(first = first, last = last)), .groups = "drop")

  tags_by_item <- item_tags |>
    mutate(name = str_trim(coalesce(name, ""))) |>
    group_by(itemID) |>
    summarise(tags = list(name), .groups = "drop")

  items |>
    left_join(fields_by_item, by = "itemID") |>
    left_join(authors_by_item, by = "itemID") |>
    left_join(tags_by_item, by = "itemID") |>
    mutate(
      fields  = map(fields,  or_empty, set_names(character(), character())),
      authors = map(authors, or_empty, tibble(first = character(), last = character())),
      tags    = map(tags,    or_empty, character())
    )
}

# --- main ------------------------------------------------------------------

main <- function() {
  tmp_path <- tempfile(fileext = ".sqlite")
  file.copy(ZOTERO_DB, tmp_path, overwrite = TRUE)
  con <- dbConnect(RSQLite::SQLite(), tmp_path)
  on.exit({ dbDisconnect(con); unlink(tmp_path) }, add = TRUE)
  items <- load_items(con)

  # Build records for included items.
  recs <- items |>
    pmap(function(itemID, type, modified, fields, authors, tags) {
      item <- list(type = type, modified = modified,
                   fields = fields, authors = authors, tags = tags)
      if (!is_included(item)) return(NULL)
      rec <- to_record(item)
      if (is.null(rec) || !nzchar(rec$title)) return(NULL)
      rec$`_modified` <- modified
      rec
    }) |>
    compact()

  # De-duplicate by normalized title; keep most complete, then most recently modified.
  better <- function(a, b) {
    ca <- completeness(a); cb <- completeness(b)
    if (ca != cb) return(ca > cb)
    a$`_modified` > b$`_modified`
  }
  best <- list()
  for (rec in recs) {
    key <- str_sub(str_replace_all(str_to_lower(rec$title), "[^a-z0-9]", ""), 1, 60)
    cur <- best[[key]]
    if (is.null(cur) || better(rec, cur)) best[[key]] <- rec
  }
  recs <- unname(best)
  # method = "radix" sorts strings in the C locale (bytewise), matching Python's
  # default code-point string comparison so tied-year entries order identically.
  recs <- recs[order(-as.integer(map_chr(recs, "year")), map_chr(recs, "title"),
                     method = "radix")]

  order_keys <- c("author", "title", "year", "journal", "booktitle", "institution",
                  "publisher", "volume", "number", "pages", "address", "doi", "keywords")
  lines <- c(
    "% Generated by sync_zotero.R from ~/Zotero/zotero.sqlite — do not edit by hand.",
    sprintf("%% %d publications.\n", length(recs))
  )
  for (i in seq_along(recs)) {
    rec <- recs[[i]]
    lines <- c(lines, sprintf("@%s{cv%d,", rec$`_type`, i))
    body <- character()
    for (k in order_keys) {
      v <- rec[[k]]
      if (!is.null(v) && nzchar(v)) {
        val <- if (k == "year") v else paste0("{", v, "}")
        body <- c(body, paste0("  ", k, " = ", val))
      }
    }
    lines <- c(lines, paste(body, collapse = ",\n"), "}\n")
  }

  dir.create(dirname(OUT_BIB), showWarnings = FALSE, recursive = TRUE)
  cat(paste(lines, collapse = "\n"), file = OUT_BIB)
  cat(sprintf("Wrote %d publications to %s\n", length(recs), OUT_BIB))
}

main()
