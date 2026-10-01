#!/usr/bin/env Rscript
# Postseason picks: fetch, score, and render.
#
# Reads everyone's picks and the actual results, scores each submission, and
# writes the two CSVs the website renders. Base R only, on purpose, so the
# GitHub Actions build never fails on a missing package.
#
# Usage:
#   Rscript playoffs/score.R                  refresh picks from the sheet, then re-score
#   Rscript playoffs/score.R picks.csv        score a specific picks CSV (testing / manual export)
#
# Environment:
#   PLAYOFF_SHEET_ID   Google Sheets file ID. When set, picks are re-downloaded
#                      from the sheet before scoring. When empty, the committed
#                      data/picks.csv is reused.
#   PLAYOFF_SHEET_GID  Sheet tab id to export (default 0).
#
# Inputs:  results.csv (you fill this in as series get played)
#          data/picks.csv (written by this script)
# Outputs: data/picks.csv, data/standings.csv

SERIES <- data.frame(
  key       = c("alds1", "alds2", "nlds1", "nlds2", "alcs", "nlcs", "ws"),
  label     = c("ALDS 1", "ALDS 2", "NLDS 1", "NLDS 2", "ALCS", "NLCS", "World Series"),
  min_games = c(3L, 3L, 3L, 3L, 4L, 4L, 4L),
  stringsAsFactors = FALSE
)

# Must stay in sync with SCORING in predictions.qmd.
SCORING <- list(winner = 2, length = 1, off_by_one = 0.5, sweep = 1)

game_cols <- function() paste0(SERIES$key, "_games")

find_file <- function(path) {
  cands <- c(file.path(script_dir(), path), path,
             file.path(".", path), file.path("playoffs", path), file.path("..", path))
  for (f in cands) if (file.exists(f)) return(f)
  stop("could not locate ", path, call. = FALSE)
}

script_dir <- function() {
  # When run with Rscript, "--file=" points at this script's real location, so
  # the pipeline works no matter what the working directory is. When sourced
  # from the .qmd that flag belongs to knitr, so verify before trusting it.
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) {
    d <- dirname(f[1])
    if (file.exists(file.path(d, "score.R"))) return(d)
  }
  for (d in c(".", "playoffs", "..")) {
    if (file.exists(file.path(d, "score.R"))) return(d)
  }
  "."
}

# One series is worth 2 (winner) + 1 (length) + 1 (sweep) = 4 points.
max_per_series <- function() SCORING$winner + SCORING$length + SCORING$sweep

read_picks_csv <- function(path) {
  raw <- utils::read.csv(path, colClasses = "character", check.names = FALSE,
                         stringsAsFactors = FALSE, na.strings = character(0))
  nm <- tolower(names(raw))
  grab <- function(key, games) {
    # Accepts both the sheet layout ("ALDS 1", "ALDS 1 Games") and the
    # normalized layout this script writes ("alds1", "alds1_games").
    direct <- tolower(if (games) paste0(key, "_games") else key)
    j <- match(direct, nm)
    if (!is.na(j)) return(raw[[j]])
    lab <- tolower(SERIES$label[SERIES$key == key])
    j <- if (games) match(paste0(lab, " games"), nm) else which(nm == lab)
    if (length(j) == 0L) return(rep(NA_character_, nrow(raw)))
    raw[[j[1]]]
  }
  blank <- function(x) { x <- trimws(x); x[!nzchar(x)] <- NA_character_; x }

  name_col <- match("name", nm)
  ts_col <- match("timestamp", nm)
  out <- list(name = if (!is.na(name_col)) blank(raw[[name_col]]) else rep(NA_character_, nrow(raw)))
  if (!is.na(ts_col)) out$timestamp <- raw[[ts_col]]
  for (key in SERIES$key) {
    out[[key]] <- blank(grab(key, FALSE))
    out[[paste0(key, "_games")]] <- blank(grab(key, TRUE))
  }
  as.data.frame(out, stringsAsFactors = FALSE)
}

# A person may submit more than once; the most recent submission counts.
dedupe_picks <- function(df) {
  keep <- !is.na(df$name)
  df <- df[keep, , drop = FALSE]
  if (!nrow(df)) return(df)
  ts <- suppressWarnings(as.POSIXct(df$timestamp, tz = "UTC", format = "%m/%d/%Y %H:%M:%S"))
  if (all(is.na(ts))) ts <- seq_len(nrow(df))
  df <- df[order(df$name, ts, na.last = TRUE), , drop = FALSE]
  df[!duplicated(df$name, fromLast = TRUE), , drop = FALSE]
}

read_results <- function(path) {
  res <- data.frame(key = SERIES$key, winner = NA_character_,
                    games = NA_integer_, stringsAsFactors = FALSE)
  if (!file.exists(path)) return(res)
  raw <- utils::read.csv(path, colClasses = "character", check.names = FALSE,
                         stringsAsFactors = FALSE, na.strings = character(0))
  for (i in seq_len(nrow(res))) {
    hit <- which(trimws(raw$key) == res$key[i])
    if (length(hit) == 0L) next
    w <- trimws(raw$winner[hit[1]])
    g <- suppressWarnings(as.integer(trimws(raw$games[hit[1]])))
    if (nzchar(w)) res$winner[i] <- w
    if (!is.na(g)) res$games[i] <- g
  }
  res
}

actual_for <- function(results, key) {
  i <- match(key, results$key)
  w <- results$winner[i]
  g <- results$games[i]
  if (is.na(w) || !nzchar(w) || is.na(g)) return(NULL)
  list(winner = w, games = g)
}

# Series length points do not depend on the winner pick, so they are awarded
# even when the winner is wrong.
score_all <- function(picks, results) {
  tab <- data.frame(name = picks$name, score = 0, winners = 0L, lengths = 0L,
                    sweeps = 0L, scored = 0L, possible = 0,
                    stringsAsFactors = FALSE)
  for (i in seq_len(nrow(picks))) {
    for (key in SERIES$key) {
      act <- actual_for(results, key)
      if (is.null(act)) next
      j <- match(key, SERIES$key)
      tab$scored[i] <- tab$scored[i] + 1L
      tab$possible[i] <- tab$possible[i] + max_per_series()

      pw <- picks[[key]][i]
      if (!is.na(pw) && identical(pw, act$winner)) {
        tab$winners[i] <- tab$winners[i] + 1L
        tab$score[i] <- tab$score[i] + SCORING$winner
      }
      pg <- suppressWarnings(as.integer(picks[[paste0(key, "_games")]][i]))
      if (is.na(pg)) next
      if (pg == act$games) {
        tab$lengths[i] <- tab$lengths[i] + 1L
        tab$score[i] <- tab$score[i] + SCORING$length
        if (act$games == SERIES$min_games[j]) {
          tab$sweeps[i] <- tab$sweeps[i] + 1L
          tab$score[i] <- tab$score[i] + SCORING$sweep
        }
      } else if (abs(pg - act$games) == 1L) {
        tab$score[i] <- tab$score[i] + SCORING$off_by_one
      }
    }
  }
  tab[order(-tab$score, -tab$winners, -tab$lengths, tab$name), , drop = FALSE]
}

# ---- view models -----------------------------------------------------------

esc <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

results_pending <- function(results) {
  keys <- vapply(SERIES$key, function(k) !is.null(actual_for(results, k)), logical(1))
  list(n = sum(keys), total = length(SERIES$key), complete = all(keys))
}

standings_html <- function(tab, results) {
  if (is.null(tab) || !nrow(tab)) {
    return("<p class=\"pf-note\">No picks have been submitted yet. Check back once the form has been filled in.</p>")
  }
  pend <- results_pending(results)
  head <- sprintf("<p class=\"pf-note\">%d of %d series scored so far. Each completed series is worth %d points, so %d points are still up for grabs.</p>",
                  pend$n, pend$total, max_per_series(), (pend$total - pend$n) * max_per_series())
  if (!pend$complete) head <- paste0(head, "<span class=\"pf-tag\">In progress</span>")

  rows <- vapply(seq_len(nrow(tab)), function(i) {
    sprintf(paste0("<tr%s><td class=\"pf-rank\">%d</td><td>%s</td><td class=\"pf-score\">%s</td>",
                   "<td>%d / %d</td><td>%d / %d</td><td>%d / %d</td></tr>"),
            if (i == 1L) " class=\"pf-leader\"" else "",
            i, esc(tab$name[i]), format(tab$score[i], trim = TRUE),
            tab$winners[i], pend$n, tab$lengths[i], pend$n, tab$sweeps[i], pend$n)
  }, character(1))

  paste0(head,
    "<div class=\"pf-scroll\"><table class=\"pf-table2\"><thead><tr>",
    "<th>#</th><th>Name</th><th>Points</th><th>Winners</th><th>Lengths</th><th>Sweeps</th>",
    "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>")
}

picks_html <- function(picks, tab, results) {
  if (is.null(picks) || !nrow(picks)) {
    return("<p class=\"pf-note\">Nobody's picks are loaded yet.</p>")
  }
  # Order by standing when scored, alphabetically otherwise.
  ord <- order(picks$name)
  if (!is.null(tab) && nrow(tab)) {
    idx <- match(picks$name, tab$name)
    if (all(!is.na(idx))) ord <- idx
  }
  picks <- picks[ord, , drop = FALSE]

  head_txt <- vapply(SERIES$key, function(k) {
    act <- actual_for(results, k)
    esc(if (is.null(act)) SERIES$label[SERIES$key == k] else paste0(SERIES$label[SERIES$key == k], " (", act$winner, " in ", act$games, ")"))
  }, character(1))

  rows <- vapply(seq_len(nrow(picks)), function(i) {
    cells <- vapply(SERIES$key, function(key) {
      act <- actual_for(results, key)
      w <- picks[[key]][i]
      g <- suppressWarnings(as.integer(picks[[paste0(key, "_games")]][i]))
      j <- match(key, SERIES$key)
      wc <- if (is.null(act) || is.na(w)) "pf-unknown" else if (identical(w, act$winner)) "pf-ok" else "pf-bad"
      gc <- if (is.null(act) || is.na(g)) "pf-unknown"
             else if (g == act$games) "pf-ok"
             else if (abs(g - act$games) == 1L) "pf-near" else "pf-bad"
      paste0("<span class=\"pf-pill ", wc, "\">", esc(if (is.na(w)) "TBD" else w), "</span>",
             "<span class=\"pf-pill pf-g ", gc, "\">", if (is.na(g)) "no g" else paste0(g, " g"), "</span>")
    }, character(1))
    paste0("<tr><td class=\"pf-who\">", esc(picks$name[i]), "</td>",
           paste0("<td>", cells, "</td>", collapse = ""), "</tr>")
  }, character(1))

  paste0("<div class=\"pf-scroll\"><table class=\"pf-table2 pf-matrix\"><thead><tr><th>Name</th>",
         paste0("<th>", head_txt, "</th>", collapse = ""), "</tr></thead><tbody>",
         paste(rows, collapse = ""), "</tbody></table></div>",
         "<p class=\"pf-note\">Green means the winner or length is right, ",
         "<span class=\"pf-near-key\">amber</span> means the length was off by one game, ",
         "red means it was off by two or more.</p>")
}

# ---- pipeline --------------------------------------------------------------

fetch_sheet_csv <- function() {
  id <- Sys.getenv("PLAYOFF_SHEET_ID", "")
  if (!nzchar(id)) return(NULL)
  gid <- Sys.getenv("PLAYOFF_SHEET_GID", "0")
  url <- sprintf("https://docs.google.com/spreadsheets/d/%s/export?format=csv&gid=%s", id, gid)
  tmp <- tempfile(fileext = ".csv")
  ok <- tryCatch({
    suppressWarnings(utils::download.file(url, tmp, mode = "wb", quiet = TRUE))
    TRUE
  }, error = function(e) FALSE)
  if (!ok || !file.exists(tmp) || file.size(tmp) < 1) {
    message("Could not download picks from the sheet; keeping the committed copy.")
    return(NULL)
  }
  tmp
}

run <- function(argv = character(0)) {
  dir <- script_dir()
  data_dir <- file.path(dir, "data")
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  target <- file.path(data_dir, "picks.csv")

  if (length(argv)) {
    source_csv <- argv[1]
  } else {
    downloaded <- fetch_sheet_csv()
    if (!is.null(downloaded)) source_csv <- downloaded else source_csv <- target
  }

if (!file.exists(source_csv)) {
    message("No picks CSV at ", source_csv, "; writing an empty one.")
    picks <- data.frame(name = character(0), timestamp = character(0), stringsAsFactors = FALSE)
  } else {
    picks <- dedupe_picks(read_picks_csv(source_csv))
  }
  out <- data.frame(name = picks$name, stringsAsFactors = FALSE)
  if ("timestamp" %in% names(picks)) out$timestamp <- picks$timestamp
  for (key in SERIES$key) {
    out[[key]] <- picks[[key]]
    out[[paste0(key, "_games")]] <- picks[[paste0(key, "_games")]]
  }
  utils::write.csv(out, target, row.names = FALSE, na = "")
  message("Wrote ", nrow(out), " pick row(s) to ", target)

  results <- read_results(find_file("results.csv"))
  tab <- if (nrow(picks)) score_all(picks, results) else data.frame()
  utils::write.csv(tab, file.path(data_dir, "standings.csv"), row.names = FALSE, na = "")

  pend <- results_pending(results)
  message("Scored ", nrow(tab), " submission(s) against ", pend$n, " of ", pend$total, " completed series.")
  invisible(list(picks = picks, standings = tab, results = results))
}

if (sys.nframe() == 0L) run(commandArgs(trailingOnly = TRUE))