pkg <- "dyadicMarkov"
release_date <- as.Date("2026-03-16")
readme_file <- "README.md"

if (!requireNamespace("cranlogs", quietly = TRUE)) {
  install.packages("cranlogs", repos = "https://cloud.r-project.org")
}

get_downloads <- function(...) {
  x <- cranlogs::cran_downloads(...)
  list(
    count = sum(x$count, na.rm = TRUE),
    from = min(x$date),
    to = max(x$date)
  )
}

last_day <- get_downloads(packages = pkg, when = "last-day")
last_week <- get_downloads(packages = pkg, when = "last-week")
last_month <- get_downloads(packages = pkg, when = "last-month")
total <- get_downloads(packages = pkg, from = release_date, to = Sys.Date())

format_count <- function(x) format(x, big.mark = ",", scientific = FALSE)

block <- c(
  "<!-- cranlogs:start -->",
  "",
  "## CRAN download statistics",
  "",
  "Download counts are recorded download events from the RStudio/Posit CRAN mirror via `cranlogs`; they are not unique users.",
  "",
  "| Period | Downloads |",
  "|---|---:|",
  sprintf("| Latest available day (%s) | %s |", last_day$to, format_count(last_day$count)),
  sprintf("| Last available week (%s to %s) | %s |", last_week$from, last_week$to, format_count(last_week$count)),
  sprintf("| Last available month (%s to %s) | %s |", last_month$from, last_month$to, format_count(last_month$count)),
  sprintf("| Since CRAN release (%s to %s) | %s |", release_date, total$to, format_count(total$count)),
  "",
  sprintf("_Last automatic update: %s._", Sys.Date()),
  "",
  "<!-- cranlogs:end -->"
)

readme <- readLines(readme_file, warn = FALSE)

start <- grep("^<!-- cranlogs:start -->$", readme)
end <- grep("^<!-- cranlogs:end -->$", readme)

if (length(start) != 1 || length(end) != 1 || start >= end) {
  stop("README.md must contain exactly one valid cranlogs marker block.")
}

new_readme <- c(
  readme[seq_len(start - 1)],
  block,
  readme[(end + 1):length(readme)]
)

writeLines(new_readme, readme_file)
