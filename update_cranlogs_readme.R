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

format_count <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_age <- function(from, to = Sys.Date()) {
  days <- as.integer(to - from)

  if (is.na(days)) return("unknown")
  if (days < 30) return(paste(days, "days"))

  months <- floor(days / 30.4375)
  if (months < 12) {
    if (months == 1) return("1 month")
    return(paste(months, "months"))
  }

  years <- floor(months / 12)
  if (years == 1) return("1 year")
  paste(years, "years")
}

make_badge <- function(label, message, color = "blue") {
  label <- URLencode(as.character(label), reserved = TRUE)
  message <- URLencode(as.character(message), reserved = TRUE)

  sprintf(
    '<img src="https://img.shields.io/badge/%s-%s-%s?style=flat" alt="%s: %s">',
    label,
    message,
    color,
    label,
    message
  )
}

get_cran_version <- function(pkg) {
  desc_url <- sprintf("https://cran.r-project.org/web/packages/%s/DESCRIPTION", pkg)
  desc <- read.dcf(url(desc_url))
  desc[1, "Version"]
}

get_number_of_versions <- function(pkg) {
  archive_url <- sprintf("https://cran.r-project.org/src/contrib/Archive/%s/", pkg)
  archive_page <- tryCatch(readLines(archive_url, warn = FALSE), error = function(e) character())

  pattern <- sprintf("%s_[^\"<>]+[.]tar[.]gz", pkg)
  matches <- regmatches(archive_page, gregexpr(pattern, archive_page))
  archive_versions <- unique(unlist(matches))
  archive_count <- length(archive_versions)

  archive_count + 1L
}

version <- get_cran_version(pkg)
version_count <- get_number_of_versions(pkg)
version_updates <- version_count
version_word <- if (version_count == 1L) "one version" else paste(version_count, "versions")

last_day <- get_downloads(packages = pkg, when = "last-day")
last_week <- get_downloads(packages = pkg, when = "last-week")
last_month <- get_downloads(packages = pkg, when = "last-month")
total <- get_downloads(packages = pkg, from = release_date, to = Sys.Date())

title_html <- paste(
  "Pattern Identification",
  "for Dyadic Sequences",
  "Using Transition Matrices",
  sep = "<br>"
)

badges <- c(
  make_badge("version", version, "blue"),
  make_badge("version updates", version_updates, "brightgreen"),
  make_badge("pub age", format_age(release_date), "red"),
  make_badge("rPkgNetStats", version_word, "orange"),
  make_badge("downloads", paste0(format_count(last_day$count), "/day"), "brightgreen"),
  make_badge("downloads", paste0(format_count(last_week$count), "/week"), "brightgreen"),
  make_badge("downloads", paste0(format_count(last_month$count), "/month"), "blue"),
  make_badge("downloads", format_count(total$count), "blue")
)

block <- c(
  "<!-- cranlogs:start -->",
  "",
  '<table>',
  '  <tr>',
  sprintf('    <td style="vertical-align: middle; padding-right: 14px;">%s</td>', title_html),
  sprintf('    <td>%s</td>', paste(badges, collapse = "<br>\n")),
  '  </tr>',
  '</table>',
  "",
  "<sub>Download counts are recorded from the RStudio/Posit CRAN mirror via <code>cranlogs</code>.</sub>",
  "",
  sprintf("<!-- Last automatic update: %s -->", Sys.Date()),
  "",
  "<!-- cranlogs:end -->"
)

readme <- readLines(readme_file, warn = FALSE)

start <- grep("^<!-- cranlogs:start -->$", readme)
end <- grep("^<!-- cranlogs:end -->$", readme)

if (length(start) != 1 || length(end) != 1 || start >= end) {
  stop("README.md must contain exactly one valid cranlogs marker block.")
}

before <- if (start > 1) readme[1:(start - 1)] else character()
after <- if (end < length(readme)) readme[(end + 1):length(readme)] else character()

new_readme <- c(before, block, after)

writeLines(new_readme, readme_file)
