# Wrapper around gtrends() with exponential backoff retry on rate limiting (HTTP 429).
# Google Trends rate-limits automated access aggressively; this retries with increasing
# delays before giving up. Pass all gtrends() arguments normally.
gtrends_retry <- function(..., max_attempts = 5, wait_base = 30) {
  for (attempt in 1:max_attempts) {
    result <- tryCatch({
      gtrendsR::gtrends(...)
    }, error = function(e) {
      if (grepl("429", e$message)) {
        wait <- wait_base * (2 ^ (attempt - 1))
        message(sprintf("Rate limited (attempt %d/%d). Waiting %d seconds before retry...",
                        attempt, max_attempts, wait))
        Sys.sleep(wait)
        NULL
      } else {
        stop(e)
      }
    })
    if (!is.null(result)) return(result)
  }
  stop("Google Trends API still rate limited after ", max_attempts, " attempts.")
}

# Our package function
package_check <- function(need = character()) {
  have <- need %in% rownames(installed.packages())
  
  if (any(!have)) {
    install.packages(need[!have])
  }
  
  invisible(
    lapply(need, function(pkg) {
      library(pkg, character.only = TRUE)
    })
  )
}

# Fetch daily page view counts for a Wikipedia article via the Wikimedia REST API.
# Returns a data frame with one row per day.
get_article_views <- function(article, project = "en.wikipedia",
                               granularity = "daily",
                               start = format(Sys.Date() - 30, "%Y%m%d"),
                               end   = format(Sys.Date() - 1,  "%Y%m%d")) {
  url <- paste0(
    "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",
    project, "/all-access/user/",
    URLencode(article, reserved = TRUE), "/",
    granularity, "/",
    paste0(start, "00"), "/", paste0(end, "00")  # API requires YYYYMMDDHH
  )
  raw <- tryCatch(
    jsonlite::fromJSON(url, simplifyVector = FALSE),
    error = function(e) stop("API request failed for '", article, "' on ", project, ": ", e$message)
  )
  items <- raw$items
  data.frame(
    date    = as.Date(substr(sapply(items, `[[`, "timestamp"), 1, 8), format = "%Y%m%d"),
    views   = as.integer(sapply(items, `[[`, "views")),
    article = article,
    project = project,
    stringsAsFactors = FALSE
  )
}

# Fetch daily edit counts for a Wikipedia article via the MediaWiki Action API.
# Uses the revision history endpoint with automatic pagination.
# start/end are YYYY-MM-DD strings.
get_article_edits <- function(article, project = "en.wikipedia",
                               start = format(Sys.Date() - 30, "%Y-%m-%d"),
                               end   = format(Sys.Date() - 1,  "%Y-%m-%d")) {
  base_url <- paste0("https://", sub("wikipedia$", "wikipedia.org", project), "/w/api.php")

  all_timestamps <- c()
  continue_param <- NULL

  repeat {
    params <- list(action  = "query",
                   prop    = "revisions",
                   titles  = article,
                   rvlimit = "max",
                   rvprop  = "timestamp",
                   rvstart = paste0(start, "T00:00:00Z"),
                   rvend   = paste0(end,   "T23:59:59Z"),
                   rvdir   = "newer",
                   format  = "json")
    if (!is.null(continue_param)) params$rvcontinue <- continue_param

    query_str <- paste(names(params), unlist(params), sep = "=", collapse = "&")
    raw <- tryCatch(
      jsonlite::fromJSON(paste0(base_url, "?", query_str), simplifyVector = FALSE),
      error = function(e) stop("API request failed for '", article, "' on ", project, ": ", e$message)
    )

    page <- raw$query$pages[[1]]
    if (!is.null(page$revisions)) {
      all_timestamps <- c(all_timestamps, sapply(page$revisions, `[[`, "timestamp"))
    }

    if (!is.null(raw$`continue`$rvcontinue)) {
      continue_param <- raw$`continue`$rvcontinue
    } else break
  }

  if (length(all_timestamps) == 0) {
    return(data.frame(date    = as.Date(character()),
                      edits   = integer(),
                      article = character(),
                      project = character()))
  }

  dates  <- as.Date(substr(all_timestamps, 1, 10))
  counts <- table(dates)
  data.frame(date    = as.Date(names(counts)),
             edits   = as.integer(counts),
             article = article,
             project = project,
             stringsAsFactors = FALSE)
}

# Fetch total daily pageviews for an entire Wikipedia edition via the Wikimedia REST API.
# This is the aggregate (project-level) equivalent of get_article_views().
# Returns a data frame with one row per day; sum $views for a total over a period.
get_project_views <- function(project = "en.wikipedia",
                               granularity = "daily",
                               start = format(Sys.Date() - 30, "%Y%m%d"),
                               end   = format(Sys.Date() - 1,  "%Y%m%d")) {
  url <- paste0(
    "https://wikimedia.org/api/rest_v1/metrics/pageviews/aggregate/",
    project, "/all-access/user/",
    granularity, "/",
    paste0(start, "00"), "/", paste0(end, "00")  # API requires YYYYMMDDHH
  )
  raw <- tryCatch(
    jsonlite::fromJSON(url, simplifyVector = FALSE),
    error = function(e) stop("API request failed for project '", project, "': ", e$message)
  )
  items <- raw$items
  data.frame(
    date    = as.Date(substr(sapply(items, `[[`, "timestamp"), 1, 8), format = "%Y%m%d"),
    views   = as.integer(sapply(items, `[[`, "views")),
    project = project,
    stringsAsFactors = FALSE
  )
}

# We build a re-usable function to download data from a particular site, and store it on our local drive
file_grabber <- function(file_name_dl, file_path, file_name_final = NULL, url, compressed = TRUE) {
  
  # Ensure file_path ends with a slash
  if (!grepl("[/\\]$", file_path)) {
    file_path <- paste0(file_path, "/")
  }
  
  # Download only if file does not already exist
  if (!file.exists(paste0(file_path, file_name_dl))) {
    download.file(url, paste0(file_path, file_name_dl), mode = "wb")
    print("File downloaded.")
  } else {
    print("File already downloaded.")
  }
  
  # Unzip only if needed
  if (compressed && !is.null(file_name_final) &&
      !file.exists(paste0(file_path, "shapefiles/", file_name_final))) {
    unzip(paste0(file_path, file_name_dl), exdir = paste0(file_path, "shapefiles"))
    print("File unzipped.")
  } else {
    print("File already unzipped or no need for unzipping.")
  }
}