fire_csv_url <- function(date) {
  paste0(
    "https://dataserver-coids.inpe.br/queimadas/queimadas/focos/",
    "csv/diario/America_Sul/focos_diario_",
    format(as.Date(date), "%Y%m%d"),
    ".csv"
  )
}

fire_parse_csv <- function(content) {
  text <- if (is.raw(content)) rawToChar(content) else as.character(content)
  connection <- textConnection(enc2utf8(text), open = "r", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  fires <- utils::read.csv(
    connection,
    stringsAsFactors = FALSE,
    strip.white = TRUE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
  names(fires) <- trimws(names(fires))
  required <- c("id", "lat", "lon", "data_hora_gmt", "satelite")
  missing <- setdiff(required, names(fires))
  if (length(missing)) {
    stop("CSV do BDQueimadas sem coluna(s): ", paste(missing, collapse = ", "))
  }
  fires[required]
}

fire_download_day <- function(date) {
  url <- fire_csv_url(date)
  handle <- curl::new_handle(connecttimeout = 8, timeout = 30)
  response <- curl::curl_fetch_memory(url, handle = handle)
  if (response$status_code != 200L) {
    stop("BDQueimadas respondeu HTTP ", response$status_code, " para ", basename(url))
  }
  fire_parse_csv(response$content)
}

fire_empty_data <- function() {
  data.frame(
    id = character(), lat = numeric(), lon = numeric(),
    data_hora_gmt = character(), satelite = character(),
    stringsAsFactors = FALSE
  )
}

consolidate_fire_detections <- function(fires) {
  if (is.null(fires) || !nrow(fires)) return(fires)
  required <- c("lat", "lon", "data_hora_gmt")
  if (!all(required %in% names(fires))) return(fires)
  observed_at <- suppressWarnings(as.POSIXct(
    as.character(fires$data_hora_gmt), tz = "UTC",
    tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OSZ")
  ))
  location <- paste(
    sprintf("%.6f", as.numeric(fires$lat)),
    sprintf("%.6f", as.numeric(fires$lon)),
    sep = "|"
  )
  ordering <- order(location, observed_at, na.last = TRUE)
  fires <- fires[ordering, , drop = FALSE]
  location <- location[ordering]
  observed_at <- observed_at[ordering]
  keep <- !duplicated(location, fromLast = TRUE)
  fires <- fires[keep, , drop = FALSE]
  observed_at <- observed_at[keep]
  fires <- fires[order(observed_at, na.last = TRUE), , drop = FALSE]
  rownames(fires) <- NULL
  fires
}

fire_snapshot <- function(fires, updated, error = NULL, source = "inpe") {
  observed_at <- if (nrow(fires)) {
    suppressWarnings(as.POSIXct(
      as.character(fires$data_hora_gmt), tz = "UTC",
      tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OSZ")
    ))
  } else as.POSIXct(character(), tz = "UTC")
  latest <- if (length(observed_at) && any(!is.na(observed_at))) {
    max(observed_at, na.rm = TRUE)
  } else as.POSIXct(NA, tz = "UTC")
  list(
    fires = fires,
    latest = latest,
    updated = as.POSIXct(updated, origin = "1970-01-01", tz = "UTC"),
    error = error,
    source = source
  )
}

fire_refresh_payload <- function(now = Sys.time()) {
  now <- as.POSIXct(now, origin = "1970-01-01", tz = "UTC")
  today <- as.Date(format(now, "%Y-%m-%d", tz = "UTC"))
  dates <- seq(today - 1, today, by = "day")
  results <- lapply(dates, function(date) {
    tryCatch(
      list(data = fire_download_day(date), error = NULL),
      error = function(error) list(data = NULL, error = conditionMessage(error))
    )
  })
  available <- lapply(results, `[[`, "data")
  available <- available[!vapply(available, is.null, logical(1))]
  if (!length(available)) {
    errors <- unique(unlist(lapply(results, `[[`, "error"), use.names = FALSE))
    stop(paste(errors[nzchar(errors)], collapse = "; "))
  }
  fires <- do.call(rbind, available)
  rownames(fires) <- NULL
  fires <- normalize_fire_data(fires)
  fires <- consolidate_fire_detections(fires)
  fire_snapshot(fires, updated = now)
}

create_fire_store <- function(fallback = NULL, refresh_seconds = NULL) {
  if (is.null(refresh_seconds)) {
    refresh_minutes <- suppressWarnings(as.numeric(
      Sys.getenv("ALERTAR_FIRE_REFRESH_MINUTES", unset = "10")
    ))
    if (!is.finite(refresh_minutes) || refresh_minutes <= 0) refresh_minutes <- 10
    refresh_seconds <- refresh_minutes * 60
  }
  refresh_seconds <- max(60, as.numeric(refresh_seconds))
  fallback <- if (is.null(fallback)) fire_empty_data() else fallback
  if (!"satelite" %in% names(fallback)) fallback$satelite <- rep("AQUA", nrow(fallback))
  fallback <- normalize_fire_data(fallback)
  fallback <- consolidate_fire_detections(fallback)
  snapshot <- fire_snapshot(fallback, updated = Sys.time(), source = "cache")
  last_attempt <- as.POSIXct(NA, tz = "UTC")
  inflight <- NULL

  recently_attempted <- function(now) {
    !is.na(last_attempt) &&
      as.numeric(difftime(now, last_attempt, units = "secs")) < refresh_seconds
  }
  apply_payload <- function(payload) {
    snapshot <<- payload
    snapshot
  }
  failed_snapshot <- function(error, now) {
    snapshot$error <<- conditionMessage(error)
    snapshot$updated <<- now
    snapshot
  }
  refresh <- function(force = FALSE) {
    now <- as.POSIXct(Sys.time(), tz = "UTC")
    if (!force && recently_attempted(now)) return(snapshot)
    last_attempt <<- now
    tryCatch(
      apply_payload(fire_refresh_payload(now)),
      error = function(error) failed_snapshot(error, now)
    )
  }
  refresh_async <- function(force = FALSE) {
    now <- as.POSIXct(Sys.time(), tz = "UTC")
    if (!is.null(inflight)) return(inflight)
    if (!force && recently_attempted(now)) return(promises::promise_resolve(snapshot))
    last_attempt <<- now
    work <- promises::future_promise(fire_refresh_payload(now), seed = TRUE)
    inflight <<- promises::then(
      work,
      onFulfilled = function(payload) {
        inflight <<- NULL
        apply_payload(payload)
      },
      onRejected = function(error) {
        inflight <<- NULL
        failed_snapshot(error, now)
      }
    )
    inflight
  }

  list(
    refresh = refresh,
    refresh_async = refresh_async,
    snapshot = function() snapshot,
    refresh_seconds = refresh_seconds,
    close = function() invisible(NULL)
  )
}
