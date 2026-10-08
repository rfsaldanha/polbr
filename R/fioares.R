# Read-only consumer of arapi schema 3 (ICICT / FioAres).
fioares_config <- function() {
  # A dedicated FioAres connection must not inherit another project's PGHOST
  # or PGPASSWORD from an existing interactive R session.
  scoped <- any(nzchar(Sys.getenv(paste0("FIOARES_", c(
    "PGHOST", "PGPORT", "PGDATABASE", "PGUSER", "PGPASSWORD", "PGPASSFILE", "PGSERVICE", "PGSSLMODE"
  )))))
  setting <- function(name, default = "") {
    Sys.getenv(if (scoped) paste0("FIOARES_", name) else name, default)
  }
  positive <- function(name, default, minimum) {
    value <- suppressWarnings(as.numeric(Sys.getenv(name, as.character(default))))
    if (length(value) != 1L || !is.finite(value) || value < minimum) default else value
  }
  connection <- list(
    host = setting("PGHOST", "psql.icict.fiocruz.br"),
    port = setting("PGPORT", "5432"), dbname = setting("PGDATABASE", "estacoes_fioares"),
    user = setting("PGUSER"), password = setting("PGPASSWORD"),
    passfile = setting("PGPASSFILE"), sslmode = setting("PGSSLMODE", "require")
  )
  service <- setting("PGSERVICE")
  if (nzchar(service)) {
    # Let libpq resolve the service profile, including its host/database/user.
    connection <- list(service = service)
  }
  connection <- connection[vapply(connection, nzchar, logical(1))]
  list(connection = connection, scoped = scoped,
       configured = nzchar(service) || nzchar(setting("PGUSER")),
       refresh_seconds = positive("FIOARES_REFRESH_SECONDS", 300, 30),
       stale_hours = positive("FIOARES_STALE_HOURS", 3, 1),
       max_days = 31L)
}

fioares_connect <- function(config) {
  if (isTRUE(config$scoped)) {
    # libpq also reads PG* directly, even when RPostgres arguments omit them.
    keys <- c("PGHOST", "PGHOSTADDR", "PGPORT", "PGDATABASE", "PGUSER",
              "PGPASSWORD", "PGPASSFILE", "PGSSLMODE", "PGSERVICE")
    previous <- Sys.getenv(keys, unset = NA_character_)
    on.exit({
      Sys.unsetenv(keys)
      restored <- previous[!is.na(previous)]
      if (length(restored)) do.call(Sys.setenv, as.list(restored))
    }, add = TRUE)
    Sys.unsetenv(keys)
  }
  do.call(DBI::dbConnect, c(list(drv = RPostgres::Postgres()), config$connection,
    list(bigint = "numeric", connect_timeout = 8,
         options = "-c default_transaction_read_only=on -c statement_timeout=15000 -c timezone=UTC")))
}

fioares_with_publication <- function(config, fun) {
  con <- fioares_connect(config)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbBegin(con)
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)
  DBI::dbExecute(con, "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ, READ ONLY")
  meta <- DBI::dbGetQuery(con, paste(
    "SELECT key, value FROM public.meta WHERE key IN",
    "('schema_version', 'published_ms', 'processing_state', 'quality_profile')"))
  meta <- stats::setNames(as.list(meta$value), meta$key)
  if (!identical(meta$schema_version, "3") ||
      is.null(meta$published_ms) || !is.finite(suppressWarnings(as.numeric(meta$published_ms))) ||
      !identical(meta$processing_state, "ready") ||
      is.null(meta$quality_profile) ||
      nrow(DBI::dbGetQuery(con, "SELECT dirty_id FROM public.dirty LIMIT 1"))) {
    stop("FioAres publication unavailable")
  }
  fun(con, meta)
}

fioares_read_stations <- function(con, meta, now = Sys.time()) {
  rows <- DBI::dbGetQuery(con, paste(
    "SELECT s.station_id, s.label, s.metadata_json, latest.ts_ms,",
    "i.iqar, i.parameter, i.classification, pm.value AS pm25, pm.valid AS pm25_valid,",
    "bounds.first_ms, bounds.last_ms FROM public.stations s",
    "LEFT JOIN LATERAL (SELECT MAX(ts_ms) AS ts_ms FROM public.quality",
    "WHERE station_id = s.station_id AND parameter IN ('MP2,5','MP10','CO','O3','NO2','SO2')",
    "AND ts_ms <= $1) latest ON TRUE",
    "LEFT JOIN public.station_iqar i ON i.station_id = s.station_id AND i.ts_ms = latest.ts_ms AND i.profile = $2",
    "LEFT JOIN public.quality pm ON pm.station_id = s.station_id AND pm.parameter = 'MP2,5' AND pm.ts_ms = latest.ts_ms AND pm.profile = $2",
    "LEFT JOIN LATERAL (SELECT MIN(ts_ms) AS first_ms, MAX(ts_ms) AS last_ms FROM public.quality",
    "WHERE station_id = s.station_id AND parameter = 'MP2,5' AND ts_ms <= $1) bounds ON TRUE",
    "ORDER BY s.station_id"), params = list(floor(as.numeric(now) * 1000), meta$quality_profile))
  if (!nrow(rows)) stop("FioAres station inventory empty")
  metadata <- lapply(rows$metadata_json, jsonlite::fromJSON)
  valid <- vapply(metadata, function(m) {
    isTRUE(m$confirmed) && identical(m$datum, "SIRGAS2000") &&
      is.numeric(m$nuLatitude) && length(m$nuLatitude) == 1L && is.finite(m$nuLatitude) && abs(m$nuLatitude) <= 90 &&
      is.numeric(m$nuLongitude) && length(m$nuLongitude) == 1L && is.finite(m$nuLongitude) && abs(m$nuLongitude) <= 180 &&
      all(vapply(m[c("noEstacao", "noMunicipio", "sgUF", "statusEstacao")],
                 function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x), logical(1)))
  }, logical(1))
  if (!all(valid)) stop("Unconfirmed FioAres station metadata")
  field <- function(key) vapply(metadata, `[[`, character(1), key)
  rows$name <- gsub("\\bFIOTEC\\b", "FioAres", field("noEstacao"), ignore.case = TRUE, perl = TRUE)
  rows$city <- field("noMunicipio")
  rows$uf <- field("sgUF")
  rows$operation <- field("statusEstacao")
  rows$lon <- vapply(metadata, `[[`, numeric(1), "nuLongitude")
  rows$lat <- vapply(metadata, `[[`, numeric(1), "nuLatitude")
  geo <- sf::st_transform(sf::st_as_sf(rows, coords = c("lon", "lat"), crs = 4674), 4326)
  coordinates <- sf::st_coordinates(geo)
  rows$lon <- coordinates[, 1]
  rows$lat <- coordinates[, 2]
  rows$pm25[is.na(rows$pm25_valid) | rows$pm25_valid != 1L | !is.finite(rows$pm25)] <- NA_real_
  rows$metadata_json <- NULL
  rows
}

fioares_fetch_snapshot <- function(config) {
  fioares_with_publication(config, function(con, meta) {
    list(status = "ready", stations = fioares_read_stations(con, meta),
         publication = meta$published_ms, checked_at = Sys.time())
  })
}

fioares_date_interval <- function(start, end, timezone, max_days = 31L) {
  dates <- tryCatch(as.Date(c(as.character(start), as.character(end))), error = function(e) as.Date(NA))
  if (length(dates) != 2L || anyNA(dates) || dates[[2]] < dates[[1]]) stop("dates")
  if (as.integer(dates[[2]] - dates[[1]]) + 1L > max_days) stop("range")
  if (length(timezone) != 1L || is.na(timezone) || !timezone %in% OlsonNames()) stop("timezone")
  # The end date is inclusive in the chosen display timezone, including DST.
  bounds <- as.POSIXct(paste(c(dates[[1]], dates[[2]] + 1L), "00:00:00"), tz = timezone)
  if (anyNA(bounds)) stop("dates")
  as.numeric(bounds) * 1000
}

fioares_read_history <- function(con, meta, station_id, interval) {
  rows <- DBI::dbGetQuery(con, paste(
    "SELECT ts_ms, value, valid, rolling, rolling_valid, hour_flags, rolling_flags",
    "FROM public.quality WHERE station_id = $1 AND parameter = 'MP2,5'",
    "AND ts_ms >= $2 AND ts_ms < $3 AND profile = $4 ORDER BY ts_ms"),
    params = list(station_id, interval[[1]], interval[[2]], meta$quality_profile))
  # Missing hours remain gaps; flagged values are never plotted as valid readings.
  grid <- data.frame(ts_ms = seq(ceiling(interval[[1]] / 3600000) * 3600000,
                                 interval[[2]] - 1, by = 3600000))
  rows <- merge(grid, rows, by = "ts_ms", all.x = TRUE, sort = TRUE)
  rows$date <- as.POSIXct(rows$ts_ms / 1000, origin = "1970-01-01", tz = "UTC")
  rows$value[is.na(rows$valid) | rows$valid != 1L | !is.finite(rows$value)] <- NA_real_
  rows$rolling[is.na(rows$rolling_valid) | rows$rolling_valid != 1L | !is.finite(rows$rolling)] <- NA_real_
  rows
}

fioares_fetch_history <- function(config, station_id, interval) {
  fioares_with_publication(config, function(con, meta) {
    list(status = "ready", station_id = station_id,
         data = fioares_read_history(con, meta, station_id, interval),
         publication = meta$published_ms)
  })
}

fioares_colors <- function() c(good = "#34d399", moderate = "#facc15", bad = "#fb923c",
  very_bad = "#f43f5e", extreme = "#a855f7", unknown = "#94a3b8")

fioares_station_state <- function(stations, status = "ready", now = Sys.time(), stale_hours = 3) {
  age <- as.numeric(now) - stations$ts_ms / 1000
  recent <- is.finite(age) & age >= 0 & age <= stale_hours * 3600
  keys <- c("good", "moderate", "bad", "very_bad", "extreme")
  state <- rep("no_index", nrow(stations))
  valid <- is.finite(stations$iqar) & stations$iqar >= 0 & recent
  state[valid] <- keys[findInterval(stations$iqar[valid], c(-Inf, 40, 80, 120, 200), left.open = TRUE)]
  state[!recent] <- "stale"
  state[!is.finite(stations$ts_ms)] <- "no_data"
  state[stations$operation != "Em operação"] <- "inactive"
  if (status != "ready") state[] <- "unavailable"
  stations$state <- state
  stations$color <- unname(fioares_colors()[ifelse(state %in% keys, state, "unknown")])
  stations
}

fioares_log_error <- function(error, config, operation) {
  diagnostic <- conditionMessage(error)
  secrets <- unique(c(config$connection$password,
    Sys.getenv(c("FIOARES_PGPASSWORD", "PGPASSWORD"), unset = "")))
  for (secret in secrets[!is.na(secrets) & nzchar(secrets)]) {
    diagnostic <- gsub(secret, "[redigido]", diagnostic, fixed = TRUE)
  }
  diagnostic <- gsub("(postgres(?:ql)?://[^:[:space:]/]+:)[^@[:space:]]+@",
                     "\\1[redigido]@", diagnostic, perl = TRUE)
  message("[FioAres] ", operation, ": ", diagnostic)
  invisible(NULL)
}

create_fioares_store <- function(config = fioares_config()) {
  snapshot <- list(status = if (config$configured) "loading" else "unconfigured",
                   stations = data.frame(), publication = NULL, checked_at = as.POSIXct(NA))
  pending <- NULL
  history_cache <- cachem::cache_mem(max_size = 16 * 1024^2, max_age = config$refresh_seconds, max_n = 32)
  refresh_async <- function(force = FALSE) {
    if (!config$configured) return(promises::promise_resolve(snapshot))
    if (!is.null(pending)) return(pending)
    age <- as.numeric(difftime(Sys.time(), snapshot$checked_at, units = "secs"))
    if (!isTRUE(force) && is.finite(age) && age < config$refresh_seconds) return(promises::promise_resolve(snapshot))
    pending <<- promises::then(promises::future_promise(fioares_fetch_snapshot(config), seed = TRUE),
      onFulfilled = function(value) {
        if (!identical(snapshot$publication, value$publication)) history_cache$reset()
        snapshot <<- value
        pending <<- NULL
        value
      }, onRejected = function(error) {
        fioares_log_error(error, config, "Falha ao consultar estações")
        # Retain only the last known station locations; the UI will turn them grey.
        snapshot$status <<- "unavailable"
        snapshot$checked_at <<- Sys.time()
        history_cache$reset()
        pending <<- NULL
        snapshot
      })
    pending
  }
  history_async <- function(station_id, start, end, timezone) {
    interval <- fioares_date_interval(start, end, timezone, config$max_days)
    if (!station_id %in% snapshot$stations$station_id || snapshot$status != "ready") {
      return(promises::promise_resolve(list(status = "unavailable", data = data.frame())))
    }
    key <- digest::digest(list(station_id, interval, snapshot$publication))
    cached <- history_cache$get(key)
    if (!cachem::is.key_missing(cached)) return(promises::promise_resolve(cached))
    promises::then(promises::future_promise(fioares_fetch_history(config, station_id, interval), seed = TRUE),
      onFulfilled = function(value) { history_cache$set(key, value); value },
      onRejected = function(error) {
        fioares_log_error(error, config, "Falha ao consultar histórico")
        list(status = "unavailable", data = data.frame())
      })
  }
  list(snapshot = function() snapshot, refresh_async = refresh_async, history_async = history_async,
       config = config, close = function() history_cache$reset())
}
