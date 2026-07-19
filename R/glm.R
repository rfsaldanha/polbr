glm_parse_start_time <- function(key) {
  code <- sub(".*_s([0-9]{13})[0-9]_.*", "\\1", key)
  if (identical(code, key)) return(as.POSIXct(NA, tz = "UTC"))
  as.POSIXct(strptime(code, format = "%Y%j%H%M%S", tz = "UTC"))
}

glm_s3_keys <- function(start, end = Sys.time()) {
  start <- as.POSIXct(start, origin = "1970-01-01", tz = "UTC")
  end <- as.POSIXct(end, origin = "1970-01-01", tz = "UTC")
  first_hour <- as.POSIXct(format(start, "%Y-%m-%d %H:00:00", tz = "UTC"), tz = "UTC")
  last_hour <- as.POSIXct(format(end, "%Y-%m-%d %H:00:00", tz = "UTC"), tz = "UTC")
  hours <- seq(first_hour, last_hour, by = "hour")

  keys <- unlist(lapply(hours, function(hour) {
    prefix <- paste0("GLM-L2-LCFA/", format(hour, "%Y/%j/%H", tz = "UTC"), "/")
    lower_bound <- max(start, hour)
    start_after <- paste0(
      prefix, "OR_GLM-L2-LCFA_G19_s",
      format(lower_bound, "%Y%j%H%M%S", tz = "UTC"), "0"
    )
    endpoint <- paste0(
      "https://noaa-goes19.s3.amazonaws.com/?list-type=2&prefix=",
      utils::URLencode(prefix, reserved = TRUE),
      "&start-after=", utils::URLencode(start_after, reserved = TRUE),
      "&max-keys=1000"
    )
    handle <- curl::new_handle(connecttimeout = 6, timeout = 18)
    response <- curl::curl_fetch_memory(endpoint, handle = handle)
    if (response$status_code != 200L) stop("NOAA S3 respondeu HTTP ", response$status_code)
    xml <- rawToChar(response$content)
    matches <- regmatches(xml, gregexpr("(?<=<Key>)[^<]+", xml, perl = TRUE))[[1]]
    matches[endsWith(matches, ".nc")]
  }), use.names = FALSE)

  if (!length(keys)) return(character())
  times <- as.POSIXct(vapply(keys, function(key) as.numeric(glm_parse_start_time(key)), numeric(1)), origin = "1970-01-01", tz = "UTC")
  unique(keys[!is.na(times) & times >= start & times <= end + 60])
}

glm_read_flashes <- function(path, key, bounds) {
  dataset <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(dataset), add = TRUE)
  latitude <- ncdf4::ncvar_get(dataset, "flash_lat")
  longitude <- ncdf4::ncvar_get(dataset, "flash_lon")
  energy <- ncdf4::ncvar_get(dataset, "flash_energy")
  quality <- ncdf4::ncvar_get(dataset, "flash_quality_flag")
  observed_at <- ncdf4::ncatt_get(dataset, 0, "time_coverage_end")$value
  observed_at <- as.POSIXct(
    observed_at, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"
  )
  if (!length(observed_at) || is.na(observed_at)) observed_at <- glm_parse_start_time(key)

  valid <- is.finite(latitude) & is.finite(longitude) & is.finite(energy) & quality == 0 &
    longitude >= bounds[[1]][[1]] & longitude <= bounds[[2]][[1]] &
    latitude >= bounds[[1]][[2]] & latitude <= bounds[[2]][[2]]
  data.frame(
    lon = as.numeric(longitude[valid]),
    lat = as.numeric(latitude[valid]),
    energy = as.numeric(energy[valid]),
    observed_at = rep(as.numeric(observed_at), sum(valid)),
    stringsAsFactors = FALSE
  )
}

create_glm_store <- function(
  bounds = coverage_config("lac")$bounds,
  window_minutes = 5L,
  refresh_seconds = 45L
) {
  cache_dir <- tempfile("alertar-glm-")
  dir.create(cache_dir, recursive = TRUE)
  records <- new.env(parent = emptyenv())
  last_attempt <- as.POSIXct(NA, tz = "UTC")
  snapshot <- list(
    flashes = data.frame(lon = numeric(), lat = numeric(), energy = numeric(), observed_at = numeric()),
    latest = as.POSIXct(NA, tz = "UTC"),
    updated = as.POSIXct(NA, tz = "UTC"),
    error = NULL
  )

  refresh <- function(force = FALSE) {
    now <- as.POSIXct(Sys.time(), tz = "UTC")
    recently_attempted <- !is.na(last_attempt) &&
      as.numeric(difftime(now, last_attempt, units = "secs")) < refresh_seconds
    if (!force && recently_attempted) return(snapshot)
    last_attempt <<- now
    window_start <- now - window_minutes * 60

    tryCatch({
      keys <- glm_s3_keys(window_start - 30, now)
      if (!length(keys)) stop("Nenhum arquivo GLM recente foi publicado pelo NOAA.")
      for (key in keys) {
        if (exists(key, envir = records, inherits = FALSE)) next
        destination <- file.path(cache_dir, basename(key))
        url <- paste0("https://noaa-goes19.s3.amazonaws.com/", key)
        flashes <- tryCatch(
          {
            handle <- curl::new_handle(connecttimeout = 6, timeout = 20)
            curl::curl_download(
              url, destination, quiet = TRUE, mode = "wb", handle = handle
            )
            glm_read_flashes(destination, key, bounds)
          },
          error = function(error) {
            warning("Falha ao processar arquivo GLM ", basename(key), ": ", conditionMessage(error))
            NULL
          }
        )
        unlink(destination, force = TRUE)
        if (!is.null(flashes)) assign(key, flashes, envir = records)
      }
      if (!length(ls(envir = records, all.names = TRUE))) {
        stop("Nenhum arquivo GLM recente pôde ser processado.")
      }

      cached_keys <- ls(envir = records, all.names = TRUE)
      if (length(cached_keys)) {
        cached_times <- vapply(cached_keys, function(key) as.numeric(glm_parse_start_time(key)), numeric(1))
        expired <- cached_keys[!is.finite(cached_times) | cached_times < as.numeric(window_start - 60)]
        if (length(expired)) rm(list = expired, envir = records)
      }
      current <- ls(envir = records, all.names = TRUE)
      flashes <- if (length(current)) {
        do.call(rbind, mget(current, envir = records, inherits = FALSE))
      } else snapshot$flashes[0, , drop = FALSE]
      flashes <- flashes[
        is.finite(flashes$observed_at) & flashes$observed_at >= as.numeric(window_start),
        , drop = FALSE
      ]
      rownames(flashes) <- NULL
      latest <- if (nrow(flashes)) {
        as.POSIXct(max(flashes$observed_at), origin = "1970-01-01", tz = "UTC")
      } else as.POSIXct(NA, tz = "UTC")
      snapshot <<- list(flashes = flashes, latest = latest, updated = now, error = NULL)
      snapshot
    }, error = function(error) {
      snapshot$error <<- conditionMessage(error)
      snapshot$updated <<- now
      snapshot
    })
  }

  list(
    refresh = refresh,
    close = function() unlink(cache_dir, recursive = TRUE, force = TRUE)
  )
}
