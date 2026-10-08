is_forecast_raster_metadata_warning <- function(warning) {
  message <- conditionMessage(warning)
  valid_time_array <-
    grepl("different geometry", message, fixed = TRUE) &&
    grepl("/valid_time", message, fixed = TRUE)
  auxiliary_dimension <-
    grepl("GDAL Message 1: dimension", message, fixed = TRUE) &&
    grepl("(forecast_reference_time|forecast_period|time)", message) &&
    grepl("is not a (Time|Longitude/X|Latitude/Y) dimension", message)
  valid_time_array || auxiliary_dimension
}

read_forecast_raster <- function(path) {
  withCallingHandlers(
    terra::rast(path),
    warning = function(warning) {
      if (is_forecast_raster_metadata_warning(warning)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

normalize_fire_data <- function(fires) {
  if (is.null(fires)) return(NULL)
  if (inherits(fires, "sf")) fires <- sf::st_drop_geometry(fires)
  if (!is.data.frame(fires)) stop("O arquivo de focos de calor deve conter um data frame.")
  if (!all(c("lat", "lon") %in% names(fires))) {
    stop("O arquivo de focos de calor deve conter as colunas 'lat' e 'lon'.")
  }

  latitude <- suppressWarnings(as.numeric(fires$lat))
  longitude <- suppressWarnings(as.numeric(fires$lon))
  valid <- is.finite(latitude) & is.finite(longitude) &
    latitude >= -90 & latitude <= 90 & longitude >= -180 & longitude <= 180
  fires <- fires[valid, , drop = FALSE]
  fires$lat <- latitude[valid]
  fires$lon <- longitude[valid]
  if (!nrow(fires)) return(fires)

  key <- if ("id" %in% names(fires)) {
    id <- trimws(as.character(fires$id))
    fallback <- paste(
      round(fires$lat, 5), round(fires$lon, 5),
      if ("data_hora_gmt" %in% names(fires)) as.character(fires$data_hora_gmt) else "",
      sep = "|"
    )
    ifelse(!is.na(id) & nzchar(id), id, fallback)
  } else {
    paste(
      round(fires$lat, 5), round(fires$lon, 5),
      if ("data_hora_gmt" %in% names(fires)) as.character(fires$data_hora_gmt) else "",
      sep = "|"
    )
  }
  fires <- fires[!duplicated(key), , drop = FALSE]

  window_hours <- suppressWarnings(as.numeric(
    Sys.getenv("ALERTAR_FIRE_WINDOW_HOURS", unset = "6")
  ))
  if (
    is.finite(window_hours) && window_hours > 0 &&
      "data_hora_gmt" %in% names(fires)
  ) {
    observed_at <- suppressWarnings(as.POSIXct(
      as.character(fires$data_hora_gmt), tz = "UTC",
      tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OSZ")
    ))
    if (any(!is.na(observed_at))) {
      reference_now <- as.POSIXct(Sys.time(), tz = "UTC")
      recent <- !is.na(observed_at) &
        observed_at >= reference_now - window_hours * 3600 &
        observed_at <= reference_now + 5 * 60
      fires <- fires[recent, , drop = FALSE]
    }
  }
  rownames(fires) <- NULL
  fires
}

read_fire_data <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    normalize_fire_data(readRDS(path)),
    error = function(error) {
      warning("Falha ao ler focos de calor: ", conditionMessage(error))
      NULL
    }
  )
}

render_raster_image_file <- function(path, cfg, index, target_size, image_dir, revision) {
  raster <- read_forecast_raster(path)
  layer <- raster[[index]] * cfg$scale + cfg$offset
  layer <- terra::project(layer, "EPSG:3857", method = "bilinear")
  longest_side <- max(terra::ncol(layer), terra::nrow(layer))
  factor <- min(8L, max(1L, ceiling(target_size / longest_side)))
  if (factor > 1L) layer <- terra::disagg(layer, fact = factor, method = "bilinear")

  values <- terra::as.matrix(layer, wide = TRUE)
  alpha <- ifelse(is.finite(values), 0.82, 0)
  if (isTRUE(cfg$continuous_palette) || is.null(cfg$breaks)) {
    ramp <- grDevices::colorRampPalette(cfg$colors)(256)
    scaled <- (values - cfg$range[[1]]) / diff(cfg$range)
    color_index <- pmax(1L, pmin(256L, floor(scaled * 255) + 1L))
  } else {
    ramp <- cfg$colors
    color_index <- findInterval(values, cfg$breaks, all.inside = TRUE)
    color_index <- pmax(1L, pmin(length(ramp), color_index))
  }
  rgba <- grDevices::col2rgb(ramp[color_index], alpha = FALSE) / 255
  nr <- nrow(values)
  nc <- ncol(values)
  image <- array(0, dim = c(nr, nc, 4))
  image[, , 1] <- matrix(rgba[1, ], nrow = nr, byrow = FALSE)
  image[, , 2] <- matrix(rgba[2, ], nrow = nr, byrow = FALSE)
  image[, , 3] <- matrix(rgba[3, ], nrow = nr, byrow = FALSE)
  image[, , 4] <- alpha

  png_path <- file.path(
    image_dir,
    sprintf("%s-r%03d-%03d.png", cfg$id, as.integer(revision), as.integer(index))
  )
  temporary_path <- tempfile("raster-", tmpdir = image_dir, fileext = ".png")
  png::writePNG(image, temporary_path)
  if (!file.rename(temporary_path, png_path)) {
    unlink(png_path, force = TRUE)
    if (!file.rename(temporary_path, png_path)) stop("Falha ao publicar raster pré-carregado.")
  }

  extent <- terra::ext(layer)
  corners <- terra::vect(
    cbind(
      c(extent$xmin, extent$xmax, extent$xmax, extent$xmin),
      c(extent$ymax, extent$ymax, extent$ymin, extent$ymin)
    ),
    type = "points", crs = terra::crs(layer)
  )
  longitude_latitude <- terra::crds(terra::project(corners, "EPSG:4326"))
  coordinates <- lapply(seq_len(nrow(longitude_latitude)), function(i) {
    unname(longitude_latitude[i, ])
  })
  list(
    url = sprintf(
      "forecast-images/%s?v=%s", basename(png_path),
      as.integer(file.info(png_path)$mtime)
    ),
    coordinates = coordinates,
    index = as.integer(index),
    width = terra::ncol(layer),
    height = terra::nrow(layer),
    render_factor = factor
  )
}

validate_wind_files <- function(data_dir, horizons) {
  horizons <- sort(unique(as.integer(horizons[is.finite(horizons)])))
  paths <- file.path(data_dir, sprintf("wind_%d.json", horizons + 1L))
  missing <- paths[!file.exists(paths)]
  invalid <- character()

  candidates <- setdiff(paths, missing)
  for (path in candidates) {
    valid <- tryCatch({
      components <- jsonlite::fromJSON(path, simplifyVector = FALSE)
      if (length(components) != 2L) stop("O arquivo deve conter os componentes u e v.")
      parameter_numbers <- vapply(components, function(component) {
        as.integer(component$header$parameterNumber %||% NA_integer_)
      }, integer(1))
      dimensions_match <- vapply(components, function(component) {
        header <- component$header
        expected <- as.integer(header$nx %||% 0L) * as.integer(header$ny %||% 0L)
        expected > 0L && length(component$data) == expected
      }, logical(1))
      identical(sort(parameter_numbers), c(2L, 3L)) && all(dimensions_match)
    }, error = function(error) FALSE)
    if (!isTRUE(valid)) invalid <- c(invalid, path)
  }

  list(
    valid = length(horizons) > 0L && !length(missing) && !length(invalid),
    expected = length(paths),
    missing = basename(missing),
    invalid = basename(invalid),
    horizons = horizons
  )
}

create_data_store <- function(data_dir, catalog, coverage = coverage_config()) {
  read_generation <- function() {
    marker <- file.path(data_dir, ".cams_generation")
    if (!file.exists(marker)) return(NA_character_)
    value <- trimws(readLines(marker, n = 1L, warn = FALSE))
    if (length(value) && nzchar(value[[1]])) value[[1]] else NA_character_
  }
  current_generation <- read_generation()
  load_rasters <- function() {
    lapply(catalog, function(x) {
      path <- file.path(data_dir, x$file)
      if (file.exists(path)) read_forecast_raster(path) else NULL
    })
  }
  build_raster_horizons <- function(values) {
    result <- lapply(names(catalog), function(id) {
      x <- values[[id]]
      if (is.null(x)) return(numeric())
      seq.int(0, by = catalog[[id]]$interval, length.out = terra::nlyr(x))
    })
    names(result) <- names(catalog)
    result
  }

  rasters <- load_rasters()
  available <- names(rasters)[!vapply(rasters, is.null, logical(1))]
  if (!length(available)) stop("Nenhum raster de previsao foi encontrado em ", data_dir)
  default_indicator <- if ("pm25" %in% available) "pm25" else available[[1]]
  revision <- shiny::reactiveVal(0L)

  raster_horizons <- build_raster_horizons(rasters)

  normalize_horizon <- function(id, horizon) {
    values <- raster_horizons[[id]]
    if (!length(values)) return(NA_real_)
    horizon <- suppressWarnings(as.numeric(horizon))
    if (!is.finite(horizon)) horizon <- values[[1]]
    values[[which.min(abs(values - horizon))]]
  }

  future_horizons <- function(id, horizon, count = 1L) {
    values <- raster_horizons[[id]]
    current <- normalize_horizon(id, horizon)
    future <- values[values > current]
    head(future, max(0L, as.integer(count)))
  }

  territories_candidates <- c(
    file.path(data_dir, "territories.rds"),
    file.path(data_dir, "places.rds"),
    file.path(data_dir, "mun_epsg4326.rds"),
    file.path(data_dir, "mun_seats.rds"),
    file.path("data", "territories.rds"),
    file.path("data", "places.rds"),
    file.path("data", "mun_epsg4326.rds"),
    file.path("data", "mun_seats.rds")
  )
  territories_path <- territories_candidates[file.exists(territories_candidates)][[1]]
  territories <- normalize_territories(readRDS(territories_path))

  resolve_fire_path <- function() {
    candidates <- c(
      file.path(data_dir, "bdq_focos.rds"),
      file.path("data", "bdq_focos.rds")
    )
    existing <- candidates[file.exists(candidates)]
    if (length(existing)) existing[[1]] else candidates[[1]]
  }
  fire_path <- resolve_fire_path()
  fires <- read_fire_data(fire_path)
  fire_mtime <- if (file.exists(fire_path)) file.info(fire_path)$mtime[[1]] else as.POSIXct(NA)

  refresh_fires <- function(force = FALSE) {
    new_fire_path <- resolve_fire_path()
    if (!file.exists(new_fire_path)) return(FALSE)
    new_fire_mtime <- file.info(new_fire_path)$mtime[[1]]
    unchanged <- identical(
      normalizePath(new_fire_path, mustWork = FALSE),
      normalizePath(fire_path, mustWork = FALSE)
    ) &&
      isTRUE(all.equal(new_fire_mtime, fire_mtime))
    if (!isTRUE(force) && unchanged) return(FALSE)

    new_fires <- read_fire_data(new_fire_path)
    if (is.null(new_fires)) return(FALSE)

    fires <<- new_fires
    fire_path <<- new_fire_path
    fire_mtime <<- new_fire_mtime
    TRUE
  }

  resolve_db_path <- function() {
    candidates <- c(file.path(data_dir, "cams_forecast.duckdb"), file.path("data", "cams_forecast.duckdb"))
    existing <- candidates[file.exists(candidates)]
    if (length(existing)) existing[[1]] else candidates[[1]]
  }
  db_path <- resolve_db_path()
  con <- if (file.exists(db_path)) {
    DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  } else NULL
  tables <- if (is.null(con)) character() else DBI::dbListTables(con)

  image_cache <- cachem::cache_mem(max_size = 192 * 1024^2)
  pending_images <- new.env(parent = emptyenv())
  query_cache <- cachem::cache_mem(max_size = 64 * 1024^2)
  image_dir <- tempfile("alertar-raster-cache-")
  dir.create(image_dir, recursive = TRUE)
  shiny::addResourcePath("forecast-images", image_dir)

  raster_target_size <- suppressWarnings(as.integer(
    Sys.getenv("ALERTAR_RASTER_SIZE", unset = "1024")
  ))
  if (!is.finite(raster_target_size) || raster_target_size < 256L) {
    raster_target_size <- 1024L
  }
  raster_target_size <- min(raster_target_size, 2048L)

  render_raster <- function(layer) {
    # MapLibre image sources interpolate the four image corners in projected
    # space. Project first so continental rasters stay aligned by latitude.
    layer <- terra::project(layer, "EPSG:3857", method = "bilinear")
    longest_side <- max(terra::ncol(layer), terra::nrow(layer))
    factor <- min(8L, max(1L, ceiling(raster_target_size / longest_side)))
    if (factor > 1L) {
      layer <- terra::disagg(layer, fact = factor, method = "bilinear")
    }
    list(layer = layer, factor = factor)
  }

  raster_coordinates <- function(layer) {
    extent <- terra::ext(layer)
    corners <- terra::vect(
      cbind(
        c(extent$xmin, extent$xmax, extent$xmax, extent$xmin),
        c(extent$ymax, extent$ymax, extent$ymin, extent$ymin)
      ),
      type = "points",
      crs = terra::crs(layer)
    )
    longitude_latitude <- terra::crds(terra::project(corners, "EPSG:4326"))
    lapply(seq_len(nrow(longitude_latitude)), function(i) unname(longitude_latitude[i, ]))
  }

  raster_image <- function(id, horizon) {
    cfg <- catalog[[id]]
    x <- rasters[[id]]
    actual_horizon <- normalize_horizon(id, horizon)
    index <- match(actual_horizon, raster_horizons[[id]])
    current_revision <- shiny::isolate(revision())
    key <- paste0("image-v5-webmercator-", raster_target_size, "-r", current_revision, "-", id, "-layer-", index)
    if (image_cache$exists(key)) return(image_cache$get(key))

    layer <- x[[index]] * cfg$scale + cfg$offset
    rendered <- render_raster(layer)
    layer <- rendered$layer
    values <- terra::as.matrix(layer, wide = TRUE)
    alpha <- ifelse(is.finite(values), 0.82, 0)

    if (isTRUE(cfg$continuous_palette) || is.null(cfg$breaks)) {
      ramp <- grDevices::colorRampPalette(cfg$colors)(256)
      scaled <- (values - cfg$range[[1]]) / diff(cfg$range)
      color_index <- pmax(1L, pmin(256L, floor(scaled * 255) + 1L))
    } else {
      ramp <- cfg$colors
      color_index <- findInterval(values, cfg$breaks, all.inside = TRUE)
      color_index <- pmax(1L, pmin(length(ramp), color_index))
    }

    rgba <- grDevices::col2rgb(ramp[color_index], alpha = FALSE) / 255
    nr <- nrow(values)
    nc <- ncol(values)
    image <- array(0, dim = c(nr, nc, 4))
    image[, , 1] <- matrix(rgba[1, ], nrow = nr, byrow = FALSE)
    image[, , 2] <- matrix(rgba[2, ], nrow = nr, byrow = FALSE)
    image[, , 3] <- matrix(rgba[3, ], nrow = nr, byrow = FALSE)
    image[, , 4] <- alpha

    png_path <- file.path(image_dir, sprintf("%s-r%03d-%03d.png", id, current_revision, index))
    png::writePNG(image, png_path)
    result <- list(
      url = sprintf(
        "forecast-images/%s?v=%s",
        basename(png_path),
        as.integer(file.info(png_path)$mtime)
      ),
      coordinates = raster_coordinates(layer),
      index = index,
      horizon = actual_horizon,
      width = terra::ncol(layer),
      height = terra::nrow(layer),
      render_factor = rendered$factor
    )
    image_cache$set(key, result)
    result
  }

  raster_image_async <- function(id, horizon) {
    cfg <- catalog[[id]]
    actual_horizon <- normalize_horizon(id, horizon)
    index <- match(actual_horizon, raster_horizons[[id]])
    current_revision <- shiny::isolate(revision())
    key <- paste0("image-v5-webmercator-", raster_target_size, "-r", current_revision, "-", id, "-layer-", index)
    if (image_cache$exists(key)) return(promises::promise_resolve(image_cache$get(key)))
    if (exists(key, envir = pending_images, inherits = FALSE)) {
      return(get(key, envir = pending_images, inherits = FALSE))
    }

    path <- file.path(data_dir, cfg$file)
    cfg$id <- id
    promise <- promises::future_promise({
      render_raster_image_file(
        path, cfg, index, raster_target_size, image_dir, current_revision
      )
    }, seed = TRUE)
    promise <- promises::then(
      promise,
      onFulfilled = function(result) {
        if (exists(key, envir = pending_images, inherits = FALSE)) {
          rm(list = key, envir = pending_images)
        }
        result$horizon <- actual_horizon
        if (identical(current_revision, shiny::isolate(revision()))) {
          image_cache$set(key, result)
        }
        result
      },
      onRejected = function(error) {
        if (exists(key, envir = pending_images, inherits = FALSE)) {
          rm(list = key, envir = pending_images)
        }
        stop(error)
      }
    )
    assign(key, promise, envir = pending_images)
    promise
  }

  query_series <- function(id, territory_id) {
    key <- gsub("[^a-z0-9]", "", tolower(paste0("query", id, "territory", territory_id)))
    if (query_cache$exists(key)) return(query_cache$get(key))
    cfg <- catalog[[id]]
    table <- cfg$table
    if (is.null(con) || !table %in% tables) return(data.frame())
    fields <- DBI::dbListFields(con, table)
    if ("territory_id" %in% fields) {
      sql <- sprintf(
        "SELECT date, value FROM %s WHERE CAST(territory_id AS VARCHAR) = ? ORDER BY date",
        DBI::dbQuoteIdentifier(con, table)
      )
      params <- list(as.character(territory_id))
    } else if ("place_id" %in% fields) {
      sql <- sprintf(
        "SELECT date, value FROM %s WHERE CAST(place_id AS VARCHAR) = ? ORDER BY date",
        DBI::dbQuoteIdentifier(con, table)
      )
      params <- list(as.character(territory_id))
    } else {
      code <- suppressWarnings(as.integer(territory_id))
      if (is.na(code)) return(data.frame())
      if (nchar(as.character(territory_id)) >= 7L) {
        sql <- sprintf(
          "SELECT date, value FROM %s WHERE code_muni = ? ORDER BY date",
          DBI::dbQuoteIdentifier(con, table)
        )
        params <- list(code)
      } else {
        sql <- sprintf(
          "SELECT date, value FROM %s WHERE code_muni BETWEEN ? AND ? ORDER BY date",
          DBI::dbQuoteIdentifier(con, table)
        )
        params <- list(code * 10L, code * 10L + 9L)
      }
    }
    result <- DBI::dbGetQuery(con, sql, params = params)
    if (nrow(result)) {
      result$date <- as.POSIXct(result$date, tz = "UTC")
      missing_dates <- is.na(result$date)
      if (any(missing_dates) && any(!missing_dates)) {
        interval_seconds <- as.numeric(cfg$interval) * 3600
        expected_dates <- min(result$date, na.rm = TRUE) +
          seq.int(0, by = interval_seconds, length.out = nrow(result))
        result$date[missing_dates] <- expected_dates[missing_dates]
      }
      series_scale <- if (is.null(cfg$series_scale)) 1 else cfg$series_scale
      series_offset <- if (is.null(cfg$series_offset)) 0 else cfg$series_offset
      result$value <- result$value * series_scale + series_offset
      result <- result[
        is.finite(as.numeric(result$date)) & is.finite(result$value),
        , drop = FALSE
      ]
    }
    query_cache$set(key, result)
    result
  }

  query_report_metrics <- function(id) {
    key <- paste0("report-metrics-v2-", id)
    if (query_cache$exists(key)) return(query_cache$get(key))
    cfg <- catalog[[id]]
    table <- cfg$table
    if (is.null(con) || !table %in% tables) return(data.frame())

    fields <- DBI::dbListFields(con, table)
    key_field <- c("territory_id", "place_id", "code_muni")
    key_field <- key_field[key_field %in% fields]
    if (!length(key_field) || !all(c("date", "value") %in% fields)) return(data.frame())
    key_field <- key_field[[1]]
    key_expression <- if (identical(key_field, "code_muni")) {
      sprintf("CAST(CAST(%s AS BIGINT) AS VARCHAR)", DBI::dbQuoteIdentifier(con, key_field))
    } else {
      sprintf("CAST(%s AS VARCHAR)", DBI::dbQuoteIdentifier(con, key_field))
    }

    series_scale <- if (is.null(cfg$series_scale)) 1 else cfg$series_scale
    series_offset <- if (is.null(cfg$series_offset)) 0 else cfg$series_offset
    value_expression <- sprintf(
      "(CAST(value AS DOUBLE) * %.17g + %.17g)",
      series_scale, series_offset
    )
    category_breaks <- cfg$breaks %||% numeric()
    category_columns <- character()
    if (length(category_breaks) >= 2L) {
      category_columns <- vapply(seq_len(length(category_breaks) - 1L), function(index) {
        lower <- category_breaks[[index]]
        upper <- category_breaks[[index + 1L]]
        condition <- if (!is.finite(lower)) {
          sprintf("metric_value < %.17g", upper)
        } else if (!is.finite(upper)) {
          sprintf("metric_value >= %.17g", lower)
        } else {
          sprintf("metric_value >= %.17g AND metric_value < %.17g", lower, upper)
        }
        sprintf(
          "SUM(CASE WHEN %s THEN duration_hours ELSE 0 END) AS category_%d_hours",
          condition, index
        )
      }, character(1))
    }

    references <- cfg$references %||% list()
    reference_value <- if (length(references)) as.numeric(references[[1]]$value) else NA_real_
    averaging_hours <- if (length(references)) {
      suppressWarnings(as.numeric(references[[1]]$averaging_hours %||% NA_real_))
    } else NA_real_
    reference_cte <- ""
    metric_source <- "timed"
    reference_metric <- "metric_value"
    if (is.finite(reference_value) && is.finite(averaging_hours) && averaging_hours > 0) {
      reference_cte <- sprintf(
        paste(
          ", rolling_reference AS (",
          "SELECT e.territory_key, e.date,",
          "SUM(s.metric_value * date_diff('second',",
          "GREATEST(s.date, e.date - INTERVAL '%g hours'),",
          "LEAST(s.next_date, e.date))) / %.17g AS reference_metric",
          "FROM timed e JOIN timed s ON s.territory_key = e.territory_key",
          "AND e.date >= e.series_start + INTERVAL '%g hours'",
          "AND s.date < e.date AND s.next_date > e.date - INTERVAL '%g hours'",
          "GROUP BY e.territory_key, e.date",
          "), reference_values AS (",
          "SELECT t.*, r.reference_metric FROM timed t",
          "LEFT JOIN rolling_reference r USING (territory_key, date)",
          ")",
          sep = " "
        ),
        averaging_hours, averaging_hours * 3600, averaging_hours, averaging_hours
      )
      metric_source <- "reference_values"
      reference_metric <- "reference_metric"
    }
    reference_column <- if (is.finite(reference_value)) {
      sprintf(
        paste(
          "SUM(CASE WHEN %s > %.17g THEN duration_hours ELSE 0 END) AS hours_above_reference,",
          "SUM(CASE WHEN %s IS NOT NULL THEN duration_hours ELSE 0 END) AS reference_hours_covered"
        ),
        reference_metric, reference_value, reference_metric
      )
    } else {
      paste(
        "CAST(NULL AS DOUBLE) AS hours_above_reference,",
        "CAST(NULL AS DOUBLE) AS reference_hours_covered"
      )
    }

    select_columns <- c(
      "territory_key",
      "COUNT(*) AS sample_count",
      "MIN(date) AS period_start",
      "MAX(date) AS period_end",
      "MAX(metric_value) AS maximum_value",
      "ARG_MAX(date, metric_value) AS maximum_date",
      "AVG(metric_value) AS mean_value",
      "SUM(duration_hours) AS hours_covered",
      reference_column,
      category_columns
    )
    sql <- sprintf(
      paste(
        "WITH ordered AS (",
        "SELECT %s AS territory_key, date, %s AS metric_value,",
        "LEAD(date) OVER (PARTITION BY %s ORDER BY date) AS next_date",
        "FROM %s WHERE value IS NOT NULL AND date IS NOT NULL",
        "), timed AS (",
        "SELECT territory_key, date, metric_value, next_date,",
        "MIN(date) OVER (PARTITION BY territory_key) AS series_start,",
        "CASE WHEN next_date IS NULL THEN 0 ELSE LEAST(%.17g, GREATEST(0, date_diff('second', date, next_date) / 3600.0)) END AS duration_hours",
        "FROM ordered",
        ") %s SELECT %s FROM %s GROUP BY territory_key",
        sep = " "
      ),
      key_expression,
      value_expression,
      DBI::dbQuoteIdentifier(con, key_field),
      DBI::dbQuoteIdentifier(con, table),
      as.numeric(cfg$interval),
      reference_cte,
      paste(select_columns, collapse = ", "),
      metric_source
    )
    result <- DBI::dbGetQuery(con, sql)
    if (!nrow(result)) return(result)
    result$period_start <- as.POSIXct(result$period_start, tz = "UTC")
    result$period_end <- as.POSIXct(result$period_end, tz = "UTC")
    result$maximum_date <- as.POSIXct(result$maximum_date, tz = "UTC")

    metadata <- sf::st_drop_geometry(territories)
    metadata_key <- if (key_field %in% names(metadata)) {
      as.character(metadata[[key_field]])
    } else {
      as.character(metadata$territory_id)
    }
    metadata <- metadata[!duplicated(metadata_key), , drop = FALSE]
    metadata_key <- metadata_key[!duplicated(metadata_key)]
    result_key <- as.character(result$territory_key)
    matched <- match(result_key, metadata_key)
    if (identical(key_field, "code_muni")) {
      legacy_key <- ifelse(grepl("^[0-9]{7}$", result_key), substr(result_key, 1L, 6L), result_key)
      unmatched <- is.na(matched)
      matched[unmatched] <- match(legacy_key[unmatched], metadata_key)
    }
    keep <- !is.na(matched)
    result <- result[keep, , drop = FALSE]
    matched <- matched[keep]
    result$territory_id <- as.character(metadata$territory_id[matched])
    result$display_name <- as.character(metadata$display_name[matched])
    result$territory_type <- as.character(metadata$territory_type[matched])
    result$admin1_code <- as.character(metadata$admin1_code[matched])
    result$country_code <- as.character(metadata$country_code[matched])
    result$country_name <- as.character(metadata$country_name[matched])
    result <- result[order(-result$maximum_value, result$display_name), , drop = FALSE]
    rownames(result) <- NULL
    query_cache$set(key, result)
    result
  }

  analysis_time <- function(id) {
    key <- paste0("analysis", id)
    if (query_cache$exists(key)) return(query_cache$get(key))
    table <- catalog[[id]]$table
    value <- if (!is.null(con) && table %in% tables) {
      sql <- sprintf("SELECT MIN(date) AS date FROM %s", DBI::dbQuoteIdentifier(con, table))
      as.POSIXct(DBI::dbGetQuery(con, sql)$date[[1]], tz = "UTC")
    } else Sys.time()
    query_cache$set(key, value)
    value
  }

  territory_geometry <- function(territory_id) {
    territories[territories$territory_id == territory_id, , drop = FALSE]
  }

  expected_wind_horizons <- seq.int(0L, max(unlist(raster_horizons), na.rm = TRUE), by = 1L)
  wind_state <- validate_wind_files(data_dir, expected_wind_horizons)
  shiny::addResourcePath("wind-data", data_dir)

  refresh_wind_state <- function() {
    expected <- seq.int(0L, max(unlist(raster_horizons), na.rm = TRUE), by = 1L)
    wind_state <<- validate_wind_files(data_dir, expected)
    if (!isTRUE(wind_state$valid)) {
      warning(
        "Camada de vento desativada: ", length(wind_state$missing), " arquivo(s) ausente(s) e ",
        length(wind_state$invalid), " arquivo(s) inválido(s) de ", wind_state$expected, "."
      )
    }
    wind_state
  }

  wind_url <- function(horizon) {
    horizon <- suppressWarnings(as.integer(horizon))
    if (!isTRUE(wind_state$valid) || !horizon %in% wind_state$horizons) return(NULL)
    filename <- sprintf("wind_%d.json", horizon + 1L)
    path <- file.path(data_dir, filename)
    version <- if (file.exists(path)) as.integer(file.info(path)$mtime) else 0L
    sprintf("wind-data/%s?v=%s", filename, version)
  }

  refresh <- function() {
    new_generation <- read_generation()
    if (!is.na(new_generation) && identical(new_generation, current_generation)) {
      return(FALSE)
    }
    new_rasters <- tryCatch(load_rasters(), error = function(error) {
      warning("Falha ao recarregar rasters: ", conditionMessage(error))
      NULL
    })
    if (is.null(new_rasters)) return(FALSE)
    new_available <- names(new_rasters)[!vapply(new_rasters, is.null, logical(1))]
    if (!length(new_available)) return(FALSE)

    new_db_path <- resolve_db_path()
    new_con <- if (file.exists(new_db_path)) {
      tryCatch(
        DBI::dbConnect(duckdb::duckdb(), new_db_path, read_only = TRUE),
        error = function(error) {
          warning("Falha ao reabrir banco de previsao: ", conditionMessage(error))
          NULL
        }
      )
    } else NULL
    if (file.exists(new_db_path) && is.null(new_con)) return(FALSE)
    new_tables <- if (is.null(new_con)) character() else DBI::dbListTables(new_con)

    old_con <- con
    rasters <<- new_rasters
    raster_horizons <<- build_raster_horizons(new_rasters)
    refresh_wind_state()
    available <<- new_available
    db_path <<- new_db_path
    con <<- new_con
    tables <<- new_tables
    image_cache$reset()
    rm(list = ls(envir = pending_images, all.names = TRUE), envir = pending_images)
    query_cache$reset()
    unlink(list.files(image_dir, full.names = TRUE), recursive = TRUE, force = TRUE)
    refresh_fires()
    revision(shiny::isolate(revision()) + 1L)
    current_generation <<- new_generation

    if (!is.null(old_con) && DBI::dbIsValid(old_con)) {
      DBI::dbDisconnect(old_con, shutdown = TRUE)
    }
    TRUE
  }

  list(
    data_dir = data_dir,
    catalog = catalog,
    coverage = coverage,
    available = available,
    default_indicator = default_indicator,
    rasters = rasters,
    territories = territories,
    fires = function() fires,
    refresh_fires = refresh_fires,
    raster_image = raster_image,
    raster_image_async = raster_image_async,
    forecast_horizons = function(id) raster_horizons[[id]],
    normalize_horizon = normalize_horizon,
    future_horizons = future_horizons,
    query_series = query_series,
    query_report_metrics = query_report_metrics,
    analysis_time = analysis_time,
    revision = revision,
    refresh = refresh,
    territory_geometry = territory_geometry,
    wind_available = function() isTRUE(wind_state$valid),
    wind_diagnostics = function() wind_state,
    wind_url = wind_url,
    close = function() {
      if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
      unlink(image_dir, recursive = TRUE, force = TRUE)
    }
  )
}

normalize_territories <- function(x) {
  stopifnot(inherits(x, "sf"))
  names_x <- names(x)
  source_municipality_code <- if ("code_muni" %in% names_x) as.character(x$code_muni) else NULL
  pick <- function(candidates, fallback = NA_character_) {
    hit <- candidates[candidates %in% names_x]
    if (length(hit)) {
      as.character(x[[hit[[1]]]])
    } else if (length(fallback) == nrow(x)) {
      as.character(fallback)
    } else {
      rep(fallback, nrow(x))
    }
  }

  x$territory_id <- pick(c("territory_id", "place_id", "code_muni", "geocode", "id"))
  x$territory_name <- pick(c("territory_name", "place_name", "name_muni", "name", "locality"))
  x$territory_type <- pick(c("territory_type", "place_type", "area_type", "type"), "Municipio")
  x$admin1_code <- pick(c("admin1_code", "abbrev_state", "state_code", "iso_3166_2"), "")
  x$country_code <- pick(c("country_code", "iso3", "iso_a3"), "BRA")
  x$country_name <- pick(c("country_name", "country"), ifelse(x$country_code == "BRA", "Brasil", x$country_code))

  # As tabelas municipais usam o codigo IBGE completo (7 digitos), enquanto
  # o identificador historico do app usa os 6 primeiros digitos.
  legacy_brazil_code <- !("territory_id" %in% names_x) & !is.null(source_municipality_code) &
    x$country_code == "BRA" & grepl("^[0-9]{7}$", x$territory_id)
  x$territory_id[legacy_brazil_code] <- substr(x$territory_id[legacy_brazil_code], 1, 6)

  # Aliases mantem compatibilidade com o banco municipal brasileiro atual.
  x$place_id <- x$territory_id
  x$place_name <- x$territory_name
  x$code_muni <- if (is.null(source_municipality_code)) x$territory_id else source_municipality_code
  x$name_muni <- x$territory_name
  local_detail <- ifelse(nzchar(x$admin1_code), paste0(" - ", x$admin1_code), "")
  country_detail <- ifelse(x$country_code == "BRA", "", paste0(" · ", x$country_name))
  x$display_name <- paste0(x$territory_name, local_detail, country_detail)
  x
}
