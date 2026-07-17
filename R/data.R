create_data_store <- function(data_dir, catalog, coverage = coverage_config()) {
  rasters <- lapply(catalog, function(x) {
    path <- file.path(data_dir, x$file)
    if (file.exists(path)) terra::rast(path) else NULL
  })
  available <- names(rasters)[!vapply(rasters, is.null, logical(1))]
  if (!length(available)) stop("Nenhum raster de previsao foi encontrado em ", data_dir)

  raster_horizons <- lapply(names(catalog), function(id) {
    x <- rasters[[id]]
    if (is.null(x)) return(numeric())
    seq.int(0, by = catalog[[id]]$interval, length.out = terra::nlyr(x))
  })
  names(raster_horizons) <- names(catalog)

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

  fire_path <- file.path(data_dir, "bdq_focos.rds")
  if (!file.exists(fire_path)) fire_path <- file.path("data", "bdq_focos.rds")
  fires <- if (file.exists(fire_path)) readRDS(fire_path) else NULL

  db_path <- file.path(data_dir, "cams_forecast.duckdb")
  if (!file.exists(db_path)) db_path <- file.path("data", "cams_forecast.duckdb")
  con <- if (file.exists(db_path)) {
    DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  } else NULL
  tables <- if (is.null(con)) character() else DBI::dbListTables(con)

  image_cache <- cachem::cache_mem(max_size = 192 * 1024^2)
  query_cache <- cachem::cache_mem(max_size = 64 * 1024^2)
  image_dir <- tempfile("alertar-raster-cache-")
  dir.create(image_dir, recursive = TRUE)
  shiny::addResourcePath("forecast-images", image_dir)

  raster_image <- function(id, horizon) {
    cfg <- catalog[[id]]
    x <- rasters[[id]]
    actual_horizon <- normalize_horizon(id, horizon)
    index <- match(actual_horizon, raster_horizons[[id]])
    key <- paste0("image", id, "layer", index)
    if (image_cache$exists(key)) return(image_cache$get(key))

    layer <- x[[index]] * cfg$scale + cfg$offset
    values <- terra::as.matrix(layer, wide = TRUE)
    alpha <- ifelse(is.finite(values), 0.82, 0)

    if (is.null(cfg$breaks)) {
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

    png_path <- file.path(image_dir, sprintf("%s-%03d.png", id, index))
    png::writePNG(image, png_path)
    extent <- terra::ext(layer)
    result <- list(
      url = sprintf(
        "forecast-images/%s?v=%s",
        basename(png_path),
        as.integer(file.info(png_path)$mtime)
      ),
      coordinates = list(
        unname(c(extent$xmin, extent$ymax)),
        unname(c(extent$xmax, extent$ymax)),
        unname(c(extent$xmax, extent$ymin)),
        unname(c(extent$xmin, extent$ymin))
      ),
      index = index,
      horizon = actual_horizon
    )
    image_cache$set(key, result)
    result
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
      series_scale <- if (is.null(cfg$series_scale)) 1 else cfg$series_scale
      series_offset <- if (is.null(cfg$series_offset)) 0 else cfg$series_offset
      result$value <- result$value * series_scale + series_offset
    }
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

  wind_available <- any(file.exists(file.path(data_dir, paste0("wind_", c(1, 121), ".json"))))
  if (wind_available) shiny::addResourcePath("wind-data", data_dir)

  list(
    data_dir = data_dir,
    catalog = catalog,
    coverage = coverage,
    available = available,
    rasters = rasters,
    territories = territories,
    fires = fires,
    raster_image = raster_image,
    forecast_horizons = function(id) raster_horizons[[id]],
    normalize_horizon = normalize_horizon,
    future_horizons = future_horizons,
    query_series = query_series,
    analysis_time = analysis_time,
    territory_geometry = territory_geometry,
    wind_available = wind_available,
    wind_url = function(horizon) sprintf("wind-data/wind_%d.json", horizon + 1L),
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
