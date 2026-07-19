diurnal_fill <- function(time, timezone) {
  timezone <- normalize_timezone(timezone)
  local_hour <- as.numeric(format(time, "%H", tz = timezone)) +
    as.numeric(format(time, "%M", tz = timezone)) / 60
  daylight <- (cos((local_hour - 12) * pi / 12) + 1) / 2
  night_rgb <- c(40, 55, 68)
  day_rgb <- c(190, 155, 55)
  red <- night_rgb[[1]] + daylight * (day_rgb[[1]] - night_rgb[[1]])
  green <- night_rgb[[2]] + daylight * (day_rgb[[2]] - night_rgb[[2]])
  blue <- night_rgb[[3]] + daylight * (day_rgb[[3]] - night_rgb[[3]])
  grDevices::rgb(red / 255, green / 255, blue / 255, alpha = .12 + daylight * .02)
}

violation_color <- function(value, threshold, upper) {
  palette <- grDevices::colorRampPalette(c("#eb9850", "#e47250", "#da506f"))(64)
  span <- max(.Machine$double.eps, upper - threshold)
  severity <- pmax(0, pmin(1, (value - threshold) / span))
  palette[[floor(severity * (length(palette) - 1L)) + 1L]]
}

draw_threshold_series <- function(x, y, threshold = NA_real_, upper, lwd = 2.2) {
  lines(x, y, col = "#35d4b4", lwd = lwd)
  if (!length(threshold) || !is.finite(threshold) || length(x) < 2L) return(invisible(NULL))

  x_numeric <- as.numeric(x)
  for (i in seq_len(length(x_numeric) - 1L)) {
    x_pair <- x_numeric[c(i, i + 1L)]
    y_pair <- y[c(i, i + 1L)]
    if (!all(is.finite(c(x_pair, y_pair))) || max(y_pair) <= threshold) next
    color <- violation_color(max(y_pair), threshold, upper)
    if (min(y_pair) >= threshold) {
      lines(x_pair, y_pair, col = color, lwd = lwd)
      next
    }
    crossing <- x_pair[[1]] +
      (threshold - y_pair[[1]]) / diff(y_pair) * diff(x_pair)
    if (y_pair[[1]] > threshold) {
      lines(c(x_pair[[1]], crossing), c(y_pair[[1]], threshold), col = color, lwd = lwd)
    } else {
      lines(c(crossing, x_pair[[2]]), c(threshold, y_pair[[2]]), col = color, lwd = lwd)
    }
  }
  invisible(NULL)
}

local_midnight_ticks <- function(time, timezone) {
  timezone <- normalize_timezone(timezone)
  local_time <- in_timezone(time, timezone)
  time_range <- range(as.numeric(local_time))
  dates <- seq(
    as.Date(min(local_time), tz = timezone),
    as.Date(max(local_time), tz = timezone),
    by = "day"
  )
  ticks <- as.POSIXct(
    paste(dates, "00:00:00"),
    format = "%Y-%m-%d %H:%M:%S", tz = timezone
  )
  ticks[as.numeric(ticks) >= time_range[[1]] & as.numeric(ticks) <= time_range[[2]]]
}

forecast_y_range <- function(data, cfg) {
  configured <- as.numeric(cfg$range)
  values <- data$value[is.finite(data$value)]
  if (!length(values)) return(configured)

  observed <- range(values)
  if (observed[[1]] >= configured[[1]] && observed[[2]] <= configured[[2]]) return(configured)
  configured_span <- max(.Machine$double.eps, diff(configured))
  padding <- max(configured_span * .04, max(abs(observed)) * .04)
  candidate <- c(
    if (observed[[1]] < configured[[1]]) observed[[1]] - padding else configured[[1]],
    if (observed[[2]] > configured[[2]]) observed[[2]] + padding else configured[[2]]
  )
  candidate
}

draw_forecast_plot <- function(data, cfg, references, language, timezone, target, large = FALSE) {
  y_range <- forecast_y_range(data, cfg)
  selected_index <- which.min(abs(as.numeric(data$date - target)))
  selected_y <- pmax(y_range[[1]], pmin(y_range[[2]], data$value[[selected_index]]))
  y_ticks <- pretty(y_range, n = if (large) 5 else 3)
  y_ticks <- y_ticks[y_ticks >= y_range[[1]] & y_ticks <= y_range[[2]]]
  y_labels <- format(
    round(y_ticks, cfg$digits), trim = TRUE, scientific = FALSE,
    decimal.mark = if (language == "en") "." else ","
  )
  unit_label <- localized_unit(cfg$unit, language)
  display_dates <- in_timezone(data$date, timezone)
  display_target <- in_timezone(target, timezone)
  x_ticks <- local_midnight_ticks(display_dates, timezone)
  reference_values <- vapply(references, function(reference) reference$value, numeric(1))
  reference_values <- reference_values[
    is.finite(reference_values) & reference_values >= y_range[[1]] & reference_values <= y_range[[2]]
  ]
  threshold <- if (length(reference_values)) min(reference_values) else NA_real_

  par(
    mar = if (large) c(3.1, 2.45, .8, .7) else c(2.2, 2.2, .7, .35),
    bg = NA, fg = "#7890a0"
  )
  plot(
    display_dates, data$value,
    type = "n", ylim = y_range, axes = FALSE,
    xlab = "", ylab = "", xaxs = "i", yaxs = "i"
  )
  shade_range <- range(as.numeric(display_dates))
  shade_steps <- max(2L, ceiling(diff(shade_range) / (30 * 60)))
  shade_breaks <- seq(shade_range[[1]], shade_range[[2]], length.out = shade_steps + 1L)
  shade_midpoints <- in_timezone(
    (head(shade_breaks, -1L) + tail(shade_breaks, -1L)) / 2,
    timezone
  )
  rect(
    head(shade_breaks, -1L), par("usr")[[3]],
    tail(shade_breaks, -1L), par("usr")[[4]],
    col = diurnal_fill(shade_midpoints, timezone), border = NA
  )
  abline(h = y_ticks, col = "#7890a022", lwd = .7)
  abline(v = x_ticks, col = "#7890a014", lwd = .7)
  for (reference in references) {
    if (reference$value < y_range[[1]] || reference$value > y_range[[2]]) next
    abline(h = reference$value, col = reference$color, lwd = if (large) 1.35 else 1.15, lty = 2)
    label_x <- par("usr")[[2]] - diff(par("usr")[1:2]) * .012
    text(
      label_x, reference$value, reference$label,
      adj = c(1, -.25), col = reference$color,
      cex = if (large) .72 else .56, font = 2, xpd = FALSE
    )
  }
  draw_threshold_series(
    display_dates, data$value, threshold = threshold,
    upper = y_range[[2]], lwd = if (large) 2.7 else 2.2
  )
  selected_color <- if (is.finite(threshold) && selected_y > threshold) {
    violation_color(selected_y, threshold, y_range[[2]])
  } else "#35d4b4"
  abline(v = display_target, col = "#f8fafc66", lty = 3)
  points(
    display_target, selected_y, pch = 21, bg = selected_color,
    col = "white", cex = if (large) 1.25 else 1.1
  )
  time_format <- if (language == "en") "%m/%d" else "%d/%m"
  axis.POSIXct(
    1, x = display_dates, at = x_ticks, format = time_format,
    col = NA, col.axis = "#7890a0", cex.axis = if (large) .76 else .62, padj = .2
  )
  axis(
    2, at = y_ticks, labels = y_labels, las = 1,
    col = NA, col.axis = "#7890a0", cex.axis = if (large) .74 else .62,
    mgp = c(0, if (large) .3 else .18, 0)
  )
  mtext(
    unit_label, side = 2, line = if (large) 1.6 else 1.35,
    col = "#7890a0", cex = if (large) .72 else .58
  )
  invisible(NULL)
}

report_category_specs <- function(cfg, language) {
  breaks <- cfg$breaks %||% numeric()
  if (length(breaks) < 2L) return(list())
  Map(function(index, lower, upper, color) {
    format_bound <- function(value) report_format_number(value, cfg$digits, language)
    label <- if (!is.finite(lower)) {
      paste0("< ", format_bound(upper))
    } else if (!is.finite(upper)) {
      paste0("≥ ", format_bound(lower))
    } else {
      paste0(format_bound(lower), "–< ", format_bound(upper))
    }
    list(
      column = sprintf("category_%d_hours", index),
      label = paste(label, report_unit_text(cfg$unit, language)),
      color = color
    )
  }, seq_len(length(breaks) - 1L), head(breaks, -1L), tail(breaks, -1L), cfg$colors)
}

rank_report_units <- function(data, categories, language) {
  if (!nrow(data)) return(data)
  data <- data[order(-data$maximum_value, data$display_name), , drop = FALSE]
  data$rank <- seq_len(nrow(data))
  if (length(categories)) {
    columns <- vapply(categories, `[[`, character(1), "column")
    category_hours <- as.matrix(data[, columns, drop = FALSE])
    category_hours[!is.finite(category_hours)] <- 0
    dominant <- max.col(category_hours, ties.method = "first")
    data$dominant_band <- vapply(categories[dominant], `[[`, character(1), "label")
    data$dominant_color <- vapply(categories[dominant], `[[`, character(1), "color")
  } else {
    data$dominant_band <- tr(language, "report_no_reference")
    data$dominant_color <- "#7890a0"
  }
  rownames(data) <- NULL
  data
}

app_server <- function(store, glm_store = NULL) {
  force(store)
  function(input, output, session) {
    catalog <- store$catalog
    weather_catalog <- weather_observation_catalog()
    territories <- store$territories
    current_language <- reactive(normalize_language(input$language))
    current_timezone <- reactive(normalize_timezone(input$timezone))
    data_revision <- reactive(store$revision())
    totem_active <- reactiveVal(FALSE)
    playing_before_totem <- reactiveVal(FALSE)
    totem_last_refresh <- reactiveVal(Sys.time())
    totem_refresh_hours <- suppressWarnings(as.numeric(
      Sys.getenv("ALERTAR_TOTEM_REFRESH_HOURS", unset = "3")
    ))
    if (!is.finite(totem_refresh_hours) || totem_refresh_hours <= 0) totem_refresh_hours <- 3
    totem_live_refresh_minutes <- suppressWarnings(as.numeric(
      Sys.getenv("ALERTAR_TOTEM_LIVE_REFRESH_MINUTES", unset = "10")
    ))
    if (!is.finite(totem_live_refresh_minutes) || totem_live_refresh_minutes <= 0) {
      totem_live_refresh_minutes <- 10
    }
    totem_live_refresh_trigger <- reactiveVal(0L)

    municipality_types <- tolower(as.character(territories$territory_type))
    municipality_ids <- unique(as.character(territories$territory_id[
      municipality_types %in% c("municipio", "município", "municipality", "commune")
    ]))
    municipality_ids <- municipality_ids[!is.na(municipality_ids) & nzchar(municipality_ids)]
    localized_indicator_choices <- function(language) {
      stats::setNames(
        store$available,
        vapply(
          store$available,
          function(id) indicator_text(language, id, "short", catalog[[id]]$short),
          character(1)
        )
      )
    }
    localized_territory_choices <- function(language) {
      types <- as.character(territories$territory_type)
      is_municipality <- tolower(types) %in% c("municipio", "município", "municipality", "commune")
      types[is_municipality] <- tr(language, "territory_municipality")
      lapply(
        split(seq_len(nrow(territories)), types),
        function(i) stats::setNames(territories$territory_id[i], territories$display_name[i])
      )
    }
    localized_weather_source_choices <- function(language) {
      stats::setNames(
        names(weather_catalog),
        vapply(weather_catalog, function(source) tr(language, source$label_key), character(1))
      )
    }
    localized_weather_product_choices <- function(source_id, language) {
      source_id <- if (source_id %in% names(weather_catalog)) source_id else names(weather_catalog)[[1]]
      products <- weather_catalog[[source_id]]$products
      stats::setNames(
        names(products),
        vapply(products, function(product) tr(language, product$label_key), character(1))
      )
    }
    default_territory <- if ("330455" %in% territories$territory_id) "330455" else territories$territory_id[[1]]
    map_ready <- reactiveVal(FALSE)
    displayed_horizon <- reactiveVal(12)
    pending_raster <- reactiveVal(NULL)
    lightning_snapshot <- reactiveVal(NULL)
    lightning_loading <- reactiveVal(FALSE)
    raster_request_sequence <- 0L
    lightning_request_sequence <- 0L

    session$onFlushed(function() {
      language <- isolate(current_language())
      updateSelectizeInput(
        session, "territory",
        choices = localized_territory_choices(language), selected = default_territory,
        options = list(placeholder = tr(language, "territory_placeholder")), server = TRUE
      )
    }, once = TRUE)

    output$forecast_map <- mapgl::renderMaplibre({
      initial_id <- isolate(input$indicator %||% store$default_indicator)
      initial_horizon <- isolate(input$horizon %||% 12)
      initial_image <- store$raster_image(initial_id, initial_horizon)
      displayed_horizon(initial_image$horizon)

      map <- mapgl::maplibre(
        style = mapgl::carto_style("dark-matter"),
        center = store$coverage$center, zoom = store$coverage$zoom, projection = "globe",
        attributionControl = list(compact = TRUE)
      ) |>
        mapgl::add_globe_control(position = "top-right") |>
        mapgl::add_navigation_control(position = "top-right", visualize_pitch = TRUE) |>
        mapgl::add_scale_control(position = "bottom-right", unit = "metric") |>
        mapgl::add_image_source(
          id = "forecast", url = initial_image$url,
          coordinates = initial_image$coordinates
        ) |>
        mapgl::add_raster_layer(
          id = "forecast", source = "forecast", raster_opacity = .82,
          raster_fade_duration = 120, raster_resampling = "linear"
        )

      fires <- store$fires()
      if (!is.null(fires) && nrow(fires)) {
        fires <- sf::st_as_sf(fires, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
        map <- map |>
          mapgl::add_circle_layer(
            id = "fires", source = fires, circle_radius = 2.5,
            circle_color = "#ff3b30", circle_opacity = .92,
            circle_stroke_width = 0,
            visibility = "none"
          )
      }
      map
    })

    selected_config <- reactive({
      req(input$indicator)
      catalog[[input$indicator]]
    })

    selected_horizon <- reactive({
      req(input$indicator, input$horizon)
      store$normalize_horizon(input$indicator, input$horizon)
    })

    observeEvent(input$indicator, {
      req(input$indicator)
      horizons <- store$forecast_horizons(input$indicator)
      req(length(horizons))
      current <- isolate(input$horizon %||% 12)
      updateSliderInput(
        session,
        "horizon",
        min = min(horizons),
        max = max(horizons),
        step = catalog[[input$indicator]]$interval,
        value = store$normalize_horizon(input$indicator, current)
      )
    }, ignoreInit = FALSE)

    layer_request <- reactive({
      req(input$indicator, input$horizon)
      list(id = input$indicator, horizon = selected_horizon(), revision = data_revision())
    }) |> debounce(50)

    update_raster <- function(id, horizon, revision = data_revision()) {
      image <- store$raster_image(id, horizon)
      raster_request_sequence <<- raster_request_sequence + 1L
      token <- paste(id, image$horizon, revision, raster_request_sequence, sep = "-")
      pending_raster(list(token = token, id = id, horizon = image$horizon))
      session$sendCustomMessage(
        "alertar:raster",
        list(
          mapId = session$ns("forecast_map"),
          url = image$url,
          coordinates = image$coordinates,
          token = token
        )
      )
    }

    animation_wind_horizon <- 60

    effective_wind_horizon <- function(horizon) {
      if (isTRUE(isolate(playing()))) animation_wind_horizon else horizon
    }

    send_wind_update <- function(horizon) {
      if (!isTRUE(isolate(map_ready()))) return(invisible(FALSE))
      wind_horizon <- effective_wind_horizon(horizon)
      show_wind <- isolate(input$show_wind)
      wind_enabled <- if (is.null(show_wind)) store$wind_available() else isTRUE(show_wind)
      session$sendCustomMessage(
        "alertar:wind",
        list(
          mapId = session$ns("forecast_map"),
          active = wind_enabled && store$wind_available(),
          url = if (store$wind_available()) store$wind_url(wind_horizon) else NULL
        )
      )
      invisible(TRUE)
    }

    selected_weather_source <- function() {
      id <- isolate(input$weather_source %||% names(weather_catalog)[[1]])
      if (!id %in% names(weather_catalog)) id <- names(weather_catalog)[[1]]
      list(id = id, config = weather_catalog[[id]])
    }

    selected_weather_product <- function(source) {
      id <- isolate(input$weather_product %||% names(source$config$products)[[1]])
      if (!id %in% names(source$config$products)) id <- names(source$config$products)[[1]]
      list(id = id, config = source$config$products[[id]])
    }

    send_weather_update <- function() {
      if (!isTRUE(isolate(map_ready()))) return(invisible(FALSE))
      source <- selected_weather_source()
      product <- selected_weather_product(source)
      refresh_bucket <- floor(as.numeric(Sys.time()) / (source$config$refresh_minutes * 60))
      session$sendCustomMessage(
        "alertar:weather-observation",
        list(
          mapId = session$ns("forecast_map"),
          timeInputId = session$ns("weather_observation_time"),
          active = isTRUE(isolate(input$show_weather)),
          url = weather_observation_tile_url(source$config, product$config),
          sourceId = source$id,
          productId = product$id,
          refreshKey = refresh_bucket,
          maxzoom = product$config$maxzoom %||% source$config$maxzoom,
          opacity = product$config$opacity %||% source$config$opacity %||% .78,
          attribution = source$config$provider
        )
      )
      invisible(TRUE)
    }

    send_fire_update <- function() {
      if (!isTRUE(isolate(map_ready()))) return(invisible(FALSE))
      fires <- store$fires()
      if (is.null(fires) || !nrow(fires) || !all(c("lon", "lat") %in% names(fires))) {
        longitude <- latitude <- numeric()
      } else {
        longitude <- suppressWarnings(as.numeric(fires$lon))
        latitude <- suppressWarnings(as.numeric(fires$lat))
        valid <- is.finite(longitude) & is.finite(latitude)
        longitude <- longitude[valid]
        latitude <- latitude[valid]
      }
      session$sendCustomMessage(
        "alertar:fire-data",
        list(
          mapId = session$ns("forecast_map"),
          layerId = "fires",
          active = isTRUE(isolate(input$show_fires)),
          lon = unname(longitude),
          lat = unname(latitude)
        )
      )
      invisible(TRUE)
    }

    preload_generation <- 0L
    preload_indicator <- NULL
    preload_revision <- NULL
    preload_last_horizon <- NULL
    preload_jobs <- new.env(parent = emptyenv())
    preload_completed <- new.env(parent = emptyenv())

    reset_raster_preload <- function(id, horizon, revision) {
      preload_generation <<- preload_generation + 1L
      preload_indicator <<- id
      preload_revision <<- revision
      preload_last_horizon <<- horizon
      rm(list = ls(envir = preload_jobs, all.names = TRUE), envir = preload_jobs)
      rm(list = ls(envir = preload_completed, all.names = TRUE), envir = preload_completed)
    }

    schedule_raster_preload <- function(id, horizon, frames = 16L, revision = data_revision()) {
      horizon <- store$normalize_horizon(id, horizon)
      restart <- !identical(id, preload_indicator) ||
        !identical(revision, preload_revision) ||
        (!is.null(preload_last_horizon) && is.finite(horizon) && horizon < preload_last_horizon)
      if (restart) reset_raster_preload(id, horizon, revision)
      preload_last_horizon <<- horizon

      generation <- preload_generation
      preload_horizons <- store$future_horizons(id, horizon, frames)
      queue_index <- 0L
      for (i in seq_along(preload_horizons)) {
        preload_horizon <- preload_horizons[[i]]
        key <- paste(id, preload_horizon, revision, sep = "-")
        if (exists(key, envir = preload_jobs, inherits = FALSE) ||
            exists(key, envir = preload_completed, inherits = FALSE)) next

        queue_index <- queue_index + 1L
        assign(key, TRUE, envir = preload_jobs)
        local({
          job_id <- id
          job_key <- key
          job_horizon <- preload_horizon
          job_generation <- generation
          preload_delay <- 0.05 + (queue_index - 1L) * 0.24
          later::later(function() {
            if (job_generation != preload_generation) return(invisible(NULL))
            promise <- store$raster_image_async(job_id, job_horizon)
            promises::then(
              promise,
              onFulfilled = function(image) {
                if (exists(job_key, envir = preload_jobs, inherits = FALSE)) {
                  rm(list = job_key, envir = preload_jobs)
                }
                if (session$isClosed() || job_generation != preload_generation || is.null(image)) {
                  return(invisible(NULL))
                }
                assign(job_key, TRUE, envir = preload_completed)
                session$sendCustomMessage(
                  "alertar:preload",
                  list(
                    rasterUrls = list(image$url),
                    windUrls = if (store$wind_available() && !isTRUE(isolate(playing()))) {
                      Filter(Negate(is.null), list(store$wind_url(job_horizon)))
                    } else list()
                  )
                )
                invisible(NULL)
              },
              onRejected = function(error) {
                if (exists(job_key, envir = preload_jobs, inherits = FALSE)) {
                  rm(list = job_key, envir = preload_jobs)
                }
                invisible(NULL)
              }
            )
          }, delay = preload_delay)
        })
      }
      invisible(NULL)
    }

    session$onSessionEnded(function() {
      preload_generation <<- preload_generation + 1L
    })

    observeEvent(layer_request(), {
      req(map_ready())
      request <- layer_request()
      update_raster(request$id, request$horizon, request$revision)
      schedule_raster_preload(request$id, request$horizon)
      if (!isTRUE(isolate(playing()))) send_wind_update(request$horizon)
    }, ignoreInit = TRUE)

    observeEvent(
      list(map_ready(), input$show_wind),
      {
        req(map_ready())
        send_wind_update(isolate(selected_horizon()))
      },
      ignoreInit = FALSE, ignoreNULL = FALSE
    )

    observeEvent(
      list(map_ready(), input$forecast_transparency),
      {
        req(map_ready())
        transparency <- suppressWarnings(as.numeric(input$forecast_transparency %||% 18))
        if (!is.finite(transparency)) transparency <- 18
        transparency <- max(0, min(100, transparency))
        session$sendCustomMessage(
          "alertar:forecast-opacity",
          list(
            mapId = session$ns("forecast_map"),
            opacity = 1 - transparency / 100
          )
        )
      },
      ignoreInit = FALSE, ignoreNULL = FALSE
    )

    weather_refresh_minutes <- min(vapply(
      weather_catalog, function(source) source$refresh_minutes, numeric(1)
    ))
    weather_refresh <- reactiveTimer(weather_refresh_minutes * 60 * 1000, session)
    observe({
      weather_refresh()
      totem_live_refresh_trigger()
      req(map_ready())
      input$show_weather
      input$weather_source
      input$weather_product
      send_weather_update()
    })

    lightning_refresh <- reactiveTimer(60 * 1000, session)
    observe({
      lightning_refresh()
      totem_live_refresh_trigger()
      req(map_ready())
      active <- isTRUE(input$show_lightning)
      lightning_request_sequence <<- lightning_request_sequence + 1L
      request_sequence <- lightning_request_sequence
      if (!active || is.null(glm_store)) {
        lightning_loading(FALSE)
        lightning_snapshot(NULL)
        session$sendCustomMessage(
          "alertar:lightning",
          list(mapId = session$ns("forecast_map"), active = FALSE)
        )
        return()
      }

      lightning_loading(TRUE)
      promise <- glm_store$refresh_async()
      promises::then(
        promise,
        onFulfilled = function(snapshot) {
          if (session$isClosed() || request_sequence != lightning_request_sequence) {
            return(invisible(NULL))
          }
          lightning_loading(FALSE)
          if (!isTRUE(isolate(input$show_lightning))) return(invisible(NULL))
          lightning_snapshot(snapshot)
          flashes <- snapshot$flashes
          session$sendCustomMessage(
            "alertar:lightning",
            list(
              mapId = session$ns("forecast_map"),
              active = TRUE,
              windowSeconds = 5 * 60,
              flashes = list(
                lon = unname(flashes$lon),
                lat = unname(flashes$lat),
                energy = unname(flashes$energy),
                observedAt = unname(flashes$observed_at)
              )
            )
          )
          invisible(NULL)
        },
        onRejected = function(error) {
          if (!session$isClosed() && request_sequence == lightning_request_sequence) {
            lightning_loading(FALSE)
            lightning_snapshot(list(
              flashes = data.frame(), latest = as.POSIXct(NA), updated = Sys.time(),
              error = conditionMessage(error)
            ))
          }
          invisible(NULL)
        }
      )
    })

    observeEvent(
      list(map_ready(), input$show_fires),
      {
        req(map_ready())
        send_fire_update()
      },
      ignoreInit = FALSE, ignoreNULL = FALSE
    )

    update_selected_territory <- function() {
      req(map_ready(), input$territory)
      territory <- store$territory_geometry(input$territory)
      req(nrow(territory) == 1)
      proxy <- mapgl::maplibre_proxy("forecast_map", session)
      try(mapgl::clear_layer(proxy, "selected-territory-outline"), silent = TRUE)
      try(mapgl::clear_layer(proxy, "selected-territory"), silent = TRUE)
      geometry_type <- as.character(sf::st_geometry_type(territory, by_geometry = TRUE)[[1]])
      if (geometry_type %in% c("POINT", "MULTIPOINT")) {
        proxy |>
          mapgl::add_circle_layer(
            id = "selected-territory", source = territory,
            circle_radius = 7, circle_color = "#f8fafc",
            circle_opacity = .98, circle_stroke_color = "#35d4b4",
            circle_stroke_width = 3
          )
      } else {
        proxy |>
          mapgl::add_fill_layer(
            id = "selected-territory", source = territory,
            fill_color = "#35d4b4", fill_opacity = .14
          ) |>
          mapgl::add_line_layer(
            id = "selected-territory-outline", source = "selected-territory",
            line_color = "#f8fafc", line_width = 2.5, line_opacity = .95
          )
      }
      if (isTRUE(isolate(totem_active()))) {
        focus <- suppressWarnings(sf::st_point_on_surface(sf::st_union(sf::st_geometry(territory))))
        coordinates <- sf::st_coordinates(focus)
        if (nrow(coordinates)) {
          mapgl::fly_to(
            proxy,
            center = unname(coordinates[1, c("X", "Y")]),
            zoom = 5.2,
            duration = 2200,
            essential = TRUE
          )
        }
      }
    }

    observeEvent(input$territory, update_selected_territory(), ignoreInit = TRUE)

    observeEvent(input$forecast_map_bbox, {
      map_ready(TRUE)
      req(input$indicator, input$horizon)
      horizon <- selected_horizon()
      update_raster(input$indicator, horizon)
      schedule_raster_preload(input$indicator, horizon)
      update_selected_territory()
      send_weather_update()
      send_fire_update()
      if (store$wind_available()) {
        session$sendCustomMessage(
          "alertar:preload",
          list(rasterUrls = list(), windUrls = Filter(Negate(is.null), list(store$wind_url(animation_wind_horizon))))
        )
      }
      send_wind_update(horizon)
      later::later(function() {
        if (session$isClosed() || !isTRUE(isolate(map_ready()))) return(invisible(NULL))
        send_wind_update(isolate(selected_horizon()))
      }, delay = 0.75)
      session$sendCustomMessage(
        "alertar:language",
        list(
          mapId = session$ns("forecast_map"),
          language = map_language_code(current_language())
        )
      )
      language <- current_language()
      session$sendCustomMessage(
        "alertar:interface",
        list(
          mapControls = stats::setNames(
            list(
              tr(language, "map_zoom_in"), tr(language, "map_zoom_out"),
              tr(language, "map_reset_bearing"), tr(language, "map_toggle_projection")
            ),
            c(".maplibregl-ctrl-zoom-in", ".maplibregl-ctrl-zoom-out", ".maplibregl-ctrl-compass", ".maplibregl-ctrl-globe")
          )
        )
      )
    }, once = TRUE)

    series <- reactive({
      req(input$indicator, input$territory)
      data_revision()
      store$query_series(input$indicator, input$territory)
    }) |> bindCache(input$indicator, input$territory, data_revision())

    forecast_origin <- reactive({
      data_revision()
      store$analysis_time(input$indicator)
    }) |> bindCache(input$indicator, data_revision())

    selected_value <- reactive({
      data <- series()
      if (!nrow(data)) return(NA_real_)
      target <- forecast_origin() + displayed_horizon() * 3600
      data$value[[which.min(abs(as.numeric(difftime(data$date, target, units = "secs"))))]]
    })

    territorial_report_data <- reactive({
      req(input$indicator, input$territory)
      data_revision()
      language <- current_language()
      id <- input$indicator
      cfg <- catalog[[id]]
      territory <- territories[territories$territory_id == input$territory, , drop = FALSE]
      req(nrow(territory) == 1L)
      metrics <- store$query_report_metrics(id)
      if (!nrow(metrics)) return(list(available = FALSE))

      selected_type <- tolower(as.character(territory$territory_type[[1]]))
      selected_country <- as.character(territory$country_code[[1]])
      selected_state <- as.character(territory$admin1_code[[1]])
      comparable <- metrics[
        tolower(metrics$territory_type) == selected_type &
          metrics$country_code == selected_country,
        , drop = FALSE
      ]
      if (!nrow(comparable)) return(list(available = FALSE))

      categories <- report_category_specs(cfg, language)
      national <- rank_report_units(comparable, categories, language)
      state <- comparable[
        nzchar(selected_state) & comparable$admin1_code == selected_state,
        , drop = FALSE
      ]
      state <- rank_report_units(state, categories, language)
      selected_index <- match(as.character(input$territory), national$territory_id)
      if (is.na(selected_index)) return(list(available = FALSE))
      selected <- national[selected_index, , drop = FALSE]
      selected$national_rank <- national$rank[[selected_index]]
      state_index <- match(as.character(input$territory), state$territory_id)
      selected$state_rank <- if (is.na(state_index)) NA_integer_ else state$rank[[state_index]]

      references <- Map(
        function(reference, index) localized_reference(language, id, reference, index),
        cfg$references %||% list(), seq_along(cfg$references %||% list())
      )
      reference <- if (length(references)) references[[1]] else NULL
      territory_type_label <- as.character(territory$territory_type[[1]])
      if (selected_type %in% c("municipio", "município", "municipality", "commune")) {
        territory_type_label <- tr(language, "territory_municipality")
      }

      list(
        available = TRUE,
        id = id,
        cfg = cfg,
        territory = sf::st_drop_geometry(territory),
        territory_type_label = territory_type_label,
        selected_id = as.character(input$territory),
        selected = selected,
        national = national,
        state = state,
        categories = categories,
        reference_value = if (is.null(reference)) NA_real_ else as.numeric(reference$value),
        reference_label = if (is.null(reference)) "" else reference$label,
        reference_note = if (is.null(reference)) "" else reference_note(language, id, cfg$reference_note %||% ""),
        country_label = as.character(territory$country_name[[1]]),
        state_label = if (nzchar(selected_state)) selected_state else "—",
        period = c(selected$period_start[[1]], selected$period_end[[1]]),
        generated_at = Sys.time()
      )
    }) |> bindCache(input$indicator, input$territory, current_language(), data_revision())

    output$update_badge <- renderUI({
      language <- current_language()
      timezone <- current_timezone()
      time <- in_timezone(forecast_origin(), timezone)
      date_format <- if (language == "en") "%Y-%m-%d • %H:%M" else "%d/%m • %H:%M"
      tagList(
        span(class = "status-dot"),
        span(tr(language, "updated", format(time, date_format, tz = timezone), timezone_code(timezone)))
      )
    })

    output$forecast_time <- renderUI({
      language <- current_language()
      timezone <- current_timezone()
      horizon <- displayed_horizon()
      time <- in_timezone(forecast_origin() + horizon * 3600, timezone)
      date_format <- if (language == "en") "%Y-%m-%d · %H:%M" else "%d/%m/%Y · %H:%M"
      tagList(
        strong(class = "forecast-datetime", format(time, date_format, tz = timezone)),
        span(class = "forecast-step", sprintf("+%03dh · %s", horizon, timezone_code(timezone)))
      )
    })

    output$timeline_labels <- renderUI({
      language <- current_language()
      timezone <- current_timezone()
      horizons <- seq(0, 120, by = 24)
      times <- in_timezone(forecast_origin() + horizons * 3600, timezone)
      date_format <- if (language == "en") "%m/%d" else "%d/%m"

      tagList(lapply(seq_along(horizons), function(index) {
        span(
          class = "timeline-date-label",
          `data-horizon` = horizons[[index]],
          style = sprintf("--timeline-position: %.4f%%", 100 * horizons[[index]] / max(horizons)),
          span(class = "timeline-label-date", format(times[[index]], date_format, tz = timezone)),
          span(class = "timeline-label-time", format(times[[index]], "%H:%M", tz = timezone))
        )
      }))
    })

    output$indicator_summary <- renderUI({
      language <- current_language()
      cfg <- selected_config()
      id <- input$indicator
      tagList(
        div(class = "indicator-name", indicator_text(language, id, "label", cfg$label)),
        p(indicator_text(language, id, "description", cfg$description)),
        div(class = "source-label", tr(language, "source_model", cfg$interval))
      )
    })

    output$local_reading <- renderUI({
      req(input$territory)
      language <- current_language()
      timezone <- current_timezone()
      cfg <- selected_config()
      territory <- territories[territories$territory_id == input$territory, , drop = FALSE]
      value <- selected_value()
      forecast_time <- in_timezone(forecast_origin() + displayed_horizon() * 3600, timezone)
      date_format <- if (language == "en") "%Y-%m-%d" else "%d/%m/%Y"
      territory_type <- as.character(territory$territory_type[[1]])
      if (tolower(territory_type) %in% c("municipio", "município", "municipality", "commune")) {
        territory_type <- tr(language, "territory_municipality")
      }
      decimal_mark <- if (language == "en") "." else ","
      tagList(
        div(
          class = "territory-label",
          territory$display_name[[1]],
          span(class = "territory-type", territory_type)
        ),
        div(
          class = "reading-summary",
          div(
            class = "value-row",
            span(class = "reading-value", if (is.finite(value)) format(round(value, cfg$digits), nsmall = cfg$digits, decimal.mark = decimal_mark) else "—"),
            span(class = "reading-unit", pretty_unit(cfg$unit, language))
          ),
          div(
            class = "reading-time",
            icon("clock"),
            tags$time(
              datetime = format(forecast_time, "%Y-%m-%dT%H:%M:%S%z", tz = timezone),
              span(class = "reading-date", format(forecast_time, date_format, tz = timezone)),
              span(class = "reading-hour", paste(format(forecast_time, "%H:%M", tz = timezone), timezone_code(timezone)))
            )
          )
        ),
        div(class = "reading-caption", tr(language, "reading_caption"))
      )
    })

    output$forecast_spark <- renderPlot({
      data <- series()
      language <- current_language()
      validate(need(nrow(data), tr(language, "series_unavailable")))
      cfg <- selected_config()
      id <- input$indicator
      timezone <- current_timezone()
      references <- Map(
        function(reference, index) localized_reference(language, id, reference, index),
        cfg$references %||% list(), seq_along(cfg$references %||% list())
      )
      target <- forecast_origin() + displayed_horizon() * 3600
      draw_forecast_plot(data, cfg, references, language, timezone, target, large = FALSE)
    }, bg = "transparent", res = 110)

    output$forecast_detail_plot <- renderPlot({
      data <- series()
      language <- current_language()
      validate(need(nrow(data), tr(language, "series_unavailable")))
      cfg <- selected_config()
      id <- input$indicator
      timezone <- current_timezone()
      references <- Map(
        function(reference, index) localized_reference(language, id, reference, index),
        cfg$references %||% list(), seq_along(cfg$references %||% list())
      )
      target <- forecast_origin() + displayed_horizon() * 3600
      draw_forecast_plot(data, cfg, references, language, timezone, target, large = TRUE)
    }, bg = "#091720", res = 130)

    output$download_forecast_plot <- downloadHandler(
      filename = function() {
        sprintf("alertar_grafico_%s_%s.png", input$indicator, input$territory)
      },
      content = function(file) {
        data <- series()
        req(nrow(data))
        language <- current_language()
        cfg <- selected_config()
        id <- input$indicator
        timezone <- current_timezone()
        references <- Map(
          function(reference, index) localized_reference(language, id, reference, index),
          cfg$references %||% list(), seq_along(cfg$references %||% list())
        )
        target <- forecast_origin() + displayed_horizon() * 3600
        grDevices::png(file, width = 1800, height = 1000, res = 180, bg = "#091720")
        on.exit(grDevices::dev.off(), add = TRUE)
        draw_forecast_plot(data, cfg, references, language, timezone, target, large = TRUE)
      },
      contentType = "image/png"
    )

    output$forecast_references <- renderUI({
      language <- current_language()
      cfg <- selected_config()
      id <- input$indicator
      references <- Map(
        function(reference, index) localized_reference(language, id, reference, index),
        cfg$references %||% list(), seq_along(cfg$references %||% list())
      )
      if (!length(references)) {
        return(div(
          class = "reference-empty",
          tr(language, "no_reference")
        ))
      }

      tagList(
        div(class = "reference-heading", tr(language, "technical_references")),
        lapply(references, function(reference) {
          tags$a(
            class = "reference-item",
            href = reference$url,
            target = "_blank",
            rel = "noopener noreferrer",
            span(class = "reference-swatch", style = paste0("--reference-color:", reference$color)),
            span(class = "reference-name", reference$label),
            span(class = "reference-detail", reference$detail)
          )
        }),
        p(class = "reference-note", reference_note(language, id, cfg$reference_note %||% ""))
      )
    })

    output$map_legend <- renderUI({
      language <- current_language()
      cfg <- selected_config()
      id <- input$indicator
      labels <- if (isTRUE(cfg$continuous_palette) || is.null(cfg$breaks)) {
        format(seq(cfg$range[[1]], cfg$range[[2]], length.out = 5), trim = TRUE)
      } else {
        finite <- cfg$breaks[is.finite(cfg$breaks)]
        format(c(0, finite), trim = TRUE)
      }
      tagList(
        div(class = "legend-title", indicator_text(language, id, "short", cfg$short), span(pretty_unit(cfg$unit, language))),
        div(class = "legend-gradient", style = paste0("--legend:", paste(cfg$colors, collapse = ","))),
        div(class = "legend-labels", lapply(labels, span))
      )
    })

    output$download_series <- downloadHandler(
      filename = function() sprintf("alertar_%s_%s.csv", input$indicator, input$territory),
      content = function(file) utils::write.csv2(series(), file, row.names = FALSE)
    )

    output$territorial_report <- renderUI({
      territorial_report_view(territorial_report_data(), current_language(), current_timezone())
    })

    output$download_territorial_report <- downloadHandler(
      filename = function() {
        sprintf("alertar_relatorio_%s_%s.html", input$indicator, input$territory)
      },
      content = function(file) {
        html <- territorial_report_html(
          territorial_report_data(), current_language(), current_timezone()
        )
        writeLines(enc2utf8(html), file, useBytes = TRUE)
      },
      contentType = "text/html"
    )

    playing <- reactiveVal(FALSE)
    frame_pending <- reactiveVal(FALSE)
    animation_restarting <- reactiveVal(FALSE)
    totem_cycle_waiting <- reactiveVal(FALSE)
    totem_cycle_generation <- reactiveVal(0L)
    animation_duration_ms <- 30 * 1000
    totem_overview_duration_ms <- 2200
    totem_overview_hold_ms <- 1000
    totem_overview_zoom <- 2.25

    update_play_button <- function() {
      language <- isolate(current_language())
      is_playing <- isolate(playing())
      updateActionButton(
        session,
        "play",
        label = if (is_playing) tr(language, "pause") else tr(language, "animate"),
        icon = icon(if (is_playing) "pause" else "play")
      )
      send_wind_update(isolate(displayed_horizon()))
    }

    select_random_municipality <- function() {
      if (!length(municipality_ids)) return(invisible(NULL))
      current <- as.character(isolate(input$territory %||% ""))
      candidates <- setdiff(municipality_ids, current)
      if (!length(candidates)) candidates <- municipality_ids
      selected <- sample(candidates, 1L)
      selected_index <- match(selected, as.character(territories$territory_id))
      selected_label <- if (is.na(selected_index)) selected else territories$display_name[[selected_index]]
      session$sendCustomMessage(
        "alertar:territory-selection",
        list(
          inputId = session$ns("territory"),
          value = selected,
          label = selected_label
        )
      )
      invisible(selected)
    }

    cancel_totem_cycle <- function() {
      totem_cycle_generation(isolate(totem_cycle_generation()) + 1L)
      totem_cycle_waiting(FALSE)
    }

    schedule_totem_cycle <- function() {
      if (!isTRUE(isolate(totem_active())) || isTRUE(isolate(totem_cycle_waiting()))) {
        return(invisible(FALSE))
      }
      generation <- isolate(totem_cycle_generation()) + 1L
      totem_cycle_generation(generation)
      totem_cycle_waiting(TRUE)

      later::later(function() {
        if (!isTRUE(isolate(totem_active()))) return()
        if (!identical(isolate(totem_cycle_generation()), generation)) return()

        regional_view <- coverage_config("lac")
        try(
          mapgl::fly_to(
            mapgl::maplibre_proxy("forecast_map", session),
            center = regional_view$center,
            zoom = totem_overview_zoom,
            duration = totem_overview_duration_ms,
            essential = TRUE
          ),
          silent = TRUE
        )

        later::later(function() {
          if (!isTRUE(isolate(totem_active()))) return()
          if (!identical(isolate(totem_cycle_generation()), generation)) return()

          select_random_municipality()
          id <- isolate(input$indicator %||% store$default_indicator)
          horizons <- store$forecast_horizons(id)
          totem_cycle_waiting(FALSE)
          frame_pending(FALSE)
          if (length(horizons)) {
            updateSliderInput(session, "horizon", value = min(horizons))
          }
        }, delay = (totem_overview_duration_ms + totem_overview_hold_ms) / 1000)
      }, delay = 5)
      invisible(TRUE)
    }

    observeEvent(input$language, {
      language <- current_language()
      selected_indicator <- isolate(input$indicator %||% store$default_indicator)
      selected_territory <- isolate(input$territory %||% default_territory)

      updateSelectInput(
        session, "indicator",
        choices = localized_indicator_choices(language), selected = selected_indicator
      )
      updateSelectizeInput(
        session, "territory",
        choices = localized_territory_choices(language), selected = selected_territory,
        options = list(placeholder = tr(language, "territory_placeholder")), server = TRUE
      )
      updateCheckboxInput(session, "show_wind", label = tr(language, "wind_particles"))
      updateCheckboxInput(session, "show_fires", label = tr(language, "heat_spots"))
      updateCheckboxInput(session, "show_lightning", label = tr(language, "lightning_flashes"))
      updateSliderInput(session, "forecast_transparency", label = tr(language, "forecast_transparency"))
      updateCheckboxInput(session, "show_weather", label = tr(language, "weather_imagery"))
      selected_weather_source_id <- isolate(input$weather_source %||% names(weather_catalog)[[1]])
      if (!selected_weather_source_id %in% names(weather_catalog)) {
        selected_weather_source_id <- names(weather_catalog)[[1]]
      }
      selected_weather_product_id <- isolate(
        input$weather_product %||% names(weather_catalog[[selected_weather_source_id]]$products)[[1]]
      )
      updateSelectInput(
        session, "weather_source",
        choices = localized_weather_source_choices(language), selected = selected_weather_source_id
      )
      updateSelectInput(
        session, "weather_product",
        choices = localized_weather_product_choices(selected_weather_source_id, language), selected = selected_weather_product_id
      )
      update_play_button()

      session$sendCustomMessage(
        "alertar:interface",
        list(
          language = map_language_code(language),
          title = tr(language, "app_title"),
          territoryPlaceholder = tr(language, "territory_placeholder"),
          timezoneLabel = tr(language, "timezone_label"),
          timezoneOptions = stats::setNames(
            as.list(timezone_full_labels(language)),
            timezone_catalog()$timezone
          ),
          chartLabel = tr(language, "chart_expand"),
          totemToggle = list(
            enter = tr(language, "totem_mode"),
            exit = tr(language, "exit_totem")
          ),
          detailsToggle = list(
            minimize = tr(language, "minimize_panel"),
            restore = tr(language, "restore_panel")
          ),
          text = stats::setNames(
            list(
              tr(language, "history"), tr(language, "reports"), tr(language, "about"),
              tr(language, "layer_heading"), tr(language, "local_heading"),
              tr(language, "download_series"), tr(language, "forecast_horizon"),
              tr(language, "weather_heading"), tr(language, "weather_source"), tr(language, "weather_product"),
              tr(language, "forecast_section"), tr(language, "forecast_badge"),
              tr(language, "forecast_section_note"), tr(language, "forecast_variable"),
              tr(language, "wind_detail"), tr(language, "recent_section"),
              tr(language, "recent_badge"), tr(language, "recent_section_note"),
              tr(language, "lightning_detail"), tr(language, "live_badge"),
              tr(language, "heat_spots_detail"), tr(language, "near_live_badge"),
              tr(language, "weather_detail"), tr(language, "near_live_badge")
            ),
            c(
              "label-history", "label-reports", "label-about", "label-layer-heading", "label-local-heading",
              "label-download", "label-forecast-horizon", "label-weather-heading", "label-weather-source", "label-weather-product",
              "label-forecast-section", "label-forecast-badge", "label-forecast-note", "label-forecast-variable",
              "label-wind-detail", "label-recent-section", "label-recent-badge", "label-recent-note",
              "label-lightning-detail", "label-live-badge", "label-fires-detail", "label-near-live-fires",
              "label-weather-detail", "label-near-live-weather"
            )
          ),
          mapControls = stats::setNames(
            list(
              tr(language, "map_zoom_in"), tr(language, "map_zoom_out"),
              tr(language, "map_reset_bearing"), tr(language, "map_toggle_projection")
            ),
            c(".maplibregl-ctrl-zoom-in", ".maplibregl-ctrl-zoom-out", ".maplibregl-ctrl-compass", ".maplibregl-ctrl-globe")
          )
        )
      )

      if (isTRUE(isolate(map_ready()))) {
        session$sendCustomMessage(
          "alertar:language",
          list(mapId = session$ns("forecast_map"), language = map_language_code(language))
        )
      }
    }, ignoreInit = FALSE)

    observeEvent(input$weather_source, {
      source_id <- input$weather_source
      if (!source_id %in% names(weather_catalog)) return()
      current <- isolate(input$weather_product)
      choices <- localized_weather_product_choices(source_id, current_language())
      selected <- if (current %in% unname(choices)) current else unname(choices)[[1]]
      updateSelectInput(session, "weather_product", choices = choices, selected = selected)
    }, ignoreInit = TRUE)

    output$weather_status <- renderUI({
      language <- current_language()
      timezone <- current_timezone()
      source_id <- input$weather_source %||% names(weather_catalog)[[1]]
      if (!source_id %in% names(weather_catalog)) source_id <- names(weather_catalog)[[1]]
      source <- weather_catalog[[source_id]]
      product_id <- input$weather_product %||% names(source$products)[[1]]
      if (!product_id %in% names(source$products)) product_id <- names(source$products)[[1]]
      product <- source$products[[product_id]]
      observation_input <- input$weather_observation_time
      observation_value <- if (is.list(observation_input)) {
        matches_selection <- identical(as.character(observation_input$sourceId), source_id) &&
          identical(as.character(observation_input$productId), product_id)
        if (matches_selection) observation_input$observedAt else NA_character_
      } else {
        observation_input
      }
      observed_at <- suppressWarnings(as.POSIXct(
        observation_value,
        format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"
      ))
      timestamp <- if (length(observed_at) && !is.na(observed_at)) {
        format(
          in_timezone(observed_at, timezone),
          if (language == "en") "%Y-%m-%d · %H:%M" else "%d/%m/%Y · %H:%M",
          tz = timezone
        )
      } else {
        tr(language, "weather_latest")
      }
      div(
        class = "weather-status", role = "status", `aria-live` = "polite",
        span(class = "weather-live-dot", `aria-hidden` = "true"),
        div(
          tags$strong(timestamp),
          tags$small(tr(language, "weather_refresh", source$refresh_minutes, timezone_code(timezone))),
          if (!is.null(product$detail_key)) tags$small(tr(language, product$detail_key)),
          tags$small(source$provider),
          if (!is.null(product$legend_url)) tags$img(
            class = "weather-product-legend",
            src = product$legend_url,
            alt = tr(language, product$legend_alt_key %||% "weather_legend_alt")
          )
        )
      )
    })

    output$lightning_status <- renderUI({
      language <- current_language()
      timezone <- current_timezone()
      snapshot <- lightning_snapshot()
      if (isTRUE(lightning_loading()) || is.null(snapshot)) {
        return(div(
          class = "lightning-status is-loading", role = "status", `aria-live` = "polite",
          span(class = "lightning-loading-spinner", `aria-hidden` = "true"),
          span(tr(language, "lightning_loading"))
        ))
      }
      if (!nrow(snapshot$flashes) && !is.null(snapshot$error)) {
        return(div(class = "lightning-status is-error", tr(language, "lightning_unavailable")))
      }
      latest <- snapshot$latest
      latest_label <- if (length(latest) && !is.na(latest)) {
        format(
          in_timezone(latest, timezone),
          if (language == "en") "%Y-%m-%d · %H:%M:%S" else "%d/%m/%Y · %H:%M:%S",
          tz = timezone
        )
      } else "—"
      div(
        class = "lightning-status", role = "status", `aria-live` = "polite",
        span(class = "lightning-live-dot", `aria-hidden` = "true"),
        div(
          tags$strong(tr(language, "lightning_count", nrow(snapshot$flashes))),
          tags$small(tr(language, "lightning_latest", latest_label, timezone_code(timezone))),
          tags$small("NOAA GOES-East · GLM-L2-LCFA")
        )
      )
    })
    outputOptions(output, "lightning_status", suspendWhenHidden = FALSE)

    observeEvent(input$totem_mode, {
      active <- isTRUE(input$totem_mode)
      was_active <- isTRUE(isolate(totem_active()))
      if (identical(active, was_active)) return()

      if (active) {
        playing_before_totem(isolate(playing()))
        totem_active(TRUE)
        totem_last_refresh(Sys.time())
        tryCatch(
          store$refresh_fires(force = TRUE),
          error = function(error) warning(
            "Falha na atualização dos focos de calor do modo totem: ",
            conditionMessage(error)
          )
        )
        send_fire_update()
        totem_live_refresh_trigger(isolate(totem_live_refresh_trigger()) + 1L)
        cancel_totem_cycle()
        animation_restarting(FALSE)
        playing(TRUE)
        update_play_button()
        id <- isolate(input$indicator %||% store$default_indicator)
        horizons <- store$forecast_horizons(id)
        if (length(horizons)) {
          frame_pending(FALSE)
          updateSliderInput(session, "horizon", value = min(horizons))
        }
        update_selected_territory()
      } else {
        cancel_totem_cycle()
        totem_active(FALSE)
        playing(isTRUE(isolate(playing_before_totem())))
        update_play_button()
      }
    }, ignoreInit = FALSE)

    observeEvent(input$play, {
      if (isTRUE(isolate(playing()))) {
        playing(FALSE)
        animation_restarting(FALSE)
        if (isTRUE(isolate(totem_active()))) cancel_totem_cycle()
        update_play_button()
        return()
      }

      if (isTRUE(isolate(totem_active()))) {
        playing(TRUE)
        update_play_button()
        return()
      }

      id <- isolate(input$indicator %||% store$default_indicator)
      horizons <- store$forecast_horizons(id)
      current_horizon <- store$normalize_horizon(id, isolate(input$horizon))
      if (length(horizons) && identical(current_horizon, max(horizons))) {
        animation_restarting(TRUE)
        frame_pending(TRUE)
        updateSliderInput(session, "horizon", value = min(horizons))
      } else {
        animation_restarting(FALSE)
      }
      playing(TRUE)
      update_play_button()
    })

    observeEvent(input$forecast_map_raster_ready, {
      result <- input$forecast_map_raster_ready
      pending <- isolate(pending_raster())
      if (is.null(pending) || !identical(as.character(result$token), pending$token)) return()
      frame_pending(FALSE)
      if (identical(result$ok, FALSE)) {
        if (!isTRUE(isolate(totem_active()))) {
          playing(FALSE)
          animation_restarting(FALSE)
        }
        update_play_button()
        showNotification(tr(current_language(), "raster_error"), type = "error")
      } else if (isTRUE(isolate(totem_active()))) {
        displayed_horizon(pending$horizon)
        id <- pending$id
        horizons <- store$forecast_horizons(id)
        current_horizon <- pending$horizon
        if (length(horizons) && identical(current_horizon, max(horizons))) {
          schedule_totem_cycle()
        }
      } else {
        displayed_horizon(pending$horizon)
      }
    })

    observe({
      id <- isolate(input$indicator %||% store$default_indicator)
      horizons <- store$forecast_horizons(id)
      frame_interval <- animation_duration_ms / max(1L, length(horizons) - 1L)
      invalidateLater(max(100, round(frame_interval)), session)
      if (!isolate(playing())) return()
      if (isolate(frame_pending())) return()
      current_horizon <- store$normalize_horizon(id, isolate(input$horizon))
      if (isTRUE(isolate(animation_restarting()))) {
        if (!length(horizons) || !identical(current_horizon, min(horizons))) return()
        animation_restarting(FALSE)
      }
      next_horizon <- store$future_horizons(id, current_horizon, 1L)
      if (!length(next_horizon)) {
        if (isTRUE(isolate(totem_active()))) {
          schedule_totem_cycle()
          return()
        } else {
          playing(FALSE)
          update_play_button()
          return()
        }
      }
      frame_pending(TRUE)
      updateSliderInput(session, "horizon", value = next_horizon[[1]])
    })

    refresh_timer <- reactiveTimer(60 * 1000, session)
    observe({
      refresh_timer()
      if (!isTRUE(isolate(totem_active()))) return()

      now <- Sys.time()
      hours_since_refresh <- as.numeric(difftime(
        now, isolate(totem_last_refresh()), units = "hours"
      ))
      if (!is.finite(hours_since_refresh) || hours_since_refresh < totem_refresh_hours) return()

      refreshed <- tryCatch(store$refresh(), error = function(error) {
        warning("Falha na atualização do modo totem: ", conditionMessage(error))
        FALSE
      })
      if (!isTRUE(refreshed)) return()

      totem_last_refresh(now)
      frame_pending(FALSE)
      id <- isolate(input$indicator %||% store$default_indicator)
      horizons <- store$forecast_horizons(id)
      if (length(horizons)) {
        current <- store$normalize_horizon(id, isolate(input$horizon %||% min(horizons)))
        updateSliderInput(
          session, "horizon",
          min = min(horizons), max = max(horizons),
          step = catalog[[id]]$interval, value = current
        )
      }
      send_fire_update()
      totem_live_refresh_trigger(isolate(totem_live_refresh_trigger()) + 1L)
    })

    live_refresh_timer <- reactiveTimer(totem_live_refresh_minutes * 60 * 1000, session)
    observe({
      live_refresh_timer()
      if (!isTRUE(isolate(totem_active()))) return()

      fires_changed <- tryCatch(
        store$refresh_fires(),
        error = function(error) {
          warning(
            "Falha na atualização periódica dos focos de calor: ",
            conditionMessage(error)
          )
          FALSE
        }
      )
      if (isTRUE(fires_changed)) send_fire_update()

      # Reativa os observadores das fontes quase em tempo real. Cada fonte
      # preserva sua própria janela de cache (GLM, GOES e GPM IMERG).
      totem_live_refresh_trigger(isolate(totem_live_refresh_trigger()) + 1L)
    })

    observeEvent(input$open_history, {
      showModal(historical_data_modal(current_language()))
    }, ignoreInit = TRUE)

    observeEvent(input$open_reports, {
      showModal(territorial_report_modal(current_language()))
    }, ignoreInit = TRUE)

    observeEvent(input$forecast_spark_click, {
      req(input$indicator, input$territory)
      language <- current_language()
      territory <- territories[territories$territory_id == input$territory, , drop = FALSE]
      req(nrow(territory) == 1L)
      title <- paste(
        tr(language, "chart_title"),
        indicator_text(language, input$indicator, "short", catalog[[input$indicator]]$short),
        territory$display_name[[1]],
        sep = " · "
      )
      showModal(forecast_chart_modal(language, title))
    }, ignoreInit = TRUE)

    observeEvent(input$open_about, {
      showModal(about_project_modal(current_language()))
    }, ignoreInit = TRUE)
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x
