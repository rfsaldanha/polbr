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

draw_forecast_plot <- function(data, cfg, references, language, timezone, target, large = FALSE) {
  y_range <- cfg$range
  selected_index <- which.min(abs(as.numeric(data$date - target)))
  selected_y <- pmax(y_range[[1]], pmin(y_range[[2]], data$value[[selected_index]]))
  y_ticks <- pretty(y_range, n = if (large) 5 else 3)
  y_ticks <- y_ticks[y_ticks >= y_range[[1]] & y_ticks <= y_range[[2]]]
  y_labels <- format(
    round(y_ticks, cfg$digits), trim = TRUE, scientific = FALSE,
    decimal.mark = if (language == "en") "." else ","
  )
  unit_label <- switch(cfg$unit, "ug/m3" = "µg/m³", "C" = "°C", cfg$unit)
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

app_server <- function(store) {
  force(store)
  function(input, output, session) {
    catalog <- store$catalog
    territories <- store$territories
    current_language <- reactive(normalize_language(input$language))
    current_timezone <- reactive(normalize_timezone(input$timezone))
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
    default_territory <- if ("330455" %in% territories$territory_id) "330455" else territories$territory_id[[1]]
    map_ready <- reactiveVal(FALSE)

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

      map <- mapgl::maplibre(
        style = mapgl::carto_style("dark-matter"),
        center = store$coverage$center, zoom = store$coverage$zoom, projection = "globe",
        attribution_control = FALSE
      ) |>
        mapgl::set_fog(
          range = c(.6, 8),
          color = "#12242d",
          high_color = "#071018",
          space_color = "#02060a",
          horizon_blend = .16,
          star_intensity = .18
        ) |>
        mapgl::add_globe_control(position = "top-right") |>
        mapgl::add_navigation_control(position = "top-right", visualize_pitch = TRUE) |>
        mapgl::add_scale_control(position = "bottom-right", unit = "metric") |>
        mapgl::add_raster_source(
          id = "satellite",
          tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
          tileSize = 256
        ) |>
        mapgl::add_raster_layer(
          id = "satellite", source = "satellite", raster_opacity = .76,
          visibility = "none"
        ) |>
        mapgl::add_image_source(
          id = "forecast", url = initial_image$url,
          coordinates = initial_image$coordinates
        ) |>
        mapgl::add_raster_layer(
          id = "forecast", source = "forecast", raster_opacity = .82,
          raster_fade_duration = 0, raster_resampling = "linear"
        )

      if (!is.null(store$fires) && nrow(store$fires)) {
        fires <- sf::st_as_sf(store$fires, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
        map <- map |>
          mapgl::add_circle_layer(
            id = "fires", source = fires, circle_radius = 2.5,
            circle_color = "#ff6b35", circle_opacity = .88,
            circle_stroke_color = "#fff1d6", circle_stroke_width = .5,
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
      list(id = input$indicator, horizon = selected_horizon())
    }) |> debounce(100)

    update_raster <- function(id, horizon) {
      image <- store$raster_image(id, horizon)
      token <- paste(id, horizon, sep = "-")
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

    preload_generation <- 0L
    schedule_raster_preload <- function(id, horizon, frames = 4L) {
      preload_generation <<- preload_generation + 1L
      generation <- preload_generation
      preload_horizons <- store$future_horizons(id, horizon, frames)
      for (i in seq_along(preload_horizons)) {
        local({
          preload_horizon <- preload_horizons[[i]]
          preload_delay <- 0.15 + (i - 1L) * 0.35
          later::later(function() {
            if (generation != preload_generation) return(invisible(NULL))
            image <- tryCatch(
              store$raster_image(id, preload_horizon),
              error = function(error) NULL
            )
            if (is.null(image)) return(invisible(NULL))
            session$sendCustomMessage(
              "alertar:preload",
              list(
                rasterUrls = list(image$url),
                windUrls = if (store$wind_available) list(store$wind_url(preload_horizon)) else list()
              )
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
      update_raster(request$id, request$horizon)
      schedule_raster_preload(request$id, request$horizon)
      session$sendCustomMessage(
        "alertar:wind",
        list(
          mapId = session$ns("forecast_map"),
          active = isTRUE(input$show_wind) && store$wind_available,
          url = if (store$wind_available) store$wind_url(request$horizon) else NULL
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(input$show_wind, {
      req(map_ready())
      session$sendCustomMessage(
        "alertar:wind",
        list(
          mapId = session$ns("forecast_map"),
          active = isTRUE(input$show_wind) && store$wind_available,
          url = if (store$wind_available) store$wind_url(selected_horizon()) else NULL
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(input$show_satellite, {
      req(map_ready())
      mapgl::maplibre_proxy("forecast_map", session) |>
        mapgl::set_layout_property("satellite", "visibility", if (isTRUE(input$show_satellite)) "visible" else "none")
    }, ignoreInit = TRUE)

    observe({
      req(map_ready())
      if (is.null(store$fires) || !nrow(store$fires)) return()
      visible <- isTRUE(input$show_fires)
      mapgl::maplibre_proxy("forecast_map", session) |>
        mapgl::set_layout_property("fires", "visibility", if (visible) "visible" else "none")
    })

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
    }

    observeEvent(input$territory, update_selected_territory(), ignoreInit = TRUE)

    observeEvent(input$forecast_map_bbox, {
      map_ready(TRUE)
      req(input$indicator, input$horizon)
      horizon <- selected_horizon()
      update_raster(input$indicator, horizon)
      schedule_raster_preload(input$indicator, horizon)
      update_selected_territory()
      mapgl::maplibre_proxy("forecast_map", session) |>
        mapgl::set_layout_property("satellite", "visibility", if (isTRUE(input$show_satellite)) "visible" else "none")
      if (!is.null(store$fires) && nrow(store$fires)) {
        visible <- isTRUE(input$show_fires)
        mapgl::maplibre_proxy("forecast_map", session) |>
          mapgl::set_layout_property("fires", "visibility", if (visible) "visible" else "none")
      }
      session$sendCustomMessage(
        "alertar:wind",
        list(
          mapId = session$ns("forecast_map"),
          active = isTRUE(input$show_wind) && store$wind_available,
          url = if (store$wind_available) store$wind_url(horizon) else NULL
        )
      )
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
      store$query_series(input$indicator, input$territory)
    }) |> bindCache(input$indicator, input$territory)

    forecast_origin <- reactive(store$analysis_time(input$indicator)) |>
      bindCache(input$indicator)

    selected_value <- reactive({
      data <- series()
      if (!nrow(data)) return(NA_real_)
      target <- forecast_origin() + selected_horizon() * 3600
      data$value[[which.min(abs(as.numeric(difftime(data$date, target, units = "secs"))))]]
    })

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
      horizon <- selected_horizon()
      time <- in_timezone(forecast_origin() + horizon * 3600, timezone)
      date_format <- if (language == "en") "%Y-%m-%d • %H:%M" else "%d/%m/%Y • %H:%M"
      tagList(
        strong(sprintf("+%03dh", horizon)),
        span(paste(format(time, date_format, tz = timezone), timezone_code(timezone)))
      )
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
      cfg <- selected_config()
      territory <- territories[territories$territory_id == input$territory, , drop = FALSE]
      value <- selected_value()
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
          class = "value-row",
          span(class = "reading-value", if (is.finite(value)) format(round(value, cfg$digits), nsmall = cfg$digits, decimal.mark = decimal_mark) else "—"),
          span(class = "reading-unit", pretty_unit(cfg$unit))
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
      target <- forecast_origin() + selected_horizon() * 3600
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
      target <- forecast_origin() + selected_horizon() * 3600
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
        target <- forecast_origin() + selected_horizon() * 3600
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
      labels <- if (is.null(cfg$breaks)) {
        format(seq(cfg$range[[1]], cfg$range[[2]], length.out = 5), trim = TRUE)
      } else {
        finite <- cfg$breaks[is.finite(cfg$breaks)]
        format(c(0, finite), trim = TRUE)
      }
      tagList(
        div(class = "legend-title", indicator_text(language, id, "short", cfg$short), span(pretty_unit(cfg$unit))),
        div(class = "legend-gradient", style = paste0("--legend:", paste(cfg$colors, collapse = ","))),
        div(class = "legend-labels", lapply(labels, span))
      )
    })

    output$download_series <- downloadHandler(
      filename = function() sprintf("alertar_%s_%s.csv", input$indicator, input$territory),
      content = function(file) utils::write.csv2(series(), file, row.names = FALSE)
    )

    playing <- reactiveVal(FALSE)
    frame_pending <- reactiveVal(FALSE)

    update_play_button <- function() {
      language <- isolate(current_language())
      is_playing <- isolate(playing())
      updateActionButton(
        session,
        "play",
        label = if (is_playing) tr(language, "pause") else tr(language, "animate"),
        icon = icon(if (is_playing) "pause" else "play")
      )
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
      updateCheckboxInput(session, "show_satellite", label = tr(language, "satellite_image"))
      update_play_button()

      session$sendCustomMessage(
        "alertar:interface",
        list(
          language = map_language_code(language),
          title = tr(language, "app_title"),
          territoryPlaceholder = tr(language, "territory_placeholder"),
          timezoneLabel = tr(language, "timezone_label"),
          chartLabel = tr(language, "chart_expand"),
          detailsToggle = list(
            minimize = tr(language, "minimize_panel"),
            restore = tr(language, "restore_panel")
          ),
          text = stats::setNames(
            list(
              tr(language, "history"), tr(language, "about"),
              tr(language, "layer_heading"), tr(language, "local_heading"),
              tr(language, "download_series"), tr(language, "now"), tr(language, "forecast_horizon")
            ),
            c("label-history", "label-about", "label-layer-heading", "label-local-heading", "label-download", "label-now", "label-forecast-horizon")
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

    observeEvent(input$play, {
      playing(!playing())
      update_play_button()
    })

    observeEvent(input$forecast_map_raster_ready, {
      result <- input$forecast_map_raster_ready
      frame_pending(FALSE)
      if (identical(result$ok, FALSE)) {
        playing(FALSE)
        update_play_button()
        showNotification(tr(current_language(), "raster_error"), type = "error")
      }
    })

    timer <- reactiveTimer(2000, session)
    observe({
      timer()
      if (!isolate(playing())) return()
      if (isolate(frame_pending())) return()
      id <- isolate(input$indicator)
      current_horizon <- store$normalize_horizon(id, isolate(input$horizon))
      next_horizon <- store$future_horizons(id, current_horizon, 1L)
      if (!length(next_horizon)) {
        playing(FALSE)
        update_play_button()
        return()
      }
      frame_pending(TRUE)
      updateSliderInput(session, "horizon", value = next_horizon[[1]])
    })

    observeEvent(input$open_history, {
      showModal(historical_data_modal(current_language()))
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
