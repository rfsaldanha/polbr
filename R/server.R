app_server <- function(store) {
  force(store)
  function(input, output, session) {
    catalog <- store$catalog
    territories <- store$territories
    default_territory <- if ("330455" %in% territories$territory_id) "330455" else territories$territory_id[[1]]
    territory_choices <- lapply(
      split(seq_len(nrow(territories)), territories$territory_type),
      function(i) stats::setNames(territories$territory_id[i], territories$display_name[i])
    )
    map_ready <- reactiveVal(FALSE)

    session$onFlushed(function() {
      updateSelectizeInput(session, "territory", choices = territory_choices, selected = default_territory, server = TRUE)
    }, once = TRUE)

    output$forecast_map <- mapgl::renderMaplibre({
      initial_id <- isolate(input$indicator %||% store$available[[1]])
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
            id = "fires", source = fires, circle_radius = 3.5,
            circle_color = "#ff6b35", circle_opacity = .88,
            circle_stroke_color = "#fff1d6", circle_stroke_width = .7,
            visibility = "none", tooltip = "data_hora_gmt"
          )
      }
      map
    })

    selected_config <- reactive({
      req(input$indicator)
      catalog[[input$indicator]]
    })

    layer_request <- reactive({
      req(input$indicator, input$horizon)
      list(id = input$indicator, horizon = input$horizon)
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
      first_horizon <- as.integer(horizon) + 3L
      last_horizon <- min(120L, as.integer(horizon) + frames * 3L)
      if (first_horizon > last_horizon) return(invisible(NULL))

      future_horizons <- seq.int(first_horizon, last_horizon, by = 3L)
      for (i in seq_along(future_horizons)) {
        local({
          preload_horizon <- future_horizons[[i]]
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
      update_selected_territory()
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
          url = if (store$wind_available) store$wind_url(input$horizon %||% 0) else NULL
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
      req(input$indicator)
      if (is.null(store$fires) || !nrow(store$fires)) return()
      visible <- isTRUE(input$show_fires) && input$indicator %in% c("pm25", "pm10", "aerosol")
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
      update_raster(input$indicator, input$horizon)
      schedule_raster_preload(input$indicator, input$horizon)
      update_selected_territory()
      mapgl::maplibre_proxy("forecast_map", session) |>
        mapgl::set_layout_property("satellite", "visibility", if (isTRUE(input$show_satellite)) "visible" else "none")
      if (!is.null(store$fires) && nrow(store$fires)) {
        visible <- isTRUE(input$show_fires) && input$indicator %in% c("pm25", "pm10", "aerosol")
        mapgl::maplibre_proxy("forecast_map", session) |>
          mapgl::set_layout_property("fires", "visibility", if (visible) "visible" else "none")
      }
      session$sendCustomMessage(
        "alertar:wind",
        list(
          mapId = session$ns("forecast_map"),
          active = isTRUE(input$show_wind) && store$wind_available,
          url = if (store$wind_available) store$wind_url(input$horizon) else NULL
        )
      )
      session$sendCustomMessage(
        "alertar:language",
        list(
          mapId = session$ns("forecast_map"),
          language = store$coverage$map_language
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
      target <- forecast_origin() + input$horizon * 3600
      data$value[[which.min(abs(as.numeric(difftime(data$date, target, units = "secs"))))]]
    })

    output$update_badge <- renderUI({
      time <- as.POSIXct(forecast_origin(), tz = "America/Sao_Paulo")
      tagList(span(class = "status-dot"), span("Atualizado ", format(time, "%d/%m • %H:%M"), " BRT"))
    })

    output$forecast_time <- renderUI({
      time <- as.POSIXct(forecast_origin() + input$horizon * 3600, tz = "America/Sao_Paulo")
      tagList(
        strong(sprintf("+%03dh", input$horizon)),
        span(format(time, "%d/%m/%Y • %H:%M BRT"))
      )
    })

    output$indicator_summary <- renderUI({
      cfg <- selected_config()
      tagList(
        div(class = "indicator-name", cfg$label),
        p(cfg$description),
        div(class = "source-label", "MODELO CAMS • HORIZONTE 120H")
      )
    })

    output$local_reading <- renderUI({
      req(input$territory)
      cfg <- selected_config()
      territory <- territories[territories$territory_id == input$territory, , drop = FALSE]
      value <- selected_value()
      tagList(
        div(
          class = "territory-label",
          territory$display_name[[1]],
          span(class = "territory-type", territory$territory_type[[1]])
        ),
        div(
          class = "value-row",
          span(class = "reading-value", if (is.finite(value)) format(round(value, cfg$digits), nsmall = cfg$digits, decimal.mark = ",") else "—"),
          span(class = "reading-unit", pretty_unit(cfg$unit))
        ),
        div(class = "reading-caption", "Media espacial no territorio e horario selecionados")
      )
    })

    output$forecast_spark <- renderPlot({
      data <- series()
      validate(need(nrow(data), "Serie territorial indisponivel"))
      target <- forecast_origin() + input$horizon * 3600
      par(mar = c(1.2, 1, .5, .5), bg = NA, fg = "#7890a0")
      plot(data$date, data$value, type = "l", col = "#35d4b4", lwd = 2.2, axes = FALSE, xlab = "", ylab = "")
      abline(v = target, col = "#f8fafc66", lty = 3)
      points(target, data$value[[which.min(abs(as.numeric(data$date - target)))]], pch = 21, bg = "#35d4b4", col = "white", cex = 1.1)
      axis.POSIXct(1, at = seq(min(data$date), max(data$date), length.out = 4), format = "%d/%m", col = NA, col.axis = "#7890a0", cex.axis = .72)
    }, bg = "transparent", res = 110)

    output$map_legend <- renderUI({
      cfg <- selected_config()
      labels <- if (is.null(cfg$breaks)) {
        format(seq(cfg$range[[1]], cfg$range[[2]], length.out = 5), trim = TRUE)
      } else {
        finite <- cfg$breaks[is.finite(cfg$breaks)]
        format(c(0, finite), trim = TRUE)
      }
      tagList(
        div(class = "legend-title", cfg$short, span(pretty_unit(cfg$unit))),
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
      updateActionButton(
        session,
        "play",
        label = if (playing()) "Pausar" else "Animar",
        icon = icon(if (playing()) "pause" else "play")
      )
    }

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
        showNotification("Nao foi possivel carregar o proximo quadro do mapa.", type = "error")
      }
    })

    timer <- reactiveTimer(2000, session)
    observe({
      timer()
      if (!isolate(playing())) return()
      if (isolate(frame_pending())) return()
      current_horizon <- isolate(input$horizon)
      if (current_horizon >= 120) {
        playing(FALSE)
        update_play_button()
        return()
      }
      next_value <- min(120, current_horizon + 3)
      frame_pending(TRUE)
      updateSliderInput(session, "horizon", value = next_value)
    })

    observeEvent(input$toggle_details, {
      session$sendCustomMessage("alertar:toggle-details", list())
    })
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x
