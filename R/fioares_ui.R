fioares_translations <- list(
  pt = c(retry = "Tentar novamente", layer = "Estações FioAres", title = "Histórico de PM2.5", start = "Data de início", end = "Data de fim",
    good = "Boa", moderate = "Moderada", bad = "Ruim", very_bad = "Muito ruim", extreme = "Péssima",
    unknown = "Sem índice atual", stale = "Sem atualização recente", no_index = "IQAr indisponível neste horário",
    no_data = "Sem medições", inactive = "Estação fora de operação", unavailable = "Dados FioAres indisponíveis. Tentaremos novamente na próxima atualização.",
    unconfigured = "Conexão com FioAres ainda não configurada.", loading = "Consultando FioAres…",
    count = "%s estações · %s com IQAr recente", note = "Cores: IQAr observado da estação. Cinza: sem índice válido nas últimas %s h. Clique para consultar PM2.5.",
    latest = "Última medição", index = "IQAr da estação", parameter = "Poluente determinante",
    hourly = "PM2.5 · média horária", rolling = "PM2.5 · média móvel de 24 h", credit = "Dados: FioAres/Fiocruz",
    dates = "Selecione datas válidas, com início anterior ou igual ao fim.", range = "Selecione até %s dias por consulta.",
    empty = "Não há medições válidas de PM2.5 neste período.", period = "Datas no fuso %s · até %s dias por consulta.",
    gaps = "Somente medições válidas na fonte. Lacunas e valores invalidados não são interpolados. A média de 24 h segue a validação do FioAres.",
    observed = "Condições observadas", history_note = "O IQAr resume os poluentes disponíveis; o gráfico abaixo mostra exclusivamente PM2.5.",
    available = "Histórico disponível: %s a %s", close = "Fechar"),
  en = c(retry = "Try again", layer = "FioAres stations", title = "PM2.5 history", start = "Start date", end = "End date",
    good = "Good", moderate = "Moderate", bad = "Poor", very_bad = "Very poor", extreme = "Extremely poor",
    unknown = "No current index", stale = "No recent update", no_index = "IQAr unavailable at this hour",
    no_data = "No readings", inactive = "Station not operating", unavailable = "FioAres data unavailable. We will retry at the next update.",
    unconfigured = "FioAres connection is not configured yet.", loading = "Loading FioAres…",
    count = "%s stations · %s with recent IQAr", note = "Colors: observed station IQAr. Grey: no valid index in the last %s h. Click to view PM2.5.",
    latest = "Latest reading", index = "Station IQAr", parameter = "Dominant pollutant",
    hourly = "PM2.5 · hourly mean", rolling = "PM2.5 · 24 h rolling mean", credit = "Data: FioAres/Fiocruz",
    dates = "Choose valid dates, with the start on or before the end.", range = "Select up to %s days per query.",
    empty = "No valid PM2.5 readings in this period.", period = "Dates in %s · up to %s days per query.",
    gaps = "Only readings validated by the source. Gaps and invalid values are not interpolated. The 24 h mean follows FioAres validation.",
    observed = "Observed conditions", history_note = "IQAr summarizes available pollutants; this chart shows PM2.5 only.",
    available = "Available history: %s to %s", close = "Close"),
  es = c(retry = "Reintentar", layer = "Estaciones FioAres", title = "Historial de PM2.5", start = "Fecha de inicio", end = "Fecha de fin",
    good = "Buena", moderate = "Moderada", bad = "Mala", very_bad = "Muy mala", extreme = "Pésima",
    unknown = "Sin índice actual", stale = "Sin actualización reciente", no_index = "IQAr no disponible en esta hora",
    no_data = "Sin mediciones", inactive = "Estación fuera de servicio", unavailable = "Datos FioAres no disponibles. Se volverá a intentar en la próxima actualización.",
    unconfigured = "Conexión con FioAres aún no configurada.", loading = "Consultando FioAres…",
    count = "%s estaciones · %s con IQAr reciente", note = "Colores: IQAr observado de la estación. Gris: sin índice válido en las últimas %s h. Pulse para consultar PM2.5.",
    latest = "Última medición", index = "IQAr de la estación", parameter = "Contaminante determinante",
    hourly = "PM2.5 · media horaria", rolling = "PM2.5 · media móvil de 24 h", credit = "Datos: FioAres/Fiocruz",
    dates = "Seleccione fechas válidas, con inicio anterior o igual al fin.", range = "Seleccione hasta %s días por consulta.",
    empty = "No hay mediciones válidas de PM2.5 en este período.", period = "Fechas en %s · hasta %s días por consulta.",
    gaps = "Solo mediciones válidas en la fuente. No se interpolan vacíos ni valores invalidados. La media de 24 h sigue la validación de FioAres.",
    observed = "Condiciones observadas", history_note = "El IQAr resume los contaminantes disponibles; este gráfico muestra solo PM2.5.",
    available = "Historial disponible: %s a %s", close = "Cerrar"),
  fr = c(retry = "Réessayer", layer = "Stations FioAres", title = "Historique de PM2.5", start = "Date de début", end = "Date de fin",
    good = "Bonne", moderate = "Modérée", bad = "Mauvaise", very_bad = "Très mauvaise", extreme = "Extrêmement mauvaise",
    unknown = "Aucun indice actuel", stale = "Aucune mise à jour récente", no_index = "IQAr indisponible à cette heure",
    no_data = "Aucune mesure", inactive = "Station hors service", unavailable = "Données FioAres indisponibles. Nouvelle tentative à la prochaine mise à jour.",
    unconfigured = "Connexion FioAres non configurée.", loading = "Chargement de FioAres…",
    count = "%s stations · %s avec un IQAr récent", note = "Couleurs : IQAr observé de la station. Gris : aucun indice valide depuis %s h. Cliquez pour consulter PM2.5.",
    latest = "Dernière mesure", index = "IQAr de la station", parameter = "Polluant déterminant",
    hourly = "PM2.5 · moyenne horaire", rolling = "PM2.5 · moyenne mobile de 24 h", credit = "Données : FioAres/Fiocruz",
    dates = "Choisissez des dates valides, avec un début antérieur ou égal à la fin.", range = "Sélectionnez jusqu’à %s jours par requête.",
    empty = "Aucune mesure valide de PM2.5 sur cette période.", period = "Dates dans le fuseau %s · jusqu’à %s jours par requête.",
    gaps = "Uniquement les mesures validées à la source. Aucune interpolation des lacunes ou valeurs invalidées. La moyenne de 24 h suit la validation de FioAres.",
    observed = "Conditions observées", history_note = "L’IQAr résume les polluants disponibles ; ce graphique montre uniquement PM2.5.",
    available = "Historique disponible : %s à %s", close = "Fermer")
)

fioares_text <- function(language, key, ...) {
  value <- fioares_translations[[normalize_language(language)]][[key]]
  if (length(list(...))) sprintf(value, ...) else value
}

fioares_timestamp <- function(ms, timezone) {
  if (!is.finite(ms)) return("—")
  format(as.POSIXct(ms / 1000, origin = "1970-01-01", tz = "UTC"), "%d/%m/%Y %H:%M", tz = timezone)
}

fioares_plot <- function(data, station, language, timezone) {
  # Explicit local timestamps avoid the browser silently choosing its own timezone.
  time <- format(data$date, "%Y-%m-%d %H:%M:%S", tz = timezone)
  label <- format(data$date, "%d/%m/%Y %H:%M", tz = timezone)
  plotly::plot_ly() |>
    plotly::add_trace(x = time, y = data$value, type = "scatter", mode = "lines+markers",
      name = fioares_text(language, "hourly"), connectgaps = FALSE,
      line = list(color = "#35d4b4", width = 2), marker = list(size = 3),
      text = label, hovertemplate = paste0("%{text} · ", timezone, "<br>PM2.5: %{y:.1f} µg/m³<extra></extra>")) |>
    plotly::add_trace(x = time, y = data$rolling, type = "scatter", mode = "lines",
      name = fioares_text(language, "rolling"), connectgaps = FALSE,
      line = list(color = "#ffd166", width = 2, dash = "dash"),
      text = label, hovertemplate = paste0("%{text} · ", timezone, "<br>24 h: %{y:.1f} µg/m³<extra></extra>")) |>
    plotly::layout(
      title = list(text = paste0("PM2.5 · ", htmltools::htmlEscape(station$city[[1]])), x = .02, font = list(size = 17)),
      paper_bgcolor = "#091720", plot_bgcolor = "#091720", font = list(color = "#cbd9df"),
      margin = list(l = 65, r = 20, t = 85, b = 105), hovermode = "x unified",
      xaxis = list(type = "date", title = list(text = timezone), gridcolor = "#22343e"),
      yaxis = list(title = list(text = "PM2.5 (µg/m³)"), rangemode = "tozero", gridcolor = "#22343e"),
      legend = list(orientation = "h", x = 0, y = 1.14, font = list(size = 11)),
      annotations = list(list(text = paste0("<b>", fioares_text(language, "credit"), "</b>"),
        x = 1, y = -.28, xref = "paper", yref = "paper", xanchor = "right", yanchor = "top",
        showarrow = FALSE, font = list(size = 13, color = "#e4f0f5")))) |>
    plotly::config(displaylogo = FALSE, responsive = TRUE,
      toImageButtonOptions = list(format = "png", filename = paste0("fioares_pm25_", station$station_id[[1]])))
}

fioares_modal <- function(station, language, timezone, max_days, plot_id = "fioares_history_widget") {
  today <- as.Date(Sys.time(), tz = timezone)
  has_history <- is.finite(station$first_ms[[1]]) && is.finite(station$last_ms[[1]])
  last <- if (is.finite(station$last_ms[[1]])) as.Date(as.POSIXct(station$last_ms[[1]] / 1000,
    origin = "1970-01-01", tz = "UTC"), tz = timezone) else today
  first <- if (is.finite(station$first_ms[[1]])) as.Date(as.POSIXct(station$first_ms[[1]] / 1000,
    origin = "1970-01-01", tz = "UTC"), tz = timezone) else last - 6
  modalDialog(
    title = tagList(icon("tower-broadcast"), fioares_text(language, "title"), " · ", station$city[[1]]),
    div(class = "fioares-history", `data-station-id` = station$station_id[[1]],
      p(class = "fioares-station-name", strong(station$name[[1]]), " · ", station$city[[1]], " / ", station$uf[[1]]),
      uiOutput("fioares_station_summary"),
      p(class = "layer-group-note", fioares_text(language, "history_note")),
      div(class = "fioares-date-controls",
        dateInput("fioares_start", fioares_text(language, "start"), value = max(first, last - 6),
          min = first, max = today, language = language, format = "dd/mm/yyyy"),
        dateInput("fioares_end", fioares_text(language, "end"), value = last,
          min = first, max = today, language = language, format = "dd/mm/yyyy")),
      p(class = "layer-group-note", fioares_text(language, "period", timezone, max_days)),
      uiOutput("fioares_history_status"),
      div(id = "fioares_history_plot", plotly::plotlyOutput(plot_id, height = "420px")),
      p(class = "fioares-credit", fioares_text(language, "credit")),
      p(class = "layer-group-note", fioares_text(language, "gaps")),
      if (has_history) p(class = "layer-group-note", fioares_text(language, "available", format(first, "%d/%m/%Y"), format(last, "%d/%m/%Y")))),
    footer = modalButton(fioares_text(language, "close")), easyClose = TRUE, size = "l")
}

fioares_server <- function(store, input, output, session, map_ready, language, timezone) {
  snapshot <- reactiveVal(store$snapshot())
  selected <- reactiveVal(NULL)
  modal_revision <- reactiveVal(0L)
  history <- reactiveVal(list(status = "loading", data = data.frame()))
  history_sequence <- 0L
  active_plot_id <- NULL
  clock <- reactiveTimer(60000, session)
  stations <- reactive({
    clock()
    value <- snapshot()
    if (!nrow(value$stations)) return(value$stations)
    fioares_station_state(value$stations, value$status, stale_hours = store$config$stale_hours)
  })
  refresh <- function(force = FALSE) {
    promises::then(store$refresh_async(force = force), onFulfilled = function(value) {
      if (!session$isClosed()) snapshot(value)
      invisible(NULL)
    })
    invisible(NULL)
  }
  observe({
    invalidateLater(store$config$refresh_seconds * 1000, session)
    if (isTRUE(input$show_fioares)) refresh()
  })
  observeEvent(input$fioares_retry, refresh(force = TRUE), ignoreInit = TRUE)
  observe({
    req(map_ready())
    x <- stations()
    lng <- language(); tz <- timezone()
    markers <- lapply(seq_len(nrow(x)), function(i) {
      row <- x[i, ]
      status <- fioares_text(lng, row$state)
      label <- paste(row$name, paste0(row$city, " / ", row$uf), status, sep = " · ")
      title <- paste(label, paste(fioares_text(lng, "latest"), fioares_timestamp(row$ts_ms, tz), tz),
        if (row$state %in% names(fioares_colors())[1:5]) paste(fioares_text(lng, "index"), row$iqar, "·", row$parameter),
        "FioAres/Fiocruz", sep = "\n")
      list(id = row$station_id, lon = row$lon, lat = row$lat, color = row$color,
           label = label, title = title, state = row$state)
    })
    session$sendCustomMessage("alertar:fioares", list(mapId = session$ns("forecast_map"),
      inputId = session$ns("fioares_station_click"), active = isTRUE(input$show_fioares), stations = markers))
  })
  observeEvent(language(), updateCheckboxInput(session, "show_fioares", label = fioares_text(language(), "layer")))
  output$fioares_status <- renderUI({
    value <- snapshot(); x <- stations(); lng <- language()
    tagList(
      if (value$status != "ready") p(role = "status", fioares_text(lng, value$status)),
      if (value$status == "unavailable") actionButton("fioares_retry", fioares_text(lng, "retry"), class = "btn-sm"),
      if (nrow(x)) p(fioares_text(lng, "count", nrow(x), sum(x$state %in% names(fioares_colors())[1:5]))),
      div(class = "fioares-legend", lapply(names(fioares_colors()), function(key) {
        span(span(class = "fioares-legend-dot", style = paste0("background:", fioares_colors()[[key]])), fioares_text(lng, key))
      })),
      p(class = "layer-group-note", fioares_text(lng, "note", store$config$stale_hours)))
  })
  observeEvent(input$fioares_station_click, {
    id <- input$fioares_station_click
    x <- stations()
    if (!is.character(id) || length(id) != 1L || !id %in% x$station_id) return()
    selected(id)
    modal_revision(isolate(modal_revision()) + 1L)
    history_sequence <<- history_sequence + 1L
    history(list(status = "loading", data = data.frame()))
    # Rebinding a fixed htmlwidget id restores Shiny's previous plot value before
    # the new query completes. Give each opening its own output and retire the old one.
    if (!is.null(active_plot_id)) output[[active_plot_id]] <- NULL
    active_plot_id <<- paste0("fioares_history_widget_", isolate(modal_revision()))
    output[[active_plot_id]] <- plotly::renderPlotly({
      value <- history()
      req(value$status == "ready", identical(value$station_id, id), selected() == id,
          any(is.finite(value$data$value) | is.finite(value$data$rolling)))
      row <- stations(); row <- row[row$station_id == id, ]; req(nrow(row))
      fioares_plot(value$data, row, language(), timezone())
    })
    showModal(fioares_modal(x[x$station_id == id, ], language(), timezone(), store$config$max_days,
                           plot_id = active_plot_id))
  })
  output$fioares_station_summary <- renderUI({
    x <- stations(); req(selected())
    row <- x[x$station_id == selected(), ]; req(nrow(row))
    tagList(
      div(class = "fioares-current", span(class = "fioares-legend-dot", style = paste0("background:", row$color)),
          strong(fioares_text(language(), row$state)),
          if (row$state %in% names(fioares_colors())[1:5]) span(paste(fioares_text(language(), "index"), row$iqar, "·", row$parameter))),
      p(class = "layer-group-note", paste(fioares_text(language(), "latest"), fioares_timestamp(row$ts_ms, timezone()), timezone())))
  })
  request <- reactive(list(id = selected(), opening = modal_revision(), start = input$fioares_start, end = input$fioares_end,
                          timezone = timezone(), publication = snapshot()$publication,
                          status = snapshot()$status)) |> debounce(300)
  observeEvent(request(), {
    value <- request(); req(value$id, value$start, value$end)
    history_sequence <<- history_sequence + 1L
    sequence <- history_sequence
    issue <- tryCatch({ fioares_date_interval(value$start, value$end, value$timezone, store$config$max_days); NULL },
                      error = function(e) conditionMessage(e))
    if (!is.null(issue)) {
      history(list(status = if (identical(issue, "range")) "range" else "dates", data = data.frame()))
      return()
    }
    history(list(status = "loading", data = data.frame()))
    promises::then(store$history_async(value$id, value$start, value$end, value$timezone), onFulfilled = function(result) {
      if (!session$isClosed() && sequence == history_sequence) history(result)
      invisible(NULL)
    })
  }, ignoreNULL = FALSE)
  output$fioares_history_status <- renderUI({
    value <- history()
    status <- value$status
    if (status == "ready" && !any(is.finite(value$data$value) | is.finite(value$data$rolling))) status <- "empty"
    if (status == "ready") return(NULL)
    p(role = "status", class = "fioares-history-message",
      if (status == "range") fioares_text(language(), status, store$config$max_days) else fioares_text(language(), status))
  })
  invisible(list(snapshot = snapshot, stations = stations, selected = selected, history = history))
}
