app_ui <- function(store) {
  catalog <- store$catalog[store$available]
  indicator_choices <- stats::setNames(names(catalog), vapply(catalog, `[[`, character(1), "short"))

  bslib::page_fillable(
    title = "AlertAr Saude | Previsao atmosferica",
    theme = bslib::bs_theme(version = 5, bg = "#071018", fg = "#edf7ff", primary = "#35d4b4"),
    fillable_mobile = TRUE,
    tags$head(
      tags$meta(name = "theme-color", content = "#071018"),
      tags$link(rel = "preconnect", href = "https://basemaps.cartocdn.com"),
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$script(src = "app.js", defer = "defer"),
      if (file.exists("google-analytics.html")) includeHTML("google-analytics.html")
    ),
    div(
      class = "app-shell",
      mapgl::maplibreOutput("forecast_map", width = "100%", height = "100%"),
      div(class = "map-vignette"),

      tags$header(
        class = "brand-panel glass-panel",
        tags$img(
          class = "brand-logo",
          src = "pin_obs_horizontal_dark.png",
          alt = "Observatorio de Clima e Saude - ICICT - Fiocruz"
        ),
        div(
          class = "product-lockup",
          h1("AlertAr", span(" Saude")),
          uiOutput("update_badge", container = div, class = "update-row")
        )
      ),

      tags$aside(
        class = "variable-panel glass-panel",
        div(class = "panel-heading", span("CAMADA ATMOSFERICA"), span(class = "live-dot")),
        selectInput("indicator", NULL, choices = indicator_choices, selected = names(indicator_choices)[[1]]),
        uiOutput("indicator_summary"),
        div(class = "divider"),
        div(
          class = "layer-switches",
          checkboxInput("show_wind", "Particulas de vento", value = store$wind_available),
          checkboxInput("show_fires", "Focos de calor", value = TRUE),
          checkboxInput("show_satellite", "Imagem de satelite", value = FALSE)
        )
      ),

      tags$aside(
        class = "place-panel glass-panel",
        div(class = "panel-heading", span("LEITURA LOCAL"), actionButton("toggle_details", "−", class = "icon-button")),
        selectizeInput("territory", NULL, choices = NULL, options = list(placeholder = "Buscar municipio, terra indigena, quilombo...")),
        div(
          id = "details-body",
          uiOutput("local_reading"),
          plotOutput("forecast_spark", height = "112px"),
          downloadButton("download_series", "Baixar serie", class = "download-link")
        )
      ),

      div(
        class = "legend-panel glass-panel",
        uiOutput("map_legend")
      ),

      tags$footer(
        class = "timeline-panel glass-panel",
        div(
          class = "timeline-meta",
          actionButton("play", label = tagList(icon("play"), span("Animar")), class = "play-button"),
          div(uiOutput("forecast_time", container = div), class = "forecast-clock")
        ),
        div(
          class = "timeline-control",
          sliderInput("horizon", NULL, min = 0, max = 120, value = 12, step = 3, ticks = FALSE),
          div(class = "day-labels", span("Agora"), span("+24h"), span("+48h"), span("+72h"), span("+96h"), span("+120h"))
        )
      ),

      div(class = "credits", paste0("CAMS / Copernicus  •  ", store$coverage$label, "  •  LIS / ICICT / Fiocruz"))
    )
  )
}
