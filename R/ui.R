historical_record_card <- function(label, archive_url, update_url) {
  div(
    class = "history-card",
    h4(label),
    tags$a(
      icon("archive"), span("2003–2024"),
      href = archive_url, target = "_blank", rel = "noopener noreferrer"
    ),
    tags$a(
      icon("calendar"), span("Jan–ago de 2025"),
      href = update_url, target = "_blank", rel = "noopener noreferrer"
    )
  )
}

historical_data_modal <- function() {
  modalDialog(
    title = tagList(icon("database"), "Dados históricos"),
    div(
      class = "modal-intro",
      p(
        "Séries diárias municipais produzidas a partir da reanálise global CAMS EAC4. ",
        "Os conjuntos incluem estatísticas zonais em CSV e Parquet e estão publicados no Zenodo."
      )
    ),
    div(
      class = "history-grid",
      historical_record_card("PM2.5", "https://zenodo.org/records/16374139", "https://zenodo.org/records/18552120"),
      historical_record_card("PM10", "https://zenodo.org/records/16419737", "https://zenodo.org/records/18554403"),
      historical_record_card("Ozônio (O₃)", "https://zenodo.org/records/17025187", "https://zenodo.org/records/18558668"),
      historical_record_card("Monóxido de carbono (CO)", "https://zenodo.org/records/16984341", "https://zenodo.org/records/18555894"),
      historical_record_card("Dióxido de nitrogênio (NO₂)", "https://zenodo.org/records/17019753", "https://zenodo.org/records/18556588"),
      historical_record_card("Dióxido de enxofre (SO₂)", "https://zenodo.org/records/17047073", "https://zenodo.org/records/18559084")
    ),
    footer = modalButton("Fechar"),
    easyClose = TRUE,
    size = "l"
  )
}

about_project_modal <- function() {
  modalDialog(
    title = tagList(icon("info-circle"), "Sobre o projeto"),
    div(
      class = "about-grid",
      tags$section(
        h3("AlertAr Saúde"),
        p(
          "Painel interativo de previsões atmosféricas para apoiar a vigilância em saúde, ",
          "o planejamento de ações preventivas e o acesso público à informação ambiental. ",
          "O horizonte de previsão é de até 120 horas."
        ),
        p(
          "Desenvolvido pelo Observatório de Clima e Saúde, no Laboratório de Informação ",
          "em Saúde (LIS/ICICT) da Fundação Oswaldo Cruz (Fiocruz)."
        )
      ),
      tags$section(
        h3("Dados e métodos"),
        p(
          "As previsões são obtidas do Copernicus Atmosphere Monitoring Service (CAMS). ",
          "PM2.5, PM10, temperatura, IUV, vento, aerossol e chuva acumulada são horários; ",
          "O₃, CO, NO₂, SO₂ e IQAr são apresentados a cada três horas."
        ),
        p(
          "O valor e o gráfico de cada território representam a média espacial das células ",
          "que intersectam sua geometria. O IQAr segue as faixas de referência do CONAMA."
        )
      ),
      tags$section(
        h3("Cobertura"),
        p(
          "A cobertura operacional atual é o Brasil. A arquitetura aceita pontos e polígonos ",
          "de diferentes tipos territoriais e está preparada para expansão à América Latina e Caribe."
        )
      ),
      tags$section(
        h3("Código aberto"),
        p("Os códigos de processamento e da aplicação estão disponíveis publicamente."),
        tags$a(
          icon("github"), " Processamento de dados",
          href = "https://github.com/rfsaldanha/camsdata", target = "_blank", rel = "noopener noreferrer"
        ),
        tags$a(
          icon("github"), " Aplicação R Shiny",
          href = "https://github.com/rfsaldanha/polbr", target = "_blank", rel = "noopener noreferrer"
        )
      )
    ),
    footer = modalButton("Fechar"),
    easyClose = TRUE,
    size = "l"
  )
}

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

      tags$nav(
        class = "utility-nav glass-panel",
        actionButton(
          "open_history",
          label = tagList(icon("database"), span("Dados históricos")),
          class = "utility-button"
        ),
        actionButton(
          "open_about",
          label = tagList(icon("info-circle"), span("Sobre")),
          class = "utility-button"
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
