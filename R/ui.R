historical_record_card <- function(label, archive_url, update_url, language = "pt") {
  div(
    class = "history-card",
    h4(label),
    tags$a(
      icon("archive"), span(tr(language, "history_archive")),
      href = archive_url, target = "_blank", rel = "noopener noreferrer"
    ),
    tags$a(
      icon("calendar"), span(tr(language, "history_update")),
      href = update_url, target = "_blank", rel = "noopener noreferrer"
    )
  )
}

historical_data_modal <- function(language = "pt") {
  modalDialog(
    title = tagList(icon("database"), tr(language, "history_title")),
    div(
      class = "modal-intro",
      p(tr(language, "history_intro"))
    ),
    div(
      class = "history-grid",
      historical_record_card(indicator_text(language, "pm25", "short", "PM2.5"), "https://zenodo.org/records/16374139", "https://zenodo.org/records/18552120", language),
      historical_record_card(indicator_text(language, "pm10", "short", "PM10"), "https://zenodo.org/records/16419737", "https://zenodo.org/records/18554403", language),
      historical_record_card(indicator_text(language, "o3", "label", "O₃"), "https://zenodo.org/records/17025187", "https://zenodo.org/records/18558668", language),
      historical_record_card(indicator_text(language, "co", "label", "CO"), "https://zenodo.org/records/16984341", "https://zenodo.org/records/18555894", language),
      historical_record_card(indicator_text(language, "no2", "label", "NO₂"), "https://zenodo.org/records/17019753", "https://zenodo.org/records/18556588", language),
      historical_record_card(indicator_text(language, "so2", "label", "SO₂"), "https://zenodo.org/records/17047073", "https://zenodo.org/records/18559084", language)
    ),
    footer = modalButton(tr(language, "close")),
    easyClose = TRUE,
    size = "l"
  )
}

about_project_modal <- function(language = "pt") {
  modalDialog(
    title = tagList(icon("info-circle"), tr(language, "about_title")),
    div(
      class = "about-grid",
      tags$section(
        h3(tr(language, "about_project_heading")),
        p(tr(language, "about_project_p1")),
        p(tr(language, "about_project_p2"))
      ),
      tags$section(
        h3(tr(language, "about_methods_heading")),
        p(tr(language, "about_methods_p1")),
        p(tr(language, "about_methods_p2"))
      ),
      tags$section(
        h3(tr(language, "about_coverage_heading")),
        p(tr(language, "about_coverage_p1"))
      ),
      tags$section(
        class = "about-cams-credit",
        h3(tr(language, "about_cams_heading")),
        p(tr(language, "about_cams_attribution", format(Sys.Date(), "%Y"))),
        tags$a(
          tr(language, "about_cams_link"),
          href = "https://confluence.ecmwf.int/spaces/CKB/pages/116952716/SFTP-FTP-HTTPS+data+access+to+CAMS+global+data#SFTPFTPHTTPSdataaccesstoCAMSglobaldata-Attribution",
          target = "_blank", rel = "noopener noreferrer"
        )
      ),
      tags$section(
        h3(tr(language, "about_code_heading")),
        p(tr(language, "about_code_p1")),
        tags$a(
          icon("github"), paste0(" ", tr(language, "data_processing")),
          href = "https://github.com/rfsaldanha/camsdata", target = "_blank", rel = "noopener noreferrer"
        ),
        tags$a(
          icon("github"), paste0(" ", tr(language, "shiny_application")),
          href = "https://github.com/rfsaldanha/polbr", target = "_blank", rel = "noopener noreferrer"
        )
      ),
      tags$section(
        class = "about-logo-card",
        tags$img(
          src = "pin_obs_horizontal_dark.png",
          alt = "Observatório de Clima e Saúde — ICICT — Fiocruz"
        )
      )
    ),
    footer = modalButton(tr(language, "close")),
    easyClose = TRUE,
    size = "l"
  )
}

forecast_chart_modal <- function(language, title) {
  modalDialog(
    title = tagList(icon("chart-line"), title),
    div(
      class = "forecast-modal-chart",
      plotOutput("forecast_detail_plot", height = "58vh")
    ),
    footer = tagList(
      downloadButton(
        "download_forecast_plot", tr(language, "download_chart"),
        icon = icon("download"), class = "chart-download-button"
      ),
      modalButton(tr(language, "close"))
    ),
    easyClose = TRUE,
    size = "xl"
  )
}

app_ui <- function(store) {
  catalog <- store$catalog[store$available]
  timezones <- timezone_catalog()
  indicator_choices <- stats::setNames(
    names(catalog),
    vapply(names(catalog), function(id) indicator_text("pt", id, "short", catalog[[id]]$short), character(1))
  )

  bslib::page_fillable(
    title = tr("pt", "app_title"),
    theme = bslib::bs_theme(version = 5, bg = "#071018", fg = "#edf7ff", primary = "#35d4b4"),
    padding = 0,
    gap = 0,
    fillable_mobile = TRUE,
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
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
          h1("AlertAr", span(" Saúde")),
          uiOutput("update_badge", container = div, class = "update-row")
        )
      ),

      tags$nav(
        class = "utility-nav glass-panel",
        div(
          class = "language-control",
          icon("globe"),
          tags$select(
            id = "language",
            class = "shiny-input-select form-control",
            title = "Idioma / Langue / Language",
            `aria-label` = "Idioma / Langue / Language",
            tags$option(value = "pt", selected = "selected", "PT"),
            tags$option(value = "fr", "FR"),
            tags$option(value = "es", "ES"),
            tags$option(value = "en", "EN")
          )
        ),
        div(
          class = "timezone-control",
          icon("clock"),
          tags$select(
            id = "timezone",
            class = "shiny-input-select form-control",
            title = tr("pt", "timezone_label"),
            `aria-label` = tr("pt", "timezone_label"),
            lapply(seq_len(nrow(timezones)), function(i) {
              tags$option(
                value = timezones$timezone[[i]],
                selected = if (timezones$timezone[[i]] == "America/Sao_Paulo") "selected" else NULL,
                title = timezones$label[[i]],
                timezones$code[[i]]
              )
            })
          )
        ),
        tags$button(
          id = "toggle_totem", type = "button", class = "utility-button totem-button",
          title = tr("pt", "totem_mode"), `aria-label` = tr("pt", "totem_mode"),
          `aria-pressed` = "false",
          icon("expand"), span(id = "label-totem", tr("pt", "totem_mode"))
        ),
        actionButton(
          "open_history",
          label = tagList(icon("database"), span(id = "label-history", tr("pt", "history"))),
          class = "utility-button"
        ),
        actionButton(
          "open_about",
          label = tagList(icon("info-circle"), span(id = "label-about", tr("pt", "about"))),
          class = "utility-button"
        )
      ),

      tags$aside(
        class = "variable-panel glass-panel",
        div(class = "panel-heading", span(id = "label-layer-heading", tr("pt", "layer_heading"))),
        selectInput("indicator", NULL, choices = indicator_choices, selected = store$default_indicator),
        uiOutput("indicator_summary"),
        div(class = "divider"),
        div(
          class = "layer-switches",
          checkboxInput("show_wind", tr("pt", "wind_particles"), value = store$wind_available),
          checkboxInput("show_fires", tr("pt", "heat_spots"), value = TRUE),
          checkboxInput("show_satellite", tr("pt", "satellite_image"), value = FALSE)
        )
      ),

      tags$aside(
        class = "place-panel glass-panel",
        div(
          class = "panel-heading",
          span(id = "label-local-heading", tr("pt", "local_heading")),
          tags$button(
            id = "toggle_details", type = "button", class = "icon-button",
            title = tr("pt", "minimize_panel"),
            `aria-label` = tr("pt", "minimize_panel"),
            `aria-expanded` = "true",
            "−"
          )
        ),
        div(
          id = "place-panel-body",
          selectizeInput("territory", NULL, choices = NULL, options = list(placeholder = tr("pt", "territory_placeholder"))),
          div(
            id = "details-body",
            uiOutput("local_reading"),
            htmltools::tagAppendAttributes(
              plotOutput("forecast_spark", height = "190px", click = "forecast_spark_click"),
              class = "forecast-spark-clickable", role = "button", tabindex = "0",
              title = tr("pt", "chart_expand"), `aria-label` = tr("pt", "chart_expand")
            ),
            uiOutput("forecast_references", container = div, class = "reference-guide"),
            downloadButton("download_series", span(id = "label-download", tr("pt", "download_series")), class = "download-link")
          )
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
          actionButton("play", label = tagList(icon("play"), span(tr("pt", "animate"))), class = "play-button"),
          span(id = "label-forecast-horizon", class = "timeline-title", tr("pt", "forecast_horizon")),
          div(uiOutput("forecast_time", container = div), class = "forecast-clock")
        ),
        div(
          class = "timeline-control",
          sliderInput("horizon", NULL, min = 0, max = 120, value = 12, step = 3, ticks = FALSE),
          div(class = "day-labels", span(id = "label-now", tr("pt", "now")), span("+24h"), span("+48h"), span("+72h"), span("+96h"), span("+120h"))
        )
      )
    )
  )
}
