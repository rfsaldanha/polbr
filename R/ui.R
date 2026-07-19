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

territorial_report_modal <- function(language) {
  modalDialog(
    title = tagList(icon("file-lines"), tr(language, "report_title")),
    uiOutput("territorial_report"),
    footer = tagList(
      downloadButton(
        "download_territorial_report", tr(language, "report_download_html"),
        icon = icon("download"), class = "chart-download-button"
      ),
      modalButton(tr(language, "close"))
    ),
    easyClose = TRUE,
    size = "xl",
    class = "territorial-report-modal"
  )
}

report_format_number <- function(value, digits, language) {
  if (!length(value) || !is.finite(value[[1]])) return("—")
  format(
    round(value[[1]], digits), nsmall = digits, trim = TRUE,
    scientific = FALSE, decimal.mark = if (language == "en") "." else ","
  )
}

report_format_hours <- function(value, language) {
  if (!length(value) || !is.finite(value[[1]])) return(tr(language, "report_no_reference"))
  paste0(report_format_number(value, if (abs(value - round(value)) < 1e-8) 0 else 1, language), " h")
}

report_format_datetime <- function(value, language, timezone) {
  if (!length(value) || is.na(value[[1]])) return("—")
  value <- in_timezone(as.POSIXct(value[[1]], origin = "1970-01-01", tz = "UTC"), timezone)
  format(value, if (language == "en") "%Y-%m-%d %H:%M" else "%d/%m/%Y %H:%M", tz = timezone)
}

report_format_rank <- function(rank, total, language) {
  if (!length(rank) || !is.finite(rank[[1]]) || !is.finite(total) || total < 1L) return("—")
  tr(language, "report_rank_value", as.integer(rank[[1]]), as.integer(total))
}

report_metric_card <- function(label, value, detail = NULL, accent = FALSE) {
  div(
    class = paste("report-metric", if (accent) "is-accent" else ""),
    span(class = "report-metric-label", label),
    strong(value),
    if (!is.null(detail)) tags$small(detail)
  )
}

report_unit_text <- function(unit) {
  switch(unit, "ug/m3" = "µg/m³", "C" = "°C", unit)
}

report_ranking_rows <- function(data, report, language, timezone, limit = 10L) {
  cfg <- report$cfg
  unit <- report_unit_text(cfg$unit)
  data <- utils::head(data, limit)
  lapply(seq_len(nrow(data)), function(index) {
    row <- data[index, , drop = FALSE]
    selected <- identical(as.character(row$territory_id[[1]]), report$selected_id)
    tags$tr(
      class = if (selected) "is-selected" else NULL,
      tags$td(paste0("#", row$rank[[1]])),
      tags$td(row$display_name[[1]]),
      tags$td(
        class = "report-value-cell",
        report_format_number(row$maximum_value, cfg$digits, language),
        tags$small(unit)
      ),
      tags$td(report_format_datetime(row$maximum_date, language, timezone)),
      tags$td(report_format_hours(row$hours_above_reference, language)),
      tags$td(
        span(
          class = "report-band-chip",
          style = paste0("--band-color:", row$dominant_color[[1]]),
          row$dominant_band[[1]]
        )
      )
    )
  })
}

report_ranking_payload <- function(data, report, language, timezone) {
  cfg <- report$cfg
  unit <- report_unit_text(cfg$unit)
  payload <- lapply(seq_len(nrow(data)), function(index) {
    row <- data[index, , drop = FALSE]
    hours <- row$hours_above_reference[[1]]
    list(
      rank = as.integer(row$rank[[1]]),
      territory = as.character(row$display_name[[1]]),
      maximum = as.numeric(row$maximum_value[[1]]),
      maximumLabel = paste(report_format_number(row$maximum_value, cfg$digits, language), unit),
      maximumTime = report_format_datetime(row$maximum_date, language, timezone),
      maximumTimeSort = as.numeric(row$maximum_date[[1]]),
      hours = if (is.finite(hours)) as.numeric(hours) else NULL,
      hoursLabel = report_format_hours(hours, language),
      band = as.character(row$dominant_band[[1]]),
      bandColor = as.character(row$dominant_color[[1]]),
      selected = identical(as.character(row$territory_id[[1]]), report$selected_id)
    )
  })
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, na = "null", digits = NA)
  gsub("</", "<\\/", json, fixed = TRUE)
}

report_sort_header <- function(label, key, type = "text") {
  tags$th(
    tags$button(
      type = "button", class = "report-sort-button",
      `data-sort-key` = key, `data-sort-type` = type,
      label, span(class = "report-sort-indicator", `aria-hidden` = "true")
    )
  )
}

report_ranking_table <- function(data, report, language, timezone) {
  if (!nrow(data)) return(p(class = "report-empty", tr(language, "report_data_unavailable")))
  div(
    class = "report-table-shell",
    div(
      class = "report-table-toolbar",
      tags$label(
        class = "report-search-control",
        icon("magnifying-glass"),
        tags$input(
          type = "search", class = "report-table-search",
          placeholder = tr(language, "report_search"),
          `aria-label` = tr(language, "report_search")
        )
      ),
      tags$label(
        class = "report-page-size-control",
        span(tr(language, "report_rows")),
        tags$select(
          class = "report-page-size", `aria-label` = tr(language, "report_rows"),
          tags$option(value = "10", "10"),
          tags$option(value = "25", "25"),
          tags$option(value = "50", "50")
        )
      )
    ),
    div(
      class = "report-table-scroll",
      tags$table(
        class = "report-ranking-table report-interactive-table",
        tags$thead(tags$tr(
          report_sort_header(tr(language, "report_rank"), "rank", "number"),
          report_sort_header(tr(language, "report_territory"), "territory"),
          report_sort_header(tr(language, "report_maximum"), "maximum", "number"),
          report_sort_header(tr(language, "report_maximum_time"), "maximumTimeSort", "number"),
          report_sort_header(tr(language, "report_hours_above"), "hours", "number"),
          report_sort_header(tr(language, "report_dominant_band"), "band")
        )),
        tags$tbody(report_ranking_rows(data, report, language, timezone))
      )
    ),
    div(class = "report-table-empty", hidden = "hidden", tr(language, "report_no_matches")),
    div(
      class = "report-table-pagination",
      tags$span(class = "report-table-count"),
      div(
        tags$button(type = "button", class = "report-page-previous", tr(language, "report_previous")),
        tags$button(type = "button", class = "report-page-next", tr(language, "report_next"))
      )
    ),
    tags$script(
      type = "application/json", class = "report-table-data",
      HTML(report_ranking_payload(data, report, language, timezone))
    )
  )
}

report_ranking_chart <- function(data, report, language) {
  if (!nrow(data)) return(p(class = "report-empty", tr(language, "report_data_unavailable")))
  cfg <- report$cfg
  chart_data <- utils::head(data, 10L)
  selected <- data[data$territory_id == report$selected_id, , drop = FALSE]
  if (nrow(selected) && !report$selected_id %in% chart_data$territory_id) {
    chart_data <- rbind(chart_data, selected[1, , drop = FALSE])
  }
  chart_data <- chart_data[order(chart_data$rank), , drop = FALSE]
  finite_values <- chart_data$maximum_value[is.finite(chart_data$maximum_value)]
  value_range <- range(finite_values)
  widths <- if (!length(finite_values) || diff(value_range) < .Machine$double.eps) {
    rep(100, nrow(chart_data))
  } else {
    14 + 86 * (chart_data$maximum_value - value_range[[1]]) / diff(value_range)
  }
  unit <- report_unit_text(cfg$unit)

  div(
    class = "report-ranking-chart",
    lapply(seq_len(nrow(chart_data)), function(index) {
      row <- chart_data[index, , drop = FALSE]
      selected_row <- identical(as.character(row$territory_id[[1]]), report$selected_id)
      value <- paste(report_format_number(row$maximum_value, cfg$digits, language), unit)
      div(
        class = paste("report-chart-row", if (selected_row) "is-selected" else ""),
        title = paste(row$display_name[[1]], value, sep = " · "),
        div(
          class = "report-chart-label",
          span(paste0("#", row$rank[[1]]), row$display_name[[1]]),
          strong(value)
        ),
        div(
          class = "report-chart-track",
          span(style = sprintf("--report-chart-width:%.3f%%", widths[[index]]))
        )
      )
    })
  )
}

report_scope_panel <- function(scope, data, report, language, timezone, active = FALSE) {
  label <- if (identical(scope, "national")) report$country_label else report$state_label
  div(
    class = paste("report-scope-panel", if (active) "is-active" else ""),
    `data-report-scope` = scope,
    if (!active) hidden = "hidden" else NULL,
    div(
      class = "report-visual-grid",
      div(
        class = "report-visual-card",
        h3(tr(language, "report_chart_title")),
        p(paste(label, "·", tr(language, "report_top_ten"))),
        report_ranking_chart(data, report, language)
      ),
      div(
        class = "report-visual-card report-table-card",
        h3(tr(language, "report_table_title")),
        p(paste(label, "·", tr(language, "report_units", nrow(data)))),
        report_ranking_table(data, report, language, timezone)
      )
    )
  )
}

territorial_report_view <- function(report, language, timezone) {
  if (is.null(report) || !isTRUE(report$available)) {
    return(div(class = "report-empty-state", tr(language, "report_data_unavailable")))
  }
  cfg <- report$cfg
  selected <- report$selected
  unit <- pretty_unit(cfg$unit)
  period_start <- report_format_datetime(report$period[[1]], language, timezone)
  period_end <- report_format_datetime(report$period[[2]], language, timezone)
  report_reference_note <- report$reference_note %||% ""
  reference_detail <- if (is.finite(report$reference_value)) {
    tr(language, "report_reference", paste(report$reference_label, report_format_number(report$reference_value, cfg$digits, language), unit))
  } else tr(language, "report_no_reference")

  category_section <- if (length(report$categories)) {
    max_hours <- max(selected$hours_covered[[1]], 1)
    div(
      class = "report-category-section",
      h3(tr(language, "report_category_hours")),
      div(class = "report-category-list", lapply(seq_along(report$categories), function(index) {
        category <- report$categories[[index]]
        hours <- selected[[category$column]][[1]]
        div(
          class = "report-category-row",
          div(
            class = "report-category-meta",
            span(class = "report-band-chip", style = paste0("--band-color:", category$color), category$label),
            strong(report_format_hours(hours, language))
          ),
          div(
            class = "report-category-track",
            span(style = sprintf("--band-color:%s;--band-width:%.3f%%", category$color, 100 * hours / max_hours))
          )
        )
      }))
    )
  } else {
    div(
      class = "report-category-section",
      h3(tr(language, "report_category_hours")),
      p(class = "report-empty", tr(language, "report_no_categories"))
    )
  }

  div(
    class = "territorial-report",
    tags$header(
      class = "report-hero",
      div(
        span(class = "report-kicker", indicator_text(language, report$id, "short", cfg$short)),
        h2(report$territory$display_name[[1]]),
        p(tr(language, "report_intro"))
      ),
      div(
        class = "report-period",
        span(tr(language, "report_period", period_start, period_end, timezone_code(timezone))),
        tags$small(tr(language, "report_generated", report_format_datetime(report$generated_at, language, timezone)))
      )
    ),
    tags$section(
      class = "report-local-section",
      div(class = "report-section-heading", div(h2(tr(language, "report_local_view")), p(report$territory_type_label))),
      div(
        class = "report-metric-grid",
        report_metric_card(
          tr(language, "report_maximum"),
          paste(report_format_number(selected$maximum_value, cfg$digits, language), unit),
          report_format_datetime(selected$maximum_date, language, timezone), TRUE
        ),
        report_metric_card(tr(language, "report_mean"), paste(report_format_number(selected$mean_value, cfg$digits, language), unit)),
        report_metric_card(tr(language, "report_hours_above"), report_format_hours(selected$hours_above_reference, language), reference_detail),
        report_metric_card(
          tr(language, "report_selected_rank"),
          paste(report$country_label, "·", report_format_rank(selected$national_rank, nrow(report$national), language)),
          paste(report$state_label, "·", report_format_rank(selected$state_rank, nrow(report$state), language))
        )
      ),
      category_section,
      if (nzchar(report_reference_note)) p(class = "report-reference-note", report_reference_note)
    ),
    tags$section(
      class = "report-comparison-section",
      div(
        class = "report-section-heading",
        div(h2(tr(language, "report_comparison_title")), p(tr(language, "report_interactive_hint")))
      ),
      div(
        class = "report-scope-tabs", role = "tablist",
        tags$button(
          type = "button", class = "report-scope-tab is-active",
          `data-report-scope-target` = "national", role = "tab", `aria-selected` = "true",
          tr(language, "report_national_view"), span(tr(language, "report_units", nrow(report$national)))
        ),
        tags$button(
          type = "button", class = "report-scope-tab",
          `data-report-scope-target` = "state", role = "tab", `aria-selected` = "false",
          tr(language, "report_state_view"), span(tr(language, "report_units", nrow(report$state)))
        )
      ),
      report_scope_panel("national", report$national, report, language, timezone, active = TRUE),
      report_scope_panel("state", report$state, report, language, timezone)
    ),
    p(class = "report-method-note", tr(language, "report_method_note"))
  )
}

territorial_report_html <- function(report, language, timezone) {
  css_path <- file.path("www", "styles.css")
  script_path <- file.path("www", "report.js")
  app_css <- if (file.exists(css_path)) paste(readLines(css_path, warn = FALSE), collapse = "\n") else ""
  report_script <- if (file.exists(script_path)) paste(readLines(script_path, warn = FALSE), collapse = "\n") else ""
  css <- paste(
    app_css,
    "html,body{width:auto;height:auto;min-height:100%;overflow:auto}",
    ".report-export{width:min(1380px,100%);margin:auto;padding:32px}",
    ".report-export .territorial-report{padding:0}",
    "@media(max-width:760px){.report-export{padding:12px}}",
    sep = "\n"
  )
  document <- tags$html(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title(tr(language, "report_title")),
      tags$style(HTML(css))
    ),
    tags$body(
      div(class = "report-export", territorial_report_view(report, language, timezone)),
      tags$script(HTML(report_script))
    )
  )
  paste0("<!doctype html>\n", as.character(document))
}

app_ui <- function(store) {
  catalog <- store$catalog[store$available]
  timezones <- timezone_catalog()
  indicator_choices <- stats::setNames(
    names(catalog),
    vapply(names(catalog), function(id) indicator_text("pt", id, "short", catalog[[id]]$short), character(1))
  )
  languages <- language_choices()

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
      tags$script(src = "report.js", defer = "defer"),
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
            class = "shiny-input-select form-control compact-native-select",
            title = "Idioma / Langue / Language",
            `aria-label` = "Idioma / Langue / Language",
            lapply(seq_along(languages), function(index) {
              code <- unname(languages[[index]])
              abbreviation <- toupper(code)
              tags$option(
                value = code,
                selected = if (identical(code, "pt")) "selected" else NULL,
                `data-short-label` = abbreviation,
                `data-full-label` = paste(abbreviation, names(languages)[[index]], sep = " · "),
                abbreviation
              )
            })
          )
        ),
        div(
          class = "timezone-control",
          icon("clock"),
          tags$select(
            id = "timezone",
            class = "shiny-input-select form-control compact-native-select",
            title = tr("pt", "timezone_label"),
            `aria-label` = tr("pt", "timezone_label"),
            lapply(seq_len(nrow(timezones)), function(index) {
              tags$option(
                value = timezones$timezone[[index]],
                selected = if (identical(timezones$timezone[[index]], "America/Sao_Paulo")) "selected" else NULL,
                `data-short-label` = timezones$code[[index]],
                `data-full-label` = timezones$label[[index]],
                timezones$code[[index]]
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
          "open_reports",
          label = tagList(icon("file-lines"), span(id = "label-reports", tr("pt", "reports"))),
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
          uiOutput("timeline_labels", container = div, class = "day-labels")
        )
      )
    )
  )
}
