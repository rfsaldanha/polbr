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

about_source_card <- function(icon_name, title, description, links) {
  link_tags <- lapply(seq_along(links), function(index) {
    tags$a(
      icon("arrow-up-right-from-square"), names(links)[[index]],
      href = unname(links[[index]]), target = "_blank", rel = "noopener noreferrer"
    )
  })
  div(
    class = "about-source-card",
    div(class = "about-source-title", icon(icon_name), tags$strong(title)),
    p(description),
    div(class = "about-source-links", link_tags)
  )
}

about_project_modal <- function(language = "pt") {
  source_link_label <- tr(language, "about_source_link")
  modalDialog(
    title = tagList(icon("info-circle"), tr(language, "about_title")),
    div(
      class = "about-grid",
      tags$section(
        class = "about-hero",
        div(
          class = "about-hero-copy",
          h3(tr(language, "about_project_heading")),
          p(tr(language, "about_project_p1")),
          p(tr(language, "about_project_p2"))
        ),
        tags$img(
          src = "pin_obs_horizontal_dark.png",
          alt = "Observatório de Clima e Saúde — ICICT — Fiocruz"
        )
      ),
      tags$section(
        class = "about-methods",
        h3(tr(language, "about_methods_heading")),
        div(
          class = "about-method-columns",
          div(
            p(tr(language, "about_methods_p1")),
            p(tr(language, "about_methods_p2"))
          ),
          div(
            p(tr(language, "about_methods_p3")),
            p(tr(language, "about_methods_p4"))
          )
        )
      ),
      tags$section(
        class = "about-sources",
        h3(tr(language, "about_sources_heading")),
        p(class = "about-sources-intro", tr(language, "about_sources_intro")),
        div(
          class = "about-source-grid",
          about_source_card(
            "cloud-sun", "CAMS / ECMWF", tr(language, "about_source_cams"),
            stats::setNames("https://ads.atmosphere.copernicus.eu/datasets/cams-global-atmospheric-composition-forecasts?tab=overview", source_link_label)
          ),
          about_source_card(
            "satellite", "NOAA GOES-East / NASA GIBS", tr(language, "about_source_goes"),
            stats::setNames("https://www.ncei.noaa.gov/products/goes-terrestrial-weather-abi-glm", source_link_label)
          ),
          about_source_card(
            "cloud-rain", "GPM IMERG / NASA", tr(language, "about_source_gpm"),
            stats::setNames("https://gpm.nasa.gov/data/imerg", source_link_label)
          ),
          about_source_card(
            "bolt", "GLM / NOAA", tr(language, "about_source_glm"),
            stats::setNames("https://www.nesdis.noaa.gov/our-satellites/currently-flying/goes-east-west/geostationary-lightning-mapper-glm", source_link_label)
          ),
          about_source_card(
            "fire", "BDQueimadas / INPE", tr(language, "about_source_fires"),
            stats::setNames("https://terrabrasilis.dpi.inpe.br/queimadas/portal/", source_link_label)
          ),
          about_source_card(
            "draw-polygon", "IBGE / geobr", tr(language, "about_source_territories"),
            stats::setNames("https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html", source_link_label)
          ),
          about_source_card(
            "map", "CARTO / OpenStreetMap", tr(language, "about_source_basemap"),
            c(
              "CARTO" = "https://carto.com/attributions",
              "OpenStreetMap" = "https://www.openstreetmap.org/copyright"
            )
          ),
          about_source_card(
            "book-medical", "OMS / CONAMA", tr(language, "about_source_references"),
            c(
              "OMS / WHO" = "https://www.who.int/teams/environment-climate-change-and-health/air-quality-and-health/health-impacts/types-of-pollutants",
              "CONAMA" = "https://www.gov.br/mma/pt-br/assuntos/meio-ambiente-urbano-recursos-hidricos-qualidade-ambiental/qualidade-do-ar/indice-de-qualidade-do-ar-iqar/orientacao-tecnica-indice-de-qualidade-do-ar-jan-25.pdf"
            )
          )
        )
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
        class = "about-coverage",
        h3(tr(language, "about_coverage_heading")),
        p(tr(language, "about_coverage_p1"))
      ),
      tags$section(
        class = "about-code",
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
      )
    ),
    footer = modalButton(tr(language, "close")),
    easyClose = TRUE,
    size = "xl",
    class = "about-modal-body"
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
    div(
      class = "report-output-shell",
      uiOutput("territorial_report", class = "territorial-report-output"),
      div(
        class = "report-loading-state", role = "status", `aria-live` = "polite",
        span(class = "report-loading-spinner", `aria-hidden` = "true"),
        span(tr(language, "report_loading"))
      )
    ),
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

report_unit_text <- function(unit, language = "pt") {
  localized_unit(unit, language)
}

report_ranking_rows <- function(data, report, language, timezone, limit = 10L) {
  cfg <- report$cfg
  unit <- report_unit_text(cfg$unit, language)
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
  unit <- report_unit_text(cfg$unit, language)
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
  unit <- report_unit_text(cfg$unit, language)

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
    hidden = if (!active) "hidden" else NULL,
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

territorial_report_view <- function(
  report, language, timezone,
  logo_src = "pin_obs_horizontal_dark.png"
) {
  if (is.null(report) || !isTRUE(report$available)) {
    return(div(class = "report-empty-state", tr(language, "report_data_unavailable")))
  }
  cfg <- report$cfg
  selected <- report$selected
  unit <- report_unit_text(cfg$unit, language)
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
        class = "report-hero-main",
        div(
          class = "report-hero-logo",
          tags$img(
            src = logo_src,
            alt = "Observatório de Clima e Saúde — ICICT — Fiocruz"
          )
        ),
        div(
          class = "report-hero-copy",
          span(class = "report-kicker", indicator_text(language, report$id, "short", cfg$short)),
          h2(report$territory$display_name[[1]]),
          p(tr(language, "report_intro"))
        )
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
  asset_path <- function(filename) {
    candidates <- unique(c(file.path("www", filename), file.path(getwd(), "www", filename)))
    existing <- candidates[file.exists(candidates)]
    if (!length(existing)) stop("Recurso do relatório não encontrado: ", filename)
    existing[[1]]
  }
  css_lines <- readLines(asset_path("styles.css"), warn = FALSE)
  css_lines <- css_lines[!grepl("^\\s*@import\\s+url", css_lines)]
  app_css <- paste(css_lines, collapse = "\n")
  report_script <- paste(readLines(asset_path("report.js"), warn = FALSE), collapse = "\n")
  report_script <- gsub("</script", "<\\/script", report_script, fixed = TRUE)
  logo_path <- asset_path("pin_obs_horizontal.png")
  logo_data <- paste0(
    "data:image/png;base64,",
    jsonlite::base64_enc(readBin(logo_path, what = "raw", n = file.info(logo_path)$size))
  )
  css <- paste(
    app_css,
    "*,*::before,*::after{box-sizing:border-box}",
    ":root{color-scheme:light;--ink:#102a43;--muted:#52697a;--line:rgba(31,78,95,.2);--glass:#fff;--accent:#007f73}",
    "html,body{width:auto;height:auto;min-height:100%;overflow:auto}",
    "body{margin:0;color:var(--ink);background:#eef3f5;line-height:1.4;-webkit-font-smoothing:antialiased}",
    "button,input,select{font:inherit}",
    "[hidden]{display:none!important}",
    ".report-export{width:min(1380px,100%);margin:auto;padding:28px 32px 36px}",
    ".report-export .territorial-report{padding:0}",
    ".report-export .report-hero{background:linear-gradient(125deg,#d9f4ef 0%,#e7f0fb 58%,#fff4dc 100%);border-color:#afcdd0;box-shadow:0 10px 30px rgba(23,56,75,.08)}",
    ".report-export .report-hero-logo{background:#fff;border-color:#c5d6d9}",
    ".report-export .report-period{color:#29495a}",
    ".report-export .territorial-report section{background:#fff;border-color:#cbdcdf;box-shadow:0 8px 24px rgba(23,56,75,.06)}",
    ".report-export .report-metric{background:#f4f8f9;border-color:#cbdcdf}",
    ".report-export .report-metric.is-accent{background:#dff5f0;border-color:#5cb9aa}",
    ".report-export .report-metric.is-accent strong,.report-export .report-kicker{color:#006f65}",
    ".report-export .report-visual-card,.report-export .report-category-row{background:#f5f8fa;border-color:#d5e1e4}",
    ".report-export .report-category-meta strong{color:#29495a}",
    ".report-export .report-band-chip{color:color-mix(in srgb,var(--band-color),#102a43 42%);background:color-mix(in srgb,var(--band-color),#fff 86%);border-color:color-mix(in srgb,var(--band-color),#52697a 35%)}",
    ".report-export .report-scope-tabs{background:#e6eef1;border-color:#c4d4d9}",
    ".report-export .report-scope-tab{color:#415b6c}",
    ".report-export .report-scope-tab:hover{color:#0b4f4a;background:#d6ebe7}",
    ".report-export .report-scope-tab.is-active{color:#fff;background:#007f73}",
    ".report-export .report-chart-label{color:#405c6c}",
    ".report-export .report-chart-label strong{color:#17384a}",
    ".report-export .report-chart-track,.report-export .report-category-track{background:#dbe6e9}",
    ".report-export .report-chart-track span{background:linear-gradient(90deg,#2563a6,#00a990)}",
    ".report-export .report-chart-row.is-selected{background:#fff4d6;border-color:#e3aa32}",
    ".report-export .report-chart-row.is-selected .report-chart-label span,.report-export .report-chart-row.is-selected .report-chart-label strong{color:#8a4b00}",
    ".report-export .report-chart-row.is-selected .report-chart-track span{background:linear-gradient(90deg,#ef7d32,#f3bd32)}",
    ".report-export .report-search-control,.report-export .report-page-size-control select{color:#17384a;background:#fff;border-color:#bfd1d7}",
    ".report-export .report-ranking-table th{color:#3d5968;background:#dfeaec}",
    ".report-export .report-ranking-table td{color:#29495a;border-color:#dbe5e8}",
    ".report-export .report-ranking-table tbody tr.is-selected{background:#dff5f0}",
    ".report-export .report-ranking-table tbody tr:hover{background:#edf7f5}",
    ".report-export .report-value-cell{color:#102a43!important}",
    ".report-export .report-table-pagination button{color:#29495a;background:#fff;border-color:#bfd1d7}",
    ".report-export-footer{display:flex;justify-content:space-between;gap:20px;margin-top:14px;padding:12px 2px;color:var(--muted);font:9px/1.45 'Space Mono',monospace}",
    ".report-export .report-search-control::before{content:'⌕';color:var(--muted);font-size:14px}",
    ".report-export .report-search-control>.svg-inline--fa{display:none}",
    "@media(max-width:760px){.report-export{padding:12px}.report-export-footer{flex-direction:column}}",
    "@media print{@page{margin:12mm}*{-webkit-print-color-adjust:exact;print-color-adjust:exact}body{background:#fff}.report-export{width:100%;max-width:none;padding:0}.report-hero{break-inside:avoid}.report-scope-tabs,.report-table-toolbar,.report-table-pagination{display:none!important}.report-export .report-scope-panel[hidden]{display:block!important}.report-scope-panel+.report-scope-panel{margin-top:16px;break-before:page}.report-table-scroll{overflow:visible}.report-ranking-table{min-width:0}.report-export-footer{break-inside:avoid}}",
    sep = "\n"
  )
  document <- tags$html(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$meta(name = "color-scheme", content = "light"),
      tags$meta(name = "theme-color", content = "#eef3f5"),
      tags$title(tr(language, "report_title")),
      tags$style(HTML(css))
    ),
    tags$body(
      div(
        class = "report-export",
        territorial_report_view(report, language, timezone, logo_src = logo_data),
        tags$footer(
          class = "report-export-footer",
          span("Observatório de Clima e Saúde · LIS/ICICT/Fiocruz"),
          span(paste(
            indicator_text(language, report$id, "short", report$cfg$short),
            report$territory$display_name[[1]], sep = " · "
          ))
        )
      ),
      tags$script(HTML(report_script))
    )
  )
  rendered <- htmltools::renderTags(document)
  html <- sub(
    "<html>",
    paste0("<html>\n<head>\n", as.character(rendered$head), "\n</head>"),
    as.character(rendered$html), fixed = TRUE
  )
  paste0("<!doctype html>\n", html)
}

app_ui <- function(store) {
  catalog <- store$catalog[store$available]
  weather_catalog <- weather_observation_catalog()
  default_weather_source <- names(weather_catalog)[[1]]
  default_weather_product <- names(weather_catalog[[default_weather_source]]$products)[[1]]
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
                `data-full-label` = timezone_full_labels("pt")[[index]],
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
        div(
          class = "panel-heading layer-panel-heading",
          icon("layer-group", class = "layer-heading-icon"),
          span(id = "label-layer-heading", tr("pt", "layer_heading"))
        ),
        tags$details(
          class = "layer-group forecast-layer-group",
          open = NA,
          tags$summary(
            div(
              class = "layer-group-title",
              span(class = "layer-group-icon", icon("cloud-sun")),
              span(id = "label-forecast-section", tr("pt", "forecast_section"))
            ),
            span(id = "label-forecast-badge", class = "layer-group-badge forecast-badge", tr("pt", "forecast_badge")),
            icon("chevron-down", class = "layer-group-chevron")
          ),
          div(
            class = "layer-group-body",
            p(id = "label-forecast-note", class = "layer-group-note", tr("pt", "forecast_section_note")),
            tags$label(
              `for` = "indicator", id = "label-forecast-variable", class = "layer-field-label",
              tr("pt", "forecast_variable")
            ),
            selectInput("indicator", NULL, choices = indicator_choices, selected = store$default_indicator),
            uiOutput("indicator_summary"),
            div(
              class = "forecast-transparency-control",
              sliderInput(
                "forecast_transparency", tr("pt", "forecast_transparency"),
                min = 0, max = 100, value = 18, step = 5, post = "%", ticks = FALSE
              )
            ),
            div(
              class = "layer-option forecast-layer-option",
              checkboxInput("show_wind", tr("pt", "wind_particles"), value = store$wind_available()),
              div(
                class = "layer-option-meta",
                span(id = "label-wind-detail", tr("pt", "wind_detail"))
              )
            )
          )
        ),
        tags$details(
          class = "layer-group recent-layer-group",
          open = NA,
          tags$summary(
            div(
              class = "layer-group-title",
              span(class = "layer-group-icon", icon("satellite-dish")),
              span(id = "label-recent-section", tr("pt", "recent_section"))
            ),
            span(id = "label-recent-badge", class = "layer-group-badge recent-badge", tr("pt", "recent_badge")),
            icon("chevron-down", class = "layer-group-chevron")
          ),
          div(
            class = "layer-group-body",
            p(id = "label-recent-note", class = "layer-group-note", tr("pt", "recent_section_note")),
            div(
              class = "weather-layer-control recent-layer-options",
              div(
                class = "layer-option lightning-layer-option",
                checkboxInput("show_lightning", tr("pt", "lightning_flashes"), value = FALSE),
                div(
                  class = "layer-option-meta",
                  span(id = "label-lightning-detail", tr("pt", "lightning_detail")),
                  span(id = "label-live-badge", class = "freshness-badge live-badge", tr("pt", "live_badge"))
                ),
                conditionalPanel(
                  condition = "input.show_lightning === true",
                  uiOutput("lightning_status", class = "lightning-status-output")
                )
              ),
              div(
                class = "layer-option fire-layer-option",
                checkboxInput("show_fires", tr("pt", "heat_spots"), value = TRUE),
                div(
                  class = "layer-option-meta",
                  span(id = "label-fires-detail", tr("pt", "heat_spots_detail")),
                  span(id = "label-near-live-fires", class = "freshness-badge near-live-badge", tr("pt", "near_live_badge"))
                ),
                conditionalPanel(
                  condition = "input.show_fires === true",
                  uiOutput("fire_status", class = "fire-status-output")
                )
              ),
              div(
                class = "layer-option weather-layer-option",
                checkboxInput("show_weather", tr("pt", "weather_imagery"), value = FALSE),
                div(
                  class = "layer-option-meta",
                  span(id = "label-weather-detail", tr("pt", "weather_detail")),
                  span(id = "label-near-live-weather", class = "freshness-badge near-live-badge", tr("pt", "near_live_badge"))
                ),
                conditionalPanel(
                  condition = "input.show_weather === true",
                  div(
                    class = "weather-source-card",
                    div(class = "weather-card-kicker", icon("sliders"), span(id = "label-weather-heading", tr("pt", "weather_heading"))),
                    tags$label(`for` = "weather_source", id = "label-weather-source", tr("pt", "weather_source")),
                    selectizeInput(
                      "weather_source", NULL,
                      choices = stats::setNames(default_weather_source, tr("pt", weather_catalog[[default_weather_source]]$label_key)),
                      selected = default_weather_source,
                      options = list(
                        dropdownParent = "body",
                        dropdownClass = "selectize-dropdown weather-layer-dropdown"
                      )
                    ),
                    tags$label(`for` = "weather_product", id = "label-weather-product", tr("pt", "weather_product")),
                    selectizeInput(
                      "weather_product", NULL,
                      choices = stats::setNames(
                        names(weather_catalog[[default_weather_source]]$products),
                        vapply(
                          weather_catalog[[default_weather_source]]$products,
                          function(product) tr("pt", product$label_key),
                          character(1)
                        )
                      ),
                      selected = default_weather_product,
                      options = list(
                        dropdownParent = "body",
                        dropdownClass = "selectize-dropdown weather-layer-dropdown"
                      )
                    ),
                    uiOutput("weather_status", class = "weather-status-output")
                  )
                )
              )
            )
          )
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
