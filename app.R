required_packages <- c(
  "shiny",
  "bslib",
  "mapgl",
  "terra",
  "sf",
  "DBI",
  "duckdb",
  "jsonlite",
  "png",
  "cachem"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Pacotes ausentes: ",
    paste(missing_packages, collapse = ", "),
    ". Consulte o README para instalar as dependencias."
  )
}

suppressPackageStartupMessages(library(shiny))

options(shiny.autoreload = FALSE, shiny.maxRequestSize = 30 * 1024^2)

invisible(lapply(
  c("R/config.R", "R/i18n.R", "R/data.R", "R/ui.R", "R/server.R"),
  sys.source,
  envir = environment()
))

data_dir <- resolve_data_dir()
store <- create_data_store(data_dir, indicator_catalog())

onStop(function() store$close())

shiny::shinyApp(
  ui = app_ui(store),
  server = app_server(store),
  options = list(launch.browser = TRUE)
)
