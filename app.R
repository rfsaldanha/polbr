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
  "cachem",
  "curl",
  "ncdf4",
  "promises",
  "future",
  "parallelly"
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
async_workers <- suppressWarnings(as.integer(Sys.getenv("ALERTAR_ASYNC_WORKERS", "2")))
if (!is.finite(async_workers) || async_workers < 1L) async_workers <- 2L
available_workers <- max(1L, as.integer(parallelly::availableCores())[[1]])
async_workers <- min(async_workers, available_workers, 4L)
if (async_workers > 1L) {
  future::plan(future::multisession, workers = async_workers)
} else {
  future::plan(future::sequential)
}

invisible(lapply(
  c("R/config.R", "R/i18n.R", "R/glm.R", "R/data.R", "R/ui.R", "R/server.R"),
  sys.source,
  envir = environment()
))

data_dir <- resolve_data_dir()
store <- create_data_store(data_dir, indicator_catalog())
glm_store <- create_glm_store()

onStop(function() {
  store$close()
  glm_store$close()
  future::plan(future::sequential)
})

shiny::shinyApp(
  ui = app_ui(store),
  server = app_server(store, glm_store),
  options = list(launch.browser = TRUE)
)
