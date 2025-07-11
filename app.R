# Packages
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(sf)
library(leaflet)
library(DBI)
library(duckdb)
library(ggplot2)
library(terra)

# Database connection
con <- dbConnect(duckdb(), "data/cams_forecast.duckdb", read_only = TRUE)

# Table
tb_pm25 <- "pm25_mun_forecast"

# Forecast raster
rst_pm25 <- rast("data/cams_forecast_pm25.nc") * 1000000000
rst_pm25 <- project(x = rst_pm25, "EPSG:4326")

# Read municipality data
mun_seats <- readRDS("data/mun_seats.rds")

# Municipality list for selector
mun_names <- mun_seats$code_muni
names(mun_names) <- paste(mun_seats$name_muni, "-", mun_seats$abbrev_state)

# Municipality codes and names
ref_mun_names <- mun_seats |>
  st_drop_geometry() |>
  select(code_muni, name_muni, abbrev_state) |>
  mutate(name_muni = paste(name_muni, "-", abbrev_state)) |>
  select(-abbrev_state) |>
  as_tibble()

# Interface
ui <- page_navbar(
  title = "Poluentes",
  theme = bs_theme(bootswatch = "shiny"),

  # Logo
  tags$head(
    tags$script(
      HTML(
        '$(document).ready(function() {
             $(".navbar .container-fluid")
               .append("<img id = \'myImage\' src=\'selo_obs_h.png\' align=\'right\' height = \'57.5px\'>"  );
            });'
      )
    ),
    tags$style(
      HTML(
        '@media (max-width:992px) { #myImage { position: fixed; right: 10%; top: 0.5%; }}'
      )
    )
  ),

  # Translation
  tags$script(
    HTML(
      "
      $(document).ready(function() {
        // Change the text 'Expand' in all tooltips
        $('.card.bslib-card bslib-tooltip > div').each(function() {
          if ($(this).text().includes('Expand')) {
            $(this).text('Expandir');
          }
        });
  
        // Use MutationObserver to change the text 'Close'
        var observer = new MutationObserver(function(mutations) {
          $('.bslib-full-screen-exit').each(function() {
            if ($(this).html().includes('Close')) {
              $(this).html($(this).html().replace('Close', 'Fechar'));
            }
          });
        });
  
        // Observe all elements with the class 'card bslib-card'
        $('.card.bslib-card').each(function() {
          observer.observe(this, { 
            attributes: true, 
            attributeFilter: ['data-full-screen'] 
          });
        });
      });
    "
    )
  ),

  # Map page
  nav_panel(
    title = "Início",

    # Sidebar
    layout_sidebar(
      sidebar = sidebar(
        uiOutput(outputId = "update_time"),
        selectInput(
          inputId = "municipality",
          label = "Município",
          choices = NULL
        ),
        selectInput(
          inputId = "pollutant",
          label = "Poluente",
          choices = c("PM2.5")
        ),
        sliderInput(
          inputId = "forecast",
          label = "Previsão (horas)",
          min = 0,
          max = 120,
          value = 1,
          animate = TRUE
        )
      ),

      # Pane layout
      page_fillable(
        layout_columns(
          col_widths = c(6, 6),
          # Map card
          card(
            full_screen = TRUE,
            card_body(
              class = "p-0", # Fill card, used for maps
              leafletOutput(outputId = "map")
            )
          ),

          # Graph card
          card(
            full_screen = TRUE,
            card_header("Previsão no tempo"),
            plotOutput(outputId = "graph")
          )
        ),
      )
    )
  ),

  # Graphs page
  nav_panel(
    title = "Ranking",

    layout_sidebar(
      sidebar = sidebar(),
      # Table card
      card(
        card_header("Tabela")
      ),
    )
  ),

  # About page
  nav_panel(
    title = "Sobre",
    card(
      card_header("Card title"),
      p("Bla bla bla.")
    ),
    accordion(
      multiple = FALSE,
      accordion_panel(
        "Título A",
        p("Bla bla bla.")
      ),
      accordion_panel(
        "Título B",
        p("Bla bla bla.")
      ),
      accordion_panel(
        "Título C",
        p("Bla bla bla.")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Update municipality selector
  updateSelectizeInput(
    session = session,
    server = TRUE,
    inputId = "municipality",
    choices = mun_names
  )

  # Update time text
  output$update_time <- renderUI({
    res <- mun_data()
    res <- format(min(res$date), "%d/%m/%Y %H:%M")

    HTML(paste("Atualização:</br>", res))
  })

  # Map
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      fitBounds(-118.47, 33.28, -34.1, -56.65)
    # c(33.28, -118.47, -56.65, -34.1),
  })

  observeEvent(input$municipality, {
    req(input$municipality)

    leafletProxy("map", session) |>
      removeMarker(layerId = "mun_marker")

    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    leafletProxy("map", session) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker")
  })

  observeEvent(input$forecast, {
    mm <- minmax(rst_pm25)
    pal <- colorNumeric(
      palette = "Spectral",
      domain = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
      reverse = TRUE
    )

    leafletProxy("map", session) |>
      removeImage(layerId = "raster") |>
      removeControl(layerId = "legend")

    depth <- input$forecast + 1

    leafletProxy("map", session) |>
      addRasterImage(
        x = rst_pm25[[depth]],
        opacity = .7,
        colors = pal,
        layerId = "raster"
      ) |>
      addLegend(
        pal = pal,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend"
      )
  })

  # Graph
  mun_data <- reactive({
    req(input$municipality)
    req(input$pollutant)

    # Table selection
    table_name <- NULL
    if (input$pollutant == "PM2.5") {
      table_name <- tb_pm25
    }

    tbl(con, table_name) |>
      mutate(code_muni = substr(as.character(code_muni), 0, 6)) |>
      filter(code_muni == !!input$municipality) |>
      collect()
  })

  output$graph <- renderPlot({
    res <- mun_data()

    vline_value <- unique(res$date)[input$forecast + 1]

    ggplot(data = res, aes(x = date, y = value)) +
      geom_vline(xintercept = vline_value, col = "gray50") +
      geom_line(col = "red", lwd = 1) +
      ylim(c(0, NA)) +
      labs(
        title = "PM2.5 (μg/m³)",
        x = "Data e hora",
        y = "Valor previsto"
      ) +
      theme_light()
  })
}

shinyApp(ui, server)
