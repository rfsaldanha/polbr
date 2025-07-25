# Packages
library(shiny)
library(shinyWidgets)
library(bslib)
library(dplyr)
library(lubridate)
library(sf)
library(leaflet)
library(DBI)
library(duckdb)
library(ggplot2)
library(geomtextpath)
library(terra)
library(DT)
library(readr)

# Database connection
# con <- dbConnect(
#   duckdb(),
#   "/dados/home/rfsaldanha/camsdata/cams_forecast.duckdb",
#   read_only = TRUE
# )
con <- dbConnect(
  duckdb(),
  "data/cams_forecast.duckdb",
  read_only = TRUE
)

# Table
tb_pm25 <- "pm25_mun_forecast"
tb_o3 <- "o3_mun_forecast"

# Read forecast rasters
# rst_pm25 <- rast("/dados/home/rfsaldanha/camsdata/cams_forecast_pm25.nc") *
#   1000000000 # kg/m3 to μg/m3
rst_pm25 <- rast("data/cams_forecast_pm25.nc") *
  1000000000 # kg/m3 to μg/m3
rst_pm25 <- project(x = rst_pm25, "EPSG:3857")
rst_o3 <- rast("data/cams_forecast_o3.nc") *
  44698 # kg/m2 to DU
rst_o3 <- project(x = rst_o3, "EPSG:3857")

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
  title = "Previsão de poluentes atmosféricos",
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

  sidebar = sidebar(
    uiOutput(outputId = "update_time"),
    pickerInput(
      inputId = "municipality",
      label = "Município",
      choices = NULL
    ),
    sliderInput(
      inputId = "forecast",
      label = "Previsão (horas)",
      min = 0,
      max = 120,
      value = 24,
      animate = TRUE
    ),
    uiOutput(outputId = "forecast_time"),
    checkboxInput(
      inputId = "trend_line",
      label = "Linha de tendência",
      value = TRUE
    ),
    checkboxInput(
      inputId = "oms_line",
      label = "Limite OMS",
      value = TRUE
    ),
    checkboxInput(
      inputId = "conama_line",
      label = "Limite CONAMA",
      value = TRUE
    ),
    downloadButton(outputId = "download_data", label = "Download")
  ),

  # PM2.5
  nav_panel(
    title = "PM 2.5",
    page_fillable(
      layout_columns(
        col_widths = c(6, 6),
        # Map card
        card(
          full_screen = TRUE,
          card_body(
            class = "p-0", # Fill card, used for maps
            leafletOutput(outputId = "map_pm25")
          )
        ),

        # Graph card
        card(
          full_screen = TRUE,
          plotOutput(outputId = "graph_pm25")
        )
      )
    )
  ),

  # O3
  nav_panel(
    title = "Ozônio (O3)",
    page_fillable(
      layout_columns(
        col_widths = c(6, 6),
        # Map card
        card(
          full_screen = TRUE,
          card_body(
            class = "p-0", # Fill card, used for maps
            leafletOutput(outputId = "map_o3")
          )
        ),

        # Graph card
        card(
          full_screen = TRUE,
          plotOutput(outputId = "graph_o3")
        )
      )
    )
  ),

  # Alerts page
  nav_panel(
    title = "Alertas",

    card(
      tabsetPanel(
        tabPanel(
          title = "Valores máximos",
          DTOutput("rank_max")
        ),
        tabPanel(
          title = "Horas acumuladas acima de 15 μg/m³ (OMS)",
          DTOutput(outputId = "rank_oms")
        ),
        tabPanel(
          title = "Horas acumuladas acima de 50 μg/m³ (CONAMA)",
          DTOutput("rank_conama")
        )
      )
    )
  ),

  # About page
  nav_panel(
    title = "Sobre",
    accordion(
      multiple = FALSE,
      accordion_panel(
        "Projeto",
        p(
          "Este painel tem por objetivo apresentar a previsão de concentração de material particulado de 2,5 μg/m³ (PM2.5) por município."
        )
      ),
      accordion_panel(
        "Fonte dos dados",
        p(
          "Os dados de projeção de PM2.5 são obtidos diariamente duas vezes por dia consultando a API do Copernicus/CAMS. Uma atualização é feita a meia-noite e outra meio-dia."
        )
      ),
      accordion_panel(
        "Método",
        p(
          "Em cada atualiazção de dados, a estimativa de 0 a 120 horas é obtida junto ao Copernicus/CAMS. Após o download dos dados, estes são processados e municipalizados utilizando a estatística zonal de média."
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Update municipality selector
  updatePickerInput(
    session = session,
    # server = TRUE,
    inputId = "municipality",
    choices = mun_names,
    options = pickerOptions(container = "body", liveSearch = TRUE)
  )

  # Update time text
  output$update_time <- renderUI({
    res <- mun_data_pm25()
    res <- format(min(res$date), "%d/%m/%Y %H:%M")

    HTML(paste("Atualização:</br>", res))
  })

  # Update forecast time
  output$forecast_time <- renderUI({
    res <- mun_data_pm25()
    forecast_date <- unique(res$date)[input$forecast + 1]
    forecast_date <- format(forecast_date, "%d/%m/%Y %H:%M")

    HTML(paste0("<em>", forecast_date), "</em>")
  })

  # Map PM2.5 initial state
  output$map_pm25 <- renderLeaflet({
    req(input$municipality)

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Palette
    mm <- minmax(rst_pm25)
    pal <- colorBin(
      palette = "YlOrRd",
      bins = c(15, 30, 50, 100, 200, 300, 500, Inf),
      na.color = NA,
      reverse = FALSE
    )

    # Depth (forecast)
    depth <- input$forecast + 1

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-118, 33, -30, -56) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker") |>
      addRasterImage(
        x = rst_pm25[[depth]],
        opacity = .7,
        colors = pal,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addLegend(
        pal = pal,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("PM2.5 (μg/m³)")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Update municipality marker on map pm25
  observeEvent(input$municipality, {
    req(input$municipality)

    # Remove old layer
    leafletProxy("map_pm25", session) |>
      removeMarker(layerId = "mun_marker")

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Update map
    leafletProxy("map_pm25", session) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker")
  })

  # Update raster and date text on map
  observeEvent(input$forecast, {
    # Palette
    mm <- minmax(rst_pm25)
    pal <- colorBin(
      palette = "YlOrRd",
      bins = c(15, 30, 50, 100, 200, 300, 500, Inf),
      na.color = NA,
      reverse = FALSE
    )

    # Remove old layers
    leafletProxy("map_pm25", session) |>
      removeImage(layerId = "raster") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- input$forecast + 1

    # Update map
    leafletProxy("map_pm25", session) |>
      addRasterImage(
        x = rst_pm25[[depth]],
        opacity = .7,
        colors = pal,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addLegend(
        pal = pal,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("PM2.5 (μg/m³)")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Graph pm25
  mun_data_pm25 <- reactive({
    req(input$municipality)

    tbl(con, tb_pm25) |>
      mutate(code_muni = substr(as.character(code_muni), 0, 6)) |>
      filter(code_muni == !!input$municipality) |>
      collect() |>
      mutate(date = with_tz(date, "America/Sao_Paulo"))
  })

  output$graph_pm25 <- renderPlot({
    res <- mun_data_pm25()

    vline_value <- unique(res$date)[input$forecast + 1]

    g <- ggplot(data = res, aes(x = date, y = value)) +
      geom_line(col = "red", lwd = 1) +
      geom_vline(xintercept = vline_value, col = "gray50") +
      ylim(c(0, NA)) +
      scale_x_datetime(date_labels = "%d %b", date_breaks = "1 day") +
      labs(
        title = "Previsão de PM2.5 (μg/m³)",
        subtitle = paste0(names(mun_names[mun_names == input$municipality])),
        caption = paste0(
          "Previsão atmosférica: Copernicus/CAMS\n",
          "Atualização: ",
          format(min(res$date), "%d/%m/%Y %H:%M"),
          "\n",
          "Análise: LIS/ICICT/Fiocruz"
        ),
        x = "Data e hora",
        y = "Valor previsto"
      ) +
      theme_light()

    if (input$trend_line == TRUE) {
      g <- g +
        geom_smooth(color = "purple", se = TRUE, size = 0.7)
    }

    if (input$oms_line == TRUE) {
      g <- g +
        geom_texthline(
          yintercept = 15,
          label = "OMS",
          hjust = 0.1,
          color = "blue",
          linetype = "dashed"
        )
    }

    if (input$conama_line == TRUE) {
      g <- g +
        geom_texthline(
          yintercept = 50,
          label = "CONAMA",
          hjust = 0.1,
          color = "blue",
          linetype = "dashed"
        )
    }

    g
  })

  # Download pm25
  output$download_data <- downloadHandler(
    filename = function() {
      res <- mun_data_pm25()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("pm25_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_pm25(), file)
    }
  )

  # Map O3 initial state
  output$map_o3 <- renderLeaflet({
    req(input$municipality)

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Palette
    mm <- minmax(rst_o3)
    pal <- colorBin(
      palette = "RdPu",
      bins = c(100, 150, 200, 250, 300, 350, 400, Inf),
      na.color = NA,
      reverse = FALSE
    )

    # Depth (forecast)
    depth <- input$forecast + 1

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-118, 33, -30, -56) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker") |>
      addRasterImage(
        x = rst_o3[[depth]],
        opacity = .7,
        colors = pal,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addLegend(
        pal = pal,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("O3 (DU)")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Update municipality marker on map o3
  observeEvent(input$municipality, {
    req(input$municipality)

    # Remove old layer
    leafletProxy("map_o3", session) |>
      removeMarker(layerId = "mun_marker")

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Update map
    leafletProxy("map_o3", session) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker")
  })

  # Update raster and date text on map
  observeEvent(input$forecast, {
    # Palette
    mm <- minmax(rst_o3)
    pal <- colorBin(
      palette = "RdPu",
      bins = c(100, 150, 200, 250, 300, 350, 400, Inf),
      na.color = NA,
      reverse = FALSE
    )

    # Remove old layers
    leafletProxy("map_o3", session) |>
      removeImage(layerId = "raster") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- input$forecast + 1

    # Update map
    leafletProxy("map_o3", session) |>
      addRasterImage(
        x = rst_o3[[depth]],
        opacity = .7,
        colors = pal,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addLegend(
        pal = pal,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("O3 (DU)")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Graph o3
  mun_data_o3 <- reactive({
    req(input$municipality)

    tbl(con, tb_o3) |>
      mutate(code_muni = substr(as.character(code_muni), 0, 6)) |>
      filter(code_muni == !!input$municipality) |>
      collect() |>
      mutate(date = with_tz(date, "America/Sao_Paulo"))
  })

  output$graph_o3 <- renderPlot({
    res <- mun_data_o3()

    vline_value <- unique(res$date)[input$forecast + 1]

    g <- ggplot(data = res, aes(x = date, y = value)) +
      geom_line(col = "red", lwd = 1) +
      geom_vline(xintercept = vline_value, col = "gray50") +
      ylim(c(100, NA)) +
      scale_x_datetime(date_labels = "%d %b", date_breaks = "1 day") +
      labs(
        title = "Previsão de O3 (DU)",
        subtitle = paste0(names(mun_names[mun_names == input$municipality])),
        caption = paste0(
          "Previsão atmosférica: Copernicus/CAMS\n",
          "Atualização: ",
          format(min(res$date), "%d/%m/%Y %H:%M"),
          "\n",
          "Análise: LIS/ICICT/Fiocruz"
        ),
        x = "Data e hora",
        y = "Valor previsto"
      ) +
      theme_light()

    if (input$trend_line == TRUE) {
      g <- g +
        geom_smooth(color = "purple", se = TRUE, size = 0.7)
    }

    g
  })

  # Alerts
  output$rank_max <- renderDT({
    tbl(con, tb_pm25) |>
      group_by(code_muni) |>
      filter(value == max(value)) |>
      ungroup() |>
      arrange(-value) |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      collect() |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      mutate(date = format(date, "%d/%m/%Y %H:%M")) |>
      rename(`Município` = name_muni, `Data e hora` = date, `PM2.5` = value)
  })

  output$rank_oms <- renderDT({
    tbl(con, tb_pm25) |>
      mutate(ref = ifelse(value > 15, TRUE, FALSE)) |>
      filter(ref == TRUE) |>
      group_by(code_muni) |>
      summarise(freq = n()) |>
      ungroup() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      collect() |>
      arrange(-freq) |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      rename(`Município` = name_muni, `Horas` = freq)
  })

  output$rank_conama <- renderDT({
    tbl(con, tb_pm25) |>
      mutate(ref = ifelse(value > 50, TRUE, FALSE)) |>
      filter(ref == TRUE) |>
      group_by(code_muni) |>
      summarise(freq = n()) |>
      ungroup() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      collect() |>
      arrange(-freq) |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      rename(`Município` = name_muni, `Horas` = freq)
  })
}

shinyApp(ui, server)
