# Packages
library(shiny)
library(shinyWidgets)
library(fs)
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
options(DT.options = list(pageLength = 5, dom = 'ftp'))

# Data dir
# data_dir <- path("/dados/home/rfsaldanha/camsdata/forecast_data/")
data_dir <- path("data/")

# Database connection
con <- dbConnect(
  duckdb(),
  path(data_dir, "cams_forecast.duckdb"),
  read_only = TRUE
)

# Table
tb_pm25 <- "pm25_mun_forecast"
tb_o3 <- "o3_mun_forecast"
tb_temp <- "temp_mun_forecast"
tb_uv <- "uv_mun_forecast"

# Read forecast rasters
rst_pm25 <- rast(
  path(data_dir, "cams_forecast_pm25.nc")
) *
  1000000000 # kg/m3 to μg/m3
rst_pm25 <- project(x = rst_pm25, "EPSG:3857")

rst_o3 <- rast(
  path(data_dir, "cams_forecast_o3.nc")
) *
  28.9644 /
  47.9982 *
  1e9 # # kg/kg to μg/m3
rst_o3 <- project(x = rst_o3, "EPSG:3857")

rst_temp <- rast(
  path(data_dir, "cams_forecast_temp.nc")
) -
  272.15 # K to °C
rst_temp <- project(x = rst_temp, "EPSG:3857")

rst_uv <- rast(
  path(data_dir, "cams_forecast_uv.nc")
) *
  40 # Wm2 to UVI
rst_uv <- project(x = rst_uv, "EPSG:3857")

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

# bdqueimadas data
bdq_focos <- readRDS(file = path(data_dir, "bdq_focos.rds"))

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
      step = 3,
      value = 24,
      animate = TRUE
    ),
    uiOutput(outputId = "forecast_time"),
    checkboxInput(
      inputId = "trend_line",
      label = "Linha de tendência",
      value = TRUE
    )
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

        accordion(
          multiple = FALSE,
          accordion_panel(
            "Gráfico",
            card(
              full_screen = TRUE,
              plotOutput(outputId = "graph_pm25")
            )
          ),
          accordion_panel(
            "Configuração do gráfico e download",
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
            downloadButton(outputId = "download_data_pm25", label = "CSV")
          ),
          accordion_panel(
            "Descrição",
            HTML(
              "O material particulado fino (PM2.5), composto por partículas com diâmetro aerodinâmico inferior a 2,5 micrômetros, é um importante indicador de poluição atmosférica devido à sua capacidade de penetrar profundamente nos pulmões e alcançar a corrente sanguínea. O monitoramento do PM2.5 é fundamental para avaliar os riscos associados à exposição prolongada a poluentes, especialmente em áreas urbanas e industrializadas. Alguns estudos têm associado concentrações elevadas de PM2.5 ao aumento da incidência de doenças respiratórias e cardiovasculares, além de impactos adversos no desenvolvimento infantil e no envelhecimento. O uso desse indicador permite a formulação de políticas públicas de controle da poluição do ar, a avaliação de desigualdades ambientais e a implementação de estratégias preventivas voltadas à proteção da saúde das populações mais vulneráveis."
            )
          )
        )
      )
    )
  ),

  # Temperature
  nav_panel(
    title = "Temperatura",
    page_fillable(
      layout_columns(
        col_widths = c(6, 6),
        # Map card
        card(
          full_screen = TRUE,
          card_body(
            class = "p-0", # Fill card, used for maps
            leafletOutput(outputId = "map_temp")
          )
        ),

        accordion(
          multiple = FALSE,
          accordion_panel(
            "Gráfico",
            card(
              full_screen = TRUE,
              plotOutput(outputId = "graph_temp")
            )
          ),
          accordion_panel(
            "Download",
            downloadButton(outputId = "download_data_temp", label = "CSV")
          ),
          accordion_panel(
            "Descrição",
            HTML(
              "A temperatura do ar é um dos principais determinantes climáticos com impacto direto e indireto na saúde humana. O monitoramento das variações de temperatura é essencial para compreender e prevenir agravos relacionados ao calor extremo, como desidratação, insolação, agravamento de doenças cardiovasculares e respiratórias, além de aumento da mortalidade em populações vulneráveis, como idosos e crianças. Ondas de calor têm sido associadas a surtos de morbimortalidade em diversas regiões do mundo, enquanto temperaturas mais baixas também podem aumentar o risco de infecções respiratórias. Além disso, a temperatura influencia a dinâmica de vetores de doenças, como os mosquitos transmissores da dengue, zika e chikungunya. A análise de padrões térmicos é fundamental para o planejamento de ações de vigilância em saúde, desenvolvimento de sistemas de alerta precoce e formulação de políticas de adaptação às mudanças climáticas."
            )
          )
        )
      )
    )
  ),

  # UV
  nav_panel(
    title = "Índice UV",
    page_fillable(
      layout_columns(
        col_widths = c(6, 6),
        # Map card
        card(
          full_screen = TRUE,
          card_body(
            class = "p-0", # Fill card, used for maps
            leafletOutput(outputId = "map_uv")
          )
        ),

        accordion(
          multiple = FALSE,
          accordion_panel(
            "Gráfico",
            card(
              full_screen = TRUE,
              plotOutput(outputId = "graph_uv")
            )
          ),
          accordion_panel(
            "Download",
            downloadButton(outputId = "download_data_uv", label = "CSV")
          ),
          accordion_panel(
            "Descrição",
            HTML(
              "O Índice Ultravioleta (Índice UV) é um indicador que quantifica a intensidade da radiação solar ultravioleta na superfície da Terra, sendo fundamental para avaliar o risco de danos à saúde causados pela exposição excessiva ao sol. Esse índice é amplamente utilizado para orientar a população sobre medidas de proteção solar, especialmente em horários de maior radiação, contribuindo para a prevenção de doenças como câncer de pele, queimaduras, envelhecimento precoce e danos oculares, como catarata. O monitoramento do Índice UV permite a emissão de alertas diários e sazonais, facilitando a adoção de comportamentos preventivos, como o uso de protetor solar, roupas adequadas e a limitação da exposição ao sol."
            )
          )
        )
      )
    )
  ),

  # O3
  nav_panel(
    title = "Ozônio",
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

        accordion(
          multiple = FALSE,
          accordion_panel(
            "Gráfico",
            card(
              full_screen = TRUE,
              plotOutput(outputId = "graph_o3")
            )
          ),
          accordion_panel(
            "Download",
            downloadButton(outputId = "download_data_o3", label = "CSV")
          ),
          accordion_panel(
            "Descrição",
            HTML(
              "O ozônio ao nível do solo (ozônio troposférico) é um poluente. Diferente do ozônio estratosférico, que é benéfico para a redução do aquecimento global, o ozônio troposférico é prejudicial à saúde humana e ao meio ambiente. Em saúde pública, sua concentração elevada está associada a uma série de efeitos adversos, especialmente respiratórios e cardiovasculares.  O monitoramento do ozônio ao nível do solo é fundamental para a emissão de alertas de qualidade do ar, subsidiando políticas de controle da poluição atmosférica e estratégias preventivas voltadas à proteção da saúde da população exposta, especialmente em áreas urbanas e industrializadas."
            )
          )
        )
      )
    )
  ),

  # Alerts page
  nav_panel(
    title = "Alertas",

    accordion(
      multiple = FALSE,
      accordion_panel(
        "PM 2.5",
        tabsetPanel(
          tabPanel(
            title = "Valores máximos",
            DTOutput("rank_pm25_max")
          ),
          tabPanel(
            title = "Horas acumuladas acima de 15 μg/m³ (OMS)",
            DTOutput(outputId = "rank_pm25_oms")
          ),
          tabPanel(
            title = "Horas acumuladas acima de 50 μg/m³ (CONAMA)",
            DTOutput("rank_pm25_conama")
          )
        )
      ),
      accordion_panel(
        "Temperatura",
        tabsetPanel(
          tabPanel(
            title = "Temperaturas máximas",
            DTOutput("rank_temp_max")
          ),
          tabPanel(
            title = "Temperaturas mínimas",
            DTOutput("rank_temp_min")
          ),
          tabPanel(
            title = "Horas acima de 35°C",
            DTOutput("rank_temp_35")
          ),
          tabPanel(
            title = "Horas abaixo de 10°C",
            DTOutput("rank_temp_10")
          ),
        )
      ),
      accordion_panel(
        "Índice UV",
        tabsetPanel(
          tabPanel(
            title = "Valores máximos",
            DTOutput("rank_uv_max")
          ),
          tabPanel(
            title = "Horas com UV moderado (3) ou maior",
            DTOutput("rank_uv_3")
          ),
          tabPanel(
            title = "Horas com UV alto (6) ou maior",
            DTOutput("rank_uv_6")
          ),
          tabPanel(
            title = "Horas com UV muito alto (8) ou maior",
            DTOutput("rank_uv_8")
          ),
          tabPanel(
            title = "Horas com UV extremo (11) ou maior",
            DTOutput("rank_uv_11")
          )
        )
      ),
      accordion_panel(
        "Ozônio"
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
    depth <- 24 + 1

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-118, 33, -30, -56) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker") |>
      addCircleMarkers(
        lng = bdq_focos$lon,
        lat = bdq_focos$lat,
        fillColor = "firebrick",
        fillOpacity = .5,
        radius = 4,
        stroke = FALSE,
        group = "INPE/BDQueimadas"
      ) |>
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
        overlayGroups = c("raster", "INPE/BDQueimadas"),
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
        overlayGroups = c("raster", "INPE/BDQueimadas"),
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
          "Elaboração: LIS/ICICT/Fiocruz"
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
  output$download_data_pm25 <- downloadHandler(
    filename = function() {
      res <- mun_data_pm25()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("pm25_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_pm25(), file)
    }
  )

  # Map temperature initial state
  output$map_temp <- renderLeaflet({
    req(input$municipality)

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Palette
    mm <- minmax(rst_temp)
    pal <- colorBin(
      palette = "RdYlBu",
      bins = c(-Inf, 0, 10, 15, 20, 25, 30, 35, 40, Inf),
      na.color = NA,
      reverse = TRUE
    )

    # Depth (forecast)
    depth <- 24 + 1

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-118, 33, -30, -56) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker") |>
      addRasterImage(
        x = rst_temp[[depth]],
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
        title = paste0("Temperatura (°C)")
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

  # Update municipality marker on map temperature
  observeEvent(input$municipality, {
    req(input$municipality)

    # Remove old layer
    leafletProxy("map_temp", session) |>
      removeMarker(layerId = "mun_marker")

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Update map
    leafletProxy("map_temp", session) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker")
  })

  # Update raster and date text on map
  observeEvent(input$forecast, {
    # Palette
    mm <- minmax(rst_temp)
    pal <- colorBin(
      palette = "RdYlBu",
      bins = c(-Inf, 0, 10, 15, 20, 25, 30, 35, 40, Inf),
      na.color = NA,
      reverse = TRUE
    )

    # Remove old layers
    leafletProxy("map_temp", session) |>
      removeImage(layerId = "raster") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- input$forecast + 1

    # Update map
    leafletProxy("map_temp", session) |>
      addRasterImage(
        x = rst_temp[[depth]],
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
        title = paste0("Temperatura (°C)")
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

  # Graph temperature
  mun_data_temp <- reactive({
    req(input$municipality)

    tbl(con, tb_temp) |>
      mutate(code_muni = substr(as.character(code_muni), 0, 6)) |>
      filter(code_muni == !!input$municipality) |>
      collect() |>
      mutate(date = with_tz(date, "America/Sao_Paulo"))
  })

  output$graph_temp <- renderPlot({
    res <- mun_data_temp()

    vline_value <- unique(res$date)[input$forecast + 1]

    g <- ggplot(data = res, aes(x = date, y = value)) +
      geom_line(col = "red", lwd = 1) +
      geom_vline(xintercept = vline_value, col = "gray50") +
      ylim(c(-5, 45)) +
      scale_x_datetime(date_labels = "%d %b", date_breaks = "1 day") +
      labs(
        title = "Previsão de temperatura (°C)",
        subtitle = paste0(names(mun_names[mun_names == input$municipality])),
        caption = paste0(
          "Previsão atmosférica: Copernicus/CAMS\n",
          "Atualização: ",
          format(min(res$date), "%d/%m/%Y %H:%M"),
          "\n",
          "Elaboração: LIS/ICICT/Fiocruz"
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

  # Download temp
  output$download_data_temp <- downloadHandler(
    filename = function() {
      res <- mun_data_temp()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("temp_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_temp(), file)
    }
  )

  # Map UV initial state
  output$map_uv <- renderLeaflet({
    req(input$municipality)

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Palette
    mm <- minmax(rst_uv)
    pal <- colorBin(
      palette = "PuOr",
      bins = c(0, 1, 2, 3, 5, 6, 7, 8, 10, 11, Inf),
      na.color = NA,
      reverse = TRUE
    )

    # Depth (forecast)
    depth <- 24 + 1

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-118, 33, -30, -56) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker") |>
      addRasterImage(
        x = rst_uv[[depth]],
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
        title = paste0("Índice UV")
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

  # Update municipality marker on map UV
  observeEvent(input$municipality, {
    req(input$municipality)

    # Remove old layer
    leafletProxy("map_uv", session) |>
      removeMarker(layerId = "mun_marker")

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Update map
    leafletProxy("map_uv", session) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker")
  })

  # Update raster and date text on map
  observeEvent(input$forecast, {
    # Palette
    mm <- minmax(rst_uv)
    pal <- colorBin(
      palette = "PuOr",
      bins = c(0, 1, 2, 3, 5, 6, 7, 8, 10, 11, Inf),
      na.color = NA,
      reverse = TRUE
    )

    # Remove old layers
    leafletProxy("map_uv", session) |>
      removeImage(layerId = "raster") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- input$forecast + 1

    # Update map
    leafletProxy("map_uv", session) |>
      addRasterImage(
        x = rst_uv[[depth]],
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
        title = paste0("Índice UV")
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

  # Graph UV
  mun_data_uv <- reactive({
    req(input$municipality)

    tbl(con, tb_uv) |>
      mutate(code_muni = substr(as.character(code_muni), 0, 6)) |>
      filter(code_muni == !!input$municipality) |>
      collect() |>
      mutate(date = with_tz(date, "America/Sao_Paulo"))
  })

  output$graph_uv <- renderPlot({
    res <- mun_data_uv()

    vline_value <- unique(res$date)[input$forecast + 1]

    g <- ggplot(data = res, aes(x = date, y = value)) +
      geom_line(col = "red", lwd = 1) +
      geom_vline(xintercept = vline_value, col = "gray50") +
      ylim(c(0, 15)) +
      scale_x_datetime(date_labels = "%d %b", date_breaks = "1 day") +
      labs(
        title = "Índice UV",
        subtitle = paste0(names(mun_names[mun_names == input$municipality])),
        caption = paste0(
          "Previsão atmosférica: Copernicus/CAMS\n",
          "Atualização: ",
          format(min(res$date), "%d/%m/%Y %H:%M"),
          "\n",
          "Elaboração: LIS/ICICT/Fiocruz"
        ),
        x = "Data e hora",
        y = "Valor previsto"
      ) +
      theme_light()

    g
  })

  # Download UV
  output$download_data_uv <- downloadHandler(
    filename = function() {
      res <- mun_data_uv()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("uv_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_uv(), file)
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
      bins = c(0, 20, 40, 60, 80, 100, 200, 400, 600, 800, Inf),
      na.color = NA,
      reverse = FALSE
    )

    # Depth (forecast)
    depth <- (24 + 1 + 2) / 3
    print(depth)

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
        title = paste0("O3 (μg/m³)")
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
      bins = c(0, 20, 40, 60, 80, 100, 200, 400, 600, 800, Inf),
      na.color = NA,
      reverse = FALSE
    )

    # Remove old layers
    leafletProxy("map_o3", session) |>
      removeImage(layerId = "raster") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- (input$forecast + 1 + 2) / 3

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
        title = paste0("O3 (μg/m³)")
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

    vline_value <- unique(res$date)[(input$forecast + 1 + 2) / 3]

    g <- ggplot(data = res, aes(x = date, y = value)) +
      geom_line(col = "red", lwd = 1) +
      geom_vline(xintercept = vline_value, col = "gray50") +
      ylim(c(0, NA)) +
      scale_x_datetime(date_labels = "%d %b", date_breaks = "1 day") +
      labs(
        title = "Previsão de O3 (μg/m³)",
        subtitle = paste0(names(mun_names[mun_names == input$municipality])),
        caption = paste0(
          "Previsão atmosférica: Copernicus/CAMS\n",
          "Atualização: ",
          format(min(res$date), "%d/%m/%Y %H:%M"),
          "\n",
          "Elaboração: LIS/ICICT/Fiocruz"
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

  # Download O3
  output$download_data_o3 <- downloadHandler(
    filename = function() {
      res <- mun_data_o3()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("o3_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_o3(), file)
    }
  )

  # Alerts
  output$rank_pm25_max <- renderDT({
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

  output$rank_pm25_oms <- renderDT({
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

  output$rank_pm25_conama <- renderDT({
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

  output$rank_temp_max <- renderDT({
    tbl(con, tb_temp) |>
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
      rename(
        `Município` = name_muni,
        `Data e hora` = date,
        `Temperatura` = value
      )
  })

  output$rank_temp_min <- renderDT({
    tbl(con, tb_temp) |>
      group_by(code_muni) |>
      filter(value == min(value)) |>
      ungroup() |>
      arrange(value) |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      collect() |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      mutate(date = format(date, "%d/%m/%Y %H:%M")) |>
      rename(
        `Município` = name_muni,
        `Data e hora` = date,
        `Temperatura` = value
      )
  })

  output$rank_temp_35 <- renderDT({
    tbl(con, tb_temp) |>
      mutate(ref = ifelse(value >= 35, TRUE, FALSE)) |>
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

  output$rank_temp_10 <- renderDT({
    tbl(con, tb_temp) |>
      mutate(ref = ifelse(value <= 10, TRUE, FALSE)) |>
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

  output$rank_uv_max <- renderDT({
    tbl(con, tb_uv) |>
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
      rename(
        `Município` = name_muni,
        `Data e hora` = date,
        `Temperatura` = value
      )
  })

  output$rank_uv_3 <- renderDT({
    tbl(con, tb_uv) |>
      mutate(ref = ifelse(value >= 3, TRUE, FALSE)) |>
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

  output$rank_uv_6 <- renderDT({
    tbl(con, tb_uv) |>
      mutate(ref = ifelse(value >= 6, TRUE, FALSE)) |>
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

  output$rank_uv_8 <- renderDT({
    tbl(con, tb_uv) |>
      mutate(ref = ifelse(value >= 8, TRUE, FALSE)) |>
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

  output$rank_uv_11 <- renderDT({
    tbl(con, tb_uv) |>
      mutate(ref = ifelse(value >= 11, TRUE, FALSE)) |>
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
