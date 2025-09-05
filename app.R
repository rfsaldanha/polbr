# Packages
library(shiny)
library(shinyWidgets)
library(fs)
library(bslib)
library(dplyr)
library(lubridate)
library(sf)
library(leaflet)
library(leaflet.extras2)
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
data_dir <- path("../camsdata/forecast_data/")

# Database connection
con <- dbConnect(
  duckdb(),
  path(data_dir, "cams_forecast.duckdb"),
  read_only = TRUE
)

# Table
tb_iqar <- "iqar_mun_forecast"
tb_pm25 <- "pm25_mun_forecast"
tb_pm10 <- "pm10_mun_forecast"
tb_o3 <- "o3_mun_forecast"
tb_co <- "co_mun_forecast"
tb_no2 <- "no2_mun_forecast"
tb_so2 <- "so2_mun_forecast"
tb_temp <- "temp_mun_forecast"
tb_uv <- "uv_mun_forecast"
tb_wind_speed <- "wind_speed_mun_forecast"
tb_aerosol <- "aerosol_mun_forecast"
tb_prec <- "prec_mun_forecast"

# Read forecast rasters
rst_iqar <- rast(path(data_dir, "iqar.nc"))
rst_iqar <- project(x = rst_iqar, "EPSG:3857")

rst_pm25 <- rast(path(data_dir, "cams_forecast_pm25.nc")) * 1e9 # kg/m3 to μg/m3
rst_pm25 <- project(x = rst_pm25, "EPSG:3857")

rst_pm10 <- rast(path(data_dir, "cams_forecast_pm10.nc")) * 1e9 # kg/m3 to μg/m3
rst_pm10 <- project(x = rst_pm10, "EPSG:3857")

rst_o3 <- rast(path(data_dir, "cams_forecast_o3_mc.nc")) * 1e9 # kg/m3 to μg/m3
rst_o3 <- project(x = rst_o3, "EPSG:3857")

rst_co <- rast(path(data_dir, "cams_forecast_co_mc.nc")) # PPM
rst_co <- project(x = rst_co, "EPSG:3857")

rst_no2 <- rast(path(data_dir, "cams_forecast_no2_mc.nc")) * 1e9 # kg/m3 to μg/m3
rst_no2 <- project(x = rst_no2, "EPSG:3857")

rst_so2 <- rast(path(data_dir, "cams_forecast_so2_mc.nc")) * 1e9 # kg/m3 to μg/m3
rst_so2 <- project(x = rst_so2, "EPSG:3857")

rst_temp <- rast(path(data_dir, "cams_forecast_temp.nc")) - 272.15 # K to °C
rst_temp <- project(x = rst_temp, "EPSG:3857")

rst_uv <- rast(path(data_dir, "cams_forecast_uv.nc")) * 40 # Wm2 to UVI
rst_uv <- project(x = rst_uv, "EPSG:3857")

rst_wind_speed <- rast(path(data_dir, "cams_forecast_wind_speed.nc"))
rst_wind_speed <- project(x = rst_wind_speed, "EPSG:3857")

rst_aerosol <- rast(path(data_dir, "cams_forecast_aerosol.nc"))
rst_aerosol <- project(x = rst_aerosol, "EPSG:3857")

rst_prec <- rast(path(data_dir, "cams_forecast_prec.nc")) * 1e3 # m to mm
rst_prec <- project(x = rst_prec, "EPSG:3857")

# Wind files
wind_files <- path(data_dir, paste0("wind_", 1:121, ".json"))

# Wind map options
wind_opts <- velocityOptions(
  speedUnit = "k/h",
  colorScale = colorRampPalette(c("gray50", "black"), alpha = TRUE)(5),
  minVelocity = 0,
  maxVelocity = 10,
  velocityScale = 0.005
)

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

# UFs
ufs <- tibble(
  abbrev = c(
    "AC",
    "AL",
    "AM",
    "AP",
    "BA",
    "CE",
    "DF",
    "ES",
    "GO",
    "MA",
    "MG",
    "MS",
    "MT",
    "PA",
    "PB",
    "PE",
    "PI",
    "PR",
    "RJ",
    "RN",
    "RO",
    "RR",
    "RS",
    "SC",
    "SE",
    "SP",
    "TO"
  ),
  code = c(
    12,
    27,
    13,
    16,
    29,
    23,
    53,
    32,
    52,
    21,
    31,
    50,
    51,
    15,
    25,
    26,
    22,
    41,
    33,
    24,
    11,
    14,
    43,
    42,
    28,
    35,
    17
  )
)

# bdqueimadas data
bdq_focos <- readRDS(file = path(data_dir, "bdq_focos.rds"))

# Maps pallet
pal_iqar <- colorBin(
  palette = c("green", "yellow", "orange", "red", "purple"),
  bins = c(0, 40, 80, 120, 200, Inf),
  na.color = NA,
  reverse = FALSE
)

pal_pm25 <- colorBin(
  palette = "YlOrRd",
  bins = c(15, 25, 37.5, 50, 75, Inf),
  na.color = NA,
  reverse = FALSE
)

pal_pm10 <- colorBin(
  palette = "YlOrBr",
  bins = c(45, 50, 75, 100, 150, Inf),
  na.color = NA,
  reverse = FALSE
)

pal_temp <- colorBin(
  palette = "RdYlBu",
  bins = c(-Inf, 0, 10, 15, 20, 25, 30, 35, 40, Inf),
  na.color = NA,
  reverse = TRUE
)

pal_uv <- colorBin(
  palette = "PuOr",
  bins = c(-Inf, 0, 3, 6, 8, 11, Inf),
  na.color = NA,
  reverse = TRUE
)

pal_o3 <- colorBin(
  palette = "RdPu",
  bins = c(100, 120, 160, Inf),
  na.color = NA,
  reverse = FALSE
)

pal_co <- colorBin(
  palette = "Purples",
  bins = c(0, 9, 11, 13, 15, Inf),
  na.color = NA,
  reverse = FALSE
)

pal_no2 <- colorBin(
  palette = "BuPu",
  bins = c(25, 50, 120, Inf),
  na.color = NA,
  reverse = FALSE
)

pal_so2 <- colorBin(
  palette = "PuBuGn",
  bins = c(0, 40, 50, 125, Inf),
  na.color = NA,
  reverse = FALSE
)

pal_wind_speed <- colorBin(
  palette = "viridis",
  bins = c(0, 5, 10, 30, 50, 80, 100, Inf),
  na.color = NA,
  reverse = FALSE
)

pal_aerosol <- colorBin(
  palette = "magma",
  bins = c(.1, .2, .3, .4, .6, .8, 1, 3, Inf),
  na.color = NA,
  reverse = TRUE
)

pal_prec <- colorBin(
  palette = "BuPu",
  bins = c(0, 10, 20, 40, 60, 80, 100, Inf),
  na.color = NA,
  reverse = FALSE
)

# Interface
ui <- page_navbar(
  tags$head(includeHTML("google-analytics.html")),

  title = "MonitorAr Brasil",
  theme = bs_theme(bootswatch = "shiny"),

  # Logo
  tags$head(
    tags$script(
      HTML(
        '$(document).ready(function() {
             $(".navbar .container-fluid")
               .append("<img id = \'myImage\' src=\'pin_obs_horizontal.png\' align=\'right\' height = \'50px\'>"  );
            });'
      )
    ),
    tags$style(
      HTML(
        '@media (max-width:992px) { #myImage { position: fixed; right: 20%; top: 0.5%; }}'
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
      inputId = "uf",
      label = "UF",
      choices = c("Todas", ufs$abbrev),
      options = list(`live-search` = TRUE)
    ),
    uiOutput(outputId = "municipality_ui"),
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
    ),
    checkboxInput(
      inputId = "conama_line",
      label = "Parâmetros CONAMA",
      value = TRUE
    ),
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
            "Download",
            downloadButton(
              outputId = "download_data_pm25_mun",
              label = "Município selecionado"
            ),
            downloadButton(
              outputId = "download_data_pm25_uf",
              label = "UF selecionada"
            )
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

  # PM10
  nav_panel(
    title = "PM 10",
    page_fillable(
      layout_columns(
        col_widths = c(6, 6),
        # Map card
        card(
          full_screen = TRUE,
          card_body(
            class = "p-0", # Fill card, used for maps
            leafletOutput(outputId = "map_pm10")
          )
        ),

        accordion(
          multiple = FALSE,
          accordion_panel(
            "Gráfico",
            card(
              full_screen = TRUE,
              plotOutput(outputId = "graph_pm10")
            )
          ),
          accordion_panel(
            "Download",
            downloadButton(
              outputId = "download_data_pm10_mun",
              label = "Município selecionado"
            ),
            downloadButton(
              outputId = "download_data_pm10_uf",
              label = "UF selecionada"
            )
          ),
          accordion_panel(
            "Descrição",
            HTML(
              "O material particulado inalável (PM 10) é constituído por partículas com diâmetro aerodinâmico igual ou inferior a 10 micrômetros, capazes de penetrar nas vias respiratórias superiores e médias, como nariz, faringe, laringe e brônquios. Embora não alcancem as regiões mais profundas dos pulmões com a mesma facilidade que o PM 2.5, essas partículas podem provocar irritação das mucosas, inflamação das vias aéreas e agravamento de doenças respiratórias como asma e bronquite. A exposição prolongada ao PM 10 está associada ao aumento de hospitalizações por doenças respiratórias e cardiovasculares, bem como ao incremento da mortalidade, especialmente entre crianças, idosos e indivíduos com condições crônicas. Suas principais fontes incluem poeira de solo, desgaste de pneus e freios, emissões veiculares, processos industriais e queimadas."
            )
          )
        )
      )
    )
  ),

  # Aerosol
  nav_panel(
    title = "Aerosol",
    page_fillable(
      layout_columns(
        col_widths = c(6, 6),
        # Map card
        card(
          full_screen = TRUE,
          card_body(
            class = "p-0", # Fill card, used for maps
            leafletOutput(outputId = "map_aerosol")
          )
        ),

        accordion(
          multiple = FALSE,
          accordion_panel(
            "Gráfico",
            card(
              full_screen = TRUE,
              plotOutput(outputId = "graph_aerosol")
            )
          ),
          accordion_panel(
            "Download",
            downloadButton(
              outputId = "download_data_aerosol_mun",
              label = "Município selecionado"
            ),
            downloadButton(
              outputId = "download_data_aerosol_uf",
              label = "UF selecionada"
            )
          ),
          accordion_panel(
            "Descrição",
            HTML(
              ""
            )
          )
        )
      )
    )
  ),

  # O3
  nav_panel(
    title = "O3",
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
            downloadButton(
              outputId = "download_data_o3_mun",
              label = "Município selecionado"
            ),
            downloadButton(
              outputId = "download_data_o3_uf",
              label = "UF selecionada"
            )
          ),
          accordion_panel(
            "Descrição",
            HTML(
              "O ozônio ao nível do solo (ozônio troposférico) é um poluente secundário formado na baixa atmosfera. Diferente do ozônio estratosférico, que é benéfico para a redução do aquecimento global, o ozônio troposférico é prejudicial à saúde humana, provocando irritação e inflamação das vias respiratórias, reduzindo a função pulmonar e exacerbando doenças como asma e doença pulmonar. O monitoramento do ozônio ao nível do solo é fundamental para a emissão de alertas de qualidade do ar, subsidiando políticas de controle da poluição atmosférica e estratégias preventivas voltadas à proteção da saúde da população exposta, especialmente em áreas urbanas e industrializadas."
            )
          )
        )
      )
    )
  ),

  # CO
  nav_panel(
    title = "CO",
    page_fillable(
      layout_columns(
        col_widths = c(6, 6),
        # Map card
        card(
          full_screen = TRUE,
          card_body(
            class = "p-0", # Fill card, used for maps
            leafletOutput(outputId = "map_co")
          )
        ),

        accordion(
          multiple = FALSE,
          accordion_panel(
            "Gráfico",
            card(
              full_screen = TRUE,
              plotOutput(outputId = "graph_co")
            )
          ),
          accordion_panel(
            "Download",
            downloadButton(
              outputId = "download_data_co_mun",
              label = "Município selecionado"
            ),
            downloadButton(
              outputId = "download_data_co_uf",
              label = "UF selecionada"
            )
          ),
          accordion_panel(
            "Descrição",
            HTML(
              "O monóxido de carbono (CO) é um gás resultante da combustão incompleta de combustíveis fósseis, como gasolina, carvão e madeira. Em ambientes urbanos, suas principais fontes incluem veículos automotores, indústrias e queimadas.  A exposição a concentrações elevadas pode causar sintomas como tontura, náusea, confusão mental e, em casos graves, levar à perda de consciência e morte. Crianças, gestantes, idosos e pessoas com doenças cardiovasculares são particularmente vulneráveis aos seus efeitos. O monitoramento das concentrações de monóxido de carbono é essencial para a avaliação da qualidade do ar, emissão de alertas e formulação de políticas públicas voltadas à redução das emissões."
            )
          )
        )
      )
    )
  ),

  # NO2
  nav_panel(
    title = "NO2",
    page_fillable(
      layout_columns(
        col_widths = c(6, 6),
        # Map card
        card(
          full_screen = TRUE,
          card_body(
            class = "p-0", # Fill card, used for maps
            leafletOutput(outputId = "map_no2")
          )
        ),

        accordion(
          multiple = FALSE,
          accordion_panel(
            "Gráfico",
            card(
              full_screen = TRUE,
              plotOutput(outputId = "graph_no2")
            )
          ),
          accordion_panel(
            "Download",
            downloadButton(
              outputId = "download_data_no2_mun",
              label = "Município selecionado"
            ),
            downloadButton(
              outputId = "download_data_no2_uf",
              label = "UF selecionada"
            )
          ),
          accordion_panel(
            "Descrição",
            HTML(
              "O dióxido de nitrogênio (NO2) é um gás irritante resultante principalmente da combustão de combustíveis fósseis, com destaque para emissões veiculares e processos industriais. Altamente reativo, o NO2 penetra nas vias respiratórias, causando inflamação da mucosa brônquica, redução da função pulmonar e aumento da sensibilidade a infecções respiratórias. A exposição de curto prazo pode desencadear sintomas como tosse, chiado e dificuldade respiratória, enquanto a exposição prolongada está associada ao agravamento de doenças crônicas, como asma e doença pulmonar obstrutiva crônica (DPOC), além de aumentar o risco de hospitalizações e mortalidade por causas respiratórias e cardiovasculares. Crianças, idosos e pessoas com doenças pré-existentes são mais suscetíveis aos seus efeitos nocivos."
            )
          )
        )
      )
    )
  ),

  # SO2
  nav_panel(
    title = "SO2",
    page_fillable(
      layout_columns(
        col_widths = c(6, 6),
        # Map card
        card(
          full_screen = TRUE,
          card_body(
            class = "p-0", # Fill card, used for maps
            leafletOutput(outputId = "map_so2")
          )
        ),

        accordion(
          multiple = FALSE,
          accordion_panel(
            "Gráfico",
            card(
              full_screen = TRUE,
              plotOutput(outputId = "graph_so2")
            )
          ),
          accordion_panel(
            "Download",
            downloadButton(
              outputId = "download_data_so2_mun",
              label = "Município selecionado"
            ),
            downloadButton(
              outputId = "download_data_so2_uf",
              label = "UF selecionada"
            )
          ),
          accordion_panel(
            "Descrição",
            HTML(
              "O dióxido de enxofre (SO2) é um gás incolor produzido principalmente pela queima de combustíveis fósseis com alto teor de enxofre, como carvão e óleo, e por processos industriais, incluindo a fundição de minérios. Altamente solúvel em água, o SO2 reage rapidamente nas vias aéreas superiores, formando ácidos que irritam a mucosa respiratória e provocam broncoconstrição. A exposição aguda pode causar tosse, sensação de aperto no peito e dificuldade respiratória, especialmente em pessoas com asma ou outras doenças respiratórias crônicas. Em concentrações elevadas, pode desencadear crises asmáticas e aumentar hospitalizações por problemas respiratórios e cardiovasculares. A exposição crônica, mesmo a níveis moderados, está associada a inflamação persistente, declínio da função pulmonar e aumento do risco de mortalidade prematura."
            )
          )
        )
      )
    )
  ),

  # IQAr
  nav_panel(
    title = "IQAr",
    page_fillable(
      layout_columns(
        col_widths = c(6, 6),
        # Map card
        card(
          full_screen = TRUE,
          card_body(
            class = "p-0", # Fill card, used for maps
            leafletOutput(outputId = "map_iqar")
          )
        ),

        accordion(
          multiple = FALSE,
          accordion_panel(
            "Gráfico",
            card(
              full_screen = TRUE,
              plotOutput(outputId = "graph_iqar")
            )
          ),
          accordion_panel(
            "Download",
            downloadButton(
              outputId = "download_data_iqar_mun",
              label = "Município selecionado"
            ),
            downloadButton(
              outputId = "download_data_iqar_uf",
              label = "UF selecionada"
            )
          ),
          accordion_panel(
            "Descrição",
            HTML(
              "O Índice de Qualidade do Ar (IQAr), definido pelo Conselho Nacional do Meio Ambiente (CONAMA), é um indicador que sintetiza, em uma escala padronizada, a concentração de poluentes atmosféricos e seus potenciais efeitos na saúde humana e no meio ambiente. Calculado a partir de medições de poluentes-chave, como material particulado (PM₂.₅ e PM₁₀), ozônio (O₃), dióxido de nitrogênio (NO₂), dióxido de enxofre (SO₂) e monóxido de carbono (CO), o IQAr classifica a qualidade do ar em faixas que variam de “boa” a “péssima”."
            )
          )
        )
      )
    )
  ),

  # IUV
  nav_panel(
    title = "IUV",
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
            downloadButton(
              outputId = "download_data_uv_mun",
              label = "Município selecionado"
            ),
            downloadButton(
              outputId = "download_data_uv_uf",
              label = "UF selecionada"
            )
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
            downloadButton(
              outputId = "download_data_temp_mun",
              label = "Município selecionado"
            ),
            downloadButton(
              outputId = "download_data_temp_uf",
              label = "UF selecionada"
            )
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

  # Precipitation
  nav_panel(
    title = "Precipitação",
    page_fillable(
      layout_columns(
        col_widths = c(6, 6),
        # Map card
        card(
          full_screen = TRUE,
          card_body(
            class = "p-0", # Fill card, used for maps
            leafletOutput(outputId = "map_prec")
          )
        ),

        accordion(
          multiple = FALSE,
          accordion_panel(
            "Gráfico",
            card(
              full_screen = TRUE,
              plotOutput(outputId = "graph_prec")
            )
          ),
          accordion_panel(
            "Download",
            downloadButton(
              outputId = "download_data_prec_mun",
              label = "Município selecionado"
            ),
            downloadButton(
              outputId = "download_data_prec_uf",
              label = "UF selecionada"
            )
          ),
          accordion_panel(
            "Descrição",
            HTML(
              ""
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
        "IQAr",
        tabsetPanel(
          tabPanel(
            title = "Valores máximos",
            DTOutput("rank_iqar_max")
          ),
          tabPanel(
            title = "Horas acumuladas no nível moderado ou pior",
            DTOutput(outputId = "rank_iqar_moderado")
          ),
          tabPanel(
            title = "Horas acumuladas no nível ruim ou pior",
            DTOutput("rank_iqar_ruim")
          ),
          tabPanel(
            title = "Horas acumuladas no nível muito ruim ou pior",
            DTOutput("rank_iqar_muito_ruim")
          ),
          tabPanel(
            title = "Horas acumuladas no nível péssimo",
            DTOutput("rank_iqar_pessimo")
          )
        )
      ),
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
      )
    )
  ),

  # Historical data
  nav_panel(
    title = "Dados históricos",
    accordion(
      multiple = FALSE,
      accordion_panel(
        "Dados históricos de poluição por município",
        p(
          "O Copernicus disponibiliza dados históricos de poluentes do ar (CAMS global reanalysis - EAC4) à partir do ano de 2003. Com esses dados, estatísticas zonais foram criadas para os municípios brasileiros. Os links abaixo permitem o download destes dados nos formatos CSV e parquet."
        )
      ),
      accordion_panel(
        "PM 2.5",
        tags$a(
          "2003 - 2024",
          target = "_blank",
          href = "https://zenodo.org/records/16374139"
        )
      ),
      accordion_panel(
        "PM 10",
        tags$a(
          "2003 - 2024",
          target = "_blank",
          href = "https://zenodo.org/records/16419737"
        )
      ),
      accordion_panel(
        "O3",
        tags$a(
          "2003 - 2024",
          target = "_blank",
          href = "https://zenodo.org/records/17025187"
        )
      ),
      accordion_panel(
        "CO",
        tags$a(
          "2003 - 2024",
          target = "_blank",
          href = "https://zenodo.org/records/16984341"
        )
      ),
      accordion_panel(
        "NO2",
        tags$a(
          "2003 - 2024",
          target = "_blank",
          href = "https://zenodo.org/records/17019753"
        )
      ),
      accordion_panel(
        "SO2",
        tags$a(
          "2003 - 2024",
          target = "_blank",
          href = "https://zenodo.org/records/17047073"
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
          "Este é um painel interativo de dados que apresenta estimativas futuras sobre a poluição atmosférica nos municípios brasileiros nas próximas 120 horas. A ferramenta apresenta resultados de modelos preditivos do Copernicus (CAMS) sobre a concentração de poluentes e calcula, à partir destes indicadores, uma projeção do Índice de Qualidade do Ar (IQAr) do CONAMA para os municípios brasileiros.  Além de permitir a visualização georreferenciada e comparativa das estimativas, o painel oferece alertas direcionados aos municípios quando os níveis previstos de poluição ultrapassam limites de segurança para a saúde. O painel tem por objetivo apoiar os gestores públicos na adoção de medidas preventivas e no planejamento de ações de mitigação, além de ampliar a transparência e o acesso da sociedade às informações ambientais, fortalecendo a capacidade de resposta frente a riscos à saúde pública associados à qualidade do ar."
        ),
        p(
          "Desenvolvido pelo Observatório de Clima e Saúde, Laboratório de Informação em Saúde (LIS), Instituto de Comunicação e Informação Científica e Tecnológica em Saúde (ICICT), da Fundação Oswaldo Cruz (Fiocruz)."
        )
      ),
      accordion_panel(
        "Dados e métodos",
        p(
          "Os dados de previsão de poluentes e condições atmosféricas são obtidos diariamente por meio de consulta à API do programa Copernicus Atmosphere Monitoring Service (CAMS), cobrindo o horizonte de 120 horas. Após a coleta, essas informações são convertidas para as unidades de análise apropriadas e utilizadas para o cálculo do Índice de Qualidade do Ar (IQAr), conforme metodologia do CONAMA."
        ),
        p(
          "Os dados de PM 2.5, PM 10, temperatura e IUV são projeções de hora em hora pelo CAMS. Os dados de O3, CO, NO2 e SO2 são previsões de três em três horas pelo CAMS. Como o cálculo do IQAr utiliza dados de gases, sua previsão também é apresentada de três em três horas."
        ),
        p(
          "Os mapas de PM 2.5 e PM 10 apresentam, além do nível de concentração dos poluentes, o focos de calor identificados pelo programa BDQueimadas do INPE, dos últimos três dias."
        ),
        p(
          "Os gráficos apresentados no painel representam a média espacial dos pixels de estimativas que intersectam o território de cada município."
        )
      ),
      accordion_panel(
        "Alertas",
        p(
          "Esta aba do painel apresenta rankings dos municípios para o IQAr, concentração de poluentes, Índice UV e temperatura. Os rankings indicam, para cada município, os valores máximos registrados e o número de horas acima dos valores de referência ao longo de todo o horizonte de previsão (120 horas)."
        )
      ),
      accordion_panel(
        "Código aberto",
        p(
          "Os repositórios de códigos deste projeto são abertos, disponíveis nos links abaixo."
        ),
        p(tags$a(
          "Rotinas de download e processamento de dados",
          target = "_blank",
          href = "https://github.com/rfsaldanha/camsdata"
        )),
        p(tags$a(
          "Aplicação em R Shiny",
          target = "_blank",
          href = "https://github.com/rfsaldanha/polbr"
        )),
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  output$municipality_ui <- renderUI({
    req(input$uf)

    if (input$uf != "Todas") {
      sub <- mun_seats |>
        filter(abbrev_state == input$uf)

      res <- sub$code_muni
      names(res) <- paste(sub$name_muni, "-", sub$abbrev_state)
    } else {
      res <- mun_seats$code_muni
      names(res) <- paste(mun_seats$name_muni, "-", mun_seats$abbrev_state)
    }

    pickerInput(
      inputId = "municipality",
      label = "Município",
      choices = res,
      options = list(`live-search` = TRUE)
    )
  })

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

  # Map IQAr initial state
  output$map_iqar <- renderLeaflet({
    req(input$municipality)
    req(input$forecast)

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Palette
    mm <- minmax(rst_iqar)

    # Depth (forecast)
    depth <- (input$forecast + 1 + 2) / 3

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-71.10, 6.06, -32.20, -34.17) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker") |>
      addRasterImage(
        x = rst_iqar[[depth]],
        opacity = .7,
        colors = pal_iqar,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_iqar,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("IQAr")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster", "vento"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Update municipality marker on map iqar
  observeEvent(input$municipality, {
    req(input$municipality)

    # Remove old layer
    leafletProxy("map_iqar", session) |>
      removeMarker(layerId = "mun_marker")

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Update map
    leafletProxy("map_iqar", session) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker")
  })

  # Update raster and date text on map
  observeEvent(input$forecast, {
    # Palette
    mm <- minmax(rst_iqar)

    # Remove old layers
    leafletProxy("map_iqar", session) |>
      removeImage(layerId = "raster") |>
      removeVelocity(group = "vento") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- (input$forecast + 1 + 2) / 3

    # Update map
    leafletProxy("map_iqar", session) |>
      addRasterImage(
        x = rst_iqar[[depth]],
        opacity = .7,
        colors = pal_iqar,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_iqar,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("IQAr")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster", "vento"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Graph IQAr
  mun_data_iqar <- reactive({
    req(input$municipality)

    tbl(con, tb_iqar) |>
      mutate(code_muni = substr(as.character(code_muni), 0, 6)) |>
      filter(code_muni == !!input$municipality) |>
      collect() |>
      mutate(date = with_tz(date, "America/Sao_Paulo"))
  })

  output$graph_iqar <- renderPlot({
    res <- mun_data_iqar()

    vline_value <- unique(res$date)[(input$forecast + 1 + 2) / 3]

    g <- ggplot(data = res, aes(x = date, y = value)) +
      geom_line(col = "red", lwd = 1) +
      geom_vline(xintercept = vline_value, col = "gray50") +
      ylim(c(0, NA)) +
      scale_x_datetime(date_labels = "%d %b", date_breaks = "1 day") +
      labs(
        title = "Previsão de IQAr",
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

    if (input$conama_line == TRUE) {
      g <- g +
        geom_texthline(
          yintercept = 40,
          label = "N2 - Moderada",
          hjust = 0.1,
          color = "gold4",
          linetype = "dashed"
        ) +
        geom_texthline(
          yintercept = 80,
          label = "N3 - Ruim",
          hjust = 0.1,
          color = "darkorange",
          linetype = "dashed"
        ) +
        geom_texthline(
          yintercept = 120,
          label = "N4 - Muito ruim",
          hjust = 0.1,
          color = "red",
          linetype = "dashed"
        ) +
        geom_texthline(
          yintercept = 200,
          label = "N5 - Péssimo",
          hjust = 0.1,
          color = "purple",
          linetype = "dashed"
        )
    }

    g
  })

  # Download iqar
  output$download_data_iqar_mun <- downloadHandler(
    filename = function() {
      res <- mun_data_iqar()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("iqar_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_iqar() |> rename(`iqar` = value), file)
    }
  )

  output$download_data_iqar_uf <- downloadHandler(
    filename = function() {
      res <- mun_data_iqar()
      res <- format(min(res$date), "%Y%m%d_%H%M")

      paste0("iqar_previsao_", res, "_", input$uf, ".csv")
    },
    content = function(file) {
      res_1 <- tbl(con, tb_iqar) |>
        mutate(
          code_muni = as.numeric(substr(as.character(code_muni), 0, 6)),
          uf = substr(as.character(code_muni), 0, 2)
        )

      if (input$uf == "Todas") {
        res_2 <- res_1 |>
          arrange(code_muni, date) |>
          collect()
      } else {
        uf_code <- ufs[ufs$abbrev == input$uf, ]$code
        res_2 <- res_1 |>
          filter(uf == uf_code) |>
          arrange(code_muni, date) |>
          collect()
      }

      res_3 <- res_2 |>
        left_join(ref_mun_names, by = "code_muni") |>
        mutate(date = with_tz(date, "America/Sao_Paulo")) |>
        select(code_muni, name_muni, date, value) |>
        mutate(
          label = case_when(
            value >= 0 & value <= 40 ~ "N1 - Boa",
            value > 40 & value <= 80 ~ "N2 - Moderada",
            value > 80 & value <= 120 ~ "N3 - Ruim",
            value > 120 & value <= 200 ~ "N4 - Muito ruim",
            value > 200 ~ "N5 - Péssima",
          )
        ) |>
        rename(`iqar` = value)

      write_csv2(res_3, file)
    }
  )

  # Map PM2.5 initial state
  output$map_pm25 <- renderLeaflet({
    req(input$municipality)
    req(input$forecast)

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Palette
    mm <- minmax(rst_pm25)

    # Depth (forecast)
    depth <- input$forecast + 1

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-71.10, 6.06, -32.20, -34.17) |>
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
        colors = pal_pm25,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_pm25,
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
        overlayGroups = c("raster", "INPE/BDQueimadas", "vento"),
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

    # Remove old layers
    leafletProxy("map_pm25", session) |>
      removeImage(layerId = "raster") |>
      removeVelocity(group = "vento") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- input$forecast + 1

    # Update map
    leafletProxy("map_pm25", session) |>
      addRasterImage(
        x = rst_pm25[[depth]],
        opacity = .7,
        colors = pal_pm25,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_pm25,
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
        overlayGroups = c("raster", "INPE/BDQueimadas", "vento"),
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

    if (input$conama_line == TRUE) {
      g <- g +
        geom_texthline(
          yintercept = 15,
          label = "PF - Meta",
          hjust = 0.1,
          color = "gold4",
          linetype = "dashed"
        ) +
        geom_texthline(
          yintercept = 50,
          label = "PI-2 (2025)",
          hjust = 0.1,
          color = "darkorange",
          linetype = "dashed"
        )
    }

    g
  })

  # Download pm25
  output$download_data_pm25_mun <- downloadHandler(
    filename = function() {
      res <- mun_data_pm25()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("pm25_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_pm25() |> rename(`pm2.5` = value), file)
    }
  )

  output$download_data_pm25_uf <- downloadHandler(
    filename = function() {
      res <- mun_data_pm25()
      res <- format(min(res$date), "%Y%m%d_%H%M")

      paste0("pm25_previsao_", res, "_", input$uf, ".csv")
    },
    content = function(file) {
      res_1 <- tbl(con, tb_pm25) |>
        mutate(
          code_muni = as.numeric(substr(as.character(code_muni), 0, 6)),
          uf = substr(as.character(code_muni), 0, 2)
        )

      if (input$uf == "Todas") {
        res_2 <- res_1 |>
          arrange(code_muni, date) |>
          collect()
      } else {
        uf_code <- ufs[ufs$abbrev == input$uf, ]$code
        res_2 <- res_1 |>
          filter(uf == uf_code) |>
          arrange(code_muni, date) |>
          collect()
      }

      res_3 <- res_2 |>
        left_join(ref_mun_names, by = "code_muni") |>
        mutate(date = with_tz(date, "America/Sao_Paulo")) |>
        select(code_muni, name_muni, date, value) |>
        rename(`pm2.5` = value)

      write_csv2(res_3, file)
    }
  )

  # Map PM10 initial state
  output$map_pm10 <- renderLeaflet({
    req(input$municipality)
    req(input$forecast)

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Palette
    mm <- minmax(rst_pm10)

    # Depth (forecast)
    depth <- input$forecast + 1

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-71.10, 6.06, -32.20, -34.17) |>
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
        x = rst_pm10[[depth]],
        opacity = .7,
        colors = pal_pm10,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_pm10,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("PM10 (μg/m³)")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster", "INPE/BDQueimadas", "vento"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Update municipality marker on map pm10
  observeEvent(input$municipality, {
    req(input$municipality)

    # Remove old layer
    leafletProxy("map_pm10", session) |>
      removeMarker(layerId = "mun_marker")

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Update map
    leafletProxy("map_pm10", session) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker")
  })

  # Update raster and date text on map
  observeEvent(input$forecast, {
    # Palette
    mm <- minmax(rst_pm10)

    # Remove old layers
    leafletProxy("map_pm10", session) |>
      removeImage(layerId = "raster") |>
      removeVelocity(group = "vento") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- input$forecast + 1

    # Update map
    leafletProxy("map_pm10", session) |>
      addRasterImage(
        x = rst_pm10[[depth]],
        opacity = .7,
        colors = pal_pm10,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_pm10,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("PM10 (μg/m³)")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster", "INPE/BDQueimadas", "vento"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Graph pm10
  mun_data_pm10 <- reactive({
    req(input$municipality)

    tbl(con, tb_pm10) |>
      mutate(code_muni = substr(as.character(code_muni), 0, 6)) |>
      filter(code_muni == !!input$municipality) |>
      collect() |>
      mutate(date = with_tz(date, "America/Sao_Paulo"))
  })

  output$graph_pm10 <- renderPlot({
    res <- mun_data_pm10()

    vline_value <- unique(res$date)[input$forecast + 1]

    g <- ggplot(data = res, aes(x = date, y = value)) +
      geom_line(col = "red", lwd = 1) +
      geom_vline(xintercept = vline_value, col = "gray50") +
      ylim(c(0, NA)) +
      scale_x_datetime(date_labels = "%d %b", date_breaks = "1 day") +
      labs(
        title = "Previsão de PM10 (μg/m³)",
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

    if (input$conama_line == TRUE) {
      g <- g +
        geom_texthline(
          yintercept = 45,
          label = "PF - Meta",
          hjust = 0.1,
          color = "gold4",
          linetype = "dashed"
        ) +
        geom_texthline(
          yintercept = 100,
          label = "PI-2 (2025)",
          hjust = 0.1,
          color = "darkorange",
          linetype = "dashed"
        )
    }

    g
  })

  # Download pm10
  output$download_data_pm10_mun <- downloadHandler(
    filename = function() {
      res <- mun_data_pm10()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("pm10_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_pm10() |> rename(`pm10` = value), file)
    }
  )

  output$download_data_pm10_uf <- downloadHandler(
    filename = function() {
      res <- mun_data_pm10()
      res <- format(min(res$date), "%Y%m%d_%H%M")

      paste0("pm10_previsao_", res, "_", input$uf, ".csv")
    },
    content = function(file) {
      res_1 <- tbl(con, tb_pm10) |>
        mutate(
          code_muni = as.numeric(substr(as.character(code_muni), 0, 6)),
          uf = substr(as.character(code_muni), 0, 2)
        )

      if (input$uf == "Todas") {
        res_2 <- res_1 |>
          arrange(code_muni, date) |>
          collect()
      } else {
        uf_code <- ufs[ufs$abbrev == input$uf, ]$code
        res_2 <- res_1 |>
          filter(uf == uf_code) |>
          arrange(code_muni, date) |>
          collect()
      }

      res_3 <- res_2 |>
        left_join(ref_mun_names, by = "code_muni") |>
        mutate(date = with_tz(date, "America/Sao_Paulo")) |>
        select(code_muni, name_muni, date, value) |>
        rename(`pm10` = value)

      write_csv2(res_3, file)
    }
  )

  # Map temperature initial state
  output$map_temp <- renderLeaflet({
    req(input$municipality)
    req(input$forecast)

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Palette
    mm <- minmax(rst_temp)

    # Depth (forecast)
    depth <- input$forecast + 1

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-71.10, 6.06, -32.20, -34.17) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker") |>
      addRasterImage(
        x = rst_temp[[depth]],
        opacity = .7,
        colors = pal_temp,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_temp,
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
        overlayGroups = c("raster", "vento"),
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

    # Remove old layers
    leafletProxy("map_temp", session) |>
      removeImage(layerId = "raster") |>
      removeVelocity(group = "vento") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- input$forecast + 1

    # Update map
    leafletProxy("map_temp", session) |>
      addRasterImage(
        x = rst_temp[[depth]],
        opacity = .7,
        colors = pal_temp,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_temp,
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
        overlayGroups = c("raster", "vento"),
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
  output$download_data_temp_mun <- downloadHandler(
    filename = function() {
      res <- mun_data_temp()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("temp_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_temp() |> rename(`temp` = value), file)
    }
  )

  output$download_data_temp_uf <- downloadHandler(
    filename = function() {
      res <- mun_data_temp()
      res <- format(min(res$date), "%Y%m%d_%H%M")

      paste0("temp_previsao_", res, "_", input$uf, ".csv")
    },
    content = function(file) {
      res_1 <- tbl(con, tb_temp) |>
        mutate(
          code_muni = as.numeric(substr(as.character(code_muni), 0, 6)),
          uf = substr(as.character(code_muni), 0, 2)
        )

      if (input$uf == "Todas") {
        res_2 <- res_1 |>
          arrange(code_muni, date) |>
          collect()
      } else {
        uf_code <- ufs[ufs$abbrev == input$uf, ]$code
        res_2 <- res_1 |>
          filter(uf == uf_code) |>
          arrange(code_muni, date) |>
          collect()
      }

      res_3 <- res_2 |>
        left_join(ref_mun_names, by = "code_muni") |>
        mutate(date = with_tz(date, "America/Sao_Paulo")) |>
        select(code_muni, name_muni, date, value) |>
        rename(`temp` = value)

      write_csv2(res_3, file)
    }
  )

  # Map UV initial state
  output$map_uv <- renderLeaflet({
    req(input$municipality)
    req(input$forecast)

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Palette
    mm <- minmax(rst_uv)

    # Depth (forecast)
    depth <- input$forecast + 1

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-71.10, 6.06, -32.20, -34.17) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker") |>
      addRasterImage(
        x = rst_uv[[depth]],
        opacity = .7,
        colors = pal_uv,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_uv,
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
        overlayGroups = c("raster", "vento"),
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

    # Remove old layers
    leafletProxy("map_uv", session) |>
      removeImage(layerId = "raster") |>
      removeVelocity(group = "vento") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- input$forecast + 1

    # Update map
    leafletProxy("map_uv", session) |>
      addRasterImage(
        x = rst_uv[[depth]],
        opacity = .7,
        colors = pal_uv,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_uv,
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
        overlayGroups = c("raster", "vento"),
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

    if (input$conama_line == TRUE) {
      g <- g +
        geom_texthline(
          yintercept = 3,
          label = "Moderado",
          hjust = 0.1,
          color = "gold4",
          linetype = "dashed"
        ) +
        geom_texthline(
          yintercept = 6,
          label = "Alto",
          hjust = 0.1,
          color = "darkorange",
          linetype = "dashed"
        ) +
        geom_texthline(
          yintercept = 8,
          label = "Muito alto",
          hjust = 0.1,
          color = "red",
          linetype = "dashed"
        ) +
        geom_texthline(
          yintercept = 11,
          label = "Extremo",
          hjust = 0.1,
          color = "purple",
          linetype = "dashed"
        )
    }

    g
  })

  # Download UV
  output$download_data_uv_mun <- downloadHandler(
    filename = function() {
      res <- mun_data_uv()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("uv_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_uv() |> rename(`iuv` = value), file)
    }
  )

  output$download_data_uv_uf <- downloadHandler(
    filename = function() {
      res <- mun_data_uv()
      res <- format(min(res$date), "%Y%m%d_%H%M")

      paste0("uv_previsao_", res, "_", input$uf, ".csv")
    },
    content = function(file) {
      res_1 <- tbl(con, tb_uv) |>
        mutate(
          code_muni = as.numeric(substr(as.character(code_muni), 0, 6)),
          uf = substr(as.character(code_muni), 0, 2)
        )

      if (input$uf == "Todas") {
        res_2 <- res_1 |>
          arrange(code_muni, date) |>
          collect()
      } else {
        uf_code <- ufs[ufs$abbrev == input$uf, ]$code
        res_2 <- res_1 |>
          filter(uf == uf_code) |>
          arrange(code_muni, date) |>
          collect()
      }

      res_3 <- res_2 |>
        left_join(ref_mun_names, by = "code_muni") |>
        mutate(date = with_tz(date, "America/Sao_Paulo")) |>
        select(code_muni, name_muni, date, value) |>
        rename(`uv` = value)

      write_csv2(res_3, file)
    }
  )

  # Map O3 initial state
  output$map_o3 <- renderLeaflet({
    req(input$municipality)
    req(input$forecast)

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Palette
    mm <- minmax(rst_o3)

    # Depth (forecast)
    depth <- (input$forecast + 1 + 2) / 3

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-71.10, 6.06, -32.20, -34.17) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker") |>
      addRasterImage(
        x = rst_o3[[depth]],
        opacity = .7,
        colors = pal_o3,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_o3,
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
        overlayGroups = c("raster", "vento"),
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

    # Remove old layers
    leafletProxy("map_o3", session) |>
      removeImage(layerId = "raster") |>
      removeVelocity(group = "vento") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- (input$forecast + 1 + 2) / 3

    # Update map
    leafletProxy("map_o3", session) |>
      addRasterImage(
        x = rst_o3[[depth]],
        opacity = .7,
        colors = pal_o3,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_o3,
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
        overlayGroups = c("raster", "vento"),
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

    if (input$conama_line == TRUE) {
      g <- g +
        geom_texthline(
          yintercept = 100,
          label = "PF - Meta",
          hjust = 0.1,
          color = "gold4",
          linetype = "dashed"
        ) +
        geom_texthline(
          yintercept = 130,
          label = "PI-2 (2025)",
          hjust = 0.1,
          color = "darkorange",
          linetype = "dashed"
        )
    }

    g
  })

  # Download O3
  output$download_data_o3_mun <- downloadHandler(
    filename = function() {
      res <- mun_data_o3()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("o3_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_o3() |> rename(`o3` = value), file)
    }
  )

  output$download_data_o3_uf <- downloadHandler(
    filename = function() {
      res <- mun_data_o3()
      res <- format(min(res$date), "%Y%m%d_%H%M")

      paste0("o3_previsao_", res, "_", input$uf, ".csv")
    },
    content = function(file) {
      res_1 <- tbl(con, tb_o3) |>
        mutate(
          code_muni = as.numeric(substr(as.character(code_muni), 0, 6)),
          uf = substr(as.character(code_muni), 0, 2)
        )

      if (input$uf == "Todas") {
        res_2 <- res_1 |>
          arrange(code_muni, date) |>
          collect()
      } else {
        uf_code <- ufs[ufs$abbrev == input$uf, ]$code
        res_2 <- res_1 |>
          filter(uf == uf_code) |>
          arrange(code_muni, date) |>
          collect()
      }

      res_3 <- res_2 |>
        left_join(ref_mun_names, by = "code_muni") |>
        mutate(date = with_tz(date, "America/Sao_Paulo")) |>
        select(code_muni, name_muni, date, value) |>
        rename(`o3` = value)

      write_csv2(res_3, file)
    }
  )

  # Map CO initial state
  output$map_co <- renderLeaflet({
    req(input$municipality)
    req(input$forecast)

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Palette
    mm <- minmax(rst_co)

    # Depth (forecast)
    depth <- (input$forecast + 1 + 2) / 3

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-71.10, 6.06, -32.20, -34.17) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker") |>
      addRasterImage(
        x = rst_co[[depth]],
        opacity = .7,
        colors = pal_co,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_co,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("CO (PPM)")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster", "vento"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Update municipality marker on map co
  observeEvent(input$municipality, {
    req(input$municipality)

    # Remove old layer
    leafletProxy("map_co", session) |>
      removeMarker(layerId = "mun_marker")

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Update map
    leafletProxy("map_co", session) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker")
  })

  # Update raster and date text on map
  observeEvent(input$forecast, {
    # Palette
    mm <- minmax(rst_co)

    # Remove old layers
    leafletProxy("map_co", session) |>
      removeImage(layerId = "raster") |>
      removeVelocity(group = "vento") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- (input$forecast + 1 + 2) / 3

    # Update map
    leafletProxy("map_co", session) |>
      addRasterImage(
        x = rst_co[[depth]],
        opacity = .7,
        colors = pal_co,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_co,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("CO (PPM)")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster", "vento"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Graph co
  mun_data_co <- reactive({
    req(input$municipality)

    tbl(con, tb_co) |>
      mutate(code_muni = substr(as.character(code_muni), 0, 6)) |>
      filter(code_muni == !!input$municipality) |>
      collect() |>
      mutate(date = with_tz(date, "America/Sao_Paulo"))
  })

  output$graph_co <- renderPlot({
    res <- mun_data_co()

    vline_value <- unique(res$date)[(input$forecast + 1 + 2) / 3]

    g <- ggplot(data = res, aes(x = date, y = value)) +
      geom_line(col = "red", lwd = 1) +
      geom_vline(xintercept = vline_value, col = "gray50") +
      ylim(c(0, NA)) +
      scale_x_datetime(date_labels = "%d %b", date_breaks = "1 day") +
      labs(
        title = "Previsão de CO (PPM)",
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

    if (input$conama_line == TRUE) {
      g <- g +
        geom_texthline(
          yintercept = 9,
          label = "PF - Meta",
          hjust = 0.1,
          color = "gold4",
          linetype = "dashed"
        )
    }

    g
  })

  # Download CO
  output$download_data_co_mun <- downloadHandler(
    filename = function() {
      res <- mun_data_co()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("co_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_co() |> rename(`co` = value), file)
    }
  )

  output$download_data_co_uf <- downloadHandler(
    filename = function() {
      res <- mun_data_co()
      res <- format(min(res$date), "%Y%m%d_%H%M")

      paste0("co_previsao_", res, "_", input$uf, ".csv")
    },
    content = function(file) {
      res_1 <- tbl(con, tb_co) |>
        mutate(
          code_muni = as.numeric(substr(as.character(code_muni), 0, 6)),
          uf = substr(as.character(code_muni), 0, 2)
        )

      if (input$uf == "Todas") {
        res_2 <- res_1 |>
          arrange(code_muni, date) |>
          collect()
      } else {
        uf_code <- ufs[ufs$abbrev == input$uf, ]$code
        res_2 <- res_1 |>
          filter(uf == uf_code) |>
          arrange(code_muni, date) |>
          collect()
      }

      res_3 <- res_2 |>
        left_join(ref_mun_names, by = "code_muni") |>
        mutate(date = with_tz(date, "America/Sao_Paulo")) |>
        select(code_muni, name_muni, date, value) |>
        rename(`co` = value)

      write_csv2(res_3, file)
    }
  )

  # Map NO2 initial state
  output$map_no2 <- renderLeaflet({
    req(input$municipality)
    req(input$forecast)

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Palette
    mm <- minmax(rst_no2)

    # Depth (forecast)
    depth <- (input$forecast + 1 + 2) / 3

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-71.10, 6.06, -32.20, -34.17) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker") |>
      addRasterImage(
        x = rst_no2[[depth]],
        opacity = .7,
        colors = pal_no2,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_no2,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("NO2 (μg/m³)")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster", "vento"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Update municipality marker on map no2
  observeEvent(input$municipality, {
    req(input$municipality)

    # Remove old layer
    leafletProxy("map_no2", session) |>
      removeMarker(layerId = "mun_marker")

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Update map
    leafletProxy("map_no2", session) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker")
  })

  # Update raster and date text on map
  observeEvent(input$forecast, {
    # Palette
    mm <- minmax(rst_no2)

    # Remove old layers
    leafletProxy("map_no2", session) |>
      removeImage(layerId = "raster") |>
      removeVelocity(group = "vento") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- (input$forecast + 1 + 2) / 3

    # Update map
    leafletProxy("map_no2", session) |>
      addRasterImage(
        x = rst_no2[[depth]],
        opacity = .7,
        colors = pal_no2,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_no2,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("NO2 (μg/m³)")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster", "vento"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Graph no2
  mun_data_no2 <- reactive({
    req(input$municipality)

    tbl(con, tb_no2) |>
      mutate(code_muni = substr(as.character(code_muni), 0, 6)) |>
      filter(code_muni == !!input$municipality) |>
      collect() |>
      mutate(date = with_tz(date, "America/Sao_Paulo"))
  })

  output$graph_no2 <- renderPlot({
    res <- mun_data_no2()

    vline_value <- unique(res$date)[(input$forecast + 1 + 2) / 3]

    g <- ggplot(data = res, aes(x = date, y = value)) +
      geom_line(col = "red", lwd = 1) +
      geom_vline(xintercept = vline_value, col = "gray50") +
      ylim(c(0, NA)) +
      scale_x_datetime(date_labels = "%d %b", date_breaks = "1 day") +
      labs(
        title = "Previsão de NO2 (μg/m³)",
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

    if (input$conama_line == TRUE) {
      g <- g +
        geom_texthline(
          yintercept = 200,
          label = "PF - Meta",
          hjust = 0.1,
          color = "gold4",
          linetype = "dashed"
        ) +
        geom_texthline(
          yintercept = 240,
          label = "PI-2 (2025)",
          hjust = 0.1,
          color = "darkorange",
          linetype = "dashed"
        )
    }

    g
  })

  # Download NO2
  output$download_data_no2_mun <- downloadHandler(
    filename = function() {
      res <- mun_data_no2()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("no2_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_no2() |> rename(`no2` = value), file)
    }
  )

  output$download_data_no2_uf <- downloadHandler(
    filename = function() {
      res <- mun_data_no2()
      res <- format(min(res$date), "%Y%m%d_%H%M")

      paste0("no2_previsao_", res, "_", input$uf, ".csv")
    },
    content = function(file) {
      res_1 <- tbl(con, tb_no2) |>
        mutate(
          code_muni = as.numeric(substr(as.character(code_muni), 0, 6)),
          uf = substr(as.character(code_muni), 0, 2)
        )

      if (input$uf == "Todas") {
        res_2 <- res_1 |>
          arrange(code_muni, date) |>
          collect()
      } else {
        uf_code <- ufs[ufs$abbrev == input$uf, ]$code
        res_2 <- res_1 |>
          filter(uf == uf_code) |>
          arrange(code_muni, date) |>
          collect()
      }

      res_3 <- res_2 |>
        left_join(ref_mun_names, by = "code_muni") |>
        mutate(date = with_tz(date, "America/Sao_Paulo")) |>
        select(code_muni, name_muni, date, value) |>
        rename(`no2` = value)

      write_csv2(res_3, file)
    }
  )

  # Map SO2 initial state
  output$map_so2 <- renderLeaflet({
    req(input$municipality)
    req(input$forecast)

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Palette
    mm <- minmax(rst_so2)

    # Depth (forecast)
    depth <- (input$forecast + 1 + 2) / 3

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-71.10, 6.06, -32.20, -34.17) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker") |>
      addRasterImage(
        x = rst_so2[[depth]],
        opacity = .7,
        colors = pal_so2,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_so2,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("SO2 (μg/m³)")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster", "vento"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Update municipality marker on map no2
  observeEvent(input$municipality, {
    req(input$municipality)

    # Remove old layer
    leafletProxy("map_so2", session) |>
      removeMarker(layerId = "mun_marker")

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Update map
    leafletProxy("map_so2", session) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker")
  })

  # Update raster and date text on map
  observeEvent(input$forecast, {
    # Palette
    mm <- minmax(rst_so2)

    # Remove old layers
    leafletProxy("map_so2", session) |>
      removeImage(layerId = "raster") |>
      removeVelocity(group = "vento") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- (input$forecast + 1 + 2) / 3

    # Update map
    leafletProxy("map_so2", session) |>
      addRasterImage(
        x = rst_so2[[depth]],
        opacity = .7,
        colors = pal_so2,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_so2,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("SO2 (μg/m³)")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster", "vento"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Graph so2
  mun_data_so2 <- reactive({
    req(input$municipality)

    tbl(con, tb_so2) |>
      mutate(code_muni = substr(as.character(code_muni), 0, 6)) |>
      filter(code_muni == !!input$municipality) |>
      collect() |>
      mutate(date = with_tz(date, "America/Sao_Paulo"))
  })

  output$graph_so2 <- renderPlot({
    res <- mun_data_so2()

    vline_value <- unique(res$date)[(input$forecast + 1 + 2) / 3]

    g <- ggplot(data = res, aes(x = date, y = value)) +
      geom_line(col = "red", lwd = 1) +
      geom_vline(xintercept = vline_value, col = "gray50") +
      ylim(c(0, NA)) +
      scale_x_datetime(date_labels = "%d %b", date_breaks = "1 day") +
      labs(
        title = "Previsão de SO2 (μg/m³)",
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

    if (input$conama_line == TRUE) {
      g <- g +
        geom_texthline(
          yintercept = 40,
          label = "PF - Meta",
          hjust = 0.1,
          color = "gold4",
          linetype = "dashed"
        ) +
        geom_texthline(
          yintercept = 50,
          label = "PI-2 (2025)",
          hjust = 0.1,
          color = "darkorange",
          linetype = "dashed"
        )
    }

    g
  })

  # Download SO2
  output$download_data_so2_mun <- downloadHandler(
    filename = function() {
      res <- mun_data_so2()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("so2_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_so2() |> rename(`so2` = value), file)
    }
  )

  output$download_data_so2_uf <- downloadHandler(
    filename = function() {
      res <- mun_data_so2()
      res <- format(min(res$date), "%Y%m%d_%H%M")

      paste0("so2_previsao_", res, "_", input$uf, ".csv")
    },
    content = function(file) {
      res_1 <- tbl(con, tb_so2) |>
        mutate(
          code_muni = as.numeric(substr(as.character(code_muni), 0, 6)),
          uf = substr(as.character(code_muni), 0, 2)
        )

      if (input$uf == "Todas") {
        res_2 <- res_1 |>
          arrange(code_muni, date) |>
          collect()
      } else {
        uf_code <- ufs[ufs$abbrev == input$uf, ]$code
        res_2 <- res_1 |>
          filter(uf == uf_code) |>
          arrange(code_muni, date) |>
          collect()
      }

      res_3 <- res_2 |>
        left_join(ref_mun_names, by = "code_muni") |>
        mutate(date = with_tz(date, "America/Sao_Paulo")) |>
        select(code_muni, name_muni, date, value) |>
        rename(`so2` = value)

      write_csv2(res_3, file)
    }
  )

  # Map aerosol initial state
  output$map_aerosol <- renderLeaflet({
    req(input$municipality)
    req(input$forecast)

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Palette
    mm <- minmax(rst_aerosol)

    # Depth (forecast)
    depth <- input$forecast + 1

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-71.10, 6.06, -32.20, -34.17) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker") |>
      addRasterImage(
        x = rst_aerosol[[depth]],
        opacity = .7,
        colors = pal_aerosol,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_aerosol,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("Aerosol (org.) 550nm")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster", "vento"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Update municipality marker on map aerosol
  observeEvent(input$municipality, {
    req(input$municipality)

    # Remove old layer
    leafletProxy("map_aerosol", session) |>
      removeMarker(layerId = "mun_marker")

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Update map
    leafletProxy("map_aerosol", session) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker")
  })

  # Update raster and date text on map
  observeEvent(input$forecast, {
    # Palette
    mm <- minmax(rst_aerosol)

    # Remove old layers
    leafletProxy("map_aerosol", session) |>
      removeImage(layerId = "raster") |>
      removeVelocity(group = "vento") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- input$forecast + 1

    # Update map
    leafletProxy("map_aerosol", session) |>
      addRasterImage(
        x = rst_aerosol[[depth]],
        opacity = .7,
        colors = pal_aerosol,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_aerosol,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("Aerosol (org.) 550nm")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster", "vento"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Graph aerosol
  mun_data_aerosol <- reactive({
    req(input$municipality)

    tbl(con, tb_aerosol) |>
      mutate(code_muni = substr(as.character(code_muni), 0, 6)) |>
      filter(code_muni == !!input$municipality) |>
      collect() |>
      mutate(date = with_tz(date, "America/Sao_Paulo"))
  })

  output$graph_aerosol <- renderPlot({
    res <- mun_data_aerosol()

    vline_value <- unique(res$date)[input$forecast + 1]

    g <- ggplot(data = res, aes(x = date, y = value)) +
      geom_line(col = "red", lwd = 1) +
      geom_vline(xintercept = vline_value, col = "gray50") +
      ylim(c(0, NA)) +
      scale_x_datetime(date_labels = "%d %b", date_breaks = "1 day") +
      labs(
        title = "Aerosol (org.) 550nm",
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

    if (input$conama_line == TRUE) {
      g <- g +
        geom_texthline(
          yintercept = 45,
          label = "PF - Meta",
          hjust = 0.1,
          color = "gold4",
          linetype = "dashed"
        ) +
        geom_texthline(
          yintercept = 100,
          label = "PI-2 (2025)",
          hjust = 0.1,
          color = "darkorange",
          linetype = "dashed"
        )
    }

    g
  })

  # Download aerosol
  output$download_data_aerosol_mun <- downloadHandler(
    filename = function() {
      res <- mun_data_aerosol()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("aerosol_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_aerosol() |> rename(`aerosol` = value), file)
    }
  )

  output$download_data_aerosol_uf <- downloadHandler(
    filename = function() {
      res <- mun_data_aerosol()
      res <- format(min(res$date), "%Y%m%d_%H%M")

      paste0("aerosol_vento_previsao_", res, "_", input$uf, ".csv")
    },
    content = function(file) {
      res_1 <- tbl(con, tb_aerosol) |>
        mutate(
          code_muni = as.numeric(substr(as.character(code_muni), 0, 6)),
          uf = substr(as.character(code_muni), 0, 2)
        )

      if (input$uf == "Todas") {
        res_2 <- res_1 |>
          arrange(code_muni, date) |>
          collect()
      } else {
        uf_code <- ufs[ufs$abbrev == input$uf, ]$code
        res_2 <- res_1 |>
          filter(uf == uf_code) |>
          arrange(code_muni, date) |>
          collect()
      }

      res_3 <- res_2 |>
        left_join(ref_mun_names, by = "code_muni") |>
        mutate(date = with_tz(date, "America/Sao_Paulo")) |>
        select(code_muni, name_muni, date, value) |>
        rename(`wind_speed` = value)

      write_csv2(res_3, file)
    }
  )

  # Map precipitation initial state
  output$map_prec <- renderLeaflet({
    req(input$municipality)
    req(input$forecast)

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Palette
    mm <- minmax(rst_prec)

    # Depth (forecast)
    depth <- input$forecast + 1

    leaflet() |>
      addTiles(group = "Open Street Maps") |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagem de satélite"
      ) |>
      fitBounds(-71.10, 6.06, -32.20, -34.17) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker") |>
      addRasterImage(
        x = rst_prec[[depth]],
        opacity = .7,
        colors = pal_prec,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_prec,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("Precipitação acum. (mm)")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster", "vento"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Update municipality marker on map precipitation
  observeEvent(input$municipality, {
    req(input$municipality)

    # Remove old layer
    leafletProxy("map_prec", session) |>
      removeMarker(layerId = "mun_marker")

    # Municipality coordinates
    coord <- mun_seats |>
      filter(code_muni == input$municipality) |>
      st_coordinates() |>
      as.vector()

    # Update map
    leafletProxy("map_prec", session) |>
      addMarkers(lng = coord[1], lat = coord[2], layerId = "mun_marker")
  })

  # Update raster and date text on map
  observeEvent(input$forecast, {
    # Palette
    mm <- minmax(rst_prec)

    # Remove old layers
    leafletProxy("map_prec", session) |>
      removeImage(layerId = "raster") |>
      removeVelocity(group = "vento") |>
      removeControl(layerId = "legend") |>
      removeControl(layerId = "title")

    # Depth (forecast)
    depth <- input$forecast + 1

    # Update map
    leafletProxy("map_prec", session) |>
      addRasterImage(
        x = rst_prec[[depth]],
        opacity = .7,
        colors = pal_prec,
        layerId = "raster",
        project = FALSE,
        group = "raster"
      ) |>
      addVelocity(
        content = wind_files[depth],
        group = "vento",
        layerId = "vento",
        options = wind_opts
      ) |>
      addLegend(
        pal = pal_prec,
        values = c(min(t(mm)[, 1]), max(t(mm)[, 2])),
        layerId = "legend",
        title = paste0("Precipitação (mm)")
      ) |>
      # Layers control
      addLayersControl(
        baseGroups = c(
          "Open Street Maps",
          "Imagem de satélite"
        ),
        overlayGroups = c("raster", "vento"),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "bottomleft"
        )
      )
  })

  # Graph precipitation
  mun_data_prec <- reactive({
    req(input$municipality)

    tbl(con, tb_prec) |>
      mutate(code_muni = substr(as.character(code_muni), 0, 6)) |>
      filter(code_muni == !!input$municipality) |>
      collect() |>
      mutate(date = with_tz(date, "America/Sao_Paulo"))
  })

  output$graph_prec <- renderPlot({
    res <- mun_data_prec()

    vline_value <- unique(res$date)[input$forecast + 1]

    g <- ggplot(data = res, aes(x = date, y = value)) +
      geom_line(col = "red", lwd = 1) +
      geom_vline(xintercept = vline_value, col = "gray50") +
      ylim(c(0, NA)) +
      scale_x_datetime(date_labels = "%d %b", date_breaks = "1 day") +
      labs(
        title = "Precipitação acumulada (mm)",
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

    # if (input$conama_line == TRUE) {
    #   g <- g +
    #     geom_texthline(
    #       yintercept = 40,
    #       label = "N2 - Moderada",
    #       hjust = 0.1,
    #       color = "gold4",
    #       linetype = "dashed"
    #     ) +
    #     geom_texthline(
    #       yintercept = 50,
    #       label = "N3 - Ruim",
    #       hjust = 0.1,
    #       color = "darkorange",
    #       linetype = "dashed"
    #     ) +
    #     geom_texthline(
    #       yintercept = 125,
    #       label = "N4 - Muito ruim",
    #       hjust = 0.1,
    #       color = "red",
    #       linetype = "dashed"
    #     ) +
    #     geom_texthline(
    #       yintercept = 800,
    #       label = "N5 - Péssimo",
    #       hjust = 0.1,
    #       color = "purple",
    #       linetype = "dashed"
    #     )
    # }

    g
  })

  # Download precipitation
  output$download_data_prec_mun <- downloadHandler(
    filename = function() {
      res <- mun_data_prec()
      res <- format(min(res$date), "%Y%m%d_%H%M")
      paste0("precipitacao_previsao_", res, "_", input$municipality, ".csv")
    },
    content = function(file) {
      write_csv2(mun_data_prec() |> rename(`precipitation` = value), file)
    }
  )

  output$download_data_prec_uf <- downloadHandler(
    filename = function() {
      res <- mun_data_prec()
      res <- format(min(res$date), "%Y%m%d_%H%M")

      paste0("precipitacao_vento_previsao_", res, "_", input$uf, ".csv")
    },
    content = function(file) {
      res_1 <- tbl(con, tb_prec) |>
        mutate(
          code_muni = as.numeric(substr(as.character(code_muni), 0, 6)),
          uf = substr(as.character(code_muni), 0, 2)
        )

      if (input$uf == "Todas") {
        res_2 <- res_1 |>
          arrange(code_muni, date) |>
          collect()
      } else {
        uf_code <- ufs[ufs$abbrev == input$uf, ]$code
        res_2 <- res_1 |>
          filter(uf == uf_code) |>
          arrange(code_muni, date) |>
          collect()
      }

      res_3 <- res_2 |>
        left_join(ref_mun_names, by = "code_muni") |>
        mutate(date = with_tz(date, "America/Sao_Paulo")) |>
        select(code_muni, name_muni, date, value) |>
        rename(`wind_speed` = value)

      write_csv2(res_3, file)
    }
  )

  # Alerts
  output$rank_iqar_max <- renderDT({
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_iqar) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_iqar)
    }

    res |>
      group_by(code_muni) |>
      filter(value == max(value)) |>
      ungroup() |>
      arrange(-value) |>
      collect() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      mutate(date = format(date, "%d/%m/%Y %H:%M")) |>
      mutate(
        `Classe` = case_when(
          value >= 0 & value <= 40 ~ "N1 - Boa",
          value > 40 & value <= 80 ~ "N2 - Moderada",
          value > 80 & value <= 120 ~ "N3 - Ruim",
          value > 120 & value <= 200 ~ "N4 - Muito ruim",
          value > 200 ~ "N5 - Péssima",
        )
      ) |>
      rename(`Município` = name_muni, `Data e hora` = date, `IQAr` = value)
  })

  output$rank_iqar_moderado <- renderDT({
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_iqar) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_iqar)
    }

    res |>
      mutate(ref = ifelse(value > 40, TRUE, FALSE)) |>
      filter(ref == TRUE) |>
      group_by(code_muni) |>
      summarise(freq = n()) |>
      ungroup() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      arrange(-freq) |>
      collect() |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      rename(`Município` = name_muni, `Horas` = freq)
  })

  output$rank_iqar_ruim <- renderDT({
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_iqar) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_iqar)
    }

    res |>
      mutate(ref = ifelse(value > 80, TRUE, FALSE)) |>
      filter(ref == TRUE) |>
      group_by(code_muni) |>
      summarise(freq = n()) |>
      ungroup() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      arrange(-freq) |>
      collect() |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      rename(`Município` = name_muni, `Horas` = freq)
  })

  output$rank_iqar_muito_ruim <- renderDT({
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_iqar) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_iqar)
    }

    res |>
      mutate(ref = ifelse(value > 120, TRUE, FALSE)) |>
      filter(ref == TRUE) |>
      group_by(code_muni) |>
      summarise(freq = n()) |>
      ungroup() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      arrange(-freq) |>
      collect() |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      rename(`Município` = name_muni, `Horas` = freq)
  })

  output$rank_iqar_pessimo <- renderDT({
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_iqar) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_iqar)
    }

    res |>
      mutate(ref = ifelse(value > 200, TRUE, FALSE)) |>
      filter(ref == TRUE) |>
      group_by(code_muni) |>
      summarise(freq = n()) |>
      ungroup() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      arrange(-freq) |>
      collect() |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      rename(`Município` = name_muni, `Horas` = freq)
  })

  output$rank_pm25_max <- renderDT({
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_pm25) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_pm25)
    }

    res |>
      group_by(code_muni) |>
      filter(value == max(value)) |>
      ungroup() |>
      arrange(-value) |>
      collect() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      mutate(date = format(date, "%d/%m/%Y %H:%M")) |>
      rename(`Município` = name_muni, `Data e hora` = date, `PM2.5` = value)
  })

  output$rank_pm25_oms <- renderDT({
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_pm25) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_pm25)
    }

    res |>
      mutate(ref = ifelse(value > 15, TRUE, FALSE)) |>
      filter(ref == TRUE) |>
      group_by(code_muni) |>
      summarise(freq = n()) |>
      ungroup() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      arrange(-freq) |>
      collect() |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      rename(`Município` = name_muni, `Horas` = freq)
  })

  output$rank_pm25_conama <- renderDT({
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_pm25) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_pm25)
    }

    res |>
      mutate(ref = ifelse(value > 50, TRUE, FALSE)) |>
      filter(ref == TRUE) |>
      group_by(code_muni) |>
      summarise(freq = n()) |>
      ungroup() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      arrange(-freq) |>
      collect() |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      rename(`Município` = name_muni, `Horas` = freq)
  })

  output$rank_temp_max <- renderDT({
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_temp) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_temp)
    }

    res |>
      group_by(code_muni) |>
      filter(value == max(value)) |>
      ungroup() |>
      arrange(-value) |>
      collect() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
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
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_temp) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_temp)
    }

    res |>
      group_by(code_muni) |>
      filter(value == min(value)) |>
      ungroup() |>
      arrange(value) |>
      collect() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
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
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_temp) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_temp)
    }

    res |>
      mutate(ref = ifelse(value >= 35, TRUE, FALSE)) |>
      filter(ref == TRUE) |>
      group_by(code_muni) |>
      summarise(freq = n()) |>
      ungroup() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      arrange(-freq) |>
      collect() |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      rename(`Município` = name_muni, `Horas` = freq)
  })

  output$rank_temp_10 <- renderDT({
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_temp) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_temp)
    }

    res |>
      mutate(ref = ifelse(value <= 10, TRUE, FALSE)) |>
      filter(ref == TRUE) |>
      group_by(code_muni) |>
      summarise(freq = n()) |>
      ungroup() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      arrange(-freq) |>
      collect() |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      rename(`Município` = name_muni, `Horas` = freq)
  })

  output$rank_uv_max <- renderDT({
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_uv) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_uv)
    }

    res |>
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
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_uv) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_uv)
    }

    res |>
      mutate(ref = ifelse(value >= 3, TRUE, FALSE)) |>
      filter(ref == TRUE) |>
      group_by(code_muni) |>
      summarise(freq = n()) |>
      ungroup() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      arrange(-freq) |>
      collect() |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      rename(`Município` = name_muni, `Horas` = freq)
  })

  output$rank_uv_6 <- renderDT({
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_uv) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_uv)
    }

    res |>
      mutate(ref = ifelse(value >= 6, TRUE, FALSE)) |>
      filter(ref == TRUE) |>
      group_by(code_muni) |>
      summarise(freq = n()) |>
      ungroup() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      arrange(-freq) |>
      collect() |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      rename(`Município` = name_muni, `Horas` = freq)
  })

  output$rank_uv_8 <- renderDT({
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_uv) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_uv)
    }

    res |>
      mutate(ref = ifelse(value >= 8, TRUE, FALSE)) |>
      filter(ref == TRUE) |>
      group_by(code_muni) |>
      summarise(freq = n()) |>
      ungroup() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      arrange(-freq) |>
      collect() |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      rename(`Município` = name_muni, `Horas` = freq)
  })

  output$rank_uv_11 <- renderDT({
    req(input$uf)

    if (input$uf != "Todas") {
      uf_code <- ufs |>
        filter(abbrev == input$uf) |>
        pull(code)

      res <- tbl(con, tb_uv) |>
        filter(substr(as.character(code_muni), 0, 2) == uf_code)
    } else {
      res <- tbl(con, tb_uv)
    }

    res |>
      mutate(ref = ifelse(value >= 11, TRUE, FALSE)) |>
      filter(ref == TRUE) |>
      group_by(code_muni) |>
      summarise(freq = n()) |>
      ungroup() |>
      mutate(code_muni = as.numeric(substr(as.character(code_muni), 0, 6))) |>
      arrange(-freq) |>
      collect() |>
      left_join(ref_mun_names) |>
      select(-code_muni) |>
      relocate(name_muni) |>
      rename(`Município` = name_muni, `Horas` = freq)
  })
}

shinyApp(ui, server)
