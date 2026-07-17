chart_reference <- function(value, label, detail, color, url) {
  list(value = value, label = label, detail = detail, color = color, url = url)
}

indicator_catalog <- function() {
  who_air_url <- "https://www.who.int/teams/environment-climate-change-and-health/air-quality-and-health/health-impacts/types-of-pollutants"
  who_uv_url <- "https://www.who.int/news-room/questions-and-answers/item/radiation-the-ultraviolet-%28uv%29-index"
  conama_iqar_url <- "https://www.gov.br/mma/pt-br/assuntos/meio-ambiente-urbano-recursos-hidricos-qualidade-ambiental/qualidade-do-ar/indice-de-qualidade-do-ar-iqar/orientacao-tecnica-indice-de-qualidade-do-ar-jan-25.pdf"

  list(
    iqar = list(
      label = "Qualidade do ar", short = "IQAr", unit = "indice",
      file = "iqar.nc", table = "iqar_mun_forecast", interval = 3,
      scale = 1, offset = 0, range = c(0, 240), digits = 0,
      colors = c("#34d399", "#facc15", "#fb923c", "#f43f5e", "#a855f7"),
      breaks = c(-Inf, 40, 80, 120, 200, Inf),
      references = list(
        chart_reference(40, "Boa", "limite 40", "#34d399", conama_iqar_url),
        chart_reference(80, "Moderada", "limite 80", "#facc15", conama_iqar_url),
        chart_reference(120, "Ruim", "limite 120", "#fb923c", conama_iqar_url),
        chart_reference(200, "Muito ruim", "limite 200", "#f43f5e", conama_iqar_url)
      ),
      reference_note = "Faixas de comunicação do IQAr segundo orientação técnica do CONAMA.",
      description = "Indice integrado da qualidade do ar segundo as faixas do CONAMA."
    ),
    pm25 = list(
      label = "Particulas finas", short = "PM2.5", unit = "ug/m3",
      file = "cams_forecast_pm25.nc", table = "pm25_mun_forecast", interval = 1,
      scale = 1e9, offset = 0, range = c(0, 80), digits = 1,
      colors = c("#22d3ee", "#84cc16", "#facc15", "#fb923c", "#f43f5e", "#a855f7"),
      breaks = c(-Inf, 15, 25, 37.5, 50, 75, Inf),
      references = list(chart_reference(15, "OMS 2021", "15 µg/m³ · média 24h", "#ffd166", who_air_url)),
      reference_note = "Referência OMS de curta duração. A curva mostra o passo nativo, não a média móvel de 24h.",
      description = "Material particulado fino, capaz de atingir as regioes profundas dos pulmoes."
    ),
    pm10 = list(
      label = "Particulas inalaveis", short = "PM10", unit = "ug/m3",
      file = "cams_forecast_pm10.nc", table = "pm10_mun_forecast", interval = 1,
      scale = 1e9, offset = 0, range = c(0, 160), digits = 1,
      colors = c("#22d3ee", "#84cc16", "#facc15", "#fb923c", "#f43f5e", "#a855f7"),
      breaks = c(-Inf, 45, 50, 75, 100, 150, Inf),
      references = list(chart_reference(45, "OMS 2021", "45 µg/m³ · média 24h", "#ffd166", who_air_url)),
      reference_note = "Referência OMS de curta duração. A curva mostra o passo nativo, não a média móvel de 24h.",
      description = "Particulas inalaveis associadas a poeira, emissoes, industria e queimadas."
    ),
    o3 = list(
      label = "Ozonio", short = "O3", unit = "ug/m3",
      file = "cams_forecast_o3_mc.nc", table = "o3_mun_forecast", interval = 3,
      scale = 1e9, offset = 0, range = c(0, 180), digits = 1,
      colors = c("#0ea5e9", "#22d3ee", "#a3e635", "#facc15", "#fb923c", "#f43f5e", "#a855f7", "#701a75"),
      breaks = c(-Inf, 20, 40, 60, 80, 100, 120, 160, Inf),
      references = list(chart_reference(100, "OMS 2021", "100 µg/m³ · máxima média 8h", "#ffd166", who_air_url)),
      reference_note = "Referência OMS para a máxima média de 8h. A curva mostra previsões a cada 3h.",
      description = "Ozonio troposferico, poluente secundario irritante para as vias respiratorias."
    ),
    co = list(
      label = "Monoxido de carbono", short = "CO", unit = "ppm",
      file = "cams_forecast_co_mc.nc", table = "co_mun_forecast", interval = 3,
      scale = 1, offset = 0, range = c(0, 15), digits = 2,
      colors = c("#22d3ee", "#84cc16", "#facc15", "#fb923c", "#f43f5e", "#a855f7"),
      breaks = c(-Inf, 1, 4, 9, 11, 13, Inf),
      references = list(chart_reference(4 * 24.45 / 28.0101, "OMS 2021", "4 mg/m³ ≈ 3,5 ppm · média 24h", "#ffd166", who_air_url)),
      reference_note = "Referência OMS de 4 mg/m³ em 24h, convertida aproximadamente para ppm a 25 °C.",
      description = "Gas gerado por combustao incompleta, especialmente veiculos, industria e queimadas."
    ),
    no2 = list(
      label = "Dioxido de nitrogenio", short = "NO2", unit = "ug/m3",
      file = "cams_forecast_no2_mc.nc", table = "no2_mun_forecast", interval = 3,
      scale = 1e9, offset = 0, range = c(0, 220), digits = 1,
      colors = c("#22d3ee", "#84cc16", "#facc15", "#fb923c", "#f43f5e", "#a855f7"),
      breaks = c(-Inf, 10, 25, 50, 120, 200, Inf),
      references = list(chart_reference(25, "OMS 2021", "25 µg/m³ · média 24h", "#ffd166", who_air_url)),
      reference_note = "Referência OMS de curta duração. A curva mostra previsões a cada 3h, não a média móvel de 24h.",
      description = "Gas reativo ligado principalmente a emissoes veiculares e processos industriais."
    ),
    so2 = list(
      label = "Dioxido de enxofre", short = "SO2", unit = "ug/m3",
      file = "cams_forecast_so2_mc.nc", table = "so2_mun_forecast", interval = 3,
      scale = 1e9, offset = 0, range = c(0, 140), digits = 1,
      colors = c("#22d3ee", "#84cc16", "#facc15", "#fb923c", "#f43f5e", "#a855f7"),
      breaks = c(-Inf, 5, 20, 40, 50, 125, Inf),
      references = list(chart_reference(40, "OMS 2021", "40 µg/m³ · média 24h", "#ffd166", who_air_url)),
      reference_note = "Referência OMS de curta duração. A curva mostra previsões a cada 3h, não a média móvel de 24h.",
      description = "Gas irritante associado a combustiveis com enxofre e a processos industriais."
    ),
    temp = list(
      label = "Temperatura", short = "Temperatura", unit = "C",
      file = "cams_forecast_temp.nc", table = "temp_mun_forecast", interval = 1,
      scale = 1, offset = -273.15, series_offset = -1,
      range = c(-5, 45), digits = 1,
      colors = c("#312e81", "#2563eb", "#22d3ee", "#a3e635", "#facc15", "#fb923c", "#ef4444"),
      breaks = NULL,
      description = "Temperatura do ar prevista proxima a superficie."
    ),
    uv = list(
      label = "Radiacao ultravioleta", short = "Indice UV", unit = "indice",
      file = "cams_forecast_uv.nc", table = "uv_mun_forecast", interval = 1,
      scale = 40, offset = 0, range = c(0, 14), digits = 1,
      colors = c("#22d3ee", "#84cc16", "#facc15", "#fb923c", "#ef4444", "#a855f7"),
      breaks = c(-Inf, 3, 6, 8, 11, 14, Inf),
      references = list(
        chart_reference(3, "Proteção solar", "recomendada a partir de IUV 3", "#facc15", who_uv_url),
        chart_reference(8, "Exposição muito alta", "evitar o sol ao meio-dia", "#fb923c", who_uv_url)
      ),
      reference_note = "Níveis de ação recomendados pela OMS para proteção contra radiação ultravioleta.",
      description = "Intensidade da radiacao ultravioleta e risco associado a exposicao solar."
    ),
    wind_speed = list(
      label = "Velocidade do vento", short = "Vento", unit = "km/h",
      file = "cams_forecast_wind_speed.nc", table = "wind_speed_mun_forecast", interval = 1,
      scale = 1, offset = 0, range = c(0, 100), digits = 1,
      colors = c("#172554", "#0369a1", "#06b6d4", "#84cc16", "#facc15", "#f97316"),
      breaks = NULL,
      description = "Velocidade do vento proxima a superficie. As particulas mostram sua direcao."
    ),
    aerosol = list(
      label = "Aerossol organico", short = "Aerossol", unit = "AOD 550 nm",
      file = "cams_forecast_aerosol.nc", table = "aerosol_mun_forecast", interval = 1,
      scale = 1, offset = 0, range = c(0, .32), digits = 3,
      colors = c("#172554", "#0e7490", "#14b8a6", "#a3e635", "#facc15", "#f97316"),
      breaks = NULL,
      description = "Profundidade optica associada a particulas organicas em suspensao."
    ),
    prec = list(
      label = "Precipitação acumulada", short = "Precipitação acumulada", unit = "mm",
      file = "cams_forecast_prec.nc", table = "prec_mun_forecast", interval = 1,
      scale = 1e3, offset = 0, range = c(0, 100), digits = 1,
      colors = c("#172554", "#1d4ed8", "#06b6d4", "#22c55e", "#facc15", "#f8fafc"),
      breaks = NULL,
      description = "Precipitacao acumulada prevista."
    )
  )
}

coverage_config <- function(id = Sys.getenv("ALERTAR_COVERAGE", "brazil")) {
  map_language <- Sys.getenv("ALERTAR_MAP_LANGUAGE", "pt")
  coverages <- list(
    brazil = list(
      id = "brazil",
      label = "Brasil",
      center = c(-60, -17),
      zoom = 2.5,
      map_language = map_language,
      bounds = list(c(-74.5, -34.8), c(-32.0, 6.5))
    ),
    lac = list(
      id = "lac",
      label = "America Latina e Caribe",
      center = c(-73, -12),
      zoom = 1.35,
      map_language = map_language,
      bounds = list(c(-118, -56), c(-28, 33))
    )
  )
  if (!id %in% names(coverages)) {
    warning("ALERTAR_COVERAGE desconhecida; usando 'brazil'.")
    id <- "brazil"
  }
  coverages[[id]]
}

resolve_data_dir <- function() {
  candidates <- unique(c(
    Sys.getenv("POLBR_DATA_DIR", unset = NA_character_),
    "/dados/home/rfsaldanha/camsdata/forecast_data",
    file.path(getwd(), "..", "camsdata", "forecast_data"),
    file.path(getwd(), "data")
  ))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  found <- candidates[dir.exists(candidates)]
  if (!length(found)) stop("Diretorio de dados nao encontrado. Defina POLBR_DATA_DIR.")
  normalizePath(found[[1]], mustWork = TRUE)
}

timezone_catalog <- function() {
  data.frame(
    timezone = c(
      "America/Sao_Paulo", "UTC",
      "America/Manaus", "America/Rio_Branco", "America/Noronha",
      "America/Mexico_City", "America/Guatemala", "America/Bogota",
      "America/Lima", "America/Caracas", "America/La_Paz",
      "America/Santiago", "America/Argentina/Buenos_Aires",
      "America/Montevideo", "America/Puerto_Rico", "America/Havana"
    ),
    code = c(
      "BRT", "UTC", "AMT", "ACT", "FNT", "MEX", "CAM", "COT",
      "PET", "VET", "BOT", "CHL", "ART", "UYT", "AST", "CUB"
    ),
    label = c(
      "BRT · Brasília", "UTC",
      "AMT · Manaus", "ACT · Rio Branco", "FNT · Fernando de Noronha",
      "MEX · Cidade do México", "CAM · Guatemala", "COT · Bogotá",
      "PET · Lima", "VET · Caracas", "BOT · La Paz", "CHL · Santiago",
      "ART · Buenos Aires", "UYT · Montevidéu", "AST · Caribe", "CUB · Havana"
    ),
    stringsAsFactors = FALSE
  )
}

normalize_timezone <- function(timezone) {
  timezone <- timezone %||% "America/Sao_Paulo"
  available <- timezone_catalog()$timezone
  if (timezone %in% available) timezone else "America/Sao_Paulo"
}

timezone_code <- function(timezone) {
  timezone <- normalize_timezone(timezone)
  catalog <- timezone_catalog()
  catalog$code[[match(timezone, catalog$timezone)]]
}

in_timezone <- function(time, timezone) {
  as.POSIXct(
    as.numeric(time), origin = "1970-01-01",
    tz = normalize_timezone(timezone)
  )
}

pretty_unit <- function(unit) {
  switch(
    unit,
    "ug/m3" = HTML("&micro;g/m&sup3;"),
    "C" = HTML("&deg;C"),
    unit
  )
}
