# AlertAr Saude

Painel Shiny/WebGL para explorar a previsao atmosferica do CAMS no Brasil. A interface usa um globo MapLibre em tela cheia, rasters atualizados por proxy e uma camada canvas para particulas de vento. O controle de projecao permite alternar entre o globo e o mapa plano.

## Executar

```r
install.packages(c(
  "shiny", "bslib", "mapgl", "terra", "sf", "DBI", "duckdb",
  "jsonlite", "png", "cachem"
))
shiny::runApp()
```

Por padrao, a aplicacao procura os dados em `POLBR_DATA_DIR`, no diretorio de producao historico, em `../camsdata/forecast_data` e em `./data`, nessa ordem.

```sh
POLBR_DATA_DIR=/caminho/forecast_data Rscript -e 'shiny::runApp()'
```

Os quadros do mapa sao reamostrados em memoria para cerca de 1024 pixels no
maior eixo antes da aplicacao da paleta, mantendo a grade cientifica original e
melhorando a definicao no navegador. Para ajustar esse limite, use, por exemplo,
`ALERTAR_RASTER_SIZE=1536`; valores entre 256 e 2048 sao aceitos.

No modo totem, os dados são reabertos automaticamente a cada três horas. O intervalo pode ser ajustado, em horas, com `ALERTAR_TOTEM_REFRESH_HOURS=3`.

O modo totem também pode ser ativado no carregamento pelo parâmetro de URL
`totem`. São aceitos `?totem`, `?totem=1`, `?totem=true`, `?totem=yes`,
`?totem=on` e `?totem=sim`. O parâmetro ativa a animação e o ciclo do totem,
mas não solicita tela cheia nativa; para uma instalação dedicada, inicie o
navegador em modo quiosque, por exemplo:

```sh
/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome \
  --kiosk "https://servidor/app/?totem=1" --no-first-run
```

A cobertura geografica e configuravel sem alterar o codigo. O modo `brazil` e o padrao atual; `lac` prepara enquadramento e rotulos para America Latina e Caribe.

```sh
ALERTAR_COVERAGE=lac POLBR_DATA_DIR=/caminho/forecast_data Rscript -e 'shiny::runApp()'
```

Os nomes do mapa-base sao apresentados em portugues por padrao. Para outra lingua suportada pelos tiles, defina `ALERTAR_MAP_LANGUAGE`, por exemplo `ALERTAR_MAP_LANGUAGE=es`.

Para uma base multinacional e multiterritorial, use `territories.rds` (objeto `sf`) com as colunas canonicas `territory_id`, `territory_name`, `territory_type`, `admin1_code`, `country_code` e `country_name`. A geometria pode ser ponto ou poligono. Isso permite combinar municipios, terras indigenas, territorios quilombolas e outras areas no mesmo buscador. Durante a transicao, `places.rds` e `mun_seats.rds` continuam sendo normalizados automaticamente para esse contrato.

Os rasters presentes determinam as camadas exibidas. O banco `cams_forecast.duckdb` habilita leituras e downloads territoriais; novas tabelas podem usar `territory_id`, enquanto as tabelas municipais legadas continuam compativeis. Arquivos `wind_1.json` a `wind_121.json` habilitam a animacao de vento.

## Arquitetura

- `R/config.R`: catalogo, unidades, escalas e paletas.
- `R/data.R`: acesso lazy aos NetCDF, cache de PNGs e consultas DuckDB parametrizadas.
- `R/ui.R`: interface responsiva em tela cheia.
- `R/server.R`: reatividade, proxy MapLibre, timeline e downloads.
- `www/app.js`: particulas de vento sincronizadas ao mapa.
- `www/styles.css`: identidade visual escura.
