# AlertAr Saude

Painel Shiny/WebGL para explorar a previsao atmosferica do CAMS no Brasil. A interface usa um globo MapLibre em tela cheia, rasters atualizados por proxy e uma camada canvas para particulas de vento. O controle de projecao permite alternar entre o globo e o mapa plano.

## Executar

```r
install.packages(c(
  "shiny", "bslib", "mapgl", "terra", "sf", "DBI", "duckdb",
  "jsonlite", "png", "cachem", "curl", "ncdf4", "promises", "future",
  "parallelly"
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
O pré-carregamento dos rasters e a coleta de raios usam até dois processos por
padrão, respeitando os núcleos disponíveis. Defina `ALERTAR_ASYNC_WORKERS` entre
1 e 4 para ajustar esse limite; com apenas um worker, o app usa execução
sequencial sem tentar abrir um cluster local.

No modo totem, os dados de previsão são reabertos automaticamente a cada três
horas. O intervalo pode ser ajustado, em horas, com
`ALERTAR_TOTEM_REFRESH_HOURS=3`. As fontes observadas permanecem em atualização
periódica durante o ciclo: raios GLM a cada minuto, imagens GOES a cada 10
minutos e GPM IMERG a cada 30 minutos. O arquivo `bdq_focos.rds` é verificado
por alteração a cada 10 minutos, sem interromper a animação; esse intervalo pode
ser configurado com `ALERTAR_TOTEM_LIVE_REFRESH_MINUTES=10`. A produção e a
substituição desse arquivo continuam sendo responsabilidade do processo externo
de dados.

O arquivo externo de focos pode conservar os três dias exigidos pelo app
histórico. Nesta interface, IDs repetidos e coordenadas inválidas são removidos e
o mapa apresenta, por padrão, apenas as últimas 24 horas disponíveis. A janela
pode ser alterada com `ALERTAR_FIRE_WINDOW_HOURS`.

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

O controle **Imagens meteorológicas** oferece observações em tempo quase real
distribuídas como tiles Web Mercator pelo NASA GIBS. Para o GOES-East estão
disponíveis cores naturais, infravermelho térmico, massas de ar, poeira,
temperatura de incêndios e canal visível. A fonte GPM IMERG Early Run V07
acrescenta a taxa de precipitação média em 30 minutos, em mm/h, com resolução
global de 0,1° e latência nominal próxima de quatro horas. O app consulta a
atualização dessa camada a cada 30 minutos e exibe a legenda oficial do GIBS. O
horário efetivamente servido pelo provedor aparece no fuso escolhido.

A camada **Raios · GLM/NOAA** consulta o produto vetorial `GLM-L2-LCFA` do
GOES-East no NOAA Open Data Dissemination. Os flashes de boa qualidade dos cinco
minutos mais recentes são exibidos como pulsos luminosos, que perdem intensidade
gradualmente conforme envelhecem. A consulta é incremental: depois da primeira
carga, somente arquivos novos são baixados. O campo de visão do GLM alcança
aproximadamente 54°N–54°S.

A área de relatórios compara a unidade selecionada com unidades do mesmo tipo no
estado e no país. Os rankings, horas acima da referência e horas por faixa são
calculados no DuckDB sem formar médias espaciais estaduais ou nacionais. O
relatório inclui gráficos comparativos e tabelas pesquisáveis, ordenáveis e
paginadas. O resultado pode ser exportado como um único arquivo HTML
autocontido, mantendo essas interações.

## Arquitetura

- `R/config.R`: catalogos de indicadores, observações, unidades, escalas e paletas.
- `R/glm.R`: acesso incremental e leitura dos flashes recentes do GOES-East GLM.
- `R/data.R`: acesso lazy aos NetCDF, cache de PNGs e consultas DuckDB parametrizadas.
- `R/ui.R`: interface responsiva em tela cheia.
- `R/server.R`: reatividade, proxy MapLibre, timeline e downloads.
- `www/app.js`: partículas de vento e pulsos GLM sincronizados ao mapa.
- `www/report.js`: interatividade das tabelas e escalas dos relatorios.
- `www/styles.css`: identidade visual escura.
