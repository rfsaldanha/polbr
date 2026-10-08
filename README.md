# AlertAr Saude

Painel Shiny/WebGL para explorar a previsao atmosferica do CAMS no Brasil. A interface usa um globo MapLibre em tela cheia, rasters atualizados por proxy e uma camada canvas para particulas de vento. O controle de projecao permite alternar entre o globo e o mapa plano.

## Executar

```r
install.packages(c(
  "shiny", "bslib", "mapgl", "terra", "sf", "DBI", "duckdb", "RPostgres", "plotly", "digest",
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
periódica durante o ciclo: raios GLM a cada minuto, focos de calor do INPE a
cada 10 minutos, imagens GOES a cada 10 minutos e GPM IMERG a cada 30 minutos.
O intervalo dos focos pode ser configurado com
`ALERTAR_FIRE_REFRESH_MINUTES=10`. A consulta usa os CSVs diários
multissatélite do Programa Queimadas para hoje e ontem, preservando o último
resultado válido em memória quando a fonte estiver temporariamente indisponível.

O arquivo `bdq_focos.rds` é usado apenas como fallback na inicialização. IDs
repetidos e coordenadas inválidas são removidos. Detecções sucessivas na mesma
coordenada são consolidadas, mantendo a mais recente; a opacidade diminui com a
idade. O mapa apresenta, por padrão, as detecções das últimas 6 horas em relação
ao horário atual. A janela pode ser
alterada com `ALERTAR_FIRE_WINDOW_HOURS`.

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

## Estações FioAres

A camada **Estações FioAres**, em observações recentes, apresenta o cadastro confirmado do banco `estacoes_fioares` do ICICT. As coordenadas SIRGAS2000 são transformadas para WGS84 na exibição. Nenhuma estação ou medição é criada pelo painel.

As cores representam o **IQAr observado da estação**, já calculado pelo projeto `arapi` e armazenado em `station_iqar`: boa (verde), moderada (amarelo), ruim (laranja), muito ruim (vermelho) e péssima (roxo). A correspondência usa as [faixas do IQAr apresentadas pelo MMA](https://conama.mma.gov.br/index.php?id=27171&option=com_sisconama&task=documento.download). O índice pode ser determinado por outro poluente além de PM2.5; o poluente determinante aparece junto ao índice. As cores independem do horizonte de previsão CAMS.

O índice deve corresponder ao horário mais recente de observação de poluentes da estação e ter no máximo três horas. Estações sem índice válido nesse horário, com observações antigas, fora de operação ou cuja consulta falhou ficam cinza, com o motivo explícito. A camada é consultada a cada cinco minutos; a idade das observações é reavaliada a cada minuto. Em falhas, o último cadastro permanece visível em cinza.

Clique no ícone da estação para abrir o histórico de **PM2.5 (µg/m³)**. O gráfico mostra médias horárias e médias móveis de 24 horas lidas de `quality`, parâmetro `MP2,5`, respeitando separadamente `valid` e `rolling_valid`. Valores inválidos e horas ausentes permanecem como lacunas. Não há interpolação, imputação nem nova conversão de unidades.

Os controles de início e fim incluem ambos os dias no fuso escolhido no painel. Cada consulta admite até 31 dias. Ao abrir uma estação, o período inicial abrange os últimos sete dias do seu histórico disponível. O crédito **“FioAres/Fiocruz”** aparece dentro do gráfico, inclusive na exportação PNG do Plotly, e abaixo dele.

### Conexão ao ICICT

O consumidor usa o esquema 3 do `arapi`: `public.stations`, `public.quality`, `public.station_iqar`, `public.meta` e `public.dirty`. Cada consulta abre uma conexão PostgreSQL com transação `REPEATABLE READ, READ ONLY`, verifica a publicação e fecha a conexão. Publicação ausente, processamento pendente ou revisões em `dirty` tornam a consulta indisponível. Consultas usam parâmetros SQL e timeouts; as requisições são assíncronas e o cache é limitado e invalidado quando muda a publicação.

Configure usuário e senha no arquivo `~/.Renviron` ou no `.Renviron` **na pasta da aplicação**, seguindo [.Renviron.example](.Renviron.example):

```dotenv
FIOARES_PGUSER=arapi_reader
FIOARES_PGPASSWORD=
```

Preencha a senha entre aspas somente no seu `.Renviron` local. O campo está vazio no exemplo de propósito; não coloque credenciais reais no modelo versionado.

O host e o banco têm os padrões indicados na tabela abaixo. Ao iniciar, o aplicativo lê primeiro `~/.Renviron` (ou o arquivo indicado por `R_ENVIRON_USER`) e depois o `.Renviron` da aplicação, cujas chaves têm precedência. Isso também ocorre quando `shiny::runApp()` é chamado de uma sessão R já aberta ou pelo Shiny Server. Pare e inicie novamente a aplicação depois de editar esse arquivo. No servidor, crie o `.Renviron` na pasta da implantação `alertarsaude`, legível pelo usuário que executa o Shiny; ele não é enviado pelo Git e não recebe o conteúdo do seu `~/.Renviron` local.

Use um usuário com permissão de leitura e restrinja o acesso ao arquivo de configuração. Quando há configuração `FIOARES_PG*`, a conexão usa apenas essas variáveis e os padrões abaixo, sem herdar host ou senha de outra conexão `PG*` da sessão R. As variáveis padrão `PG*` são usadas apenas se não houver configuração específica FioAres. `FIOARES_PGPASSFILE` e `FIOARES_PGSERVICE` continuam disponíveis como alternativas; não são necessários quando usuário e senha estão no `.Renviron`. O Git ignora os arquivos locais `.Renviron`, `.env`, `.pgpass` e `.pg_service.conf`, suas variantes cobertas pelo `.gitignore` e os arquivos de sessão `.Rhistory`, `.RData` e `.Ruserdata`. Mantenha arquivos de credenciais com outros nomes fora do repositório. Em Linux/macOS, restrinja a leitura dos arquivos usados com `chmod 600 .Renviron .pgpass`.

Se uma consulta falhar, o console R/log do aplicativo registra o diagnóstico, com senhas removidas. O aviso no mapa permite tentar novamente sem aguardar o intervalo normal de atualização.

| Variável | Padrão | Finalidade |
|---|---|---|
| `FIOARES_PGHOST` | `psql.icict.fiocruz.br` | Servidor PostgreSQL |
| `FIOARES_PGPORT` | `5432` | Porta |
| `FIOARES_PGDATABASE` | `estacoes_fioares` | Banco |
| `FIOARES_PGUSER` | — | Usuário com acesso de leitura |
| `FIOARES_PGPASSWORD` | — | Senha do usuário, definida no `.Renviron` |
| `FIOARES_PGPASSFILE` | padrão do libpq | Caminho do arquivo de senha |
| `FIOARES_PGSSLMODE` | `require` | Modo SSL; pode usar `verify-full` com CA configurada |
| `FIOARES_PGSERVICE` | — | Perfil libpq; quando definido, resolve a conexão pelo perfil |
| `FIOARES_REFRESH_SECONDS` | `300` | Intervalo entre consultas, mínimo 30 segundos |
| `FIOARES_STALE_HOURS` | `3` | Idade máxima do IQAr para colorir o ícone, mínimo 1 hora |

Sem uma conexão configurada, o painel informa a indisponibilidade da camada FioAres e mantém as demais funções disponíveis.

## Publicação FioAres (08/10/2026)

A versão de produção fica em `nxctic009:/dados/htdocs/shiny.icict.fiocruz.br/alertarsaude`, disponível em <https://shiny.icict.fiocruz.br/alertarsaude/>. Esta integração parte da branch `dev` e conserva a correção de produção que verifica o ciclo CAMS a cada minuto, inclusive fora do modo totem.

Antes da ativação, a versão foi testada como usuário `shiny` em uma porta privada do servidor, com `tests/test-fioares.R` e `tests/browser-fioares.R`. A configuração FioAres é mantida apenas no `.Renviron` do servidor. Para reiniciar somente este painel, use `touch restart.txt` na pasta da aplicação.
