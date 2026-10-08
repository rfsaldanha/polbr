# AlertAr Saúde — polbr

Painel Shiny com previsões atmosféricas municipais do CAMS e observações das estações FioAres/Fiocruz.

## Execução

Instale as dependências em R:

```r
install.packages(c(
  "shiny", "shinyWidgets", "fs", "bslib", "dplyr", "lubridate", "sf",
  "leaflet", "leaflet.extras2", "DBI", "duckdb", "ggplot2", "geomtextpath",
  "terra", "DT", "readr", "RPostgres", "plotly", "jsonlite", "cachem",
  "digest", "promises", "future", "parallelly"
))
```

Informe o diretório dos dados CAMS publicados, contendo o banco `cams_forecast.duckdb`, os rasters NetCDF, os arquivos `wind_<passo>.json` e `bdq_focos.rds`:

```bash
POLBR_DATA_DIR=/caminho/para/forecast_data Rscript -e 'shiny::runApp()'
```

Sem `POLBR_DATA_DIR`, o caminho usado continua sendo `/dados/home/rfsaldanha/camsdata/forecast_data/`. Os cadastros municipais são lidos da pasta `data/` deste projeto.

## Estações FioAres

Ative **Estações FioAres** na barra lateral para exibir as estações nos mapas. A camada usa o cadastro confirmado do banco `estacoes_fioares` do ICICT, com coordenadas transformadas de SIRGAS2000 para WGS84. A implementação foi adaptada do projeto `polbr_sesai` para os mapas Leaflet deste painel.

As cores representam o **IQAr observado da estação**, publicado pelo `arapi` em `station_iqar`: boa, moderada, ruim, muito ruim e péssima. O índice e seu poluente determinante aparecem na identificação da estação. As cores independem da variável e do horizonte de previsão selecionados. Estações fora de operação, sem índice válido no horário mais recente, com observações antigas ou com falha na consulta ficam cinza, com o motivo indicado.

Clique em uma estação para abrir seu histórico de **PM2.5 (µg/m³)**. O gráfico apresenta médias horárias e médias móveis de 24 horas, lidas de `quality` para o parâmetro `MP2,5`, respeitando separadamente `valid` e `rolling_valid`. Horas ausentes e valores inválidos permanecem como lacunas, sem interpolação ou conversão adicional de unidades. O crédito FioAres/Fiocruz também aparece na imagem exportada pelo Plotly.

As datas incluem ambos os dias, no fuso **America/Sao_Paulo**, usado nos gráficos municipais. Cada consulta admite até 31 dias; a abertura usa os últimos sete dias disponíveis da estação. Clicar em uma estação não altera o município selecionado.

A consulta das estações é atualizada a cada cinco minutos enquanto a camada está ativada; a idade das observações é reavaliada a cada minuto. Em falhas, o último cadastro permanece visível em cinza e o painel oferece **Tentar novamente**. Sem configuração, a barra lateral informa a indisponibilidade da conexão.

### Conexão

Defina as credenciais em `~/.Renviron` ou no `.Renviron` da aplicação, seguindo [.Renviron.example](.Renviron.example):

```dotenv
FIOARES_PGUSER=arapi_reader
FIOARES_PGPASSWORD=
```

Preencha a senha apenas no arquivo local. O aplicativo lê primeiro `~/.Renviron` (ou `R_ENVIRON_USER`) e depois `.Renviron` do projeto, que tem precedência. Reinicie a aplicação após editar a configuração. Na implantação, configure o arquivo para o usuário que executa o Shiny; as credenciais locais não são enviadas pelo Git.

| Variável | Padrão | Finalidade |
|---|---|---|
| `FIOARES_PGHOST` | `psql.icict.fiocruz.br` | Servidor PostgreSQL |
| `FIOARES_PGPORT` | `5432` | Porta |
| `FIOARES_PGDATABASE` | `estacoes_fioares` | Banco |
| `FIOARES_PGUSER` | — | Usuário com permissão de leitura |
| `FIOARES_PGPASSWORD` | — | Senha |
| `FIOARES_PGSSLMODE` | `require` | Modo SSL |
| `FIOARES_PGPASSFILE` | padrão do libpq | Arquivo de senha alternativo |
| `FIOARES_PGSERVICE` | — | Perfil libpq alternativo |
| `FIOARES_REFRESH_SECONDS` | `300` | Intervalo de atualização, mínimo 30 segundos |
| `FIOARES_STALE_HOURS` | `3` | Idade máxima do índice para colorir a estação, mínimo 1 hora |
| `ALERTAR_ASYNC_WORKERS` | `2` | Processos de consulta, limitados aos núcleos disponíveis e a 4 |

Quando existem configurações `FIOARES_PG*`, a conexão não herda as configurações genéricas `PG*`. Estas são usadas apenas na ausência de configuração específica FioAres. Os arquivos locais de credenciais são ignorados pelo Git; mantenha arquivos com outros nomes fora do repositório.

Cada consulta abre e fecha uma conexão independente, com transação `REPEATABLE READ, READ ONLY`, parâmetros SQL e timeouts. O consumidor exige o esquema 3 do `arapi`, publicação pronta em `meta` e ausência de processamento pendente em `dirty`. As consultas são assíncronas, com cache limitado e invalidado quando muda a publicação. O painel não grava no banco FioAres. Diagnósticos de falhas são registrados no console com senhas removidas.

## Validação

Os testes exigem `testthat` e `withr`. A navegação automatizada usa `chromote` e Chrome/Chromium:

```r
install.packages(c("testthat", "withr", "chromote"))
```

```bash
Rscript tests/test-fioares.R
POLBR_DATA_DIR=/caminho/para/forecast_data Rscript -e 'shiny::runApp(".", port = 3878, launch.browser = FALSE)'
# Em outro terminal:
Rscript tests/browser-fioares.R
```

Os testes R verificam configuração, datas inclusivas, retentativa, conexão somente de leitura, classificação e fidelidade das séries ao banco real. Sem credenciais, apenas as verificações independentes do banco são executadas. O roteiro de navegador exige a conexão FioAres e compara as curvas de cada estação com a fonte, verifica troca de estação, datas inválidas, visibilidade da camada, mudanças de previsão/município/aba e apresentação em tela pequena. `FIOARES_TEST_URL` permite usar outra URL local.
