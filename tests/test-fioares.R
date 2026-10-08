# Rscript tests/test-fioares.R (live DB checks run only when configured).
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(testthat))
# Use the same Renviron loading as app startup, without starting its stores.
renviron_startup <- parse('app.R')[[1L]]
eval(renviron_startup)
for (module in c('config','i18n','fioares','fioares_ui','server')) source(paste0('R/',module,'.R'))


test_that('startup merges personal and app Renviron with app precedence', {
  temporary <- tempfile('fioares-renviron-')
  dir.create(temporary)
  withr::defer(unlink(temporary, recursive=TRUE))
  personal <- file.path(temporary, 'personal.Renviron')
  writeLines(c('FIOARES_PGUSER=fixture_user', 'FIOARES_PGPASSWORD="fixture password"',
               'FIOARES_PGHOST=personal.invalid'), personal)
  writeLines('FIOARES_PGHOST=project.invalid', file.path(temporary, '.Renviron'))
  withr::local_envvar(c(R_ENVIRON_USER=personal, FIOARES_PGUSER=NA_character_,
    FIOARES_PGPASSWORD=NA_character_, FIOARES_PGHOST=NA_character_))
  withr::local_dir(temporary)
  eval(renviron_startup)
  expect_equal(Sys.getenv('FIOARES_PGUSER'), 'fixture_user')
  expect_equal(Sys.getenv('FIOARES_PGPASSWORD'), 'fixture password')
  expect_equal(Sys.getenv('FIOARES_PGHOST'), 'project.invalid')
})

test_that('dedicated FioAres settings do not mix with a generic PG connection', {
  suffixes <- c('PGHOST','PGPORT','PGDATABASE','PGUSER','PGPASSWORD','PGPASSFILE','PGSERVICE','PGSSLMODE')
  keys <- c(suffixes, paste0('FIOARES_',suffixes))
  withr::local_envvar(stats::setNames(rep(NA_character_,length(keys)),keys))
  Sys.setenv(PGHOST='generic.invalid', PGDATABASE='generic_db', PGUSER='generic_user',
    PGPASSWORD='generic_password', FIOARES_PGUSER='fixture_user', FIOARES_PGPASSWORD='fixture_password')
  config <- fioares_config()
  expect_true(config$configured)
  expect_true(config$scoped)
  expect_equal(config$connection$host, 'psql.icict.fiocruz.br')
  expect_equal(config$connection$dbname, 'estacoes_fioares')
  expect_equal(config$connection$password, 'fixture_password')
  Sys.unsetenv(c('FIOARES_PGUSER','FIOARES_PGPASSWORD'))
  generic <- fioares_config()
  expect_false(generic$scoped)
  expect_equal(generic$connection$host, 'generic.invalid')
  expect_equal(generic$connection$password, 'generic_password')
})

test_that('diagnostics retain useful errors without passwords', {
  withr::local_envvar(c(FIOARES_PGPASSWORD='fixture_secret', PGPASSWORD='generic_secret'))
  diagnostic <- character()
  withCallingHandlers(fioares_log_error(simpleError(paste(
    'connection failed fixture_secret generic_secret config_secret',
    'postgresql://reader:url_secret@host/db')), list(connection=list(password='config_secret')), 'consulta'),
    message=function(condition) { diagnostic <<- conditionMessage(condition); invokeRestart('muffleMessage') })
  expect_match(diagnostic, 'connection failed', fixed=TRUE)
  expect_false(grepl('fixture_secret|generic_secret|config_secret|url_secret', diagnostic))
  expect_match(diagnostic, '[redigido]', fixed=TRUE)
})

test_that('a manual retry bypasses the failure cache', {
  isolated <- new.env(parent=globalenv())
  sys.source('R/fioares.R', envir=isolated)
  isolated$fioares_fetch_snapshot <- function(config) stop('simulated connection failure')
  isolated$fioares_log_error <- function(...) invisible(NULL)
  old_plan <- future::plan(future::sequential)
  withr::defer(future::plan(old_plan))
  store <- isolated$create_fioares_store(list(configured=TRUE, refresh_seconds=300))
  withr::defer(store$close())
  await <- function(promise) {
    done <- FALSE
    result <- NULL
    promises::then(promise, onFulfilled=function(value) { result <<- value; done <<- TRUE })
    deadline <- Sys.time()+10
    while (!done && Sys.time()<deadline) later::run_now(.05)
    if (!done) stop('Promise did not resolve')
    result
  }
  first <- await(store$refresh_async())
  expect_equal(first$status, 'unavailable')
  cached <- await(store$refresh_async())
  expect_identical(cached$checked_at, first$checked_at)
  retried <- await(store$refresh_async(force=TRUE))
  expect_gt(as.numeric(retried$checked_at), as.numeric(first$checked_at))
})

test_that('date ranges are inclusive in the selected timezone and bounded', {
  bounds <- fioares_date_interval('2026-09-20','2026-09-20','America/Sao_Paulo')
  expect_equal(diff(bounds),86400000)
  expect_equal(diff(fioares_date_interval('2026-08-01','2026-08-31','America/Sao_Paulo')),31*86400000)
  expect_error(fioares_date_interval('2026-08-01','2026-09-01','America/Sao_Paulo'),'range')
  expect_equal(format(as.POSIXct(bounds[1]/1000,origin='1970-01-01',tz='UTC'),tz='UTC'), '2026-09-20 03:00:00')
  expect_error(fioares_date_interval('2026-09-21','2026-09-20','UTC'),'dates')
  expect_error(fioares_date_interval(NA,'2026-09-20','UTC'),'dates')
  expect_error(fioares_date_interval('2024-01-01','2026-01-01','UTC'),'range')
})

test_that('all user-facing messages are translated', {
  for (language in c('en','es','fr')) expect_setequal(names(fioares_translations[[language]]),names(fioares_translations$pt))
})

cfg <- fioares_config()
if (!cfg$configured) {
  cat('Live database checks skipped: configure FIOARES_PGUSER or FIOARES_PGSERVICE.\n')
} else {
  snapshot <- fioares_fetch_snapshot(cfg)
  x <- snapshot$stations
  test_that('the ICICT connection is read-only and returns confirmed station locations', {
    if (isTRUE(cfg$scoped)) {
      withr::local_envvar(c(PGHOSTADDR='127.0.0.1', PGUSER='unrelated_user', PGPASSWORD='unrelated_password'))
    }
    con <- fioares_connect(cfg)
    if (isTRUE(cfg$scoped)) {
      expect_equal(Sys.getenv('PGHOSTADDR'), '127.0.0.1')
      expect_equal(Sys.getenv('PGPASSWORD'), 'unrelated_password')
    }
    on.exit(DBI::dbDisconnect(con))
    expect_equal(DBI::dbGetQuery(con,'SHOW transaction_read_only')[[1]],'on')
    expect_true(nrow(x)>0)
    expect_false(anyDuplicated(x$station_id)>0)
    expect_true(all(is.finite(x$lon)&abs(x$lon)<=180))
    expect_true(all(is.finite(x$lat)&abs(x$lat)<=90))
    expect_true(all(nzchar(x$name)))
  })
  test_that('real station observations expire and cannot retain a good color on failure', {
    for (i in seq_len(nrow(x))) {
      if (!is.finite(x$ts_ms[i])) next
      time <- as.POSIXct(x$ts_ms[i]/1000,origin='1970-01-01',tz='UTC')
      live <- fioares_station_state(x[i,],now=time)
      if (is.finite(x$iqar[i]) && x$operation[i]=='Em operação') {
        labels <- c(good='Boa',moderate='Moderada',bad='Ruim',very_bad='Muito ruim',extreme='Péssima')
        expect_equal(unname(labels[live$state]),x$classification[i])
      }
      expect_equal(fioares_station_state(x[i,],now=time+10801)$color,unname(fioares_colors()['unknown']))
      expect_equal(fioares_station_state(x[i,],status='unavailable',now=time)$state,'unavailable')
    }
  })
  station <- x[which.max(x$last_ms),]
  end <- as.Date(as.POSIXct(station$last_ms/1000,origin='1970-01-01',tz='UTC'),tz='America/Sao_Paulo')
  interval <- fioares_date_interval(end-6,end,'America/Sao_Paulo')
  result <- fioares_fetch_history(cfg,station$station_id,interval)
  test_that('PM2.5 equals the source and preserves invalid and missing hours', {
    expect_identical(result$station_id, station$station_id)
    fioares_with_publication(cfg,function(con,meta) {
      direct <- DBI::dbGetQuery(con,paste("SELECT ts_ms,value,valid,rolling,rolling_valid FROM public.quality",
        "WHERE station_id=$1 AND parameter='MP2,5' AND ts_ms >= $2 AND ts_ms < $3 AND profile=$4 ORDER BY ts_ms"),
        params=list(station$station_id,interval[1],interval[2],meta$quality_profile))
      direct <- direct[match(result$data$ts_ms,direct$ts_ms),]
      expected <- direct$value
      expected[is.na(direct$valid)|direct$valid!=1|!is.finite(expected)] <- NA_real_
      expected_rolling <- direct$rolling
      expected_rolling[is.na(direct$rolling_valid)|direct$rolling_valid!=1|!is.finite(expected_rolling)] <- NA_real_
      expect_equal(result$data$value,expected,tolerance=0)
      expect_equal(result$data$rolling,expected_rolling,tolerance=0)
      expect_equal(nrow(result$data),168L)
      expect_true(all(diff(result$data$ts_ms)==3600000))
    })
    # A parameterized invalid identifier cannot broaden the query.
    bad <- fioares_fetch_history(cfg,"' OR 1=1 --",interval)
    expect_false(any(is.finite(bad$data$value)))
  })
  test_that('the interactive chart includes source credit and does not connect gaps', {
    chart <- plotly::plotly_build(fioares_plot(result$data,station,'pt','America/Sao_Paulo'))
    expect_match(chart$x$layout$annotations[[1]]$text,'FioAres/Fiocruz',fixed=TRUE)
    expect_true(all(vapply(chart$x$data,function(trace) identical(trace$connectgaps,FALSE),logical(1))))
    expect_equal(chart$x$layout$yaxis$title$text,'PM2.5 (µg/m³)')
    expect_equal(chart$x$layout$xaxis$title$text,'America/Sao_Paulo')
  })
  cat('Live FioAres checks passed using ICICT data.\n')
}
