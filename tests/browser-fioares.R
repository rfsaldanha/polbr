# Start the app first; FIOARES_TEST_URL defaults to localhost:3877.
library(chromote)
eval(parse('app.R')[[1L]])
for (module in c('config','i18n','fioares','fioares_ui','server')) source(paste0('R/',module,'.R'))
fioares_test_config <- fioares_config()
stopifnot(fioares_test_config$configured)
fioares_test_stations <- fioares_fetch_snapshot(fioares_test_config)$stations
options(chromote.chrome_args=c('--no-sandbox','--disable-dev-shm-usage','--enable-unsafe-swiftshader'))
b <- ChromoteSession$new()
withr::defer(b$close())
js <- function(code) {
  answer <- b$Runtime$evaluate(code,returnByValue=TRUE)
  if (!is.null(answer$exceptionDetails)) stop(answer$exceptionDetails$exception$description)
  answer$result$value
}
wait_js <- function(code,timeout=300) {
  deadline <- Sys.time()+timeout
  repeat {
    if (isTRUE(tryCatch(js(code),error=function(e) FALSE))) return(invisible(TRUE))
    if (Sys.time()>deadline) {
      print(js("({status:document.querySelector('#fioares_status')?.textContent,history:document.querySelector('#fioares_history_status')?.textContent,errors:[...document.querySelectorAll('.shiny-output-error')].map(x=>x.textContent)})"))
      stop('Browser timeout: ',code)
    }
    Sys.sleep(.25)
  }
}
check <- function(code) stopifnot(isTRUE(js(paste0("Boolean(",code,")"))))
b$Emulation$setDeviceMetricsOverride(width=1440,height=1000,deviceScaleFactor=1,mobile=FALSE)
b$Page$navigate(Sys.getenv('FIOARES_TEST_URL','http://127.0.0.1:3877'),wait_=FALSE)
wait_js("document.querySelectorAll('.fioares-marker').length > 0")
cat('Real station markers loaded.\n')
js(paste0('window.fioaresCitiesByName=',jsonlite::toJSON(stats::setNames(as.list(fioares_test_stations$city),fioares_test_stations$name),auto_unbox=TRUE),';true'))
js("window.fioaresStalePlots=[];window.fioaresPlotMonitor=setInterval(()=>{const name=document.querySelector('.fioares-station-name strong')?.textContent;const title=document.querySelector('#fioares_history_plot .gtitle')?.textContent;const city=window.fioaresCitiesByName[name];if(city && title && title !== 'PM2.5 · '+city) window.fioaresStalePlots.push({city,title});},20);true")
check("document.querySelectorAll('.fioares-marker-button').length === 5")
check("[...document.querySelectorAll('.fioares-marker-button')].every(x=>x.getAttribute('aria-label') && x.title.includes('FioAres/Fiocruz'))")
js("window.beforeTerritory=document.querySelector('#territory').selectize.getValue();document.querySelector('.fioares-marker-button').click();true")
wait_js("!!document.querySelector('#fioares_history_plot .main-svg') && document.querySelector('#fioares_history_status').textContent.trim() === ''")
check("document.querySelector('#territory').selectize.getValue() === beforeTerritory")
check("document.querySelector('#fioares_history_plot').textContent.includes('FioAres/Fiocruz')")
check("document.querySelector('#fioares_start input') && document.querySelector('#fioares_end input')")
check("document.querySelector('#fioares_history_plot').textContent.includes('PM2.5')")
cat('Station click and historical plot passed.\n')

# Switching stations must never restore the previous modal's htmlwidget.
# Compare the actual traces with the source, including the cached first station.
for (id in c(fioares_test_stations$station_id[-1], fioares_test_stations$station_id[1])) {
  js("document.querySelector('#shiny-modal .modal-footer button').click();true")
  wait_js("!document.querySelector('#shiny-modal')")
  js(paste0("document.querySelector('.fioares-marker[data-station-id=",jsonlite::toJSON(id,auto_unbox=TRUE),"] button').click();true"))
  station <- fioares_test_stations[fioares_test_stations$station_id==id,]
  title <- jsonlite::toJSON(paste0('PM2.5 · ',station$city),auto_unbox=TRUE)
  wait_js(paste0("(()=>{const p=document.querySelector('#fioares_history_plot .js-plotly-plot');return !!p?.data && p.layout.title.text === ",title," && document.querySelector('#fioares_history_status').textContent.trim() === '';})()"))
  payload <- js("(()=>{const p=document.querySelector('#fioares_history_plot .js-plotly-plot');return {id:Shiny.shinyapp.$inputValues.fioares_station_click,start:Shiny.shinyapp.$inputValues['fioares_start:shiny.date'],end:Shiny.shinyapp.$inputValues['fioares_end:shiny.date'],traces:p.data.map(t=>({x:t.x,y:t.y}))};})()")
  stopifnot(identical(payload$id,id))
  expected <- fioares_fetch_history(fioares_test_config,id,fioares_date_interval(payload$start,payload$end,'America/Sao_Paulo'))$data
  times <- format(expected$date,'%Y-%m-%d %H:%M:%S',tz='America/Sao_Paulo')
  for (index in seq_along(payload$traces)) {
    trace <- payload$traces[[index]]
    # Plotly inserts null timestamps at gaps; keep them aligned with null y values.
    plotted_times <- vapply(trace$x,function(value) if(is.null(value)) NA_character_ else value,character(1))
    positions <- match(plotted_times,times)
    stopifnot(!any(is.na(positions) & !is.na(plotted_times)))
    observed <- vapply(trace$y,function(value) if(is.null(value)) NA_real_ else as.numeric(value),numeric(1))
    values <- expected[[c('value','rolling')[[index]]]]
    stopifnot(sum(is.finite(observed))==sum(is.finite(values)))
    comparison <- all.equal(observed,values[positions],tolerance=1e-7,check.attributes=FALSE)
    if (!isTRUE(comparison)) {
      saveRDS(list(station=id,trace=index,observed=observed,expected=values[positions],times=trace$x),'/tmp/polbr-fioares-trace-mismatch.rds')
      stop('Source mismatch for ',id,' trace ',index,': ',paste(comparison,collapse='; '))
    }
  }
  cat('Station history matches ICICT:',id,'\n')
}
check("fioaresStalePlots.length === 0")
cat('Station switching shows only the selected station.\n')

b$screenshot(filename='/tmp/polbr-fioares-history.png')
js("window.historyChart=document.querySelector('#fioares_history_plot.js-plotly-plot') || document.querySelector('#fioares_history_plot .js-plotly-plot');true")
check("historyChart.data.every(trace=>trace.connectgaps===false)")
js("window.lastDate=document.querySelector('#fioares_end input').value;Shiny.setInputValue('fioares_start:shiny.date',Shiny.shinyapp.$inputValues['fioares_end:shiny.date'],{priority:'event'});true")
wait_js("document.querySelector('#fioares_history_status').textContent.trim() === '' && historyChart.data[0].x.length > 0 && historyChart.data[0].x.length <= 24 && historyChart.data[0].x.every(x => x.startsWith(Shiny.shinyapp.$inputValues['fioares_end:shiny.date']))")
cat('Date selection passed.\n')
js("Shiny.setInputValue('fioares_start:shiny.date','2026-09-22',{priority:'event'});Shiny.setInputValue('fioares_end:shiny.date','2026-09-21',{priority:'event'});true")
wait_js("document.querySelector('#fioares_history_status').textContent.includes('Selecione datas válidas')")
js("Shiny.setInputValue('fioares_start:shiny.date','2024-01-01',{priority:'event'});Shiny.setInputValue('fioares_end:shiny.date','2026-09-21',{priority:'event'});true")
wait_js("document.querySelector('#fioares_history_status').textContent.includes('31')")
js("Shiny.setInputValue('fioares_start:shiny.date','2000-01-01',{priority:'event'});Shiny.setInputValue('fioares_end:shiny.date','2000-01-02',{priority:'event'});true")
wait_js("document.querySelector('#fioares_history_status').textContent.includes('Não há medições válidas')")
cat('Invalid ranges and empty history passed.\n')
js("document.querySelector('#shiny-modal .modal-footer button').click();document.querySelector('#show_fioares').click();true")
wait_js("document.querySelectorAll('.fioares-marker').length === 0")
js("document.querySelector('#show_fioares').click();true")
wait_js("document.querySelectorAll('.fioares-marker').length === 5")
js("document.querySelector('.fioares-marker-button').click();true")
wait_js("!!document.querySelector('#fioares_history_plot .main-svg') && document.querySelector('#fioares_history_status').textContent.trim() === ''")
check("document.querySelectorAll('.shiny-output-error').length===0")
b$screenshot(filename='/tmp/polbr-fioares-history.png')
b$Emulation$setDeviceMetricsOverride(width=390,height=844,deviceScaleFactor=1,mobile=TRUE)
Sys.sleep(1)
check("document.querySelector('.modal-body').scrollWidth <= document.querySelector('.modal-body').clientWidth + 2")
b$screenshot(filename='/tmp/polbr-fioares-mobile.png')
check("fioaresStalePlots.length === 0")
js("clearInterval(fioaresPlotMonitor);true")
cat('FioAres browser checks passed. Screenshots: /tmp/polbr-fioares-history.png and /tmp/polbr-fioares-mobile.png\n')
