############ БИБЛИОТЕКИ ############
if (!require(pacman)) install.packages('pacman')

# Библиотеки для получения данных
pacman::p_load(xml2, httr, rvest, jsonlite)

# Тайминг и производительность
pacman::p_install(microbenchmark, force = FALSE)
pacman::p_install(tictoc, force = FALSE)

# Работа с базами данных
# pacman::p_install('DBI', force = TRUE)
# pacman::p_load(DBI, RPostgres, update = TRUE)

# Библиотеки для обработки данных
pacman::p_load(
  dplyr,
  dtplyr,
  tidyr,
  stringr,
  purrr,
  broom,
  quantmod, # Для конвертации валют
  lubridate,
  # Always update
  update = T
)

# Визуализация и разведочный анализ
pacman::p_load(dlookr, ggplot2)

# Работа с текстом
pacman::p_install(c('tm', 'textreuse', 'tidytext', 'R.temis'), force = FALSE)

############ ВНЕШНИЕ РЕСУРСЫ ############
# Словарь русских словоформ
# © OpenCorpora
# http://opencorpora.org/

if (!dir.exists('tools')) dir.create('tools')

if (!file.exists('tools/dict.opcorpora.xml')) {
  if (!file.exists('tools/dict.opcorpora.xml.zip')) {
    download.file(
      'http://opencorpora.org/files/export/dict/dict.opcorpora.xml.zip',
      destfile = 'tools/dict.opcorpora.xml.zip',
      method = 'libcurl'
    )
  }
  unzip('tools/dict.opcorpora.xml.zip', exdir = 'tools')
}
dict <- xml2::read_xml('tools/dict.opcorpora.xml')