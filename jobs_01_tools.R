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
pacman::p_load(
  tm,        # Матрицы документ-текст + база стоп-слов
  R.temis,   # Выделение ключевых терминов по классам
  textreuse, # Быстрая токенизация и расчет расстояния между документами
  tidytext,  # Пайплайн от таблицы данных к матрице документ-текст
  stringdist # Редакторское расстояние между строковыми значениями
)

############ ВНЕШНИЕ РЕСУРСЫ ############
# MyStem
# © Яндекс
# https://yandex.ru/dev/mystem/
if (!dir.exists('tools')) dir.create('tools')

if (!file.exists('tools/mystem.exe')) {
  if (!file.exists('tools/mystem.zip')) {
    download.file(
      'http://download.cdn.yandex.net/mystem/mystem-3.1-win-64bit.zip',
      destfile = 'tools/mystem.zip',
      method = 'libcurl' # Метод, при котором скачивание не зависало
    )
  }
  unzip('tools/mystem.zip', exdir = 'tools')
}
