############ БИБЛИОТЕКИ ############
if (!require(pacman)) install.packages('pacman')

# Библиотеки для получения данных
pacman::p_load(httr, rvest, jsonlite)

# Тайминг и производительность
pacman::p_load(microbenchmark, tictoc)

# Работа с базами данных
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
  forcats
)

# Визуализация и разведочный анализ
pacman::p_load(dlookr, ggplot2, rcompanion)

# Работа с текстом
pacman::p_load(
  tm,           # Матрицы документ-текст + база стоп-слов
  # textreuse,  # Быстрая токенизация и расчет расстояния между документами
  tidytext,     # Пайплайн от таблицы данных к матрице документ-текст
  # stringdist, # Редакторское расстояние между строковыми значениями
  R.temis       # Выделение ключевых терминов по классам
)
# pacman::p_load_gh('johnmyleswhite/TextRegression')

# Репрезентация и обработка больших данных
pacman::p_install(Matrix, force = FALSE, try.bioconductor = FALSE)
pacman::p_install(doParallel, force = FALSE, try.bioconductor = FALSE)

# Регрессия
pacman::p_install(glmnet, force = FALSE, try.bioconductor = FALSE)
pacman::p_install(smurf, force = FALSE, try.bioconductor = FALSE)

############ ВНЕШНИЕ РЕСУРСЫ ############
############ 
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

############
# Словарь тональности Kartaslov
# Данные: https://github.com/dkulagin/kartaslov/
# Лицензия: https://creativecommons.org/licenses/by-nc-sa/4.0/
# Молодец: https://github.com/dkulagin
if (!dir.exists('tools')) dir.create('tools')

dest <- 'tools/emo_dict.csv'
if (!file.exists(dest)) {
  root <- 'https://raw.githubusercontent.com/dkulagin/kartaslov/master/dataset/'
  repo <- 'emo_dict'
  download.file(
    paste0(root, repo, '/emo_dict.csv'),
    destfile = dest
  )
}
rm(root, repo, dest)
