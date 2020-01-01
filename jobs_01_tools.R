if (!require(pacman)) install.packages('pacman')

# Библиотеки для получения данных
pacman::p_load(httr, rvest, jsonlite)

# Работа с базами данных
# pacman::p_install('DBI', force = TRUE)
# pacman::p_load(DBI, RPostgres, update = TRUE)
p_install(fst)

# Библиотеки для обработки данных
pacman::p_load(
  dplyr,
  dbplyr,
  dtplyr,
  tidyr,
  stringr,
  purrr,
  broom,
  lubridate,
  # Always update
  update = T
)

# Визуализация и разведочный анализ
pacman::p_load(dlookr, ggplot2, GGally, ggvis)
