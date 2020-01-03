if (!require(pacman)) install.packages('pacman')

# Библиотеки для получения данных
pacman::p_load(httr, rvest, jsonlite)

# Тайминг и производительность
pacman::p_install(microbenchmark, force = FALSE)
pacman::p_install(tictoc, force = FALSE)

# Работа с базами данных
# pacman::p_install('DBI', force = TRUE)
# pacman::p_load(DBI, RPostgres, update = TRUE)
pacman::p_install(fst, force = FALSE)

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
pacman::p_load(dlookr, ggplot2, GGally, ggvis)

# Работа с текстом
pacman::p_load(tm, textreuse, tidytext, R.temis)
