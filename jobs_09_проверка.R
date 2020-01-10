if (!require(pacman)) install.packages('pacman')
pacman::p_load(smurf, dplyr, purrr)

# Загрузка готовых моделей
models_full <- readRDS('data/models/02_variables.RDS')

# Метрики качества
models_full$model_full %>%
  map('accuracy') %>%
  set_names(models_full$job) %>%
  map(select, response, n, lambda, mean_abs_error, RMSE, R_sq.adj) %>%
map(
  set_names,
  c(
    'Зависимая переменная',
    'Количество наблюдений',
    'Вычисленная лямбда',
    'Средняя абсолютная ошибка',
    'Среднеквадратичная ошибка',
    'R_sq.adj'
  )
)

# Коэффициенты
models_full$model_full %>%
  map('coefficients') %>%
  set_names(models_full$job) %>%
  map(select, fname, ftype, beta) %>%
  map(filter, beta != 0) %>%
  map(as.data.frame())

# Объективно модели получились слабыми