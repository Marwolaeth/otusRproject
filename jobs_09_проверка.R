if (!require(pacman)) install.packages('pacman')
pacman::p_load(smurf, dplyr, purrr)

# Загрузка готовых моделей
models_full <- readRDS('data/models/02d_variables.RDS')

# Метрики качества
models_full$model_full %>%
  map('accuracy') %>%
  set_names(models_full$job) %>%
  get_subset() %>%
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
coefs <- models_full$model_full %>%
  map('coefficients') %>%
  set_names(models_full$job) %>%
  map(safely(select), fname, ftype, beta) %>%
  map('result') %>%
  get_subset() %>%
  map(~ mutate(., fname = str_remove(fname, '^.+:\\s'))) %>%
  map(filter, beta != 0)

for (i in seq_along(coefs)) {
  cat(paste0(models_full$job[i], ':'))
  print(coefs[[i]], n = 60, width = 100)
}

# Объективно модели получились слабыми