if (!require(pacman)) install.packages('pacman')
pacman::p_load(smurf, dplyr, purrr)

# Загрузка готовых моделей
models_full <- readRDS('data/models/02d_variables.RDS')

# Метрики качества
models_full$model_full %>%
  map('accuracy') %>%
  set_names(models_full$job) %>%
  get_subset() %>%
  map(select, response, n, lambda, MAE, RMSE, R_sq.adj) %>%
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
  map(safely(select), fname, ftype, beta_hat, beta_hat_log) %>%
  map('result') %>%
  get_subset() %>%
  map(~ mutate(., fname = str_remove(fname, '^.+:\\s'))) %>%
  map(filter, beta_hat != 0)

for (i in seq_along(coefs)) {
  cat(paste0(models_full$job[i], ':\n'))
  print(coefs[[i]], n = 60, width = 100)
}

# Объективно модели получились слабыми