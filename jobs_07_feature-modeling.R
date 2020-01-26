options(scipen = 999999999)

index_dtm <- list.files('data/textual', pattern = '^dtm', full.names = TRUE)

dtm_full <- map(index_dtm, readRDS) %>%
  reduce(mice::cbind)
dtm_full
rm(index_dtm)

df <- readRDS('data/headhunter_plus.RDS')
dict_features <- readRDS('data/textual/feature_dictionary.RDS')

df <- mutate(df, description_sentiment = scale(description_sentiment)[,1])
saveRDS(df, 'data/headhunter_plus.RDS')

df <- mutate(
  df,
  description_language = fct_relevel(
    df$description_language,
    'English',
    after = 2
  )
)
summary(df)
saveRDS(df, 'data/headhunter_plus.RDS')

models <- tibble(
  job = levels(df$job)
)

tic()
models <- models %>%
  mutate(
    model_features = map(
      job,
      salary_glm_sparse,
      lambda_range = c(1000, .00001),
      lambda_n = 200
    )
  ) %>%
  mutate(thedata = map(model_features, 'dataframe'))
toc()
models
if (!dir.exists('data/models')) dir.create('data/models')
saveRDS(models, 'data/models/01f_features.RDS')
models <- readRDS('data/models/01f_features.RDS')

tic()
models_full <- models %>%
  mutate(
    model_full = map(
      thedata,
      ~ tryCatch(salary_glm_full(., pen.text = TRUE), error = function(e) {
        print(e)
        return(as.character(e))
      }
      )
    )
  )
toc()

# tic()
# models_full <- models %>%
#   mutate(
#     model_full = map(
#       thedata,
#       salary_glm_full
#     )
#   )
# toc()

models_full
saveRDS(models_full, 'data/models/02z_variables.RDS')
models_full <- readRDS('data/models/02m_variables.RDS') # The best so far
str(models_full, 1)

models_full$model_full %>% map('accuracy')

# # Метрики качества
# models_full$model_full %>%
#   map('accuracy') %>%
#   set_names(models_full$job) %>%
#   map(select, n, lambda, mean_abs_error, RMSE, R_sq.adj) %>%
#   map(
#     set_names,
#     c(
#       'Количество наблюдений',
#       'Вычисленная лямбда',
#       'Средняя абсолютная ошибка',
#       'Среднеквадратичная ошибка',
#       'R_sq.adj'
#     )
#   )
# 
# # Коэффициенты
# models_full$model_full %>%
#   map('coefficients') %>%
#   set_names(models_full$job) %>%
#   map(select, fname, ftype, beta) %>%
#   map(filter, beta != 0)

################
# Всё очень плохо