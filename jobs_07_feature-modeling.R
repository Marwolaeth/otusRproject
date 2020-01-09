options(scipen = 999999999)

index_dtm <- list.files('data/textual', pattern = '^dtm', full.names = TRUE)

dtm_full <- map(index_dtm, readRDS) %>%
  reduce(mice::cbind)
dtm_full
rm(index_dtm)

df <- readRDS('data/headhunter_plus.RDS')
dict_features <- readRDS('data/textual/feature_dictionary.RDS')

models <- tibble(
  job = levels(df$job)
)

models <- models %>%
  mutate(model_features = map(job, salary_glm_sparse)) %>%
  mutate(thedata = map_df(model_features, 'dataframe'))
models
if (!dir.exists('data/models')) dir.create('data/models')
saveRDS(models, 'data/models/01_features.RDS')
models <- readRDS('data/models/01_features.RDS')

tic()
models_full <- models %>%
  mutate(
    model_full = map(
      thedata,
      ~ tryCatch(salary_glm_full(.), error = function(e) {
        print(e)
        return('Features unavailable')
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
saveRDS(models_full, 'data/models/02_variables.RDS')
str(models_full, 1)

models_full$model_full[2:7] %>% map('accuracy')

################
# Всё очень плохо