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
  mutate(data = map(model_features, 'dataframe'))
models
if (!dir.exists('data/models')) dir.create('data/models')
saveRDS(models, 'data/models/01_features.RDS')
