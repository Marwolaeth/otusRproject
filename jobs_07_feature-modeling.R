options(scipen = 999999999)

index_dtm <- list.files('data/textual', pattern = '^dtm', full.names = TRUE)

dtm_full <- map(index_dtm, readRDS) %>%
  reduce(mice::cbind)
dtm_full
rm(index_dtm)

df <- readRDS('data/headhunter_plus.RDS')
dict_features <- readRDS('data/textual/feature_dictionary.RDS')

# df <- mutate(df, description_sentiment = scale(description_sentiment)[,1])
df <- mutate(df, description_sentiment = description_sentiment * 100)
saveRDS(df, 'data/headhunter_plus.RDS')

# df <- mutate(
#   df,
#   description_language = fct_relevel(
#     df$description_language,
#     'English',
#     after = 2
#   )
# )
summary(df)

# df <- df %>%
#   mutate(description_sentiment = description_sentiment[,1])
# 
# df <- df %>%
#   select(-rus, -eng, -ratio)

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
      lambda_range = c(400, .00001),
      lambda_n = 200
    )
  ) %>%
  mutate(thedata = map(model_features, 'dataframe'))
toc()
models
map(models$model_features, 'accuracy')
if (!dir.exists('data/models')) dir.create('data/models')
saveRDS(models, 'data/models/01xxx_features.RDS')
models <- readRDS('data/models/01xx_features.RDS')

tic()
models_full <- models %>%
  mutate(
    model_full = map(
      thedata,
      ~ tryCatch(
        salary_glm_full(., pen.text = FALSE),
        error = function(e) {
          print(e)
          return(as.character(e))
        }
      )
    )
  )
toc()

models_full
saveRDS(models_full, 'data/models/02xx_variables.RDS')
models_full <- readRDS('data/models/02xx_variables.RDS') # The best so far
str(models_full, 1)

models_full$model_full %>% map('accuracy')
models_full$model_full %>% map('coefficients')

tic()
models_full <- models_full %>%
  mutate(
    model_lm = map(
      thedata,
      ~ tryCatch(
        salary_lm_stepwise(., contrast.ordinal = 'Treatment', save = TRUE),
        error = function(e) {
          print(e)
          return(as.character(e))
        }
      )
    )
  )
toc()

models_full
saveRDS(models_full, 'data/models/03xx_ols.RDS')

################
# Всё очень плохо