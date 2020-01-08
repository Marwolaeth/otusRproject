df <- readRDS('data/headhunter_cut.RDS')
sapply(df, function(x) sum(is.na(x)))

df <- df %>%
  filter(!duplicated(description)) %>% # !!!!!
  mutate(description_length = nchar(description)) %>%
  mutate_at(
    vars(key_skills, specializations, employer.industries),
    function(x) ifelse(x == '', NA, x)
  ) %>%
  mutate_if(is.character, replace_na, '<missing>') %>%
  mutate_if(is.factor, fct_explicit_na, '<missing>')

saveRDS(df, 'data/headhunter_cut.RDS')
df <- readRDS('data/headhunter_cut.RDS')

if (!dir.exists('data/textual')) dir.create('data/textual')
############ ОТРАСЛИ КОМПАНИЙ ############
tf_industries <- df %>%
  unnest_tokens(
    input = employer.industries,
    output = industry,
    token = 'regex',
    pattern = '::',
    to_lower = FALSE
  )
saveRDS(tf_industries, 'data/textual/tf_industries.RDS')

dtm_industries <- tf_industries %>%
  count(id, industry) %>%
  cast_dtm(id, industry, n, weighting = tm::weightBin)
dtm_industries <- dtm_industries[df$id,]
inspect(dtm_industries)

#### Разведывание ####
quantile(col_sums(dtm_industries))
# colnames(dtm_industries)[which.max(col_sums(dtm_industries))]
# ↑ guess wut?:)
findFreqTerms(dtm_industries, 25)
####################

dtm_industries <- dtm_industries[
  , union('<missing>', findFreqTerms(dtm_industries, 25))
]
saveRDS(dtm_industries, 'data/textual/dtm_industries.RDS')
(dict_features <- tibble(
  fname = colnames(dtm_industries),
  fid = paste('industry', seq_along(colnames(dtm_industries)), sep = '_'),
  ftype = 'Отрасль'
))
rm(list = grep('_industr', ls(), value = TRUE))

############ КЛЮЧЕВЫЕ СЛОВА ############
tic()
tf_descriptions <- df %>%
  dtplyr::lazy_dt() %>%
  mutate(
    description = str_lemmatise_all(description)
  )
toc() # 21 min

save(tf_descriptions, file = 'data/textual/headhunter_lemmatised.RData')
# load('data/textual/headhunter_lemmatised.RData')

tic()
tf_descriptions <- tf_descriptions %>%
  unnest_tokens(
    input = description,
    output = term,
    token = 'ngrams',
    n = 2L,
    n_min = 1L
  ) %>%
  filter(
    !(term %in% union(ru_stopwords, c(tm::stopwords('en'), 'u')))
  ) %>%
  filter(!str_detect(term, '^[udbcf0-9\\s]+$')) %>%
  filter(!str_detect(term,'^\\d+$')) %>%
  count(id, term)
head(tf_descriptions, 10)
saveRDS(tf_descriptions, 'data/textual/tf_descriptions_cnt.RDS')
toc()

dtm_descriptions <- cast_dtm(
  tf_descriptions,
  id,
  term,
  n,
  weighting = tm::weightBin
) # %>% weightSMART(spec = 'btn') %>% round()
dtm_descriptions <- dtm_descriptions[df$id,]
dtm_descriptions

#### Разведывание ####
quantile(col_sums(dtm_descriptions))
colnames(dtm_descriptions)[which.max(col_sums(dtm_descriptions))]
dtm_descriptions <- dtm_descriptions[, col_sums(dtm_descriptions) < 2000]
inspect(dtm_descriptions[1:10,])
set.seed(200108)
inspect(dtm_descriptions[sample(nrow(df), 10),111:122])

ft_headhunter <- tf_descriptions %>%
  count(term) %>%
  arrange(desc(n)) %>%
  slice(1:25)
saveRDS(ft_headhunter, 'data/textual/ft_headhunter.RDS')
rm(ft_headhunter)
####################

saveRDS(dtm_descriptions, 'data/textual/dtm_descriptions.RDS')

all(df$id == rownames(dtm_descriptions))
st_descriptions <- specific_terms(
  dtm_descriptions,
  variable = df$job,
  p = .02,
  n = 200,
  min_occ = 20
) %>%
  map(as_tibble, .name_repair = make.names, rownames = NA) %>%
  map(tibble::rownames_to_column, var = 'term') %>%
  map(filter, t.value > 0) %>%
  bind_rows(.id = 'job') %>%
  filter(str_remove_punctuation(str_to_lower(job)) != term) %>%
  set_names(c('job', specific_term_vars)) %>%
  mutate_at(vars(starts_with('p_')), ~ . * .01) %>%
  select(-p_term_global) %>%
  mutate(odds = p_level_term / (.001 + 1 - p_level_term)) %>%
  group_by(job) %>%
  group_modify(~ arrange(., desc(odds))) %>%
  ungroup()
saveRDS(st_descriptions, 'data/textual/st_descriptions.RDS')
st_descriptions
# На диаграммы!

(dict_features <- dict_features %>%
  bind_rows(
    tibble(
      fname = st_descriptions$term,
      fid = forcats::fct_anon(as.factor(fname), prefix = 'keyword_'),
      # fid = paste('keyword', seq_along(fname), sep = '_'),
      ftype = 'Ключевое слово',
      job = st_descriptions$job
    )
  ))

dtm_descriptions <- dtm_descriptions[, unique(st_descriptions$term)]
saveRDS(dtm_descriptions, 'data/textual/dtm_descriptions_a')

#### Бонус
# Почему-то эти слова не нейтральны
true_neutral <- c(
  'gross',
  'cloud',
  'hedge',
  'sap',
  'slack',
  'sass',
  'ax',
  'work',
  'excel',
  'marvel',
  'soft'
)
description_sentiments <- tf_descriptions %>%
  dtplyr::lazy_dt() %>%
  left_join(kartaslov_emo_dict) %>%
  select(id, term, n, value) %>%
  mutate(value = replace_na(value, 0)) %>%
  left_join(
    # И английские описания
    sentiments %>%
      mutate(score = if_else(sentiment == 'positive', 1, -1)) %>%
      select(term = word, score)
  ) %>%
  mutate(value = if_else(is.na(score), value, score)) %>%
  select(-score) %>%
  mutate(value = if_else(term %in% true_neutral, 0, value)) %>%
  mutate(sent = value * n) %>%
  group_by(id) %>%
  summarise(description_sentiment = mean(sent)) %>%
  as_tibble()
df <- df %>% left_join(description_sentiments)
tapply(df$description_sentiment, df$job, summary)
cor(df$description_sentiment, log(df$salary))
tapply(df$description_sentiment, df$experience, summary) # → На диаграмму!

saveRDS(description_sentiments, 'data/textual/description_sentiments.RDS')
saveRDS(df, 'data/headhunter_plus.RDS')
rm(list = c('kartaslov_emo_dict', 'true_neutral', grep('desc', ls(), value = T)))
