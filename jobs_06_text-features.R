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

summary(df)
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
all(df$id == rownames(dtm_industries))
inspect(dtm_industries)

#### Разведывание ####
quantile(col_sums(dtm_industries))
# colnames(dtm_industries)[which.max(col_sums(dtm_industries))]
# ↑ guess wut?:)
findFreqTerms(dtm_industries, lowfreq = 10)
####################

dtm_industries <- dtm_industries[
  , union('<missing>', findFreqTerms(dtm_industries, 20))
]
saveRDS(dtm_industries, 'data/textual/dtm_industries.RDS')

st_industries <- specific_terms(
  dtm_industries,
  variable = df$job,
  p = .01,
  n = 10,
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
saveRDS(st_industries, 'data/textual/st_industries.RDS')
st_industries # → На диаграмму!

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
  as_tibble() %>%
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
      job = st_descriptions$job,
      n_job = st_descriptions$n_term_level,
      odds_job = st_descriptions$odds
    )
  ))

dtm_descriptions <- dtm_descriptions[, unique(st_descriptions$term)]
saveRDS(dtm_descriptions, 'data/textual/dtm_descriptions_a.RDS')

#### Бонус
kartaslov_emo_dict <- read.csv(
  'tools/emo_dict.csv',
  sep = ';',
  dec = '.',
  na.strings = c('', ' '),
  colClasses = c('character', 'factor', rep('numeric', 6)),
  fileEncoding = 'UTF-8'
) %>%
  as_tibble()

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
df <- df %>%
  left_join(description_sentiments) %>%
  mutate(description_sentiment = scale(description_sentiment))
tapply(df$description_sentiment, df$job, summary)
cor(df$description_sentiment, log(df$salary))
tapply(df$description_sentiment, df$experience, summary) # → На диаграмму!

saveRDS(description_sentiments, 'data/textual/description_sentiments.RDS')
saveRDS(df, 'data/headhunter_plus.RDS')

### ЯЗЫК ОПИСАНИЯ ###
load('data/textual/headhunter_lemmatised.RData')

tic()
tf_descriptions_lan <- tf_descriptions %>%
  unnest_tokens(
    input = description,
    output = term,
    token = 'ngrams',
    n = 2L,
    n_min = 1L
  ) %>%
  filter(term != 'u') %>%
  filter(!str_detect(term, '^[udbcf0-9\\s]+$')) %>%
  filter(!str_detect(term,'^\\d+$')) %>%
  group_by(id) %>%
  summarise(
    rus = sum(term %in% ru_stopwords),
    eng = sum(term %in% tm::stopwords('en')),
    ratio = (eng + 1) / (rus + 1)
  ) %>%
  mutate(
    description_language = as.factor(
      if_else(ratio > 1, 'English', 'Русский'),
      levels = c('Русский', 'English')
    )
  )
toc()
head(tf_descriptions_lan, 10)
hist(tf_descriptions_lan$ratio, breaks = 50)
table(tf_descriptions_lan$description_language)
tf_descriptions_lan %>% filter(description_language == 'English')
df[df$id == '34784431',] %>% View()

saveRDS(tf_descriptions_lan, 'data/textual/tf_descriptions_lan.RDS')

df <- df %>% left_join(tf_descriptions_lan)
saveRDS(df, 'data/headhunter_plus.RDS')

rm(list = c('kartaslov_emo_dict', 'true_neutral', grep('desc', ls(), value = T)))
############ СПЕЦИАЛИЗАЦИИ ############
tf_specializations <- df %>%
  unnest_tokens(
    input = specializations,
    output = specialization,
    token = 'regex',
    pattern = '::',
    to_lower = FALSE
  )
saveRDS(tf_specializations, 'data/textual/tf_specializations.RDS')

dtm_specializations <- tf_specializations %>%
  count(id, specialization) %>%
  cast_dtm(id, specialization, n, weighting = tm::weightBin)
dtm_specializations <- dtm_specializations[df$id,]
all(df$id == rownames(dtm_specializations))
inspect(dtm_specializations)

#### Разведывание ####
quantile(col_sums(dtm_specializations))
colnames(dtm_specializations)[which.max(col_sums(dtm_specializations))]
####################

# dtm_specializations <- dtm_specializations[
#   , union('<missing>', findFreqTerms(dtm_specializations, 25))
#   ]
saveRDS(dtm_specializations, 'data/textual/dtm_specializations.RDS')

st_specializations <- specific_terms(
  dtm_specializations,
  variable = df$job,
  p = .1,
  n = 600,
  min_occ = 2
) %>%
  map(as_tibble, .name_repair = make.names, rownames = NA) %>%
  map(tibble::rownames_to_column, var = 'term') %>%
  map(filter, t.value > 0) %>%
  bind_rows(.id = 'job') %>%
  set_names(c('job', specific_term_vars)) %>%
  mutate_at(vars(starts_with('p_')), ~ . * .01) %>%
  select(-p_term_global) %>%
  mutate(odds = p_level_term / (.001 + 1 - p_level_term)) %>%
  group_by(job) %>%
  group_modify(~ arrange(., desc(odds))) %>%
  ungroup()
saveRDS(st_specializations, 'data/textual/st_specializations.RDS')
st_specializations
# На диаграммы!

(dict_features <- dict_features %>%
    bind_rows(
      tibble(
        fname = st_specializations$term,
        fid = forcats::fct_anon(as.factor(fname), prefix = 'spec_'),
        ftype = 'Специализация',
        job = st_specializations$job,
        n_job = st_specializations$n_term_level,
        odds_job = st_specializations$odds
      )
    ))

dtm_specializations <- dtm_specializations[, unique(st_specializations$term)]
saveRDS(dtm_specializations, 'data/textual/dtm_specializations_a.RDS')

rm(list = grep('_special', ls(), value = TRUE))

############ КЛЮЧЕВЫЕ НАВЫКИ ############
tf_skills <- df %>%
  unnest_tokens(
    input = key_skills,
    output = skill,
    token = 'regex',
    pattern = '::',
    to_lower = FALSE
  )
saveRDS(tf_skills, 'data/textual/tf_skills.RDS')

dtm_skills <- tf_skills %>%
  count(id, skill) %>%
  cast_dtm(id, skill, n, weighting = tm::weightBin)
dtm_skills <- dtm_skills[df$id,]
all(df$id == rownames(dtm_skills))
inspect(dtm_skills)

#### Разведывание ####
quantile(col_sums(dtm_skills))
colnames(dtm_skills)[which.max(col_sums(dtm_skills))]
####################

# dtm_skills <- dtm_skills[
#   , union('<missing>', findFreqTerms(dtm_skills, 25))
#   ]
saveRDS(dtm_skills, 'data/textual/dtm_skills.RDS')

st_skills <- specific_terms(
  dtm_skills,
  variable = df$job,
  p = .1,
  n = 600,
  min_occ = 2
) %>%
  map(as_tibble, .name_repair = make.names, rownames = NA) %>%
  map(tibble::rownames_to_column, var = 'term') %>%
  map(filter, t.value > 0) %>%
  bind_rows(.id = 'job') %>%
  set_names(c('job', specific_term_vars)) %>%
  mutate_at(vars(starts_with('p_')), ~ . * .01) %>%
  select(-p_term_global) %>%
  mutate(odds = p_level_term / (.001 + 1 - p_level_term)) %>%
  group_by(job) %>%
  group_modify(~ arrange(., desc(odds))) %>%
  ungroup()
saveRDS(st_skills, 'data/textual/st_skills.RDS')
st_skills
# На диаграммы!

(dict_features <- dict_features %>%
    bind_rows(
      tibble(
        fname = st_skills$term,
        fid = forcats::fct_anon(as.factor(fname), prefix = 'skill_'),
        ftype = 'Навык',
        job = st_skills$job,
        n_job = st_skills$n_term_level,
        odds_job = st_skills$odds
      )
    ))

dtm_skills <- dtm_skills[, unique(st_skills$term)]
saveRDS(dtm_skills, 'data/textual/dtm_skills_a.RDS')

saveRDS(dict_features, 'data/textual/feature_dictionary.RDS')
rm(list = grep('_skill', ls(), value = TRUE))

################################
index_dtm <- list.files('data/textual', pattern = '^dtm', full.names = TRUE)

dtm_full <- map(index_dtm, readRDS) %>%
  reduce(mice::cbind)
dtm_full
inherits(dtm_full, 'sparseMatrix') # NO