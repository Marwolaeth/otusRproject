################ ПОЛЕЗНЫЕ РЕСУРСЫ ################
# https://github.com/johnmyleswhite/TextRegression/blob/master/R/regress.text.R
# https://www.rdocumentation.org/packages/glmnet/versions/3.0-2/topics/cv.glmnet
# https://www.vernier.com/til/1014/


# Словарь лемматизации
# Викиданные по работодателям: https://cran.r-project.org/web/packages/WikidataR/vignettes/Introduction.html
# Даты публикации вакансий
# Зарплаты
# Разведочный анализ
# Выделение ключевых слов


# l <- list(c(1:5), 'A', NULL, 2:5, c(TRUE, FALSE, TRUE), NULL, 1:11)
# get_subset(l)
# rm(l)

q <- hh_set_query('менеджер по продажам', per_page = 1)
q <- hh_set_query('менеджер по продажам', per_page = 1, date_from = '2019-11-11')
q
res <- hh_vacancy_search(q)
str(res, 1)

q <- hh_set_query('менеджер по продажам', date_from = '2019-12-02', clusters = TRUE)
q
res <- hh_vacancy_search(q)
str(res, 1)
content(res) %>% str(1)
res$headers
res$all_headers
res$request

res <- hh_vacancy_search(q)
str(res, 1)
res$arguments
res$clusters
str(res$clusters, 1)
str(res$clusters, 2)
res$clusters[[4]]$items %>% str(2)
res$items[[1]]

q <- hh_modify_query(q, describe_arguments = FALSE)
q <- hh_modify_query(
  q,
  describe_arguments = TRUE,
  clusters = FALSE,
  metro = 'Славянский бульвар',
  salary = 45000
)
q

map_chr(res$items, 'id')

x <- c('a' = 1, 'b' = 2, 'c' = 3)
get_subset(x, exclude = 'c')
get_subset(x, exclude = c('c', 'b'))

str(res$items[[1]])
as.Date(res$items[[100]]$published_at)

q <- hh_set_query('менеджер по продажам', date_from = '2019-12-01')
res <- hh_get_query(q)
str(res, 1)
vcs <- hh_vacancy_search(q)
any(duplicated(vcs))
sum(duplicated(vcs))
vcs[duplicated(vcs)]

x <- res$items[1] %>%
  transpose() %>%
  map(get_subset) %>%
  get_subset()
x
y <- as_tibble(x)
for (v in names(y)) {
  y <- unnest(y, v)
}

x <- res$items[[1]] %>%
  map(unlist)

x <- res$items[[1]] %>%
  unlist(recursive = F) %>%
  get_subset() %>%
  unlist(recursive = F) %>%
  as_tibble()

x <- res$items[1:3] %>%
  map(unlist, recursive = F) %>%
  map(get_subset) %>%
  map(unlist, recursive = F) %>%
  transpose() %>%
  as_tibble()

hh_get_vacancy(vcs[[2]])

gt <- hh_get_vacancy(vcs[[1]])
gt
str(gt)
######
(q <- hh_set_query(
  'менеджер по продажам',
  date_from = '2020-01-01'
))
vcs <- hh_vacancy_search(q)
saveRDS(vcs, file = 'draft_vacancies.RDS')
v <- hh_get_vacancy(vcs[[1]])
str(v)

# parse_vacancy <- cmpfun(hh_parse_vacancy)
# 
# microbenchmark::microbenchmark(
#   hh_parse_vacancy(v),
#   parse_vacancy(v),
#   times = 1000L
# )

v %>%
  select(
    -premium,
    -contains('billing_type'),
    -ends_with('.id'),
    -ends_with('url'),
    -matches('\\d$'),
    -ends_with('_id'),
    -contains('stations'),
    -allow_messages,
    -site.name,
    -archived,
    -hidden,
    -quick_responses_allowed,
    -accept_incomplete_resumes
  ) %>%
  glimpse()

v %>%
  select(
    -premium,
    -type.name,
    -contains('billing_type'),
    -ends_with('.id'),
    -ends_with('url'),
    -matches('\\d$'),
    -ends_with('_id'),
    -contains('stations'),
    -allow_messages,
    -site.name,
    -archived,
    -hidden,
    -quick_responses_allowed,
    -accept_incomplete_resumes
  ) %>%
  # Зарплата и описания — в последнюю очередь
  unite(
    'key_skills',
    starts_with('key_skills'),
    sep = '::',
    na.rm = TRUE
  ) %>%
  unite(
    'specializations',
    matches('^specializations\\d*\\.name$'),
    sep = '::',
    na.rm = TRUE
  ) %>%
  unite(
    'specializations.profarea',
    ends_with('profarea_name'),
    sep = '::',
    na.rm = TRUE
  ) %>%
  # View()
  glimpse()


######
# (q <- hh_set_query(
#   'менеджер по продажам',
#   date_from = '2019-12-01'
# ))
# vcs <- hh_vacancy_search(q)
# saveRDS(vcs, file = 'draft_vacancies.RDS')
v <- hh_get_vacancy(vcs[[1]])
str(v)

sales_db <- vcs[1:20] %>%
  map(hh_get_vacancy, preprocess = FALSE)

sales_db <- vcs[1:20] %>%
  map(~ hh_get_vacancy(.) %>% hh_parse_vacancy) %>%
  bind_rows()

v <- hh_get_vacancy(vcs[[3]])
str(v)

sales_db$experience

sales_db <- vcs %>%
  map(hh_get_vacancy, sleep = .5) %>%
  map(hh_parse_vacancy) %>%
  bind_rows()

dir.create('data')
fst::write_fst(sales_db, path = 'data/draft_sales.fst', compress = 20)

sales_db2 <- fst::read_fst('data/draft_sales.fst')
summary(sales_db2)
summary(factor(sales_db2$salary.currency))

sales_db2 %>%
  filter(salary.currency == 'RUR') %>%
  pull(salary.from) %>%
  hist(breaks = 50)

###########################################
emp <- fst::read_fst(
  'data/employers/employers_2020-01-041578154870.46152.fst')
mutate(emp, employer.industries = iconv(employer.industries, from = 'Windows-1252', to = 'UTF8'))
write.csv2(emp, file = 'data/test.csv')
emp <- read.csv2('data/test.csv', encoding = 'UTF-8')

emp <- readRDS('data/employers/employers_2020-01-041578156294.67105.RDS')
#############################
dict <- xml2::read_xml('tools/dict.opcorpora.xml')
###################################################

txt <- vacancies %>%
  slice(1:3) %>%
  pull(description) %>%
  map_chr(strip_html)

# str_stem(txt[1])
map_chr(txt, str_stem)

#####################################################
pacman::p_load(WikidataR)
?WikidataR::find_item
hh <- find_item('headhunter', language = 'ru')
str(hh, 1)
str(hh[[1]], 1)
str(hh[[1]]$match)
str(hh, 2)

company_search_template <- '(compan)|(corporat)|(organi)|(recruit)|(enterpri)|(personnel)|(staff)|(employ)'

(hhmatch <- map_chr(
  hh,
  'description'
) %>%
  str_detect(company_search_template))
(hhid <- get_subset(hh, hhmatch) %>% getElement(1) %>% getElement('id'))

hh <- get_item(hhid) %>% getElement(1)
get_property('P31')
str(hh, 1)
str(hh$claims, 1)

as.integer(
  str_extract(
    hh$claims$P571$mainsnak$datavalue$value$time,
    '\\+*\\d{4}'
  )
)

hh$claims$P31
hh$claims$P571
hh$claims$P856
hh$claims$P1454
get_property('P1128')[[1]]$labels$ru$value
get_property('P1128')[[1]]$labels$en$value
# P31  : instance of
# P571 : inception (creation date)
# P856 : official website
# P1454: организационно-правовая форма
# P3377: код компании Bloomberg
# P5181: relationship Science organization ID
# P2391: ОКПО !!!!!
# P1320: код по OpenCorporates
# P2771: код DUNS
# P3185: учётная запись ВКонтакте
# P2013: учётная запись Facebook
# P5163: учётная запись в Одноклассниках
# P2002: учётная запись в Твиттере
# P2003: учётная запись в Instagram
# P4264: ID компании в LinkedIn
# P5232: D&B Hoovers company profile
# P154 : логотип
# P159 : расположение штаб-квартиры
# P452 : отрасль
# P1128: число сотрудников

wikidata_parse_employer('HeadHunter')
wikidata_parse_employer('Сбербанк') %>% View()
emp_name <- 'Adecco'
wikidata_parse_employer('Procter&Gamble - Новомосковск')
wikidata_parse_employer('Adecco Russia')
wikidata_parse_employer('Ozon.ru')
wikidata_parse_employer(emp_name = "Озон", site = 'https://ozon.ru')
wikidata_parse_employer('Hoff')

emps <- distinct(vacancies, employer.name) %>% slice(1:20) %>% pull(employer.name)
# map(emps, wikidata_parse_employer)
wikidata <- vector('list', 20L)
for (i in seq_along(emps)) {
  emp <- emps[[i]]
  wikidata[[i]] <- wikidata_parse_employer(emp)
}
wikidata <- bind_rows(wikidata)
emp_name <- emp
##########################################
str_remove('http://sberbank.ru/', '^https*\\://(www\\.)*')%>%
  str_remove('/$')

###########################################################
###########################################################rel_job <- relate(dfs, job)
rel_job
plot(rel_job)
tidy(rel_job)
rel_job$coefficients
print(rel_job)
df_test <- df %>%
  mutate(job = as.character(job)) %>%
  mutate(job = str_replace_all(job, '\\s', '_')) %>%
  mutate(job = as.factor(job))
test_job <- pairwiseMedianTest(salary ~ job, data = df_test)
(test_job_groups <- cldList(
  p.adjust ~ Comparison,
  data = test_job,
  threshold = .05
))
test_job_groups %>%
  left_join(
    df_test %>%
      left_join(test_job_groups, by = c('job' = 'Group')) %>%
      group_by(Letter) %>%
      summarise(median = median(salary), mean = mean(salary))
  )

df_test <- df %>%
  mutate(experience = as.character(experience)) %>%
  mutate(experience = str_replace_all(experience, '\\s', '_')) %>%
  mutate(experience = as.factor(experience))
test_experience <- pairwiseMedianTest(salary ~ experience, data = df_test)
(test_experience_groups <- cldList(
  p.adjust ~ Comparison,
  data = test_experience,
  threshold = .05
))
test_experience_groups %>%
  left_join(
    df_test %>%
      left_join(test_experience_groups, by = c('experience' = 'Group')) %>%
      group_by(Letter) %>%
      summarise(median = median(salary), mean = mean(salary))
  )

test_wiki <- pairwiseMedianTest(salary ~ employer.has_wiki, data = dfs)
(test_wiki_groups <- cldList(
  p.adjust ~ Comparison,
  data = test_wiki,
  threshold = .05
))
test_wiki_groups %>%
  left_join(
    dfs %>%
      left_join(test_wiki_groups, by = c('employer.has_wiki' = 'Group')) %>%
      group_by(Letter) %>%
      summarise(median = median(salary), mean = mean(salary))
  )
rm(list = c('rel_job', 'df_test', grep('^test', ls(), value = T)))

##########################################
str_stem(df$description[1:22])
a <- map_chr(df$description, strip_html)
str_stem(a[1:22])

rm(list = c('rel_job', 'df_test', grep('^test', ls(), value = T)))

str_stem(
  str_remove_punctuation(
    str_replace_all(df$description[1:22], '[\r\n]', ' ')
  )
)

str_remove_punctuation(df$description[1:22])

stem_debug <- function(x) {
  str_stem(
    str_remove_punctuation(
      str_replace_all(x, '[\r\n]', ' ')
    )
  )
}

sapply(df$description, function(x) length(stem_debug(x)), USE.NAMES = FALSE)
a <- .Last.value
which(a > 1)

stem_debug(df$description[901])
df$description[901]
max(nchar(df$description[a==1], type = 'bytes'))
min(nchar(df$description[a>1], type = 'bytes'))
a[a>1]

stem_debug_paste <- function(x) {
  paste(
    str_stem(
      str_remove_punctuation(
        str_replace_all(x, '[\r\n]', ' ')
      )
    ),
    collapse = ' '
  )
}

b <- sapply(df$description, function(x) length(stem_debug_paste(x)), USE.NAMES = FALSE)
b[b>1]

stem_description <- function() {
  sapply(df$description, stem_debug_paste, USE.NAMES = FALSE)
}
stem_description <- compiler::cmpfun(stem_description)

tf_descriptions <- tf_descriptions %>% filter(!str_detect(term,'^\\d+$'))

all(df$id == rownames(dtm_industries))
all(df$id == rownames(dtm_descriptions))
head(df$id)
head(rownames(dtm_descriptions))

any(duplicated(df$id))
any(duplicated(df$description))
filter(df, duplicated(description)) %>% pull(description)

tf_descriptions <- filter(
  tf_descriptions,
  !(term %in% union(ru_stopwords, c(tm::stopwords('en'), 'u')))
) %>%
  filter(!str_detect(term, '^[udbcf0-9\\s]+$'))

arrange(tf_descriptions, desc(n)) %>% slice(1:10)

map2(1:5, 5:1, ~ rep(.x, .y)) %>% unlist %>% sum
sum((1:5) * (5:1))

all(df$id == rownames(dtm_specializations))

################################
dtm_industries <- dtm_industries[, colnames(dtm_industries) != '<missing>']
dtm_specializations_a <- dtm_specializations_a[, colnames(dtm_specializations_a) != '<missing>']
dtm_skills_a <- dtm_skills_a[, colnames(dtm_skills_a) != '<missing>']
dtm_descriptions_a <- dtm_descriptions_a[, colnames(dtm_descriptions_a) != '<missing>']

dtm_full <- mice::cbind(
  dtm_industries,
  dtm_descriptions_a,
  dtm_specializations_a,
  dtm_skills_a
)
dtm_full

summary(col_sums(dtm_full))

rm(list = grep('dtm_', ls(), value = TRUE))

accountant_terms <- dict_features %>%
  filter(fname != '<missing>', is.na(job) | job == 'Бухгалтер') %>% pull(fname)
accountant_ids <- filter(df, job == 'Бухгалтер') %>% pull(id)
dtm_accountant <- dtm_full[accountant_ids, accountant_terms]
dtm_accountant

X <- as.Matrix(dtm_accountant)
X
y <- filter(df, job == 'Бухгалтер') %>% select(id, salary)
all(y$id == rownames(X))
y <- y$salary

library(glmnet)
library(doParallel)
vignette('gettingstartedParallel')
cl <- makeCluster(2)
registerDoParallel(cl)

test_glm <- cv.glmnet(
  X,
  y,
  family = 'gaussian',
  standardize = FALSE,
  type.gaussian = 'naive',
  type.measure = 'mse',
  nfolds = 13,
  alignment = 'fraction',
  keep = FALSE,
  parallel = TRUE,
  trace.it = 1
)
parallel::stopCluster(cl)

plot(test_glm)
test_glm
test_glm$glmnet.fit
str(test_glm)
coef(test_glm)
(test_results <- broom::tidy(test_glm) %>%
  mutate(rmse = sqrt(estimate)))

test_coefs <- broom::tidy(coef(test_glm, s = 'lambda.min')) %>%
  as_tibble() %>%
  rename(beta_lmin = value) %>%
  left_join(
    broom::tidy(coef(test_glm, s = 'lambda.1se')) %>%
      rename(beta_l1se = value)
  ) %>%
  mutate(beta_l1se = replace_na(beta_l1se, 0)) %>%
  arrange(beta_l1se, desc(beta_lmin))

(test_predict <- tibble(
  salary = y,
  predicted = as.numeric(predict(test_glm, X, s = 'lambda.min')),
  error = predicted - salary
))
test_predict %>%
  summarise(
    mean_abs_error = mean(abs(error)),
    median_abs_error = median(abs(error)),
    RMSE = sqrt(mean(error^2))
  )
###
cl <- makeCluster(2)
registerDoParallel(cl)

test_glm_log <- cv.glmnet(
  X,
  log(y),
  family = 'gaussian',
  standardize = FALSE,
  type.gaussian = 'naive',
  type.measure = 'mse',
  nfolds = 13,
  alignment = 'fraction',
  keep = FALSE,
  parallel = TRUE,
  trace.it = 1
)
parallel::stopCluster(cl)

(test_results_log <- broom::tidy(test_glm_log) %>%
    mutate(rmse = sqrt(estimate) * 100))

test_coefs_log <- broom::tidy(coef(test_glm_log, s = 'lambda.min')) %>%
  as_tibble() %>%
  rename(beta_lmin = value) %>%
  left_join(
    broom::tidy(coef(test_glm_log, s = 'lambda.1se')) %>%
      rename(beta_l1se = value)
  ) %>%
  mutate(beta_l1se = replace_na(beta_l1se, 0)) %>%
  arrange(beta_l1se, desc(beta_lmin)) %>%
  mutate_at(vars(starts_with('beta')), ~ . * 100)

(test_predict_log <- tibble(
  salary = y,
  log_salary = log(y),
  log_predicted = as.numeric(predict(test_glm_log, X, s = 'lambda.min')),
  predicted = exp(log_predicted),
  error = predicted - salary
))
test_predict_log %>%
  summarise(
    mean_abs_error = mean(abs(error)),
    median_abs_error = median(abs(error)),
    RMSE = sqrt(mean(error^2))
  )

p_unload(parallel, doParallel)

test <- salary_glm_sparse('Бухгалтер')
str(test, 1)
test$accuracy
test$features

test <- salary_glm_sparse('Менеджер по продажам')
str(test, 1)
test$accuracy
test$features

test <- salary_glm_sparse('Дизайнер', nfold = 10)
str(test, 1)
test$accuracy
test$features
rm(test)

#######################################
# https://cran.r-project.org/web/packages/smurf/vignettes/smurf.html

library(smurf)
d <- filter(models, job == 'Бухгалтер') %>% pull(data) %>% getElement(1) %>%
  mutate(employer.has_logo = as.factor(employer.has_logo)) %>%
  mutate_if(is.factor, droplevels)
feature_vars <- grep('_\\d+$', names(d), value = TRUE)

formu <- log(salary) ~
  p(experience, pen = 'flasso') + p(employer.has_logo, pen = 'lasso') +
  p(employer.type, pen = 'gflasso', refcat = '<missing>') +
  p(address.metro.station, pen = 'gflasso', refcat = '<missing>') +
  p(address.metro.line, pen = 'gflasso', refcat = '<missing>') +
  p(log(description_length), pen = 'lasso') +
  p(description_sentiment, pen = 'lasso')

# formu <- formu %>%
#   str_from_formula() %>%
#   paste(
#     paste(
#       enclose(feature_vars, c('p(', ', pen = "lasso")')),
#       collapse = ' + '
#     ),
#     sep = ' + '
#   ) %>%
#   as.formula()
# formu

formu <- formu %>%
  str_from_formula() %>%
  paste(
    paste(
      feature_vars,
      collapse = ' + '
    ),
    sep = ' + '
  ) %>%
  as.formula()
formu

# 10 min
# tic()
# test_fit_log <- glmsmurf(
#   formu,
#   family = gaussian(),
#   data = d,
#   lambda = 'is.bic',
#   control = list(lambda.max = 800, lambda.min = 0.008, print = TRUE)
# )
# toc()

tic()
test_fit_log <- glmsmurf(
  formu,
  family = gaussian(),
  data = d,
  lambda = 'is.bic',
  control = list(lambda.max = 600, lambda.min = 0.008, print = TRUE)
)
toc()
summary(test_fit_log)
(test_predict_log <- tibble(
  salary = d$salary,
  log_salary = log(d$salary),
  log_predicted = fitted_reest(test_fit_log),
  predicted = exp(log_predicted),
  error = predicted - salary
))
test_predict_log %>%
  summarise(
    mean_abs_error = mean(abs(error)),
    median_abs_error = median(abs(error)),
    RMSE = sqrt(mean(error^2))
  )

broom::tidy(coef_reest(test_fit_log))
# d <- filter(models, job == 'Бухгалтер') %>% pull(data) %>% getElement(1) %>%
#   mutate(employer.has_logo = as.factor(employer.has_logo)) %>%
#   mutate_if(is.factor, droplevels)
# feature_vars <- grep('_\\d+$', names(d), value = TRUE)
# 
# formu <- log(salary) ~
#   experience + employer.has_logo + address.metro.line + employer.type +
#   log(description_length) + experience:description_sentiment
# 
# formu <- formu %>%
#   str_from_formula() %>%
#   paste(
#     paste(feature_vars, collapse = ' + '),
#     sep = ' + '
#   ) %>%
#   as.formula()
# formu
# 
# fit <- lm(formu, data = d)
# fit
# summary(fit)

fit <- salary_glm_full(d)
fit

d <- filter(models, job == 'Дизайнер') %>% pull(thedata) %>% getElement(1)

d <- d %>%
  bind_rows(
    slice(d, sample(nrow(d), 1)) %>%
      mutate(employer.type = '<missing>', salary = mean(d$salary))
  ) %>%
  select(-id, -job) %>%
  mutate(employer.type = as.factor(employer.type)) %>%
  mutate(employer.has_logo = as.factor(employer.has_logo)) %>%
  mutate_if(is.factor, droplevels)

fit_init <- lm(
  salary ~ experience*description_sentiment + .,
  data = d,
  contrasts = list(address.metro.station = 'contr.SAS')
)
summary(fit_init)

contrasts(d$experience)
contr.treatment(d$experience, base = 1)
contr.poly(d$experience, contrasts = FALSE)
contr.treatment(d$address.metro.line, base = 14, sparse = TRUE)

map(models_full$model_full, 'accuracy') %>%
  set_names(models_full$job) %>%
  bind_rows(.id = 'job')

map(models_full$model_full, 'coefficients') %>%
  set_names(models_full$job)

################################################################
group = 'SMM-менеджер'
category = 'Ключевые слова'
fvar = 'odds_job'
fvar_caption = 'Шанс упоминания в профессии'
colour = '#ea5e5e'
l = dict_features
  
plot_specific_features <- function(
  group,
  category,
  colour,
  fvar = 'value',
  fvar.caption = fvar,
  l,
  separator = str_pad('#', 20, pad = '#'),
  sleep = 5
) {
  d <- getElement(l, group) %>%
    getElement(category) %>%
    rename_(., fvar = 'fvar')
  
  print(separator)
  
  p <- ggplot(
    d,
    aes(x = fname, y = fvar)
  ) +
    geom_col(show.legend = FALSE, fill = colour) +
    scale_x_discrete(category) +
    geom_text(
      aes(
        label = format(round(fvar, 2), decimal.mark = ','),
        y = min(fvar) * .5
      ),
      colour = 'white',
      fontface = 'bold',
      hjust = 1
    ) +
    scale_y_sqrt(fvar.caption) +
    coord_flip() +
    # facet_grid(. ~ ftype, scales = 'free') +
    ggtitle(sprintf('%s, %s, ТОП-%d:', group, tolower(category), nrow(d))) +
    theme_minimal()
  # print(p)
  Sys.sleep(sleep)
  return(p)
}

plot_specific_features(group, category, fvar, fvar_caption, colour, l)
rm(group, category, fvar, fvar_caption, colour, l)

category_colours <- data.frame(
  category = names(dict_features[[1]]),
  colour   = c('#ea5e5e', '#6f9a8d', '#1f6650')
)
(group_grid <- expand.grid(
  category = category_colours$category,
  g = names(dict_features)
) %>%
  left_join(category_colours))

pmap(
  group_grid[1:2,],
  plot_specific_features,
  fvar = 'odds_job',
  fvar.caption = 'Шанс упоминания в профессии',
  l = dict_features
)

##############################
set.seed(1111)
df <- map(
  c(26, 14, 19, 23),
  ~ tibble(
    x = rnorm(500, 20, 4.2),
    y = rnorm(500, ., 2.4) + x * (runif(500, 3, 5)) + rnorm(500, 2, 1)
  )
) %>%
  set_names(LETTERS[1:4]) %>%
  bind_rows(.id = 'group') %>%
  mutate(group = factor(group))
df
summary(df)
cor(df$x, df$y)
tapply(df$y, df$group, mean)
hist(df$y)
plot(df$x, df$y)
boxplot(df$y ~ df$group)

(m <- lm(y ~ ., df))

(df_sum <- mutate(df, group = C(group, sum)))
(m <- lm(y ~ x + group, df_sum))
summary(m)
aov(m)

cmatrix <- matrix(
  1,
  nrow = 4,
  ncol = 3
)
diag(cmatrix) <- -3
colnames(cmatrix) <- 1:3
rownames(cmatrix) <- 1:4
cmatrix

cmatrix <- cbind(c(1,-3,1,1), c(1,1,-3,1), c(1,1,1,-3))

contrasts(df_sum$group) <- cmatrix
str(df_sum$group)
(m <- lm(y ~ x + group, df_sum))
summary(m)
aov(m)

ggplot(df, aes(x, y, colour = group)) +
  geom_point(alpha = .6) +
  stat_smooth(method = 'lm', fullrange = TRUE)

library(smurf)
?glmsmurf

############################
d <- models_full$thedata[[2]]
d <- select(d, -id, -job, -address.metro.station)
d <- d %>%
  mutate(
    address.metro.line = C(as.factor(address.metro.line), sum),
    employer.type = C(employer.type, sum)
  )
(mod <- lm(salary ~ ., d))
summary(mod)
car::vif(mod)

smod <- olsrr::ols_step_both_p(mod, pent = .05, prem = .8, details = TRUE)
smod
summary(smod$model)
(model_summary <- broom::tidy(smod$model))

#########
models$model_features[[1]]$features

# seq_exp <- function(from = 1, to = 1, lengt.out = 10) {
#   x <- from
#   while(min(x) > to) {
#     x <- c(x, min(x) - max(log(min(x), base = 1.2), .0001))
#   }
#   return(x)
# }
# seq_exp(1000, 1)
# 
# exp(seq(log(1000), log(.0001), 
#         length.out = 50))

# set.seed(119)
mod <- salary_glm_sparse('SMM-менеджер', lambda_range = c(1000, .00001), lambda_n = 200)
mod

mod <- salary_glm_sparse('SMM-менеджер')
mod

d <- models_full$thedata[[1]]
mod <- salary_glm_full(d, pen.text = TRUE)
mod$coefficients %>% View()

coefs <- mod$coefficients %>%
  filter(fid != 'Intercept') %>%
  top_n(25, abs(beta_hat)) %>%
  mutate(ftype = as.factor(ftype)) %>%
  mutate(fname = str_wrap(fname, width = 40)) %>%
  mutate(fname = fct_reorder(as.factor(fname), beta_hat))
summary(coefs)

ggplot(
  coefs,
  aes(x = fname, y = beta_hat, fill = ftype)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual('Тип предиктора', values = ftype_colours) +
  theme_light()

rm(d, mod, coefs, fcl, palette_muted)

get_exchange_rates('USD', 'RUB', dates = '2020-01-23')
# a <- 'Архаровед'
# b <- 10
# str_interp('${a}: топ-${b} коэффициентов')

rm(.data, colours, .job, .model, n, a, b)

d <- models_full$thedata[[1]]
tapply(d$salary, d$description_language, mean)
