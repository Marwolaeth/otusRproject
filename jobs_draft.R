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
