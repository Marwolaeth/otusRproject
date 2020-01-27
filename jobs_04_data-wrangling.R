employers_wikidata <- readRDS('data/employers_wikidata.RDS')
vacancies <- readRDS('data/vacancies.RDS')
employers <- readRDS('data/employers.RDS')

############ ВИКИДАННЫЕ ############
employers_wikidata <- select(employers_wikidata, -contains('social'))
(wikidata_cols <- setdiff(names(employers_wikidata), names(employers)))
glimpse(employers_wikidata)
(garbage <- apply(employers_wikidata, 1, function(x) sum(is.na(x)) >= 7))
sum(garbage)
employers_wikidata <- filter(employers_wikidata, !garbage)
saveRDS(employers_wikidata, 'data/employers_wikidata.RDS')
summary(employers_wikidata)

# employers_wikidata <- employers_wikidata %>%
#   mutate_if(is.character, as.factor) %>%
#   mutate_at(dplyr::vars(employer.name, employer.id), as.character)
# glimpse(employers_wikidata)
# summary(employers_wikidata)
# unique(employers_wikidata$employer.country)
# 
# c_russian <- c('Россия', 'Русское царство', 'Российская империя')
# c_cis     <- c('Эстония', 'Украина', 'Казахстан')
# c_other   <- c(
#   'Китайская Народная Республика',
#   'Турция',
#   'Япония',
#   'Египет',
#   'Аргентина',
#   'Катар'
# )
# employer_class_levels <- c(
#   'Малое предприятие',
#   'Среднее предприятие',
#   'Крупное предприятие',
#   'Корпорация'
# )
# 
# hist(employers_wikidata$employer.no_employees)
# 
# wikidata <- employers_wikidata %>%
#   mutate(
#     employer.region = factor(
#       case_when(
#         is.na(employer.country) ~ 'Россия',
#         employer.country %in% c_russian ~ 'Россия',
#         employer.country %in% c_cis ~ 'быв. СССР',
#         employer.country %in% c_other ~ 'Другой',
#         TRUE ~ 'Запад'
#       )
#     ),
#     employer.founded.cat = factor(
#       case_when(
#         is.na(employer.founded) ~ NA_character_,
#         employer.founded >= 2010 ~ 'Z',
#         between(employer.founded, 1989, 2009) ~ 'Y',
#         between(employer.founded, 1949, 1988) ~ 'X',
#         between(employer.founded, 1901, 1948) ~ 'Old',
#         TRUE ~ 'Extremely old'
#       ),
#       ordered = TRUE
#     ),
#     employer.class = factor(
#       case_when(
#         employer.no_employees <= 50 ~ 'Малое предприятие',
#         between(employer.no_employees, 51, 500) ~ 'Среднее предприятие',
#         between(employer.no_employees, 501, 10000) ~ 'Крупное предприятие',
#         is.na(employer.no_employees) ~ NA_character_,
#         TRUE ~ 'Корпорация'
#       ),
#       ordered = TRUE,
#       levels = employer_class_levels
#     )
#   ) %>%
#   select(-c(employer.country, employer.founded, employer.headquarters, employer.no_employees))
# glimpse(wikidata)

wikidata <- mutate(employers_wikidata, employer.has_wiki = TRUE) %>%
  select(employer.id, employer.has_wiki)

saveRDS(wikidata, 'data/employers_wikidata2.RDS')
rm(
  list = c(
    grep('^c', ls(), value = T),
    'employers_wikidata',
    'employer_class_levels'
  )
)
rm(employer_urls, employer_names, employer_ids, garbage, wikidata_get, wikidata_parse_employer)
saveRDS(wikidata, 'data/employers_wikidata.RDS')

############ РАБОТОДАТЕЛИ ############
# employers <- left_join(employers, wikidata) %>%
#   mutate_if(is.character, as.factor) %>%
#   mutate_at(dplyr::vars(employer.name, employer.id), as.character) %>%
#   mutate_at(vars(contains('site')), as.character) %>%
#   mutate(
#     employer.region = as.character(employer.region),
#     employer.region = factor(
#       ifelse(
#         str_detect(employer.site_url, 'ru$'),
#         'Россия',
#         employer.region
#       )
#     )
#   )
# 
# site_tmp <- stringdist(
#   str_remove(employers$employer.site_url, '^https*\\://(www\\.)*'),
#   str_remove(employers$employer.wiki_site, '^https*\\://(www\\.)*'),
#   method = 'lv'
# )
# wikidata <- wikidata %>%
#   anti_join(filter(employers, site_tmp >= 25))
# employers <- readRDS('data/employers.RDS')
# employers <- left_join(employers, wikidata) %>%
#   mutate_if(is.character, as.factor) %>%
#   mutate_at(dplyr::vars(employer.name, employer.id), as.character) %>%
#   mutate_at(vars(contains('site')), as.character) %>%
#   mutate(
#     employer.region = as.character(employer.region),
#     employer.region = factor(
#       ifelse(
#         str_detect(employer.site_url, 'ru$'),
#         'Россия',
#         employer.region
#       )
#     )
#   )

employers <- employers %>%
  left_join(wikidata) %>%
  mutate(employer.has_wiki = if_else(is.na(employer.has_wiki), FALSE, TRUE))
saveRDS(employers, 'data/employers.RDS')

############ ВАКАНСИИ ############
vacancies <- vacancies %>%
  mutate(
    salary.currency = if_else(
      salary.currency == 'RUR',
      'RUB',
      salary.currency
    )
  )
saveRDS(vacancies, 'data/vacancies_tmp.RDS')

exchange_rates <- map_df(
  unique(vacancies$salary.currency),
  ~ data.frame(salary.currency = ., rate = get_exchange_rates(., 'RUB', dates = '2020-01-23'))
)

# vacancies <- vacancies %>%
#   left_join(exchange_rates) %>%
#   mutate(salary = salary * rate) %>%
#   select(-rate)

vacancies <- vacancies %>%
  mutate_at(
    vars(ends_with('_at')),
    ymd_hms
  )
summary(vacancies)
sapply(vacancies, function(x) sum(is.na(x)))

vacancies <- vacancies %>%
  # mutate(
  #   description = if_else(
  #     is.na(description_branded),
  #     description,
  #     description_branded
  #   )
  # ) %>%
  select(-description_branded) %>%
  mutate(description = map_chr(description, strip_html))
saveRDS(vacancies, 'data/vacancies.RDS')
length(unique(vacancies$employer.id))

vacancies <- vacancies %>%
  mutate_at(vars(salary.from, salary.to), as.numeric) %>%
  mutate(
    salary.to = if_else(salary.to > 500000, salary.to * .1, salary.to)
  ) %>%
  mutate(
    salary = case_when(
      !is.na(salary.from) & !is.na(salary.to) ~ (salary.from + salary.to) / 2,
      is.na(salary.to) ~ salary.from * 1.05,
      is.na(salary.from) ~ salary.to * .9,
      TRUE ~ as.numeric(salary.from)
    )
  ) %>%
  mutate(
    salary.currency = if_else(
      salary.currency == 'RUR',
      'RUB',
      salary.currency
    )
  ) %>%
  left_join(exchange_rates) %>%
  mutate(salary = salary * rate) %>%
  select(-rate) %>%
  mutate(
    salary.gross = if_else(is.na(salary.gross), TRUE, salary.gross),
    salary = if_else(
      salary.gross,
      salary * .87,
      salary
    )
  )
sum(is.na(vacancies$salary))
quantile(vacancies$salary)

df <- vacancies %>%
  left_join(employers) %>%
  mutate_at(
    vars(
      job,
      site,
      area,
      address.metro.station,
      address.metro.line,
      department,
      employer.type,
      employer.area
    ),
    as.factor
  ) %>%
  select(-starts_with('salary.'))
summary(df)
glimpse(df)
saveRDS(df, 'data/headhunter.RDS')
