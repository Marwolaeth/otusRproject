employers_wikidata <- readRDS('data/employers_wikidata.RDS')
vacancies <- readRDS('data/vacancies.RDS')
employers <- readRDS('data/employers.RDS')

############ ВИКИДАННЫЕ ############
employers_wikidata <- select(employers_wikidata, -contains('social'))
glimpse(employers_wikidata)
(garbage <- apply(employers_wikidata, 1, function(x) sum(is.na(x)) >= 7))
sum(garbage)
employers_wikidata <- filter(employers_wikidata, !garbage)
saveRDS(employers_wikidata, 'data/employers_wikidata.RDS')
summary(employers_wikidata)

employers_wikidata <- employers_wikidata %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at(dplyr::vars(employer.name, employer.id), as.character)
glimpse(employers_wikidata)
summary(employers_wikidata)
unique(employers_wikidata$employer.country)

c_russian <- c('Россия', 'Русское царство', 'Российская империя')
c_cis     <- c('Эстония', 'Украина', 'Казахстан')
c_other   <- c(
  'Китайская Народная Республика',
  'Турция',
  'Япония',
  'Египет',
  'Аргентина',
  'Катар'
)
employer_class_levels <- c(
  'Малое предприятие',
  'Среднее предприятие',
  'Крупное предприятие',
  'Корпорация'
)

hist(employers_wikidata$employer.no_employees)

wikidata <- employers_wikidata %>%
  mutate(
    employer.region = factor(
      case_when(
        is.na(employer.country) ~ 'Россия',
        employer.country %in% c_russian ~ 'Россия',
        employer.country %in% c_cis ~ 'быв. СССР',
        employer.country %in% c_other ~ 'Другой',
        TRUE ~ 'Запад'
      )
    ),
    employer.founded.cat = factor(
      case_when(
        is.na(employer.founded) ~ NA_character_,
        employer.founded >= 2010 ~ 'Z',
        between(employer.founded, 1989, 2009) ~ 'Y',
        between(employer.founded, 1949, 1988) ~ 'X',
        between(employer.founded, 1901, 1948) ~ 'Old',
        TRUE ~ 'Extremely old'
      ),
      ordered = TRUE
    ),
    employer.class = factor(
      case_when(
        employer.no_employees <= 50 ~ 'Малое предприятие',
        between(employer.no_employees, 51, 500) ~ 'Среднее предприятие',
        between(employer.no_employees, 501, 10000) ~ 'Крупное предприятие',
        is.na(employer.no_employees) ~ NA_character_,
        TRUE ~ 'Корпорация'
      ),
      ordered = TRUE,
      levels = employer_class_levels
    )
  ) %>%
  select(-c(employer.country, employer.founded, employer.headquarters, employer.no_employees))
glimpse(wikidata)

saveRDS(wikidata, 'data/employers_wikidata.RDS')
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
employers <- left_join(employers, wikidata) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at(dplyr::vars(employer.name, employer.id), as.character) %>%
  mutate_at(vars(contains('site')), as.character) %>%
  mutate(
    employer.region = as.character(employer.region),
    employer.region = factor(
      ifelse(
        str_detect(employer.site_url, 'ru$'),
        'Россия',
        employer.region
      )
    )
  )

site_tmp <- stringdist(
  str_remove(employers$employer.site_url, '^https*\\://(www\\.)*'),
  str_remove(employers$employer.wiki_site, '^https*\\://(www\\.)*'),
  method = 'lv'
)
wikidata <- wikidata %>%
  anti_join(filter(employers, site_tmp >= 25))
employers <- readRDS('data/employers.RDS')
employers <- left_join(employers, wikidata) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at(dplyr::vars(employer.name, employer.id), as.character) %>%
  mutate_at(vars(contains('site')), as.character) %>%
  mutate(
    employer.region = as.character(employer.region),
    employer.region = factor(
      ifelse(
        str_detect(employer.site_url, 'ru$'),
        'Россия',
        employer.region
      )
    )
  )
saveRDS(employers, 'data/employers.RDS')

############ ВАКАНСИИ ############
vacancies <- vacancies %>%
  mutate(
    salary = case_when(
      !is.na(salary.from) & !is.na(salary.to) ~ (salary.from + salary.to) / 2,
      is.na(salary.to) ~ salary.from * 1.05,
      is.na(salary.from) ~ salary.to * .9,
      TRUE ~ as.numeric(salary.from)
    ),
    salary.gross = if_else(is.na(salary.gross), FALSE, salary.gross)
  ) %>%
  mutate(
    salary = if_else(
      salary.gross,
      salary * .87,
      salary
    )
  ) %>%
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
  ~ data.frame(salary.currency = ., rate = get_exchange_rates(., 'RUB'))
)

vacancies <- vacancies %>%
  left_join(exchange_rates) %>%
  mutate(salary = salary * rate) %>%
  select(-rate)

vacancies <- vacancies %>%
  mutate_at(
    vars(ends_with('_at')),
    ymd_hms
  )
summary(vacancies)
saveRDS(vacancies, 'data/vacancies.RDS')
