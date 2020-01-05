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

c_russian <- c('Россия', 'Русское царство')
c_cis     <- c('Эстония', 'Украина')
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

############ РАБОТОДАТЕЛИ ############
employers <- left_join(employers, wikidata)