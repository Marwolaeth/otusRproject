############ начало ############
exist <- character(0)
saveRDS(exist, file = 'data/exist.RDS')

###### ПРОДАЖИ ######

(q <- hh_set_query(
  'менеджер по продажам',
  date_from = '2019-09-01'
))
vcs <- hh_vacancy_search(q)

sales_db <- vcs %>%
  map(hh_get_vacancy, sleep = .5) %>%
  map(hh_parse_vacancy) %>%
  bind_rows()

###### БУХГАЛТЕР ######
(q <- hh_set_query(
  'бухгалтер',
  date_from = '2019-09-01'
))
vcs <- hh_vacancy_search(q)

account_db <- vcs %>%
  map(hh_get_vacancy, sleep = .5) %>%
  map(hh_parse_vacancy) %>%
  bind_rows()

###### ДИЗАЙНЕР ######
(q <- hh_set_query(
  'дизайнер',
  date_from = '2019-09-01'
))
vcs <- hh_vacancy_search(q)

design_db <- vcs %>%
  map(hh_get_vacancy, sleep = .5) %>%
  map(hh_parse_vacancy) %>%
  bind_rows()

############ сохранение ############
fst::write_fst(sales_db, path = 'data/sales10.fst', compress = 20)
fst::write_fst(account_db, path = 'data/accounts10.fst', compress = 20)
fst::write_fst(design_db, path = 'data/design10.fst', compress = 20)

exist <- fst::read_fst('data/sales10.fst') %>%
  bind_rows(fst::read_fst('data/accounts10.fst')) %>%
  bind_rows(fst::read_fst('data/design10.fst')) %>%
  pull(id)
saveRDS(exist, file = 'data/exist.RDS')
# readRDS('data/exist.RDS')

############  ############
###### ПРОДАЖИ ######

(q <- hh_set_query(
  'менеджер по продажам',
  date_from = '2019-09-01'
))
vcs <- hh_vacancy_search(q)

sales_db <- vcs %>%
  setdiff(exist) %>%
  map(hh_get_vacancy, sleep = .5) %>%
  map(hh_parse_vacancy) %>%
  bind_rows()

###### БУХГАЛТЕР ######
(q <- hh_set_query(
  'бухгалтер',
  date_from = '2019-09-01'
))
vcs <- hh_vacancy_search(q)

account_db <- vcs %>%
  setdiff(exist) %>%
  map(hh_get_vacancy, sleep = .5) %>%
  map(hh_parse_vacancy) %>%
  bind_rows()

###### ДИЗАЙНЕР ######
(q <- hh_set_query(
  'дизайнер',
  date_from = '2019-09-01'
))
vcs <- hh_vacancy_search(q)

design_db <- vcs %>%
  setdiff(exist) %>%
  map(hh_get_vacancy, sleep = .5) %>%
  map(hh_parse_vacancy) %>%
  bind_rows()

###### МАРКЕТОЛОГ ######
(q <- hh_set_query(
  'маркетолог',
  date_from = '2019-09-01'
))
vcs <- hh_vacancy_search(q)

market_db <- vcs %>%
  setdiff(exist) %>%
  map(hh_get_vacancy, sleep = .5) %>%
  map(hh_parse_vacancy) %>%
  bind_rows()

###### SMM ######
(q <- hh_set_query(
  'smm',
  date_from = '2019-09-01'
))
vcs <- hh_vacancy_search(q)

smm_db <- vcs %>%
  setdiff(exist) %>%
  map(hh_get_vacancy, sleep = .5) %>%
  map(hh_parse_vacancy) %>%
  bind_rows()

############ сохранение ############
jobs <- grep('\\_db', ls(), value = TRUE)
job_names <- str_split(jobs, '_', simplify = TRUE)[, 1]

exist <- c(exist, map(jobs, ~ get(.) %>% pull(id)) %>% unlist()) %>% unique()

walk2(
  jobs,
  job_names,
  ~ write_fst(
    get(.x),
    path = paste0('data/', .y, '_', Sys.Date(), '.fst')
  )
)

saveRDS(exist, file = 'data/exist.RDS')

############ загрузка ############
job_names_ru <- c(
  'Бухгалтер',
  'Дизайнер',
  'Маркетолог',
  'Менеджер по продажам',
  'SMM-менеджер'
) %>%
  set_names(job_names)

db <- map2(
  job_names_ru,
  job_names,
  function(j, jn) { # Явный вызов анонимной функции с именами аргументов,
    list.files(     # Чтобы не путались с аргументами внутренних map()
      path = 'data/',
      pattern = jn,
      recursive = FALSE,
      full.names = TRUE    # Иначе невозможно прочесть файл в субдиректории
    ) %>%
      map(read_fst) %>%
      bind_rows() %>%
      distinct(id, .keep_all = TRUE) %>%
      mutate(job = j) %>%
      select(id, job, everything())
  }
) %>%
  bind_rows() %>%
  mutate(job = factor(job))

names(db)
