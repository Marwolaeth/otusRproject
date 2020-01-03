############ начало ############
exist <- character(0)
saveRDS(exist, file = 'data/exist.RDS')
# readRDS('data/exist.RDS')

job_names <- c(
  sales = 'Менеджер по продажам',
  accountant = 'Бухгалтер',
  designer = 'Дизайнер',
  marketologist = 'Маркетолог',
  smm = 'SMM-менеджер'
)

for (i in seq_along(job_names)) {
  (q <- hh_set_query(
    job_names[[i]],
    date_from = '2019-08-01'
  ))
  cat(sprintf('%d. Ищем: «%s»...', i, job_names[[i]]), '\n')
  vcs <- hh_vacancy_search(q)
  cat(sprintf('   Найдено %d. Парсинг...', length(vcs)), '\n')
  df_path = paste0(
    'data/',
    names(job_names)[[i]],
    '_',
    Sys.Date(),
    '.fst'
  )
  vcs %>%
    setdiff(exist) %>%
    map(hh_get_vacancy, sleep = .5) %>%
    map(hh_parse_vacancy) %>%
    bind_rows() %>%
    mutate(job = job_names[[i]]) %>%
    select(id, job, everything()) %>%
    write_fst(
      path = df_path,
      compress = 20
    )
  cat(sprintf('   Сохранено: %s', df_path), '\n\n')
  exist <- c(exist, vcs)
}

############  ############
###### ПРОДАЖИ ######

(q <- hh_set_query(
  'менеджер по продажам',
  date_from = '2019-08-01'
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
  date_from = '2019-08-01'
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
  date_from = '2019-08-01'
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
  date_from = '2019-08-01'
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
  date_from = '2019-08-01'
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
saveRDS(exist, file = 'data/exist.RDS')

walk2(
  jobs,
  job_names,
  ~ write_fst(
    get(.x),
    path = paste0('data/', .y, '_', Sys.Date(), '.fst')
  )
)

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
