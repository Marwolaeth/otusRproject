############ ВАКАНСИИ ############
# Вакансии, которые уже есть в базе; изначально — пустое множество
# exist <- character(0)
if (!dir.exists('data')) dir.create('data')
# saveRDS(exist, file = 'data/exist.RDS')
exist <- readRDS('data/exist.RDS')

job_names <- c(
  sales = 'Менеджер по продажам',
  accountant = 'Бухгалтер',
  economist = 'Экономист',
  hr = 'Менеджер по персоналу',
  designer = 'Дизайнер',
  marketologist = 'Маркетолог',
  smm = 'SMM-менеджер'
)

if (!dir.exists('data/vacancies')) dir.create('data/vacancies')
tictoc::tic()
for (i in seq_along(job_names)) {
  (q <- hh_set_query(
    job_names[[i]],
    date_from = '2019-08-01'
  ))
  cat(sprintf('%d. Ищем: «%s»...', i, job_names[[i]]), '\n')
  vcs <- hh_vacancy_search(q, sleep = .1)
  cat(sprintf('   Найдено %d', length(vcs)), '\n')
  # Не загружаем уже загруженные вакансии
  vcs <- setdiff(vcs, exist)
  cat(sprintf('   Новых: %d. Парсинг...', length(vcs)), '\n')
  df_path <- paste0(
    'data/vacancies/',
    names(job_names)[[i]],
    '_',
    Sys.Date(),
    '-',
    as.numeric(Sys.time()),
    '.RDS'
  )
  if (length(vcs) > 0) {
    vcs %>%
      setdiff(exist) %>%
      map(hh_get_vacancy, sleep = .1) %>%
      map(hh_parse_vacancy) %>%
      bind_rows() %>%
      mutate(job = job_names[[i]]) %>%
      select(id, job, everything()) %>%
      saveRDS(
        file = df_path,
        compress = TRUE
      )
    cat(sprintf('   Сохранено: %s', df_path), '\n\n')
    exist <- c(exist, vcs)
  }
}
tictoc::toc()
rm(q, df_path, vcs, i)
saveRDS(exist, file = 'data/exist.RDS')

vacancies <- list.files(
  'data/vacancies',
  pattern = '.RDS',
  full.names = TRUE
) %>%
  map(readRDS) %>%
  reduce(bind_rows) %>%
  dtplyr::lazy_dt()
saveRDS(as_data_frame(vacancies), 'data/vacancies.RDS')

############ РАБОТОДАТЕЛИ ############
# employers_exist <- character(0)
employers_exist <- readRDS('data/employers_exist.RDS')
if (!dir.exists('data/employers')) dir.create('data/employers')
employer_ids   <- distinct(vacancies, employer.id) %>%
  pull(employer.id) %>%
  setdiff(employers_exist)
employer_names <- filter(vacancies, employer.id %in% employer_ids) %>%
  distinct(employer.id, .keep_all = TRUE) %>%
  pull(employer.name)
df_path <- paste0(
  'data/employers/',
  'employers',
  '_',
  Sys.Date(),
  '-',
  as.numeric(Sys.time()),
  '.RDS'
)
tictoc::tic()
if (length(employer_ids) > 0) {
  employers <- employer_ids %>%
    map(hh_get_employer, sleep = .3) %>%
    map(hh_parse_employer) %>%
    bind_rows()
  saveRDS(employers, file = df_path, compress = TRUE)
}
tictoc::toc()

employers <- list.files(
  'data/employers',
  pattern = '.RDS',
  full.names = TRUE
) %>%
  map(readRDS) %>%
  bind_rows()
saveRDS(employers, 'data/employers.RDS')
rm(df_path)

employers_exist <- unique(employers$employer.id)
saveRDS(employers_exist, 'data/employers_exist.RDS')

if (!dir.exists('data/employers/wikidata')) dir.create('data/employers/wikidata')

# Функция получения выгрузки нестабильна
# Поэтому временно фиксится функцией-оболочкой
# С сохранением каждого отдельного результата
wikidata_get <- function(x, y, z) {
  d <- skip_null(wikidata_parse_employer(x, site = z))
  success <- skip_null(nrow(d))
  if (is.na(success) | success == 0) success <- FALSE else success <- TRUE
  if (success) {
    d$employer.id <- y
    saveRDS(d, paste0('data/employers/wikidata/', y, '.RDS'))
  }
  # “Change...”
  return(success)
  # “...Going and coming without error...”
}
employer_ids   <- distinct(employers, employer.id) %>%
  pull(employer.id)
employer_names <- filter(employers, employer.id %in% employer_ids) %>%
  distinct(employer.id, .keep_all = TRUE) %>%
  pull(employer.name)
employer_urls <- filter(employers, employer.id %in% employer_ids) %>%
  distinct(employer.id, .keep_all = TRUE) %>%
  pull(employer.site_url)

# wikidata <- map2_lgl(employer_names, employer_ids, wikidata_get)
wikidata <- pmap_lgl(
  list(employer_names, employer_ids, employer_urls),
  wikidata_get
)
sum(wikidata)
wikidata <- list.files('data/employers/wikidata/', full.names = TRUE)

employers_wikidata <- map(
  wikidata,
  readRDS
) %>%
  bind_rows()
saveRDS(employers_wikidata, 'data/employers_wikidata.RDS')