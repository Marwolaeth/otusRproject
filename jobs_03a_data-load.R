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

tictoc::tic()
for (i in seq_along(job_names)) {
  (q <- hh_set_query(
    job_names[[i]],
    date_from = '2019-08-01'
  ))
  cat(sprintf('%d. Ищем: «%s»...', i, job_names[[i]]), '\n')
  vcs <- hh_vacancy_search(q, sleep = .1)
  cat(sprintf('   Найдено %d. Парсинг...', length(vcs)), '\n')
  df_path <- paste0(
    'data/',
    names(job_names)[[i]],
    '_',
    Sys.Date(),
    '.fst'
  )
  vcs %>%
    setdiff(exist) %>%
    map(hh_get_vacancy, sleep = .1) %>%
    map(hh_parse_vacancy) %>%
    bind_rows() %>%
    mutate(job = job_names[[i]]) %>%
    select(id, job, everything()) %>%
    fst::write_fst(
      path = df_path,
      compress = 20
    )
  cat(sprintf('   Сохранено: %s', df_path), '\n\n')
  exist <- c(exist, vcs)
}
tictoc::toc()
rm(q, df_path, vcs, i)
saveRDS(exist, file = 'data/exist.RDS')