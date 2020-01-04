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
#############################
dict <- xml2::read_xml('tools/dict.opcorpora.xml')