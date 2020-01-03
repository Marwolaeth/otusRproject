start_page <- 'https://api.hh.ru/'
agens <- 'SalaryRegression/1.0 (a.pawluczenko@gmail.com, https://github.com/Marwolaeth/otusRproject)'

# Функция конвертации валют
# © Max Ghenis
# https://stackoverflow.com/a/26694739
# Немного модифицирована с учетом названий объектов, возвращаемых getFX()
# Важно!!! рубль идентифицируется как RUB
get_exchange_rates <- function(from, to, dt = Sys.Date() - 1) {
  require(quantmod)
  obj.names <- getFX(paste0(from, "/", to), from = dt, to = dt)
  result <- numeric(length(obj.names))
  names(result) <- obj.names
  data.names <- gsub('/', '', obj.names)
  for (i in seq_along(obj.names)) {
    result[obj.names[i]] <- as.numeric(get(data.names[i]))[1]
  }
  return(result)
}

# Аналог getElement() для выбора подмножества
# Подходит для включения в пайплайн %>%
get_subset <- function(x, .what = sapply(x, length) > 0, exclude = NULL) {
  if (!is.null(exclude)) x <- x[setdiff(names(x), exclude)]
  '['(x, .what)
}

# Функция для быстрого заключения строки в скобки/кавычки/и т.д.
enclose <- function(s, enclosure = c('(', ')')){
  if (enclosure == '(')   enclosure <- c(enclosure, ')')
  if (enclosure == '((')  enclosure <- c(enclosure, '))')
  if (enclosure == '[')   enclosure <- c(enclosure, ']')
  if (enclosure == '[[')  enclosure <- c(enclosure, ']]')
  if (enclosure == '[[[') enclosure <- c(enclosure, ']]]')
  if (enclosure == '{')   enclosure <- c(enclosure, '}')
  if (enclosure == '{{')  enclosure <- c(enclosure, '}}')
  if (enclosure == '<')   enclosure <- c(enclosure, '>')
  if (enclosure == '<<')  enclosure <- c(enclosure, '>>')
  if (enclosure == '>')   enclosure <- c(enclosure, '<')
  if (enclosure == '«')   enclosure <- c(enclosure, '»')
  if (enclosure == '‘')   enclosure <- c(enclosure, '’')
  if (enclosure == '“')   enclosure <- c(enclosure, '”')
  paste0(enclosure[1], s, enclosure[length(enclosure)])
}

# Вместо отсутствующего значения/пустого множества (NULL)
# возвращает пропущенное значение (NA)
skip_null <- function(x) {
  x <- tryCatch(x, error = function(e) NULL)
  if (is.null(x)) return(NA) else return(x)
}

if_na <- function(x, alt) {
  if (is.na(x)) return(alt) else return(x)
}

######## Функции для работы с API HeadHunter ########
hh_set_query <- function(
  text,
  search_fields = 'name',
  salary = NULL,
  experience = NULL,
  area = 1,
  metro = NULL,
  only_with_salary = TRUE,
  currency = 'RUR',
  employment = 'full',
  schedule = 'fullDay',
  specialization = NULL,
  industry = NULL, # отрасль или отрасль.сфера
  date_from = NULL,
  date_to = if (is.null(date_from)) NULL else Sys.Date(),
  period =  if (!is.null(date_from)) NULL else 30,
  order_by = 'publication_time',
  vacancy_label = NULL,
  clusters = FALSE,
  page = 0,
  per_page = 100, # глубина возвращаемых результатов не может быть больше 2000
  describe_arguments = TRUE
) {
  query <- get_subset(as.list(environment()))
  return(query)
}

hh_modify_query <- function(
  .query,
  ...
) {
  modifications = list(...)
  for (m in seq_along(modifications)) {
    .query[names(modifications)[[m]]] <- modifications[[m]]
  }
  return(.query)
}

hh_get_query <- function(.query, .agens = agens) {
  require(httr)
  require(rvest)
  require(jsonlite)
  httr::content(
    GET(
      'https://api.hh.ru/vacancies',
      query = .query,
      hostname = 'api.hh.ru',
      user_agent(.agens),
      accept_json(),
      add_headers(
        'User-Agent' = .agens
      )
    )
  )
}

hh_vacancy_search <- function(
  query = NULL,
  sleep = 1,
  text = NULL,
  search_fields = 'name',
  salary = NULL,
  experience = NULL,
  area = 1,
  metro = NULL,
  only_with_salary = TRUE,
  currency = 'RUR',
  employment = 'full',
  schedule = 'fullDay',
  specialization = NULL,
  industry = NULL, # отрасль или отрасль.сфера
  date_from = NULL,
  date_to = if (is.null(date_from)) NULL else Sys.Date(),
  period = if (is.null(date_from)) NULL else 30,
  order_by = 'publication_time',
  vacancy_label = NULL,
  clusters = FALSE,
  page = 0,
  per_page = 100, # !глубина возвращаемых результатов не может быть больше 2000
  describe_arguments = TRUE
) {
  require(purrr)
  if (is.null(query)) {
    query <- get_subset(as.list(environment()), exclude = c('query', 'sleep'))
  }
  # ↓ Необходимо для добора вакансий, не входящих в первые 2000 найденных
  query <- hh_modify_query(query, order_by = 'publication_time')
  initial <- hh_get_query(query)
  found_count <- initial$found
  vacancies <- vector('integer', length = found_count)
  items_count <- length(initial$items)
  vacancies[1:items_count] <- map_chr(initial$items, 'id')
  pages <- 1:(ceiling(min(found_count, 2000) / query$per_page) - 1)
  for (p in pages) {
    Sys.sleep(sleep)
    res <- hh_modify_query(query, page = p, clusters = FALSE) %>%
      hh_get_query()
    caught_count <- items_count + length(res$items)
    try(vacancies[(items_count + 1):caught_count] <- map_chr(res$items, 'id'))
    items_count <- caught_count
    to <- res$items[[length(res$items)]]$published_at
  }
  while (items_count < found_count) {
    query <- hh_modify_query(query, date_to = to)
    add <- hh_get_query(query)
    caught_count <- items_count + length(add$items)
    try(vacancies[(items_count + 1):caught_count] <- map_chr(add$items, 'id'))
    items_count <- caught_count
    pages <- 1:(ceiling(min(add$found, 2000) / query$per_page) - 1)
    for (p in pages) {
      Sys.sleep(sleep)
      res <- hh_modify_query(query, page = p, clusters = FALSE) %>%
        hh_get_query()
      caught_count <- items_count + length(res$items)
      try(vacancies[(items_count + 1):caught_count] <- map_chr(res$items, 'id'))
      items_count <- caught_count
      to <- res$items[[length(res$items)]]$published_at
    }
  }
  return(unique(vacancies))
}

hh_get_vacancy <- function(
  vid,
  sleep = 1
) {
  require(httr)
  require(rvest)
  require(jsonlite)
  Sys.sleep(runif(1, sleep - sleep*.2, sleep + sleep*.4))
  v <- GET(
    paste0(start_page, 'vacancies/', vid),
    hostname = 'api.hh.ru',
    user_agent(agens),
    accept_json(),
    add_headers(
      'User-Agent' = agens
    )
  ) %>%
    httr::content()
  return (v) # Temporary debug
  #   unlist(recursive = F) %>%
  #   get_subset() %>%
  #   unlist(recursive = F) %>%
  #   get_subset() %>%
  #   unlist(recursive = F) %>%
  #   as_tibble()
  # if (!preprocess) return(v)
  # v %>%
  #   select(
  #     # contains <- на случай, если в выгрузке такой колонки нет, иначе ошибка
  #     -contains('premium'),
  #     -contains('type.name'),
  #     -contains('billing_type'),
  #     -starts_with('contacts'),
  #     -ends_with('.id'),
  #     -ends_with('url'),
  #     -matches('\\d$'),
  #     -ends_with('_id'),
  #     -contains('stations'),
  #     -contains('allow_messages'),
  #     -contains('site.name'),
  #     -contains('archived'),
  #     -contains('hidden'),
  #     -contains('quick_responses_allowed'),
  #     -contains('accept_incomplete_resumes')
  #   ) %>%
  #   # Зарплата и описания — в последнюю очередь
  #   unite(
  #     'key_skills',
  #     starts_with('key_skills'),
  #     sep = '::',
  #     na.rm = TRUE
  #   ) %>%
  #   unite(
  #     'specializations',
  #     matches('^specializations\\d*\\.name$'),
  #     sep = '::',
  #     na.rm = TRUE
  #   ) %>%
  #   unite(
  #     'specializations.profarea',
  #     ends_with('profarea_name'),
  #     sep = '::',
  #     na.rm = TRUE
  #   ) %>%
  #   return()
}

hh_dict_experience <- GET('https://api.hh.ru/dictionaries') %>%
  httr::content() %>%
  getElement('experience') %>%
  map_chr('name')

hh_dict_schedule <- GET('https://api.hh.ru/dictionaries') %>%
  httr::content() %>%
  getElement('schedule') %>%
  map_chr('name')

hh_dict_licenses <- GET('https://api.hh.ru/dictionaries') %>%
  httr::content() %>%
  getElement('driver_license_types') %>%
  map_chr('id')

hh_dict_education <- GET('https://api.hh.ru/dictionaries') %>%
  httr::content() %>%
  getElement('education_level') %>%
  map_chr('name')

hh_dict_employment <- GET('https://api.hh.ru/dictionaries') %>%
  httr::content() %>%
  getElement('employment') %>%
  map_chr('name')

hh_parse_vacancy <- function(v) {
  require(tibble)
  require(purrr)
  v <- tibble(
    id = v$id,
    name = v$name,
    site = skip_null(v$site$name),
    employer.name = skip_null(v$employer$name),
    employer.id = skip_null(v$employer$id),
    employer.trusted = skip_null(v$employer$trusted),
    employer.has_logo = !is.null(v$employer$logo_urls),
    area = skip_null(v$area$name),
    address_raw = skip_null(v$address$raw),
    address.street = skip_null(v$address$street),
    address.lat = skip_null(v$address$lat),
    address.lng = skip_null(v$address$lng),
    address.metro.station = if_na(
      skip_null(v$address$metro$station_name),
      skip_null(v$address$metro_stations[[1]]$station_name)
    ),
    address.metro.line = if_na(
      skip_null(v$address$metro$line_name),
      skip_null(v$address$metro_stations[[1]]$line_name)
    ),
    experience = factor(
      skip_null(v$experience$name),
      levels = hh_dict_experience,
      ordered = TRUE
    ),
    schedule = factor(
      skip_null(v$schedule$name),
      levels = hh_dict_schedule
    ),
    employment = factor(
      skip_null(v$employment$name),
      levels = hh_dict_employment
    ),
    department = skip_null(v$department$name),
    salary.from = skip_null(v$salary$from),
    salary.to = skip_null(v$salary$to),
    salary.currency = skip_null(v$salary$currency),
    salary.gross = skip_null(v$salary$gross),
    description = skip_null(v$description),
    description_branded = skip_null(v$branded_description),
    key_skills = skip_null(
      v$key_skills %>%
        map_chr('name') %>%
        paste(collapse = '::')
    ),
    accept_handicapped = v$accept_handicapped,
    accept_kids = v$accept_kids,
    specializations = skip_null(
      v$specializations %>%
        map('profarea_name') %>%
        map2_chr(map(v$specializations, 'name'), paste, sep = ': ') %>%
        paste(collapse = '::')
    ),
    driver_license_types = skip_null(
      v$driver_license_types %>%
        map_chr('id') %>%
        paste(collapse = '::')
    ),
    has_test = v$has_test,
    published_at = v$published_at,
    created_at = v$created_at
  )
}

https://github.com/hhru/api/blob/master/docs/employers.md#item