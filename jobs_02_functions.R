start_page <- 'https://api.hh.ru/'
agens <- 'SalaryRegression/1.0 (a.pawluczenko@gmail.com, https://github.com/Marwolaeth/otusRproject)'

# Функция конвертации валют
# © Max Ghenis
# https://stackoverflow.com/a/26694739
# Немного модифицирована с учетом названий объектов, возвращаемых getFX()
# Важно!!! рубль идентифицируется как RUB
.exchange_rates <- function(from, to, dt = Sys.Date() - 1) {
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
get_exchange_rates <- function(from, to, dates = Sys.Date() - 1) {
  map2_dbl(from, to, .exchange_rates, dt = dates)
}

# Аналог getElement() для выбора подмножества
# Подходит для включения в пайплайн %>%
get_subset <- function(x, .what = sapply(x, length) > 0, exclude = NULL) {
  if (!is.null(exclude)) x <- x[setdiff(names(x), exclude)]
  '['(x, .what)
}

# Функция для быстрого заключения строки в скобки/кавычки/и т.д.
str_enclose <- function(s, enclosure = c('(', ')')){
  if (enclosure[1] == '(')   enclosure <- c(enclosure, ')')
  if (enclosure[1] == '((')  enclosure <- c(enclosure, '))')
  if (enclosure[1] == '[')   enclosure <- c(enclosure, ']')
  if (enclosure[1] == '[[')  enclosure <- c(enclosure, ']]')
  if (enclosure[1] == '[[[') enclosure <- c(enclosure, ']]]')
  if (enclosure[1] == '{')   enclosure <- c(enclosure, '}')
  if (enclosure[1] == '{{')  enclosure <- c(enclosure, '}}')
  if (enclosure[1] == '<')   enclosure <- c(enclosure, '>')
  if (enclosure[1] == '<<')  enclosure <- c(enclosure, '>>')
  if (enclosure[1] == '>')   enclosure <- c(enclosure, '<')
  if (enclosure[1] == '«')   enclosure <- c(enclosure, '»')
  if (enclosure[1] == '‘')   enclosure <- c(enclosure, '’')
  if (enclosure[1] == '“')   enclosure <- c(enclosure, '”')
  paste0(enclosure[1], s, enclosure[length(enclosure)])
}

# Вместо отсутствующего значения/пустого множества (NULL)
# возвращает пропущенное значение (NA)
skip_null <- function(x) {
  x <- tryCatch(x, error = function(e) NULL)
  if (is.null(x) || length(x) == 0) return(NA) else return(x)
}

if_na <- function(x, alt) {
  if (all(is.na(x))) return(alt) else return(x)
}

######## Функции для работы с текстом ########
# Удаление из текста HTML-тегов
strip_html <- function(s) {
  require(rvest)
  html_text(read_html(paste0('<body>', s, '</body>')))
}

# Русские стоп-слова
stopwords_ru <- readLines('tools/stopwords_ru.txt', encoding = 'UTF-8')
# из разных источников
ru_stopwords <- Reduce(
  base::union,
  list(
    stopwords_ru,
    stopwords::data_stopwords_snowball$ru,
    stopwords::data_stopwords_stopwordsiso$ru
  )
)
rm(stopwords_ru)

specific_term_vars <- c(
  # 'level',
  'term',
  'p_term_level',
  'p_level_term',
  'p_term_global',
  'n_term_level',
  'n_term_global',
  't.value',
  'p.value'
)

# Стеммер (лемматизатор) от Яндекса
# © Филипп Управителев (http://r.psylab.info/blog/author/konhis), 2015
# © Яндекс, 2019
str_lemmatise <- function(x) {
  x <- enc2utf8(x)
  res <- system(
    'tools/mystem -cl -e cp1251',
    intern = TRUE,
    input = x
  )
  res <- gsub('[{}]', '', res)
  res <- gsub('(\\|[^ ]+)', '', res)
  res <- gsub('\\?', '', res)
  res <- gsub('\\s+', ' ', res)
  res
}

.lemmatise <- function(x) {
  require(stringr)
  paste(
    str_lemmatise(
      str_remove_punctuation(
        str_replace_all(x, '[\r\n]', ' ')
      )
    ),
    collapse = ' '
  )
}

str_remove_punctuation <- function(s) {
  require(stringr)
  str_replace_all(s, '[\\,\\-——«»\\";\\?!\\(\\)]|[\\.|:](?=\\s)', ' ') %>%
    str_squish()
}

str_lemmatise_all <- function(s) {
  sapply(s, .lemmatise, USE.NAMES = FALSE)
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
    # Время публикации самой ранней из найденных вакансий
    to <- res$items[[length(res$items)]]$published_at
  }
  while (items_count < found_count) {
  # Повторяем запрос и поиск снова и снова,
  # Пока не получим все доступные вакансии за период
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
  require(jsonlite)
  # Просто запрос вакансии к API по ID
  Sys.sleep(runif(1, sleep - sleep*.2, sleep + sleep*.4))
  GET(
    paste0(start_page, 'vacancies/', vid),
    hostname = 'api.hh.ru',
    user_agent(agens),
    accept_json(),
    add_headers(
      'User-Agent' = agens
    )
  ) %>%
    httr::content() %>%
    return()
}

# Списки значений переменных HH: названия категорий на русском
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
  # Парсинг результата запроса вакансии в таблицу данных
  # Намного надежнее, чем jsonlite::fromJSON()
  tibble(
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
    # Переменные, которые в вакансиях могут принимать сразу множество значений
    # Кодируем в одну текстовую строку с редким разделителем
    # Для последующего извлечения dummy-переменных
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
  ) %>%
    return()
}

hh_get_employer <- function(
  eid,
  sleep = 1
) {
  require(httr)
  require(jsonlite)
  Sys.sleep(runif(1, sleep - sleep*.2, sleep + sleep*.4))
  GET(
    paste0(start_page, 'employers/', eid),
    hostname = 'api.hh.ru',
    user_agent(agens),
    accept_json(),
    add_headers(
      'User-Agent' = agens
    )
  ) %>%
    httr::content() %>%
    return()
}

hh_parse_employer <- function(emp) {
  require(tibble)
  require(purrr)
  # Парсинг результата запроса по работодателю в таблицу данных
  # Лучше поддается автоматической обработке jsonlite::fromJSON()
  tibble(
    employer.id = skip_null(emp$id),
    employer.name = skip_null(emp$name),
    employer.type = skip_null(emp$type),
    employer.site_url = skip_null(emp$site_url),
    employer.area = skip_null(emp$area$name),
    employer.industries = skip_null(
      emp$industries %>%
        map_chr('name') %>%
        paste(collapse = '::')
    )
  ) %>%
    return()
}

# Поиск информации о работодателе в Викиданных
company_search_template <- '(compan)|(corporat)|(organ)|(recruit)|(enterpri)|(personnel)|(staff)|(employ)|(agency)|(govern)|(profit)|(banking)|(institu)|(university)|(pharma)|(concern)|(ventur)|(factory)|(manufactur)|(consult)|(human resou)|(industr)|(servi)|(commerc)|(develop)|(retail)|(aggregator)|(suppli)'

wikidata_parse_employer <- function(emp_name, site = NULL) {
  require(WikidataR)
  require(purrr)
  require(stringr)
  # Самый надежный результат — у поиска по сайту
  if (is.null(site)) {
    site <- NA
    if(is.na(site) & str_detect(emp_name, '\\.(ru)|(com)|(io)|(me)|(su)$')) {
      site <- emp_name
    } else if (is.na(site)) {
      site <- 0
    }
  } else if (is.na(site)) {
    site <- 0
  }
  emp_name_full <- emp_name
  emp_name <- tolower(emp_name)
  emp_name <- str_remove_all(
    emp_name,
    '(russia)|(cis)|(пао )|(зао )|(ооо )'
  ) %>%
    str_remove_all('\\s\\-\\s[а-я]+$')
  wiki <- find_item(emp_name, language = 'ru', limit = 21)
  if (length(wiki) == 0) {
    # Если результатов нет (что редко) — пробуем искать по сайту
    if (!is.null(site) & !is.na(site) & site != 0) {
      site <- str_remove(site, '^https*\\://(www\\.)*') %>%
        str_remove('/$')
      wiki <- find_item(site, language = 'ru')
    }
  }
  # Названия и описания найденных сущностей
  labelss <- wiki %>%
    map_chr(
      ~ skip_null(getElement(., 'label'))
    )
  descriptions <- wiki %>%
    map_chr(
      ~ skip_null(getElement(., 'description'))
    ) %>%
    tolower()
  if (any(tolower(labelss) == tolower(site))) {
    # Если одно из названий точно соответствует сайту
    eid <- wiki %>%
      getElement(which(tolower(labelss) == tolower(site))[1]) %>%
      getElement('id')
  } else if (any(labelss == emp_name_full & is.na(descriptions))) {
    # Если одно из названий точно соответствует названию работодателя
    eid <- wiki %>%
      getElement(which(labelss == emp_name_full & is.na(descriptions))[1]) %>%
      getElement('id')
  } else {
    # Иначе — ищем самое релевантное описание,
    # Указывающее на компанию (организацию)
    descriptions <- paste(tolower(labelss), descriptions)
    guess <- str_detect(descriptions, company_search_template)
    if (is.null(guess)) return(NULL)
    guess[is.na(guess)] <- FALSE
    if (!any(guess, na.rm = TRUE)) {
      if (!is.null(site) & !is.na(site) & site != 0) {
        # Если подходящих описаний нет, проводим поиск по домену
        # Повторяем алгоритм поиска выше
        site <- str_remove(site, '^https*\\://(www\\.)*') %>%
          str_remove('/$')
        wiki <- find_item(site, language = 'ru')
        labelss <- wiki %>%
          map_chr(
            ~ skip_null(getElement(., 'label'))
          )
        descriptions <- wiki %>%
          map_chr(
            ~ skip_null(getElement(., 'description'))
          ) %>%
          tolower()
        if (any(tolower(labelss) == tolower(site))) {
          eid <- wiki %>%
            getElement(which(tolower(labelss) == tolower(site))[1]) %>%
            getElement('id')
        } else if (any(labelss == emp_name_full & is.na(descriptions))) {
          eid <- wiki %>%
            getElement(which(labelss == emp_name_full & is.na(descriptions))[1]) %>%
            getElement('id')
        } else {
          descriptions <- paste(tolower(labelss), descriptions)
          guess <- str_detect(descriptions, company_search_template)
          # Если и поиск по сайту не помог
          if (is.null(guess)) return(NULL)
          # Предпочитаем среди найденных российские организации
          guess_rus <- guess & str_detect(descriptions, 'russ')
          if (any(guess_rus)) {
            guess_rus <- min(which(guess_rus))
          } else {
            guess_rus <- 0
          }
          best_guess <- max(min(which(guess)), guess_rus)
          # Считаем, что нашли подходящую организацию
          eid <- wiki %>%
            # get_subset(guess) %>%
            getElement(best_guess) %>%
            getElement('id')
        }
      } else {
        return(NULL)
      }
    } else {
      # Предпочитаем среди найденных российские организации
      guess_rus <- guess & str_detect(descriptions, 'russ')
      if (any(guess_rus)) guess_rus <- min(which(guess_rus)) else guess_rus <- 0
      best_guess <- max(min(which(guess)), guess_rus)
      # Считаем, что нашли подходящую организацию
      eid <- wiki %>%
        # get_subset(guess) %>%
        getElement(best_guess) %>%
        getElement('id') 
    }
  }
  # Парсинг характеристик найденной организации
  # Очень много уровней вложенности!
  # Используются коды отношений Wikidata
  # Например, P17 это «Суверенное государство этого элемента»
  # А P571 — «Дата основания/создания/возникновения»
  wiki <- get_item(eid) %>% getElement(1)
  tibble(
    employer.name = emp_name_full,
    employer.founded = skip_null(
      as.integer(
        str_extract(
          wiki$claims$P571$mainsnak$datavalue$value$time,
          '\\+*\\d{4}'
        )
      )
    ),
    employer.country = skip_null(
      get_item(wiki$claims$P159$mainsnak$datavalue$value$id) %>%
        getElement(1) %>%
        getElement('claims') %>%
        getElement('P17') %>%
        getElement('mainsnak') %>%
        getElement('datavalue') %>%
        getElement('value') %>%
        getElement('id') %>%
        get_item() %>%
        getElement(1) %>%
        getElement('labels') %>%
        getElement('ru') %>%
        getElement('value')
    ),
    employer.headquarters = skip_null(
      get_item(wiki$claims$P159$mainsnak$datavalue$value$id) %>%
        getElement(1) %>%
        getElement('labels') %>%
        getElement('ru') %>%
        getElement('value')
    ),
    employer.wiki_site = skip_null(
      wiki$claims$P856$mainsnak$datavalue$value[[1]]
    ),
    employer.legal_form = skip_null(
      get_item(wiki$claims$P1454$mainsnak$datavalue$value$id) %>%
        getElement(1) %>%
        getElement('labels') %>%
        getElement('ru') %>%
        getElement('value')
    ),
    employer.wiki_industry = skip_null(
      get_item(wiki$claims$P452$mainsnak$datavalue$value$id) %>%
        getElement(1) %>%
        getElement('labels') %>%
        getElement('ru') %>%
        getElement('value')
    ),
    employer.no_employees = skip_null(
      skip_null(as.numeric(wiki$claims$P1128$mainsnak$datavalue$value$amount)) *
        if_na(
          skip_null(
            as.numeric(wiki$claims$P1128$mainsnak$datavalue$value$unit)
          ),
          1
        )
    ),
    employer.social_vk = skip_null(
      !is.null(wiki$claims$P3185)
    ),
    employer.social_fb = skip_null(
      !is.null(wiki$claims$P2013)
    ),
    employer.social_tw = skip_null(
      !is.null(wiki$claims$P2002)
    ),
    employer.social_ig = skip_null(
      !is.null(wiki$claims$P2003)
    ),
    employer.social_in = skip_null(
      !is.null(wiki$claims$P4264)
    )
  ) %>%
    slice(1) %>%
    return()
}

######## Функции для потокового моделирования ########
str_from_formula <- function(f) {
  stopifnot(is_formula(f))
  Reduce(paste, deparse(f))
}

as.Matrix <- function(dtm) {
  # Преобразование матрицы класса DocumentTermMatrix в объект Matrix
  stopifnot(inherits(dtm, 'simple_triplet_matrix'))
  require(Matrix)
  sparseMatrix(
    i = dtm$i,
    j = dtm$j,
    x = dtm$v,
    dims = c(dtm$nrow, dtm$ncol),
    dimnames = dtm$dimnames,
    symmetric = FALSE,
    triangular = FALSE,
    index1 = TRUE,
    check = TRUE
  )
}

salary_glm_sparse <- function(
  # Лассо-регрессия: отбраковка терминов с незначимыми коэффициентами
  .job,
  .df = df,
  dtm = dtm_full,
  dict = dict_features,
  nfold = NULL,
  lambda_range = NULL,
  lambda_n = 50
) {
  require(dplyr)
  require(broom)
  .df <- .df %>%
    filter(job == .job) %>%
    select(-description, -key_skills, -specializations, -employer.industries)
  
  job_terms <- dict %>%
    filter(fname != '<missing>', is.na(job) | job == .job) %>%
    pull(fname)
  job_ids <- .df$id
  y <- .df$salary
  
  # Количество корзин для кроссвалидации
  # Для данных с малым количеством наблюдений
  if (is.null(nfold)) {
    if (length(y) < 250) {
      min(nfold <- round(length(y) * .05), 5)
    } else {
      nfold <- 30
    }
  }
  
  X <- as.Matrix(dtm[job_ids, unique(job_terms)])
  
  # Проверка соответствия строк разреженной матрицы и исходного набора данных
  stopifnot(all(rownames(X) == .df$id))
  
  # Экспоненциальный спуск при переборе значений лямбды
  if (!is.null(lambda_range)) {
    lambda_seq <- exp(
      seq(
        log(lambda_range[1]),
        log(lambda_range[2]),
        length.out = lambda_n
      )
    )
  } else {
    lambda_seq = NULL
  }
  
  require(glmnet)
  fit <- cv.glmnet(
    X,
    y,
    family = 'gaussian',
    standardize = FALSE,
    type.gaussian = 'naive',
    type.measure = 'mse',
    nfolds = nfold,
    alignment = 'fraction',
    lambda = lambda_seq,
    keep = FALSE,
    trace.it = 1
  )
  coefs <- suppressWarnings(
    broom::tidy(coef(fit, s = 'lambda.min'))
  ) %>%
    as_tibble() %>%
    rename(beta_hat_lmin = value) %>%
    left_join(
      suppressWarnings(broom::tidy(coef(fit, s = 'lambda.1se'))) %>%
        rename(beta_hat_l1se = value)
    ) %>%
    mutate(beta_hat_l1se = replace_na(beta_hat_l1se, 0)) %>%
    arrange(beta_hat_l1se, desc(beta_hat_lmin))
  
  # Метрики качества модели
  fit_predict <- tibble(
    salary = y,
    predicted = as.numeric(predict(fit, X, s = 'lambda.min')),
    error = predicted - salary
  ) %>%
    summarise(
      lambda = fit$lambda.min,
      MAE = mean(abs(error)),
      median_abs_error = median(abs(error)),
      RMSE = sqrt(mean(error^2))
    ) %>%
    mutate(response = 'As Is') %>%
    select(response, everything())
  
  # Регрессия на логарифмированное значение зависимой переменной
  fit_log <- cv.glmnet(
    X,
    log(y),
    family = 'gaussian',
    standardize = FALSE,
    type.gaussian = 'naive',
    type.measure = 'mse',
    nfolds = nfold,
    alignment = 'fraction',
    lambda = lambda_seq,
    keep = FALSE,
    trace.it = 1
  )
  coefs_log <- suppressWarnings(
    broom::tidy(coef(fit_log, s = 'lambda.min'))
  ) %>%
    as_tibble() %>%
    rename(beta_hat_lmin = value) %>%
    full_join(
      suppressWarnings(broom::tidy(coef(fit_log, s = 'lambda.1se'))) %>%
        rename(beta_hat_l1se = value)
    ) %>%
    mutate(beta_hat_l1se = replace_na(beta_hat_l1se, 0)) %>%
    arrange(beta_hat_l1se, desc(beta_hat_lmin)) %>%
    mutate_at(vars(starts_with('beta')), ~ . * 100)
  
  fit_predict_log <- tibble(
    salary = y,
    log_salary = log(y),
    log_predicted = as.numeric(predict(fit_log, X, s = 'lambda.min')),
    predicted = exp(log_predicted),
    error = predicted - salary
  ) %>%
    summarise(
      lambda = fit_log$lambda.min,
      MAE = mean(abs(error)),
      median_abs_error = median(abs(error)),
      RMSE = sqrt(mean(error^2))
    ) %>%
    mutate(response = 'Log') %>%
    select(response, everything())
  fit_predict <- fit_predict %>%
    bind_rows(fit_predict_log) %>%
    mutate(response = factor(response, levels = c('As Is', 'Log')))
  rm(fit_predict_log)
  
  chosen_features <- dict %>%
    # filter((fname %in% coefs_log$row) & job == .job) %>%
    # filter(job == .job) %>%
    full_join(select(coefs, fname = row, beta_hat = beta_hat_lmin)) %>%
    full_join(select(coefs_log, fname = row, beta_hat_log = beta_hat_lmin)) %>%
    filter(fname != '(Intercept)', !is.na(fname)) %>%
    mutate_at(vars(starts_with('beta')), replace_na, 0) %>%
    # filter(beta_hat != 0 & beta_hat_log != 0) %>%
    # filter(beta_hat != 0) %>%
    # Термины, для которых значим любой из коэффициентов
    # (при исходном или логарифмированном значении зависимой переменной)
    filter(beta_hat != 0 | beta_hat_log != 0) %>%
    arrange(desc(abs(beta_hat))) %>%
    distinct(fname, .keep_all = TRUE)
  
  X <- X[, chosen_features$fname]

  require(tibble)
  dataframe <- X %>%
    as.matrix() %>%
    as_tibble(rownames = NA) %>%
    rownames_to_column('id') %>%
    set_names(c('id', chosen_features$fid)) %>%
    left_join(.df) %>%
    select(setdiff(names(.df), 'salary'), chosen_features$fid, salary)
  
  return(
    list(
      model = fit,
      X = X,
      dataframe = dataframe,
      accuracy = fit_predict,
      # coefs,
      # coefs_log,
      features = chosen_features
    )
  )
  # Аминь
}

salary_glm_full <- function(
  # Fision Lasso регрессия
  # Учитывает значимость не только отдельных переменных,
  # Но и уровней категориальных переменных
  d,
  lmax = 1000,
  lmin = 0.001,
  # Кросс-валидация невозможна из-за отсутствия некоторых уровней переменных
  # По умолчанию используется критерий Bayesian Information Criterion
  ic   = 'is.bic',
  pen.text      = FALSE, # Нужно ли использовать регуляризацию для терминов
  trim.outliers = TRUE,  # Нужно ли обрабатывать выбросы
  trim.levels   = TRUE,  # Объединять ли редкие классы в «Другое»
  use.pen.weights = TRUE
) {
  require(dplyr)
  require(broom)
  require(stringr)
  require(smurf)
  
  d <- d %>%
    # Добавление фиктивного наблюдения с редкими значениями классов
    # Костыль для обхода ошибки
    bind_rows(
      slice(d, sample(nrow(d), 1)) %>%
        mutate(
          employer.type = '<missing>',
          salary = mean(d$salary),
          description_language = 'English',
          address.metro.line = 'Другая'
        )
    ) %>%
    mutate(employer.type = as.factor(employer.type)) %>%
    mutate(employer.has_logo = as.factor(employer.has_logo)) %>%
    mutate(description_language = as.factor(description_language)) %>%
    mutate_if(is.factor, droplevels)
  
  cat(str_pad(unique(d$job), width = 40, side = 'both', pad = '-'), '\n')
  
  if (trim.outliers) {
    d <- d %>%
      mutate(
        salary = imputate_outlier(d, salary, method = 'capping', no_attrs = TRUE)
      )
  }
  
  if (trim.levels) {
    d <- d %>%
      mutate(
        address.metro.line = fct_lump(
          address.metro.line,
          prop = 0.01,
          other_level = 'Другая'
        )
      )
    reflevel = 'Другая'
  } else {
    reflevel = '<missing>'
  }
  
  # d <- d %>% 
  #   mutate(
  #     address.metro.line = C(address.metro.line, sum),
  #     employer.type = C(employer.type, sum)
  #   )
  feature_vars <- grep('_\\d+$', names(d), value = TRUE)
  
  formu <- salary ~
    p(experience, pen = 'flasso') + p(employer.has_logo, pen = 'lasso') +
    p(employer.type, pen = 'gflasso', refcat = '<missing>') +
    # p(address.metro.station, pen = 'gflasso', refcat = '<missing>') +
    # p(address.metro.line, pen = 'gflasso', refcat = reflevel) +
    p(log(description_length), pen = 'lasso') +
    p(description_sentiment, pen = 'lasso') +
    p(description_language, pen = 'lasso')
  
  if (pen.text) {
    formu_features <- paste(
      str_enclose(feature_vars, c('p(', ')')),
      collapse = ' + '
    )
  } else {
    formu_features <- paste(
      feature_vars,
      collapse = ' + '
    )
  }
  
  if (length(feature_vars) > 0) {
    formu <- formu %>%
      str_from_formula() %>%
      paste(
        formu_features,
        sep = ' + '
      ) %>%
      as.formula()
  }
  
  fit <- glmsmurf(
    formu,
    family = gaussian(),
    data = d,
    lambda = ic,
    control = list(lambda.max = lmax, lambda.min = lmin, print = TRUE)
  )
  coefs <- suppressWarnings(
    broom::tidy(coef_reest(fit))
  ) %>%
    as_tibble() %>%
    rename(beta_hat = x, fid = names)
  
  # Для расчета метрик качества
  n <- nrow(d)
  p <- length(setdiff(names(d), c('id', 'job')))
  
  fit_predict <- tibble(
    salary = d$salary,
    predicted = fitted_reest(fit),
    error = residuals_reest(fit)
  ) %>%
    summarise(
      y_hat = 'As is',
      n = n - 1,
      lambda = fit$lambda,
      MAE = mean(abs(error)),
      median_abs_error = median(abs(error)),
      RMSE = sqrt(mean(error^2)),
      # R_sq = cor(salary, predicted)^2,
      R_sq = 1 - sum(error^2) / sum((salary - mean(salary))^2),
      R_sq.adj = 1 - ((1 - R_sq) * (n - 1)) / (n - p - 1)
    )
  
  # formu <- formu %>%
  #   str_from_formula() %>%
  #   str_replace('salary', 'log(salary)') %>%
  #   as.formula()
  # 
  # fit_log <- glmsmurf(
  #   formu,
  #   family = gaussian(),
  #   data = d,
  #   lambda = ic,
  #   control = list(
  #     lambda.max = 10000,
  #     lambda.min = 0.001,
  #     print = TRUE)
  # )
 
  coefs <- coefs %>%
    # full_join(
    #   suppressWarnings(
    #     broom::tidy(coef_reest(fit_log))
    #   ) %>%
    #     as_tibble() %>%
    #     rename(beta_hat_log = x, fid = names)
    # ) %>%
    left_join(filter(dict_features, is.na(job) | job == unique(d$job))) %>%
    mutate(job = unique(d$job)) %>%
    mutate(
      fname = case_when(
        !is.na(fname) ~ fname,
        fid == 'Intercept' ~ 'Пересечение',
        str_detect(fid, '^exp') ~ str_remove(fid, '^experience'),
        str_detect(fid, '^employer.has_logo') ~
          str_replace(fid, '^employer.has_logo', 'Логотип работодателя: '),
        str_detect(fid, '^employer.type') ~
          str_replace(fid, '^.+<missing>', 'Тип работодателя: '),
        str_detect(fid, 'metro') ~ str_remove(fid, '^.+<missing>'),
        str_detect(fid, 'length') ~ 'Логарифм длины описания',
        str_detect(fid, 'Engl') ~ 'Описание на английском',
        str_detect(fid, 'Русск') ~ 'Описание на русском',
        fid == 'description_sentiment' ~ 'Тональность описания'
      )
    ) %>%
    mutate(
      ftype = case_when(
        !is.na(ftype) ~ ftype,
        fid == 'Intercept' ~ 'Постоянный член',
        str_detect(fid, '^exp') ~ 'Опыт работы',
        str_detect(fid, '^emp') ~ 'Информация о работодателе',
        str_detect(fid, 'metro.line') ~ 'Линия метро',
        str_detect(fid, 'metro.station') ~ 'Станция метро',
        str_detect(fid, 'descript') ~ 'Свойства описания'
      )
    ) %>%
    select(fid, fname, ftype, beta_hat, odds_job)
  
  # fit_predict_log <- tibble(
  #   salary = d$salary,
  #   log_salary = log(d$salary),
  #   log_predicted = fitted_reest(fit_log),
  #   predicted = exp(log_predicted),
  #   error = predicted - salary
  # ) %>%
  #   summarise(
  #     n = n - 1,
  #     lambda = fit_log$lambda,
  #     MAE = mean(abs(error)),
  #     median_abs_error = median(abs(error)),
  #     RMSE = sqrt(mean(error^2)),
  #     # R_sq = cor(salary, predicted)^2,
  #     R_sq = 1 - sum(error^2) / sum((salary - mean(salary))^2),
  #     R_sq.adj = 1 - ((1 - R_sq) * (n - 1)) / (n - p - 1)
  #   ) %>%
  #   mutate(response = 'Log') %>%
  #   select(response, everything())
  
  # оценка качества с округлением предсказанных значений
  fit_predict_round <- tibble(
    salary = d$salary,
    predicted = round(fitted_reest(fit), -3),
    error = predicted - salary
  ) %>%
    summarise(
      y_hat = 'Rounded',
      n = n - 1,
      lambda = fit$lambda,
      MAE = mean(abs(error)),
      median_abs_error = median(abs(error)),
      RMSE = sqrt(mean(error^2)),
      # R_sq = cor(salary, predicted)^2,
      R_sq = 1 - sum(error^2) / sum((salary - mean(salary))^2),
      R_sq.adj = 1 - ((1 - R_sq) * (n - 1)) / (n - p - 1)
    )
  fit_predict <- bind_rows(fit_predict, fit_predict_round)
 
  return(
    list(
      model = fit,
      coefficients = coefs,
      accuracy = fit_predict
    )
  )
}

salary_lm_stepwise <- function(
  # Обычная (Ordinary Least Squares) регрессия
  # С пошаговым подбором значимы переменных
  d,
  .pent = .9, # Переменные с p-значением выше данного не рассматриваются
  .prem = .1, # Верхний порог p-значения для включения в окончательную модель
  .details = TRUE,
  contrast.ordinal = 'poly', # Контрасты для категориальных переменных
  contrast.nominal = 'sum',
  trim.outliers    = TRUE,
  trim.levels      = TRUE,
  conf.level       = 1 - .prem,
  save = FALSE
) {
  require(dplyr)
  require(forcats)
  require(dlookr)
  require(broom)
  require(stringr)
  require(car)
  require(olsrr)
  
  .job = unique(d$job)
  cat(str_pad(.job, width = 40, side = 'both', pad = '-'), '\n')
  
  d <- d %>%
    # Добавление фиктивного наблюдения с редкими значениями классов
    # Костыль для обхода ошибки
    bind_rows(
      slice(d, sample(nrow(d), 1)) %>%
        mutate(
          employer.type = '<missing>',
          salary = mean(d$salary[d$experience == experience]),
          description_language = 'English',
          address.metro.line = 'Сокольническая',
          address.metro.station = 'Университет'
        )
    ) %>%
    mutate(employer.type = as.factor(employer.type)) %>%
    mutate(employer.has_logo = as.factor(employer.has_logo)) %>%
    mutate(description_language = as.factor(description_language)) %>%
    mutate_if(is.factor, droplevels)
  
  d <- d %>% select(-id, -job)
  
  if (trim.outliers) {
    d <- d %>%
      mutate(
        salary = imputate_outlier(d, salary, method = 'capping', no_attrs = TRUE)
      )
  }
  
  if (trim.levels) {
    d <- d %>%
      mutate(
        address.metro.line = fct_lump(
          address.metro.line,
          prop = 0.01,
          other_level = 'Другая'
        )
      ) %>%
      mutate(
        address.metro.line = fct_relevel(
          address.metro.line,
          'Другая',
          after = 0
        )
      ) %>%
      mutate(
        address.metro.station = fct_lump(
          address.metro.station,
          prop = 0.01,
          other_level = 'Другая'
        )
      ) %>%
      mutate(
        address.metro.station = fct_relevel(
          address.metro.station,
          'Другая',
          after = 0
        )
      )
  } else {
    d <- d %>%
      mutate(
        address.metro.line    = fct_relevel(
          address.metro.line,
          '<missing>',
          after = 0
        ),
        address.metro.station = fct_relevel(
          address.metro.station,
          '<missing>',
          after = 0
        )
      )
  }
  
  nominal <- paste('contr', contrast.nominal, sep = '.')
  ordinal <- paste('contr', contrast.ordinal, sep = '.')
  contrast.nominal <- match.fun(nominal)
  contrast.ordinal <- match.fun(ordinal)
  
  d <- d %>%
    mutate(
      address.metro.line = C(fct_drop(address.metro.line), contrast.nominal),
      address.metro.station = C(fct_drop(address.metro.station), contrast.nominal),
      employer.type = C(employer.type, contrast.nominal),
      description_length = log(description_length),
      experience = C(fct_drop(experience), contrast.ordinal)
    )
  
  employer_types <- c(
    'Кадровое агентство'   = 'agency',
    'Компания'             = 'company',
    'Частный рекрутер'     = 'private_recruiter',
    'Руководитель проекта' = 'project_director',
    '<missing>'            = '<missing>'
  ) %>%
    intersect(levels(d$employer.type)) %>%
    names()
  metro_lines    <- levels(d$address.metro.line)
  metro_stations <- levels(d$address.metro.station)
  experiences    <- levels(d$experience)
  cat('Data prepared!\n\n')
  
  # Полная мультиколлинеарность!
  fit <- lm(salary ~ ., data = d)
  als <- alias(fit)[['Complete']]
  
  while (!is.null(als)) {
    als <- als[,colSums(als) > 0] %>% colnames()
    als <- ifelse(
      str_detect(als, '\\d$') & !str_detect(als, '\\_'),
      str_remove(als, '\\d+$'),
      als
    ) %>%
      unique()
    cat(
      paste(
        'Aliases:',
        paste(als, collapse = '; ')
      )
    )
    d <- d[, setdiff(names(d), als)]
    
    fit <- lm(salary ~ ., data = d)
    als <- alias(fit)[['Complete']]
  }
  cat('Aliases removed!\n\n')
  
  # Костыль
  # Такой косяк ols_step_both_p(): ищет данные в .GlobalEnv
  d <<- d
  
  fit_stepwise <- ols_step_both_p(
    fit,
    pent = .pent,
    prem = .prem,
    details = .details,
    data = d
  )
  
  rm(d, envir = .GlobalEnv)
  
  coefs <- fit_stepwise$model %>%
    tidy(conf.int = TRUE, conf.level = conf.level) %>%
    rename(beta_hat = estimate, fid = term)
  
  fit_predict <- tibble(
    error = residuals(fit_stepwise$model)
  ) %>%
    summarise(
      model = 'Parsimonious',
      n   = n(),
      n_predictors = length(coef(fit_stepwise$model)),
      MAE = mean(abs(error)),
      MedAE = median(abs(error)),
      RMSE = sqrt(mean(error^2)),
      R_sq     = summary(fit_stepwise$model)$r.squared,
      R_sq.adj = summary(fit_stepwise$model)$adj.r.squared
    )
  
  fit_initial <- tibble(
    error = residuals(fit)
  ) %>%
    summarise(
      model = 'Initial',
      n   = n(),
      n_predictors = length(coef(fit)),
      MAE = mean(abs(error)),
      MedAE = median(abs(error)),
      RMSE  = sqrt(mean(error^2)),
      R_sq     = summary(fit)$r.squared,
      R_sq.adj = summary(fit)$adj.r.squared
    )
  
  fit_predict <- bind_rows(fit_initial, fit_predict)
  
    coefs <- coefs %>%
    left_join(filter(dict_features, is.na(job) | job == unique(.job))) %>%
    mutate(job = unique(.job)) %>%
    mutate(
      fname = case_when(
        !is.na(fname) ~ fname,
        fid == '(Intercept)' ~ 'Пересечение',
        str_detect(fid, '^exp') & !str_detect(fid, '([A-Z]|\\d)$') ~
          str_remove(fid, '^experience'),
        str_detect(fid, '^exp') & str_detect(fid, '\\d$') ~
          map(
            str_extract(coefs$fid, '\\d+$') %>%
              as.numeric(),
            ~ experiences[.]
          ) %>%
          map(skip_null) %>%
          reduce(c),
        str_detect(fid, '^exp') & str_detect(fid, 'L$') ~
          'Опыт работы: линейная функция',
        str_detect(fid, '^exp') & str_detect(fid, 'Q$') ~
          'Опыт работы: квадратичная функция',
        str_detect(fid, '^exp') & str_detect(fid, 'C$') ~
          'Опыт работы: кубическая функция',
        # str_detect(fid, '^exp') & str_detect(fid, '[A-Z]$') ~
        #   paste(
        #     'Опыт работы:',
        #     switch(
        #       str_extract(fid, '[A-Z]$'),
        #       L = 'Линейная',
        #       Q = 'Квадратичная',
        #       C = 'Кубическая'
        #     )
        #   ),
        str_detect(fid, '^employer.has_logo') ~
          str_replace(fid, '^employer.has_logo', 'Логотип работодателя: '),
        # str_detect(fid, '^employer.type') & str_detect(fid, '\\d$') ~
        #   paste(
        #     'Тип работодателя:',
        #     levels(d$employer.type)[as.numeric(str_extract(fid, '\\d$'))]
        #   ),
        str_detect(fid, '^employer.type') & str_detect(fid, '\\d$') ~
          paste(
            'Тип работодателя:',
            map(
              str_extract(coefs$fid, '\\d+$') %>%
                as.numeric(),
              ~ employer_types[.]
            ) %>%
              map(skip_null) %>%
              reduce(c)
          ),
        str_detect(fid, '^employer.type') & !str_detect(fid, '\\d$') ~
          paste(
            'Тип работодателя:',
            str_remove(fid, 'employer.type')
          ),
        str_detect(fid, 'metro.line') & str_detect(fid, '\\d$') ~
          paste(
            'Линия метро:',
            map(
              str_extract(coefs$fid, '\\d+$') %>%
                as.numeric(),
              ~ metro_lines[.]
            ) %>%
              map(skip_null) %>%
              reduce(c)
          ),
        str_detect(fid, 'metro.line') & !str_detect(fid, '\\d$') ~
          paste(
            'Линия метро:',
            str_remove(fid, 'address.metro.line')
          ),
        str_detect(fid, 'metro.station') & str_detect(fid, '\\d$') ~
          paste(
            'Станция метро:',
            map(
              str_extract(coefs$fid, '\\d+$') %>%
                as.numeric(),
              ~ metro_stations[.]
            ) %>%
              map(skip_null) %>%
              reduce(c)
          ),
        str_detect(fid, 'metro.station') & !str_detect(fid, '\\d$') ~
          paste(
            'Станция метро:',
            str_remove(fid, 'address.metro.station')
          ),
        str_detect(fid, 'length') ~ 'Логарифм длины описания',
        str_detect(fid, 'Engl')   ~ 'Описание на английском',
        str_detect(fid, 'Русск')  ~ 'Описание на русском',
        fid == 'description_sentiment' ~ 'Тональность описания'
      )
    ) %>%
    mutate(
      ftype = case_when(
        !is.na(ftype) ~ ftype,
        fid == '(Intercept)' ~ 'Постоянный член',
        str_detect(fid, '^exp') ~ 'Опыт работы',
        str_detect(fid, '^emp') ~ 'Информация о работодателе',
        str_detect(fid, 'metro') ~ 'Метро',
        str_detect(fid, 'descript') ~ 'Свойства описания'
      )
    ) %>%
    select(
      fid, fname, ftype, beta_hat, p.value, conf.low, conf.high, odds_job, job
    )
    attr(coefs, 'conf.level') <- conf.level
  
    res <- list(
      job_name = .job,
      model = fit_stepwise$model,
      contrasts = list(
        nominal = nominal,
        ordinal = ordinal
      ),
      vif = vif(fit_stepwise$model),
      coefficients = coefs,
      accuracy = fit_predict
    )
    if (save) {
      save(
        res,
        file = file.path(
          'data',
          'models',
          paste0(
            'model',
            '-',
            round(as.numeric(Sys.time())),
            '.RData'
          )
        )
      )
    }
    return(res)
}

############ ОФОРМЛЕНИЕ ############
# Бутстреп — доверительный интервал для медианы (для отображения на диаграмме)
# Рассчитывается очень долго
median_cl_boot <- function(x, conf.level = .95, na.rm = TRUE, nsim = 100000) {
  y <- replicate(nsim, median(sample(x, replace = TRUE), na.rm = na.rm))
  ymin = quantile(y, (1 - conf.level) / 2)
  ymax = quantile(y, 1 - (1 - conf.level) / 2)
  y    = median(y)
  return(data.frame(y, ymin, ymax))
}

ftype_colours <- c(
  'Специализация'     = '#0077BB',
  'Навык'             = '#CC3311',
  'Опыт работы'       = '#BBBBBB',
  'Отрасль'           = '#009988',
  'Свойства описания' = '#EE3377',
  'Ключевое слово'    = '#33BBEE',
  'Метро'             = '#E782F6',
  'Информация о работодателе' = '#EE7733'
)

experience_colours <- c(
  'Нет опыта'          = '#c994c7',
  'От 1 года до 3 лет' = '#e7298a',
  'От 3 до 6 лет'      = '#980043',
  'Более 6 лет'        = '#67001f'
)

plot_specific_features <- function(
  # Диаграмма специфичных для профессии терминов
  # (слов из описания, навыков и специализаций)
  # Громоздкая и плохо продуманная формула
  group,
  category,
  colour,
  fvar = 'value',
  fvar.caption = fvar,
  l,
  # separator = str_pad('#', 20, pad = '#'),
  sleep = 5
) {
  d <- getElement(l, group) %>%
    getElement(category) %>%
    rename_(., fvar = 'fvar')
  
  p <- ggplot(
    d,
    aes(x = fname, y = fvar)
  ) +
    geom_col(show.legend = FALSE, fill = colour) +
    scale_x_discrete(category) +
    geom_text(
      aes(
        label = format(round(fvar, 2), decimal.mark = ','),
        y = min(fvar) * .5
      ),
      colour = 'white',
      fontface = 'bold',
      hjust = 1
    ) +
    scale_y_sqrt(fvar.caption) +
    coord_flip() +
    # facet_grid(. ~ ftype, scales = 'free') +
    ggtitle(sprintf('%s, %s, ТОП-%d:', group, tolower(category), nrow(d))) +
    theme_minimal()
  # print(p)
  Sys.sleep(sleep)
  return(p)
}

plot_salary_coefficients <- function(
  # Визуализация значимых коэффициентов регрессии
  # Говорят, так не делают
  .job = NULL,
  .data = models_full,
  .model = 'model_full',
  coefficients_table = NULL,
  n = 20,
  colours = ftype_colours,
  ar = 1.2,
  geom = c('col', 'error'),
  p.threshold = .1
) {
  
  if (!is.null(.job)) {
    .mid <- which(names(.data) == .model)
    coefs <- .data %>%
      filter(job == .job) %>%
      pull(.mid) %>%
      getElement(1) %>%
      getElement('coefficients')
  } else {
    coefs <- coefficients_table
    .job <- unique(coefs$job)
  }
  
  if ('p.value' %in% names(coefs)) {
    sgnf  <- 'значимых'
    coefs <- coefs %>%
      filter(sign(conf.low) == sign(conf.high)) %>%
      filter(p.value <= p.threshold)
    pvl   <- str_interp('(p < ${p.threshold})')
    cnfnt <- attr(coefs, 'conf.level') * 100
  } else {
    sgnf  <- ''
    pvl   <- ''
    cnfnt <- ''
  }
  
  coefs <- coefs %>%
    filter(ftype != 'Постоянный член') %>%
    # filter(!str_detect(fname, 'Описание на')) %>%
    top_n(n, abs(beta_hat)) %>%
    mutate(ftype = factor(ftype, levels = names(ftype_colours))) %>%
    mutate(fname = str_wrap(fname, width = 45)) %>%
    mutate(fname = fct_reorder(as.factor(fname), beta_hat))
  
  n <- nrow(coefs)
  
  geom <- match.arg(geom)
  # geom_coef <- switch(
  #   geom,
  #   col = geom_col,
  #   error = geom_errorbar
  # )
  
  # ggplot(
  #   coefs,
  #   aes(x = fname, y = beta_hat, fill = ftype)
  # ) +
  #   geom_coef(aes(ymin = conf.low, ymax = conf.high, colour = ftype), lwd = 2) +
  #   scale_x_discrete('Значение предиктора') +
  #   scale_y_continuous('Вычисленные коэффициенты') +
  #   scale_fill_manual(
  #     'Тип предиктора',
  #     values = colours,
  #     aesthetics = c('colour', 'fill')
  #   ) +
  #   geom_text(
  #     aes(
  #       label = round(beta_hat),
  #       # hjust = min(1, 1 + sign(beta_hat)),
  #       y = min(abs(beta_hat)) * .5 * sign(beta_hat)
  #     ),
  #     colour = 'black',
  #     # fontface = 'bold',
  #     hjust = .5,
  #     size = 4
  #   ) +
  #   coord_flip() +
  #   ggtitle(str_squish(str_interp('${.job}: топ-${n} ${sgnf} коэффициентов'))) +
  #   theme_minimal() +
  #   geom_hline(yintercept = 0, lty = 'dashed', colour = 'darkgrey') +
  #   theme(
  #     text = element_text(family = 'serif'),
  #     legend.box.margin = margin(0,0,0,0),
  #     legend.margin = margin(0,0,0,0),
  #     legend.position = 'top',
  #     legend.direction = 'vertical',
  #     legend.justification = 'left',
  #     legend.key.size = unit(.4, 'cm'),
  #     legend.title = element_text(size = 11),
  #     legend.text = element_text(size = 9),
  #     axis.text.y = element_text(size = 8),
  #     aspect.ratio = ar
  #   )
  
  p <- ggplot(
    coefs,
    aes(x = fname, y = beta_hat, fill = ftype, colour = ftype)
  ) +
    scale_x_discrete('Значение предиктора') +
    scale_fill_manual(
      'Тип предиктора',
      values = colours,
      aesthetics = c('colour', 'fill')
    )
  if (geom == 'error') {
    p <- p +
      geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        lwd = 1.6
      ) +
      scale_y_continuous(
        str_interp(
          'Вычисленные коэффициенты + ${cnfnt}% доверительный интервал'
        )
      ) +
      geom_point(pch = 23, size = 3) +
      geom_text(
        aes(
          label = round(beta_hat),
          hjust = min(1, 1 + sign(beta_hat)),
          y = min(abs(beta_hat) + 2500) * (0 - sign(beta_hat))
        ),
        colour = 'black',
        # fontface = 'bold',
        # hjust = 0,
        size = 4
      )
  } else {
    p <- p +
      geom_col() +
      scale_y_continuous('Вычисленные коэффициенты') +
      geom_text(
        aes(
          label = round(beta_hat),
          # hjust = min(1, 1 + sign(beta_hat)),
          y = min(abs(beta_hat)) * .5 * sign(beta_hat)
        ),
        colour = 'black',
        # fontface = 'bold',
        hjust = .5,
        size = 4
      )
  }
    p <- p +
    coord_flip() +
    ggtitle(
      str_squish(
        str_interp('${.job}: топ-${n} ${sgnf} коэффициентов ${pvl}')
      )
    ) +
    theme_light() +
    geom_hline(yintercept = 0, lty = 'dashed', colour = 'darkgrey') +
    theme(
      text = element_text(family = 'serif'),
      legend.box.margin = margin(0,0,0,0),
      legend.margin = margin(0,0,0,0),
      legend.position = 'top',
      legend.direction = 'vertical',
      legend.justification = 'left',
      legend.key.size = unit(.4, 'cm'),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9),
      axis.text.y = element_text(size = 10),
      aspect.ratio = ar
    )
    p
}
