options(scipen = 999999999)

df <- readRDS('data/headhunter_plus.RDS')

median_cl_boot <- function(x, conf.level = .95, na.rm = TRUE, nsim = 100000) {
  y <- replicate(nsim, median(sample(x, replace = TRUE), na.rm = na.rm))
  ymin = quantile(y, (1 - conf.level) / 2)
  ymax = quantile(y, 1 - (1 - conf.level) / 2)
  y    = median(y)
  return(data.frame(y, ymin, ymax))
}

theme_set(theme_minimal())

# 1
df <- df %>%
  mutate(
    salary = imputate_outlier(., salary, method = 'capping', no_attrs = TRUE)
  )
ggplot(df, aes(x = salary, fill = job)) +
  geom_density(alpha = .8, show.legend = FALSE) +
  facet_wrap(. ~ job) +
  scale_x_continuous(
    'Заработная плата (без выбросов)') +
  scale_y_continuous('Эмпирическая плотность распределения', labels = NULL) +
  ggtitle('Распределение зарплат по профессиям')

# 2
ggplot(
  df,
  aes(x = job, y = salary, colour = experience, fill = experience)
) +
  geom_jitter(alpha = .1, width = .4, height = 1000) +
  # scale_y_log10() +
  stat_summary(
    fun.data = 'mean_cl_boot', geom = 'errorbar', lwd = .8, show.legend = TRUE
  ) +
  # stat_summary(fun.data = 'median_cl_boot', geom = 'errorbar', lwd = .8) +
  stat_summary(fun.y = 'mean', geom = 'point', pch = 23, size = 3) +
  # stat_summary(fun.y = 'median', geom = 'point', pch = 15, size = 3) +
  scale_x_discrete('Профессия (поисковый запрос к API HeadHunter)') +
  scale_y_continuous(
    'Заработная плата + 95% доверительный интервал для среднего значения'
  ) +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle('Распределение зарплат по профессиям\nи требуемому опыту')

dict_features <- readRDS('data/textual/feature_dictionary.RDS') %>%
  mutate(
    odds_job = if_else(
      odds_job >= quantile(
        odds_job, .75, na.rm = TRUE) + 1.5 * IQR(odds_job, na.rm = TRUE),
      (quantile(
        odds_job, .75, na.rm = TRUE) + 1.5 * IQR(odds_job, na.rm = TRUE) +
         log(odds_job) +
         runif(length(odds_job), 0, 6)
      ),
      odds_job
    ),
    fname = str_wrap(str_remove(fname, '^.+:\\s'), 40)
  ) %>%
  filter(!is.na(job)) %>%
  group_by(job, ftype) %>%
  top_n(15, odds_job) %>%
  split(.$job) %>%
  map(
    ~ split(., .$ftype) %>%
      set_names(c('Ключевые слова', 'Навыки', 'Специализации'))
  ) %>%
  map(~ map(., mutate, fname = fct_reorder(fname, odds_job)))
summary(dict_features$odds_job)
hist(dict_features$odds_job)
summary(dict_features)

# 3: Специфичные признаки
ggplot(
  dict_features[[1]],
  aes(x = fname, y = odds_job, fill = ftype)
) +
  geom_col(show.legend = FALSE) +
  scale_x_discrete('Ключевое слово', labels = NULL) +
  scale_y_continuous('Шанс упоминания в профессии') +
  geom_text(
    aes(
      label = format(round(odds_job, 2), decimal.mark = ','),
      y = odds_job - min(odds_job) * .3
    ),
    colour = 'white',
    fontface = 'bold'
  ) +
  coord_flip() +
  facet_rep_grid(~ ftype, scales = 'free', repeat.tick.labels = 'left') +
  # facet_grid(. ~ ftype, scales = 'free') +
  ggtitle('SMM-менеджер:') +
  theme(strip.text.x = element_text(margin = margin(0,0,0,0)))

plot_specific_features <- function(
  group,
  category,
  colour,
  fvar = 'value',
  fvar.caption = fvar,
  l,
  separator = str_pad('#', 20, pad = '#'),
  sleep = 5
) {
  d <- getElement(l, group) %>%
    getElement(category) %>%
    rename_(., fvar = 'fvar')
  
  print(separator)
  
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

category_colours <- data.frame(
  category = names(dict_features[[1]]),
  colour   = c('#33BBEE', '#CC3311', '#0077BB')
)
(group_grid <- expand.grid(
  category = category_colours$category,
  g = names(dict_features)
) %>%
    left_join(category_colours))

pmap(
  group_grid,
  plot_specific_features,
  fvar = 'odds_job',
  fvar.caption = 'Шанс упоминания в профессии',
  l = dict_features
)

rmarkdown::render(input = 'pawluczenko_OtusRproject.Rmd', output_format = 'html_document', encoding = 'UTF-8')
