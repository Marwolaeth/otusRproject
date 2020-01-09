options(scipen = 999999999)

df <- readRDS('data/headhunter_plus.RDS')

theme_set(theme_minimal() + theme(text = element_text(family = 'helvetica')))

# 1
ggplot(df, aes(x = salary, fill = job)) +
  geom_density(alpha = .3) +
  xlim(0, 300000) +
  facet_grid(job ~ .)

# 2
ggplot(
  df,
  aes(x = job, y = salary, colour = experience, fill = experience)
) +
  geom_jitter(alpha = .1, width = .4) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
  stat_summary(fun.y = 'mean', geom = 'point', pch = 23)

dict_features <- readRDS('data/textual/feature_dictionary.RDS') %>%
  mutate(
    odds_job = if_else(
      odds_job > 1.5 * IQR(odds_job, na.rm = TRUE),
      1.5 * IQR(odds_job, na.rm = TRUE),
      odds_job
    )
  ) %>%
  filter(!is.na(job)) %>%
  group_by(job, ftype) %>%
  top_n(13, odds_job)
summary(dict_features$odds_job)
summary(dict_features)

# 3: Специфичные признаки
ggplot(
  dict_features,
  aes(x = fname, y = odds_job, fill = fname)
) +
  geom_col(width = .5, legend = FALSE) +
  coord_flip() +
  facet_grid(job ~ ftype)
