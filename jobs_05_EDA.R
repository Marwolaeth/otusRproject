options(scipen = 99999999)

df <- readRDS('data/headhunter.RDS')
# vacancies <- readRDS('data/vacancies.RDS')
# 
# vacancies %>%
#   select(starts_with('salary')) %>% arrange(desc(salary))

dfs <- df %>%
  select(
    -ends_with('id'),
    -contains('site'),
    -ends_with('name'),
    -address_raw,
    -address.street,
    -schedule,
    -employment,
    -department,
    -area
  ) %>%
  mutate_if(is.logical, as.factor) %>%
  target_by(salary)
summary(dfs)
map_int(dfs, ~ length(levels(.)))
relate(dfs, employer.has_wiki)
dlookr::eda_report(
  dfs,
  salary,
  output_dir = getwd(),
  output_format = 'html'
)

saveRDS(df, 'data/headhunter_full.RDS')
df <- readRDS('data/headhunter_full.RDS')

# Убираем точно не нужные переменные
df <- df %>%
  filter(!duplicated(description)) %>%
  select(
    -contains('site'),
    -address_raw,
    -matches('address\\.[a-z]+$'),
    -employer.trusted,
    -schedule,
    -employment,
    -department,
    # -contains('area'),
    -area,
    -starts_with('accept'),
    -ends_with('wiki'),
    -has_test,
    -ends_with('_at'),
    -employer.id,
    -employer.name
  ) %>%
  mutate(
    description = paste(name, description, driver_license_types)
  ) %>%
  select(-name, -driver_license_types)

saveRDS(df, 'data/headhunter_cut.RDS')

dfs <- df %>%
  mutate(description_length = nchar(description)) %>%
  target_by(salary)
relate(dfs, description_length)

lm(log(salary) ~ log(description_length)*job + experience, data = dfs) %>% tidy()

dfs %>%
  group_by(job, experience) %>%
  correlate(description_length) %>%
  View()

ggplot(dfs, aes(x = description_length, y = salary, colour = experience)) +
  geom_jitter(alpha = .2) +
  stat_smooth(method = 'lm') +
  stat_smooth(method = 'lm', aes(group = 1), colour = 'navy') +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ job) +
  theme_minimal() +
  theme(text = element_text(family = 'georgia'))

dfs <- df %>%
  target_by(salary)

ggplot(dfs, aes(x = description_sentiment, y = salary, colour = experience)) +
  geom_jitter(alpha = .2) +
  stat_smooth(method = 'lm') +
  stat_smooth(method = 'lm', aes(group = 1), colour = 'navy') +
  scale_y_log10() +
  facet_wrap(~ job) +
  theme_minimal() +
  theme(text = element_text(family = 'garamond'))
