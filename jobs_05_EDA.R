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

# Убираем точно не нужные переменные
df <- df %>%
  select(
    -contains('site'),
    -address_raw,
    -matches('address\\.[a-z]+$'),
    -employer.trusted,
    -schedule,
    -employment,
    -department,
    -contains('area'),
    -starts_with('accept'),
    -contains('has_'),
    -ends_with('_at'),
    -employer.id,
    -employer.name
  ) %>%
  mutate(
    description = paste(name, description, driver_license_types)
  ) %>%
  select(-name, -driver_license_types)
saveRDS(df, 'data/headhunter_cut.RDS')
