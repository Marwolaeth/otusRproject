df <- readRDS('data/headhunter_cut.RDS')
sapply(df, function(x) sum(is.na(x)))

df <- df %>%
  mutate_at(vars(contains('metro'), employer.type), as.character) %>%
  mutate_at(
    vars(key_skills, specializations, employer.industries),
    function(x) ifelse(x == '', NA, x)
  ) %>%
  mutate_if(is.character, replace_na, '<MISSING>') %>%
  mutate_at(vars(contains('metro'), employer.type), as.factor) %>%
  sample_frac(1)

saveRDS(df, 'data/headhunter_cut.RDS')
############ ОТРАСЛИ КОМПАНИЙ ############
tf_industries <- df %>%
  unnest_tokens(
    input = employer.industries,
    output = industry,
    token = 'regex',
    pattern = '::',
    to_lower = FALSE
  )
dtm_industries <- tf_industries %>%
  count(id, industry) %>%
  cast_dtm(id, industry, n, weighting = tm::weightBin)
inspect(dtm_industries)

# tf_industries <- specific_terms(
#   dtm_industries,
#   df$job,
#   n = 10,
#   min_occ = 10
# )
# tf_industries %>%
#   map(as.data.frame) %>%
#   map(tibble::rownames_to_column, 'term') %>%
#   bind_rows(.id = 'job') %>%
#   as_tibble() -> tf_industries
quantile(col_sums(dtm_industries))
findFreqTerms(dtm_industries, 25)

dtm_industries<- dtm_industries[, union('<MISSING>', findFreqTerms(dtm_industries, 25))]
(tf_industries_names <- tibble(
  fname = colnames(dtm_industries),
  fid = paste('industry', seq_along(colnames(dtm_industries)), sep = '_')
))

tf_descriptions <- df[1:22, ] %>%
  mutate(
    description = str_stem(
      removePunctuation(
        str_replace_all(description, '[\r\n]', ' '),
        preserve_intra_word_dashes = TRUE
      )
    )
  )

tf_descriptions <- df[1:22, ] %>%
  mutate(
    description = str_stem(
      description
    )
  )
