infer4 <- function(x, n_reps = 10000) {
  longitud = x %>%
    na.omit() %>%
    length()
  
  set.seed(123)
  res = x %>%
    na.omit() %>%
    as_tibble() %>%
    rep_sample_n(size = longitud,
                 replace = TRUE,
                 reps = n_reps) %>%
    group_by(replicate) %>%
    summarise(promedio = mean(value)) %>%
    ungroup() %>%
    summarise(
      `Intención de voto (%)` = mean(promedio),
      se = sd(promedio),
      `L. Inferior` = `Intención de voto (%)` - (qnorm(p = 0.975) * se),
      `L. Superior` = `Intención de voto (%)` + (qnorm(p = 0.975) * se)
    ) %>% 
    select(-se)
  
  return(res)
}
