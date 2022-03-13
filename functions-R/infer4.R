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
      estimate_infer4 = mean(promedio),
      se = sd(promedio),
      li_infer4 = estimate_infer4 - (qnorm(p = 0.975) * se),
      ls_infer4 = estimate_infer4 + (qnorm(p = 0.975) * se)
    ) %>% 
    select(-se)
  
  return(res)
}