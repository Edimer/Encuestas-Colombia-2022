infer6 <- function(x, n_reps = 10000) {
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
    summarise(maximo = max(value)) %>%
    ungroup() %>%
    summarise(
      `Intención de voto (%)` = mean(maximo),
      `L. Inferior` = quantile(maximo, probs = 0.025),
      `L. Superior` = quantile(maximo, probs = 0.975)
    )
  
  return(res)
}
