infer5 <- function(x, n_reps = 10000) {
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
    summarise(mediana = median(value)) %>%
    ungroup() %>%
    summarise(
      `Intención de voto (%)` = mean(mediana),
      `L. Inferior` = quantile(mediana, probs = 0.025),
      `L. Superior` = quantile(mediana, probs = 0.975)
    )
  
  return(res)
}
