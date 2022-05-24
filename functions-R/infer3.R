infer3 <- function(x, n_reps = 10000) {
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
      `L. Inferior` = quantile(promedio, probs = 0.025),
      `L. Superior` = quantile(promedio, probs = 0.975)
    )
  
  return(res)
}
