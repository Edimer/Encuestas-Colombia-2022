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
      estimate_infer3 = mean(promedio),
      li_infer3 = quantile(promedio, probs = 0.025),
      ls_infer3 = quantile(promedio, probs = 0.975)
    )
  
  return(res)
}