infer2 <- function(x) {
  res = x %>%
    log1p() %>%
    t.test() %>%
    tidy() %>%
    mutate(
      estimate = expm1(estimate),
      conf.low = exp(conf.low),
      conf.high = exp(conf.high)
    ) %>%
    select(
      `Intención de voto (%)` = estimate,
      `L. Inferior` = conf.low,
      `L. Superior` = conf.high
    )
  
  return(res)
  
}
