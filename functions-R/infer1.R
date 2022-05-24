infer1 <- function(x) {
  res =
    x %>%
    t.test() %>%
    tidy() %>%
    select(
      `Intención de voto (%)` = estimate,
      `L. Inferior` = conf.low,
      `L. Superior` = conf.high
    )
  
  return(res)
}
