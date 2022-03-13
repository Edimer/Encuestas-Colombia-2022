infer2 <- function(x) {
  res = x %>%
    log1p() %>%
    t.test() %>%
    tidy() %>%
    select(
      estimate_infer2 = estimate,
      li_infer2 = conf.low,
      ls_infer2 = conf.high
    ) %>%
    mutate(
      estimate_infer2 = expm1(estimate_infer2),
      li_infer2 = exp(li_infer2),
      ls_infer2 = exp(ls_infer2)
    )
  
  return(res)
  
}