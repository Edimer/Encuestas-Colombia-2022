infer1 <- function(x) {
  res =
    x %>%
    t.test() %>%
    tidy() %>%
    select(
      estimate_infer1 = estimate,
      li_infer1 = conf.low,
      ls_infer1 = conf.high
    )
  
  return(res)
}