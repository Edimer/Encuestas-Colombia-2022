clean_variable_string <- function(x) {
  text_clean = x %>%
    str_remove(pattern = "%") %>%
    str_replace_all(pattern = ",", replacement = ".")
  
  return(text_clean)
}
