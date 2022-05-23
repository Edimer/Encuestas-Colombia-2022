data_final <- function(x) {
  res = x %>%
    html_table(header = TRUE) %>%
    {
      .[-1]
    } %>%
    map(.f = clean_data_wikipedia_update) %>%
    bind_rows() %>%
    filter(
      candidato %in% c(
        "ingrid_betancourt",
        "enrique_gomez",
        "gustavo_petro",
        "federico_gutierrez",
        "rodolfo_hernandez",
        "sergio_fajardo"
      )
    ) %>%
    mutate(tamano_de_muestra = as.integer(tamano_de_muestra)) %>%
    filter(!is.na(tamano_de_muestra)) %>%
    mutate(bandera = str_detect(fecha, "al")) %>%
    separate(fecha, into = c("v1", "v2"), sep = "al") %>%
    mutate(fecha = if_else(bandera == TRUE, true = v2, false = v1)) %>%
    select(-c(v1, v2, bandera)) %>%
    relocate(encuesta,
             fecha,
             tamano_de_muestra,
             margen_de_error,
             everything()) %>%
    mutate(across(
      c(margen_de_error, blanco, porcentaje),
      clean_variable_string
    ),
    across(c(margen_de_error, blanco, porcentaje), as.numeric)) %>%
    distinct(encuesta, fecha, tamano_de_muestra, candidato, .keep_all = TRUE) %>%
    pivot_wider(names_from = candidato, values_from = porcentaje)
  
  return(res)
}