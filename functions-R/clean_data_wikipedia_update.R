clean_data_wikipedia_update <- function(x) {
  res = x %>%
    clean_names()
  
  nombres_res = res %>% names()
  
  if ("fecha_de_publicacion" %in% nombres_res) {
    res = res %>%
      rename(fecha = fecha_de_publicacion)
    
  } else if ("fecha_de_realizacion" %in% nombres_res) {
    res = res %>%
      rename(fecha = fecha_de_realizacion)
    
  } else{
    print("La variable fecha tiene un nombre diferente!")
  }
  
  res = res %>%
    select(encuesta,
           tamano_de_muestra,
           fecha,
           margen_de_error,
           blanco,
           contains(
             c(
               "gustavo_petro",
               "federico_",
               "rodolfo_",
               "sergio_fajardo",
               "ingrid_",
               "enrique_gomez"
             )
           )) %>%
    pivot_longer(
      cols = -c(encuesta, tamano_de_muestra, fecha, margen_de_error,
                blanco),
      names_to = "candidato",
      values_to = "porcentaje"
    ) %>%
    separate(
      candidato,
      into = c("v1", "v2", "v3"),
      sep = "_",
      remove = TRUE
    ) %>%
    unite(v1, v2, col = "candidato", sep = "_") %>%
    select(-v3) %>%
    mutate(tamano_de_muestra = as.character(tamano_de_muestra))
  
  
  return(res)
  
}