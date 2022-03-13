clean_data <- function(x) {
  res =
    x %>%
    clean_names() %>%
    filter(tamano_de_muestra != "Tamaño de muestra") %>% 
    mutate(tamano_de_muestra = as.integer(tamano_de_muestra)) %>% 
    rename(
      fecha_publicacion = fecha_de_publicacion_de_encuesta,
      muestra = tamano_de_muestra
    ) %>% 
    relocate(
      fuente,
      firma_encuestadora,
      fecha_publicacion,
      muestra,
      margen_de_error,
      everything()
    )
  
  return(res)
}