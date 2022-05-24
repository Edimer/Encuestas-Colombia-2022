#' Esta función permite construir cuatro tipos de gráficos para exploración de
#' firmas encuestadoras.
#' @param tipo (string) tipo de gráfico:
#' - Total Encuestas
#' - Tamaño Muestral
#' - Intención de voto - 1
#' - Intención de voto - 1
#'

plot_encuestas <- function(tipo = "Total Encuestas") {
  if (tipo == "Total Encuestas") {
    datos %>%
      count(encuesta) %>%
      mutate(encuesta = fct_reorder(encuesta, n)) %>%
      ggplot(aes(x = encuesta, y = n, label = n)) +
      geom_col() +
      geom_label() +
      coord_flip() +
      labs(x = "", y = "Total encuestas")
  } else if (tipo == "Tamaño Muestral") {
    datos %>%
      ggplot(aes(x = fct_reorder(encuesta, tamano_de_muestra), y = tamano_de_muestra)) +
      geom_boxplot() +
      labs(x = "Firma encuestadora", y = "Tamaño muestral (n)")
  } else if (tipo == "Intención de voto - 1") {
    datos %>%
      select(all_of(candidatos), encuesta) %>%
      pivot_longer(cols = -encuesta) %>%
      ggplot(aes(
        x = encuesta,
        y = value,
        fill = name,
        color = name
      )) +
      geom_boxplot(alpha = 0.5) +
      scale_y_log10() +
      labs(
        color = "",
        fill = "",
        x = "",
        y = "Intención de voto (%)"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else if (tipo == "Intención de voto - 2") {
    datos %>%
      select(all_of(candidatos), encuesta) %>%
      pivot_longer(cols = -encuesta) %>%
      group_by(name, encuesta) %>%
      summarise(metrica = max(value, na.rm = TRUE)) %>%
      ggplot(aes(
        x = encuesta,
        y = metrica,
        fill = name,
        color = name
      )) +
      geom_col(alpha = 0.5, position = "dodge") +
      labs(
        color = "",
        fill = "",
        x = "",
        y = "Intención de voto (%)"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
}