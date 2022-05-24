#' Esta función permite construir el diagrama de disperisón para dos candidatos de interés.
#' @param data (dataframe) base de datos con información de intención de voto.
#' @param candidato_x (string) nombre del candidato para el eje x
#' @param candidato_y (string) nombre del candidato para el eje y
#'

plot_scatter <- function(data, candidato_x, candidato_y) {
  data %>%
    ggplot(aes(
      x = !!sym(candidato_x),
      y = !!sym(candidato_y)
    )) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(
      method = "gam",
      se = FALSE,
      formula = y ~ ns(x, df = 2),
      color = "red"
    ) +
    geom_smooth(
      method = "gam",
      se = FALSE,
      formula = y ~ ns(x, df = 3),
      color = "green"
    ) +
    scale_x_log10() +
    scale_y_log10()
}