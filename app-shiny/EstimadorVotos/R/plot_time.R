#' Esta función permite construir cuatro tipos de gráficos para explorar la intención
#' de voto a través del tiempo.
#' @param data (dataframe) base de datos con información de intención de voto.
#' @param tipo (string) tipo de gráfico:
#' - Serie Individual
#' - Serie Agrupada
#' - Variabilidad
#' - Variación Anual
#' @param candidato (string) nombre del candidato
#'

plot_time <- function(data, tipo = "Serie Individual", candidato) {
  maximo_candidato =
    data %>%
    pull(!!sym(candidato)) %>%
    max(na.rm = TRUE)
  
  if (tipo == "Serie Individual") {
    data %>%
      filter(!is.na(!!sym(candidato))) %>%
      ggplot(aes(x = fecha, y = !!sym(candidato))) +
      geom_line(color = "gray30", size = 0.3) +
      geom_point(size = 0.1) +
      labs(x = "Fecha", y = "Intención de voto (%)") +
      geom_vline(
        xintercept = ymd("2021-01-01"),
        lty = 2,
        size = 0.1
      ) +
      geom_vline(
        xintercept = ymd("2022-01-01"),
        lty = 2,
        size = 0.1
      ) +
      scale_y_continuous(limits = c(0, maximo_candidato)) +
      geom_smooth(
        se = TRUE,
        color = "black",
        lty = 2,
        size = 0.5,
        fill = "gray65",
        method = "gam",
        formula = y ~ ns(x, df = 3)
      )
  } else if (tipo == "Serie Agrupada") {
    data %>%
      select(all_of(candidatos), fecha) %>%
      pivot_longer(cols = -fecha) %>%
      ggplot(aes(x = fecha, y = value, color = name)) +
      labs(x = "Fecha", y = "Intención de voto (%)") +
      geom_vline(
        xintercept = ymd("2021-01-01"),
        lty = 2,
        size = 0.1
      ) +
      geom_vline(
        xintercept = ymd("2022-01-01"),
        lty = 2,
        size = 0.1
      ) +
      scale_y_continuous(limits = c(0, 60)) +
      geom_smooth(
        se = FALSE,
        size = 0.5,
        method = "gam",
        formula = y ~ ns(x, df = 3)
      ) +
      labs(color = "")
  } else if (tipo == "Variabilidad") {
    data %>%
      select(fecha, all_of(candidatos)) %>%
      pivot_longer(cols = -fecha) %>%
      group_by(name) %>%
      arrange(fecha) %>%
      ungroup() %>%
      group_by(fecha, name) %>%
      summarise(prop_voto = mean(value, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(name) %>%
      mutate(lag_prop_voto = lag(prop_voto),
             dif_voto = prop_voto - lag_prop_voto) %>%
      ungroup() %>%
      ggplot(aes(x = fecha, y = dif_voto, color = name)) +
      geom_smooth(
        se = FALSE,
        size = 0.5,
        method = "gam",
        formula = y ~ ns(x, df = 3),
        lty = 1
      ) +
      geom_hline(yintercept = 0,
                 lty = 2,
                 size = 0.1) +
      labs(color = "", x = "Fecha", y = "Variabilidad (%)\n (t) - (t-1)")
  } else if (tipo == "Variación Anual") {
    data %>%
      select(fecha, all_of(candidatos)) %>%
      pivot_longer(cols = -fecha) %>%
      mutate(year = as.factor(year(fecha))) %>%
      ggplot(aes(
        x = year,
        y = value,
        fill = name,
        color = name
      )) +
      geom_boxplot(alpha = 0.5) +
      scale_y_log10() +
      labs(
        x = "Año",
        y = "Intención de voto (%)",
        color = "",
        fill = ""
      )
  }
}