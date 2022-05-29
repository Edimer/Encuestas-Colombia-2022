#' Esta función permite construir tres tipos de gráficos que representan la distribución
#' de la intención de voto (%).
#' @param data (dataframe) base de datos con información de intención de voto.
#' @param agrupado (logical) gráfico individual (FALSE) o agrupado (TRUE)
#' @param tipo (string) tipo de gráfico: boxplot o densidad (default)
#' @param candidato (string) nombre del candidato
#'

plot_distribution <-
  function(data,
           agrupado = FALSE,
           tipo = "densidad",
           candidato) {
    media_candidato = data %>%
      pull(!!sym(candidato)) %>%
      mean(na.rm = TRUE)
    
    mediana_candidato = data %>%
      pull(!!sym(candidato)) %>%
      median(na.rm = TRUE)
    
    valorp_normalidad =
      data %>%
      pull(!!sym(candidato)) %>%
      shapiro.test() %>%
      tidy() %>%
      pull(p.value) %>%
      round(digits = 5) %>%
      str_c("Valor p - Shapiro-Wilk = ", .)
    
    if (agrupado == FALSE) {
      if (tipo == "densidad") {
        data %>%
          ggplot(aes(x = !!sym(candidato))) +
          geom_density(fill = "dodgerblue2",
                       color = "dodgerblue2",
                       alpha = 0.3) +
          geom_vline(xintercept = media_candidato,
                     color = "red",
                     lty = 2) +
          geom_vline(xintercept = mediana_candidato,
                     color = "black",
                     lty = 2) +
          labs(
            title = "Distribución de intención de voto",
            subtitle = valorp_normalidad,
            X = candidato,
            y = "Densidad",
            caption = "Línea roja: promedio\nLínea negra: mediana"
          ) +
          scale_x_log10()
      } else if (tipo == "boxplot") {
        data %>%
          ggplot(aes(x = 1, y = !!sym(candidato))) +
          geom_boxplot(fill = "dodgerblue2",
                       color = "dodgerblue2",
                       alpha = 0.3) +
          geom_hline(yintercept = media_candidato,
                     color = "red",
                     lty = 2) +
          geom_hline(yintercept = mediana_candidato,
                     color = "black",
                     lty = 2) +
          labs(
            title = "Distribución de intención de voto",
            subtitle = valorp_normalidad,
            X = candidato,
            y = "Intención de voto (%)",
            caption = "Línea roja: promedio\nLínea negra: mediana"
          ) +
          scale_y_log10()
      }
      
      
    } else{
      if (tipo == "densidad") {
        data %>%
          select(all_of(candidatos)) %>%
          pivot_longer(cols = everything(),
                       names_to = "Candidato",
                       values_to = "Proporción") %>%
          ggplot(aes(
            x = Proporción,
            color = Candidato,
            fill = Candidato
          )) +
          geom_density(alpha = 0.3) +
          labs(
            x = "Intención de voto (%)",
            y = "Densidad",
            color = "",
            fill = ""
          ) +
          scale_x_log10()
        
      } else if (tipo == "boxplot") {
        data %>%
          select(all_of(candidatos)) %>%
          pivot_longer(cols = everything(),
                       names_to = "Candidato",
                       values_to = "Proporción") %>%
          mutate(Candidato = as.factor(Candidato)) %>%
          ggplot(aes(
            x = fct_reorder(Candidato, Proporción, median, na.rm = TRUE),
            y =  Proporción,
            color = Candidato,
            fill = Candidato
          )) +
          geom_boxplot(show.legend = FALSE, alpha = 0.5) +
          labs(
            x = "Candidato",
            y = "Intención de voto (%)",
            color = "",
            fill = ""
          ) +
          scale_y_log10() +
          coord_flip()
        
      } else{
        NULL
      }
    }
  }