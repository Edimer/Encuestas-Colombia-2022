explor_candidate <- function(data, candidate) {
  promedio = data %>%
    pull(candidate) %>%
    mean(na.rm = TRUE)
  
  mediana = data %>%
    pull(candidate) %>%
    median(na.rm = TRUE)
  
  media_general = data %>%
    select(alfredo_saade:federico_gutierrez) %>%
    pivot_longer(cols = everything()) %>%
    pull(value) %>%
    mean(na.rm = TRUE)
  
  gg1 =
    data %>%
    ggplot(aes(x = !!sym(candidate))) +
    geom_density() +
    geom_rug() +
    geom_vline(
      xintercept = promedio,
      lty = 2,
      size = 0.5,
      color = "#CE3D32FF"
    ) +
    geom_vline(
      xintercept = mediana,
      lty = 2,
      size = 0.5,
      color = "#5050FFFF"
    ) +
    scale_x_log10() +
    labs(x = "Intención de voto (%)",
         y = "Densidad",
         caption = "Línea roja: promedio \n Línea azul: mediana")
  
  
  gg2 =
    data %>%
    ggplot(aes(x = fecha_publicacion, y = !!sym(candidate))) +
    geom_point(size = 2) +
    geom_smooth(method = "gam",
                formula = y ~ ns(x, df = 3),
                se = TRUE) +
    labs(y = "Intención de voto (%)", x = "Fecha")
  
  gg3 =
    data %>%
    mutate(year = year(fecha_publicacion)) %>%
    group_by(year) %>%
    summarise(promedio = mean(!!sym(candidate), na.rm = TRUE),
              desv = sd(!!sym(candidate), na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!is.na(year)) %>%
    mutate(year = as.factor(year)) %>%
    ggplot(aes(
      x = year,
      y = promedio,
      ymin = promedio - desv,
      ymax = promedio + desv
    )) +
    geom_point(size = 1.5) +
    geom_errorbar(width = 0.1) +
    labs(x = "Año", y = "Intención de voto (%)")
  
  
  
  gg4 =
    data %>%
    select(alfredo_saade:federico_gutierrez) %>%
    pivot_longer(cols = everything()) %>%
    group_by(name) %>%
    summarise(promedio = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(bandera = ifelse(name == candidate, "si", "no")) %>%
    ggplot(aes(
      x = fct_reorder(name, promedio),
      y = promedio,
      fill = bandera
    )) +
    geom_col(show.legend = FALSE) +
    scale_fill_igv() +
    geom_hline(yintercept = media_general,
               lty = 2,
               color = "black") +
    coord_flip() +
    labs (x = "", y = "Intención de voto (%)",
          caption = "Línea punteada: promedio general")
  
  res = plot_grid(
    gg1,
    gg2,
    gg3,
    gg4,
    ncol = 2,
    nrow = 2,
    align = "hv",
    labels = list("A)", "B)", "C)", "D)")
  )
  
  return(res)
  
  
}