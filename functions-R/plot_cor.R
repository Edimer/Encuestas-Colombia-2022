#' Esta función permite construir la matriz de correlación de la intención de voto entre
#' candidatos.
#' @param data (dataframe) base de datos con información de intención de voto.

plot_cor <- function(data) {
  data %>%
    select(all_of(candidatos)) %>%
    cor(method = "spearman", use = "pairwise.complete.obs") %>%
    as.data.frame() %>%
    rownames_to_column(var = "variable") %>%
    pivot_longer(cols = -variable) %>%
    ggplot(aes(x = variable, y = name, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label = round(value, digits = 2)), color = "white") +
    labs(x = "", y = "", fill = "Correlación")
}