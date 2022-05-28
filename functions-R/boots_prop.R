boots_prop <- function(candidato,
                       total_votos,
                       total_encuesta,
                       num_reps = 1000,
                       semilla = 2022) {
  candidato = rep(candidato, times = total_votos)
  otros_candidatos = rep("Otro", times = total_encuesta - total_votos)
  
  tabla = tibble(candidato = c(candidato, otros_candidatos))
  
  set.seed(semilla)
  remuestreo = tabla %>%
    specify(response = candidato, success = candidato) %>%
    generate(reps = num_reps, type = "bootstrap") %>%
    calculate(stat = "prop")
  
  prom_remuestreo =
    remuestreo  %>%
    pull(stat) %>%
    mean()
  
  res =
    remuestreo %>%
    get_confidence_interval(level = 0.95, type = "percentile") %>%
    mutate(`Intención de voto (%)` = prom_remuestreo) %>%
    rename(`L. Inferior` = lower_ci,
           `L. Superior` = upper_ci) %>%
    relocate(`Intención de voto (%)`, everything())
  
  return(res)
}