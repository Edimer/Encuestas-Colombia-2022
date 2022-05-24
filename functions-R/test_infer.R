test_infer <- function(n_reps = 100) {
  datos %>%
    select(all_of(candidatos)) %>%
    map(.f = infer1) %>%
    bind_rows() %>%
    mutate(Candidato = candidatos,
           across(where(is.numeric), round, digits = 2),
           Método = "t-student") %>%
    bind_rows(
      datos %>%
        select(all_of(candidatos)) %>%
        map(.f = infer2) %>%
        bind_rows() %>%
        mutate(
          Candidato = candidatos,
          across(where(is.numeric), round, digits = 2),
          Método = "t-student - log"
        )
    ) %>%
    bind_rows(
      datos %>%
        select(all_of(candidatos)) %>%
        map(.f = ~ infer3(x = .x, n_reps = n_reps)) %>%
        bind_rows() %>%
        mutate(
          Candidato = candidatos,
          across(where(is.numeric), round, digits = 2),
          Método = "BootPromedio-ICQ"
        )
    ) %>%
    bind_rows(
      datos %>%
        select(all_of(candidatos)) %>%
        map(.f = ~ infer4(x = .x, n_reps = n_reps)) %>%
        bind_rows() %>%
        mutate(
          Candidato = candidatos,
          across(where(is.numeric), round, digits = 2),
          Método = "BootPromedio-ICEE"
        )
    ) %>%
    bind_rows(
      datos %>%
        select(all_of(candidatos)) %>%
        map(.f = ~ infer5(x = .x, n_reps = n_reps)) %>%
        bind_rows() %>%
        mutate(
          Candidato = candidatos,
          across(where(is.numeric), round, digits = 2),
          Método = "BootMediana-ICQ"
        )
    )
}