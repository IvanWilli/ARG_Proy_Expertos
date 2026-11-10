# Trayectorias de mortalidad. Nivel y estructura sexo/edad

library(readxl)
library(tidyverse)
library(DemoTools)
library(writexl)
library(patchwork)
library(MortCast)
library(MortalityLaws)
library(future)
library(furrr)
pool_pond <- read.csv("resultados/pool_pond.csv")
set.seed(1010)

# cuantas realizaciones de la distribucion conjunta
n_sim <- 5000

# Las tres distribuciones conjuntas de mortalidad se muestrean "independientemente"
# las e0s
e0_mujeres_2030 <- quantile(
  pool_pond |> filter(indicator == "e0_mujeres_2030") |> pull(valor),
  runif(n_sim), type = 8) |> as.numeric()
e0_mujeres_2075 <- quantile(
  pool_pond |> filter(indicator == "e0_mujeres_2075") |> pull(valor),
  runif(n_sim), type = 8) |> as.numeric()

# que e(0) mujeres de 2075 sea superior a la de 2030.
invalido <- e0_mujeres_2075 <= e0_mujeres_2030
sum(invalido)/length(invalido)
while(any(invalido)){
  e0_mujeres_2075[invalido] <- quantile(
    pool_pond |> filter(indicator == "e0_mujeres_2075") |> pull(valor),
    runif(sum(invalido)), type = 8
  ) |> as.numeric()
  invalido <- e0_mujeres_2075 <= e0_mujeres_2030}

# brecha no tiene inválidos por límite técnico
brecha_e0_2075 <- quantile(
  pool_pond |>
    filter(indicator == "brecha_e0_mujeres_varones_2075") |>
    pull(valor),
  runif(n_sim), type = 8) |> as.numeric()
e0_varones_2075 <- e0_mujeres_2075 - brecha_e0_2075

# simulaciones
anclas <- tibble(
  simulacion = seq_len(n_sim),
  e0_mujeres_2030,
  e0_mujeres_2075,
  brecha_e0_2075,
  e0_varones_2075
)

# tendencia previa
lt_2000a2025 <- read_xlsx("data/ARG_2000a2025.xlsx", sheet = "lt_2000a2024") |>
  rename(Age = age, t = year) |>
  arrange(t, sex, Age) |>
  mutate(
    lx = if ("lx" %in% names(pick(everything()))) lx else
      DemoTools::lt_single_mx(
        nMx = nMx,
        Age = Age,
        Sex = if_else(sex[1] == "M", "f", "m")
      )$lx,
    .by = c(t, sex)
  )
e0_prev <- lt_2000a2025 |> filter(Age == 0) |> select(t, sex, ex)
# e0_prev |> ggplot(aes(t, ex, col = sex)) + geom_line()

# estimar brecha a 2030 según tendencia previa (no se preguntó en encuesta)
e0_2024_M <- e0_prev |> filter(sex == "M", t == 2024) |> pull(ex)
e0_2024_V <- e0_prev |> filter(sex == "V", t == 2024) |> pull(ex)
brecha_prev <- e0_prev |>
  summarise(brecha = ex[sex == "M"] - ex[sex == "V"], .by = t) |>
  arrange(t)
modelo_brecha_prev <- lm(brecha ~ t, data = brecha_prev |> filter(!t %in% 2020:2022))
brecha_2030 <- predict(modelo_brecha_prev, newdata = tibble(t = 2030)) |> as.numeric()

# 2024 -> e(0) 2030 -> e(0) 2075.
# Mujeres se interpola por tramos; brecha ya se extrapoló hasta 2030 y luego va lineal a 2075.
anios <- 2025:2075
trayectorias_mort <- crossing(
  simulacion = anclas$simulacion,
  anio = anios) |>
  left_join(
    anclas |>
      select(
        simulacion,
        e0_mujeres_2030,
        e0_mujeres_2075,
        brecha_e0_2075), by = "simulacion") |>
  mutate(
    # mujeres
    mujeres = if_else(
      # hasta 2030
      anio <= 2030, 
      e0_2024_M + (e0_mujeres_2030 - e0_2024_M) * (anio - 2024) / (2030 - 2024),
      # año > 2030
      e0_mujeres_2030 + (e0_mujeres_2075 - e0_mujeres_2030) * (anio - 2030) / (2075 - 2030)),
    # brecha
    brecha = if_else(
      anio <= 2030,
      predict(modelo_brecha_prev, newdata = tibble(t = anio + 0.5)) |> as.numeric(),
      brecha_2030 + (brecha_e0_2075 - brecha_2030) * (anio - 2030) / (2075 - 2030)),
    # varones
    varones = mujeres - brecha) |>
  select(simulacion, anio, Mujeres = mujeres, Varones = varones) |>
  pivot_longer(c(Mujeres, Varones), names_to = "sexo", values_to = "valor")

# tasas de salida en 2024
mx0_mort <- list(
  Mujeres = lt_2000a2025 |>
    filter(t == max(t), sex == "M") |>
    arrange(Age) |>
    (\(x) setNames(x$nMx, x$Age))(),
  Varones = lt_2000a2025 |>
    filter(t == max(t), sex == "V") |>
    arrange(Age) |>
    (\(x) setNames(x$nMx, x$Age))()
)

mx0_mort_obs <- mx0_mort
suavizar_hp <- function(mx) {
  fit <- MortalityLaws::MortalityLaw(
    x = as.numeric(names(mx)),
    mx = as.numeric(mx),
    law = "HP",
    opt.method = "LF2")
  setNames(pmax(as.numeric(fit$fitted.values), .Machine$double.eps), names(mx))
}
mx0_mort <- map(mx0_mort_obs, suavizar_hp)

plot_mort_hp_diag <- imap_dfr(
  mx0_mort_obs,
  \(mx, sexo_i) tibble(
    sexo = sexo_i,
    Age = as.numeric(names(mx)),
    Observada = as.numeric(mx),
    `Heligman-Pollard` = as.numeric(mx0_mort[[sexo_i]]))) |>
  pivot_longer(c(Observada, `Heligman-Pollard`), names_to = "serie", values_to = "mx") |>
  ggplot(aes(Age, mx, color = serie)) +
  geom_line(linewidth = .8) +
  facet_wrap(~sexo) +
  scale_y_log10() +
  theme_bw(base_size = 12) +
  labs(x = "Edad", y = "mx 2024", color = NULL)
  # plot_mort_hp_diag

# función de pmd model para cada simulación, año y sexo
sexo_pmd <- c(Mujeres = "female", Varones = "male")
lt_pmd_df <- function(e0, anio, sexo) {
  # e0 <- 77; sexo = "Mujeres"; anio = 2035
  # print(paste(sexo, anio))
  pred <- MortCast::pmd(
    e0 = e0,
    mx0 = mx0_mort[[sexo]],
    sex = sexo_pmd[[sexo]],
    nx = 1,
    keep.lt = TRUE
  )
  edades <- suppressWarnings(as.numeric(rownames(pred$mx)))
  if (anyNA(edades)) edades <- seq_len(nrow(pred$mx)) - 1
  Tx <- apply(pred$Lx, 2, function(x) rev(cumsum(rev(x))))
  n_edades <- length(edades)
  n_periodos <- length(anio)
  tibble(
    anio = rep(anio, each = n_edades),
    Age = rep(edades, times = n_periodos),
    AgeInt = pred$nx,
    e0 = rep(e0, each = n_edades),
    mx = as.vector(pred$mx),
    lx = as.vector(pred$lx),
    nqx = 1 - as.vector(pred$sr),
    nLx = as.vector(pred$Lx),
    Tx = as.vector(Tx),
    ex = as.vector(Tx / pred$lx)
  )
}

# aplica cálculo de tabla según pmd para cada simulación, año y sexo. paraleliza
future::plan(multisession)
lts_mort <- trayectorias_mort |>
  arrange(simulacion, sexo, anio) |>
  group_by(simulacion, sexo) |>
  group_split() |>
  future_map_dfr(
    \(x) {
      simulacion <- x$simulacion[[1]]
      sexo <- x$sexo[[1]]
      lt_pmd_df(
        e0 = x$valor,
        anio = x$anio,
        sexo = sexo) |>
        mutate(
          simulacion = simulacion,
          sexo = sexo,
          .before = 1)},
    .options = furrr_options(seed = TRUE),
    .progress = TRUE
  )
future::plan(sequential)

# plot e0 y mostrar algunas trayectorias (que no sean muy disruptivas)
id_plot <- trayectorias_mort |>
  filter(
    anio == 2030 & sexo == "Mujeres" & between(valor, 75, 85) |
    anio == 2030 & sexo == "Varones" & between(valor, 70, 80)) |> 
  distinct(simulacion) |>
  slice_sample(n = 300) |>
  pull(simulacion)
plot_mort_traj_e0 <- lts_mort |>
  filter(Age == 0, simulacion %in% id_plot) |>
  ggplot(aes(anio, ex, color = sexo)) +
  geom_line(aes(group = interaction(simulacion, sexo)), linewidth = .10, alpha = .1) +
  geom_line(
    data = lt_2000a2025 |>
      filter(Age == 0) |>
      transmute(anio = t, sexo = if_else(sex == "M", "Mujeres", "Varones"), ex),
    linewidth = .8) +
  geom_line(
    data = lts_mort |>
      filter(Age == 0) |>
      summarise(
        p5 = quantile(ex, .05),
        p50 = quantile(ex, .50),
        p95 = quantile(ex, .95),
        .by = c(anio, sexo)) |>
      pivot_longer(c(p5, p50, p95), names_to = "percentil", values_to = "ex"),
    aes(y = ex, linetype = percentil, linewidth = percentil,
        group = interaction(sexo, percentil))) +
  geom_vline(xintercept = 2025, col = "grey60", linetype = 2) +
  scale_linetype_manual(values = c(p5 = "dashed", p50 = "solid", p95 = "dashed")) +
  scale_linewidth_manual(values = c(p5 = .7, p50 = 1, p95 = .7), guide = "none") +
  scale_color_manual(
    values = c(Brecha = "#050505", Mujeres = "#F8766D", Varones = "#619CFF"),
    breaks = c("Mujeres", "Varones")) +
  theme_bw(base_size = 13) +
  labs(x = "Año", y = "e(0)", color = NULL, linetype = NULL)
plot_mort_traj_e0 |>
  ggsave(filename = "plots/plot_mort_traj_e0.pdf", height = 5, width = 8)

# plot brecha, 5q0, 45q15 y e60
componentes_mort_otros <- c("Brecha", "5q0", "45q15", "e60")
# calcularlos para cada simulación
indicadores_mort_otros <- bind_rows(
  lts_mort |>
    filter(Age %in% c(0, 5, 15, 60)) |>
    summarise(
      `5q0` = 1 - lx[match(5, Age)] / lx[match(0, Age)],
      `45q15` = 1 - lx[match(60, Age)] / lx[match(15, Age)],
      e60 = ex[match(60, Age)],
      .by = c(simulacion, anio, sexo)) |>
    pivot_longer(c(`5q0`, `45q15`, e60), names_to = "componente", values_to = "valor"),
  lts_mort |>
    filter(Age == 0) |>
    select(simulacion, anio, sexo, ex) |>
    pivot_wider(names_from = sexo, values_from = ex) |>
    transmute(simulacion, anio, sexo = "Brecha", componente = "Brecha",
              valor = Mujeres - Varones)) |>
  mutate(componente = factor(componente, levels = componentes_mort_otros))
# obtener tendencia previa
indicadores_mort_otros_prev <- bind_rows(
  lt_2000a2025 |>
    filter(Age %in% c(0, 5, 15, 60)) |>
    mutate(anio = t, sexo = if_else(sex == "M", "Mujeres", "Varones")) |>
    summarise(
      `5q0` = 1 - lx[match(5, Age)] / lx[match(0, Age)],
      `45q15` = 1 - lx[match(60, Age)] / lx[match(15, Age)],
      e60 = ex[match(60, Age)],
      .by = c(anio, sexo)) |>
    pivot_longer(c(`5q0`, `45q15`, e60), names_to = "componente", values_to = "valor"),
  lt_2000a2025 |>
    filter(Age == 0) |>
    transmute(anio = t, sexo = if_else(sex == "M", "Mujeres", "Varones"), ex) |>
    pivot_wider(names_from = sexo, values_from = ex) |>
    transmute(anio, sexo = "Brecha", componente = "Brecha",
              valor = Mujeres - Varones)) |>
  mutate(componente = factor(componente, levels = componentes_mort_otros))
# tomar percentiles
percentiles_mort_otros <- indicadores_mort_otros |>
  summarise(
    p5 = quantile(valor, .05),
    p50 = quantile(valor, .50),
    p95 = quantile(valor, .95),
    .by = c(anio, sexo, componente)) |>
  pivot_longer(c(p5, p50, p95), names_to = "percentil", values_to = "valor")
plot_mort_traj_otros <- indicadores_mort_otros |>
  filter(simulacion %in% id_plot) |>
  ggplot(aes(anio, valor, color = sexo)) +
  geom_line(aes(group = interaction(simulacion, sexo, componente)), linewidth = .10, alpha = .1) +
  geom_line(data = indicadores_mort_otros_prev, linewidth = .8) +
  geom_line(
    data = percentiles_mort_otros,
    aes(linetype = percentil, linewidth = percentil,
        group = interaction(sexo, componente, percentil))) +
  facet_wrap(~componente, scales = "free_y") +
  scale_color_manual(
    values = c(Brecha = "#050505", Mujeres = "#F8766D", Varones = "#619CFF"),
    breaks = c("Mujeres", "Varones")
  ) +
  scale_linetype_manual(values = c(p5 = "dashed", p50 = "solid", p95 = "dashed")) +
  scale_linewidth_manual(values = c(p5 = .7, p50 = 1, p95 = .7), guide = "none") +
  theme_bw(base_size = 13) +
  geom_vline(xintercept = 2025, col = "grey60", linetype = 2) +
  labs(x = "Año", y = NULL, color = NULL, linetype = NULL)
plot_mort_traj_otros |>
  ggsave(filename = "plots/plot_mort_traj_otros.pdf", height = 6, width = 8)

# Guardar separadamente Sx
Sx_mort <- lts_mort |> 
  arrange(sexo, simulacion, anio, Age) |>
  mutate(Sx = lt_id_Ll_S(nLx = nLx, Age = Age, N = 1), .by = c(simulacion, anio, sexo)) |>
  select(simulacion, Age, sexo, anio, Sx)

# export resultados
save(trayectorias_mort, file = "resultados/proy_e0.rda")
save(lts_mort, file = "resultados/proy_lts.rda")
save(Sx_mort, file = "resultados/proy_Sx.rda")

# Tasas de mortalidad por sexo, edad y año -------------------------------
mx_mort_plot <- lts_mort |>
  filter(is.finite(mx), mx > 0) |>
  summarise(mx = median(mx), .by = c(sexo, anio, Age))

plot_mort_mx_edad <- mx_mort_plot |>
  ggplot(aes(Age, mx, color = anio, group = anio)) +
  geom_line(linewidth = .35, alpha = .8) +
  facet_wrap(~sexo, nrow = 1) +
  scale_y_log10(labels = scales::label_scientific()) +
  scale_color_viridis_c(
    option = "plasma",
    breaks = pretty(mx_mort_plot$anio, n = 5)) +
  theme_bw(base_size = 13) +
  labs(x = "Edad", y = "mx", color = "Año")

plot_mort_mx_anio <- mx_mort_plot |>
  ggplot(aes(anio, mx, color = Age, group = Age)) +
  geom_line(linewidth = .25, alpha = .55) +
  facet_wrap(~sexo, nrow = 1) +
  scale_y_log10(labels = scales::label_scientific()) +
  scale_color_viridis_c(
    option = "viridis",
    breaks = pretty(mx_mort_plot$Age, n = 6)) +
  theme_bw(base_size = 13) +
  labs(x = "Año", y = "mx", color = "Edad")

plot_mort_mx_edad_anio <- plot_mort_mx_edad / plot_mort_mx_anio
plot_mort_mx_edad_anio |>
  ggsave(filename = "plots/plot_mort_mx_edad_anio.pdf", height = 8, width = 10)
