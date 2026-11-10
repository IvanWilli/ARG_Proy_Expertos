# Trayectorias de migración neta. Nivel y estructura sexo/edad

library(readxl)
library(tidyverse)
library(writexl)
library(furrr)
pool_pond <- read.csv("resultados/pool_pond.csv")
set.seed(1010)
n_sim <- 5000

# inmigrantes y emigrantes 2025-2029. Se sortean independientemente.
inmigrantes_2025_2029 <- quantile(
  pool_pond |> filter(indicator == "inmigrantes_2025_2029") |> pull(valor),
  runif(n_sim),
  type = 8) |>
  as.numeric()
emigrantes_2025_2029 <- quantile(
  pool_pond |> filter(indicator == "emigrantes_2025_2029") |> pull(valor),
  runif(n_sim),
  type = 8) |>
  as.numeric()
saldo_2025_2029 <- inmigrantes_2025_2029 - emigrantes_2025_2029

# Población usada como denominador para transformar el saldo medio 2025-2029 en una tasa alrededor del punto medio del período.
poblacion_referencia_2027 <- 46540071 # INDEC Proy 1/7/2027
tmn_2027 <- 1000 * saldo_2025_2029 /poblacion_referencia_2027

# La tasa de 2075 también se sortea independientemente.
tmn_2075 <- quantile(
  pool_pond |> filter(indicator == "tmn_2075") |> pull(valor),
  runif(n_sim),
  type = 8) |>
  as.numeric()

# simulaciones
anclas_mig <- tibble(
  simulacion = seq_len(n_sim),
  inmigrantes_2025_2029,
  emigrantes_2025_2029,
  saldo_2025_2029,
  tmn_2027,
  tmn_2075
)

# trayectorias 
anios <- 2025:2075
trayectorias_mig <- crossing(
  simulacion = seq_len(n_sim),
  anio = anios) |>
  left_join(
    anclas_mig,
    by = "simulacion") |>
  mutate(
    tmn = if_else(
      # período 2025-2029
      anio <= 2029,
      tmn_2027,
      # lineal a 2075
      tmn_2027 +
        (tmn_2075 - tmn_2027) *
        (anio - 2029) / (2075 - 2029)),
    saldo_total = tmn * poblacion_referencia_2027 / 1000
  )

# tendencia previa
pop_2000a2025 <- read_xlsx("data/ARG_2000a2025.xlsx", sheet = "pop_2000a2025_1Enero")
SM_2000a2024 <- read_xlsx("data/SM_2000a2024.xlsx")
SM_2000a2024 <- SM_2000a2024 |>
  bind_rows(
    SM_2000a2024 |>
      filter(ym == 2022) |>
      slice(rep(row_number(), each = 2)) |>
      mutate(ym = rep(c(2023, 2024), times = n() / 2))
  ) |>
  mutate(tipo = case_when(
    ym %in% c(2002, 2022) ~ "Negativo",
    ym %in% c(2008, 2017) ~ "Positivo", T ~NA))

tnm_prev <- SM_2000a2024 |>
  summarise(saldo = sum(NM, na.rm = TRUE), .by = ym) |>
  left_join(
    pop_2000a2025 |>
      summarise(poblacion = sum(pop, na.rm = TRUE), .by = t),
    by = c("ym" = "t")) |>
  transmute(anio = ym, tmn = 1000 * saldo / poblacion) |>
  arrange(anio)

# plot
id_plot <- anclas_mig |>
  slice_sample(n = 300) |>
  pull(simulacion)
resumen_mig <- trayectorias_mig |>
  summarise(
    p10 = quantile(tmn, .05),
    p50 = quantile(tmn, .5),
    p90 = quantile(tmn, .95),
    .by = anio)
plot_tnm_trajs <- ggplot() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line(
    data = trayectorias_mig |>
      filter(simulacion %in% id_plot),
    aes(anio, tmn, group = simulacion),
    linewidth = .15,
    alpha = .15
  ) +
  geom_ribbon(
    data = resumen_mig,
    aes(anio, ymin = p10, ymax = p90),
    alpha = .18
  ) +
  geom_line(
    data = resumen_mig,
    aes(anio, p50),
    linewidth = 1
  ) +
  geom_line(
    data = tnm_prev,
    aes(anio, tmn),
    color = "#0072B2",
    linewidth = .9
  ) +
  geom_point(
    data = tnm_prev,
    aes(anio, tmn),
    color = "#0072B2",
    size = 1.2
  ) +
  theme_minimal() +   geom_vline(xintercept = 2025, col = "grey60", linetype = 2) +
  labs(x = "Año", y = "Por 1.000 habitantes")
plot_tnm_trajs |> 
  ggsave(filename = "plots/plot_tnm_trajs.pdf", height = 8, width = 8)

# estrtructura edad ------------------------------------------------------

mig_estructura_sexo_edad <- SM_2000a2024 |>
  filter(!is.na(tipo)) |>
  summarise(NM = mean(NM, na.rm = TRUE), .by = c(tipo, sex, age)) |>
  mutate(prop = NM / sum(NM, na.rm = TRUE), .by = tipo) |>
  transmute(tipo, edad = age, sexo = sex, prop)

# plot de estrcuturas según signo en años seleccionados
sm_edad_plot <- SM_2000a2024 |>
  filter(!is.na(tipo)) |>
  mutate(prop = NM / sum(NM, na.rm = TRUE), .by = c(tipo, ym)) |>
  transmute(tipo, anio = ym, edad = age, sexo = sex, prop)
plot_sm_edad <- ggplot() +
  geom_line(
    data = sm_edad_plot,
    aes(edad, prop, col = factor(anio), group = interaction(anio, sexo)),
    linewidth = .6,
    alpha = .8) +
  geom_line(
    data = mig_estructura_sexo_edad,
    aes(edad, prop),
    color = "black",
    linewidth = 1) +
  scale_x_continuous(
    breaks = seq(0, 100, 10),
    labels = seq(0, 100, 10),
    expand = expansion(mult = c(.01, .05))) +
  facet_grid(cols = vars(tipo), rows = vars(sexo)) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor.x = element_blank()) +
  labs(x = "Edad", y = "Proporción", col = "Año")
plot_sm_edad |>
  ggsave(filename = "plots/plot_sm_edad.pdf", height = 7, width = 8)


# estructura sexo --------------------------------------------------------

mig_estructura_sexo <- SM_2000a2024 |>
  filter(!is.na(tipo)) |>
  summarise(p_muj = sum(NM[sex == "M"])/sum(NM), .by = c(tipo))

# export ------------------------------------------------------------------

trayectorias_mig <- trayectorias_mig |>
  mutate(tipo = if_else(tmn < 0, "Negativo", "Positivo")) |> 
  select(simulacion, anio, tipo, tmn)
save(trayectorias_mig, file = "resultados/proy_tmn.rda")
save(mig_estructura_sexo, mig_estructura_sexo_edad, file = "resultados/proy_migr_sexo_edad.rda")
