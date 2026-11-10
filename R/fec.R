# Trayectorias de fecundidad. Nivel y estructura edad

library(readxl)
library(tidyverse)
library(writexl)
library(furrr)
library(patchwork)
source("R/funs.R")
pool_pond <- read.csv("resultados/pool_pond.csv")
set.seed(1010)

# cuantas realizaciones de la distribucion conjunta
n_sim <- 5000

# Correlacion entre TGF minima y anio del minimo (copula gaussiana)
# independencia (no se ve correlación en plots de distribuciones individuales)
# tgf min
z1 <- rnorm(n_sim)
tgf_min <- quantile(
  pool_pond |> filter(indicator == "tgf_min") |> pull(valor),
  pnorm(z1),
  type = 8) |>
  as.numeric()

# año mínimo (independiente de nivel)
rho_minimo <- 0
z2 <- rho_minimo * z1 + sqrt(1 - rho_minimo^2) * rnorm(n_sim)
anio_min_continuo <- quantile(
  pool_pond |> filter(indicator == "anio_min") |> pull(valor),
  pnorm(z2),
  type = 8) |>
  as.numeric()
# no puede irse del rango
anio_min <- round(anio_min_continuo) |> pmax(2025) |> pmin(2075)
  # data.frame((anio_min |> trunc() |> table()) / length(anio_min)) |> plot()
  # summary(anio_min)

# TGF 2075
tgf_2075 <- quantile(
  pool_pond |> filter(indicator == "tgf_2075") |> pull(valor),
  runif(n_sim),
  type = 8) |>
  as.numeric()

# ESTO SE COMENTA, INTERPRETANDO AL "MINIMO" COMO PIVOTE
# Si el minimo ocurre antes de 2075, la TGF en 2075 no puede quedar por debajo de ese minimo.
# invalido <- tgf_2075 < tgf_min & anio_min < 2075
# while(any(invalido)){
#   tgf_2075[invalido] <- quantile(
#     pool_pond |> filter(indicator == "tgf_2075") |> pull(valor),
#     runif(sum(invalido)),
#     type = 8) |>
#     as.numeric()
#   invalido <- tgf_2075 < tgf_min & anio_min < 2075
# }

# Si el anio del minimo es exactamente 2075, necesariamente: TGF_2075 = TGF minima.
tgf_2075[anio_min == 2075] <- tgf_min[anio_min == 2075]

# simulaciones
anclas_fec <- tibble(
  simulacion = seq_len(n_sim),
  tgf_min,
  anio_min_continuo,
  anio_min,
  tgf_2075
)

# tendencia previa -------------------------------------------------------
asfr_2000a2025 <- read_xlsx("data/ARG_2000a2025.xlsx", sheet = "asfr_2000a2024")

# Interpolacion lineal: 2024 -> minimo -> 2075.
tgf_2024 <- asfr_2000a2025 |> 
  filter(t == max(t)) |> 
  summarise(tgf = sum(asfr), .by = t) |> pull(tgf)
anios <- 2025:2075
trayectorias_fec <- crossing(
  simulacion = seq_len(n_sim),
  anio = anios) |>
  left_join(
    anclas_fec,
    by = "simulacion"
  ) |>
  mutate(
    valor = if_else(
      # hasta el min
      anio <= anio_min,
      tgf_2024 +
        (tgf_min - tgf_2024) *
        (anio - 2024) / (anio_min - 2024),
      # luego del min
      tgf_min +
        (tgf_2075 - tgf_min) *
        (anio - anio_min) / (2075 - anio_min)
    )
  )

# algunas trays
  # trayectorias_fec |>
  #   filter(simulacion %in% sample(1:n_sim, size = 10)) |>
  #   ggplot(aes(anio, valor, group = simulacion)) +
  #   geom_line(alpha = .8) +
  #   geom_hline(yintercept = tgf_2024, col = 2) +
  #   scale_y_continuous(limits = c(0, 2.5)) +
  #   theme_minimal() +
  #   labs(x = "Año", y = "TGF")

# trayectorias
resumen_fec <- trayectorias_fec |>
  group_by(anio) |>
  summarise(
    p5 = quantile(valor, .05),
    p50 = quantile(valor, .50),
    p95 = quantile(valor, .95),
    .groups = "drop")

# plot nivel
id_plot <- anclas_fec |>
  slice_sample(n = 300) |>
  pull(simulacion)
tgf_prev <- asfr_2000a2025 |> 
  summarise(tgf = sum(asfr), .by = t) 
plot_tgf_traj <- ggplot() +
  geom_line(
    data = trayectorias_fec |>
      filter(simulacion %in% id_plot),
    aes(anio, valor, group = simulacion),
    linewidth = .1,
    alpha = .1) +
  geom_ribbon(
    data = resumen_fec,
    aes(anio, ymin = p5, ymax = p95),
    alpha = .18) +
  geom_line(
    data = resumen_fec,
    aes(anio, p50),
    linewidth = 1) +
  geom_line(data = tgf_prev |> select(anio = t, valor = tgf), aes(anio, valor)) +
  theme_bw(base_size = 13) +
  geom_vline(xintercept = 2024.5, col = "grey60", linetype = 2) +
  labs(x = "Año", y = "TGF")
plot_tgf_traj |> 
  ggsave(filename = "plots/plot_tgf_traj.pdf", height = 5, width = 8)

# estructuras ------------------------------------------------------------
# tendencia previa
asfr_2000a2025 <- read_xlsx("data/asfr_2000a2024.xlsx")

# países HFD
asfr_hfd <- read.table("data/asfrRR.txt", skip = 2, header = TRUE)

# cálculo de indicadores
anio_inicio <- 1960
momentos_asfr <- bind_rows(
  asfr_hfd |>
    filter(Year >= anio_inicio) |> 
    transmute(pais = Code, anio = Year, age = parse_number(as.character(Age)), asfr = ASFR),
  asfr_2000a2025 |>
    transmute(pais = "Argentina", anio = t, age, asfr)) |>
  filter(age >= 15, age <= 49, !is.na(asfr), anio >= anio_inicio) |>
  summarise(
    tgf = sum(asfr),
    edad_media = sum(age * asfr) / tgf,
    desv_standard = sqrt(sum((age - edad_media)^2 * asfr) / tgf),
    cv = desv_standard / edad_media,
    .by = c(pais, anio)) |>
  filter(tgf > 0)

# relacion entre indicadores
paises_destacados <- c("SWE", "ITA", "ESP", "JPN", "Argentina")
colores_destacados <- c(SWE = "red", ITA = "darkgreen", ESP = "blue", JPN = "purple", Argentina = "black", Otros = "grey75")
momentos_asfr_plot_dest <- momentos_asfr |>
  mutate(pais_destacado = if_else(pais %in% paises_destacados, pais, "Otros"))
p1 <- momentos_asfr_plot_dest |>
  ggplot(aes(anio, tgf)) +
  geom_path(aes(group = pais, color = pais_destacado, linewidth = pais_destacado != "Otros"), alpha = .6) +
  scale_color_manual(values = colores_destacados, breaks = paises_destacados) +
  scale_linewidth_manual(values = c(`FALSE` = .35, `TRUE` = .9), guide = "none") +
  theme_bw(base_size = 13) +
  labs(title = "A", color = NULL, x = "Año", y = "TGF")
p2 <- momentos_asfr_plot_dest |>
  ggplot(aes(edad_media, tgf)) +
  geom_path(aes(group = pais, color = pais_destacado, linewidth = pais_destacado != "Otros"), alpha = .6) +
  scale_color_manual(values = colores_destacados, breaks = paises_destacados) +
  scale_linewidth_manual(values = c(`FALSE` = .35, `TRUE` = .9), guide = "none") +
  theme_bw(base_size = 13) +
  labs(title = "B", color = NULL, x = "Edad Media", y = "TGF")
p3 <- momentos_asfr_plot_dest |>
  ggplot(aes(edad_media, cv)) +
  geom_path(aes(group = pais, color = pais_destacado, linewidth = pais_destacado != "Otros"), alpha = .6) +
  scale_color_manual(values = colores_destacados, breaks = paises_destacados) +
  scale_linewidth_manual(values = c(`FALSE` = .35, `TRUE` = .9), guide = "none") +
  theme_bw(base_size = 13) +
  labs(title = "C", color = NULL, x = "Edad Media", y = "CV") +
  geom_smooth(span = .5, col = 1, 
    data = momentos_asfr_plot_dest |> filter(between(edad_media, 26, 32)), linetype = 2)
plot_relacion_indicadores <- (p1 | (p2 / p3)) +
  plot_layout(guides = "collect", widths = c(1.15, 1)) &
  theme(legend.position = "bottom")
plot_relacion_indicadores
plot_relacion_indicadores |>
  ggsave(filename = "plots/plot_relacion_indicadores_hfd.pdf", height = 7, width = 10)

# parámetros de llegada
anio_fin <- 2075
momentos_asfr |> filter(anio >= 2024) |>  summarise(mean(edad_media), mean(cv))
parametros_extrapolacion <- tibble(
  indicador_id = c("edad_media", "cv"),
  indicador = factor(c("Edad media", "CV"), levels = c("Edad media", "CV")),
  L = c(32, 0.15),
  anio_llegada = anio_fin)

# función interpolacion lineal entre 2024 y el valor target
interpola_llegada <- function(y0, t0, t_proj, L, anio_llegada) {
  tibble(t = t_proj) |>
    mutate(y = y0 + (L - y0) * (t - t0) / (anio_llegada - t0)) |>
    mutate(y = if_else(t == anio_llegada, L, y)) |>
    select(t, y)
}

# extrapolación
momentos_asfr_plot <- momentos_asfr |>
  pivot_longer(
    all_of(parametros_extrapolacion$indicador_id),
    names_to = "indicador_id",
    values_to = "valor") |>
  left_join(parametros_extrapolacion, by = "indicador_id")
momentos_arg_base <- momentos_asfr_plot |>
  filter(pais == "Argentina") |>
  group_by(indicador_id) |>
  slice_max(anio, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(indicador_id, indicador, L, anio_llegada, anio_base = floor(anio), valor_base = valor)
momentos_arg_proy_plot <- momentos_arg_base |>
  rowwise() |>
  mutate(
    proyeccion = list(interpola_llegada(
      y0 = valor_base,
      t0 = anio_base,
      t_proj = (anio_base + 1):anio_llegada,
      L = L,
      anio_llegada = anio_llegada))) |>
  ungroup() |>
  unnest(proyeccion) |>
  transmute(pais = "Argentina proyeccion", anio = t, indicador_id, indicador, L, valor = y)

# plot de extrpoalción
limites_extrapolacion <- parametros_extrapolacion |> transmute(indicador_id, indicador, valor = L)
plot_momentos_extrapolacion <- ggplot() +
  geom_line(
    data = momentos_asfr_plot |> filter(pais != "Argentina"),
    aes(anio, valor, group = pais),
    color = "grey70",
    alpha = .35,
    linewidth = .35) +
  geom_point(
    data = momentos_asfr_plot |> filter(pais != "Argentina"),
    aes(anio, valor),
    color = "grey70",
    alpha = .18,
    size = .5) +
  geom_line(
    data = momentos_asfr_plot |> filter(pais == "Argentina"),
    aes(anio, valor),
    color = "black",
    linewidth = 1) +
  geom_point(
    data = momentos_asfr_plot |> filter(pais == "Argentina"),
    aes(anio, valor),
    color = "black",
    size = 1.8) +
  geom_point(
    data = momentos_arg_base,
    aes(anio_base, valor_base),
    color = "blue",
    size = 2.4) +
  geom_line(
    data = momentos_arg_proy_plot,
    aes(anio, valor),
    color = "blue",
    linewidth = 1.1,
    linetype = 2) +
  geom_hline(
    data = limites_extrapolacion,
    aes(yintercept = valor),
    color = "grey45",
    linetype = 3) +
  facet_wrap(~indicador, scales = "free_y", ncol = 1) +
  theme_bw(base_size = 13) +
  labs(x = "Año", y = NULL)
plot_momentos_extrapolacion |> 
  ggsave(filename = "plots/plot_momentos_extrapolacion_hfd_arg.pdf", height = 8, width = 8)

# proyectar la beta y las proporciones de fecundidad por edad
ar2024 <- asfr_2000a2025 |>
  filter(t == max(t), age >= 15, age <= 49) |> arrange(age) |> 
  mutate(age, prop = asfr / sum(asfr))
proy_momentos <- momentos_arg_proy_plot |>
  select(anio, indicador_id, valor) |>
  pivot_wider(names_from = indicador_id, values_from = valor) |>
  transmute(anio, media = edad_media, sd = cv * edad_media)
asfr_prop <- beta_asfr(
  age = ar2024$age,
  asfr_0 = ar2024$asfr,
  proy = proy_momentos,
  anio_0 = 2024,
  transicion = 6)
momentos_arg_proy_plot_nuevo <- momentos_arg_proy_plot
ar2024_nuevo <- ar2024
asfr_prop_nuevo <- asfr_prop
asfr_prop_hfd_ult_nuevo <- asfr_hfd |>
  transmute(
    pais = Code,
    anio = Year,
    age = parse_number(as.character(Age)),
    asfr = ASFR) |>
  filter(age >= 15, age <= 49, !is.na(asfr)) |>
  group_by(pais) |>
  filter(anio == max(anio)) |>
  ungroup() |>
  group_by(pais, anio) |>
  mutate(
    tgf = sum(asfr),
    prop = asfr / tgf) |>
  ungroup() |>
  filter(tgf > 0)

# plot de proyección de estructura
plot_proy_asfr_prop <- ggplot() +
  geom_line(
    data = asfr_prop_hfd_ult_nuevo,
    aes(age, prop, group = pais),
    color = "grey70",
    alpha = .45,
    linewidth = .35) +
  geom_line(
    data = ar2024,
    aes(age, prop),
    color = "#070707",
    size = 1.7) +
  geom_line(
    data = asfr_prop,
    aes(age, prop, color = anio, group = anio),
    linewidth = .9) +
  scale_color_gradientn(
    colors = c("#2C7BB6", "#FFFFBF", "#D7191C"),
    breaks = pretty(asfr_prop$anio, n = 5)) +
  theme_minimal(base_size = 13) +
  labs(
    x = "Edad",
    y = "Proporción",
    color = "Año")
plot_proy_asfr_prop |> 
  ggsave(filename = "plots/plot_proy_asfr_prop.pdf", height = 6, width = 8)

# convertir en tasas
asfr_proyectadas <- trayectorias_fec |>
  select(simulacion, anio, tgf = valor) |>
  left_join(
    asfr_prop |> filter(anio %in% unique(trayectorias_fec$anio)) |> select(anio, age, prop),
    by = "anio",
    relationship = "many-to-many") |>
  mutate(asfr = tgf * prop) |>
  select(simulacion, anio, age, asfr)

# export -----------------------------------------------------------------

save(trayectorias_fec, file = "resultados/proy_tgf.rda")
save(asfr_proyectadas, file = "resultados/proy_asfr.rda")


##################################################################### viejo

# # parámetros logística en media y desvío
# n_ajuste <- 10
# anio_ajuste <- 2015:2021
# anio_inicio <- 1960
# anio_fin <- 2075
# parametros_logistica <- tibble(
#   indicador_id = c("edad_media", "cv"),
#   indicador = factor(c("Edad media", "CV"), levels = c("Edad media", "CV")),
#   L = c(10, 0.15),
#   U = c(33, 0.5))

# # unir
# momentos_asfr <- bind_rows(
#   asfr_hfd |>
#     transmute(pais = Code, anio = Year, age = parse_number(as.character(Age)), asfr = ASFR),
#   asfr_2000a2025 |>
#     transmute(pais = "Argentina", anio = t, age, asfr)) |>
#   filter(age >= 15, age <= 49, !is.na(asfr), anio >= anio_inicio) |>
#   summarise(
#     tgf = sum(asfr),
#     edad_media = sum(age * asfr) / tgf,
#     desv_standard = sqrt(sum((age - edad_media)^2 * asfr) / tgf),
#     cv = desv_standard/edad_media,
#     .by = c(pais, anio)) |>
#   filter(tgf > 0)
# momentos_asfr |> 
#   pivot_longer(tgf:cv) |> 
#   ggplot(aes(anio, value, group = pais)) +
#   geom_line() +
#   facet_wrap(~name, scales = "free")

# # aplica logística en arg
# momentos_asfr_plot <- momentos_asfr |>
#   pivot_longer(
#     all_of(parametros_logistica$indicador_id),
#     names_to = "indicador_id",
#     values_to = "valor") |>
#   left_join(parametros_logistica, by = "indicador_id")
# momentos_arg_ajuste <- momentos_asfr_plot |>
#   filter(pais == "Argentina") |>
#   arrange(indicador_id, anio) |>
#   group_by(indicador_id) |>
#   filter(trunc(anio) %in% anio_ajuste) |>
#   ungroup() 
# momentos_arg_proy_plot <- momentos_arg_ajuste |>
#   summarise(
#     proyeccion = list(logistica_regresion(
#       y = valor,
#       t = anio,
#       U = first(U),
#       L = first(L),
#       t_proj = seq(floor(max(momentos_asfr$anio[momentos_asfr$pais == "Argentina"])) + 1, anio_fin, by = 1) + .5,
#       jump_off = TRUE)),
#     .by = c(indicador_id, indicador, L, U)) |>
#   unnest(proyeccion) |>
#   transmute(pais = "Argentina proyeccion", anio = t, indicador_id, indicador, L, U, valor = y)

# # plot resultado de proyección media y desvío
# limites_logistica <- parametros_logistica |>
#   pivot_longer(c(L, U), names_to = "limite", values_to = "valor")
# plot_momentos_logistica <- ggplot() +
#   geom_line(
#     data = momentos_asfr_plot |> filter(pais != "Argentina"),
#     aes(anio, valor, group = pais),
#     color = "grey70",
#     alpha = .35,
#     linewidth = .35) +
#   geom_point(
#     data = momentos_asfr_plot |> filter(pais != "Argentina"),
#     aes(anio, valor),
#     color = "grey70",
#     alpha = .18,
#     size = .5) +
#   geom_line(
#     data = momentos_asfr_plot |> filter(pais == "Argentina"),
#     aes(anio, valor),
#     color = "black",
#     linewidth = 1) +
#   geom_point(
#     data = momentos_asfr_plot |> filter(pais == "Argentina"),
#     aes(anio, valor),
#     color = "black",
#     size = 1.8) +
#   geom_point(
#     data = momentos_arg_ajuste,
#     aes(anio, valor),
#     color = "blue",
#     size = 2.4) +
#   geom_line(
#     data = momentos_arg_proy_plot,
#     aes(anio, valor),
#     color = "blue",
#     linewidth = 1.1,
#     linetype = 2) +
#   geom_hline(
#     data = limites_logistica |> 
#       filter(
#         (indicador_id == "edad_media" & limite == "U") |
#         (indicador_id == "cv" & limite == "L")
#       ),
#     aes(yintercept = valor),
#     color = "grey45",
#     linetype = 3) +
#   facet_wrap(~indicador, scales = "free_y", ncol = 1) +
#   theme_bw(base_size = 13) +
#   labs(x = "Anio", y = NULL)

# plot_momentos_logistica |>
#   ggsave(filename = "plots/plot_momentos_logistica_hfd_arg.pdf", height = 8, width = 8)

# momentos_arg_proy_plot_viejo <- momentos_arg_proy_plot
# momentos_arg_proy_plot <- momentos_arg_proy_plot_nuevo

# ar2024 <- asfr_2000a2025 |>
#   filter(t == max(t), age >= 15, age <= 49) |>
#   arrange(age)
# anio_base <- floor(max(ar2024$t))
# proy <- momentos_arg_proy_plot |>
#   select(anio, indicador_id, valor) |>
#   pivot_wider(names_from = indicador_id, values_from = valor) |>
#   transmute(
#     anio,
#     media = edad_media,
#     sd = cv*edad_media)
# asfr_prop <- beta_asfr(
#   age = ar2024$age,
#   asfr_0 = ar2024$asfr,
#   proy = proy,
#   anio_0 = anio_base,
#   transicion = 6)
# asfr_prop_diag <- asfr_prop |>
#   filter(anio > anio_base) |>
#   transmute(anio, age, prop)
# asfr_obs_diag <- ar2024 |>
#   transmute(age, prop = asfr / sum(asfr))
# plot_asfr_prop_diag <- ggplot() +
#   geom_line(
#     data = asfr_obs_diag,
#     aes(age, prop),
#     color = "#070707",
#     linewidth = 1.2) +
#   geom_point(
#     data = asfr_obs_diag,
#     aes(age, prop),
#     color = "#070707",
#     size = 1.7) +
#   geom_line(
#     data = asfr_prop_diag,
#     aes(age, prop, color = anio, group = anio),
#     linewidth = .9) +
#   geom_point(
#     data = asfr_prop_diag,
#     aes(age, prop, color = anio),
#     size = 1.4) +
#   scale_color_gradientn(
#     colors = c("#2C7BB6", "#FFFFBF", "#D7191C"),
#     breaks = pretty(asfr_prop_diag$anio, n = 5)) +
#   theme_minimal(base_size = 13) +
#   labs(
#     x = "Edad",
#     y = "ASFR/TGF",
#     color = "Anio")
# plot_asfr_prop_diag |>
#   ggsave(filename = "plots/plot_asfr_prop_diag.pdf", height = 6, width = 8)

# asfr_proyectadas <- trayectorias_fec |>
#   select(simulacion, anio, tgf = valor) |>
#   left_join(
#     asfr_prop |> filter(anio %in% unique(trayectorias_fec$anio)) |> select(anio, age, prop),
#     by = "anio",
#     relationship = "many-to-many") |>
#   mutate(asfr = tgf * prop) |>
#   select(simulacion, anio, age, asfr)

# # compare 2075 proyectada con ultima disponible para países HFD
# asfr_prop_hfd_ult <- asfr_hfd |>
#   transmute(
#     pais = Code,
#     anio = Year,
#     age = parse_number(as.character(Age)),
#     asfr = ASFR) |>
#   filter(age >= 15, age <= 49, !is.na(asfr)) |>
#   group_by(pais) |>
#   filter(anio == max(anio)) |>
#   ungroup() |>
#   group_by(pais, anio) |>
#   mutate(
#     tgf = sum(asfr),
#     prop = asfr / tgf) |>
#   ungroup() |>
#   filter(tgf > 0)
# asfr_prop_arg_comp <- bind_rows(
#   ar2024 |>
#     transmute(serie = "Argentina 2024", anio = t, age, prop = asfr / sum(asfr)),
#   asfr_prop |>
#     filter(anio == 2075) |>
#     transmute(serie = "Argentina 2075", anio, age, prop))
# plot_asfr_prop_hfd_arg <- ggplot() +
#   geom_line(
#     data = asfr_prop_hfd_ult,
#     aes(age, prop, group = pais),
#     color = "grey70",
#     alpha = .45,
#     linewidth = .35) +
#   geom_line(
#     data = asfr_prop_arg_comp,
#     aes(age, prop, color = serie),
#     linewidth = 1) +
#   scale_color_manual(values = c("Argentina 2024" = "black", "Argentina 2075" = "blue")) +
#   theme_bw(base_size = 10) +
#   labs(x = "Edad", y = "ASFR/TGF", color = NULL)
# plot_asfr_prop_hfd_arg |>
#   ggsave(filename = "plots/plot_asfr_prop_hfd_arg.pdf", height = 3.5, width = 5)

# # export -----------------------------------------------------------------

# save(trayectorias_fec, asfr_proyectadas, file = "resultados/traj_tgf_asfr.rda")
