
library(tidyverse)
library(readxl)
library(vital)
# library(anomalize)
# library(weird)
# library(ggrepel)
# library(broom)
# library(writexl)
# library(dplyr)
# library(ggplot2)
# library(tsibble)

######Lectura de lo datos

# Población
pop <- read_excel("data/pop_2000a2025_1Enero.xlsx") |>
  rename(
    Age = age,
    Year = t,
    Exposure = pop,
    Sex = Sexo
  ) |>
  mutate(
    Year = as.integer(Year),
    Age = as.integer(Age)
  ) |>
  filter(Year < 2025) |> 
  # Colapsar edades >= 90
  mutate(Age = ifelse(Age >= 90, 90, Age)) |>
  group_by(Year, Sex, Age) |>
  summarise(Exposure = sum(Exposure), .groups = "drop")

# Nacimientos (asumo que son por edad de la madre)
births <- read_excel("data/births_2000a2024.xlsx") |>
  rename(
    Age = age,
    Year = yb,
    Sex = sex,
    Births = B
  ) |>
  mutate(
    Year = as.integer(Year),
    Age = as.integer(Age)
  ) |>
  # Colapsar edades >= 90
  mutate(Age = ifelse(Age >= 90, 90, Age)) |>
  group_by(Year, Sex, Age) |>
  summarise(Births = sum(Births), .groups = "drop")

#Selección de mujeres en edad fértil para fecundidad
pop_fem <- pop |>
  filter(Sex == "M", Age >= 11, Age <= 49)

#Población + nacimientos
data_births <- pop_fem |>
  left_join(
    births |> summarise(Births = sum(Births), .by = c(Age, Year)), 
    by = c("Age", "Year"))

#check
data_births |> 
  mutate(Fertility = Births / Exposure) |>
  summarise(TFR = sum(Fertility), .by = Year) |>
  ggplot(aes(Year, TFR)) + geom_line() + geom_point() + theme_bw(base_size = 13) + labs(x = "Año", y = "TGF")

#Objetos tipo vital

#Vital de fecundidad (solo mujeres 11–49)
vital_births <- data_births |>
  as_vital(
    index = Year,
    key = c(Age),
    .age = "Age",
    .pop = "Exposure"
  )

ar_fertility <- vital_births |>
  mutate(
    Fertility = Births / Exposure,
    Fertility = ifelse(is.na(Fertility), 1, Fertility)
  )

################# TGF historica y proyectada hasta 2075
# Tasas por mujer y edades simples de 11 a 49: TGF = suma por edad.
# La serie historica usa los mismos datos corregidos que el modelo.
tgf_previa <- ar_fertility |>
  as_tibble() |>
  group_by(Year) |>
  summarise(TGF = sum(Fertility), .groups = "drop")

anio_fin_tgf <- 2075L
ultimo_anio_tgf <- max(ar_fertility$Year)
h_tgf <- anio_fin_tgf - ultimo_anio_tgf
stopifnot(h_tgf > 0)

fit_fert_tgf <- ar_fertility |>
  smooth_fertility(Fertility) |>
  model(fdm = FDM(log(.smooth), order = 3))

set.seed(20261108)
n_sim_tgf <- 1000L
# Namespace explicito: weird carga distributional::generate.
sim_fert_tgf <- fit_fert_tgf |>
  vital::generate(h = h_tgf, times = n_sim_tgf, bootstrap = FALSE)

# .sim ya esta en la escala original. Sumar antes de calcular cuantiles.
tgf_trayectorias <- sim_fert_tgf |>
  as_tibble() |>
  group_by(Year, .model, .rep) |>
  summarise(TGF = sum(.sim), .groups = "drop")

# Intervalo predictivo puntual del 90%: percentiles 5 y 95 de la TGF.
tgf_resumen <- tgf_trayectorias |>
  group_by(Year, .model) |>
  summarise(
    p5 = quantile(TGF, 0.05),
    p50 = median(TGF),
    p95 = quantile(TGF, 0.95),
    .groups = "drop"
  )
tgf_mediana <- tgf_resumen |>
  transmute(Year, .model, TGF = p50)

# Estetica de fec.R: trayectorias negras tenues, banda gris y mediana negra.
reps_tgf <- sample(unique(tgf_trayectorias$.rep), size = 300L)
# La vista incluye todo el intervalo 90% y la historia; algunas trayectorias
# extremas pueden quedar fuera de vista, sin alterar los datos ni cuantiles.
limite_tgf <- max(tgf_previa$TGF, tgf_resumen$p95) * 1.05

plot_tgf_fdm <- ggplot() +
  geom_line(
    data = tgf_trayectorias |> filter(.rep %in% reps_tgf),
    aes(Year, TGF, group = .rep),
    linewidth = .1, alpha = .1
  ) +
  geom_ribbon(
    data = tgf_resumen,
    aes(Year, ymin = p5, ymax = p95),
    alpha = .18
  ) +
  geom_line(
    data = tgf_resumen,
    aes(Year, p50), linewidth = 1
  ) +
  geom_line(data = tgf_previa, aes(Year, TGF)) +
  geom_vline(
    xintercept = ultimo_anio_tgf + .5,
    col = "grey60", linetype = 2
  ) +
  coord_cartesian(
    xlim = c(2015, 2075),
    ylim = c(0, 2)
  ) +
  theme_bw(base_size = 13) +
  labs(x = "A\u00f1o", y = "TGF")

print(plot_tgf_fdm)
plot_tgf_fdm |>
  ggsave(filename = "plots/plot_tgf_fdm.pdf", height = 3.5, width = 5)

