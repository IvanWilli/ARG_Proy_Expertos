library(readxl)
library(tidyverse)
library(progressr)
library(future)
library(furrr)
library(patchwork)
library(writexl)
source("R/funs.R")

# preparar insumos y componentes ----------------------------------------

# fecundidad
load("resultados/proy_asfr.rda")
asfr_proyectadas <- asfr_proyectadas |>
  transmute(simulacion, anio = as.integer(anio), edad = age, asfr)

# mortalidad
load("resultados/proy_Sx.rda")

# migraciones
load("resultados/proy_tmn.rda")
load("resultados/proy_migr_sexo_edad.rda")
# dejar en memoria
mig_estructura_sexo <<- mig_estructura_sexo 
mig_estructura_sexo_edad <<-  mig_estructura_sexo_edad

# pop 2025
N0 <- read_xlsx("data/ARG_2000a2025.xlsx", sheet =  "pop_2000a2025_1Enero") |> filter(t == 2025)
N0_f <- N0 |> filter(sex == "M") |> arrange(age) |> pull(pop)
N0_m <- N0 |> filter(sex == "V") |> arrange(age) |> pull(pop)
cbind(N0_f, N0_m)

# indice de masculinidad al nacer
srb <- rep(105, length(2025:2074))

# proyectar por bloques --------------------------------------------------

set.seed(1010)
options(progressr.enable = TRUE)
handlers("txtprogressbar")
future::plan(multisession)
sims <- max(asfr_proyectadas$simulacion)
n_sim <- 3000 # sims/5 # 1000
ids_sim <- sample(sort(unique(asfr_proyectadas$simulacion)), size = n_sim, replace = FALSE)
tam_bloque <- n_sim/20 # 50 bloques de 20 trayectorias
bloques <- split(ids_sim, ceiling(seq_along(ids_sim) / tam_bloque))
dir.create("resultados/proyeccion_bloques", showWarnings = FALSE, recursive = TRUE)
archivos_bloques <- character(length(bloques))
for (b in seq_along(bloques)) {
  # b <- 1
  ids_bloque <- bloques[[b]]
  message(sprintf("Bloque %03d/%03d: %d simulaciones", b, length(bloques), length(ids_bloque)))
  sx_bloque <- Sx_mort |>
    filter(simulacion %in% ids_bloque, anio %in% 2025:2074)
  asfr_bloque <- asfr_proyectadas |>
    filter(simulacion %in% ids_bloque, anio %in% 2025:2075)
  mig_bloque <- trayectorias_mig |>
    filter(simulacion %in% ids_bloque, anio %in% 2025:2074)
  proyeccion_bloque <- with_progress({
    p <- progressor(along = ids_bloque)
    future_map_dfr(ids_bloque,\(s) {
        # s <- ids_bloque[1]
        out <- proyectar_simulacion(s, sx_bloque, asfr_bloque, mig_bloque)
        # p(sprintf("simulacion %s", s))
        out
      },.options = furrr_options(seed = TRUE)
    )
  })
  archivo_bloque <- sprintf("resultados/proyeccion_bloques/proyeccion_bloque_%03d.rda", b)
  save(proyeccion_bloque, file = archivo_bloque)
  archivos_bloques[[b]] <- archivo_bloque
  rm(sx_bloque, asfr_bloque, mig_bloque, proyeccion_bloque)
  gc()
}
future::plan(sequential)

# junta los bloques
proyeccion <- map_dfr(
  archivos_bloques,
  \(archivo) {
    env <- new.env(parent = emptyenv())
    load(archivo, envir = env)
    env$proyeccion_bloque
  }
)
save(proyeccion, file = "resultados/proyeccion.rda")

# resumen poblacion --------------------------------------------------------------

# total de pobl
poblacion_total_trajs <- proyeccion |>
  filter(ind == "pop") |>
  summarise(poblacion = sum(value), .by = c(simulacion, anio))
# percentiles
resumen_poblacion <- poblacion_total_trajs |> 
  summarise(
    p5 = quantile(poblacion, .05),
    p10 = quantile(poblacion, .1),
    p25 = quantile(poblacion, .25),
    p50 = quantile(poblacion, .50),
    p75 = quantile(poblacion, .75),
    p90 = quantile(poblacion, .90), 
    p95 = quantile(poblacion, .95),
    .by = anio)
# plot
poblacion_total_prev <- read_xlsx("data/ARG_2000a2025.xlsx", sheet = "pop_2000a2025_1Enero") |>
  group_by(anio = t) |>
  summarise(poblacion = sum(pop), .groups = "drop")
id_plot_pop <- sample(unique(poblacion_total_trajs$simulacion), size = 300, replace = F)
plot_poblacion_total <- ggplot() +
  geom_line(
    data = poblacion_total_trajs |> filter(simulacion %in% id_plot_pop),
    aes(anio, poblacion, group = simulacion),
    color = "grey70", linewidth = .3, alpha = .20) +
  geom_line(
    data = resumen_poblacion |>
      pivot_longer(p5:p95, names_to = "percentil", values_to = "poblacion") |>
      mutate(intervalo = case_when(
        percentil %in% c("p5", "p95") ~ "IC 90%", 
        percentil %in% c("p25", "p75") ~ "IC 50%", TRUE ~ "IC 80%")),
    aes(anio, poblacion, group = percentil, linetype = intervalo),
    color = "#4C78A8", linewidth = .65) +
  geom_line(
    data = resumen_poblacion,
    aes(anio, p50),
    color = "#1F4E79", linewidth = 1) +
  geom_line(
    data = poblacion_total_prev,
    aes(anio, poblacion),
    color = "black", linewidth = .9) +
  theme_bw(base_size = 13) +
  # scale_linetype_manual(name = NULL, values = c("IC 90%" = "dashed", "IC 95%" = "dotted")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " M")) +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "grey55"),
    axis.ticks = element_line(color = "grey55"),
    axis.text = element_text(color = "grey35"),
    axis.title = element_text(color = "grey25")
  ) +
  labs(x = "Año", y = "Población")
plot_poblacion_total |> 
  ggsave(filename = "plots/plot_poblacion_total.pdf", height = 8, width = 10)

# nacimientos, defunciones y miragción -----------------------------------------------

load("data/Pop_Births_Deaths_SM.RData")
bd_prev <- bind_rows(
  Births |>
    summarise(valor = sum(B), .by = yb) |>
    transmute(anio = yb, componente = "Nacimientos", valor),
  Deaths |>
    summarise(valor = sum(D), .by = yd) |>
    transmute(anio = yd, componente = "Defunciones", valor)
) |>
  filter(anio >= 2000)

bd_trajs <- proyeccion |>
  filter(ind %in% c("B", "D"), anio < 2075) |>
  transmute(
    simulacion, anio,
    componente = recode(ind, B = "Nacimientos", D = "Defunciones"),
    # , NM = "Migración Neta"),
    valor = value)
resumen_bd <- bd_trajs |>
  summarise(
    p025 = quantile(valor, .025),
    p5 = quantile(valor, .05),
    p50 = quantile(valor, .50),
    p95 = quantile(valor, .95),
    p975 = quantile(valor, .975),
    .by = c(componente, anio)
  )
id_plot_bd <- sample(unique(bd_trajs$simulacion), size = 300, replace = FALSE)
plot_nacimientos_defunciones <- ggplot() +
  geom_line(
    data = bd_trajs |> filter(simulacion %in% id_plot_bd),
    aes(anio, valor, group = interaction(simulacion, componente), color = componente),
    linewidth = .15, alpha = .10, show.legend = FALSE) +
  geom_line(
    data = resumen_bd |>
      pivot_longer(c(p025, p5, p95, p975), names_to = "percentil", values_to = "valor") |>
      mutate(intervalo = case_when(percentil %in% c("p5", "p95") ~ "IC 90%", TRUE ~ "IC 95%")),
    aes(anio, valor, group = interaction(componente, percentil), color = componente, linetype = intervalo),
    linewidth = .65) +
  geom_line(
    data = resumen_bd,
    aes(anio, p50, color = componente),
    linewidth = 1) +
  geom_line(
    data = bd_prev,
    aes(anio, valor, color = componente),
    linewidth = .9) +
  theme_bw(base_size = 13) +
  scale_color_manual(name = NULL, 
    values = c("Nacimientos" = "#2A9D8F", "Defunciones" = "#C44E52", "Migración Neta" = "#7534d6")) +
  scale_linetype_manual(name = NULL, values = c("IC 90%" = "dashed", "IC 95%" = "dotted")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "")) +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "grey55"),
    axis.ticks = element_line(color = "grey55"),
    axis.text = element_text(color = "grey35"),
    axis.title = element_text(color = "grey25")) +
  labs(x = "Año", y = "Eventos (miles)")
plot_nacimientos_defunciones |>
  ggsave(filename = "plots/plot_nacimientos_defunciones.pdf", height = 8, width = 10)

# piramide poblacional ----------------------------------------------------

piramide_2024 <- read_xlsx("data/ARG_2000a2025.xlsx", sheet = "pop_2000a2025_1Enero") |>
  filter(t == 2024) |>
  transmute(anio = 2024L, edad = age, sexo = case_when(sex == "M" ~ "Mujeres", sex == "V" ~ "Varones", TRUE ~ sex),
            tipo = "2024 observado", percentil = "2024 observado", poblacion = if_else(sexo == "Varones", -pop, pop))
piramide_2075 <- proyeccion |>
  filter(ind == "pop", anio == 2075, sexo %in% c("f", "m")) |>
  mutate(sexo = case_when(sexo == "f" ~ "Mujeres", sexo == "m" ~ "Varones")) |>
  summarise(p10 = quantile(value, .10), p50 = quantile(value, .50), p90 = quantile(value, .90), .by = c(anio, edad, sexo)) |>
  pivot_longer(c(p10, p50, p90), names_to = "percentil", values_to = "poblacion") |>
  mutate(tipo = "2075 proyeccion", poblacion = if_else(sexo == "Varones", -poblacion, poblacion))
id_plot_piramide <- sample(unique(proyeccion$simulacion), size = min(500, length(unique(proyeccion$simulacion))), replace = FALSE)
piramide_sim_2075 <- proyeccion |>
  filter(ind == "pop", anio == 2075, sexo %in% c("f", "m"), simulacion %in% id_plot_piramide) |>
  transmute(simulacion, edad, sexo = case_when(sexo == "f" ~ "Mujeres", sexo == "m" ~ "Varones"),
            poblacion = if_else(sexo == "Varones", -value, value)) |>
  arrange(simulacion, sexo, edad)

piramide_plot <- bind_rows(piramide_2024, piramide_2075) |>
  mutate(percentil = factor(percentil, levels = c("2024 observado", "p10", "p50", "p90"),
                            labels = c("2024 observado", "2075 p10", "2075 p50", "2075 p90"))) |>
  arrange(percentil, sexo, edad)

plot_piramide_poblacion <- ggplot(piramide_plot, aes(poblacion, edad, color = percentil, linetype = percentil, group = interaction(percentil, sexo))) +
  geom_path(data = piramide_sim_2075, aes(poblacion, edad, group = interaction(simulacion, sexo)),
            color = "grey70", linewidth = .15, alpha = .08, inherit.aes = FALSE) +
  geom_path(linewidth = .9) +
  geom_vline(xintercept = 0, color = "grey45") +
  scale_x_continuous(limits = c(-5e5, 5e5), labels = \(x) scales::label_number(scale = 1e-6, suffix = " M")(abs(x))) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5), expand = expansion(mult = 0, add = 0)) +
  scale_color_manual(values = c("2024 observado" = "black", "2075 p10" = "#4C78A8", "2075 p50" = "#1F4E79", "2075 p90" = "#4C78A8")) +
  scale_linetype_manual(values = c("2024 observado" = "solid", "2075 p10" = "dashed", "2075 p50" = "solid", "2075 p90" = "dashed")) +
  theme_bw(base_size = 13) +
  labs(x = "Poblacion", y = "Edad", color = NULL, linetype = NULL)

plot_piramide_poblacion |>
  ggsave(filename = "plots/plot_piramide_poblacion.pdf", height = 8, width = 10)

# razon de sexos por edad -------------------------------------------------

razon_sexos_2024 <- read_xlsx("data/ARG_2000a2025.xlsx", sheet = "pop_2000a2025_1Enero") |>
  filter(t == 2024, sex %in% c("M", "V")) |>
  select(edad = age, sexo = sex, pop) |>
  pivot_wider(names_from = sexo, values_from = pop) |>
  transmute(edad, percentil = "2024 observado", razon_sexos = 100 * V / M)
razon_sexos_2075 <- proyeccion |>
  filter(ind == "pop", anio == 2075, sexo %in% c("f", "m")) |>
  select(simulacion, edad, sexo, value) |>
  pivot_wider(names_from = sexo, values_from = value) |>
  transmute(simulacion, edad, razon_sexos = 100 * m / f) |>
  summarise(p10 = quantile(razon_sexos, .10), p50 = quantile(razon_sexos, .50), p90 = quantile(razon_sexos, .90), .by = edad) |>
  pivot_longer(c(p10, p50, p90), names_to = "percentil", values_to = "razon_sexos") |>
  mutate(percentil = recode(percentil, p10 = "2075 p10", p50 = "2075 p50", p90 = "2075 p90"))
razon_sexos_plot <- bind_rows(razon_sexos_2024, razon_sexos_2075) |>
  mutate(percentil = factor(percentil, levels = c("2024 observado", "2075 p10", "2075 p50", "2075 p90"))) |>
  arrange(percentil, edad)
plot_razon_sexos_edad <- ggplot(razon_sexos_plot, aes(edad, razon_sexos, color = percentil, linetype = percentil)) +
  geom_hline(yintercept = 100, color = "grey60", linetype = 3) +
  geom_line(linewidth = .9) +
  scale_color_manual(values = c("2024 observado" = "black", "2075 p10" = "#4C78A8", "2075 p50" = "#1F4E79", "2075 p90" = "#4C78A8")) +
  scale_linetype_manual(values = c("2024 observado" = "solid", "2075 p10" = "dashed", "2075 p50" = "solid", "2075 p90" = "dashed")) +
  theme_bw(base_size = 13) +
  scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10)) +
  labs(x = "Edad", y = "Varones por 100 mujeres", color = NULL, linetype = NULL)
plot_razon_sexos_edad |>
  ggsave(filename = "plots/plot_razon_sexos_edad.pdf", height = 8, width = 10)


# poblacion grandes grupos -----------------------------------------------------------------

pob_grupos_trajs <- proyeccion |>
  filter(ind == "pop") |>
  mutate(grupo_edad = case_when(edad <= 14 ~ "0-14", edad <= 64 ~ "15-64", TRUE ~ "65+")) |>
  summarise(poblacion = sum(value), .by = c(simulacion, anio, grupo_edad))
resumen_grupos <- pob_grupos_trajs |>
  summarise(p5 = quantile(poblacion, .05), p25 = quantile(poblacion, .25), p50 = quantile(poblacion, .50),
            p75 = quantile(poblacion, .75), p95 = quantile(poblacion, .95), .by = c(grupo_edad, anio))
dependencia_trajs <- pob_grupos_trajs |>
  pivot_wider(names_from = grupo_edad, values_from = poblacion) |>
  transmute(simulacion, anio, `0-14 / 15-64` = 100 * `0-14` / `15-64`, `65+ / 15-64` = 100 * `65+` / `15-64`, Total = 100 * (`65+`+ `0-14`)/ `15-64`) |>
  pivot_longer(c(`0-14 / 15-64`, `65+ / 15-64`, Total), names_to = "tipo_dependencia", values_to = "dependencia")
resumen_dependencia <- dependencia_trajs |>
  summarise(p5 = quantile(dependencia, .05), p25 = quantile(dependencia, .25), p50 = quantile(dependencia, .50),
            p75 = quantile(dependencia, .75), p95 = quantile(dependencia, .95), .by = c(tipo_dependencia, anio))
pob_grupos_prev <- read_xlsx("data/ARG_2000a2025.xlsx", sheet = "pop_2000a2025_1Enero") |>
  mutate(anio = t, grupo_edad = case_when(age <= 14 ~ "0-14", age <= 64 ~ "15-64", TRUE ~ "65+")) |>
  summarise(poblacion = sum(pop), .by = c(anio, grupo_edad))
dependencia_prev <- pob_grupos_prev |>
  pivot_wider(names_from = grupo_edad, values_from = poblacion) |>
  transmute(anio, `0-14 / 15-64` = 100 * `0-14` / `15-64`, `65+ / 15-64` = 100 * `65+` / `15-64`, Total = 100 * (`65+`+ `0-14`)/ `15-64`) |>
  pivot_longer(c(`0-14 / 15-64`, `65+ / 15-64`, Total), names_to = "tipo_dependencia", values_to = "dependencia")
id_plot_grupos <- sample(unique(pob_grupos_trajs$simulacion), size = min(300, length(unique(pob_grupos_trajs$simulacion))), replace = FALSE)

plot_poblacion_grupos <- ggplot() +
  geom_line(data = pob_grupos_trajs |> filter(simulacion %in% id_plot_grupos), aes(anio, poblacion, group = interaction(simulacion, grupo_edad), color = grupo_edad), linewidth = .12, alpha = .08, show.legend = FALSE) +
  geom_line(data = resumen_grupos |> pivot_longer(c(p5, p25, p75, p95), names_to = "percentil", values_to = "poblacion") |> mutate(intervalo = case_when(percentil %in% c("p5", "p95") ~ "IC 90%", TRUE ~ "IC 50%")),
            aes(anio, poblacion, group = interaction(grupo_edad, percentil), color = grupo_edad, linetype = intervalo), linewidth = .6) +
  geom_line(data = resumen_grupos, aes(anio, p50, color = grupo_edad), linewidth = 1) +
  geom_line(data = pob_grupos_prev, aes(anio, poblacion, color = grupo_edad), linewidth = .9) +
  theme_bw(base_size = 12) + scale_color_manual(name = NULL, values = c("0-14" = "#4C78A8", "15-64" = "#2A9D8F", "65+" = "#C44E52")) +
  scale_linetype_manual(name = NULL, values = c("IC 50%" = "dashed", "IC 90%" = "dotted")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " M")) + 
  labs(x = "Año", y = "", title = "A") + theme(legend.position = "bottom")

plot_dependencia <- ggplot() +
  geom_line(data = dependencia_trajs |> filter(simulacion %in% id_plot_grupos), aes(anio, dependencia, group = interaction(simulacion, tipo_dependencia), color = tipo_dependencia), linewidth = .12, alpha = .10, show.legend = FALSE) +
  geom_line(data = resumen_dependencia |> pivot_longer(c(p5, p25, p75, p95), names_to = "percentil", values_to = "dependencia") |> mutate(intervalo = case_when(percentil %in% c("p5", "p95") ~ "IC 90%", TRUE ~ "IC 50%")),
            aes(anio, dependencia, group = interaction(tipo_dependencia, percentil), color = tipo_dependencia, linetype = intervalo), linewidth = .6) +
  geom_line(data = resumen_dependencia, aes(anio, p50, color = tipo_dependencia), linewidth = 1) +
  geom_line(data = dependencia_prev, aes(anio, dependencia, color = tipo_dependencia), linewidth = .9) +
  theme_bw(base_size = 12) + scale_color_manual(name = NULL, values = c("0-14 / 15-64" = "#4C78A8", "65+ / 15-64" = "#C44E52", "Total" = "#575353")) +
  scale_linetype_manual(name = NULL, values = c("IC 50%" = "dashed", "IC 90%" = "dotted")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Año", y = "", title = "B") + theme(legend.position = "bottom")

plot_grupos_dependencia <- plot_poblacion_grupos + plot_dependencia
plot_grupos_dependencia |>
  ggsave(filename = "plots/plot_grupos_dependencia.pdf", height = 8, width = 12)

# tablas para exportar ---------------------------------------------------

resumir_ci <- function(x, by) {
  x |>
    summarise(
      p05 = quantile(valor, .05), p95 = quantile(valor, .95),
      p10 = quantile(valor, .10), p90 = quantile(valor, .90),
      p25 = quantile(valor, .25), p75 = quantile(valor, .75),
      mediana = quantile(valor, .50),
      .by = all_of(by))}
nacimientos <- proyeccion |>
  filter(ind == "B", anio < 2075) |>
  transmute(anio, simulacion, valor = value) |>
  resumir_ci("anio")
defunciones <- proyeccion |>
  filter(ind == "D", anio < 2075) |>
  transmute(anio, simulacion, valor = value) |>
  resumir_ci("anio")
migracion <- proyeccion |>
  filter(ind == "NM", anio < 2075) |>
  transmute(anio, simulacion, valor = value) |>
  resumir_ci("anio")
poblacion_total <- proyeccion |>
  filter(ind == "pop") |>
  summarise(valor = sum(value), .by = c(simulacion, anio)) |>
  resumir_ci("anio")
poblacion_total_sexo <- proyeccion |>
  filter(ind == "pop") |>
  summarise(valor = sum(value), .by = c(simulacion, anio, sexo)) |>
  resumir_ci(c("anio", "sexo"))
poblacion_edad <- proyeccion |>
  filter(ind == "pop") |>
  summarise(valor = sum(value), .by = c(simulacion, anio, edad)) |>
  resumir_ci(c("anio", "edad"))
poblacion_grupos_edad <- pob_grupos_trajs |>
  transmute(anio, simulacion, grupo_edad, valor = poblacion) |>
  resumir_ci(c("anio", "grupo_edad"))
relacion_dependencia <- dependencia_trajs |>
  transmute(anio, simulacion, tipo_dependencia, valor = dependencia) |>
  resumir_ci(c("anio", "tipo_dependencia"))
tablas_exportar <- list(
  nacimientos = nacimientos,
  defunciones = defunciones,
  migracion = migracion,
  poblacion_total = poblacion_total,
  poblacion_total_sexo = poblacion_total_sexo,
  poblacion_edad = poblacion_edad,
  poblacion_grupos_edad = poblacion_grupos_edad,
  relacion_dependencia = relacion_dependencia)
tablas_exportar <- tablas_exportar |>
  map(\(x) x |> mutate(across(where(is.numeric) & !any_of(c("anio", "edad")), \(z) round(z, 0))))
writexl::write_xlsx(tablas_exportar, "resultados/tablas_proyeccion.xlsx")


# additional analysis ---------------------------------------------------

# cual es la probabilidad de que los nacimientos sean mayores a las defunciones antes de 2040?
bd_antes_2040 <- proyeccion |>
  filter(ind %in% c("B", "D"), anio == 2039) |>
  select(simulacion, ind, value) |>
  pivot_wider(names_from = ind, values_from = value) |>
  mutate(yes = B < D) |> 
  summarise(sum(yes)/n())

# La probabilidad de que el máximo se produzca antes de 2040, 2050, 2060...
max_antes_yyyy <- proyeccion |>
  filter(ind %in% c("pop")) |>
  summarise(pop = sum(value), .by = c(anio, simulacion)) |> 
  group_by(simulacion) |> arrange(-pop) |> slice(1) |> ungroup() |> 
  summarise(antes2040 = sum(anio<2040)/n(), antes2050 = sum(anio<2050)/n(), antes2060 = sum(anio<2060)/n())

# poblacion edad escolar
poblacion_edad |> 
  summarise(
    mediana = sum(mediana[edad %in% 6:17]),
    p05 = sum(p05[edad %in% 6:17]),
    p90 = sum(p90[edad %in% 6:17]), 
    .by = anio) |> as.data.frame()

# pobl 50 años mas/menos
proyeccion |>
  filter(ind %in% c("pop"), anio == 2075) |>
  summarise(
    pop50mas = sum(value[edad>=50]), 
    pop50menos = sum(value[edad<50]), .by = c(simulacion)) |> 
  mutate(yes = pop50mas > pop50menos) |> 
  summarise(sum(yes)/n())

# la edad mediana
edad_media_mediana <- proyeccion |>
  filter(ind == "pop", anio %in% c(2025, 2050, 2075)) |>
  summarise(pop = sum(value), .by = c(simulacion, anio, edad)) |>
  arrange(simulacion, anio, edad) |>
  summarise(`Edad media` = weighted.mean(edad, pop),
            `Edad mediana` = edad[which(cumsum(pop) >= sum(pop)/2)[1]],
            .by = c(simulacion, anio)) |>
  pivot_longer(c(`Edad media`, `Edad mediana`), names_to = "indicador", values_to = "edad") |>
  summarise(li90 = quantile(edad, .05), mediana = quantile(edad, .5), ls90 = quantile(edad, .95),
            .by = c(anio, indicador))

# probabilidad ratio dep total sea mayor a 100
save(proyeccion, file = "resultados/proyeccion.rda")

# lexis grid
plot_lexis_poblacion_edad <- bind_rows(
  poblacion_edad |> select(t = anio, age = edad, pop = mediana) |> filter(t>2025),
  read_xlsx("data/ARG_2000a2025.xlsx", sheet = "pop_2000a2025_1Enero") |> 
    summarise(pop = sum(pop), .by = c(t, age))) |> 
  ggplot(aes(t, age, fill = pop)) +
  geom_tile(width = 1, height = 1) + geom_vline(xintercept = 2025, col = "white", linetype = 2) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(2000, 2075, 10), labels = seq(2000, 2075, 10)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10), labels = seq(0, 100, 10)) +
  coord_fixed(ratio = 1) +
  scale_fill_viridis_c(labels = scales::label_number(scale = 1e-3, suffix = " mil")) +
  theme_bw(base_size = 13) + theme(panel.grid = element_blank()) +
  labs(x = "Año", y = "Edad", fill = "Población")
plot_lexis_poblacion_edad |>
  ggsave(filename = "plots/plot_lexis_poblacion_edad.pdf", height = 8, width = 10)
