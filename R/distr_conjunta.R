# creacion de distribuciones conjuntas en cada pregunta de cada componente

library(readxl)
library(tidyverse)
library(rmetalog)
library(writexl)
library(scales)
library(patchwork)
set.seed(1010)

# respuestas de encuesta -------------------------------------------------
respuestas <- read_xlsx(
  "encuesta/Base_expertos.xlsx",
  sheet = "usada",
  range = "A1:AQ22")

# solo las que se decide incluir - Binstock con poca respuesta
respuestas <- respuestas[respuestas$incluida == 1, ]
respuestas <- respuestas |> 
  mutate(
    id = 1:nrow(respuestas),
    resp_fecundidad = if_any(matches("^(tgf_min|anio_min|tgf_2075)_"), ~ !is.na(.x)),
    resp_mortalidad = if_any(matches("^(e0_mujeres_2030|e0_mujeres_2075|brecha_e0_mujeres_varones_2075)_"), ~ !is.na(.x)),
    resp_migracion = if_any(matches("^(inmigrantes_2025_2029|emigrantes_2025_2029|tmn_2075)_"), ~ !is.na(.x)))

# perfil de encuestados ------------------------------------------------------

distr_expertise <- bind_rows(
    respuestas |> 
      select(id, tipo = tipo_analista_fecundidad, resp = resp_fecundidad) |> 
      mutate(componente = "Fecundidad"),
    respuestas |> 
      select(id, tipo = tipo_analista_mortalidad, resp = resp_mortalidad) |> 
      mutate(componente = "Mortalidad"),
    respuestas |> 
      select(id, tipo = tipo_analista_migracion, resp = resp_migracion) |> 
      mutate(componente = "Migración")) |>
  mutate(tipo = ifelse(resp, tipo, "No Responde")) |> 
  count(componente, tipo)

plot_expertise <- distr_expertise |>
  mutate(
    pct = n / sum(n),
    etiqueta = paste0(percent(pct, accuracy = 1), "\n(n=", n, ")"), .by = componente) |>
  ggplot(aes(x = "", y = n, fill = tipo)) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = .5), size = 3.5) +
  coord_polar(theta = "y") +
  facet_wrap(~componente, nrow = 1) +
  theme_void(base_size = 13) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "bottom") +
  labs(fill = "Tipo")
plot_expertise |> 
  ggsave(filename = "plots/plot_expertise.pdf", height = 8, width = 8, device = cairo_pdf)

# variables a modelar ----------------------------------------------------

#   L = limite bajo
#   M = mediana
#   U = limite alto
#   confianza = probabilidad asignada al intervalo L-U
#   Los limites inferior y superior son limites técnicos, para evitar absurdos

indicadores <- tibble::tribble(
  ~prefijo, ~etiqueta,~columna_tipo,~limite_inferior, ~limite_superior,
  # FECUNDIDAD (hay un 0.3 en L, id=5)
  "tgf_min", "TGF mínima 2025-2075", "tipo_analista_fecundidad", 0.2, 2,
  "anio_min","Año del mínimo de TGF", "tipo_analista_fecundidad", 2024, 2076,
  "tgf_2075", "TGF en 2075","tipo_analista_fecundidad",0.2, 2.5,
  # MORTALIDAD (en relación a valores 95% de Japón - WPP24)
  "e0_mujeres_2030",  "e(0) mujeres 2030","tipo_analista_mortalidad", 69.00, 96.00,
  "e0_mujeres_2075","e(0) mujeres 2075","tipo_analista_mortalidad", 70.00, 101.00,
  "brecha_e0_mujeres_varones_2075","Brecha e(0) mujeres-varones 2075","tipo_analista_mortalidad",0.00,12.00,
  # MIGRACION (para ser muy conservadores en 2075)
  "inmigrantes_2025_2029","Inmigrantes anuales promedio 2025-2029","tipo_analista_migracion",0,200000,
  "emigrantes_2025_2029","Emigrantes anuales promedio 2025-2029","tipo_analista_migracion",0,200000,
  "tmn_2075","Tasa de migración neta en 2075","tipo_analista_migracion",-10.00,10.00
)

# modelo probabilistico en cada encuestado ----------------------------------
# Objetivo: convertir cada respuesta individual en una distribucion continua usando metalog

metalog_resp <- map_dfr(indicadores$prefijo, function(p) {
  print(p)
  # p <- "tgf_min"
  # Tomo solo quienes respondieron completa esa pregunta.
  id_rta <- which(
    !is.na(respuestas[[paste0(p, "_L")]]) &
      !is.na(respuestas[[paste0(p, "_M")]]) &
      !is.na(respuestas[[paste0(p, "_U")]]) &
      !is.na(respuestas[[paste0(p, "_confianza")]]))
  map_dfr(id_rta, function(i) {
    print(i)
    # i = 16
    L <- as.numeric(respuestas[[paste0(p, "_L")]][i])
    M <- as.numeric(respuestas[[paste0(p, "_M")]][i])
    U <- as.numeric(respuestas[[paste0(p, "_U")]][i])
    C <- as.numeric(respuestas[[paste0(p, "_confianza")]][i])
    probs <- c(
      (1 - C) / 2,
      0.50,
      1 - (1 - C) / 2)
    ajuste <- metalog(
      x = c(L, M, U),
      probs = probs,
      bounds = c(
        indicadores$limite_inferior[indicadores$prefijo == p],
        indicadores$limite_superior[indicadores$prefijo == p]),
      boundedness = "b",
      term_limit = 3,
      term_lower_bound = 3)
    val <- ajuste$Validation
    tibble(
      indicator = p,
      id = respuestas$id[i],
      row_id = i,
      ajuste = list(ajuste),
      valid = val$valid[val$term == 3][1],
      method = val$method[val$term == 3][1]
    )
  })
})

# ejemplo de metalog individual para informe -----------------------------------------
id_sample <- sample(unique(metalog_resp$id), size = 1)
id_sample <- 10
datos_sample <- metalog_resp |>
  filter(
    id == id_sample,
    tolower(valid) == "yes") |>
  left_join(
    indicadores |> select(indicator = prefijo, etiqueta),
    by = "indicator") |>
  mutate(
    etiqueta = factor(etiqueta, levels = indicadores$etiqueta),
    datos = map(
      ajuste,
      \(a) tibble(
        valor = qmetalog(
          a,
          y = seq(.001, .999, length.out = 10000),
          term = 3)))) |>
  select(indicator, etiqueta, datos) |>
  unnest(datos)
params_sample_wide <- respuestas |>
  filter(id == id_sample) |>
  select(id) |>
  slice(rep(1, nrow(indicadores))) |>
  bind_cols(indicadores |> select(indicator = prefijo, etiqueta)) |>
  rowwise() |>
  transmute(
    indicator,
    etiqueta = factor(etiqueta, levels = indicadores$etiqueta),
    L = respuestas[[paste0(indicator, "_L")]][match(id_sample, respuestas$id)],
    M = respuestas[[paste0(indicator, "_M")]][match(id_sample, respuestas$id)],
    U = respuestas[[paste0(indicator, "_U")]][match(id_sample, respuestas$id)],
    confianza = respuestas[[paste0(indicator, "_confianza")]][match(id_sample, respuestas$id)]) |>
  ungroup()
params_sample <- params_sample_wide |>
  pivot_longer(
    c(L, M, U),
    names_to = "tipo",
    values_to = "valor")
confianza_sample <- params_sample_wide |>
  transmute(
    indicator, etiqueta,
    valor = (L + U) / 2,
    etiqueta_confianza = percent(confianza, accuracy = 1))
plot_metalog_indiv <- ggplot(datos_sample, aes(valor)) +
  geom_density(linewidth = 1) +
  geom_vline(data = params_sample, aes(xintercept = valor), linetype = "dashed", linewidth = .7, col = "grey50") +
  geom_text(data = params_sample, aes(x = valor, y = Inf, label = tipo), vjust = 1.5, hjust = 1.2, size = 3) +
  geom_text(data = confianza_sample, aes(x = valor, y = 0, label = etiqueta_confianza), vjust = -0.5, color = "orange", size = 3.2) +
  facet_wrap(~etiqueta, scales = "free", ncol = 3) +
  theme_minimal(base_size = 13) + 
  labs(x = "Valor", y = "Densidad")
plot_metalog_indiv |> 
  ggsave(filename = "plots/plot_metalog_indiv.pdf", height = 8, width = 10, device = cairo_pdf)

# scaterplot entre preguntas del mismo experto -----------------------------

cuantiles_indiv <- metalog_resp |>
  filter(tolower(valid) == "yes") |>
  mutate(q = map(ajuste, \(a) tibble(
        prob = c("p10", "p50", "p90"),
        valor = qmetalog(a, y = c(.10, .50, .90), term = 3)))) |>
  select(id, indicator, q) |>
  unnest(q) |>
  pivot_wider(names_from = prob, values_from = valor)
tipo_niveles <- c("Experto", "Informado", "No especializado")
tipo_colores <- c("Experto" = "#1b9e77", "Informado" = "#d95f02", "No especializado" = "#7570b3")
escala_expertise <- scale_color_manual(
  name = "Expertise",
  values = tipo_colores,
  limits = tipo_niveles,
  drop = FALSE)
corr_pair <- function(x_ind, y_ind, tipo_col) {
  cuantiles_indiv |>
    filter(indicator %in% c(x_ind, y_ind)) |>
    pivot_wider(
      id_cols = id,
      names_from = indicator,
      values_from = c(p10, p50, p90),
      names_glue = "{indicator}_{.value}") |>
    left_join(
      respuestas |> transmute(id, tipo = .data[[tipo_col]]),
      by = "id") |>
    mutate(tipo = factor(tipo, levels = tipo_niveles)) |>
    drop_na()
}

# correlaciona e0s 2030 y 2075?
datos_corr_e0s <- corr_pair("e0_mujeres_2075", "e0_mujeres_2030", "tipo_analista_mortalidad")
plot_corr_e0s <- ggplot(datos_corr_e0s, aes(e0_mujeres_2075_p50, e0_mujeres_2030_p50, col = tipo)) +
  geom_segment(aes(x = e0_mujeres_2075_p10, xend = e0_mujeres_2075_p90, y = e0_mujeres_2030_p50, yend = e0_mujeres_2030_p50), alpha = .45) +
  geom_segment(aes(x = e0_mujeres_2075_p50, xend = e0_mujeres_2075_p50, y = e0_mujeres_2030_p10, yend = e0_mujeres_2030_p90), alpha = .45) +
  geom_point(size = 2.5) +
  escala_expertise +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# correlaciona e0s 2075 y brecha?
datos_corr_e0_brecha <- corr_pair("e0_mujeres_2075", "brecha_e0_mujeres_varones_2075", "tipo_analista_mortalidad")
plot_corr_e0_brecha <- ggplot(datos_corr_e0_brecha, aes(e0_mujeres_2075_p50, brecha_e0_mujeres_varones_2075_p50, col = tipo)) +
  geom_segment(aes(x = e0_mujeres_2075_p10, xend = e0_mujeres_2075_p90, y = brecha_e0_mujeres_varones_2075_p50, yend = brecha_e0_mujeres_varones_2075_p50), alpha = .45) +
  geom_segment(aes(x = e0_mujeres_2075_p50, xend = e0_mujeres_2075_p50, y = brecha_e0_mujeres_varones_2075_p10, yend = brecha_e0_mujeres_varones_2075_p90), alpha = .45) +
  geom_point(size = 2.5) +
  escala_expertise +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# correlaciona anio y valor minimo de la TGF?
datos_corr_tgf <- corr_pair("tgf_min", "anio_min", "tipo_analista_fecundidad")
plot_corr_tgf <- ggplot(datos_corr_tgf, aes(tgf_min_p50, anio_min_p50, col = tipo)) +
  geom_segment(aes(x = tgf_min_p10, xend = tgf_min_p90, y = anio_min_p50, yend = anio_min_p50), alpha = .45) +
  geom_segment(aes(x = tgf_min_p50, xend = tgf_min_p50, y = anio_min_p10, yend = anio_min_p90), alpha = .45) +
  geom_point(size = 2.5) +
  escala_expertise +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# correlaciona Migr neta? 
poblacion_referencia_2027 <- 46540071 # INDEC Proy 1/7/2027
set.seed(1011)
tmn_2025_2029_indiv <- metalog_resp |>
  filter(indicator == "inmigrantes_2025_2029", tolower(valid) == "yes") |>
  select(id, ajuste_inm = ajuste) |>
  inner_join(
    metalog_resp |>
      filter(indicator == "emigrantes_2025_2029", tolower(valid) == "yes") |>
      select(id, ajuste_emi = ajuste),
    by = "id") |>
  rowwise() |>
  mutate(
    tmn = list({
      vals <- (
        qmetalog(ajuste_inm, y = runif(10000, .001, .999), term = 3) -
          qmetalog(ajuste_emi, y = runif(10000, .001, .999), term = 3)
      ) / poblacion_referencia_2027 * 1000
      tibble(
        tmn_2025_2029_p10 = quantile(vals, .10, type = 8),
        tmn_2025_2029_p50 = quantile(vals, .50, type = 8),
        tmn_2025_2029_p90 = quantile(vals, .90, type = 8))
    })) |>
  ungroup() |>
  select(id, tmn) |>
  unnest(tmn)
datos_corr_tmn <- cuantiles_indiv |>
  filter(indicator == "tmn_2075") |>
  transmute(id, tmn_2075_p10 = p10, tmn_2075_p50 = p50, tmn_2075_p90 = p90) |>
  inner_join(tmn_2025_2029_indiv, by = "id") |>
  left_join(respuestas |> transmute(id, tipo = tipo_analista_migracion), by = "id") |>
  mutate(tipo = factor(tipo, levels = tipo_niveles)) |>
  drop_na()
plot_corr_tmn <- ggplot(datos_corr_tmn, aes(tmn_2025_2029_p50, tmn_2075_p50, col = tipo)) +
  geom_segment(aes(x = tmn_2025_2029_p10, xend = tmn_2025_2029_p90, y = tmn_2075_p50, yend = tmn_2075_p50), alpha = .45) +
  geom_segment(aes(x = tmn_2025_2029_p50, xend = tmn_2025_2029_p50, y = tmn_2075_p10, yend = tmn_2075_p90), alpha = .45) +
  geom_point(size = 2.5) +
  escala_expertise +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# scaterplot conjunto de componentes 
legend_corr <- tibble(
  tipo = factor(tipo_niveles, levels = tipo_niveles),
  x = 1:3,
  y = 1) |>
  ggplot(aes(x, y, color = tipo)) +
  geom_point(size = 2.5, alpha = 0) +
  escala_expertise +
  theme_void(base_size = 13) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))
plot_corr_conjunto <- (
  (
    (plot_corr_e0s +
       labs(x = "e(0) Mujeres 2075", y = "e(0) Mujeres 2030")) |
      (plot_corr_e0_brecha +
         labs(x = "e(0) Mujeres 2075", y = "e(0) Brecha 2075"))
  ) /
    (
      (plot_corr_tgf +
         labs(x = "TGF Mín 2025-2075", y = "Año TGF Mín")) |
        (plot_corr_tmn +
           labs(x = "TMN 2025-2029", y = "TMN 2075"))
    )
) / legend_corr +
  plot_layout(heights = c(1, 1, .08))
plot_corr_conjunto |> 
  ggsave(filename = "plots/plot_corr_conjunto.pdf", height = 8, width = 11, device = cairo_pdf)

# distribucion colectiva en cada pregunta --------------------------------------------------

# cuántas realizaciones tomo
n_pool <- 500000
# el peso
peso_tipo <- c(Experto = 5, Informado = 3, `No especializado`=1)
pool_resp <- map_dfr(seq_len(nrow(indicadores)), # para cada indicador
  function(h){
  # h <- 2
  p   <- indicadores$prefijo[h]
  col <- indicadores$columna_tipo[h]
  print(p)
  # Metalogs validas para esta pregunta + peso de expertise
  aux <- metalog_resp |>
    filter(indicator == p, tolower(valid) == "yes") |>
    mutate(peso = peso_tipo[respuestas[[col]][match(id, respuestas$id)]])
  # Sorteamos N casos de expertos 
  sorteos <- bind_rows(
    # segun peso expertise
    tibble(
      agregacion = "Ponderado por expertise",
      k = sample(seq_len(nrow(aux)), n_pool, replace = TRUE, prob = aux$peso),
      u = runif(n_pool, min = .0005, max = .9995)
    ),
    # como comparacion
    tibble(
      agregacion = "Pesos iguales",
      k = sample(seq_len(nrow(aux)), n_pool, replace = TRUE),
      u = runif(n_pool, min = .0005, max = .9995)
    ))
  # Transformamos cada u en un valor usando la metalog
  # del experto que fue seleccionado.
  sorteos |>
    group_by(agregacion, k) |>
    mutate(
      valor = qmetalog(
        aux$ajuste[[first(k)]],
        y = u,
        term = 3)) |>
    ungroup() |>
    transmute(
      indicator = p,
      etiqueta = indicadores$etiqueta[h],
      agregacion,
      valor)
  })
glimpse(pool_resp) # 9 ind * n_pool realiz * 2 ponderaciones

# tabla resumen (termina de fomatear en excel)
orden_tabla_resumen <- c(
  "TGF mínima 2025-2075",
  "Año del mínimo de TGF",
  "TGF en 2075",
  "e(0) mujeres 2030",
  "e(0) mujeres 2075",
  "Brecha e(0) mujeres-varones 2075",
  "Emigrantes anuales promedio 2025-2029",
  "Inmigrantes anuales promedio 2025-2029",
  "Tasa de migración neta en 2075")
resumen_distribuciones_conjuntas <- pool_resp |> 
  filter(agregacion == "Ponderado por expertise") |> 
  group_by(indicator, etiqueta) |>
  summarise(
    media  = mean(valor),
    moda = {d <- density(valor); d$x[which.max(d$y)]},
    CV = sd(valor)/media,
    p05 = quantile(valor, .05, type = 8),
    p10 = quantile(valor, .10, type = 8),
    p25 = quantile(valor, .25, type = 8),
    p50 = quantile(valor, .50, type = 8),
    p75 = quantile(valor, .75, type = 8),
    p90 = quantile(valor, .90, type = 8),
    p95 = quantile(valor, .95, type = 8),
    .groups = "drop") |>
  left_join(
    indicadores |> select(indicator = prefijo, limite_inferior, limite_superior),
    by = "indicator") |>
  mutate(
    disp_p90p10_rango = (p90 - p10) / (limite_superior - limite_inferior),
    etiqueta = factor(etiqueta, levels = orden_tabla_resumen)) |>
  arrange(etiqueta)
resumen_distribuciones_conjuntas |> 
  write_xlsx("resultados/resumen_distribuciones_conjuntas.xlsx")

# plot de distribuciones individuales y conjuntas
density_tbl <- function(x) {
  d <- density(x)
  tibble(x = d$x, y = d$y)}
componentes_indicadores <- list(
  Fecundidad = c("tgf_min", "anio_min", "tgf_2075"),
  Mortalidad = c("e0_mujeres_2030", "e0_mujeres_2075", "brecha_e0_mujeres_varones_2075"),
  Migracion = c("inmigrantes_2025_2029", "emigrantes_2025_2029", "tmn_2075"))
componentes_etiquetas <- c(Fecundidad = "Fecundidad", Mortalidad = "Mortalidad", Migracion = "Migración")
etiquetas_encuesta_plot <- tribble(
  ~indicator, ~etiqueta_plot,
  "tgf_min", "Valor mínimo de la TGF\nen el período 2025 a 2075",
  "anio_min", "Año en que se alcanza\nel valor mínimo de TGF",
  "tgf_2075", "Valor de la TGF\nen el año 2075",
  "e0_mujeres_2030", "Valor de la e(0) de mujeres\nen 2030",
  "e0_mujeres_2075", "Valor de la e(0) de mujeres\nen 2075",
  "brecha_e0_mujeres_varones_2075", "Diferencia entre la e(0) de mujeres\ny de varones en 2075",
  "inmigrantes_2025_2029", "Cantidad de inmigrantes anuales\npromedio durante 2025-2029",
  "emigrantes_2025_2029", "Cantidad de emigrantes anuales\npromedio durante 2025-2029",
  "tmn_2075", "Tasa de migración neta\nen 2075") |>
  mutate(etiqueta_plot = factor(etiqueta_plot, levels = etiqueta_plot))
indicadores_plot <- imap_dfr(
  componentes_indicadores,
  \(inds, comp) tibble(
    indicator = inds,
    componente = comp
  )) |>
  left_join(
    indicadores |> select(indicator = prefijo, etiqueta),
    by = "indicator") |>
  left_join(etiquetas_encuesta_plot, by = "indicator")
rangos_plot <- pool_resp |>
  filter(agregacion == "Ponderado por expertise") |>
  group_by(indicator, etiqueta) |>
  summarise(
    xmin = quantile(valor, 0.01, type = 8),
    xmax = quantile(valor, 0.99, type = 8),
    .groups = "drop")
limites_ejes_plot <- tribble(
  ~indicator, ~xmin_eje, ~xmax_eje,
  "tgf_min", 0.4, 2,
  "tgf_2075", 0.4, 2,
  "e0_mujeres_2030", 75, 100,
  "e0_mujeres_2075", 75, 100)
limites_ejes_blank <- limites_ejes_plot |>
  pivot_longer(c(xmin_eje, xmax_eje), values_to = "x") |>
  left_join(indicadores_plot, by = "indicator") |>
  mutate(y = 0)
datos_individuales_plot <- metalog_resp |>
  filter(tolower(valid) == "yes") |>
  left_join(indicadores_plot, by = "indicator") |>
  transmute(
    indicator, etiqueta, etiqueta_plot, componente, id,
    dens = map(ajuste, \(a) density_tbl(qmetalog(a, y = seq(.001, .999, length.out = 4000), term = 3)))) |>
  unnest(dens) |>
  left_join(rangos_plot, by = c("indicator", "etiqueta")) |>
  left_join(limites_ejes_plot, by = "indicator") |>
  mutate(
    xmin_plot = coalesce(xmin_eje, xmin),
    xmax_plot = coalesce(xmax_eje, xmax)) |>
  filter(x >= xmin_plot, x <= xmax_plot) |>
  select(-xmin, -xmax, -xmin_eje, -xmax_eje, -xmin_plot, -xmax_plot)
datos_finales_plot <- pool_resp |>
  left_join(indicadores_plot, by = c("indicator", "etiqueta")) |>
  group_by(indicator, etiqueta, etiqueta_plot, componente, agregacion) |>
  summarise(
    dens = list(density_tbl(valor)),
    .groups = "drop"
  ) |>
  unnest(dens) |>
  left_join(rangos_plot, by = c("indicator", "etiqueta")) |>
  left_join(limites_ejes_plot, by = "indicator") |>
  mutate(
    xmin_plot = coalesce(xmin_eje, xmin),
    xmax_plot = coalesce(xmax_eje, xmax)) |>
  filter(x >= xmin_plot, x <= xmax_plot) |>
  select(-xmin, -xmax, -xmin_eje, -xmax_eje, -xmin_plot, -xmax_plot)
hacer_plot_componente <- function(comp){
  # comp <- "Mortalidad"
  ggplot() +
    geom_blank(
      data = limites_ejes_blank |> filter(componente == comp),
      aes(x = x, y = y)) +
    geom_line(
      data = datos_individuales_plot |> filter(componente == comp),
      aes(x = x, y = y, group = interaction(indicator, id)),
      color = "grey50", alpha = 0.5, linewidth = 0.6) +
    geom_line(
      data = datos_finales_plot |> filter(componente == comp),
      aes(x = x, y = y, color = agregacion),
      linewidth = 0.9) +
    scale_color_manual(values = c("Ponderado por expertise" = "blue", "Pesos iguales" = "green")) +
    scale_x_continuous(expand = expansion(mult = 0)) +
    facet_wrap(~etiqueta_plot, scales = "free", ncol = 3) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
    labs(
      x = NULL,
      y = "Densidad",
      color = NULL,
      linetype = NULL)
}
plots_componentes_distr_conj <- map(componentes_indicadores |> names(), hacer_plot_componente) |> set_names(c("fecundidad", "mortalidad", "migracion"))
plot_componentes_distr_conj <- wrap_plots(plots_componentes_distr_conj, ncol = 1, guides = "collect") & theme(legend.position = "bottom")
ggsave(plot_componentes_distr_conj, 
  filename = "plots/plots_componentes_distr_conj.pdf", 
  height = 8, width = 10, device = cairo_pdf)

# nos quedamos con las ponderadas
pool_pond <- pool_resp |>
  filter(agregacion == "Ponderado por expertise")

# exportar
write.csv(respuestas, "resultados/respuestas.csv")
save(metalog_resp, file = "resultados/metalog_resp.rda")
write.csv(pool_pond, "resultados/pool_pond.csv")
