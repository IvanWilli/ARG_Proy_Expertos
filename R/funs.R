
proyectar_simulacion <- function(s, sx_bloque, asfr_bloque, mig_bloque) {
  # sobrevivencia
  Sx_f <- sx_bloque |>
    filter(simulacion == s, sexo == "Mujeres") |>
    pivot_wider(names_from = anio, values_from = Sx) |>
    arrange(Age) |> select(-simulacion, -Age, -sexo) |>
    as.matrix()
  Sx_m <- sx_bloque |>
    filter(simulacion == s, sexo == "Varones") |>
    pivot_wider(names_from = anio, values_from = Sx) |>
    arrange(Age) |> select(-simulacion, -Age, -sexo) |>
    as.matrix()
  # fecundidad
  Fx <- asfr_bloque |>
    filter(simulacion == s) |> 
    complete(edad = 0:100, anio = 2025:2075, fill = list(asfr = 0)) |> 
    arrange(edad) |> pivot_wider(names_from = anio, values_from = asfr) |> 
    select(-simulacion, -edad) |> as.matrix() 
  # migra
  TNM <- mig_bloque |> filter(simulacion == s) 
  TNMx <- TNM |> 
    left_join(mig_estructura_sexo, relationship = "many-to-many", by = join_by(tipo)) |> 
    left_join(mig_estructura_sexo_edad, relationship = "many-to-many", by = join_by(tipo))
  TNMx <- TNMx |> 
    bind_rows(TNMx |> filter(edad == 99) |> mutate(edad = 100)) |> 
    arrange(simulacion, anio, sexo, edad)
  TNMx_f <- TNMx |> 
    filter(sexo == "M") |> 
    mutate(tmn_sexo_edad = tmn * p_muj * prop) |>
    select(anio, edad, tmn_sexo_edad) |> arrange(anio, edad) |> 
    pivot_wider(names_from = anio, values_from = tmn_sexo_edad) |> 
    select(-edad) |> as.matrix() 
  TNMx_m <- TNMx |> 
    filter(sexo == "V") |> 
    mutate(tmn_sexo_edad = tmn * p_muj * prop) |>
    select(anio, edad, tmn_sexo_edad) |> arrange(anio, edad) |> 
    pivot_wider(names_from = anio, values_from = tmn_sexo_edad) |> 
    select(-edad) |> as.matrix()
  
  # proy
  proy <- met_comp(
    age = 0:100,
    Sx_f = Sx_f,
    Sx_m = Sx_m,
    Fx = Fx,
    TNMx_f = TNMx_f,
    TNMx_m = TNMx_m,
    N0_f = N0_f,
    N0_m = N0_m,
    srb = srb
  )

  # handle the output
  bind_rows(
    proy$Nfx |> rename(value = pop) |> mutate(ind = "pop"),
    proy$Nmx |> rename(value = pop) |> mutate(ind = "pop"),
    proy$B |> transmute(age = -1, sex = "b", value = B, t = as.character(t), ind = "B"),
    proy$D |> transmute(age = -1, sex = "b", value = D, t = as.character(t), ind = "D"),
    proy$NM |> transmute(age = -1, sex = "b", value = NM, t = as.character(t), ind = "NM")
  ) |>
    transmute(
      simulacion = s,
      anio = 2024 + as.integer(t),
      sexo = sex,
      edad = age,
      ind = ind,
      value = value
    )
}

met_comp <- function(age = 0:100, 
                    Sx_f, Sx_m, Fx, 
                    TNMx_f, TNMx_m, 
                    N0_m, N0_f, 
                    srb, 
                    to_df = TRUE){
  
  # parámetros y objetos vacíos
  ages <- length(age)
  sex <- c("f", "m")
  sexs <- length(sex)
  year <- 0:ncol(Sx_f) + 1
  years <- length(year)
  Sx_m_t <- Sx_f_t <- matrix(0, ages, ages)
  Nx <- Bx <- Dx <- matrix(0, ages * sexs, years)
  Bf <- Bm <- B <- Dm <- Df <- D <- NM <- rep(0, years)
  NMx_f <-NMx_m <- matrix(0, ages, years)

  # población inicial
  Nx[,1] <- c(N0_f, N0_m)

  # loop
  for(t in 1:(years-1)){
    # t = 2
    # print(t)

    # prepare risks
    diag(Sx_f_t[-1,]) <- Sx_f[-1,t]
    diag(Sx_m_t[-1,]) <- Sx_m[-1,t]
    Sx_f_t[ages, ages] <- Sx_f[ages,t]
    Sx_m_t[ages, ages] <- Sx_m[ages,t]
    b_f_t <- (Fx[,t] + Fx[,t+1] * Sx_f[,t])/2 * 100/(srb[t]+100)    
    b_m_t <- (Fx[,t] + Fx[,t+1] * Sx_f[,t])/2 * srb[t]/(srb[t]+100) 
    
    # Leslie matrix 2-sex
    L_t <- rbind(
      cbind(Sx_f_t, matrix(0, ages, ages)),
      cbind(matrix(0, ages, ages), Sx_m_t))
    L_t[1,1:ages]<- b_f_t * Sx_f[1,t]
    L_t[ages+1,1:ages]<- b_m_t * Sx_m[1,t]

    # población no nacida, un año después, pre migración
    Nx[,t+1] <- L_t %*% Nx[,t]

    # nacimientos y defunciones
    Bf[t] <- sum(Nx[1:ages,t] * b_f_t) 
    Bm[t] <- sum(Nx[(ages+1):(ages*2),t] * b_m_t) 
    B[t] <- Bf[t] + Bm[t]
    
    # agregar migración a fin de año
    if(t == 1){
      pob_total_exp <- sum(Nx[,t])
    }else{
      pob_total_exp <- sum(Nx[,t]) * exp(1.5*log(sum(Nx[,t])/sum(Nx[,t-1])))
    } 
    NMx_f[,t] <- TNMx_f[,t]/1000 * pob_total_exp
    NMx_m[,t] <- TNMx_m[,t]/1000 * pob_total_exp
    NM[t] <- sum(NMx_f[,t]) + sum(NMx_m[,t])
    Nx[,t+1] <- Nx[,t+1] + c(NMx_f[,t], NMx_m[,t])

    # muertes
    Df[t] <- sum(Nx[1:ages,t]) - sum(Sx_f_t %*% Nx[1:ages,t]) + Bf[t] * (1 - Sx_f[1, t])
    Dm[t] <- sum(Nx[(ages+1):(ages*2),t]) - sum(Sx_m_t %*% Nx[(ages+1):(ages*2),t]) + Bm[t] * (1 - Sx_m[1, t])
    D[t] <- Df[t] + Dm[t]
  }

  # convert to df?
  if(to_df){
    colnames(Nx) <- 1:ncol(Nx)
    Nfx <- Nx[1:ages,] |> 
      as.data.frame() |> 
      mutate(age = age, sex = "f", .before = 1) |> 
      pivot_longer(as.character(1:ncol(Nx)), names_to ="t", values_to = "pop")
    Nmx <- Nx[(ages+1):(ages*2),] |> 
      as.data.frame() |> 
      mutate(age = age, sex = "m", .before = 1) |> 
      pivot_longer(as.character(1:ncol(Nx)), names_to ="t", values_to = "pop")
    B <- data.frame(t = 1:length(B), B = B)
    D <- data.frame(t = 1:length(D), D = D)
    NM <- data.frame(t = 1:length(NM), NM = NM)
  }

  # end
  return(list(Nmx = Nmx, Nfx = Nfx, B = B, D = D, NM = NM))
}


# # funciones logito
# logito <- function(y, li, ls) {
#   yS <- (ls-y)/(ls-li)
#   return(log(yS/(1-yS)))
# }
# anti_logito <- function(logito_y, li, ls) {
#   yS <- exp(logito_y)/(1+exp(logito_y))
#   return(ls - (ls-li)*yS)
# }
# # regresión logística
#   # t: años de observacion, puede tener decimales
#   # y: valor (por ejemplo e0 o tgf)
#   # U: límite superior
#   # L: límite inferior
#   # t_proj: años a extrapolar o interpolar
#   # jump_off = FALSE: tomar la pendiente pero partiendo de nivel de último valor observado
# logistica_regresion <- function(y, t, U, L, t_proj, jump_off = FALSE, diagn = FALSE){
#   logito <- logito(y, L, U)
#   model_logito <- lm(logito~t)
#   a <- coef(model_logito)[1]
#   b <- coef(model_logito)[2]
#   logito_hat <- a + b * t_proj
#   if(jump_off){
#     a <- logito[t == max(t)]  
#     logito_hat <- a + b * (t_proj - max(t))
#   }
#   y_hat <- anti_logito(logito_hat, L, U)
#   out <- data.frame(t = t_proj, y = y_hat)
#   if(diagn){
#     out <- list(
#       result = out,
#       model = model_logito
#     )
#     plot(t, y, xlim = range(c(t, t_proj)), ylim = range(c(y, y_hat))); lines(t_proj, y_hat); title("Fit")
#   }
#   return(out)
# }

# funciones logito
logito <- function(y, li, ls) {
  yS <- (ls - y) / (ls - li)
  return(log(yS / (1 - yS)))
}

anti_logito <- function(logito_y, li, ls) {
  yS <- exp(logito_y) / (1 + exp(logito_y))
  return(ls - (ls - li) * yS)
}

# regresion logistica
logistica_regresion <- function(y, t, U, L, t_proj, jump_off = FALSE, diagn = FALSE) {
  logito_y <- logito(y, L, U)
  model_logito <- lm(logito_y ~ t)
  a <- coef(model_logito)[1]
  b <- coef(model_logito)[2]
  logito_hat <- a + b * t_proj
  if (jump_off) {
    a <- logito_y[t == max(t)]
    logito_hat <- a + b * (t_proj - max(t))
  }
  y_hat <- anti_logito(logito_hat, L, U)
  out <- data.frame(t = t_proj, y = y_hat)
  if (diagn) {
    out <- list(
      result = out,
      model = model_logito
    )
    plot(t, y, xlim = range(c(t, t_proj)), ylim = range(c(y, y_hat)))
    lines(t_proj, y_hat)
    title("Fit")
  }
  return(out)
}


beta_asfr <- function(age, asfr_0, proy, anio_0 = 2024, transicion = 6){

  p0 <- asfr_0 / sum(asfr_0)
  x  <- age
  mu0 <- sum(x * p0)
  sd0 <- sqrt(sum((x - mu0)^2 * p0))

  a <- min(age) - .5
  b <- max(age) + .5

  out <- bind_rows(
    tibble(anio = anio_0, media = mu0, sd = sd0),
    proy
  ) |>
    mutate(
      m = (media - a) / (b - a),
      v = sd^2 / (b - a)^2,
      k = m * (1 - m) / v - 1,
      alpha = m * k,
      beta  = (1 - m) * k
    ) |>
    crossing(age = age) |>
    mutate(
      prop = pbeta((age + .5 - a)/(b-a), alpha, beta) -
             pbeta((age - .5 - a)/(b-a), alpha, beta)
    ) |>
    group_by(anio) |>
    mutate(prop = prop / sum(prop)) |>
    ungroup()

  ratio <- out |>
    filter(anio == anio_0) |>
    transmute(age, ratio = pmax(p0, 1e-10) / pmax(prop, 1e-10))

  out |>
    left_join(ratio, by = "age") |>
    mutate(
      w = pmin((anio - anio_0) / transicion, 1),
      prop = prop * ratio^(1 - w)
    ) |>
    group_by(anio) |>
    mutate(prop = prop / sum(prop)) |>
    ungroup() |>
    select(anio, age, media, sd, prop)
}
