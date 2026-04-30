# ################################################################# #
#### CARGAR LIBRERÍA Y CONFIGURACIÓN                             ####
# ################################################################# #

rm(list = ls())

library(apollo)
apollo_initialise()

# Número de clases latentes 
N_CLASES <- 2

apollo_control = list(
  modelName       = "LCCM_Duque",
  modelDescr      = paste0("Modelo de Clases Latentes (", N_CLASES, " clases) - Voto Duque"),
  indivID         = "ID",
  nCores          = 1,
  outputDirectory = "output",
  noValidation    = TRUE
)

# ################################################################# #
#### CARGAR DATOS Y LIMPIEZA                                     ####
# ################################################################# #

database = read.csv("/Users/ykath/OneDrive - Universidad de los Andes/Escritorio/Instructora/20261/Grupos armados/Datos consolidados/BD_binarias3.csv", header=TRUE)
#database = read.csv("/Users/ykath/OneDrive - Universidad de los Andes/Escritorio/Instructora/20261/Grupos armados/Datos consolidados/BD_normalizadas.csv", header=TRUE)

# Eliminar variables no deseadas
database <- database[ , !(names(database) %in% 
                            c("Cobertura.salud_Bajo", "Cobertura.salud_Alto",
                              "Cobertura.media.neta_Bajo", "Cobertura.media.neta_Alto",
                              "Cobertura.Transición_Bajo", "Cobertura.Transición_Alto",
                              "Cobertura.eléctrica.rural_Bajo", "Cobertura.eléctrica.rural_Alto", "codigo"))]


database$ID        = 1:nrow(database)
colnames(database) = make.names(colnames(database))

# 1. Definir variables del modelo de ELECCIÓN 
vars_eleccion = c("ACSN", "AGC.ahora.EGC", "Disidencias.de.las.FARC", "ELN", "EPL", "Otras.organizaciones")
#vars_eleccion = c("ELN")

# 2. Definir variables del modelo de ASIGNACIÓN 
# Serán todas las demás columnas que no sean ID, DUQUE_win, ni las de elección.
#vars_asignacion = setdiff(colnames(database), c("DUQUE_win", "ID", vars_eleccion))


vars_excluir = c()
vars_asignacion = setdiff(colnames(database), c("DUQUE_win", "ID", vars_eleccion, vars_excluir))
#vars_asignacion = c("Cobertura.media.neta", "Homicidios.x.10000.hab")

# ################################################################# #
#### DEFINIR PARÁMETROS DEL MODELO                               ####
# ################################################################# #

# Betas del modelo de ELECCIÓN 
betas_eleccion = unlist(lapply(1:N_CLASES, function(s) {
  params        = c(0)
  names(params) = paste0("asc_win_c", s)
  offset = if(s == 1) 0.01 else -0.01
  for (var in vars_eleccion) params[paste0("b_", var, "_c", s)] = offset
  params
}))

# Parámetros del modelo de ASIGNACIÓN DE CLASES (delta y gammas)

# Las constantes de clase 
betas_clases = setNames(rep(0, N_CLASES - 1), paste0("delta_c", 1:(N_CLASES - 1)))

# coeficientes (gammas) para cada variable de asignación
for (nombre_var in vars_asignacion) {
  gammas = setNames(rep(0, N_CLASES - 1), paste0("gamma_", nombre_var, "_c", 1:(N_CLASES - 1)))
  betas_clases = c(betas_clases, gammas)
}

# Unir todos los parámetros
apollo_beta  = c(betas_eleccion, betas_clases)

# Dejar fija la constante de la clase de referencia (y sus utilidades en la función posterior)
apollo_fixed = paste0("asc_win_c", N_CLASES)   

# ################################################################# #
#### DEFINIR COMPONENTES DE CLASES LATENTES                      ####
# ################################################################# #

apollo_lcPars = function(apollo_beta, apollo_inputs) {
  lcpars = list()
  
  # Constantes del modelo de elección por clase
  lcpars[["asc_win"]] = lapply(1:N_CLASES, function(s) apollo_beta[paste0("asc_win_c", s)])
  
  # Betas de variables de ELECCIÓN
  betas_por_var        = lapply(vars_eleccion, function(var) {
    lapply(1:N_CLASES, function(s) apollo_beta[paste0("b_", var, "_c", s)])
  })
  names(betas_por_var) = paste0("b_", vars_eleccion)
  lcpars               = c(lcpars, betas_por_var)
  
  # Utilidades del modelo de ASIGNACIÓN de clases
  V_clases = lapply(1:N_CLASES, function(s) {
    if (s < N_CLASES) {
      # Comienza con la constante de la clase
      v = apollo_beta[paste0("delta_c", s)]
      # Suma el impacto de cada variable de asignación (gamma * variable)
      for (nombre_var in vars_asignacion) {
        v = v + apollo_beta[paste0("gamma_", nombre_var, "_c", s)] * apollo_inputs$database[[nombre_var]]
      }
      return(v)
    } else {
      # La utilidad de la clase de referencia es 0
      return(0)
    }
  })
  

  names(V_clases) = paste0("class_", 1:N_CLASES)
  
  classAlloc_settings = list(
    classes       = setNames(1:N_CLASES, paste0("class_", 1:N_CLASES)),
    utilities     = V_clases,
    componentName = "classAlloc"
  )
  
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  
  return(lcpars)
}

# ################################################################# #
#### VALIDAR E INICIALIZAR                                       ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### FUNCIÓN DE PROBABILIDADES                                   ####
# ################################################################# #

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality = "estimate") {
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(win = 1, loss = 0),
    avail        = 1,
    choiceVar    = DUQUE_win
  )
  
  for (s in 1:N_CLASES) {
    
    # Modelo de elección: Solo usa las vars_eleccion (los grupos armados)
    V_win = asc_win[[s]]
    for (var_name in vars_eleccion) {
      V_win = V_win + get(paste0("b_", var_name))[[s]] * get(var_name)
    }
    
    mnl_settings$utilities = list(win = V_win, loss = 0)
    
    P[[paste0("Class_", s)]] = apollo_mnl(mnl_settings, functionality)
  }
  
  lc_settings  = list(inClassProb = P, classProb = pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### ESTIMACIÓN                                                  ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs)

apollo_modelOutput(model)
#apollo_saveOutput(model)

# ################################################################# #
#### POST-PROCESAMIENTO                                          ####
# ################################################################# #

apollo_sink()

# Probabilidades posteriores de pertenencia a cada clase
conditionals   = apollo_conditionals(model, apollo_probabilities, apollo_inputs)
unconditionals = apollo_unconditionals(model, apollo_probabilities, apollo_inputs)

summary(conditionals)
summary(as.data.frame(unconditionals[["pi_values"]]))


cat("\n--- Tamaño promedio de clases (pi_values) ---\n")
print(sapply(unconditionals[["pi_values"]], mean))

# Revisar si hay variables con varianza cero
cat("\n--- Verificación de varianza de todas las variables ---\n")
print(apply(database[, c(vars_eleccion, vars_asignacion)], 2, stats::var))

apollo_sink()