# ################################################################# #
#### LIBRERÍAS Y CONFIGURACIÓN                                   ####
# ################################################################# #

### Limpiar memoria
rm(list = ls())

### Cargar librería Apollo
library(apollo)

### Inicializar Apollo
apollo_initialise()

### Controles del modelo
apollo_control = list(
  modelName       = "MMNL_Duque_ASC_Aleatoria",
  modelDescr      = "Mixed Logit - ASC aleatoria normal, coeficientes fijos para variables binarias (Train 80%)",
  indivID         = "codigo",
  nCores          = 4,
  outputDirectory = "output"
)

# ################################################################# #
#### CARGAR DATOS Y LIMPIEZA                                     ####
# ################################################################# #

database = read.csv("/Users/ykath/OneDrive - Universidad de los Andes/Escritorio/Instructora/20261/Grupos armados/Datos consolidados/BD_binarias3.csv", header = TRUE)

### Eliminar variables no deseadas
database <- database[ , !(names(database) %in%
                            c("Cobertura.salud_Bajo",           "Cobertura.salud_Alto",
                              "Cobertura.media.neta_Bajo",      "Cobertura.media.neta_Alto",
                              "Cobertura.Transición_Bajo",      "Cobertura.Transición_Alto",
                              "Cobertura.eléctrica.rural_Bajo", "Cobertura.eléctrica.rural_Alto"))]

### Limpiar nombres de columnas
colnames(database) = make.names(colnames(database))

# ################################################################# #
#### DIVISIÓN 80/20 ALEATORIA                                    ####
# ################################################################# #

set.seed(123)

n              = nrow(database)
train_index    = sample(1:n, size = round(0.8 * n))
database_train = database[train_index, ]
database_test  = database[-train_index, ]

### Variables explicativas (excluir respuesta e ID)
todas_las_x = setdiff(colnames(database_train), c("DUQUE_win", "codigo"))

# ################################################################# #
#### DEFINIR PARÁMETROS                                          ####
# ################################################################# #

# ------------------------------------------------------------------
# ESPECIFICACIÓN: Mixed Logit con ASC aleatoria
#
# Con variables BINARIAS (0/1), hacer aleatorio cada coeficiente β_k
# no aporta identificación: cuando X_k ∈ {0,1}, la distribución de
# β_k · X_k colapsa sobre un único punto para quienes tienen X_k = 0,
# y Apollo detecta gradiente cero → error de influencia.
#
# La práctica estándar en este caso es:
#   · ASC aleatoria ~ N(mean_asc, sd_asc)  → captura heterogeneidad
#                                             no observada global
#   · β_k fijos para todas las variables   → identificados normalmente
#
# Los parámetros estimados son:
#   mean_asc  : media de la ASC en la población
#   sd_asc    : desviación estándar (heterogeneidad no observada)
#   b_<var>   : coeficiente fijo para cada variable binaria
# ------------------------------------------------------------------

### Construir vector de parámetros
param_names = c("mean_asc", "sd_asc")

for (var in todas_las_x) {
  param_names = c(param_names, paste0("b_", var))
}

apollo_beta             = setNames(rep(0, length(param_names)), param_names)
apollo_beta["mean_asc"] =  0      # valor inicial media ASC
apollo_beta["sd_asc"]   =  0.1    # valor inicial sd ASC (> 0 para identificación)

### Sin parámetros fijos en estimación
apollo_fixed = c()

# ################################################################# #
#### DEFINIR COMPONENTES ALEATORIOS                              ####
# ################################################################# #

### Solo UNA draw: la correspondiente a la ASC
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 500,
  interUnifDraws = c(),
  interNormDraws = c("draws_asc"),   # única draw para la ASC aleatoria
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### ASC aleatoria: asc_i ~ N(mean_asc, sd_asc)
apollo_randCoeff = function(apollo_beta, apollo_inputs) {
  
  randcoeff = list()
  
  # ASC individual = media poblacional + heterogeneidad
  randcoeff[["asc_win"]] = mean_asc + sd_asc * draws_asc
  
  return(randcoeff)
}

# ################################################################# #
#### VALIDAR INPUTS (SOLO TRAIN)                                 ####
# ################################################################# #

### Guardar todas_las_x dentro de apollo_inputs para evitar uso
### del entorno global dentro de apollo_probabilities
apollo_inputs = apollo_validateInputs(database = database_train)
apollo_inputs$todas_las_x = todas_las_x

# ################################################################# #
#### FUNCIÓN DE PROBABILIDADES                                   ####
# ################################################################# #

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality = "estimate") {
  
  ### Adjuntar inputs
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  # Leer todas_las_x desde apollo_inputs (no desde el entorno global)
  vars = apollo_inputs$todas_las_x
  
  # ----------------------------------------------------------------
  # Construcción de la utilidad de ganar (win)
  #   · asc_win  es aleatorio (definido en apollo_randCoeff)
  #   · b_<var>  son fijos (tomados de apollo_beta directamente)
  # ----------------------------------------------------------------
  V_win = asc_win   # ASC aleatoria individual
  
  for (var in vars) {
    beta_val = get(paste0("b_", var))
    data_val = get(var)
    V_win    = V_win + beta_val * data_val
  }
  
  V = list()
  V[["win"]]  = V_win
  V[["loss"]] = 0    # alternativa de referencia normalizada a cero
  
  ### Configuración MNL (núcleo del MMNL)
  mnl_settings = list(
    alternatives = c(win = 1, loss = 0),
    avail        = list(win = 1, loss = 1),
    choiceVar    = DUQUE_win,
    utilities    = V
  )
  
  ### Probabilidad condicional (dado asc_win aleatorio)
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Promedio sobre las draws de la ASC (integración simulada)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Preparar y devolver
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### ESTIMACIÓN                                                  ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs)

# ################################################################# #
#### RESULTADOS EN PANTALLA                                      ####
# ################################################################# #

apollo_modelOutput(model)

### Tabla de coeficientes estimados
res = summary(model)$results

# ################################################################# #
#### GUARDAR OUTPUTS                                             ####
# ################################################################# #

#apollo_saveOutput(model)

# ################################################################# #
#### PREDICCIÓN EN TEST (20 %)                                   ####
# ################################################################# #

apollo_inputs_test = apollo_validateInputs(database = database_test)
apollo_inputs_test$todas_las_x = todas_las_x

pred_test = apollo_prediction(model,
                              apollo_probabilities,
                              apollo_inputs_test)

### Probabilidad de ganar
prob_win = pred_test[ , "win"]

### Clasificación con umbral 0.5
pred_class = ifelse(prob_win > 0.5, 1, 0)
real_class = database_test$DUQUE_win

# ################################################################# #
#### MATRIZ DE CONFUSIÓN Y ACCURACY                              ####
# ################################################################# #

conf_matrix = table(Predicho = pred_class,
                    Real     = real_class)

print(conf_matrix)

accuracy = sum(diag(conf_matrix)) / sum(conf_matrix)
cat("\nAccuracy en Test:", round(accuracy, 4), "\n")

# ################################################################# #
#### TABLA DE RESULTADOS TEST                                    ####
# ################################################################# #

resultados_test = data.frame(
  codigo     = database_test$codigo,
  Real       = real_class,
  Prob_win   = prob_win,
  Prediccion = pred_class
)

head(resultados_test)





# Verificar cuántas observaciones y parámetros tienes
nrow(database_train)      # observaciones
length(apollo_beta)       # parámetros a estimar

# Revisar correlación entre pares de variables
cor_matrix = cor(database_train[, todas_las_x])
high_cor = which(abs(cor_matrix) > 0.7 & cor_matrix != 1, arr.ind = TRUE)
print(high_cor)




# ################################################################# #
#### LOGIT REGULARIZADO (RIDGE) - solución a separación perfecta ####
# ################################################################# #

library(glmnet)

# Preparar matrices
X_train = as.matrix(database_train[, todas_las_x])
y_train = database_train$DUQUE_win

X_test  = as.matrix(database_test[, todas_las_x])
y_test  = database_test$DUQUE_win

# Selección automática de lambda por validación cruzada
set.seed(123)
cv_ridge = cv.glmnet(X_train, y_train, 
                     family   = "binomial",
                     alpha    = 0,        # alpha=0 → Ridge, alpha=1 → LASSO
                     nfolds   = 10)

# Lambda óptimo
lambda_opt = cv_ridge$lambda.min
cat("Lambda óptimo:", round(lambda_opt, 4), "\n")

# Coeficientes estimados
coefs = coef(cv_ridge, s = "lambda.min")
print(round(coefs, 4))

# ################################################################# #
#### PREDICCIÓN EN TEST                                          ####
# ################################################################# #

prob_win   = predict(cv_ridge, newx = X_test, 
                     s = "lambda.min", type = "response")
pred_class = ifelse(prob_win > 0.5, 1, 0)

conf_matrix = table(Predicho = pred_class, Real = y_test)
print(conf_matrix)

accuracy = sum(diag(conf_matrix)) / sum(conf_matrix)
cat("\nAccuracy en Test:", round(accuracy, 4), "\n")

# ################################################################# #
#### VARIABLES SIGNIFICATIVAS                                    ####
# ################################################################# #

# Con Ridge todos los coeficientes son distintos de cero,
# pero podemos identificar los más relevantes por magnitud
coefs_df = as.data.frame(as.matrix(coefs))
colnames(coefs_df) = "Estimate_Ridge"
coefs_df = coefs_df[order(abs(coefs_df$Estimate_Ridge), decreasing = TRUE), , drop = FALSE]

cat("\nVariables ordenadas por importancia (magnitud del coeficiente):\n")
print(round(coefs_df, 4))
