# ################################################################# #
#### LIBRERÍAS Y CONFIGURACIÓN                                  ####
# ################################################################# #

library(apollo)
apollo_initialise()

# ################################################################# #
#### CARGAR DATOS Y LIMPIEZA                                     ####
# ################################################################# #

database = read.csv("/Users/ykath/OneDrive - Universidad de los Andes/Escritorio/Instructora/20261/Grupos armados/Datos consolidados/BD_binarias3.csv", header=TRUE)
#database = read.csv("/Users/ykath/OneDrive - Universidad de los Andes/Escritorio/Instructora/20261/Grupos armados/Datos consolidados/BD_normalizadas.csv", header=TRUE)
#database = read.csv("/Users/ykath/OneDrive - Universidad de los Andes/Escritorio/Instructora/20261/Grupos armados/Data/datos_input_2503.csv", header=TRUE)


# Eliminar variables no deseadas
database <- database[ , !(names(database) %in% 
                            c("Cobertura.salud_Bajo", "Cobertura.salud_Alto",
                              "Cobertura.media.neta_Bajo", "Cobertura.media.neta_Alto",
                              "Cobertura.Transición_Bajo", "Cobertura.Transición_Alto",
                              "Cobertura.eléctrica.rural_Bajo", "Cobertura.eléctrica.rural_Alto"))]

# Limpiar nombres de columnas
colnames(database) = make.names(colnames(database))

# ################################################################# #
#### DIVISIÓN 80/20 ALEATORIA                                   ####
# ################################################################# #

set.seed(123) 

n = nrow(database)
train_index = sample(1:n, size = round(0.8*n))
n
database_train = database[train_index, ]
database_test  = database[-train_index, ]

# ################################################################# #
#### CONTROLES DEL MODELO                                        ####
# ################################################################# #

apollo_control = list(
  modelName       = "Logit_Duque_Full_Train", 
  modelDescr      = "Modelo con todas las variables - Train 80%",
  indivID         = "codigo", 
  outputDirectory = "output"
)

# Identificar variables explicativas
todas_las_x = setdiff(colnames(database_train), c("DUQUE_win", "codigo"))

# ################################################################# #
#### DEFINIR PARÁMETROS                                           ####
# ################################################################# #

apollo_beta = rep(0, length(todas_las_x) + 1)
names(apollo_beta) = c("asc_win", paste0("b_", todas_las_x))

apollo_fixed = c()

# ################################################################# #
#### VALIDAR INPUTS (SOLO TRAIN)                                 ####
# ################################################################# #

apollo_inputs = apollo_validateInputs(database = database_train)

# ################################################################# #
#### FUNCIÓN DE PROBABILIDADES                                   ####
# ################################################################# #

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  # Construcción de utilidad
  V_win = asc_win
  
  for(var_name in todas_las_x){
    beta_val = get(paste0("b_", var_name))
    data_val = get(var_name)
    V_win = V_win + beta_val * data_val
  }
  
  V = list()
  V[['win']]  = V_win
  V[['loss']] = 0 
  
  mnl_settings = list(
    alternatives  = c(win=1, loss=0), 
    avail         = list(win=1, loss=1),
    choiceVar     = DUQUE_win,
    utilities     = V
  )
  
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### ESTIMACIÓN                                                   ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, apollo_inputs)

apollo_modelOutput(model)

res = summary(model)$results
res

# ################################################################# #
#### PREDICCIÓN EN TEST                                          ####
# ################################################################# #

apollo_inputs_test = apollo_validateInputs(database = database_test)

pred_test = apollo_prediction(model, 
                              apollo_probabilities, 
                              apollo_inputs_test)

# Probabilidad de ganar
prob_win = pred_test[, "win"]

# Clasificación con umbral 0.5
pred_class = ifelse(prob_win > 0.5, 1, 0)

real_class = database_test$DUQUE_win

# ################################################################# #
#### MATRIZ DE CONFUSIÓN                                         ####
# ################################################################# #

conf_matrix = table(Predicho = pred_class,
                    Real = real_class)

print(conf_matrix)

# Accuracy
accuracy = sum(diag(conf_matrix)) / sum(conf_matrix)
cat("\nAccuracy:", round(accuracy,4), "\n")


# Crear tabla con resultados
resultados_test = data.frame(
  codigo        = database_test$codigo,
  Real          = real_class,
  Prob_win      = prob_win,
  Prediccion    = pred_class
)

head(resultados_test)

#write.csv(resultados_test,"C:/Users/ykath/OneDrive - Universidad de los Andes/Escritorio/Instructora/20261/Grupos armados/Modelos/predicciones_test_logit.csv",row.names = FALSE)


