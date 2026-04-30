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

# Eliminar variables no deseadas (igual que logit_V2)
database <- database[ , !(names(database) %in% 
                            c("Cobertura.salud_Bajo", "Cobertura.salud_Alto",
                              "Cobertura.media.neta_Bajo", "Cobertura.media.neta_Alto",
                              "Cobertura.Transición_Bajo", "Cobertura.Transición_Alto",
                              "Cobertura.eléctrica.rural_Bajo", "Cobertura.eléctrica.rural_Alto"))]

# Crear ID único
#database$ID = 1:nrow(database)

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
  modelName       = "Probit_Duque_Full_Train",
  modelDescr      = "Modelo Probit con todas las variables - Train 80%",
  indivID         = "codigo",
  outputDirectory = "output"
)

# Identificar variables explicativas
todas_las_x = setdiff(colnames(database_train), c("DUQUE_win", "codigo"))

# ################################################################# #
#### DEFINIR PARÁMETROS                                          ####
# ################################################################# #

apollo_beta = rep(0, length(todas_las_x) + 1)
names(apollo_beta) = c(paste0("b_", todas_las_x), "tau_1")
apollo_beta["tau_1"] = 0

apollo_fixed = c()

# ################################################################# #
#### VALIDAR INPUTS (SOLO TRAIN)                                 ####
# ################################################################# #

apollo_inputs = apollo_validateInputs(database = database_train)

# Guardar lista de variables dentro de apollo_inputs
apollo_inputs$todas_las_x = todas_las_x

# ################################################################# #
#### FUNCIÓN DE PROBABILIDADES                                   ####
# ################################################################# #

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  # Construcción de la utilidad latente
  V_latent = 0
  for(var_name in apollo_inputs$todas_las_x){
    beta_val = get(paste0("b_", var_name))
    data_val = get(var_name)
    V_latent = V_latent + beta_val * data_val
  }
  
  # Configuración Ordered Probit
  op_settings = list(
    outcomeOrdered = DUQUE_win,
    V              = V_latent,
    tau            = c(tau_1),
    coding         = c(loss=0, win=1)
  )
  
  P[['model']] = apollo_op(op_settings, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### ESTIMACIÓN                                                  ####
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
apollo_inputs_test$todas_las_x = todas_las_x

pred_test = apollo_prediction(model,
                              apollo_probabilities,
                              apollo_inputs_test)

# Probabilidad de ganar (outcome = 1 → "win")
prob_win = pred_test[, "X1"]

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
cat("\nAccuracy:", round(accuracy, 4), "\n")

# Crear tabla con resultados
resultados_test = data.frame(
  codigo     = database_test$codigo,
  Real       = real_class,
  Prob_win   = prob_win,
  Prediccion = pred_class
)

head(resultados_test)

#write.csv(resultados_test,"C:/Users/ykath/OneDrive - Universidad de los Andes/Escritorio/Instructora/20261/Grupos armados/Modelos/predicciones_test_probit.csv",row.names = FALSE)
