# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:/Users/malen/Dropbox/MAESTRIA_CDD/DM_EyF") # Establezco el Working Directory

# Calculo clase ternaria
bd <- fread("datasets/competencia_01_crudo.csv")

setorder(bd,cols = foto_mes)

bd[, foto_mes_2 := as.Date(paste0(substr(foto_mes, 1, 4), "-", substr(foto_mes, 5, 6), "-01"))]
bd[, c("lag_2", "lag_1") := .(shift(foto_mes_2, n = 2, type = "lead"), shift(foto_mes_2, n = 1, type = "lead")), by = numero_de_cliente]
bd[,dif_lag_1 := as.numeric(as.Date(lag_1) - foto_mes_2)]
bd[,c("lag_2","lag_1") := .(fifelse(dif_lag_1 > 31, as.Date(NA),lag_2),fifelse(dif_lag_1 > 31, as.Date(NA),lag_1))]
bd[, clase_ternaria := fifelse(!is.na(lag_2), "CONTINUA", fifelse(!is.na(lag_1), "BAJA+2", "BAJA+1"))]
bd[, c("lag_1", "lag_2","foto_mes_2","dif_lag_1") := NULL]



fwrite(bd,
       file = "datasets/competencia_01.csv",
       sep = ",")

####################################################################################
####################################################################################

# cargo dataset
dataset <- fread("./datasets/competencia_01.csv")

# genero var de prediccion binaria
dataset[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", "POS", "NEG")]

###### Feature engineering ---------


# Colapso algunas variables en una

# Cantidad de tarjetas
dataset[, ctarjetas_total := ctarjeta_master + ctarjeta_visa]
dataset[,c("ctarjeta_master","ctarjeta_visa") := NULL]

# Suma de consumos de tarjetas
dataset[, mtarjetas_total := mtarjeta_visa_consumo + mtarjeta_master_consumo]
dataset[,c("mtarjeta_master_consumo","mtarjeta_visa_consumo") := NULL]

# Suma de saldos totales de tarjetas 
dataset[, saldo_tarjetas_total := Visa_msaldototal + Master_msaldototal]
dataset[,c("Visa_msaldototal","Master_msaldototal") := NULL]

# Suma de saldos en pesos de tarjetas 
dataset[, saldo_tarjetas_pesos := Visa_msaldopesos + Master_msaldopesos]
dataset[,c("Visa_msaldopesos","Master_msaldopesos") := NULL]

# Suma de limite financiacion de tarjetas 
dataset[, total_financ_tarjetas := Visa_mfinanciacion_limite + Master_mfinanciacion_limite]
dataset[,c("Visa_mfinanciacion_limite","Master_mfinanciacion_limite") := NULL]


# Homogeneizo distribuciones entre grupos de train y test para las variables que presentan drifting

dataset[, mcuenta_saldos_ranknorm := round(rank(mcuentas_saldo)/.N,6), by = as.character(foto_mes)]
dataset[,mcuentas_saldo := NULL]

dataset[, mcuenta_corriente_ranknorm := round(rank(mcuenta_corriente)/.N,6), by = as.character(foto_mes)]
dataset[,mcuenta_corriente := NULL]

dataset[, mtarjetas_total_ranknorm := round(rank(mtarjetas_total)/.N,6), by = as.character(foto_mes)]
dataset[,mtarjetas_total := NULL]

dataset[, Visa_mpagominimo_ranknorm := round(rank(Visa_mpagominimo)/.N,6), by = as.character(foto_mes)]
dataset[,Visa_mpagominimo := NULL]

dataset[, ccomisiones_mantenimiento_ranknorm := round(rank(ccomisiones_mantenimiento)/.N,6), by = as.character(foto_mes)]
dataset[,ccomisiones_mantenimiento := NULL]

dataset[, mcomisiones_otras_ranknorm := round(rank(mcomisiones_otras)/.N,6), by = as.character(foto_mes)]
dataset[,mcomisiones_otras := NULL]

dataset[, cplazo_fijo_ranknorm := round(rank(cplazo_fijo)/.N,6), by = as.character(foto_mes)]
dataset[,cplazo_fijo := NULL]

dataset[, total_financ_tarjetas_ranknorm := round(rank(total_financ_tarjetas)/.N,6), by = as.character(foto_mes)]
dataset[,total_financ_tarjetas := NULL]


dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo
dapply <- dapply[,clase_ternaria := NA_character_]
dapply <- dapply[,clase_binaria := NA_character_]

pesos <- copy( dtrain[, ifelse( clase_ternaria=="BAJA+2",100.0, 1.0)])


#### Entreno modelo ######

  
  modelo <- rpart(
    formula = "clase_binaria ~ . -clase_ternaria -numero_de_cliente",
    data = dtrain, # los datos donde voy a entrenar
    xval = 0,
    cp = -0.3, # esto significa no limitar la complejidad de los splits
    minsplit = 1388, # minima cantidad de registros para que se haga el split
    minbucket = 335, # tamaño minimo de una hoja
    maxdepth = 6,
    weights = pesos
  ) # profundidad maxima del arbol
  
  
  # grafico el arbol
  prp(modelo,
      extra = 101, digits = -5,
      branch = 1, type = 4, varlen = 0, faclen = 0
  )
  
  
  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    object = modelo,
    newdata = dapply,
    type = "prob"
  )
  
  # prediccion es una matriz con DOS columnas,
  # llamadas "POS","NEG"
  # cada columna es el vector de probabilidades
  
  # agrego a dapply una columna nueva que es la probabilidad de BAJA+2
  dapply[, prob_baja2 := prediccion[, "POS"]]
  
  # solo le envio estimulo a los registros
  #  con probabilidad de BAJA+2 mayor  a  1/40
  #dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]
  
  setorder(dapply,-prob_baja2)
  head <- dapply[, head(.SD, 9341)]
  
  dapply[,Predicted := as.numeric(numero_de_cliente %in% head$numero_de_cliente)]
  
  # genero el archivo para Kaggle
  # primero creo la carpeta donde va el experimento
  #dir.create("./exp/")
  dir.create("./exp/KA2001/version_final")
  
  # solo los campos para Kaggle
  fwrite(dapply[, list(numero_de_cliente, Predicted)],
         file = "./exp/KA2001/version_final/K101_final.csv",
         sep = ","
  )
  

