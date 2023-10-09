# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:/Users/malen/Dropbox/MAESTRIA_CDD/DM_EyF") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/competencia_01.csv")

# genero var de prediccion binaria
dataset[, clase_virtual := ifelse(clase_ternaria == "BAJA+2", "POS", "NEG")]

# Feature engineering


### Colapso algunas variables -----

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


# Discretizar mcuenta_saldos en rangos normalizados para las variables que presentan drifting

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

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables

# Mejores resultados obtenidos

ms <- c(1262,2104,2077,1931,2297)
mb <- c(477,6,3,4,3)
md <- c(7,8,10,9,6)
		
for (i in 1:5) {
  
  modelo <- rpart(
    formula = "clase_virtual ~ . - clase_ternaria - numero_de_cliente",
    data = dtrain, # los datos donde voy a entrenar
    xval = 0,
    cp = -0.3, # esto significa no limitar la complejidad de los splits
    minsplit = ms[i], # minima cantidad de registros para que se haga el split
    minbucket = mb[i], # tamaño minimo de una hoja
    maxdepth = md[i]
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
  
  # prediccion es una matriz con TRES columnas,
  # llamadas "POS","NEG"
  # cada columna es el vector de probabilidades
  
  # agrego a dapply una columna nueva que es la probabilidad de BAJA+2
  dapply[, prob_baja2 := prediccion[, "POS"]]
  
  # solo le envio estimulo a los registros
  #  con probabilidad de BAJA+2 mayor  a  1/40
  dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]
  
  # genero el archivo para Kaggle
  # primero creo la carpeta donde va el experimento
  #dir.create("./exp/")
  dir.create("./exp/KA2001")
  
  # solo los campos para Kaggle
  fwrite(dapply[, list(numero_de_cliente, Predicted)],
         file = paste0("./exp/KA2001/K101_BO_FE",i,".csv"),
         sep = ","
  )
  
}
