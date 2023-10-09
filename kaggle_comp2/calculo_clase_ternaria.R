require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
#setwd("C:/Users/malen/Dropbox/MAESTRIA_CDD/DM_EyF") # Establezco el Working Directory

# Calculo clase ternaria
bd <- fread("C:/Users/malen/Downloads/competencia_02.csv")

setorder(bd,cols = numero_de_cliente)

df <- bd[1:100,c("numero_de_cliente","foto_mes","clase_ternaria")]


bd_prueba <- bd
bd_prueba[,clase_ternaria := NULL]

bd_prueba[, foto_mes_2 := as.Date(paste0(substr(foto_mes, 1, 4), "-", substr(foto_mes, 5, 6), "-01"))]
bd_prueba[, c("lag_2", "lag_1") := .(shift(foto_mes_2, n = 2, type = "lead"), shift(foto_mes_2, n = 1, type = "lead")), by = numero_de_cliente]
bd_prueba[,dif_lag_1 := as.numeric(as.Date(lag_1) - foto_mes_2)]
bd_prueba[,c("lag_2","lag_1") := .(fifelse(dif_lag_1 > 31, as.Date(NA),lag_2),fifelse(dif_lag_1 > 31, as.Date(NA),lag_1))]
bd_prueba[, clase_ternaria := fifelse(!is.na(lag_2), "CONTINUA", fifelse(!is.na(lag_1), "BAJA+2", "BAJA+1"))]
bd_prueba[, c("lag_1", "lag_2","foto_mes_2","dif_lag_1") := NULL]
