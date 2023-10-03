library(data.table)

bd <- fread("buckets/b1/datasets/competencia_02_crudo.csv")

setorder(bd,cols = foto_mes)

bd[, foto_mes_2 := as.Date(paste0(substr(foto_mes, 1, 4), "-", substr(foto_mes, 5, 6), "-01"))]
bd[, c("lag_2", "lag_1") := .(shift(foto_mes_2, n = 2, type = "lead"), shift(foto_mes_2, n = 1, type = "lead")), by = numero_de_cliente]
bd[,dif_lag_1 := as.numeric(as.Date(lag_1) - foto_mes_2)]
bd[,c("lag_2","lag_1") := .(fifelse(dif_lag_1 > 31, as.Date(NA),lag_2),fifelse(dif_lag_1 > 31, as.Date(NA),lag_1))]
bd[, clase_ternaria := fifelse(!is.na(lag_2), "CONTINUA", fifelse(!is.na(lag_1), "BAJA+2", "BAJA+1"))]
bd[, c("lag_1", "lag_2","foto_mes_2","dif_lag_1") := NULL]

fwrite(bd,
       file = "buckets/b1/datasets/competencia_02.csv",
       sep = ",")



