require(data.table)

dataset <- fread("buckets/b1/datasets/competencia_02.csv")

dataset <- bd
require(data.table)


### Colapso algunas variables en una

# Cantidad de tarjetas
dataset[, ctarjetas_total := rowSums(.SD, na.rm = T), .SDcols = c("ctarjeta_master","ctarjeta_visa")]
dataset[,c("ctarjeta_master","ctarjeta_visa") := NULL]

# Suma de consumos de tarjetas
dataset[, mtarjetas_total := rowSums(.SD, na.rm = T), .SDcols =  c("mtarjeta_visa_consumo","mtarjeta_master_consumo")]
dataset[,c("mtarjeta_master_consumo","mtarjeta_visa_consumo") := NULL]

# Suma de saldos totales de tarjetas 
dataset[, saldo_tarjetas_total := rowSums(.SD, na.rm = T), .SDcols = c("Visa_msaldototal","Master_msaldototal")]
dataset[,c("Visa_msaldototal","Master_msaldototal") := NULL]

# Suma de saldos en pesos de tarjetas 
dataset[, saldo_tarjetas_pesos := rowSums(.SD, na.rm = T), .SDcols = c("Visa_msaldopesos","Master_msaldopesos")]
dataset[,c("Visa_msaldopesos","Master_msaldopesos") := NULL]

# Suma de saldos en dolares de tarjetas 
dataset[, saldo_tarjetas_dolares := rowSums(.SD, na.rm = T), .SDcols = c("Visa_msaldodolares","Master_msaldodolares")]
dataset[,c("Visa_msaldodolares","Master_msaldodolares") := NULL]

# Suma de limite financiacion de tarjetas 
dataset[, total_financ_tarjetas := rowSums(.SD, na.rm = T), .SDcols = c("Visa_mfinanciacion_limite","Master_mfinanciacion_limite")]
dataset[,c("Visa_mfinanciacion_limite","Master_mfinanciacion_limite") := NULL]

# Suma de limite total de tarjetas

dataset[,total_limite_tarjetas := rowSums(.SD, na.rm = T), .SDcols = c("Visa_mlimitecompra","Master_mlimitecompra")]
dataset[,c("Visa_mlimitecompra","Master_mlimitecompra") := NULL]

# Suma de transacciones realizadas con tarjetas

dataset[, ctarjetas_transacciones := rowSums(.SD, na.rm = T), .SDcols =  c("ctarjeta_visa_transacciones","ctarjeta_master_transacciones")]
dataset[,c("ctarjeta_visa_transacciones", "ctarjeta_master_transacciones") := NULL]

# Cantidad de descuentos recibidos al operar con tarjetas

dataset[, ctarjeta_descuentos := rowSums(.SD, na.rm = T), .SDcols = c("ctarjeta_visa_descuentos", "ctarjeta_master_descuentos")]
dataset[,c("ctarjeta_visa_descuentos", "ctarjeta_master_descuentos") := NULL]

# Cantidad de seguros que tiene el cliente

dataset[, cseguros := rowSums(.SD, na.rm = T), .SDcols = c("cseguro_vida","cseguro_auto","cseguro_vivienda","cseguro_accidentes_personales")]
dataset[,c("cseguro_vida","cseguro_auto","cseguro_vivienda","cseguro_accidentes_personales") := NULL]

### Homogeneizo distribuciones entre grupos de train y test para las variables que podrían presentar drifting
# por estar vinculadas a la inflación

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

dataset[, total_limite_tarjetas_ranknorm := round(rank(total_limite_tarjetas)/.N,6), by = as.character(foto_mes)]
dataset[,total_financ_tarjetas := NULL]

dataset[, mpayroll_ranknorm := round(rank(mpayroll)/.N,6), by = as.character(foto_mes)]
dataset[,mpayroll := NULL]

dataset[, mpayroll2_ranknorm := round(rank(mpayroll2)/.N,6), by = as.character(foto_mes)]
dataset[,mpayroll2 := NULL]

dataset[, mtransferencias_recibidas_ranknorm := round(rank(mtransferencias_recibidas)/.N,6), by = as.character(foto_mes)]
dataset[,mtransferencias_recibidas := NULL]

dataset[, mtransferencias_emitidas_ranknorm := round(rank(mtransferencias_emitidas)/.N,6), by = as.character(foto_mes)]
dataset[,mtransferencias_emitidas := NULL]

###### Variables historicas ######

setorder(dataset,cols = foto_mes)

## Deltas sobre periodos anteriores (hasta 3 meses)

# N de transacciones
dataset[,c("lag_quarter_trx"):= shift(ctrx_quarter, n = 1, type = "lag"), by = "numero_de_cliente"]
# Saldo de cuentas (ranknorm)

dataset[,c("lag_mcuenta_saldos"):= shift(mcuenta_saldos_ranknorm, n = 1, type = "lag"), by = "numero_de_cliente"]

# N de transacciones tarjetas

dataset[,c("lag_transacciones_tarjetas"):= shift(ctarjetas_transacciones, n = 1, type = "lag"), by = "numero_de_cliente"]
# N de pagos de payroll

dataset[,c("lag_cpayroll_trx"):= shift(cpayroll_trx, n = 1, type = "lag"), by = "numero_de_cliente"]

# monto de pagos de payroll (ranknorm)

dataset[,c("lag_mpayroll"):= shift(mpayroll_ranknorm, n = 1, type = "lag"), by = "numero_de_cliente"]


# N de pagos de payroll2

dataset[,c("lag_cpayroll2_trx"):= shift(cpayroll2_trx, n = 1, type = "lag"), by = "numero_de_cliente"]

# Monto de pagos de payroll2 (ranknorm)

dataset[,c("lag_mpayroll2"):= shift(mpayroll2_ranknorm, n = 1, type = "lag"), by = "numero_de_cliente"]


library(data.table)

fwrite(bd,
       file = "buckets/b1/datasets/competencia_02_FE2.csv",
       sep = ",")


