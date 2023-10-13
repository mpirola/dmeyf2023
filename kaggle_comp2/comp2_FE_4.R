require(data.table)


dataset <- fread("buckets/b1/datasets/competencia_02.csv.gz")


require(data.table)


### Colapso algunas variables en una

# Cantidad comisiones
dataset[, mcomisiones := rowSums(.SD, na.rm = T), .SDcols = c("mcomisiones_mantenimiento","mcomisiones_otras")]

# Cantidad de tarjetas
dataset[, ctarjetas_total := rowSums(.SD, na.rm = T), .SDcols = c("ctarjeta_master","ctarjeta_visa")]

# Suma de consumos de tarjetas
dataset[, mtarjetas_total := rowSums(.SD, na.rm = T), .SDcols =  c("Visa_mconsumototal","Master_mconsumototal")]

# Suma de saldos totales de tarjetas 
dataset[, saldo_tarjetas_total := rowSums(.SD, na.rm = T), .SDcols = c("Visa_msaldototal","Master_msaldototal")]

# Suma de saldos en pesos de tarjetas 
dataset[, saldo_tarjetas_pesos := rowSums(.SD, na.rm = T), .SDcols = c("Visa_msaldopesos","Master_msaldopesos")]

# Suma de saldos en dolares de tarjetas 
dataset[, saldo_tarjetas_dolares := rowSums(.SD, na.rm = T), .SDcols = c("Visa_msaldodolares","Master_msaldodolares")]

# Suma de limite financiacion de tarjetas 
dataset[, total_financ_tarjetas := rowSums(.SD, na.rm = T), .SDcols = c("Visa_mfinanciacion_limite","Master_mfinanciacion_limite")]

# Suma de limite total de tarjetas

dataset[,total_limite_tarjetas := rowSums(.SD, na.rm = T), .SDcols = c("Visa_mlimitecompra","Master_mlimitecompra")]

# Suma de transacciones realizadas con tarjetas

dataset[, ctarjetas_transacciones := rowSums(.SD, na.rm = T), .SDcols =  c("ctarjeta_visa_transacciones","ctarjeta_master_transacciones")]

# Cantidad de descuentos recibidos al operar con tarjetas

dataset[, ctarjeta_descuentos := rowSums(.SD, na.rm = T), .SDcols = c("ctarjeta_visa_descuentos", "ctarjeta_master_descuentos")]

# Cantidad de seguros que tiene el cliente

dataset[, cseguros := rowSums(.SD, na.rm = T), .SDcols = c("cseguro_vida","cseguro_auto","cseguro_vivienda","cseguro_accidentes_personales")]

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


dataset[, mcomisiones_mantenimiento_ranknorm := round(rank(mcomisiones_mantenimiento)/.N,6), by = as.character(foto_mes)]
dataset[,mcomisiones_mantenimiento := NULL]

dataset[, mcomisiones_ranknorm := round(rank(mcomisiones)/.N,6), by = as.character(foto_mes)]
dataset[,mcomisiones := NULL]

dataset[, mcomisiones_otras_ranknorm := round(rank(mcomisiones_otras)/.N,6), by = as.character(foto_mes)]
dataset[,mcomisiones_otras := NULL]

dataset[, cplazo_fijo_ranknorm := round(rank(cplazo_fijo)/.N,6), by = as.character(foto_mes)]
dataset[,cplazo_fijo := NULL]

dataset[, mplazo_fijo_ranknorm := round(rank(mplazo_fijo_pesos)/.N,6), by = as.character(foto_mes)]
dataset[,mplazo_fijo_pesos := NULL]

dataset[, minversion_pesos_ranknorm := round(rank(minversion1_pesos)/.N,6), by = as.character(foto_mes)]
dataset[,minversion1_pesos := NULL]

dataset[, minversion_usd_ranknorm := round(rank(minversion1_dolares)/.N,6), by = as.character(foto_mes)]
dataset[,minversion1_dolares := NULL]


dataset[, total_financ_tarjetas_ranknorm := round(rank(total_financ_tarjetas)/.N,6), by = as.character(foto_mes)]
dataset[,total_financ_tarjetas := NULL]

dataset[, total_limite_tarjetas_ranknorm := round(rank(total_limite_tarjetas)/.N,6), by = as.character(foto_mes)]
dataset[,total_limite_tarjetas := NULL]

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

###### Catastrophe #######


dataset[,mcomisiones_ranknorm := fifelse(foto_mes %in% c(201905,201910,202006),NA_real_,mcomisiones_ranknorm)]
dataset[,mactivos_margen := fifelse(foto_mes %in% c(201905,201910,202006),NA_real_,mactivos_margen)]
dataset[,mpasivos_margen := fifelse(foto_mes %in% c(201905,201910,202006),NA_real_,mpasivos_margen)]
dataset[,mrentabilidad := fifelse(foto_mes %in% c(201905,201910,202006),NA_real_,mrentabilidad)]
dataset[,mrentabilidad_annual := fifelse(foto_mes %in% c(201905,201910,202006),NA_real_,mrentabilidad_annual)]
dataset[,mttarjeta_visa_debitos_automaticos := fifelse(foto_mes==201904,NA_real_,mttarjeta_visa_debitos_automaticos)]
dataset[,ctarjeta_visa_debitos_automaticos := fifelse(foto_mes==201904,NA_real_,ctarjeta_visa_debitos_automaticos)]
dataset[,ccomisiones_otras := fifelse(foto_mes %in% c(201905,201910),NA_real_,ccomisiones_otras)]
dataset[,mcomisiones_otras_ranknorm := fifelse(foto_mes %in% c(201905,201910),NA_real_,mcomisiones_otras_ranknorm)]
dataset[,ctransferencias_recibidas := fifelse(foto_mes %in% c(201901,201905),NA_real_,ctransferencias_recibidas)]
dataset[,mtransferencias_recibidas_ranknorm := fifelse(foto_mes %in% c(201901,201905),NA_real_,mtransferencias_recibidas_ranknorm)]
dataset[,Visa_fultimo_cierre := fifelse(foto_mes == 201907,NA_real_,Visa_fultimo_cierre)]
dataset[,active_quarter := fifelse(foto_mes == 202006,NA_real_,active_quarter)]


## Lags de variables 1,3 y 6 meses

lagcols <- c("ctrx_quarter",
             "active_quarter",
             "cproductos",
             "ctarjetas_transacciones",
             "mcuenta_saldos_ranknorm",
             "cpayroll_trx",
             "mpayroll_ranknorm",
             "cpayroll2_trx",
             "mpayroll2_ranknorm",
             "cseguros",
             "total_limite_tarjetas_ranknorm",
             "minversion_pesos_ranknorm",
             "minversion_usd_ranknorm",
             "mcomisiones_ranknorm",
             "mcomisiones_otras_ranknorm",
             "mcomisiones_mantenimiento_ranknorm")
             

lagcols1_names <- paste0("lag1_",lagcols)
lagcols3_names <- paste0("lag3_",lagcols)
lagcols6_names <- paste0("lag6_", lagcols)


dataset[, (lagcols1_names) :=  shift(.SD, 1), .SDcols = lagcols, by=numero_de_cliente]
dataset[, (lagcols3_names) :=  shift(.SD, 3), .SDcols = lagcols, by=numero_de_cliente]
dataset[, (lagcols6_names) :=  shift(.SD, 6), .SDcols = lagcols, by=numero_de_cliente]

### Pseudo-tendencia


dataset[, tend_ctrx_quarter:= fifelse(rank(foto_mes) == 1, NA_real_, round((ctrx_quarter - first(ctrx_quarter))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]
dataset[, tend_active_quarter:= fifelse(rank(foto_mes) == 1, NA_real_, round((active_quarter - first(active_quarter))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]
dataset[, tend_cproductos:= fifelse(rank(foto_mes) == 1, NA_real_, round((cproductos - first(cproductos))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]
dataset[, tend_ctarjetas_transacciones:= fifelse(rank(foto_mes) == 1, NA_real_, round((ctarjetas_transacciones - first(ctarjetas_transacciones))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]
dataset[, tend_mcuenta_saldos_ranknorm:= fifelse(rank(foto_mes) == 1, NA_real_, round((mcuenta_saldos_ranknorm - first(mcuenta_saldos_ranknorm))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]
dataset[, tend_cpayroll_trx:= fifelse(rank(foto_mes) == 1, NA_real_, round((cpayroll_trx - first(cpayroll_trx))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]
dataset[, tend_mpayroll_ranknorm:= fifelse(rank(foto_mes) == 1, NA_real_, round((mpayroll_ranknorm - first(mpayroll_ranknorm))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]
dataset[, tend_cpayroll2_trx:= fifelse(rank(foto_mes) == 1, NA_real_, round((cpayroll2_trx - first(cpayroll2_trx))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]
dataset[, tend_mpayroll2_ranknorm:= fifelse(rank(foto_mes) == 1, NA_real_, round((mpayroll2_ranknorm - first(mpayroll2_ranknorm))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]
dataset[, tend_cseguros:= fifelse(rank(foto_mes) == 1, NA_real_, round((cseguros - first(cseguros))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]
dataset[, tend_total_limite_tarjetas_ranknorm:= fifelse(rank(foto_mes) == 1, NA_real_, round((total_limite_tarjetas_ranknorm - first(total_limite_tarjetas_ranknorm))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]
dataset[, tend_minversion_pesos_ranknorm:= fifelse(rank(foto_mes) == 1, NA_real_, round((minversion_pesos_ranknorm - first(minversion_pesos_ranknorm))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]
dataset[, tend_minversion_usd_ranknorm:= fifelse(rank(foto_mes) == 1, NA_real_, round((minversion_usd_ranknorm - first(minversion_usd_ranknorm))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]
dataset[, tend_mcomisiones_ranknorm:= fifelse(rank(foto_mes) == 1, NA_real_, round((mcomisiones_ranknorm - first(mcomisiones_ranknorm))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]
dataset[, tend_mcomisiones_otras_ranknorm:= fifelse(rank(foto_mes) == 1, NA_real_, round((mcomisiones_otras_ranknorm - first(mcomisiones_otras_ranknorm))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]
dataset[, tend_mcomisiones_mantenimiento_ranknorm:= fifelse(rank(foto_mes) == 1, NA_real_, round((mcomisiones_mantenimiento_ranknorm - first(mcomisiones_mantenimiento_ranknorm))/(rank(foto_mes)-1),5)), by = "numero_de_cliente"]


### Media móvil 3 meses

mmovil_names <- paste0("mmovil3_", lagcols)

dataset[,(mmovil_names) := frollmean(.SD, 3, na.rm=T), .SDcols = lagcols, by = numero_de_cliente]

### Corrijo clase ternaria para ultimos dos meses

dataset[,clase_ternaria := fifelse(foto_mes %in% c(202106,202107),NA_character_,clase_ternaria)]

fwrite(dataset,
       file = "buckets/b1/datasets/competencia_02_FE4.csv.gz",
       sep = ",")


