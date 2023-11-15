require("data.table")
#require("tidyverse")

### FEATURE ENGINEERING ##########

#Cargo datos
dataset <- fread("buckets/b1/datasets/competencia_03.csv.gz")


# Catastrophe (código 'prestado' de Juan Raman)

dataset[foto_mes == 201901, ctransferencias_recibidas := NA]
dataset[foto_mes == 201901, mtransferencias_recibidas := NA ]
dataset[foto_mes == 201902, ctransferencias_recibidas := NA]
dataset[foto_mes == 201902, mtransferencias_recibidas := NA]
dataset[foto_mes == 201903, ctransferencias_recibidas := NA]
dataset[foto_mes == 201903, mtransferencias_recibidas := NA]
dataset[foto_mes == 201904, ctarjeta_visa_debitos_automaticos := NA]
dataset[foto_mes == 201904, ctransferencias_recibidas := NA]
dataset[foto_mes == 201904, mtransferencias_recibidas := NA]
dataset[foto_mes == 201904, mttarjeta_visa_debitos_automaticos := NA]
dataset[foto_mes == 201904, Visa_mfinanciacion_limite := NA]
dataset[foto_mes == 201905, ccomisiones_otras := NA]
dataset[foto_mes == 201905, ctarjeta_visa_debitos_automaticos := NA]
dataset[foto_mes == 201905, ctransferencias_recibidas := NA]
dataset[foto_mes == 201905, mactivos_margen := NA]
dataset[foto_mes == 201905, mcomisiones := NA]
dataset[foto_mes == 201905, mcomisiones_otras := NA]
dataset[foto_mes == 201905, mpasivos_margen := NA]
dataset[foto_mes == 201905, mrentabilidad_annual := NA]
dataset[foto_mes == 201905, mrentabilidad := NA]
dataset[foto_mes == 201905, mtransferencias_recibidas := NA]
dataset[foto_mes == 201910, ccajeros_propios_descuentos := NA]
dataset[foto_mes == 201910, ccomisiones_otras := NA]
dataset[foto_mes == 201910, chomebanking_transacciones := NA]
dataset[foto_mes == 201910, ctarjeta_master_descuentos := NA]
dataset[foto_mes == 201910, ctarjeta_visa_descuentos := NA]
dataset[foto_mes == 201910, mactivos_margen := NA]
dataset[foto_mes == 201910, mcajeros_propios_descuentos := NA]
dataset[foto_mes == 201910, mcomisiones := NA]
dataset[foto_mes == 201910, mcomisiones_otras := NA]
dataset[foto_mes == 201910, mpasivos_margen := NA]
dataset[foto_mes == 201910, mrentabilidad_annual := NA]
dataset[foto_mes == 201910, mrentabilidad := NA]
dataset[foto_mes == 201910, mtarjeta_master_descuentos := NA]
dataset[foto_mes == 201910, mtarjeta_visa_descuentos := NA]
dataset[foto_mes == 202001, cliente_vip := NA]
dataset[foto_mes == 202006, active_quarter := NA]
dataset[foto_mes == 202006, catm_trx := NA]
dataset[foto_mes == 202006, catm_trx_other := NA]
dataset[foto_mes == 202006, ccajas_consultas := NA]
dataset[foto_mes == 202006, ccajas_depositos := NA]
dataset[foto_mes == 202006, ccajas_extracciones := NA]
dataset[foto_mes == 202006, ccajas_otras := NA]
dataset[foto_mes == 202006, ccajas_transacciones := NA]
dataset[foto_mes == 202006, ccallcenter_transacciones := NA]
dataset[foto_mes == 202006, ccheques_depositados := NA]
dataset[foto_mes == 202006, ccheques_depositados_rechazados := NA]
dataset[foto_mes == 202006, ccheques_emitidos := NA]
dataset[foto_mes == 202006, ccheques_emitidos_rechazados := NA]
dataset[foto_mes == 202006, ccomisiones_otras := NA]
dataset[foto_mes == 202006, cextraccion_autoservicio := NA]
dataset[foto_mes == 202006, chomebanking_transacciones := NA]
dataset[foto_mes == 202006, cmobile_app_trx := NA]
dataset[foto_mes == 202006, ctarjeta_debito_transacciones := NA]
dataset[foto_mes == 202006, ctarjeta_master_transacciones := NA]
dataset[foto_mes == 202006, ctarjeta_visa_transacciones := NA]
dataset[foto_mes == 202006, ctrx_quarter := NA]
dataset[foto_mes == 202006, mactivos_margen := NA]
dataset[foto_mes == 202006, matm := NA]
dataset[foto_mes == 202006, matm_other := NA]
dataset[foto_mes == 202006, mautoservicio := NA]
dataset[foto_mes == 202006, mcheques_depositados := NA]
dataset[foto_mes == 202006, mcheques_depositados_rechazados := NA]
dataset[foto_mes == 202006, mcheques_emitidos := NA]
dataset[foto_mes == 202006, mcheques_emitidos_rechazados := NA]
dataset[foto_mes == 202006, mcomisiones := NA]
dataset[foto_mes == 202006, mcomisiones_otras := NA]
dataset[foto_mes == 202006, mcuentas_saldo := NA]
dataset[foto_mes == 202006, mextraccion_autoservicio := NA]
dataset[foto_mes == 202006, mpasivos_margen := NA]
dataset[foto_mes == 202006, mrentabilidad_annual := NA]
dataset[foto_mes == 202006, mrentabilidad := NA]
dataset[foto_mes == 202006, mtarjeta_master_consumo := NA]
dataset[foto_mes == 202006, mtarjeta_visa_consumo := NA]
dataset[foto_mes == 202006, tcallcenter := NA]
dataset[foto_mes == 202006, thomebanking := NA]

# Creo nuevas variables

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

drift_cols <- c("saldo_tarjetas_total",
                "saldo_tarjetas_pesos",
                "saldo_tarjetas_dolares",
                "mcuentas_saldo",
                "mcuenta_corriente",
                "mcaja_ahorro",
                "mtarjetas_total",
                "Visa_mconsumototal",
                "Master_mconsumototal",
                "Master_msaldototal",
                "Visa_msaldototal",
                "Master_mconsumopesos",
                "Visa_mconsumopesos",
                "Master_mconsumodolares",
                "Visa_mconsumodolares",
                "Visa_mpagospesos",
                "Master_mpagosdolares",
                "Visa_mpagominimo",
                "Master_mpagominimo",
                "mcomisiones_mantenimiento",
                "mcomisiones",
                "mcomisiones_otras",
                "mplazo_fijo_pesos",
                "minversion1_pesos",
                "minversion1_dolares",
                "total_financ_tarjetas",
                "total_limite_tarjetas",
                "Visa_mfinanciacion_limite","Master_mfinanciacion_limite",
                "Visa_mlimitecompra","Master_mlimitecompra",
                "mpayroll",
                "mpayroll2",
                "mtransferencias_recibidas",
                "mtransferencias_emitidas")


for (col in drift_cols) {
  
  if (col %in% names(dataset)) {
    dataset[, paste0("std_",col) := (get(col) - mean(get(col),na.rm = T))/sd(get(col),na.rm = T), by = as.character(foto_mes)]
    dataset[,(col) := NULL]
  }
  
}


###### Variables historicas ######

setorder(dataset,cols = foto_mes)

## Lags de variables 1,2 y 6 meses

lagcols <- setdiff(names(dataset),c("numero_de_cliente","foto_mes","clase_ternaria"))


lagcols1_names <- paste0("lag1_",lagcols)
lagcols2_names <- paste0("lag2_",lagcols)
lagcols6_names <- paste0("lag6_", lagcols)


dataset[, (lagcols1_names) :=  shift(.SD, 1), .SDcols = lagcols, by=numero_de_cliente]
dataset[, (lagcols2_names) :=  shift(.SD, 2), .SDcols = lagcols, by=numero_de_cliente]
dataset[, (lagcols6_names) :=  shift(.SD, 6), .SDcols = lagcols, by=numero_de_cliente]

### Deltas 1, 2 y 6 meses

for (col in lagcols) {
  dataset[, paste0("delta1_" ,col) := get(col) - get(paste0("lag1_",col))]
} 

for (col in lagcols) {
  dataset[, paste0("delta2_" ,col) := get(col) - get(paste0("lag2_",col))]
} 

for (col in lagcols) {
  dataset[, paste0("delta6_" ,col) := get(col) - get(paste0("lag6_",col))]
} 

### Media móvil 3 meses

mmovil_names <- paste0("mmovil3_", lagcols)

dataset[,(mmovil_names) := frollmean(.SD, 3, na.rm=T), .SDcols = lagcols, by = numero_de_cliente]


### Corrijo clase ternaria para ultimos dos meses

dataset[,clase_ternaria := fifelse(foto_mes %in% c(202108,202109),NA_character_,clase_ternaria)]

fwrite(dataset,
       file = "buckets/b1/datasets/competencia_03_FE1.csv.gz",
       sep = ",")
