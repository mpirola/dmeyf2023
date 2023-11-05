require(data.table)

dataset <- fread("buckets/b1/datasets/competencia_02_FE4.csv.gz")

require(tidyverse)

# Cuantos clientes se van por mes?

clientes_por_mes <- dataset %>% 
  #filter(!foto_mes %in% c(202106,202107)) %>% 
  count(foto_mes, name = "n_clientes") %>% 
  arrange(foto_mes)

bajas_por_mes <- dataset %>% 
  filter(clase_ternaria %in% c("BAJA+1","BAJA+2")) %>% 
  filter(!foto_mes %in% c(202106,202107)) %>% 
  select(numero_de_cliente,foto_mes,clase_ternaria) %>%
  arrange(desc(foto_mes)) %>% 
  distinct(numero_de_cliente,.keep_all = T) %>%
  mutate(mes_baja = if_else(foto_mes == 201912 & clase_ternaria == "BAJA+1",202001,
                            if_else(foto_mes == 202012 & clase_ternaria == "BAJA+1" ,202101,
                                    if_else(foto_mes == 201912 & clase_ternaria == "BAJA+2", 202001,
                                            if_else(foto_mes == 202012 & clase_ternaria == "BAJA+2" ,202102,
                                                    if_else(clase_ternaria == "BAJA+1",foto_mes + 1,
                                                            foto_mes + 2)))))) %>% 
  count(mes_baja, name = "n_bajas") %>% 
  arrange(mes_baja)

porc_bajas_mes <- bajas_por_mes %>% 
  left_join(clientes_por_mes, by = c("mes_baja" = "foto_mes")) %>% 
  mutate(porc = round(100*n_bajas/lag(n_clientes),1)) %>% 
  mutate(mes_baja = factor(mes_baja,ordered = T)) %>% 
  rename(foto_mes = mes_baja)

total_bajas <- sum(bajas_por_mes$n_bajas)
  

gg_bajas_mes <- porc_bajas_mes %>% 
  ggplot(aes(x=foto_mes, y=n_bajas)) +
  geom_bar(stat = "identity", color = "orange",fill = "orange") +
  labs(title = "Clientes baja") +
  ylab("n bajas") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, ),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.line.y = element_line(colour = "light gray",
                                   size = 0.5),
        axis.line.x = element_line(colour = "light gray",
                                   size = 0.5),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1,size = 8),
        legend.title=element_blank(),
        legend.justification = c("right","top"))

gg_bajas_mes

gg_clientes_mes <- porc_bajas_mes %>% 
  ggplot(aes(x=foto_mes, y=n_clientes)) +
  geom_bar(stat = "identity", color = "#00C9A7",fill = "#00C9A7") +
  labs(title = "Clientes premium") +
  ylab("n clientes") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, ),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.line.y = element_line(colour = "light gray",
                                   size = 0.5),
        axis.line.x = element_line(colour = "light gray",
                                   size = 0.5),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1,size = 8),
        legend.title=element_blank(),
        legend.justification = c("right","top"))

gg_clientes_mes

# Caracterìsticas de los clientes que se van

dataset_train <- dataset[foto_mes %in% c(201901, 201902, 201903,202001,202002,202003, 202101, 202102, 202103)] %>% 
  mutate(target = if_else(clase_ternaria == "CONTINUA","CONTINUA","BAJA"))

# grafico ctrx_quarter
gg_ctrx_quarter <- dataset_train %>% 
  select(target,ctrx_quarter) %>% 
  group_by(target) %>% 
  summarise(media = mean(ctrx_quarter)) %>% 
  ggplot(aes(x=target, y= media)) +
  geom_bar(stat = "identity", color = "orange",fill = "orange") +
  labs(title = "N promedio de transacciones voluntarias") +
  ylab("N promedio") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, ),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.line.y = element_line(colour = "light gray",
                                   size = 0.5),
        axis.line.x = element_line(colour = "light gray",
                                   size = 0.5),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1,size = 8),
        legend.title=element_blank(),
        legend.justification = c("right","top"))

gg_ctrx_quarter

# grafico mcaja_ahorro
gg_mcaja_ahorro <- dataset_train %>% 
  select(target,mcaja_ahorro) %>% 
  group_by(target) %>% 
  summarise(media = mean(mcaja_ahorro)) %>% 
  ggplot(aes(x=target, y= media)) +
  geom_bar(stat = "identity", color = "orange",fill = "orange") +
  labs(title = "Promedio de saldo en caja de ahorro") +
  ylab("Saldo promedio ($)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, ),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.line.y = element_line(colour = "light gray",
                                   size = 0.5),
        axis.line.x = element_line(colour = "light gray",
                                   size = 0.5),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1,size = 8),
        legend.title=element_blank(),
        legend.justification = c("right","top"))

gg_mcaja_ahorro

# grafico cpayroll_trx
gg_cpayroll_trx <- dataset_train %>% 
  select(target,cpayroll_trx) %>% 
  group_by(target) %>% 
  summarise(media = mean(cpayroll_trx)) %>% 
  ggplot(aes(x=target, y= media)) +
  geom_bar(stat = "identity", color = "orange",fill = "orange") +
  labs(title = "Cantidad promedio de acreditaciones de haberes") +
  ylab("N promedio") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, ),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.line.y = element_line(colour = "light gray",
                                   size = 0.5),
        axis.line.x = element_line(colour = "light gray",
                                   size = 0.5),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1,size = 8),
        legend.title=element_blank(),
        legend.justification = c("right","top"))

gg_cpayroll_trx


#### Análisis por cluster

#Recupero numero_de_cliente

clusters_dbscan <- fread("buckets/b1/datasets/clusters_dbscan_2.csv.gz") 

shap_bajas <- fread("buckets/b1/datasets/shap_bajas_2.csv.gz")



# Analisis de rentabilidad 

datos_clientes_clusters <- clusters_dbscan %>% 
  arrange(foto_mes) %>% 
  distinct(numero_de_cliente,.keep_all = T) %>%
  select(numero_de_cliente,clusters) %>% 
  left_join(dataset)


rentabilidad_cluster <- datos_clientes_clusters %>%
  group_by(clusters) %>% 
  summarize(media_rent_clusters = mean(mrentabilidad,na.rm = T))

datos_clientes_clusters <- datos_clientes_clusters %>% 
  filter(clusters %in% c(0,1,3,4,5))
  
promedio_mcomisiones <- datos_clientes_clusters %>% 
  group_by(clusters) %>% 
  summarize(media_mcomisiones= mean(mcomisiones_ranknorm,na.rm = T)) 

promedio_mactivos_margen <- datos_clientes_clusters %>% 
  group_by(clusters) %>% 
  summarize(media_mactivos= mean(mactivos_margen,na.rm = T))

promedio_mpasivos_margen <- datos_clientes_clusters %>% 
  group_by(clusters) %>% 
  summarize(media_mpasivos= mean(mpasivos_margen,na.rm = T))

promedio_consumos_tarjetas <- datos_clientes_clusters %>% 
  group_by(clusters) %>% 
  summarize(consumos_tarjetas = mean(mtarjetas_total_ranknorm,na.rm = T))

promedio_msaldos <- datos_clientes_clusters %>% 
  group_by(clusters) %>% 
  summarize(msaldos = mean(mcuenta_saldos_ranknorm ,na.rm = T)) 

promedio_trx <- datos_clientes_clusters %>% 
  group_by(clusters) %>% 
  summarize(ctrx_quarter = mean(ctrx_quarter,na.rm = T))

promedio_inversiones <- datos_clientes_clusters %>% 
  group_by(clusters) %>% 
  summarize(prom_inversiones = mean(minversion2))

promedio_saldo_prestamos <- datos_clientes_clusters %>% 
  group_by(clusters) %>% 
  summarize(prom_saldo_prestamos = mean(mprestamos_hipotecarios + mprestamos_prendarios + mprestamos_personales))

mes_baja_prop <- clusters_dbscan %>%
  select(numero_de_cliente,clusters) %>% 
  left_join(dataset %>% 
              filter(clase_ternaria %in% c("BAJA+1","BAJA+2")) %>% 
              filter(!foto_mes %in% c(202106,202107)) %>% 
  select(numero_de_cliente,foto_mes,clase_ternaria) %>%
  arrange(desc(foto_mes)) %>% 
  distinct(numero_de_cliente,.keep_all = T) %>%
  mutate(mes_baja = if_else(foto_mes == 201912 & clase_ternaria == "BAJA+1",202001,
                            if_else(foto_mes == 202012 & clase_ternaria == "BAJA+1" ,202101,
                                    if_else(foto_mes == 201912 & clase_ternaria == "BAJA+2", 202001,
                                            if_else(foto_mes == 202012 & clase_ternaria == "BAJA+2" ,202102,
                                                    if_else(clase_ternaria == "BAJA+1",foto_mes + 1,
                                                            foto_mes + 2))))))) %>% 
  count(clusters,mes_baja) %>% 
  group_by(clusters) %>% 
  mutate(prop = round(100*n/sum(n),1)) %>% 
  arrange(clusters,desc(prop))


shap_bajas_vars <- shap_bajas %>% 
  select(-V1) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(media = mean(abs(value))) %>% 
  arrange(desc(media)) %>% 
  slice(1:20) 



# Veo vars mas importantes 
shap_clusters_max <- clusters_dbscan %>% 
  select(clusters) %>% 
  bind_cols(shap_bajas %>% 
              select(-V1)) %>% 
  pivot_longer(cols = -clusters) %>% 
  group_by(clusters,name) %>% 
  summarise(media = mean(value)) %>% 
  ungroup() %>% 
  group_by(clusters) %>% 
  arrange(desc(media)) %>% 
  slice(1:10) %>% 
  ungroup()


shap_clusters_vars <- shap_clusters_max %>% 
  pivot_wider(id_cols = clusters,
              names_from = name,
              values_from = media)

  


### grafico comparacion
### grafico comparacion

graficar_campo <- function(campo) {
  # quito de grafico las colas del 5% de las densidades
  qA <- quantile(clusters_dbscan[clusters == 1, get(campo)],
                 prob = c(0.05, 0.95), na.rm = TRUE
  )
  
  qB <- quantile(clusters_dbscan[clusters != 1, get(campo)],
                 prob = c(0.05, 0.95), na.rm = TRUE
  )
  
  xxmin <- pmin(qA[[1]], qB[[1]])
  xxmax <- pmax(qA[[2]], qB[[2]])
  
  densidad_A <- density(clusters_dbscan[clusters == 0, get(campo)],
                        kernel = "gaussian", na.rm = TRUE
  )
  
  densidad_B <- density(clusters_dbscan[clusters != 0, get(campo)],
                        kernel = "gaussian", na.rm = TRUE
  )
  
  plot(densidad_A,
       col = "blue",
       xlim = c(xxmin, xxmax),
       ylim = c(0, pmax(max(densidad_A$y), max(densidad_B$y))),
       main = campo
  )
  
  lines(densidad_B, col = "red", lty = 2)
  
  legend("topright",
         legend = c("1", "otros"),
         col = c("blue", "red"), lty = c(1, 2)
  )
}


campos_buenos <- colnames(shap_clusters_vars %>%
                            filter(clusters == 1) %>% 
                            select(where(~!all(is.na(.x)))))

for (campo in campos_buenos) {
  cat(campo, "  ")
  graficar_campo(campo)
  jpeg(paste0("my_plot_",campo,".jpeg"))
  dev.off()
}
 




prop_clusters <- clusters_dbscan %>% 
  count(clusters) %>% 
  mutate(porc = round(100*n/sum(n),1))

# Analisis temporal de bajas por cluster

ultimos_meses_clientes <- datos_clientes_clusters %>%
  group_by(numero_de_cliente) %>% 
  arrange(desc(foto_mes)) %>% 
  slice(1:6) %>% 
  mutate(rank_mes = rank(foto_mes)) %>% 
  ungroup()

# Cluster 0

cluster_0 <- ultimos_meses_clientes %>%
  filter(clusters == 0) %>% 
  select(rank_mes,
         mtarjeta_visa_consumo,
         mtarjeta_master_consumo,
         chomebanking_transacciones,
         mpayroll_ranknorm,
         mcuenta_saldos_ranknorm,
         mprestamos_personales,
         mprestamos_prendarios,
         mprestamos_hipotecarios) %>% 
  mutate(mtarjeta_total = sum(mtarjeta_visa_consumo,mtarjeta_master_consumo,na.rm = T),
         mprestamos_total = sum(mprestamos_prendarios,mprestamos_personales,mprestamos_hipotecarios,na.rm = T)) %>% 
  select(-mtarjeta_visa_consumo,-mtarjeta_master_consumo) %>% 
  pivot_longer(cols = -rank_mes) %>% 
  group_by(rank_mes,name) %>% 
  summarize(total = round(sum(value))) %>% 
  mutate(rank_mes = factor(rank_mes)) %>% 
  pivot_wider(id_cols = rank_mes,
              values_from = total)

gg_cluster_0_monto_tarjeta <- cluster_0 %>%  
  ggplot(aes(x=rank_mes, y= mtarjeta_total/1000000)) +
  geom_bar(stat = "identity", color = "#00C9A7",fill = "#00C9A7") +
  labs(title = "Consumos con tarjeta") +
  ylab("Millones $") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, ),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.line.y = element_line(colour = "light gray",
                                   size = 0.5),
        axis.line.x = element_line(colour = "light gray",
                                   size = 0.5),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1,size = 8),
        legend.title=element_blank(),
        legend.justification = c("right","top"))

gg_cluster_0_monto_tarjeta


# Cluster 1

cluster_1 <- ultimos_meses_clientes %>%
  filter(clusters == 1) %>% 
  select(rank_mes,
         ctarjeta_visa_debitos_automaticos,
         chomebanking_transacciones,
         ctarjetas_transacciones,
         cmobile_app_trx) %>% 
  pivot_longer(cols = -rank_mes) %>% 
  group_by(rank_mes,name) %>% 
  summarize(total = round(sum(value))) %>% 
  mutate(rank_mes = factor(rank_mes)) %>% 
  pivot_wider(id_cols = rank_mes,
              values_from = total)

gg_cluster_1_debitos <- cluster_1 %>%  
  ggplot(aes(x=rank_mes, y=ctarjeta_visa_debitos_automaticos)) +
  geom_bar(stat = "identity", color = "#00C9A7",fill = "#00C9A7") +
  labs(title = "Debitos automaticos Visa") +
  ylab("N debitos") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, ),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.line.y = element_line(colour = "light gray",
                                   size = 0.5),
        axis.line.x = element_line(colour = "light gray",
                                   size = 0.5),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1,size = 8),
        legend.title=element_blank(),
        legend.justification = c("right","top"))

gg_cluster_1_debitos

gg_cluster_1_hb <- cluster_1 %>%  
  ggplot(aes(x=rank_mes, y=chomebanking_transacciones)) +
  geom_bar(stat = "identity", color = "#00C9A7",fill = "#00C9A7") +
  labs(title = "Transacciones homebanking") +
  ylab("N transacciones") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, ),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.line.y = element_line(colour = "light gray",
                                   size = 0.5),
        axis.line.x = element_line(colour = "light gray",
                                   size = 0.5),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1,size = 8),
        legend.title=element_blank(),
        legend.justification = c("right","top"))

gg_cluster_1_hb


# Cluster 3 


cluster_3 <- ultimos_meses_clientes %>%
  filter(clusters == 3) %>% 
  select(rank_mes,
         mprestamos_personales,
         mpayroll_ranknorm,
         ctarjetas_transacciones,
         minversion_pesos_ranknorm) %>%
  pivot_longer(cols = -rank_mes) %>% 
  group_by(rank_mes,name) %>% 
  summarize(total = round(sum(value))) %>% 
  mutate(rank_mes = factor(rank_mes)) %>% 
  pivot_wider(id_cols = rank_mes,
              values_from = total)

gg_cluster_3_prestamos <- cluster_3 %>%  
  ggplot(aes(x=rank_mes, y=mprestamos_personales/1000000)) +
  geom_bar(stat = "identity", color = "#00C9A7",fill = "#00C9A7") +
  labs(title = "Deuda prestamos personales") +
  ylab("Millones $") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, ),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.line.y = element_line(colour = "light gray",
                                   size = 0.5),
        axis.line.x = element_line(colour = "light gray",
                                   size = 0.5),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1,size = 8),
        legend.title=element_blank(),
        legend.justification = c("right","top"))

gg_cluster_3_prestamos


gg_cluster_3_inversion <- cluster_3 %>%  
  ggplot(aes(x=rank_mes, y=minversion_pesos_ranknorm)) +
  geom_bar(stat = "identity", color = "#00C9A7",fill = "#00C9A7") +
  labs(title = "Inversiones en pesos (normalizada)") +
  ylab("Rank sum") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, ),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.line.y = element_line(colour = "light gray",
                                   size = 0.5),
        axis.line.x = element_line(colour = "light gray",
                                   size = 0.5),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1,size = 8),
        legend.title=element_blank(),
        legend.justification = c("right","top"))

gg_cluster_3_inversion

# Cluster 4


cluster_4 <- ultimos_meses_clientes %>%
  filter(clusters == 4) %>% 
  select(rank_mes,
         mprestamos_personales,
         mtarjeta_visa_consumo) %>%
  pivot_longer(cols = -rank_mes) %>% 
  group_by(rank_mes,name) %>% 
  summarize(total = round(sum(value))) %>% 
  mutate(rank_mes = factor(rank_mes)) %>% 
  pivot_wider(id_cols = rank_mes,
              values_from = total)

gg_cluster_4_consumo_visa <- cluster_4 %>%  
  ggplot(aes(x=rank_mes, y= mtarjeta_visa_consumo/1000000)) +
  geom_bar(stat = "identity", color = "#00C9A7",fill = "#00C9A7") +
  labs(title = "Consumos con tarjeta") +
  ylab("Millones $") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, ),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.line.y = element_line(colour = "light gray",
                                   size = 0.5),
        axis.line.x = element_line(colour = "light gray",
                                   size = 0.5),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1,size = 8),
        legend.title=element_blank(),
        legend.justification = c("right","top"))

gg_cluster_4_consumo_visa

gg_cluster_4_prestamos <- cluster_4 %>%  
  ggplot(aes(x=rank_mes, y=mprestamos_personales/1000000)) +
  geom_bar(stat = "identity", color = "#00C9A7",fill = "#00C9A7") +
  labs(title = "Deuda prestamos personales") +
  ylab("Millones $") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, ),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.line.y = element_line(colour = "light gray",
                                   size = 0.5),
        axis.line.x = element_line(colour = "light gray",
                                   size = 0.5),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1,size = 8),
        legend.title=element_blank(),
        legend.justification = c("right","top"))

gg_cluster_4_prestamos


# Cluster 5


cluster_5 <- ultimos_meses_clientes %>%
  filter(clusters == 5) %>% 
  select(rank_mes,
         mprestamos_personales,
         mtarjeta_visa_consumo,
         mcuenta_saldos_ranknorm,
         mprestamos_prendarios) %>%
  pivot_longer(cols = -rank_mes) %>% 
  group_by(rank_mes,name) %>% 
  summarize(total = round(sum(value))) %>% 
  mutate(rank_mes = factor(rank_mes)) %>% 
  pivot_wider(id_cols = rank_mes,
              values_from = total)

gg_cluster_5_prendarios <- cluster_5 %>%  
  ggplot(aes(x=rank_mes, y= mprestamos_prendarios)) +
  geom_bar(stat = "identity", color = "#00C9A7",fill = "#00C9A7") +
  labs(title = "Deuda prestamos prendarios") +
  ylab("$") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, ),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.line.y = element_line(colour = "light gray",
                                   size = 0.5),
        axis.line.x = element_line(colour = "light gray",
                                   size = 0.5),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1,size = 8),
        legend.title=element_blank(),
        legend.justification = c("right","top"))

gg_cluster_5_prendarios

gg_cluster_5_saldos <- cluster_5 %>%  
  ggplot(aes(x=rank_mes, y=mcuenta_saldos_ranknorm)) +
  geom_bar(stat = "identity", color = "#00C9A7",fill = "#00C9A7") +
  labs(title = "Saldo cuentas (normalizado)") +
  ylab("Rank sum") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, ),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.line.y = element_line(colour = "light gray",
                                   size = 0.5),
        axis.line.x = element_line(colour = "light gray",
                                   size = 0.5),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1,size = 8),
        legend.title=element_blank(),
        legend.justification = c("right","top"))

gg_cluster_5_saldos


