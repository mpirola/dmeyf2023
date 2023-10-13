require(data.table)
require(tidyverse)

dataset <- fread("buckets/b1/datasets/competencia_02_FE4.csv.gz")

# Cuantos clientes se van por mes?

clientes_por_mes <- dataset %>% 
  filter(!foto_mes %in% c(202106,202107)) %>% 
  count(foto_mes, name = "n_clientes") %>% 
  mutate(foto_mes = factor(foto_mes,ordered = T))

bajas_por_mes <- dataset %>% 
  select(numero_de_cliente,foto_mes,clase_ternaria) %>%
  arrange(desc(foto_mes)) %>% 
  filter(!foto_mes %in% c(202106,202107)) %>% 
  distinct(numero_de_cliente,.keep_all = T) %>%
  filter(clase_ternaria != "CONTINUA") %>% 
  mutate(mes_baja = if_else(foto_mes == 201912,202001,
                            if_else(foto_mes == 202012,202101,foto_mes + 1))) %>% 
  count(foto_mes, name = "n_bajas") %>% 
  mutate(foto_mes = factor(foto_mes,ordered = T))

porc_bajas_mes <- bajas_por_mes %>% 
  left_join(clientes_por_mes) %>% 
  mutate(porc = round(100*n_bajas/n_clientes,1))

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



clusters_dbscan <- fread("buckets/b1/datasets/clusters_dbscan.csv.gz")
shap_bajas <- fread("buckets/b1/datasets/shap_bajas.csv.gz")

shap_bajas_vars <- shap_bajas %>% 
  select(-V1) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(media = mean(abs(value))) %>% 
  arrange(desc(media)) %>% 
  slice(1:30) 



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
  slice(1:20) %>% 
  ungroup()


shap_clusters_vars <- shap_clusters_max %>% 
  pivot_wider(id_cols = clusters,
              names_from = name,
              values_from = media)

  


### grafico comparacion

graficar_campo <- function(campo) {
  # quito de grafico las colas del 5% de las densidades
  qA <- quantile(clusters_dbscan[clusters == 7, get(campo)],
                 prob = c(0.05, 0.95), na.rm = TRUE
  )
  
  qB <- quantile(clusters_dbscan[clusters != 7, get(campo)],
                 prob = c(0.05, 0.95), na.rm = TRUE
  )
  
  xxmin <- pmin(qA[[1]], qB[[1]])
  xxmax <- pmax(qA[[2]], qB[[2]])
  
  densidad_A <- density(clusters_dbscan[clusters == 7, get(campo)],
                        kernel = "gaussian", na.rm = TRUE
  )
  
  densidad_B <- density(clusters_dbscan[clusters != 7, get(campo)],
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
         legend = c("7", "otros"),
         col = c("blue", "red"), lty = c(1, 2)
  )
}


campos_buenos <- colnames(shap_clusters_vars %>%
                            filter(clusters == 7) %>% 
                            select(cliente_edad,where(~!all(is.na(.x)))))



for (campo in campos_buenos) {
  cat(campo, "  ")
  graficar_campo(campo)
  jpeg(paste0("my_plot_",campo,".jpeg"))
  dev.off()
}


 
  

prop_clusters <- clusters_dbscan %>% 
  select(clusters) %>% 
  count(clusters) %>% 
  mutate(porc = round(100*n/sum(n),1))

# Analisis temporal de bajas por cluster




  



