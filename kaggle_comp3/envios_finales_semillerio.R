
#Levanto archivos de predicciones


experimento <- "KA8240_FE1_goss_v4_kaggle"

dir <- paste0("~/buckets/b1/exp/",experimento)


archivos <- list.files(dir, pattern = "pred",full.names = T)

for (i in 1:length(archivos)) {
  
  if (i == 1) {
    
    predicciones <- read.csv(archivos[i],sep = ",")
    
  } else {
    
    temp <- read.csv(archivos[i],sep = ",")
    names(temp) <- c("numero_de_cliente","foto_mes",paste0("prob_",i))
    
    predicciones<-merge(predicciones,temp, by.x = c("numero_de_cliente","foto_mes"), by.y = c("numero_de_cliente","foto_mes"))
    
  }
  
} 

library(tidyverse)


predicciones <- predicciones %>% 
  pivot_longer(cols = -c(numero_de_cliente,foto_mes)) %>% 
  group_by(numero_de_cliente,foto_mes) %>% 
  mutate(prob_media = mean(value)) %>% 
  ungroup() %>% 
  distinct(numero_de_cliente,.keep_all = T)

require(data.table)

predicciones <- predicciones %>% 
  arrange(desc(prob_media)) %>% 
  as.data.table()

cortes <- c(seq(9000, 12000, by = 250))



for (envios in cortes) {

  tb_entrega <- predicciones[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]
  
  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
         file = paste0(dir,"/",experimento,"_",envios,".csv"),
         sep = ","
  )

}
