# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")



# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()

#### uso los hp de la OB realizada para v2

PARAM$experimento <- "KA8240_FE1_hpbasicos_v4_kaggle"

PARAM$input$dataset <- "./datasets/competencia_03_FE1.csv.gz"

# meses donde se entrena el modelo.
# roll forward un mes
PARAM$input$training <- c(201901,201902,201903,201904,201905,201906,201907,201908,201909,201910,201911,201912,
                          202002,202003,202004,
                          202011,202012,
                          202101,202102,202103,202104,202105,202106,202107)

PARAM$input$future <- c(202109) # meses donde se aplica el modelo


semillas <- c(528881, 583613, 661417, 894407, 915251)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)



#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

dapply <- dataset[foto_mes == PARAM$input$future]

# genero el modelo

ganancias <- tibble::tribble(~semilla,~ganancia,~envios)

probabilidades <- dapply[,c(numero_de_cliente,foto_mes)]

### Uso mismos hiperparámetros que OB v2

for (i in 1:5) {
  
  PARAM$finalmodel$semilla <- semillas[i]
  
  # hiperparametros intencionalmente 
  PARAM$finalmodel$optim$num_iterations <- 478
  
  PARAM$finalmodel$optim$learning_rate <- 0.020101787749407
  
  PARAM$finalmodel$optim$feature_fraction <- 0.924527761633925


  PARAM$finalmodel$optim$min_data_in_leaf <-6904
  
  PARAM$finalmodel$optim$num_leaves <- 740

    


  
  # Hiperparametros FIJOS de  lightgbm
  PARAM$finalmodel$lgb_basicos <- list(
    boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
    objective = "binary",
    metric = "custom",
    
    
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    force_row_wise = TRUE, # para reducir warnings
    verbosity = -100,

    min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
    min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
    lambda_l1 = 0.0, # lambda_l1 >= 0.0
    lambda_l2 = 0.0, # lambda_l2 >= 0.0
    max_bin = 31L, # lo debo dejar fijo, no participa de la BO
    max_depth = -1,
    

    is_unbalance = FALSE, #
    scale_pos_weight = 1.0, # scale_pos_weight > 0.0
    
    drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
    max_drop = 50, # <=0 means no limit
    skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0
    
    extra_trees = TRUE, # Magic Sauce
    
    seed = PARAM$finalmodel$semilla
  )
  
  param_completo <- c(PARAM$finalmodel$lgb_basicos,
                      PARAM$finalmodel$optim)
  

  modelo <- lgb.train(
    data = dtrain,
    param = param_completo,
  )
  
  #--------------------------------------
  # ahora imprimo la importancia de variables
  tb_importancia <- as.data.table(lgb.importance(modelo))
  archivo_importancia <- paste0("impo_",i,".txt")
  
  fwrite(tb_importancia,
         file = archivo_importancia,
         sep = "\t"
  )
  
  #--------------------------------------
  
  
  # aplico el modelo a los datos sin clase
  dapply <- dataset[foto_mes == PARAM$input$future]
  
  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  
  # genero la tabla de entrega
  
  
  tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
  tb_entrega[, prob := prediccion]
  
  #tb_probs <- tb_entrega[, (paste0("prob_",i)) := prediccion]
  
  #probabilidades <- probabilidades[tb_probs, on = c("numero_de_cliente","foto_mes"), nomatch = 0]
  
  #grabo las probabilidad del modelo
  fwrite(tb_entrega,
         file = paste0("prediccion_",i,".csv"),
         sep = ","
  )
  
  # ordeno por probabilidad descendente
  # setorder(tb_entrega, -prob)
  
  
  # genero archivos con los  "envios" mejores
# 
#   cortes <- c(seq(9000, 12000, by = 250))
#   
#   for (envios in cortes) {
#     
#     tb_entrega[, Predicted := 0L]
#     tb_entrega[1:envios, Predicted := 1L]
#     
#     # fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
#     #        file = paste0(PARAM$experimento, "_", i,"_",envios,".csv"),
#     #        sep = ","
#     # )
#     
#     # tb_ganancias <- tb_entrega[truth, on = c("numero_de_cliente"), nomatch = 0]
#     # tb_ganancias <- tb_ganancias[Predicted == 1,]
#     # tb_ganancias[,gan := fifelse(clase_ternaria == "BAJA+2",273000,-7000)]
#     
#     # ganancia <- tibble::tribble(~semilla,~ganancia,~envios,
#     #                             semillas[i], sum(tb_ganancias$gan),envios)
#     # 
#     # ganancias <- rbind(ganancias,ganancia)
#     
#   }
  
  print(paste0("Iteracion ",i, " finalizada"))
  
}

# write.csv(ganancias,
#        file = paste0(PARAM$experimento, "_ganancias_semillerio.csv"),
#        sep = ","
# )

write.csv(probabilidades,
          file = paste0(PARAM$experimento, "_probs_semillerio.csv"),
          sep = ","
)
  
cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
  



