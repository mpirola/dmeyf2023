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
PARAM$experimento <- "KA8240_FE1_bag_v1_test"

PARAM$input$dataset <- "./datasets/competencia_03_FE1.csv.gz"

# meses donde se entrena el modelo.
# roll forward un mes
PARAM$input$training <- c(201902,201903,201904,201905,201906,
                          202002,202003,202004,
                          202102,202103,202104,202105,202106)

PARAM$input$future <- c(202107) # meses donde se aplica el modelo

semillas <- c(528881, 583613, 661417, 894407, 915251,
              173827, 173839, 173867, 547093, 547103,
              638269, 638303, 638359, 721181, 837451, 
              878173, 910771, 910781, 942659, 942661)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

truth <- dataset[foto_mes == PARAM$input$future,c("numero_de_cliente","clase_ternaria")]


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


# genero el modelo

ganancias <- tibble::tribble(~semilla,~ganancia,~envios)


for (i in 1:20) {
  
  PARAM$finalmodel$semilla <- semillas[i]
  
  # hiperparametros intencionalmente 
  PARAM$finalmodel$optim$num_iterations <- 714
  
  PARAM$finalmodel$optim$learning_rate <- 0.0537381844560982
  
  PARAM$finalmodel$optim$feature_fraction <- 0.297652346092468
  
  PARAM$finalmodel$optim$feature_fraction_bynode <- 0.569927498248113
  
  PARAM$finalmodel$optim$min_data_in_leaf <- 2558
  
  PARAM$finalmodel$optim$num_leaves <- 232
  
  PARAM$finalmodel$optim$max_depth <- 10
    
  PARAM$finalmodel$optim$bagging_fraction <- 0.018818565432286
  
  PARAM$finalmodel$optim$pos_bagging_fraction <- 0.98225566730848
  
  PARAM$finalmodel$optim$neg_bagging_fraction <- 0.647930437995065
  
  PARAM$finalmodel$optim$bagging_freq <- 26
  
  
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
  
  # grabo las probabilidad del modelo
  # fwrite(tb_entrega,
  #        file = paste0("prediccion_",i,".txt"),
  #        sep = "\t"
  # )
  
  
  # ordeno por probabilidad descendente
  setorder(tb_entrega, -prob)
  
  
  # genero archivos con los  "envios" mejores
  
  cortes <- c(seq(9500, 12000, by = 250))
  
  for (envios in cortes) {
    
    tb_entrega[, Predicted := 0L]
    tb_entrega[1:envios, Predicted := 1L]
    
    fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
           file = paste0(PARAM$experimento, "_", i,"_",envios,".csv"),
           sep = ","
    )
    
    tb_ganancias <- tb_entrega[truth, on = c("numero_de_cliente"), nomatch = 0]
    tb_ganancias <- tb_ganancias[Predicted == 1,]
    tb_ganancias[,gan := fifelse(clase_ternaria == "BAJA+2",273000,-7000)]
    
    ganancia <- tibble::tribble(~semilla,~ganancia,~envios,
                                semillas[i], sum(tb_ganancias$gan),envios)
    
    ganancias <- rbind(ganancias,ganancia)
    
  }
  
  print(paste0("Iteracion ",i, " finalizada"))
  
}

write.csv(ganancias,
       file = paste0(PARAM$experimento, "_ganancias_semillerio.csv"),
       sep = ","
)
  
cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
  



