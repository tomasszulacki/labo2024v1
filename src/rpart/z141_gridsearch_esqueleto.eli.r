# Limpieza del entorno
rm(list = ls())
gc()

# Carga de paquetes
require("data.table")
require("rpart")
require("parallel")

# Definición de parámetros
PARAM <- list()
PARAM$semillas <- c(100019, 100043, 100049, 100057, 100069)

# Función para particionar
particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  
  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa
  ]
}

# Función para estimar la ganancia
ArbolEstimarGanancia <- function(semilla, param_basicos) {
  particionar(dataset, division = c(7, 3), agrupa = "clase_ternaria", seed = semilla)
  
  modelo <- rpart("clase_ternaria ~ .",
                  data = dataset[fold == 1],
                  xval = 0,
                  control = param_basicos
  )
  
  prediccion <- predict(modelo,
                        dataset[fold == 2],
                        type = "prob"
  )
  
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
               ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
               0
    ))
  ]
  
  ganancia_test_normalizada <- ganancia_test / 0.3
  
  return(ganancia_test_normalizada)
}

# Función para ejecutar el grid search
ArbolesMontecarlo <- function(semillas, param_basicos) {
  ganancias <- mcmapply(ArbolEstimarGanancia,
                        semillas,
                        MoreArgs = list(param_basicos),
                        SIMPLIFY = FALSE,
                        mc.cores = 5
  )
  
  ganancia_promedio <- mean(unlist(ganancias))
  
  return(ganancia_promedio)
}

# Establecer el directorio de trabajo
setwd("~/buckets/b1")

# Cargar el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")
dataset <- dataset[clase_ternaria != ""]

# Crear la carpeta para el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearcheli.txt"

# Definir los parámetros básicos para el modelo
param_basicos <- list(
  cp = -0.5,         # Complejidad mínima
  minsplit = 600,    # Mínima cantidad de registros en un nodo para hacer el split
  minbucket = 150,   # Mínima cantidad de registros en una hoja
  maxdepth = 6       # Profundidad máxima del árbol
)

# Ejecutar el modelo con los parámetros definidos
ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)

# Guardar los resultados en la tabla
tb_grid_search <- data.table(
  maxdepth = integer(),
  minsplit = integer(),
  cp = numeric(),
  minbucket = integer(),
  ganancia_promedio = numeric()
)

tb_grid_search <- rbindlist( 
  list(tb_grid_search, 
       list(param_basicos$maxdepth, param_basicos$minsplit, param_basicos$cp, param_basicos$minbucket, ganancia_promedio)
  )
)

# Escribir los resultados en el archivo de salida
fwrite(tb_grid_search, file = archivo_salida, sep = "\t")
