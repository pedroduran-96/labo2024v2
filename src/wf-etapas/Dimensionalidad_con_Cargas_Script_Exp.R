# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose= FALSE) # garbage collection

require("data.table", quietly=TRUE)
require("stats",quietly = TRUE)   # Para prcomp() y PCA
require("dplyr", quietly = TRUE)   # Para manipulación de datos
require("tidyr",quietly = TRUE)   # Opcional, para manejo de datos (si es necesario)
require("yaml",quietly = TRUE) #MARTIN: Necesario. Sino no anda.
require("pcaMethods",quietly = TRUE) #MARTIN: Necesario. Sino no anda.
require("missMDA",quietly=TRUE) #MARTIN: Necesario. Sino no anda.
require("ggplot2",quietly=TRUE)

#-------------- Corro dimensionalidad

cat("ETAPA Reducción de dimensionalidad START")

red_dimensionalidad_con_nulos<- function(data,varianza_objetivo){

datasetRed <- data

id_cols <- c()

id_cols <- datasetRed[, c("numero_de_cliente", "foto_mes", "clase_ternaria") ]
datasetRed[, c("numero_de_cliente", "foto_mes", "clase_ternaria") := NULL]

cat("Dimensiones DatasetRed:", dim(datasetRed), "\n")

# Identificar columnas con valores nulos
#Esto es porque el PCA no funciona bien con nulos. Entonces esas columnas las separamos del análisis y las reincorporaremos luego.
columnas_con_nulos <- names(datasetRed)[sapply(datasetRed, function(col) any(is.na(col)))]
#Pruebo dejando todo como está.
#columnas_con_nulos <- c()
print(columnas_con_nulos)

# Contar los valores nulos por columna
nulos_por_columna <- colSums(is.na(datasetRed))

# Mostrar los resultados
print(nulos_por_columna)


# Eliminar temporalmente las columnas con valores nulos
#data_sin_nulos <- datasetRed[, !names(datasetRed) %in% columnas_con_nulos]
#data_sin_nulos <- datasetRed[, colSums(is.na(datasetRed)) == 0,with=FALSE]
data_sin_nulos <- datasetRed[, which(colSums(is.na(datasetRed)) == 0), with = FALSE]


cat("Dimensiones data_sin_nulos:", dim(data_sin_nulos), "\n")

# Verificar que no queden nulos en el dataset sin columnas con nulos
if (any(is.na(data_sin_nulos))) {
  stop("Aún quedan valores nulos en el dataset después de eliminar columnas. Verifica los datos.")
  #Dejo de frenar, pero aviso que hay
  #cat("Aún quedan nulos! Ojo")
  }

# Realizar PCA en las columnas sin nulos
pca_result <- prcomp(data_sin_nulos, scale. = TRUE)

cargas <- pca_result$rotation

# Convertir las cargas a un data frame, donde cada columna es una componente principal
cargas_df <- as.data.frame(cargas)

# Crear una tabla con las cargas en PC1 y PC2
cargas_tabla <- data.frame(
  Variable = rownames(cargas_df),
  Carga_PC1 = cargas_df$PC1,
  Carga_PC2 = cargas_df$PC2
)

# Ver la tabla de cargas
cat("Cargas de las primeras 2 variables del PCA: ")
print(cargas_tabla)


# Opción 1: Usar la función biplot base de R
biplot(pca_result, scale = 0, cex = 0.7)

# Obtener los scores de cada observación en PC1 y PC2
scores <- as.data.frame(pca_result$x)

# Crear el gráfico de puntos con las dos primeras componentes principales
biplot_gg <- ggplot(scores, aes(x = PC1, y = PC2)) +
              geom_point(color = "blue", alpha = 0.6) +
              geom_text(data = cargas_tabla, aes(x = Carga_PC1 * max(scores$PC1), y = Carga_PC2 * max(scores$PC2), label = Variable),
                        color = "red", size = 3, vjust = -0.5) +
              geom_segment(data = cargas_tabla, aes(x = 0, y = 0, xend = Carga_PC1 * max(scores$PC1), yend = Carga_PC2 * max(scores$PC2)),
                        arrow = arrow(length = unit(0.2, "cm")), color = "red") +
              labs(title = "Biplot de PCA", x = "PC1", y = "PC2") +
            theme_minimal()

ggsave("biplot_pca.png", plot = biplot_gg, width = 8, height = 6, dpi = 300)

#Corro variante más robusta a nulos
#pca_result <- imputePCA(data_sin_nulos, nPcs = ncol(data_sin_nulos))
cat("Dimensiones pca_result:", dim(pca_result), "\n")

# Calcular la varianza acumulada y encontrar el número mínimo de componentes necesarios
varianza_acumulada <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)

print(varianza_acumulada)

n_componentes <- which(varianza_acumulada >= varianza_objetivo)[1]

# Mantener solo las primeras n_componentes del resultado de PCA
data_reducido <- as.data.frame(pca_result$x[, 1:n_componentes])

# Reincorporar las columnas que se quitaron y las que tienen valores nulos al dataset reducido

cat("Dimensiones id_cols:", dim(id_cols), "\n")
cat("Dimensiones data_reducido:", dim(data_reducido), "\n")
cat("Dimensiones columnas_con_nulos:", dim(data[, ..columnas_con_nulos, drop = FALSE]), "\n")


data_inter <- cbind(id_cols,data_reducido) %>% as.data.frame()

data_final <- cbind(data_inter, data[, ..columnas_con_nulos, drop = FALSE])

# Imprimir la cantidad de componentes seleccionada y el porcentaje de varianza retenida
cat("Componentes seleccionados:", n_componentes, "\n")
cat("Varianza retenida:", varianza_acumulada[n_componentes] * 100, "%\n")

#Para ordenar: Extraer la columna y luego eliminarla
clase_ternaria <- data_final[["clase_ternaria"]]
data_final <- data_final[, !(names(data_final) %in% "clase_ternaria")]

#Agrego al final y convierto a data.frame
data_final <- cbind(data_final, clase_ternaria) %>% as.data.frame()

return(data_final)
}



#cargo la libreria
# args <- c( "~/labo2024ba" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

action_inicializar() 


# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset
envg$PARAM$dataset <- paste0( "./", envg$PARAM$input, "/dataset.csv.gz" )
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input, "/dataset_metadata.yml" ) )

cat( "lectura del dataset\n")
action_verificar_archivo( envg$PARAM$dataset )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread(envg$PARAM$dataset)
cat( "Finalizada lectura del dataset\n" )

GrabarOutput()


dataset <- red_dimensionalidad_con_nulos(dataset,0.9)

cat( "ordenado del dataset\n")
setorderv(dataset, envg$PARAM$dataset_metadata$primarykey)

# grabo el dataset
cat( "escritura del dataset\n")
cat( "Iniciando grabado del dataset\n" )
fwrite(dataset,
       file = "dataset.csv.gz",
       logical01 = TRUE,
       sep = ","
)
cat( "Finalizado grabado del dataset\n" )

# grabo metadata
#Me parece que esto es lo que hacía ilegible al paso siguiente. Improviso modificación de col_names.
# No funcionó. Todas lo dejan igual... Raro. Pero dejemos.
# nuevas_columnas <- colnames(dataset)
# 
# dataset_metadata <- envg$PARAM$dataset_metadata
# dataset_metadata$cols <- nuevas_columnas
# 
# 
# cat( "escritura de metadata\n")
# write_yaml( dataset_metadata, #envg$PARAM$dataset_metadata, 
#             file="dataset_metadata.yml" )

#------------------------------------------------------------------------------
# copia la metadata sin modificar
cat( "grabar metadata\n")

write_yaml( envg$PARAM$dataset_metadata, 
            file="dataset_metadata.yml" )

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
       file = "dataset.campos.txt",
       sep = "\t"
)

#------------------------------------------------------------------------------
cat( "Fin del programa\n")

envg$OUTPUT$dataset$ncol <- ncol(dataset)
envg$OUTPUT$dataset$nrow <- nrow(dataset)

envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

action_finalizar( archivos = c("dataset.csv.gz","dataset_metadata.yml")) 
cat( "ETAPA  De reducción de dimensionalidad  END\n")